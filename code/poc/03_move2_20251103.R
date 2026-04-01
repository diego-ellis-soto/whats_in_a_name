# ============================================================
# Movebank Study Loop — Sleep + Text Log + integer64-safe skipping
# ============================================================

# Somehow only 261 studies were downlaoded succesfull?! ####
# Check in the log file
# Next steps: Load all ter studies in study_ids (bottom of code) and compare 
# And only re-run for the studies that are not present in the .csv file of outdir called
# summary_by_individula_all_studies_20251103 ####


# Has a text log file
# Has integer64 to avoid confusion
# Runs on move2
# Has name matching from babyname DB
# Next would be country speicfic 
# Add taxonomy via taxize
# Add species traits
# Manually verify names
# Find researchers location
# Taxize next or rgbif classification
# some are not wgs84 so reproject could not find ocuntries
# Manually help with taxonomy
# Think about species traits: Vertnet?
# --- Dependencies used below ---
library(dplyr)
library(sf)
library(readr)
library(data.table)      # fread/fwrite, integer64 support
library(bit64)           # explicit integer64 helpers
library(rnaturalearth)
library(babynames)
library(MazamaSpatialUtils)
library(tidyverse)
library(move2)
require(tidyverse)
# --- Pre-compute helpers you already had / need ---
# Baby names list (lowercased)
baby_name_list <- babynames %>%
  mutate(name_lc = tolower(name)) %>%
  group_by(name_lc) %>%
  summarise(total_locations = sum(n), .groups = "drop")

# Country polygons once (kept if you use elsewhere)
world_countries <- ne_countries(scale = "small", returnclass = "sf")

# Helper: spatial join to ISO A2
getCountryCode <- function(sf_points) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  st_join(sf_points, world["iso_a2"], join = st_within)
}

# -----------------------------
# Main for-loop function
# -----------------------------
run_movebank_loop <- function(out_dir = "outdir",
                              overwrite_global_csv = FALSE,
                              sleep_seconds = 1) {
  
  # Ensure output dirs
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  per_study_dir <- file.path(out_dir, "single_studies")
  dir.create(per_study_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Paths
  global_csv <- file.path(out_dir, "summary_by_individual_all_studies_20251103.csv")
  study_info_csv <- file.path(out_dir, "study_info_data_v2.csv")
  log_file <- file.path(out_dir, "run_log_20251103.txt")
  
  # Simple logger
  log_line <- function(sid, status, msg = "") {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    sid_chr <- if (inherits(sid, "integer64")) as.character(sid) else as.character(sid)
    cat(sprintf("%s\t%s\t%s\t%s\n", ts, sid_chr, status, msg), file = log_file, append = TRUE)
  }
  
  # Create log header if needed
  if (!file.exists(log_file)) {
    cat("timestamp\tstudy_id\tstatus\tmessage\n", file = log_file, append = FALSE)
  }
  
  # 1) Get & filter study info once
  study_list_v2 <- movebank_download_study_info(i_have_download_access = TRUE) |>
    dplyr::filter(is_test == FALSE,
                  study_type == "research",
                  sensor_type_ids != "NA",
                  taxon_ids != "NA",
                  taxon_ids != "test",
                  !grepl("tmp", name))
  
  # Save study-level info snapshot
  study_list_v2 |>
    dplyr::select(
      id, license_type, name,
      number_of_individuals, number_of_tags, number_of_deployed_locations,
      principal_investigator_address, principal_investigator_email, principal_investigator_name,
      there_are_data_which_i_cannot_see, i_have_download_access, i_can_see_data,
      timestamp_first_deployed_location, timestamp_last_deployed_location,
      taxon_ids, sensor_type_ids, contact_person_name, main_location
    ) |>
    readr::write_csv(study_info_csv)
  
  # ------------------------------------------------------------
  # Guardrail: integer64-safe removal of already-downloaded studies
  #   - Read existing global CSV (if present)
  #   - Build integer64 vector of already_downloaded
  #   - Skip the whole step if none present
  # ------------------------------------------------------------
  already_downloaded <- bit64::integer64(0)
  if (file.exists(global_csv)) {
    existing <- tryCatch(
      data.table::fread(global_csv, showProgress = FALSE),
      error = function(e) NULL
    )
    if (!is.null(existing)) {
      if ("study_id" %in% names(existing)) {
        # fread will keep integer64 if file was written by fwrite previously
        already_downloaded <- bit64::as.integer64(existing[["study_id"]])
      } else if ("id" %in% names(existing)) {
        already_downloaded <- bit64::as.integer64(existing[["id"]])
      }
      already_downloaded <- unique(stats::na.omit(already_downloaded))
    }
  }
  
  # If user asked to overwrite the global CSV, delete it AFTER harvesting skip list
  if (overwrite_global_csv && file.exists(global_csv)) file.remove(global_csv)
  
  # Base set of study IDs as integer64
  study_ids <- bit64::as.integer64(study_list_v2$id)
  
  if (!is.null(already_downloaded) && length(already_downloaded) > 0) {
    # integer64-safe set difference using data.table::fsetdiff on 1-col data.tables
    # study_ids <- data.table::fsetdiff(
    #   as.data.table(study_ids),          # column V1
    #   as.data.table(already_downloaded)  # column V1
    # )$V1
    
    study_ids = study_ids[! study_ids%in% already_downloaded]
    
    message(sprintf("Skipping %d studies already in global CSV.", length(already_downloaded)))
    # Optional: log each skipped ID (can be many)
    for (sid_skip in already_downloaded) {
      log_line(sid_skip, "SKIPPED_ALREADY_IN_GLOBAL", "present in global CSV prior to this run")
    }
  } else {
    message("No previously downloaded studies found — processing all studies.")
  }
  
  # If nothing left, stop
  if (length(study_ids) == 0) {
    message("Nothing to do. All studies already processed based on guardrails.")
    message("Run log at: ", log_file)
    return(invisible(global_csv))
  }
  
  # --- Progress bar setup ---
  n_total <- nrow(study_list_v2)             # total studies available
  n_skipped <- length(already_downloaded)    # how many are being skipped
  n_remaining <- length(study_ids)           # how many will actually run
  
  message(sprintf("Total studies: %d | Skipped (already in global): %d | Remaining to process: %d",
                  n_total, n_skipped, n_remaining))
  
  pb <- utils::txtProgressBar(min = 0, max = n_remaining, style = 3)
  on.exit(close(pb), add = TRUE)
  
  for (i in seq_along(study_ids)) {
    sid <- study_ids[i]
    message(sprintf("[Study %d/%d] Processing study_id: %s",
                    i, n_remaining, as.character(sid)))
    
    tryCatch({
      # Reference data (deployments, tags, individuals)
      ref_table <- movebank_download_deployment(study_id = sid) |>
        dplyr::select(deployment_id,
                      individual_local_identifier,
                      study_id,
                      sex,
                      taxon_canonical_name)
      
      # print(unique(ref_table$taxon_canonical_name))
      # Tracking data (as sf)
      tmp_track_data <- movebank_download_study(study_id = sid, attributes = NULL)
      
      if (nrow(tmp_track_data) == 0 || nrow(ref_table) == 0) {
        msg <- sprintf("No data returned for study_id %s", as.character(sid))
        warning(msg)
        log_line(sid, "SKIPPED_NO_DATA", msg)
        utils::setTxtProgressBar(pb, i)
        if (sleep_seconds > 0) Sys.sleep(sleep_seconds)
        next
      }
      
      # Add ISO A2 + join ref_table
      tmp_track_data <- tmp_track_data |>
        getCountryCode() |>
        dplyr::left_join(ref_table, by = "deployment_id") |>
        dplyr::mutate(
          name_lc = tolower(individual_local_identifier),
          is_human_name = name_lc %in% baby_name_list$name_lc
        )
      
      # Drop geometry for tabular ops
      tmp_track_data <- tmp_track_data %>% sf::st_drop_geometry()
      
      # (1) counts per individual x country
      country_counts <- tmp_track_data %>%
        dplyr::mutate(iso_a2 = dplyr::if_else(is.na(iso_a2) | iso_a2 == "", "NA", iso_a2)) %>%
        dplyr::count(individual_local_identifier, iso_a2, name = "n", sort = TRUE)
      
      # (2) collapse country counts to a string
      country_str <- country_counts %>%
        dplyr::arrange(individual_local_identifier, dplyr::desc(n), iso_a2) %>%
        dplyr::group_by(individual_local_identifier) %>%
        dplyr::summarise(
          country = paste(paste0(n, " ", iso_a2), collapse = ", "),
          .groups = "drop"
        )
      
      # (3) first country by time
      first_country <- tmp_track_data %>%
        dplyr::arrange(individual_local_identifier, timestamp) %>%
        dplyr::group_by(individual_local_identifier) %>%
        dplyr::summarise(first_country = dplyr::first(iso_a2[!is.na(iso_a2)]),
                         .groups = "drop")
      
      # Per-individual core fields
      track_df_summary <- tmp_track_data %>%
        dplyr::arrange(individual_local_identifier, timestamp) %>%
        dplyr::group_by(individual_local_identifier) %>%
        dplyr::summarise(
          study_id = dplyr::first(stats::na.omit(study_id)),
          sex = dplyr::first(stats::na.omit(sex)),
          taxon_canonical_name = dplyr::first(stats::na.omit(taxon_canonical_name)),
          name_lc = dplyr::first(stats::na.omit(name_lc)),
          is_human_name = dplyr::first(stats::na.omit(is_human_name)),
          .groups = "drop"
        )
      
      # Final per-individual summary for this study
      summary_by_individual <- track_df_summary %>%
        dplyr::left_join(country_str, by = "individual_local_identifier") %>%
        dplyr::left_join(first_country, by = "individual_local_identifier") %>%
        dplyr::select(
          individual_local_identifier,
          study_id,
          sex,
          taxon_canonical_name,
          name_lc,
          is_human_name,
          first_country,
          country
        ) %>%
        dplyr::arrange(individual_local_identifier)
      
      # Join study-level metadata for this study
      study_info_tmp <- study_list_v2 %>%
        dplyr::filter(id == sid) %>%
        dplyr::select(id, name,
                      principal_investigator_address, principal_investigator_email,
                      principal_investigator_name, study_objective,
                      contact_person_name, main_location,
                      acknowledgements)
      
      summary_by_individual_v2 <- summary_by_individual %>%
        dplyr::left_join(study_info_tmp, by = c("study_id" = "id"))
      
      # Save per-study CSV
      per_study_csv <- file.path(per_study_dir, paste0("summary_", as.character(sid), ".csv"))
      readr::write_csv(summary_by_individual_v2, per_study_csv)
      
      # Append to global CSV (header only if file doesn't exist)
      data.table::fwrite(summary_by_individual_v2,
                         file = global_csv,
                         append = file.exists(global_csv),
                         col.names = !file.exists(global_csv))
      
      log_line(sid, "SUCCESS", sprintf("wrote %s and appended to global CSV", basename(per_study_csv)))
    },
    error = function(e) {
      warn_msg <- sprintf("Error in study_id %s: %s", as.character(sid), conditionMessage(e))
      warning(warn_msg)
      log_line(sid, "ERROR", warn_msg)
    })
    
    utils::setTxtProgressBar(pb, i)
    if (sleep_seconds > 0) Sys.sleep(sleep_seconds)  # polite pause between studies
  }
  
  message("Done. Global CSV at: ", global_csv)
  message("Run log at: ", log_file)
  invisible(global_csv)
}

# ------------------------------------------------------------
# Example run (incremental; skips already-downloaded studies)
# ------------------------------------------------------------
run_movebank_loop(out_dir = "outdir", overwrite_global_csv = FALSE, sleep_seconds = 5)

# 985143423 did run, was 5GB+
# 3176684879 was 7.87GB+ 723/784
# 2886960828 was 6+GB 746/784]

# Latitudinal gradient of 


require(rvertnet)
library(rgbif)     
library(rvertnet)  

list.files(out_dir)
all_animals = read.csv('outdir/summary_by_individual_all_studies_20251103.csv')

get_taxonomic_class <- function(sci_name) {
  res <- tryCatch(name_backbone(name=sci_name), error=function(e) NULL)
  if (is.null(res) || !"class" %in% names(res) || is.na(res$class)) return(NA_character_)
  res$class
}

# helper: safely query GBIF backbone for one species name
get_gbif_taxonomy <- function(sci_name) {
  if (is.na(sci_name) || sci_name == "") {
    return(tibble(class = NA_character_,
                  order = NA_character_,
                  family = NA_character_,
                  gbif_usageKey = NA_integer_))
  }
  
  res <- tryCatch(
    rgbif::name_backbone(name = sci_name),
    error = function(e) NULL
  )
  
  # if no good match
  if (is.null(res) || identical(res$matchType, "NONE")) {
    return(tibble(class = NA_character_,
                  order = NA_character_,
                  family = NA_character_,
                  gbif_usageKey = NA_integer_))
  }
  
  tibble(
    class = res$class %||% NA_character_,
    order = res$order %||% NA_character_,
    family = res$family %||% NA_character_,
    gbif_usageKey = res$usageKey %||% NA_integer_
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# build lookup table for unique species in your CSV
tax_lookup <- all_animals %>%
  distinct(taxon_canonical_name) %>%
  mutate(gbif = map(taxon_canonical_name, get_gbif_taxonomy)) %>%
  unnest(gbif)

# join taxonomy back to all rows
all_animals <- all_animals %>%
  left_join(tax_lookup, by = "taxon_canonical_name")

# # build lookup table for unique species in your CSV
# tax_lookup <- all_animals %>%
#   distinct(taxon_canonical_name) %>%
#   mutate(gbif = map(taxon_canonical_name, get_gbif_taxonomy)) %>%
#   unnest(gbif)
# 
# # join taxonomy back to all rows
# all_animals <- all_animals %>%
#   left_join(tax_lookup, by = "taxon_canonical_name")
# 

lookup <- tibble(taxon_canonical_name = unique(all_animals$taxon_canonical_name),
                 class = map_chr(taxon_canonical_name, get_taxonomic_class))
head(lookup)
all_animals <- left_join(all_animals, lookup, by="taxon_canonical_name")

all_animals <- all_animals |> 
  mutate(order = if_else(class.x == "Testudines" & is.na(order), "Testudines", order))

by_class = all_animals %>%
  group_by(class.x) %>%
  summarize(
    prop = mean(is_human_name, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # optional: only keep reasonably sampled classes
  # filter(!is.na(class), n >= 50) %>%
  ggplot(aes(x = reorder(class.x, prop), y = prop)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Human-Named Individuals by Taxonomic Class (GBIF backbone)",
    x = "Class",
    y = "Proportion with human-like names"
  )

ggsave("outdir/by_class.png", by_class, width=8, height=12)

# By order:
order_dat <- all_animals %>%
  # filter(!is.na(order), !is.na(class)) %>%
  group_by(order, class.x) %>%
  summarize(
    prop = mean(is_human_name, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # if an order somehow has multiple classes, keep the dominant one
  group_by(order) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# build a class -> color map (same used for bars & y labels)
classes <- sort(unique(order_dat$class.x))
require(RColorBrewer)
pal <- brewer.pal(max(3, length(classes)), "Set3")[seq_along(classes)]
names(pal) <- classes

# order of y-axis labels (low to high prop)
order_levels <- order_dat$order[order(order_dat$prop)]


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggtext)

# By order:
order_dat <- all_animals %>%
  filter(!is.na(order)) %>%
  group_by(order, class.x) %>%
  summarize(
    prop = mean(is_human_name, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # if an order has multiple classes, keep the one with most rows
  group_by(order) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# order levels by prop
order_levels <- order_dat$order[order(order_dat$prop)]

# palette for classes
classes <- sort(unique(order_dat$class.x))
pal <- brewer.pal(max(3, length(classes)), "Set3")[seq_along(classes)]
require(viridis)
pal <- viridis_pal(option = "D")(length(classes))  # options: "A", "B", "C", "D", "E", "F"

names(pal) <- classes

# match each order to its class color (NA -> black)
label_classes <- order_dat$class.x[match(order_levels, order_dat$order)]
label_cols <- ifelse(is.na(label_classes), "black", pal[label_classes])

order_fig_vir = order_dat %>%
  ggplot(aes(x = factor(order, levels = order_levels),
             y = prop,
             fill = class.x)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = pal, na.value = "grey70") +
  scale_x_discrete(
    labels = setNames(
      sprintf("<span style='color:%s'>%s</span>", label_cols, order_levels),
      order_levels
    )
  ) +
  labs(
    title = "Human-Named Individuals by Taxonomic Order",
    x = "Order",
    y = "Proportion with human-like names",
    fill = "Class"
  ) +
  theme(
    axis.text.y = ggtext::element_markdown(face = "bold"),
    legend.position = "right"
  )

ggsave("outdir/prop_by_order_all_viridis.png", order_fig_vir, width=8, height=12)

# 

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggtext)

# By order:
order_dat <- all_animals %>%
  filter(!is.na(order)) %>%
  group_by(order, class) %>%
  summarize(
    prop = mean(is_human_name, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # if an order has multiple classes, keep the one with most rows
  group_by(order) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# order levels by prop
order_levels <- order_dat$order[order(order_dat$prop)]

# palette for classes
classes <- sort(unique(order_dat$class.x))
pal <- brewer.pal(max(3, length(classes)), "Set3")[seq_along(classes)]
names(pal) <- classes

# match each order to its class color (NA -> black)
label_classes <- order_dat$class[match(order_levels, order_dat$order)]
label_cols <- ifelse(is.na(label_classes), "black", pal[label_classes])

order_dat %>%
  ggplot(aes(x = factor(order, levels = order_levels),
             y = prop,
             fill = class)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = pal, na.value = "grey70") +
  scale_x_discrete(
    labels = setNames(
      sprintf("<span style='color:%s'>%s</span>", label_cols, order_levels),
      order_levels
    )
  ) +
  labs(
    title = "Human-Named Individuals by Taxonomic Order",
    x = "Order",
    y = "Proportion with human-like names",
    fill = "Class"
  ) +
  theme(
    axis.text.y = ggtext::element_markdown(face = "bold"),
    legend.position = "right"
  )











order_fig = order_dat %>%
  ggplot(aes(x = factor(order, levels = order_levels),
             y = prop,
             fill = class.x)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = pal, drop = FALSE) +
  labs(
    title = "Human-Named Individuals by Taxonomic Order",
    x = "Order",
    y = "Proportion with human-like names",
    fill = "Class"
  ) +
  theme(
    axis.text.y = element_text(
      colour = pal[order_dat$class[match(order_levels, order_dat$order)]],
      face = "bold"
    ),
    legend.position = "right"
  )

ggsave("outdir/prop_by_order_all.png", order_fig, width=8, height=12)


# --- 5.3 WORDCLOUD OF NAMES
require(wordcloud)
png("outdir/wordcloud_names.png", width=800, height=600)
par(mar=c(0,0,0,0))
wordcloud(all_animals$individual_local_identifier, freq=table(all_animals$individual_local_identifier), min.freq=1, random.order=FALSE, scale=c(1,0.5))
dev.off()

all_animals_true_human = all_animals |> filter(is_human_name == 'TRUE')
wordcloud(all_animals_true_human$individual_local_identifier, freq=table(all_animals_true_human$individual_local_identifier), min.freq=1, random.order=FALSE, scale=c(1,0.5))

# Next questions: Do we do two specific analysis digging deeper?
# We should have 890 studies?! -> 189 species seems a bit low to me -> do we have NA?
# Check for sanity the number of unique species in the metadata associated with our 900 studies if they are the same amount of matching species 



# Global map:
login <- movebankLogin(username = "whatsinaname", password = "whatsinaname1!")

all_studies <- getMovebank(entity_type = "study", login = login) %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(main_location_lat), !is.na(main_location_long)) %>%
  dplyr::filter(is_test == "false", study_type == "research",
                i_have_download_access == "true") %>%
  dplyr::filter(!grepl("tmp", name))

# Base map of studies with download access (optional visualization)
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

downloadable_studies_sf <- sf::st_as_sf(
  all_studies,
  coords = c("main_location_long", "main_location_lat"),
  crs = 4326
)
ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray50", size = 0.25) +
  geom_sf(data = downloadable_studies_sf, color = "darkred", size = 1, shape = 21, fill = "black") +
  theme_minimal() +
  labs(
    title = "Movebank Research Studies with Download Access",
    subtitle = "Each point is a study you can download",
    x = NULL, y = NULL
  ) +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# List of accessible study IDs to iterate
study_ids <- all_studies %>% dplyr::pull(id)
unique(all_studies$taxon_ids)
