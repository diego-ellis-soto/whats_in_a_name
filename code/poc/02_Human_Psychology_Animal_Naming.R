# Galapagos not recognized as ecuador

# ---
# TITLE: Human Psychology of Conservation: Naming Patterns
# AUTHOR: Diego Ellis Soto
# DATE: Sys.Date()
# OUTPUT: HTML Document with TOC, Theme: United
# ---

# ▶︎ KNIT OPTIONS
knitr::opts_chunk$set(
  echo    = TRUE,
  warning = FALSE,
  message = FALSE
)

# ▶︎ CREATE OUTPUT DIRECTORY
if (!dir.exists("outdir")) dir.create("outdir")

# ▶︎ LOAD PACKAGES
required_pkgs <- c(
  "move", "dplyr", "purrr", "stringr", "readr", "sf",
  "rnaturalearth", "rnaturalearthdata", "babynames",
  "stringdist", "ggplot2", "wordcloud", "tidyr", "rgbif", "ggrepel"
)

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(rgbif)


install.packages(setdiff(required_pkgs, installed.packages()[,1]), repos = "https://cran.rstudio.com")
lapply(required_pkgs, library, character.only = TRUE)

# ▶︎ 1. PREPARE BABY NAME REFERENCE
baby_name_list <- babynames %>%
  mutate(name_lc = tolower(name)) %>%
  group_by(name_lc) %>%
  summarize(total_births = sum(n), .groups = "drop")
baby_name_vec <- baby_name_list$name_lc

# ▶︎ 2. COUNTRY LOOKUP UTILITIES
world <- ne_countries(scale = "medium", returnclass = "sf")
get_country_from_coords <- function(lat, lon) {
  pts <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
  st_join(pts, world["name"])$name
}

# ▶︎ 3. METADATA EXTRACTION FUNCTION
get_animal_metadata_from_study <- function(study, login) {
  study_obj <- getMovebankStudy(study = study, login = login)
  study_id  <- study_obj$id
  df <- getMovebankData(study = study, login = login, removeDuplicatedTimestamps = TRUE)
  id_data <- as_tibble(df@idData)

  coords <- as.data.frame(df) %>%
    select(tag_local_identifier, location_lat, location_long) %>%
    distinct()
  coords$country <- get_country_from_coords(coords$location_lat, coords$location_long)

  country_per_animal <- coords %>%
    group_by(tag_local_identifier) %>%
    summarize(country = if (all(is.na(country))) NA_character_ else names(sort(table(country), decreasing = TRUE))[1], .groups = "drop")

  positive_words <- c("joy","hope","grace","peace","sun","love")
  negative_words <- c("storm","rage","pain","ghost","death","shadow")

  meta <- id_data %>%
    select(local_identifier, individual_id, sex, taxon_canonical_name) %>%
    left_join(country_per_animal, by = c("local_identifier" = "tag_local_identifier")) %>%
    mutate(
      study_id = study_id,
      name_lc = tolower(local_identifier),
      is_human_name = name_lc %in% baby_name_vec,
      creativity_score = map_dbl(name_lc, ~ min(stringdist(.x, baby_name_vec, method = "jw"), na.rm = TRUE)),
      pos_score = map_dbl(name_lc, ~ min(stringdist(.x, positive_words, method = "jw"), na.rm = TRUE)),
      neg_score = map_dbl(name_lc, ~ min(stringdist(.x, negative_words, method = "jw"), na.rm = TRUE))
    ) %>%
    left_join(baby_name_list, by = "name_lc") %>%
    mutate(total_births = replace_na(total_births, 0))
  return(meta)
}

# ▶︎ 4. RUN OVER SELECTED STUDIES
login <- movebankLogin(username = "whatsinaname", password = "whatsinaname1!")
study_list <- c(123413, 2928116)
all_animals <- map_dfr(study_list, ~ get_animal_metadata_from_study(.x, login))
write_csv(all_animals, "animal_metadata_full.csv")

# ▶︎ 5. EXPLORATORY ANALYSES
# --- 5.1 PROPORTION HUMAN-NAMED
prop_df <- all_animals %>% count(is_human_name) %>% mutate(prop = n / sum(n))
print(prop_df)
p1 <- ggplot(prop_df, aes(x = is_human_name, y = prop, fill = is_human_name)) +
  geom_col() + theme_classic()+
  labs(title="Proportion Human-Named", x="Human Name?", y="Proportion")
ggsave("outdir/prop_human_named.png", p1, width=6, height=4)

# --- 5.2 TOP NAMES BY POPULARITY AND CREATIVITY
p2 <- all_animals %>%
  arrange(desc(total_births)) %>%
  slice_head(n=10) %>%
  ggplot(aes(reorder(local_identifier, total_births), total_births)) +
  geom_col(fill="steelblue") + coord_flip() +theme_classic()+
  labs(title="Top 10 Names by Locations", x="Name", y="Birth Count")
ggsave("outdir/top10_by_locations.png", p2, width=6, height=4)

p3 <- all_animals %>%
  arrange(desc(creativity_score)) %>%
  slice_head(n=10) %>%
  ggplot(aes(reorder(local_identifier, creativity_score), creativity_score)) +
  geom_col(fill="coral") + coord_flip() +theme_classic()+
  labs(title="Top 10 Creative Names", x="Name", y="Creativity Score")
ggsave("outdir/top10_creativity.png", p3, width=6, height=4)

# --- 5.3 WORDCLOUD OF NAMES
png("outdir/wordcloud_names.png", width=800, height=600)
par(mar=c(0,0,0,0))
wordcloud(all_animals$local_identifier, freq=table(all_animals$local_identifier), min.freq=1, random.order=FALSE, scale=c(1,0.5))
dev.off()

# --- 5.4 COUNTRY-LEVEL SUMMARY
country_df <- all_animals %>%
  group_by(country) %>%
  summarize(
    total_animals = n(),
    avg_births = mean(total_births, na.rm=TRUE),
    human_named_rate = mean(is_human_name, na.rm=TRUE),
    avg_creativity = mean(creativity_score, na.rm=TRUE),
    .groups = "drop"
  )
print(country_df)

# ▶︎ 6. GEOGRAPHIC VISUALIZATION
map_plot <- ggplot(left_join(world, country_df, by=c("name"="country")), aes(fill=human_named_rate)) +
  geom_sf() + scale_fill_viridis_c(name="Prop. Human-Named", na.value="lightgrey") +
  labs(title="Prop. Human-Named by Country", caption="Movebank & babynames") +
  theme_minimal()
ggsave("outdir/map_human_named.png", map_plot, width=6, height=4)

# ▶︎ 7. TAXONOMIC CLASSIFICATION & VISUALIZATION

get_taxonomic_class <- function(sci_name) {
  res <- tryCatch(name_backbone(name=sci_name), error=function(e) NULL)
  if (is.null(res) || !"class" %in% names(res) || is.na(res$class)) return(NA_character_)
  res$class
}
lookup <- tibble(taxon_canonical_name = unique(all_animals$taxon_canonical_name),
                 class = map_chr(taxon_canonical_name, get_taxonomic_class))
all_animals <- left_join(all_animals, lookup, by="taxon_canonical_name")

p4 <- all_animals %>%
  group_by(class) %>%
  summarize(prop=mean(is_human_name, na.rm=TRUE)) %>%
  ggplot(aes(reorder(class, prop), prop)) +
  geom_col(fill="steelblue") + coord_flip() +theme_classic()+
  labs(title="Human-Named by Class", x="Class", y="Proportion")
ggsave("outdir/prop_by_class.png", p4, width=6, height=4)


world <- ne_countries(scale = "medium", returnclass = "sf")

all_studies <- getMovebank(entity_type = "study", login = login) %>%
  drop_na(main_location_lat, main_location_long) %>%
  filter(is_test == 'false', study_type == 'research',
         i_have_download_access =='true') %>%
  filter(!grepl('tmp', name))


downloadable_studies_sf <- st_as_sf(all_studies,
                                    coords = c("main_location_long", "main_location_lat"),
                                    crs = 4326
)

# ----------------------------
# 4. PLOT
# ----------------------------

p4 <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray50", size = 0.25) +
  geom_sf(data = downloadable_studies_sf, color = "darkred", size = 1, shape = 21, fill = "black") +
  theme_minimal() +
  labs(
    title = "Movebank Research Studies with Download Access",
    subtitle = "Each point is a study you can download",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("outdir/Public_movement_data.png", p4, width=6, height=4)



# Next steps:


# Undergraduate work: 
# Site of baby names python:
# Accept terms and agreements
# Taxonomy
# Look at author name responsible and country of origin
# 
# Taxonomic Done
# Accept Manually all those studies
# Add matrix of names and their countries from python global
# 
# Do some plots of language to language based on country -> 
# Global map of individuals per grid cell of this movebank study
# Add Elton Traits
# global plot of country to country
# gloal plot of GDP from country to country
# whats the most common name of a individual animal tracked?
# Plot creativity vs. name popularity


# https://github.com/sigpwned/popular-names-by-country-dataset



# ▶︎ Required Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(rgbif)

# ▶︎ Extract latitudes and taxonomic info
df <- st_drop_geometry(downloadable_studies_sf) %>%
  mutate(lat = sf::st_coordinates(downloadable_studies_sf)[,2])

# ▶︎ Separate multiple species from taxon_ids
tax_df <- df %>%
  select(id, lat, taxon_ids, number_of_individuals) %>%
  filter(!is.na(taxon_ids)) %>%
  mutate(taxon_ids = strsplit(taxon_ids, ",")) %>%
  unnest(taxon_ids) %>%
  mutate(taxon_ids = str_trim(taxon_ids))

# ▶︎ Add taxonomic lookup (get family using rgbif)
get_family <- function(sci_name) {
  res <- tryCatch(name_backbone(name = sci_name), error = function(e) NULL)
  if (is.null(res) || !"family" %in% names(res)) return(NA_character_)
  return(res$family)
}

# Lookup family (optional - can skip if only doing species)
tax_df <- tax_df %>%
  distinct(taxon_ids) %>%
  mutate(family = purrr::map_chr(taxon_ids, get_family)) %>%
  right_join(tax_df, by = "taxon_ids")

# ▶︎ Bin by latitude
tax_df <- tax_df %>%
  mutate(lat_bin = round(lat, 0))  # Use 1-degree bins

# ▶︎ Count unique species per latitude
species_lat <- tax_df %>%
  group_by(lat_bin) %>%
  summarise(n_species = n_distinct(taxon_ids))

# ▶︎ Count unique families per latitude
family_lat <- tax_df %>%
  filter(!is.na(family)) %>%
  group_by(lat_bin) %>%
  summarise(n_families = n_distinct(family))

# ▶︎ Sum individuals per latitude
individuals_lat <- df %>%
  mutate(lat_bin = round(lat, 0)) %>%
  group_by(lat_bin) %>%
  summarise(n_individuals = sum(as.numeric(number_of_individuals), na.rm = TRUE))

# ▶︎ Count studies per latitude
studies_lat <- df %>%
  mutate(lat_bin = round(lat, 0)) %>%
  group_by(lat_bin) %>%
  summarise(n_studies = n_distinct(id))

# ▶︎ Plotting function

# plot_latitude <- function(data, yvar, title, ylab, filename) {
#   p <- ggplot(data, aes(y = lat_bin, x = !!sym(yvar))) +
#     geom_area(stat = "identity", fill = "steelblue", alpha = 0.6) +
#     geom_line(color = "darkblue", size = 1) +
#     labs(
#       title = title,
#       x = ylab,
#       y = "Latitude"
#     ) +
#     theme_minimal() + ylim(-180,180)+
#     theme(panel.grid.minor = element_blank())
#   
#   print(p)
#   ggsave(paste0("outdir/", filename, ".png"), p, width = 6, height = 5)
# }

library(ggplot2)
library(dplyr)

# A flexible plotting function for latitude-based richness/abundance patterns
plot_latitude <- function(data, yvar, title, ylab, filename) {
  p <- ggplot(data, aes(y = lat_bin, x = !!sym(yvar))) +
    geom_path(stat = "identity", color = "steelblue", linewidth = 1.2) +
    geom_area(stat = "identity", fill = "steelblue", alpha = 0.3) +
    scale_y_continuous(limits = c(-90, 90), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(
      title = title,
      x = ylab,
      y = "Latitude (°)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  print(p)
  ggsave(paste0("outdir/", filename, ".png"), p, width = 6, height = 6, dpi = 300)
}

# ▶︎ Generate and save all plots
plot_latitude(species_lat, "n_species", "Species Richness by Latitude", "Species", "lat_species")
plot_latitude(family_lat, "n_families", "Family Richness by Latitude", "Families", "lat_families")
plot_latitude(individuals_lat, "n_individuals", "Tracked Individuals by Latitude", "Individuals", "lat_individuals")
plot_latitude(studies_lat, "n_studies", "Study Counts by Latitude", "Studies", "lat_studies")


