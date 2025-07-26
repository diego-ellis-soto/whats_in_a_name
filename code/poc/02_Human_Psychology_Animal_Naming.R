
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
  geom_col() +
  labs(title="Proportion Human-Named", x="Human Name?", y="Proportion")
ggsave("outdir/prop_human_named.png", p1, width=6, height=4)

# --- 5.2 TOP NAMES BY POPULARITY AND CREATIVITY
p2 <- all_animals %>%
  arrange(desc(total_births)) %>%
  slice_head(n=10) %>%
  ggplot(aes(reorder(local_identifier, total_births), total_births)) +
  geom_col(fill="steelblue") + coord_flip() +
  labs(title="Top 10 Names by Locations", x="Name", y="Birth Count")
ggsave("outdir/top10_by_locations.png", p2, width=6, height=4)

p3 <- all_animals %>%
  arrange(desc(creativity_score)) %>%
  slice_head(n=10) %>%
  ggplot(aes(reorder(local_identifier, creativity_score), creativity_score)) +
  geom_col(fill="coral") + coord_flip() +
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
  geom_col(fill="steelblue") + coord_flip() +
  labs(title="Human-Named by Class", x="Class", y="Proportion")
ggsave("outdir/prop_by_class.png", p4, width=6, height=4)
