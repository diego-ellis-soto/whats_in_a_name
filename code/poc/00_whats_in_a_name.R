# Diego Ellis Soto

# ▶︎ INSTALL & LOAD PACKAGES ------------------------------------------------

required_pkgs <- c(
  "move", "dplyr", "purrr", "stringr", "readr", "sf", 
  "rnaturalearth", "rnaturalearthdata", "babynames", 
  "stringdist", "ggplot2", "wordcloud", "tidyr",'rgbif'
)

install.packages(setdiff(required_pkgs, installed.packages()[,1]), 
                 repos = "https://cran.rstudio.com")

lapply(required_pkgs, library, character.only = TRUE)


# ▶︎ LOAD BABY NAME LIST &VOLUME POPULARITY ---------------------------------------

# Build upon PNAs paper code:
# Show sample size global of individuals in Movebank of which we analyzed data Figure 1
# 

baby_name_list <- babynames %>%
  mutate(name_lc = tolower(name)) %>%
  group_by(name_lc) %>%
  summarize(total_locations = sum(n), .groups = "drop")

baby_name_vec <- baby_name_list$name_lc


# ▶︎ WORLD MAP FOR COUNTRY LOOKUP -------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

get_country_from_coords <- function(lat, lon) {
  pts <- st_as_sf(data.frame(lon = lon, lat = lat),
                  coords = c("lon", "lat"), crs = 4326)
  joined <- st_join(pts, world["name"])
  joined$name
}

# ▶︎ MAIN METADATA FUNCTION -------------------------------------------------

get_animal_metadata_from_study <- function(study_name, login) {
  # study_obj <- getMovebankStudy(study = study_name, login = login)
  study_obj <- getMovebankStudy(study = study_name, login = login)
  
  study_id  <- study_obj$id
  
  df <- getMovebankData(study = study_name, login = login,
                        removeDuplicatedTimestamps=TRUE)
  id_data <- as_tibble(df@idData)
  
  coords <- as.data.frame(df) %>%
    select(tag_local_identifier, location_lat, location_long) %>%
    distinct()
  
  coords$country <- get_country_from_coords(
    coords$location_lat, coords$location_long
  )
  
  country_per_animal <- coords %>%
    group_by(tag_local_identifier) %>%
    summarize(
      country = if (all(is.na(country))) NA_character_ else names(sort(table(country), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Custom sentiment words (optional)
  positive_words <- c("joy", "hope", "grace", "peace", "sun", "love")
  negative_words <- c("storm", "rage", "pain", "ghost", "death", "shadow")
  
  # Merge metadata
  meta <- id_data %>%
    select(local_identifier, individual_id, sex, taxon_canonical_name) %>%
    left_join(country_per_animal, by = c("local_identifier" = "tag_local_identifier")) %>%
    mutate(
      study_id         = study_id,
      name_lc          = tolower(local_identifier),
      is_human_name    = name_lc %in% baby_name_vec,
      creativity_score = map_dbl(name_lc, ~ min(stringdist(.x, baby_name_vec, method = "jw"))),
      pos_score        = map_dbl(name_lc, ~ min(stringdist(.x, positive_words, method = "jw"))),
      neg_score        = map_dbl(name_lc, ~ min(stringdist(.x, negative_words, method = "jw")))
    ) %>%
    left_join(baby_name_list, by = "name_lc") %>%
    mutate(
      total_locations = replace_na(total_locations, 0)
    )
  
  meta
}


# ▶︎ RUN OVER STUDY LIST ----------------------------------------------------

login <- movebankLogin(username = "whatsinaname", password = "whatsinaname1!")

study_list <- c(
  "Ocelots on Barro Colorado Island, Panama",
  'Galapagos Tortoise Movement Ecology Programme'
  # Add more studies here
)

study_list <- c(
  123413, # , # "Ocelots on Barro Colorado Island, Panama",
   2928116# 'Galapagos Tortoise Movement Ecology Programme'
)

all_animals <- map_dfr(study_list, ~ get_animal_metadata_from_study(.x, login))

write_csv(all_animals, "animal_metadata_full.csv")

# ▶︎ EXPLORATORY ANALYSES ---------------------------------------------------

# 1. Human-named proportion
all_animals %>%
  count(is_human_name) %>%
  mutate(prop = n / sum(n))

# 2. Most popular human-like names
all_animals %>%
  arrange(desc(total_locations)) %>%
  select(local_identifier, total_locations) %>%
  head(10)

# 3. Most creative names
all_animals %>%
  arrange(desc(creativity_score)) %>%
  select(local_identifier, creativity_score) %>%
  head(10)

# 4. Wordcloud of names
wordcloud(
  words = all_animals$local_identifier,
  freq = table(all_animals$local_identifier),
  min.freq = 1,
  random.order = FALSE,
  scale = c(0.5, 0.5)
)

# 5. Name popularity by country
all_animals %>%
  group_by(country) %>%
  summarize(
    n = n(),
    avg_births = mean(total_locations, na.rm = TRUE),
    human_named = mean(is_human_name),
    avg_creativity = mean(creativity_score)
  ) %>%
  arrange(desc(human_named)) %>%
  print(n = Inf)

# 6. Optional: “Emotional proximity” scores
all_animals %>%
  select(local_identifier, pos_score, neg_score) %>%
  arrange(pos_score) %>%
  head(5)

# ▶︎ MAP: Countries with Human-Named Animals --------------------------------

# Prepare data: count of animals with human names per country
country_human_named <- all_animals %>%
  group_by(country) %>%
  summarize(
    total_animals = n(),
    human_named = sum(is_human_name, na.rm = TRUE),
    prop_human_named = human_named / total_animals,
    .groups = "drop"
  )

# Merge with world map
world_named <- world %>%
  left_join(country_human_named, by = c("name" = "country"))

# Plot map with proportion of human-named animals
ggplot(world_named) +
  geom_sf(aes(fill = prop_human_named)) +
  scale_fill_viridis_c(
    name = "Prop. Human-Named",
    na.value = "lightgrey",
    option = "plasma",
    direction = -1
  ) +
  labs(
    title = "Proportion of Tracked Animals with Human Names by Country",
    caption = "Data: Movebank + babynames R package"
  ) +
  theme_minimal()


# 4) Word‑cloud of animal names
par(mar = c(0, 0, 0, 0))  # remove white space
wordcloud(
  words        = all_animals$local_identifier,
  freq         = table(all_animals$local_identifier),
  min.freq     = 1,
  random.order = FALSE,
  scale        = c(1, 0.5)  # adjust size range
)


get_taxonomic_class <- function(sci_name) {
  res <- tryCatch(
    name_backbone(name = sci_name),
    error = function(e) NULL
  )
  
  if (is.null(res)) return(NA_character_)
  if (!"class" %in% names(res)) return(NA_character_)
  if (is.na(res$class)) return(NA_character_)
  
  return(res$class)
}

unique_species <- unique(all_animals$taxon_canonical_name)

tax_class_lookup <- tibble(
  taxon_canonical_name = unique_species,
  class = purrr::map_chr(unique_species, get_taxonomic_class)
)

# Join back into your full data
all_animals <- all_animals %>%
  left_join(tax_class_lookup, by = "taxon_canonical_name")


ibrary(ggplot2)
library(dplyr)

all_animals %>%
  group_by(class) %>%
  summarise(
    total = n(),
    human_named = sum(is_human_name),
    proportion_human_named = human_named / total
  ) %>%
  ggplot(aes(x = reorder(class, proportion_human_named), y = proportion_human_named)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Proportion of Animals with Human Names by Taxonomic Class",
    x = "Taxonomic Class",
    y = "Proportion Human-Named"
  )


all_animals %>%
  ggplot(aes(x = class, y = creativity_score, fill = class)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Creativity Score of Animal Names by Taxonomic Class",
    x = "Class",
    y = "Creativity Score (distance from human names)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


library(ggrepel)

all_animals %>%
  filter(is_human_name) %>%
  ggplot(aes(x = total_locations, y = creativity_score, color = class)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = local_identifier), max.overlaps = 10) +
  scale_x_log10() +
  labs(
    title = "Baby Name Popularity vs. Creativity Score",
    x = "Total U.S. Births (log scale)",
    y = "Creativity Score"
  ) +
  theme_minimal()


classes <- unique(na.omit(all_animals$class))

for (cls in classes) {
  cat("\n\n---", cls, "---\n")
  names_cls <- all_animals %>%
    filter(class == cls) %>%
    pull(local_identifier)
  
  wordcloud(words = names_cls,
            freq = table(names_cls),
            min.freq = 1,
            scale = c(1, 0.5),
            main = paste("Names in", cls))
}




library(move)
library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)

# ----------------------------
# 1. LOGIN + GET STUDY LIST
# ----------------------------

login <- movebankLogin(username = "whatsinaname", password = "whatsinaname1!")

# Only studies that are:
# - Research studies
# - Have location
# - Are not test or tmp

all_studies <- getMovebank(entity_type = "study", login = login) %>%
  drop_na(main_location_lat, main_location_long) %>%
  filter(is_test == 'false', study_type == 'research',
         i_have_download_access =='true') %>%
  filter(!grepl('tmp', name))

# ----------------------------
# 2. FILTER FOR ACCESSIBLE STUDIES
# ----------------------------

# Get IDs of studies you can download
# accessible_study_ids <- getMovebankStudyList(login = login)$id

# Filter for studies you can access
# downloadable_studies <- all_studies %>%
#   filter(id %in% accessible_study_ids)

# ----------------------------
# 3. PROJECT FOR MAPPING
# ----------------------------

# World map for base
world <- ne_countries(scale = "medium", returnclass = "sf")

# Reproject study points to match world map (if needed)
# We'll keep them in lat/lon for simplicity
downloadable_studies_sf <- st_as_sf(all_studies,
                                    coords = c("main_location_long", "main_location_lat"),
                                    crs = 4326
)

# ----------------------------
# 4. PLOT
# ----------------------------

ggplot() +
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


# Next steps:
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
