## Code to prepare australia and global life history datasets ----
library(usethis)
library(tidyverse)
library(CheckEM)
library(googlesheets4)

# THIS IS NOT UPDATED IN THE ANNOTATION SCHEMA SCRIPT 5
# australia_life_history <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS")

# usethis::use_data(australia_life_history, overwrite = TRUE)

global_life_history <- readRDS("annotation-schema/data/tidy/global_fish.life.history.RDS")

usethis::use_data(global_life_history, overwrite = TRUE)


# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth()
2

# TODO change these to github repo for Australia LH and synonyms 
# TODO change australian.common.name to common.name
# Read in sheet from googledrive ----
aus_url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"

# Synonyms ----
lh_aus_synonyms <- read_sheet(aus_url, sheet = "synonyms_updated") %>%
  distinct() %>%
  clean_names() %>%
  dplyr::filter(!keep %in% "No") %>%
  dplyr::select(-c(comment, keep, reason, source, last_checked, region, scale)) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Dasyatis" & species %in% "kuhlii")) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Dasyatis" & species %in% "leylandi")) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Neotrygon" & species %in% "kuhlii")) %>%
  glimpse()

names(lh_aus_synonyms)

caab_aus_synonyms <- readRDS("annotation-schema/data/staging/caab_synonyms.RDS") %>%
  dplyr::rename(family_correct = family,
                genus_correct = genus, 
                species_correct = species,
                family = synonym.family,
                genus = synonym.genus,
                species = synonym.species) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Dasyatis" & species %in% "kuhlii")) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Dasyatis" & species %in% "leylandi")) %>%
  dplyr::filter(!(family %in% "Dasyatidae" & genus %in% "Neotrygon" & species %in% "kuhlii")) %>%
  dplyr::select(family, genus, species, family_correct, genus_correct, species_correct)

aus_synonyms <- bind_rows(lh_aus_synonyms, caab_aus_synonyms) %>%
  distinct()

test <- aus_synonyms %>%
  group_by(family, genus, species) %>%
  summarise(n = n())

usethis::use_data(aus_synonyms, overwrite = TRUE)

