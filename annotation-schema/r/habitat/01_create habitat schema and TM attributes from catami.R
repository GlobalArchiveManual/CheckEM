# This script uses the catami classification scheme to make a TransectMeasure attribute file ----
# Please forward any feedback and improvements to brooke.gibbons@uwa.edu.au or raise an issue in the ChecKEM GitHub repository
# https://github.com/GlobalArchiveManual/CheckEM

# Clear memory
rm(list = ls())

## Libraries required
library(tidyverse)
library(CheckEM)

# Read in extra caab codes -----
# This is a manually made csv
# It contains seagrass and kelp species which UWA commonly use when annotating habitat
extra <- read.csv("annotation-schema/data/raw/extra-caab-codes.csv") %>%
  CheckEM::clean_names()

# Read in catami codes ----
# Have not included codium or calerpa because they can differ in levels with the same CAAB code
catami <- read.csv("annotation-schema/data/raw/catami-caab-codes_1.4.csv") %>%
  CheckEM::clean_names() %>%
  dplyr::rename(caab_code = species_code, parent_caab = catami_parent_id) %>%
  dplyr::select(caab_code, parent_caab, catami_display_name) %>% 
  tidyr::separate(catami_display_name, into = c("level_2", "level_3", "level_4", "level_5", "level_6", "level_7", "level_8"), sep = ": ") %>%
  dplyr::mutate(level_1 = if_else(level_2 %in% c("Substrate", "Relief"), "Physical", "Biota")) %>%
  bind_rows(extra) %>%
  dplyr::mutate(qualifiers = NA) %>%
  dplyr::mutate(qualifiers = if_else(level_2 %in% c("Macroalgae") & (!level_3 == "Encrusting"), "Drift/No epiphytes/Epiphytes algae/Epiphytes other", as.character(qualifiers))) %>%
  dplyr::mutate(qualifiers = if_else(level_2 %in% c("Seagrasses"), "No epiphytes/Epiphytes algae/Epiphytes other", as.character(qualifiers))) %>%
  dplyr::mutate(qualifiers = if_else(level_3 %in% c("Corals"), "Alive/Recruit/Bleached/Dead/Recently dead", as.character(qualifiers))) %>%
  dplyr::mutate(qualifiers = if_else(level_2 %in% c("Substrate"), "Veneer/Iceberg scour/Storm damage/Urchin barren/Turf mat", as.character(qualifiers))) %>%
  dplyr::select(caab_code, parent_caab, level_1, everything()) %>% 
  dplyr::filter(!level_2 %in% c("Biota", "Physical")) %>%
  dplyr::arrange(level_1, level_2, level_3, level_4) %>%
  dplyr::mutate(caab_code = str_pad(caab_code, 8, side = c("left"), pad = "0")) %>%
  
  # Fix for relief score
  dplyr::mutate(new_level_5 =  case_when(level_2 == "Relief" & level_3 == "Flat" ~ as.character(0),
                                     level_2 == "Relief" & level_3 == "Low / moderate" & level_4 == "Low (<1m)" ~ as.character(1),
                                     level_2 == "Relief" & level_3 == "Low / moderate" & level_4 == "Moderate (1-3m)" ~ as.character(2),
                                     level_2 == "Relief" & level_3 == "High" & level_4 == "High (>3m)" ~ as.character(3),
                                     level_2 == "Relief" & level_3 == "High" & level_4 == "Wall" ~ as.character(4),
                                     level_2 == "Relief" & level_3 == "High" & level_4 == "Caves" ~ as.character(5)
                )) %>%
  dplyr::mutate(level_5 = if_else(is.na(new_level_5), level_5, new_level_5)) %>%
  dplyr::select(-c(new_level_5)) %>%
  glimpse()

# Save a csv version ----
write.csv(catami, "annotation-schema/output/habitat/schema/benthic-annotation-schema-forward-facing.csv", row.names = FALSE, na = "")

# Save a RDS version ----
saveRDS(catami, "annotation-schema/output/habitat/schema/benthic-annotation-schema-forward-facing.RDS")

# Create TransectMeasure schema
# First for the whole schema
tm <- catami %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::mutate(scientific = str_replace_all(scientific, c("NA NA" = ""))) %>%
  dplyr::filter(level_6 %in% c("NA", NA, "")) %>%
  dplyr::select(level_2, level_3, level_4, level_5, scientific, qualifiers, caab_code) %>%
  dplyr::mutate(qualifiers = if_else(is.na(qualifiers), qualifiers, paste("/", qualifiers, sep =""))) %>%
  dplyr::mutate(qualifiers = strsplit(as.character(qualifiers), split = "/")) %>% # Create a new row for every qualifier - step 1
  unnest(qualifiers) %>% # Create a new row for every qualifier - step 2
  mutate_all(na_if, "") %>%
  mutate_all(na_if, " ") %>%
  dplyr::filter(!caab_code %in% "82003000")

# TODO remove the CAAB code from these
# Split into a Biota/Substrate and Relief schema to have two caab codes.
tm.hab <- tm %>%
  dplyr::filter(!level_2 %in% c("Relief"))

tm.relief <- tm %>%
  dplyr::filter(level_2 %in% c("Relief"))

# Save the text files
write_tsv(tm.hab, "annotation-schema/output/habitat/TransectMeasure attribute files/benthic-habitat-annotation-schema-forward-facing.txt", na = "")
write_tsv(tm.relief, "annotation-schema/output/habitat/TransectMeasure attribute files/benthic-relief-annotation-schema-forward-facing.txt", na = "")
