## Code to prepare `maturity` dataset ----
# This dataset contains length of maturity data for common West Australian recreationally and commercially targeted species
# The vast majority of this data has been obtained from Department of Fisheries public reports
# See here - https://www.fish.wa.gov.au/Documents/management_papers/fmp280.pdf

library(googlesheets4)
library(usethis)
library(tidyverse)
library(CheckEM)
library(rfishbase)

code_crosswalk_codes <- readRDS("annotation-schema/data/staging/code-crosswalk-codes.RDS")

url <- "https://docs.google.com/spreadsheets/d/176genWqd_pc3NDVQtP2CmfMh66Ui6uwWrLoJU1Ny_qw/edit?usp=sharing"

maturity <- googlesheets4::read_sheet(url, sheet = "fisheries Lm") %>%
  CheckEM::clean_names() %>%
  dplyr::filter(!species %in% "spp") %>%
  dplyr::select(family, genus, species, sex, l50_mm, measurement_type, marine_region,
                hermaphrodite, source) %>%
  dplyr::rename(marine_region_lm = marine_region) %>%
  dplyr::mutate(caab_scientific = paste(genus, species, sep = " ")) %>%
  left_join(code_crosswalk_codes) %>%
  glimpse()

species_list <- maturity$fishbase_scientific %>%
  unique()

ll_eqs <- length_length(species_list) %>%
  clean_names() %>%
  dplyr::rename(unknown = length1, known = length2) %>%
  dplyr::filter(!unknown %in% "SL", !known %in% "SL") %>% # Remove standard length
  dplyr::group_by(species, unknown) %>%
  summarise(a_ll = mean(a, na.rm = T),
            b_ll = mean(b, na.rm = T)) %>%
  group_by(species) %>%
  mutate(has_FL = any(unknown == "FL"),
         has_TL = any(unknown == "TL")) %>%
  filter(
    (has_FL & has_TL & unknown == "FL") |           # if both exist, keep FL
      (!(has_FL & has_TL))                          # if only one exists, keep it
  ) %>%
  ungroup() %>%
  select(-has_FL, -has_TL) %>%
  dplyr::mutate(eq_type = if_else(unknown %in% "FL", "standard", "reversed")) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  glimpse()

# Convert length of maturity to Fork Length or filter where not possible
maturity_conv <- maturity %>%
  left_join(ll_eqs) %>%
  dplyr::mutate(a_ll = case_when(fishbase_scientific %in% "Choerodon rubescens" ~ 0,
                                 fishbase_scientific %in% "Cnidoglanis macrocephalus" ~ 0,
                                 fishbase_scientific %in% "Platycephalus endrachtensis" ~ 0,
                                 fishbase_scientific %in% "Platycephalus speculator" ~ 0,
                                 fishbase_scientific %in% "Argyrosomus japonicus" ~ 0,
                                 .default = a_ll),
                b_ll = case_when(fishbase_scientific %in% "Choerodon rubescens" ~ 1,
                                 fishbase_scientific %in% "Cnidoglanis macrocephalus" ~ 1,
                                 fishbase_scientific %in% "Platycephalus endrachtensis" ~ 1,
                                 fishbase_scientific %in% "Platycephalus speculator" ~ 1,
                                 fishbase_scientific %in% "Argyrosomus japonicus" ~ 1,
                                 .default = b_ll)) %>%
  dplyr::mutate(adjusted_l50 = if_else(eq_type %in% "standard",
                                       (l50_mm * b_ll) + a_ll,
                                       (l50_mm - a_ll)/b_ll)) %>%
  dplyr::filter(!is.na(adjusted_l50)) %>%
  dplyr::select(family, genus, species, sex, marine_region_lm, hermaphrodite,
                source, adjusted_l50) %>%
  dplyr::rename(l50_mm = adjusted_l50,
                marine_region = marine_region_lm) %>%
  dplyr::mutate(measurement_type = "FL") %>% # We have now converted these or removed ones not able to be converted
  glimpse()

usethis::use_data(maturity, overwrite = TRUE)
