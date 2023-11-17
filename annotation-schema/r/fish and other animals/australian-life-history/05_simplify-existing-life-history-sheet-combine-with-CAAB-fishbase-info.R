# This script uses the RLS information and targeted information from the old life history sheet and combine
# it with the information scraped from FishBase, IUCN and CAAB

# load libraries required
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(CheckEM)

# Set authorisation info for googlesheets
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"
lh <- read_sheet(url) %>%
  clean_names()

# Save a copy locally ----
saveRDS(lh, "annotation-schema/data/raw/australia_original-life-history.RDS")

# Read in synonyms ----
synonyms <- read_sheet(url, sheet = "synonyms_updated") %>% distinct()
saveRDS(synonyms, "annotation-schema/data/raw/australia_fish_original-synonyms.RDS")

# Get columns to keep from original life history
names(lh)

keep <- lh %>%
  dplyr::select(family, genus, species, 
                
                rls_trophic_level,
                rls_trophic_breadth,
                rls_trophic_group,
                rls_water_column,
                rls_substrate_type,
                rls_complexity,
                rls_night_day,
                rls_gregariousness,
                rls_thermal_niche,
                rls_vulnerability,
                
                fishing_mortality,
                fishing_type,
                fishing_intensity,
                
                minlegal_nt,
                maxlegal_nt,
                bag_nt,
                
                minlegal_wa,
                maxlegal_wa,
                bag_wa,
                
                minlegal_qld,
                maxlegal_qld,
                bag_qld,
                
                minlegal_nsw,
                maxlegal_nsw,
                bag_nsw,
                
                minlegal_vic,
                maxlegal_vic,
                bag_vic,
                
                minlegal_sa,
                maxlegal_sa,
                bag_sa,
                
                minlegal_tas,
                maxlegal_tas,
                bag_tas,
                
                epbc_threat_status) %>%
  
  dplyr::rename(min_legal_nt = minlegal_nt,
                max_legal_nt = maxlegal_nt,
                min_legal_wa = minlegal_wa,
                max_legal_wa = maxlegal_wa,
                min_legal_qld = minlegal_qld,
                max_legal_qld = maxlegal_qld,
                min_legal_nsw = minlegal_nsw,
                max_legal_nsw = maxlegal_nsw,
                min_legal_vic = minlegal_vic,
                max_legal_vic = maxlegal_vic,
                min_legal_sa = minlegal_sa,
                max_legal_sa = maxlegal_sa,
                min_legal_tas = minlegal_tas,
                max_legal_tas = maxlegal_tas)

# Other data to read in from script 1 & 
caab_regions <- readRDS("annotation-schema/data/staging/australia_fish_caab-with-regions.RDS") %>%
  dplyr::filter(!is.na(caab_code)) %>%
  dplyr::mutate(caab_code = as.character(caab_code))

fishbase <- readRDS("annotation-schema/data/staging/australia_fish_fishbase-information-and-iucn-category.RDS") %>%
  dplyr::filter(!(caab_scientific %in% "Epigonus macrops" & speccode %in% "14365"))%>%
  dplyr::mutate(caab_code = as.character(caab_code)) 

caab_scraped <- readRDS("annotation-schema/data/staging/australia_fish_caab-codes_common-names.RDS") %>%
  dplyr::mutate(family = str_replace_all(.$family, "[^[:alnum:]]", "")) %>%
  dplyr::mutate(species = str_replace_all(.$species, "cffilamentosa", "filamentosa")) %>%
  
  dplyr::mutate(scraped_name = paste(family, genus, species)) %>%
  dplyr::rename(scraped_family = family,
                scraped_genus = genus, 
                scraped_species = species) %>%
  dplyr::select(caab_code, scraped_name, scraped_family, scraped_genus, scraped_species, common_name) 

caab_combined <- full_join(caab_regions, caab_scraped) %>%
  dplyr::mutate(name = paste(family, genus, species)) %>%
  # dplyr::filter(!name %in% scraped.name) %>% # for testing
  # dplyr::filter(!is.na(scraped.name)) %>%# for testing 
  dplyr::mutate(scraped_family = if_else(is.na(scraped_family), family, scraped_family)) %>%
  dplyr::mutate(scraped_genus = if_else(is.na(scraped_genus), genus, scraped_genus)) %>%
  dplyr::mutate(scraped_species = if_else(is.na(scraped_species), species, scraped_species)) %>%
  
  dplyr::mutate(family = if_else(name %in% c(scraped_name), family, scraped_family)) %>%
  dplyr::mutate(genus = if_else(name %in% scraped_name, genus, scraped_genus)) %>%
  dplyr::mutate(species = if_else(name %in% scraped_name, species, scraped_species)) %>%
  dplyr::select(-c(scraped_name, scraped_family, scraped_genus, scraped_species, name)) %>%
  dplyr::mutate(common_name = str_replace_all(.$common_name, "\\[|\\]", "")) %>%
  dplyr::filter(!is.na(species))

iucn_all <- readRDS("annotation-schema/data/staging/australia_fish_iucn-categories.RDS") %>%
  dplyr::rename(iucn_ranking = category) %>%
  dplyr::mutate(iucn_ranking = case_when(
    iucn_ranking %in% "DD" ~ "Data Deficient",
    iucn_ranking %in% "LC" ~ "Least Concern",
    iucn_ranking %in% "NT" ~ "Near Threatened",
    iucn_ranking %in% "VU" ~ "Vulnerable",
    iucn_ranking %in% "EN" ~ "Endangered",
    iucn_ranking %in% "CR" ~ "Critically Endangered",
    iucn_ranking %in% "EW" ~ "Extinct in the Wild",
    iucn_ranking %in% "EX" ~ "Extinct"
  )) %>%
  glimpse()

animals <- readRDS("annotation-schema/data/staging/australia_animals_caab-code-and-distributions.RDS") %>%
  dplyr::mutate(caab = as.character(caab)) %>%
  left_join(iucn_all) %>%
  glimpse()

# Make it simpler
australia_life_history <- caab_combined %>%
  dplyr::left_join(fishbase) %>%
  dplyr::rename(caab = caab_code, #TODO make caab_code consistent throughout all scripts
                australian_common_name = common_name,
                fb_code = speccode,
                fb_length_weight_measure = length_measure, #TODO make this happen in fishbase script
                fb_a = a, # TODO also rename allthese in fishbase script
                fb_b = b,
                fb_a_ll = aLL,
                fb_b_ll = bLL,
                fb_length_weight_source = Source_Level) %>%
  
  dplyr::left_join(keep) %>%
  
  dplyr::mutate(australian_source = "CAAB",
                global_source = "FishBase",
                local_source = "Harvey et al 2020") %>%
  
  dplyr::mutate(scientific_name = paste(genus, species)) %>%

  dplyr::mutate(global_region = "Australia") %>%
  
  dplyr::select(c(australian_source,
                caab,
                class,
                order,
                family,
                genus,
                species,
                scientific_name,
                australian_common_name, # TODO need to add in 1st script or use the scraping
                marine_region,
                
                global_source,
                fb_code,
                fb_length_at_maturity_cm,
                # TODO need to add in 1st script
                subfamily,
                global_region,
                
                fb_length_weight_measure,
                fb_a,
                fb_b,
                fb_a_ll,
                fb_b_ll,
                fb_length_weight_source,
                
                fb_vulnerability,
                fb_countries,
                fb_status,
                fb_length_max,
                fb_length_max_type,
                rls_trophic_group,
                rls_water_column,
                rls_substrate_type,
                rls_thermal_niche,
                
                local_source,
                epbc_threat_status,
                iucn_ranking,
                
                fishing_mortality,
                fishing_type,
                min_legal_nt,
                max_legal_nt,
                min_legal_wa,
                max_legal_wa,
                min_legal_qld,
                max_legal_qld,
                min_legal_nsw,
                max_legal_nsw,
                min_legal_vic,
                max_legal_vic,
                min_legal_sa,
                max_legal_sa,
                min_legal_tas,
                max_legal_tas
                )) %>%
  bind_rows(animals)

saveRDS(australia_life_history, "annotation-schema/output/fish/schema/australia_life-history.RDS") # To share with people
saveRDS(australia_life_history, "inst/shiny/CheckEM/data/australia_life-history.RDS") # to update shiny app

# For CheckEM package - Save as an rda to use as package data
usethis::use_data(australia_life_history)