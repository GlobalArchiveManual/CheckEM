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

se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"
lh <- read_sheet(url, sheet = "australia.life.history") %>%
  clean_names() %>%
  dplyr::mutate(maxlegal_vic = str_replace_all(maxlegal_vic, "TRUE", "550"))

# Save a copy locally ----
saveRDS(lh, "annotation-schema/data/raw/australia_original-life-history.RDS")

# Read in synonyms ----
synonyms <- read_sheet(url, sheet = "synonyms_updated") %>% distinct()
saveRDS(synonyms, "annotation-schema/data/raw/australia_fish_original-synonyms.RDS")

# Extra maximum sizes
max_url <- "https://docs.google.com/spreadsheets/d/1H7EXoTlpeg48LrNVszIa8irwIAh_SqYBNqMYI1WVce8/edit?resourcekey#gid=1554913827"

new_max_sizes <- read_sheet(max_url, sheet = "Responses") %>% distinct() %>%
  clean_names() %>%
  dplyr::rename(source = please_provide_a_link_to_reference_information_e_g_website_or_paper) %>%
  dplyr::select(family, genus, species, new_maximum_length_in_cm, type_of_length_measure, source) %>% 
  dplyr::group_by(family, genus, species) %>%
  slice(which.max(new_maximum_length_in_cm)) %>%
  dplyr::mutate(fishbase_scientific = paste(genus, species, sep = " ")) %>%
  ungroup() %>%
  glimpse()
 
# CONVERT THESE INTO FL WHERE POSSIBLE
library(rfishbase)

# Extract the species ----
checkem_length_species <- unique(new_max_sizes$fishbase_scientific)

# Extract length-length relationships to convert non-FL measures
ll_eqs <- length_length(checkem_length_species) %>%
  clean_names() %>%
  dplyr::rename(unknown = length1, known = length2) %>%
  dplyr::group_by(species, unknown, known) %>%
  summarise(a_ll = mean(a, na.rm = T),
            b_ll = mean(b, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(known %in% c("TL", "FL", "SL"),
                unknown %in% c("TL", "FL", "SL")) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  glimpse()


################################################################################

# Convert the length of maturity data into FL where possible ----
new_max_sizes_conv <- new_max_sizes %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  dplyr::mutate(new_max_length = case_when(type_of_length_measure %in% "FL" ~ new_maximum_length_in_cm,
                                           type_of_length_measure == known & unknown %in% "FL" ~ (new_maximum_length_in_cm * b_ll) + a_ll,
                                           type_of_length_measure == unknown & known %in% "FL" ~ (new_maximum_length_in_cm - a_ll)/b_ll)) %>%
  # dplyr::mutate(new_max_length = case_when(type_of_length_measure %in% "FL" ~ new_maximum_length_in_cm,
  #                                          type_of_length_measure %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ (new_maximum_length_in_cm * b_ll) + a_ll,
  #                                          type_of_length_measure %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ (new_maximum_length_in_cm - a_ll)/b_ll),
  #               conversion_type = case_when(type_of_length_measure %in% "FL" ~ "no-conversion",
  #                                           type_of_length_measure %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ "regular-eq",
  #                                           type_of_length_measure %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ "reversed-eq")) %>%
  dplyr::filter(!is.na(new_max_length)) %>%
  dplyr::select(fishbase_scientific, new_max_length, conversion_type) %>%
  dplyr::mutate(type_of_length_measure = "FL") %>%
  glimpse()

ts_species <- new_max_sizes %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  group_by(fishbase_scientific) %>%
  summarise(has_SL_to_TL = any(type_of_length_measure == "SL" & known == "SL" & unknown == "TL"),
            has_TL_to_FL = any(type_of_length_measure == "SL" & known == "TL" & unknown == "FL")) %>%
  dplyr::filter(has_SL_to_TL & has_TL_to_FL) %>%
  pull(fishbase_scientific)

maturity_ts <- maturity %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  dplyr::filter(fishbase_scientific %in% ts_species) %>% # Only species with possible two-step conversions
  # Convert SL to TL
  dplyr::mutate(lm_tl = case_when(type1 %in% "SL" & 
                                    unknown %in% "TL" & 
                                    known %in% "SL" ~ 
                                    (fb_length_at_maturity_cm * b_ll) + a_ll)) %>%
  dplyr::filter((type1 %in% "SL" & unknown %in% "TL" & known %in% "SL") | 
                  (type1 %in% "SL" & unknown %in% "FL" & known %in% "TL")) %>%
  group_by(fishbase_scientific) %>%
  dplyr::mutate(lm_tl = coalesce(lm_tl, lm_tl[!is.na(lm_tl)][1])) %>%
  dplyr::filter(unknown %in% "FL" & known %in% "TL") %>%
  # Convert TL to FL
  dplyr::mutate(lm_fl = (lm_tl * b_ll) + a_ll) %>%
  dplyr::select(fishbase_scientific, speccode, lm_fl) %>%
  dplyr::rename(fb_length_at_maturity_cm = lm_fl) %>%
  dplyr::mutate(conversion_type = "two-step") %>%
  glimpse()

# Join the straight converted and two-step conversion species
maturity_final <- bind_rows(maturity_conv, maturity_ts) %>%
  distinct() %>%
  dplyr::mutate(ranking = case_when(conversion_type %in% "no-conversion" ~ 1,
                                    conversion_type %in% "regular-eq" ~ 2,
                                    conversion_type %in% "reversed-eq" ~ 3,
                                    conversion_type %in% "two-step" ~ 4)) %>%
  arrange(fishbase_scientific, ranking) %>%
  group_by(fishbase_scientific) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::mutate(fb_length_at_maturity_source = case_when(conversion_type %in% "no-conversion" ~ "Fishbase",
                                                         conversion_type %in% "regular-eq" ~ "Fishbase: Converted to TL using length-length equation",
                                                         conversion_type %in% "reversed-eq" ~ "Fishbase: Converted to TL using inverse length-length relationship",
                                                         conversion_type %in% "two-step" ~ "FishBase: Converted to FL using length-length equation")) %>%
  dplyr::select(-c(ranking, conversion_type)) %>%
  glimpse()

################################################################################

# TODO add fishes of australia max sizes and depth limits
foa_max_sizes <- readRDS("annotation-schema/data/staging/fishes-of-australia_maximum-sizes.RDS") %>%
  clean_names() %>%
  dplyr::rename(foa_min_depth = min_depth, foa_max_depth = max_depth, type_of_length_measure = length_type) %>%
  dplyr::select(caab, new_maximum_length_in_cm, foa_min_depth, foa_max_depth, type_of_length_measure) %>%
  dplyr::rename(caab_code = caab)

# Extra marine regions ----
# Australian Faunal directory ----
afd_extra_aus <- read_csv("annotation-schema/data/staging/australian-fanual-directory_distributions_extra_aus.csv") %>%
  dplyr::mutate(caab_code = as.character(caab_code))

afd_extra_imcra <- read_csv("annotation-schema/data/staging/australian-fanual-directory_distributions_extra_imcras.csv") %>%
  dplyr::mutate(caab_code = as.character(caab_code))

# Google sheet ----
extra_marine_regions <- "https://docs.google.com/spreadsheets/d/10D3s-pB-GZJ6xcp93wOx2taJCa5HXqmljcVscaJQmYA/edit?resourcekey#gid=2055010068"

add_regions <- read_sheet(extra_marine_regions, sheet = "Form responses 1") %>% 
  distinct() %>%
  clean_names() %>%
  dplyr::rename(family = family_please_double_check_spelling) %>%
  dplyr::rename(genus = genus_please_double_check_spelling) %>%
  dplyr::rename(species = species_please_double_check_spelling) %>%
  dplyr::mutate(marine_region = strsplit(as.character(marine_regions_the_species_occurs_in_see_regions_here), split = ", ")) %>% # changed from "/" for old LH
  tidyr::unnest(marine_region) %>%
  dplyr::select(family, genus, species, marine_region) %>%
  # dplyr::mutate(marine_region = case_when(marine_region %in% "North-west" ~ "NW",
  #                                         marine_region %in% "North" ~ "N",
  #                                         marine_region %in% "Coral Sea" ~ "CS",
  #                                         # marine_region %in% "Temperate East" ~ "TE",
  #                                         marine_region %in% "South-east" ~ "SE",
  #                                         marine_region %in% "South-west" ~ "SW")) %>%
  rename(aus_region = marine_region)

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

# Other data to read in from script 2, 3 & 4
caab_regions <- readRDS("annotation-schema/data/staging/australia_fish_caab-with-regions.RDS") %>%
  dplyr::filter(!is.na(caab_code)) %>%
  dplyr::mutate(caab_code = as.character(caab_code))%>%
  distinct()

test <- caab_regions %>%
  dplyr::group_by(caab_code, family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_regions %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_regions %>%
  dplyr::group_by(family,genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

fishbase <- readRDS("annotation-schema/data/staging/australia_fish_fishbase-information-and-iucn-category.RDS") %>%
  dplyr::filter(!(caab_scientific %in% "Epigonus macrops" & speccode %in% "14365"))%>%
  dplyr::filter(!(caab_scientific %in% "Protosalanx chinensis" & speccode %in% "12239"))%>%
  
  dplyr::filter(!(caab_code %in% "37311059" & is.na(speccode)))%>%
  dplyr::filter(!(caab_code %in% "37361005" & is.na(speccode)))%>%
  
  # dplyr::filter(!(caab_scientific %in% "Protosalanx chinensis" & speccode %in% "12239"))%>%
  # dplyr::filter(!(caab_scientific %in% "Protosalanx chinensis" & caab_code %in% "37361005"))%>%
  dplyr::mutate(caab_code = as.character(caab_code)) %>%
  dplyr::mutate(caab_scientific = if_else((caab_code %in% "37361005"), "Microcanthus joyceae", caab_scientific)) %>%
  distinct() %>%
  dplyr::mutate(fb_trophic_level = as.numeric(fb_trophic_level)) %>%
  dplyr::filter(!caab_scientific %in% "Rhina ancylostoma") %>%
  dplyr::mutate(fb_trophic_level_se = as.numeric(fb_trophic_level_se))

test <- fishbase %>%
  dplyr::group_by(caab_scientific) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- fishbase %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

caab_scraped <- readRDS("annotation-schema/data/staging/australia_fish_caab-codes_common-names.RDS") %>%
  dplyr::mutate(family = str_replace_all(.$family, "[^[:alnum:]]", "")) %>%
  dplyr::mutate(species = str_replace_all(.$species, "cffilamentosa", "filamentosa")) %>%
  
  dplyr::mutate(scraped_name = paste(family, genus, species)) %>%
  dplyr::rename(scraped_common_name = common_name,
                scraped_family = family,
                scraped_genus = genus, 
                scraped_species = species) %>%
  dplyr::select(caab_code, scraped_name, scraped_family, scraped_genus, scraped_species, scraped_common_name) %>%
  # dplyr::select(!common_name) %>%
  distinct()

test <- caab_scraped %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_scraped %>%
  dplyr::group_by(scraped_family, scraped_genus, scraped_species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

caab_combined <- full_join(caab_regions, caab_scraped) %>%
  dplyr::mutate(name = paste(family, genus, species)) %>%
  # dplyr::filter(!name %in% scraped.name) %>% # for testing
  # dplyr::filter(!is.na(scraped.name)) %>%# for testing 
  dplyr::mutate(scraped_family = if_else(is.na(scraped_family), family, scraped_family)) %>%
  dplyr::mutate(scraped_genus = if_else(is.na(scraped_genus), genus, scraped_genus)) %>%
  dplyr::mutate(scraped_species = if_else(is.na(scraped_species), species, scraped_species)) %>%
  
  dplyr::mutate(common_name = if_else(is.na(common_name), scraped_common_name, common_name)) %>%
  
  dplyr::mutate(family = if_else(name %in% c(scraped_name), family, scraped_family)) %>%
  dplyr::mutate(genus = if_else(name %in% scraped_name, genus, scraped_genus)) %>%
  dplyr::mutate(species = if_else(name %in% scraped_name, species, scraped_species)) %>%
  dplyr::select(-c(scraped_name, scraped_family, scraped_genus, scraped_species, name, scraped_common_name)) %>%
  dplyr::mutate(common_name = str_replace_all(.$common_name, "\\[|\\]", "")) %>%
  dplyr::filter(!is.na(species)) %>%
  clean_names() %>%
  dplyr::filter(!caab_code %in% c("37246020Taxasupercededby37246019", "37118006", "37441924", "37311954", "37287949", "37096000", "37018915", "37337902")) %>%
  distinct()

test <- caab_combined %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_combined %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

genus_trophic_maturity <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(fb_trophic_level = mean(fb_trophic_level, na.rm = TRUE),
                   # TODO add in SE
                   # fb_trophic_level_se = se(fb_trophic_level), 
                   fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE)) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(species = "spp")

family_trophic_maturity <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(fb_trophic_level = mean(fb_trophic_level, na.rm = TRUE), 
                   fb_trophic_level_se = se(fb_trophic_level),
                   fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(species = "spp", genus = "Unknown")

spp_trophic_maturity <- bind_rows(genus_trophic_maturity, family_trophic_maturity) %>%
  dplyr::rename(spp_fb_trophic_level = fb_trophic_level,
               spp_fb_length_at_maturity_cm = fb_length_at_maturity_cm)

# fishbase <- bind_rows(fishbase, spp_trophic_maturity)

iucn_animals <- readRDS("annotation-schema/data/staging/iucn.RDS") %>%
  dplyr::mutate(iucn_ranking = case_when(
    category %in% "DD" ~ "Data Deficient",
    category %in% "LC" ~ "Least Concern",
    category %in% "NT" ~ "Near Threatened",
    category %in% "VU" ~ "Vulnerable",
    category %in% "EN" ~ "Endangered",
    category %in% "CR" ~ "Critically Endangered",
    category %in% "EW" ~ "Extinct in the Wild",
    category %in% "EX" ~ "Extinct"
  )) %>%
  dplyr::select(-c(category)) %>%
  glimpse()

animals <- readRDS("annotation-schema/data/staging/australia_animals_caab-code-and-distributions.RDS") %>%
  dplyr::mutate(caab = as.character(caab)) %>%
  left_join(iucn_animals) %>%
  
  dplyr::mutate(australian_source = "CAAB",
                global_source = "WoRMS"#,
                #local_source = "Not Available"
                ) %>%
  
  dplyr::mutate(australian_common_name = str_replace_all(.$australian_common_name, "\\[|\\]", "")) %>%
  dplyr::rename(caab_code = caab) %>%
  glimpse()

taxonomy <- bind_rows(animals, caab_combined) %>%
  distinct(class, order, family)

test <- taxonomy %>%
  distinct(class, order, family) %>%
  filter(is.na(order))

spps <- readRDS("annotation-schema/data/staging/australia_fish_caab-codes_spps.RDS") %>%
  dplyr::filter(!caab %in% c(unique(animals$caab_code))) %>%
  dplyr::filter(!caab %in% c(unique(caab_combined$caab_code))) %>%
  dplyr::filter(!caab %in% c(unique(fishbase$caab_code))) %>%
  dplyr::mutate(australian_source = "CAAB",
                global_source = "WoRMS"#,
                #local_source = "Not Available"
  ) %>%
  dplyr::rename(caab_code = caab)

blank_class <- spps %>%
  filter(is.na(class)) %>%
  distinct(family)

test <- animals %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- animals %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- spps %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- spps %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

# Make it simpler
australia_life_history <- caab_combined %>%
  dplyr::left_join(fishbase) %>%
  dplyr::rename(#caab = caab_code, # TODO make caab_code consistent throughout all scripts
                australian_common_name = common_name,
                fb_code = speccode,
                fb_length_weight_measure = length_measure, #TODO make this happen in fishbase script
                fb_a = a, # TODO also rename allthese in fishbase script
                fb_b = b,
                fb_a_ll = a_ll,
                fb_b_ll = b_ll,
                fb_length_max_type = fb_l_type_max,
                fb_length_weight_source = source_level,
                fb_length_weight_measure = length_measure) %>%
  
  dplyr::left_join(keep) %>%
  
  dplyr::mutate(australian_source = "CAAB",
                global_source = "FishBase",
                local_source = "Harvey et al 2020") %>%
  
  dplyr::mutate(scientific_name = paste(genus, species)) %>%

  dplyr::mutate(global_region = "Australia") %>%
  
  dplyr::select(c(australian_source,
                caab_code,
                class,
                order,
                family,
                genus,
                species,
                scientific_name,
                australian_common_name, # TODO need to add in 1st script or use the scraping
                # marine_region,
                aus_region,
                imcra_region,
                
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
                
                fb_trophic_level,
                fb_trophic_level_se,
                fb_trophic_level_source,
                
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
  bind_rows(animals) %>%
  bind_rows(spps) %>%
  dplyr::mutate(family = if_else(genus %in% "Heteroscarus", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Olisthops", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Siphonognathus", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Neoodax", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Haletta", "Labridae", family)) %>%
  
  
  dplyr::mutate(order = if_else(family %in% "Siphonariidae", "Siphonariida", order)) %>%
  
  dplyr::left_join(new_max_sizes) %>%
  dplyr::mutate(fb_length_max = if_else(!is.na(new_maximum_length_in_cm), new_maximum_length_in_cm, fb_length_max)) %>%
  dplyr::mutate(length_max_source = if_else(!is.na(new_maximum_length_in_cm), source, "Fishbase")) %>% #BRUV expert
  dplyr::mutate(fb_length_max_type = if_else(!is.na(new_maximum_length_in_cm), type_of_length_measure, fb_length_max_type)) %>%
  dplyr::select(-c(new_maximum_length_in_cm, type_of_length_measure)) %>%
  
  # dplyr::filter(species %in% "maculatus") %>%
  # dplyr::filter(genus %in% "Prionurus") %>%

  dplyr::left_join(foa_max_sizes) %>%
  tidyr::replace_na(list(new_maximum_length_in_cm = 0)) %>%
  dplyr::mutate(length_max_source = if_else(new_maximum_length_in_cm > fb_length_max, "Fishes of Australia", length_max_source)) %>%
  dplyr::mutate(fb_length_max = if_else(new_maximum_length_in_cm > fb_length_max, new_maximum_length_in_cm, fb_length_max)) %>%
  dplyr::mutate(fb_length_max_type = if_else((new_maximum_length_in_cm > fb_length_max), type_of_length_measure, fb_length_max_type)) %>%
  
  dplyr::select(-c(new_maximum_length_in_cm, type_of_length_measure)) %>%
  filter(!grepl("cf", species)) %>%
  filter(!grepl("sp\\.", species)) %>%
  
  dplyr::left_join(spp_trophic_maturity) %>%
  dplyr::mutate(fb_trophic_level = if_else(is.na(fb_trophic_level), spp_fb_trophic_level, fb_trophic_level)) %>%
  dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(fb_length_at_maturity_cm), spp_fb_length_at_maturity_cm, fb_length_at_maturity_cm)) %>%
  dplyr::select(-c(spp_fb_trophic_level, )) %>%
  
  dplyr::mutate(australian_source = "CAAB") %>%
  
  dplyr::select(# CAAB info
                australian_source,
                caab_code,
                
                # Taxanomic ranks
                # kingdom,
                # phylum,
                class,
                order,
                family,
                subfamily,
                genus,
                species,
                scientific_name,
                australian_common_name,
                
                # Regions
                global_region,
                # marine_region,
                aus_region,
                imcra_region,
                
                # Fishbase info
                global_source,
                
                fb_code,
                fb_length_at_maturity_cm,
                fb_length_weight_measure,
                fb_a,
                fb_b,
                fb_a_ll,
                fb_b_ll,
                fb_length_weight_source,
                fb_vulnerability,
                fb_countries,
                fb_status,
                
                # Maximum lenghs
                fb_length_max, # NEED TO RENAME
                fb_length_max_type,# NEED TO RENAME
                length_max_source,
                
                fb_trophic_level,
                fb_trophic_level_se,
                fb_trophic_level_source,
                
                foa_min_depth,
                foa_max_depth,
                
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
                
                ) %>%
  dplyr::rename(length_max_cm = fb_length_max, length_max_type = fb_length_max_type)

names(australia_life_history)
unique(australia_life_history$length_max_source)
unique(australia_life_history$length_max_cm) %>% sort()

expanded <- australia_life_history %>%
  dplyr::mutate(aus_region = strsplit(as.character(aus_region), split = ", ")) %>% 
  tidyr::unnest(aus_region) %>%
  glimpse()

unique(expanded$aus_region)

missing_regions <- anti_join(add_regions, expanded) %>%
  dplyr::rename(region_to_add = aus_region) %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(region_to_add = toString(region_to_add))

australia_life_history <- dplyr::left_join(australia_life_history, missing_regions) %>%
  mutate(aus_region = if_else(is.na(aus_region), region_to_add, paste(aus_region, region_to_add, sep = ", "))) %>%
  dplyr::select(-region_to_add) %>%
  dplyr::mutate(aus_region = str_replace_all(aus_region, "\\, NA", "")) %>%
  dplyr::filter(!caab_code %in% c("37280000", "37384000", "37385000")) %>%
  dplyr::rename(marine_region = aus_region)

number.with.distributions <- australia_life_history %>% 
  filter(!is.na(marine_region))

nrow(number.with.distributions)/nrow(australia_life_history) * 100 
# 49.97855% with distribution info available from worms package

number.with.distributions <- australia_life_history %>% 
  filter(!is.na(imcra_region))

nrow(number.with.distributions)/nrow(australia_life_history) * 100 
# 50.44736% with distribution info available from worms package

################# TESTING ADDING AFD MARINE REGIONS
expanded <- australia_life_history %>%
  dplyr::mutate(marine_region = strsplit(as.character(marine_region), split = ", ")) %>% 
  tidyr::unnest(marine_region) %>%
  glimpse()

unique(expanded$marine_region)

additional_afd_marine_regions <- anti_join(afd_extra_aus, expanded) %>%
  dplyr::rename(region_to_add = marine_region) %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(region_to_add = toString(region_to_add))

australia_life_history <- dplyr::left_join(australia_life_history, additional_afd_marine_regions) %>%
  mutate(marine_region = if_else(is.na(marine_region), region_to_add, paste(marine_region, region_to_add, sep = ", "))) %>%
  dplyr::select(-region_to_add) %>%
  dplyr::mutate(marine_region = str_replace_all(marine_region, "\\, NA", ""))

################


################# TESTING ADDING AFD IMCRA REGIONS
expanded <- australia_life_history %>%
  dplyr::mutate(imcra_region = strsplit(as.character(imcra_region), split = ", ")) %>% 
  tidyr::unnest(imcra_region) %>%
  glimpse()

unique(expanded$imcra_region)

additional_afd_imcra_regions <- anti_join(afd_extra_imcra, expanded) %>%
  dplyr::rename(region_to_add = imcra_region) %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(region_to_add = toString(region_to_add))

australia_life_history <- dplyr::left_join(australia_life_history, additional_afd_imcra_regions) %>%
  mutate(imcra_region = if_else(is.na(imcra_region), region_to_add, paste(imcra_region, region_to_add, sep = ", "))) %>%
  dplyr::select(-region_to_add) %>%
  dplyr::mutate(imcra_region = str_replace_all(imcra_region, "\\, NA", ""))

################
number.with.distributions <- australia_life_history %>% 
  filter(!is.na(marine_region))
nrow(number.with.distributions)/nrow(australia_life_history) * 100 
# 50.88086% with distribution info available from worms package

number.with.distributions <- australia_life_history %>% 
  filter(!is.na(imcra_region))
nrow(number.with.distributions)/nrow(australia_life_history) * 100 
# 51.64028% with distribution info available from worms package
  
unique(australia_life_history$marine_region)
unique(australia_life_history$imcra_region)

test <- australia_life_history %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- australia_life_history %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- australia_life_history %>%
  dplyr::filter(is.na(caab_code))

test <- australia_life_history %>%
  dplyr::filter(is.na(global_region))

test <- australia_life_history %>%
  dplyr::filter(is.na(family))

test <- australia_life_history %>%
  dplyr::filter(is.na(genus))

test <- australia_life_history %>%
  dplyr::filter(is.na(species))

test <- australia_life_history %>%
  dplyr::filter(is.na(class))

unique(test$family)

australia_life_history <- australia_life_history %>%
  dplyr::filter(!is.na(class))

test <- australia_life_history %>%
  dplyr::filter(is.na(order))

names(australia_life_history)
unique(australia_life_history$australian_source) # ok

unique(australia_life_history$global_source) # ok

unique(australia_life_history$local_source)

test <- australia_life_history %>%
  filter(!is.na(global_source))

unique(australia_life_history$caab_code)
unique(australia_life_history$class)
unique(australia_life_history$order)
unique(australia_life_history$family)
unique(australia_life_history$genus)
unique(australia_life_history$species)

test <- australia_life_history %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- australia_life_history %>%
  dplyr::group_by(genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)


saveRDS(australia_life_history, "annotation-schema/output/fish/schema/australia_life-history.RDS") # To share with people
write.csv(australia_life_history, "annotation-schema/output/fish/schema/australia_life-history.csv", row.names = FALSE) # To share with people
saveRDS(australia_life_history, "inst/shiny/CheckEM/data/australia_life-history.RDS") # to update shiny app

# For CheckEM package - Save as an rda to use as package data
usethis::use_data(australia_life_history, overwrite = TRUE)

