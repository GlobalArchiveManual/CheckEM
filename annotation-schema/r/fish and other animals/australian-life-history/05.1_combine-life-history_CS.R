# Claude has duplicated this script from 05_simplify-existing-life-history-sheet-combine-with-CAAB-fishbase-info.R
# Not sure why it looks like the data from 03_get-fish-life-history-information-from-rfishbase wasn't being fully used


# This script uses the RLS information and targeted information from the old life history sheet and combine
# it with the information scraped from FishBase, IUCN and CAAB

rm(list = ls())

# load libraries required
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(CheckEM)
library(rfishbase)

# Set authorisation info for googlesheets
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

se <- function(x) {
  sd(x) / sqrt(length(x))
}

code_crosswalk_codes <- readRDS("annotation-schema/data/staging/code-crosswalk-codes.RDS") %>%
  dplyr::rename(fb_code = speccode) %>% # Have renamed this here to match for in later scripts
  dplyr::mutate(caab_code = as.character(caab_code)) %>%
  glimpse()

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
  dplyr::mutate(caab_scientific = paste(genus, species, sep = " ")) %>%
  dplyr::select(caab_scientific, new_maximum_length_in_cm, type_of_length_measure, source) %>% 
  dplyr::group_by(caab_scientific) %>%
  # slice(which.max(new_maximum_length_in_cm)) %>%
  slice_max(new_maximum_length_in_cm, n = 1, with_ties = F) %>% # More control over this, which.max just returns the first even if tied
  ungroup() %>%
  left_join(code_crosswalk_codes) %>%
  glimpse()

test <- new_max_sizes %>%
  group_by(fishbase_scientific) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) # None, wonderful

unique(new_max_sizes$type_of_length_measure)

# TODO add fishes of australia max sizes and depth limits
foa_max_sizes <- readRDS("annotation-schema/data/staging/fishes-of-australia_maximum-sizes.RDS") %>%
  clean_names() %>%
  dplyr::rename(caab_code = caab) %>%
  left_join(code_crosswalk_codes) %>%
  # dplyr::mutate(fishbase_scientific = paste(genus, species, sep = " ")) %>%
  dplyr::select(fishbase_scientific,
                caab_code, 
                foa_new_maximum_length_in_cm = new_maximum_length_in_cm,
                foa_min_depth = min_depth, 
                foa_max_depth = max_depth, 
                foa_type_of_length_measure = length_type,
                ) %>%
  dplyr::mutate(source_foa = "Fishes of Australia",
                foa_type_of_length_measure = if_else(foa_type_of_length_measure %in% "DL", # There is a typo for one species (Fowleria isostigma)
                                                     "TL", foa_type_of_length_measure)) %>%
  # The ones below had a blank length measure type, but have a length measure on Fishes of Australia
  dplyr::mutate(foa_type_of_length_measure = case_when(fishbase_scientific %in% "Neatypus obliquus" ~ "TL",
                                                       fishbase_scientific %in% "Lioscorpius longiceps" ~ "SL",
                                                       fishbase_scientific %in% "Lioscorpius trifasciatus" ~ "SL",
                                                       fishbase_scientific %in% "Hemitrygon fluviorum" ~ "DW",
                                                       .default = foa_type_of_length_measure)) %>%
  dplyr::filter(!fishbase_scientific %in% "Retropinna tasmanica") %>% # This was blank for length measure and doesn't specific on FoA
  glimpse()

# Join CheckEM and Fishbase max sizes ----
all_max_sizes <- left_join(foa_max_sizes, new_max_sizes) %>%
  dplyr::mutate(new_maximum_length_in_cm = if_else(is.na(new_maximum_length_in_cm),
                                                   foa_new_maximum_length_in_cm,
                                                   new_maximum_length_in_cm),
                type_of_length_measure = if_else(is.na(type_of_length_measure),
                                                 foa_type_of_length_measure,
                                                 type_of_length_measure),
                source = if_else(is.na(source),
                                 source_foa,
                                 source),
                type_of_length_measure = if_else(type_of_length_measure %in% "TL;", # Another typo here
                                                 "TL", type_of_length_measure)) %>%
  dplyr::select(-c(foa_new_maximum_length_in_cm, foa_type_of_length_measure,
                   source_foa)) %>%
  # There's ~300 that don't specify a type of length measure
  dplyr::filter(!is.na(type_of_length_measure)) %>%  # Seems to align with FoA website
  glimpse()

# Extract the species ----
max_length_species <- unique(all_max_sizes$fishbase_scientific)

# Extract length-length relationships to convert non-FL measures
ll_eqs <- length_length(max_length_species) %>%
  clean_names() %>%
  dplyr::rename(unknown = length1, known = length2) %>%
  dplyr::group_by(species, unknown, known) %>%
  summarise(a_ll = mean(a, na.rm = T),
            b_ll = mean(b, na.rm = T)) %>%
  ungroup() %>%
  # I am not going to try and convert any disk lengths/widths, so get rid of weird measures from ll table
  dplyr::filter(known %in% c("TL", "FL", "SL"),
                unknown %in% c("TL", "FL", "SL")) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  add_row(fishbase_scientific = "Choerodon rubescens", unknown = "TL", known = "FL",
          a_ll = 0, b_ll = 1) %>% # Add species here with a known length-length
  glimpse()

# Convert the length of maturity data into FL where possible ----
max_size_conv <- all_max_sizes %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  # Do all of the no-step and one-step conversion
  dplyr::mutate(max_length_conv = case_when(type_of_length_measure %in% "FL" ~ new_maximum_length_in_cm, # No conversion necessary
                                    # Standard equations
                                    type_of_length_measure %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ (new_maximum_length_in_cm * b_ll) + a_ll,
                                    type_of_length_measure %in% "SL" & unknown %in% "FL" & known %in% "SL" ~ (new_maximum_length_in_cm * b_ll) + a_ll,
                                    # Inverse equations
                                    type_of_length_measure %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ (new_maximum_length_in_cm - a_ll)/b_ll,
                                    type_of_length_measure %in% "SL" & unknown %in% "SL" & known %in% "FL" ~ (new_maximum_length_in_cm - a_ll)/b_ll),
                conversion_type = case_when(type_of_length_measure %in% "FL" ~ "no-conversion",
                                            type_of_length_measure %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ "regular-eq",
                                            type_of_length_measure %in% "SL" & unknown %in% "FL" & known %in% "SL" ~ "regular-eq",
                                            type_of_length_measure %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ "reversed-eq",
                                            type_of_length_measure %in% "SL" & unknown %in% "SL" & known %in% "FL" ~ "reversed-eq",
                                            is.na(max_length_conv) ~ "unconverted")) %>%
  # This leaves all the unconverted ones, but we get rid of them with ranking
  dplyr::mutate(measurement_type = if_else(conversion_type %in% "unconverted", type_of_length_measure, "FL"),
                max_length_conv = if_else(conversion_type %in% "unconverted", new_maximum_length_in_cm, max_length_conv)) %>% 
  dplyr::select(fishbase_scientific, caab_code, new_maximum_length_in_cm = max_length_conv, conversion_type, measurement_type, source) %>%
  glimpse()

only_unconverted_species <- max_size_conv %>%
  group_by(fishbase_scientific) %>%
  summarise(all_unconverted = all(conversion_type == "unconverted"), .groups = "drop") %>%
  filter(all_unconverted) %>%
  pull(fishbase_scientific)

# Just get the list of species where a two-step conversion is possible
# Also include them where you can get an inverse two-step
ts_species <- all_max_sizes %>%
  dplyr::filter(fishbase_scientific %in% only_unconverted_species) %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  group_by(fishbase_scientific) %>%
  dplyr::filter(!is.na(unknown)) %>% # Remove anything that doesn't have a length-length
  summarise(has_SL_to_TL = any(known == "SL" & unknown == "TL"),
            has_SL_to_TL_rev = any(known == "TL" & unknown == "SL"),
            has_TL_to_FL = any(known == "TL" & unknown == "FL"),
            has_TL_to_FL_rev = any(known == "FL" & unknown == "TL")) %>%
  # Filter it so it has to have both equations
  dplyr::filter((has_SL_to_TL | has_SL_to_TL_rev) &
                  (has_TL_to_FL | has_TL_to_FL_rev)) %>%
  pull(fishbase_scientific)

max_size_ts <- all_max_sizes %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  dplyr::filter(fishbase_scientific %in% ts_species) %>% # Only species with possible two-step conversions
  dplyr::mutate(lm_ts = case_when(type_of_length_measure %in% "SL" & 
                                    unknown %in% "TL" & 
                                    known %in% "SL" ~ 
                                    (new_maximum_length_in_cm * b_ll) + a_ll,
                                  type_of_length_measure %in% "SL" & 
                                    unknown %in% "SL" & 
                                    known %in% "TL" ~ 
                                    (new_maximum_length_in_cm - a_ll)/b_ll,
                                  type_of_length_measure %in% "TL" & 
                                    unknown %in% "SL" & 
                                    known %in% "TL" ~ 
                                    (new_maximum_length_in_cm * b_ll) + a_ll,
                                  type_of_length_measure %in% "TL" & 
                                    unknown %in% "TL" & 
                                    known %in% "SL" ~ 
                                    (new_maximum_length_in_cm - a_ll)/b_ll),
                calc_method = case_when(
                  type_of_length_measure %in% "SL" & unknown %in% "TL" & known %in% "SL" ~ "SL → TL via SL",
                  type_of_length_measure %in% "SL" & unknown %in% "SL" & known %in% "TL" ~ "TL → SL inverse",
                  type_of_length_measure %in% "TL" & unknown %in% "SL" & known %in% "TL" ~ "TL → SL via TL",
                  type_of_length_measure %in% "TL" & unknown %in% "TL" & known %in% "SL" ~ "SL → TL inverse"
                ),
                mid_type = case_when(
                  type_of_length_measure == "TL" ~ "SL",
                  type_of_length_measure == "SL" ~ "TL",
                  TRUE ~ NA_character_
                )) 

fl_forward <- ll_eqs %>%
  dplyr::filter(unknown == "FL", known %in% c("SL", "TL")) %>%
  dplyr::select(fishbase_scientific, known, a_fwd = a_ll, b_fwd = b_ll)

fl_inverse <- ll_eqs %>%
  filter(known == "FL", unknown %in% c("SL", "TL")) %>%
  select(fishbase_scientific, unknown, a_inv = a_ll, b_inv = b_ll)

max_size_fl <- max_size_ts %>%
  # join forward on (speccode, mid_type == known)
  left_join(fl_forward, by = c("fishbase_scientific", "mid_type" = "known")) %>%
  # join inverse on (speccode, mid_type == unknown)
  left_join(fl_inverse, by = c("fishbase_scientific", "mid_type" = "unknown")) %>%
  mutate(
    # If type1 already FL, just use the original value
    fl_final = case_when(
      type_of_length_measure == "FL" ~ new_maximum_length_in_cm,
      # If we have forward eqn: FL = a + b * mid
      !is.na(a_fwd) & !is.na(lm_ts) ~ (b_fwd * lm_ts) + a_fwd,
      # Else if we have inverse eqn: FL = (mid - a) / b
      !is.na(a_inv) & !is.na(lm_ts) ~ (lm_ts - a_inv) / b_inv,
      TRUE ~ NA_real_
    ),
    fl_calc_method = case_when(
      type_of_length_measure == "FL" ~ "already FL",
      !is.na(a_fwd) & !is.na(lm_ts) ~ paste0(mid_type, " → FL (forward)"),
      !is.na(a_inv) & !is.na(lm_ts) ~ paste0(mid_type, " → FL (inverse)"),
      TRUE ~ "no FL equation found"
    )
  ) %>%
  select(-a_fwd, -b_fwd, -a_inv, -b_inv) %>%
  dplyr::filter(!fl_calc_method %in% "no FL equation found") %>% # Get rid of these
  # Conditional filter
  group_by(fishbase_scientific) %>%
  mutate(
    # detect inverse at each stage (treat NA as not inverse)
    inv_stage1 = str_detect(coalesce(calc_method, ""), "inverse"),
    inv_stage2 = str_detect(coalesce(fl_calc_method, ""), "inverse"),
    
    # if any non-inverse exists in a stage, prefer non-inverse; otherwise prefer inverse
    prefer_inverse_stage1 = !any(!inv_stage1),
    prefer_inverse_stage2 = !any(!inv_stage2),
    
    keep_stage1 = if_else(prefer_inverse_stage1, inv_stage1, !inv_stage1),
    keep_stage2 = if_else(prefer_inverse_stage2, inv_stage2, !inv_stage2),
    
    keep = keep_stage1 & keep_stage2
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  dplyr::select(fishbase_scientific, caab_code, new_maximum_length_in_cm = fl_final, source) %>%
  dplyr::mutate(conversion_type = "two-step",
                measurement_type = "FL") %>%
  glimpse()

# Join the straight converted and two-step conversion species
max_size_final <- bind_rows(max_size_conv, max_size_fl) %>%
  distinct() %>%
  dplyr::mutate(ranking = case_when(conversion_type %in% "no-conversion" ~ 1,
                                    conversion_type %in% "regular-eq" ~ 2,
                                    conversion_type %in% "reversed-eq" ~ 3,
                                    conversion_type %in% "two-step" ~ 4,
                                    conversion_type %in% "unconverted" ~ 5)) %>%
  arrange(fishbase_scientific, ranking) %>%
  group_by(fishbase_scientific) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::mutate(source = case_when(str_detect(source, "fishesofaustralia") ~ "Fishes of Australia",
                                   str_detect(source, "australian.museum") ~ "Australian Museum",
                                   str_detect(source, "museumsvictoria") ~ "Museums Victoria",
                                   str_detect(source, "fishbase") ~ "Fishbase",
                                   str_detect(source, "reeflifesurvey") ~ "Reef Life Survey",
                                   str_detect(source, "S0272771423001981") ~ "https://doi.org/10.1016/j.ecss.2023.108408",
                                   str_detect(source, "https://doi.org/10.3354/meps11000") ~ "https://doi.org/10.3354/meps11000",
                                   str_detect(source, "Marine fishes of NW") ~ "Marine fishes of NW Australia",
                                   str_detect(source, "Sea Fishes of Southern Australia Book") ~ "Sea Fishes of Southern Australia",
                                   .default = source)) %>%
  dplyr::mutate(max_length_source = case_when(
      conversion_type %in% "no-conversion" ~ source,
      conversion_type %in% "reversed-eq"   ~ paste(source, "Converted to FL using inverse length-length relationship", sep = ": "),
      conversion_type %in% "two-step"      ~ paste(source, "Converted to FL using two-step length-length equations", sep = ": "),
      conversion_type %in% "regular-eq"    ~ paste(source, "Converted to FL using length-length equations", sep = ": "),
      conversion_type %in% "unconverted"   ~ paste(source, "No equations exist to convert into FL", sep = ": "))) %>%
  dplyr::select(-c(ranking, conversion_type, source)) %>%
  dplyr::rename(max_length_type = measurement_type,
                max_length_cm = new_maximum_length_in_cm) %>%
  glimpse()

unique(max_size_final$max_length_source)

test <- max_size_final %>%
  group_by(max_length_type) %>%
  dplyr::summarise(n = n())

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
                min_legal_nt = minlegal_nt,
                max_legal_nt = maxlegal_nt,
                bag_nt,
                min_legal_wa = minlegal_wa,
                max_legal_wa = maxlegal_wa,
                bag_wa,
                min_legal_qld = minlegal_qld,
                max_legal_qld = maxlegal_qld,
                bag_qld,
                min_legal_nsw = minlegal_nsw,
                max_legal_nsw = maxlegal_nsw,
                bag_nsw,
                min_legal_vic = minlegal_vic,
                max_legal_vic = maxlegal_vic,
                bag_vic,
                min_legal_sa = minlegal_sa,
                max_legal_sa = maxlegal_sa,
                bag_sa,
                min_legal_tas = minlegal_tas,
                max_legal_tas = maxlegal_tas,
                bag_tas,
                epbc_threat_status) %>%
  glimpse()

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
  dplyr::filter(!(caab_scientific %in% "Epigonus macrops" & fb_code %in% "14365")) %>% # Duplicated species
  dplyr::filter(!(caab_scientific %in% "Protosalanx chinensis" & fb_code %in% "12239")) %>% # Duplicated species
  dplyr::filter(!(caab_code %in% "37311059" & is.na(fb_code))) %>% # Duplicated species
  dplyr::filter(!(caab_code %in% "37361005" & is.na(fb_code))) %>% # Duplicated species
  dplyr::mutate(caab_code = as.character(caab_code)) %>%                
  dplyr::mutate(caab_scientific = if_else((caab_code %in% "37361005"), # Don't think this will do anything? species name already seems correct
                                          "Microcanthus joyceae", caab_scientific)) %>%
  # distinct() %>% # Does nothing
  dplyr::mutate(fb_trophic_level = as.numeric(fb_trophic_level)) %>%
  # dplyr::filter(!caab_scientific %in% "Rhina ancylostoma") %>% # Does nothing
  dplyr::mutate(fb_trophic_level_se = as.numeric(fb_trophic_level_se)) %>%
  dplyr::filter(!is.na(caab_scientific)) %>% # Need to check this - may be some issue in script 03
  glimpse()

test <- fishbase %>%
  dplyr::group_by(caab_scientific) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) # 30 species with NA caab_scientific

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
  distinct() %>%
  glimpse()

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
  dplyr::filter(!caab_code %in% c("37246020Taxasupercededby37246019", 
                                  "37118006", 
                                  "37441924", 
                                  # "37311954",  # Serranidae spp
                                  "37287949", 
                                  "37096000", 
                                  # "37018915", # Carcharhinidae spp
                                  "37337902")) %>%
  distinct() %>%
  glimpse()

test <- caab_combined %>%
  dplyr::group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_combined %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

# Things to consider for the below
# Measurement type = can't average across different length measures
# Standard error
# Length at maturity source

# A lot of the entries have a trophic level but no length of maturity
# genus_trophic_maturity <- fishbase %>%
#   left_join(caab_combined) %>%
#   dplyr::group_by(family, genus, fb_length_at_maturity_type) %>%
#   dplyr::summarise(spp_fb_trophic_level_mean = mean(fb_trophic_level, na.rm = TRUE),
#                    spp_fb_trophic_level_se = se(fb_trophic_level),
#                    spp_fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE),
#                    .groups = "drop") %>% # Same as doing ungroup() afterwards
#   dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
#   dplyr::mutate(species = "spp",
#                 ranking = case_when(fb_length_at_maturity_type %in% 'FL' ~ 1,
#                                     fb_length_at_maturity_type %in% 'TL' ~ 2,
#                                     fb_length_at_maturity_type %in% 'SL' ~ 3,
#                                     is.na(fb_length_at_maturity_type)    ~ 4)) %>%
#   dplyr::group_by(family, genus) %>%
#   slice_min(ranking) %>%
#   ungroup() %>%
#   dplyr::mutate(spp_fb_length_at_maturity_source = case_when(is.na(fb_length_at_maturity_type) ~ NA,
#                                                          !is.na(fb_length_at_maturity_type) ~ "Fishbase: Genus-level average")) %>%
#   glimpse()

# Genus level trophic level
genus_trophic <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family, genus) %>% # This is using CAAB name
  dplyr::summarise(spp_fb_trophic_level_mean = mean(fb_trophic_level, na.rm = TRUE),
                   spp_fb_trophic_level_se = se(fb_trophic_level),
                   .groups = "drop") %>% # Same as doing ungroup() afterwards
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>% # These are only calculated off 1 value - no SE for single sample
  dplyr::mutate(species = "spp") %>% 
  glimpse()

genus_maturity <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family, genus,  # This is using CAAB name
                  fb_length_at_maturity_type) %>%
  dplyr::summarise(spp_fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE),
                   .groups = "drop") %>% # Same as doing ungroup() afterwards
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(species = "spp",
                ranking = case_when(fb_length_at_maturity_type %in% 'FL' ~ 1,
                                    fb_length_at_maturity_type %in% 'TL' ~ 2,
                                    fb_length_at_maturity_type %in% 'SL' ~ 3,
                                    is.na(fb_length_at_maturity_type)    ~ 4)) %>%
  dplyr::group_by(family, genus) %>%
  slice_min(ranking) %>%
  ungroup() %>%
  dplyr::mutate(spp_fb_length_at_maturity_source = case_when(is.na(fb_length_at_maturity_type) ~ NA,
                                                         !is.na(fb_length_at_maturity_type) ~ "Fishbase: Genus-level average")) %>%
  dplyr::select(-ranking) %>%
  glimpse()

genus_trophic_maturity <- genus_trophic %>%
  dplyr::left_join(genus_maturity) # There are lots of length of maturities that are NA by genus, and some trophic levels

test <- genus_trophic_maturity %>%
  dplyr::group_by(family, genus) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

# family_trophic_maturity <- fishbase %>%
#   left_join(caab_combined) %>%
#   dplyr::group_by(family, fb_length_at_maturity_type) %>%
#   dplyr::summarise(spp_fb_trophic_level_mean = mean(fb_trophic_level, na.rm = TRUE), 
#                    spp_fb_trophic_level_se = se(fb_trophic_level),
#                    spp_fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE),
#                    .groups = "drop") %>%
#   mutate_all(~ifelse(is.nan(.), NA, .)) %>%
#   dplyr::mutate(species = "spp", genus = "Unknown",
#                 ranking = case_when(fb_length_at_maturity_type %in% 'FL' ~ 1,
#                                     fb_length_at_maturity_type %in% 'TL' ~ 2,
#                                     fb_length_at_maturity_type %in% 'SL' ~ 3,
#                                     is.na(fb_length_at_maturity_type)    ~ 4)) %>%
#   dplyr::group_by(family) %>%
#   slice_min(ranking) %>%
#   ungroup() %>%
#   dplyr::mutate(spp_fb_length_at_maturity_source = case_when(is.na(fb_length_at_maturity_type) ~ NA,
#                                                          !is.na(fb_length_at_maturity_type) ~ "Fishbase: Family-level average")) %>%
#   glimpse()

family_trophic <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family) %>% # This is using CAAB name
  dplyr::summarise(spp_fb_trophic_level_mean = mean(fb_trophic_level, na.rm = TRUE),
                   spp_fb_trophic_level_se = se(fb_trophic_level),
                   .groups = "drop") %>% # Same as doing ungroup() afterwards
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>% # These are only calculated off 1 value - no SE for single sample
  dplyr::mutate(genus = "Unknown", species = "spp") %>% 
  glimpse()

family_maturity <- fishbase %>%
  left_join(caab_combined) %>%
  dplyr::group_by(family, # This is using CAAB names
                  fb_length_at_maturity_type) %>%
  dplyr::summarise(spp_fb_length_at_maturity_cm = mean(fb_length_at_maturity_cm, na.rm = TRUE),
                   .groups = "drop") %>% # Same as doing ungroup() afterwards
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(genus = "Unknown", species = "spp",
                ranking = case_when(fb_length_at_maturity_type %in% 'FL' ~ 1,
                                    fb_length_at_maturity_type %in% 'TL' ~ 2,
                                    fb_length_at_maturity_type %in% 'SL' ~ 3,
                                    is.na(fb_length_at_maturity_type)    ~ 4)) %>%
  dplyr::group_by(family, genus) %>%
  slice_min(ranking) %>%
  ungroup() %>%
  dplyr::mutate(spp_fb_length_at_maturity_source = case_when(is.na(fb_length_at_maturity_type) ~ NA,
                                                             !is.na(fb_length_at_maturity_type) ~ "Fishbase: Family-level average")) %>%
  dplyr::select(-ranking) %>%
  glimpse()

family_trophic_maturity <- family_trophic %>%
  dplyr::left_join(family_maturity)

spp_trophic_maturity <- bind_rows(genus_trophic_maturity, family_trophic_maturity) %>%
  dplyr::rename(spp_fb_length_at_maturity_type = fb_length_at_maturity_type) %>%
  dplyr::select(family, genus, species, everything()) %>%
  glimpse()

# fishbase <- bind_rows(fishbase, spp_trophic_maturity)

# Where does the IUCN data get made??
iucn_animals <- readRDS("annotation-schema/data/staging/iucn.RDS") %>%
  dplyr::mutate(iucn_ranking = case_when(
    category %in% "DD" ~ "Data Deficient",
    category %in% c("LC", "LR/lc") ~ "Least Concern", # Old classification - aligns with Least Concern
    category %in% c("NT", "LR/nt") ~ "Near Threatened", # Old classification - aligns with Near Threatened
    category %in% "VU" ~ "Vulnerable",
    category %in% "EN" ~ "Endangered",
    category %in% "CR" ~ "Critically Endangered",
    category %in% "EW" ~ "Extinct in the Wild",
    category %in% "EX" ~ "Extinct",
  )) %>%
  # There are some that are ranked as "LR/cd" - Lower risk/conservation dependent
  # That is an old category that doesn't have a mdoern equivalent
  dplyr::select(-c(category)) %>%
  glimpse()

test <- dplyr::filter(iucn_animals, is.na(iucn_ranking)) # Should these get filtered? or is there a reason to keep

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
  filter(is.na(order)) # There are some here missing order or class - fix?

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
    australian_common_name = common_name #,
    # fb_code = speccode,
    # fb_length_weight_measure = length_measure, #TODO make this happen in fishbase script
    # fb_a = a, # TODO also rename all these in fishbase script
    # fb_b = b,
    # fb_a_ll = a_ll,
    # fb_b_ll = b_ll,
    # fb_ll_equation_type = ll_equation_type,
    # fb_length_max_type = fb_l_type_max,
    # fb_length_weight_source = source_level
    ) %>%
  dplyr::left_join(keep) %>% # This is the old life history sheet
  dplyr::mutate(australian_source = "CAAB",
                global_source = "FishBase",
                local_source = "Harvey et al 2020") %>% # What is this?
  dplyr::mutate(scientific_name = paste(genus, species)) %>%
  dplyr::mutate(global_region = "Australia") %>%
  dplyr::select(australian_source,
                  caab_code,
                  class,
                  order,
                  family,
                  genus,
                  species,
                  scientific_name,
                  australian_common_name, # TODO need to add in 1st script or use the scraping
                  aus_region,
                  imcra_region,
                  global_source,
                  fb_code,
                  fb_length_at_maturity_cm,
                  fb_length_at_maturity_type,
                  fb_length_at_maturity_source,
                  # TODO need to add in 1st script
                  subfamily,
                  global_region,
                  fb_length_weight_measure,
                  fb_a,
                  fb_b,
                  fb_a_ll,
                  fb_b_ll,
                  fb_ll_equation_type,
                  fb_length_weight_source,
                  fb_trophic_level,
                  fb_trophic_level_se,
                  fb_trophic_level_source,
                  fb_vulnerability,
                  fb_countries,
                  fb_status,
                  fb_length_max_cm,
                  fb_length_max_type,
                  fb_length_max_source,
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
                  max_legal_tas) %>%
  bind_rows(animals) %>%
  bind_rows(spps) %>%
  # These replaces all don't seem to be doing anything??
  # dplyr::mutate(family = if_else(genus %in% "Heteroscarus", "Labridae", family)) %>%
  # dplyr::mutate(family = if_else(genus %in% "Olisthops", "Labridae", family)) %>%
  # dplyr::mutate(family = if_else(genus %in% "Siphonognathus", "Labridae", family)) %>%
  # dplyr::mutate(family = if_else(genus %in% "Neoodax", "Labridae", family)) %>%
  # dplyr::mutate(family = if_else(genus %in% "Haletta", "Labridae", family)) %>%
  # dplyr::mutate(order = if_else(family %in% "Siphonariidae", "Siphonariida", order)) %>%
  dplyr::left_join(max_size_final) %>%
  dplyr::mutate(length_max_cm = if_else(!is.na(max_length_cm), max_length_cm, fb_length_max_cm)) %>%
  dplyr::mutate(length_max_source = if_else(!is.na(max_length_source), max_length_source, fb_length_max_source)) %>% # BRUV expert
  dplyr::mutate(length_max_type = if_else(!is.na(max_length_type), max_length_type, fb_length_max_type)) %>%
  dplyr::select(-c(fb_length_max_cm, fb_length_max_type, fb_length_max_source,
                   max_length_cm, max_length_source, max_length_type)) %>%
  # dplyr::left_join(foa_max_sizes) %>%
  # tidyr::replace_na(list(new_maximum_length_in_cm = 0)) %>%
  # dplyr::mutate(length_max_source = if_else(new_maximum_length_in_cm > fb_length_max, "Fishes of Australia", length_max_source)) %>%
  # dplyr::mutate(fb_length_max = if_else(new_maximum_length_in_cm > fb_length_max, new_maximum_length_in_cm, fb_length_max)) %>%
  # dplyr::mutate(fb_length_max_type = if_else((new_maximum_length_in_cm > fb_length_max), type_of_length_measure, fb_length_max_type)) %>%
  # dplyr::select(-c(new_maximum_length_in_cm, type_of_length_measure)) %>%
  filter(!grepl("cf", species)) %>%
  filter(!grepl("sp\\.", species)) %>%
  dplyr::left_join(spp_trophic_maturity) %>%
  # Spp length of maturity type
  dplyr::mutate(fb_length_at_maturity_type = if_else(is.na(fb_length_at_maturity_type), spp_fb_length_at_maturity_type, fb_length_at_maturity_type)) %>%
  # Spp trophic level mean
  dplyr::mutate(fb_trophic_level = if_else(is.na(fb_trophic_level), spp_fb_trophic_level_mean, fb_trophic_level)) %>%
  # Spp trophic level SE
  dplyr::mutate(fb_trophic_level_se = if_else(is.na(fb_trophic_level_se), spp_fb_trophic_level_se, fb_trophic_level_se)) %>%
  # Spp length of maturity
  dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(fb_length_at_maturity_cm), spp_fb_length_at_maturity_cm, fb_length_at_maturity_cm)) %>%
  # Spp length of maturity source
  dplyr::mutate(fb_length_at_maturity_source = if_else(is.na(fb_length_at_maturity_source), spp_fb_length_at_maturity_source, fb_length_at_maturity_source)) %>%
  # dplyr::mutate(fb_trophic_level = if_else(is.na(fb_trophic_level), spp_fb_trophic_level, fb_trophic_level)) %>%
  # dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(fb_length_at_maturity_cm), spp_fb_length_at_maturity_cm, fb_length_at_maturity_cm)) %>%
  # dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(fb_length_at_maturity_cm), spp_fb_length_at_maturity_cm, fb_length_at_maturity_cm)) %>%
  dplyr::select(-c(spp_fb_length_at_maturity_type, spp_fb_trophic_level_mean, spp_fb_trophic_level_se, spp_fb_length_at_maturity_cm, spp_fb_length_at_maturity_source)) %>%
  dplyr::mutate(australian_source = "CAAB") %>%
  dplyr::left_join(foa_max_sizes %>% dplyr::select(caab_code, foa_min_depth, foa_max_depth)) %>% # This just joins the min and max depths
  dplyr::select(# CAAB info
    australian_source,
    caab_code,
    class,
    order,
    family,
    subfamily,
    genus,
    species,
    scientific_name,
    australian_common_name,
    global_region,
    aus_region,
    imcra_region,
    global_source,
    fb_code,
    fb_length_at_maturity_cm,
    fb_length_at_maturity_type,
    fb_length_at_maturity_source, 
    fb_length_weight_measure,
    fb_a,
    fb_b,
    fb_a_ll,
    fb_b_ll,
    fb_ll_equation_type, 
    fb_length_weight_source,
    fb_vulnerability,
    fb_countries,
    fb_status,
    length_max_cm,
    length_max_type,
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
    max_legal_tas)

spp_test <- australia_life_history %>%
  dplyr::filter(species %in% "spp")

names(australia_life_history)

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
# 49.98008% with distribution info available from worms package

number.with.distributions <- australia_life_history %>% 
  filter(!is.na(imcra_region))

nrow(number.with.distributions)/nrow(australia_life_history) * 100 
# 50.44888% with distribution info available from worms package

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
  dplyr::mutate(imcra_region = str_replace_all(imcra_region, "\\, NA", "")) %>%
  distinct()

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

test <- australia_life_history %>%
  dplyr::filter(length_max_type %in% c("SL", "TL"),
                !is.na(fb_ll_equation_type)) %>%
  dplyr::select(scientific_name, length_max_type, length_max_cm,
                length_max_source,
                fb_length_at_maturity_type,
                fb_length_weight_measure, fb_ll_equation_type) # These 2 species make sense


saveRDS(australia_life_history, "annotation-schema/output/fish/schema/australia_life-history.RDS") # To share with people
write.csv(australia_life_history, "annotation-schema/output/fish/schema/australia_life-history.csv", row.names = FALSE) # To share with people
# saveRDS(australia_life_history, "inst/shiny/CheckEM/data/australia_life-history.RDS") # to update shiny app ## Haven't ran this!!!

# For CheckEM package - Save as an rda to use as package data
usethis::use_data(australia_life_history, overwrite = TRUE)

