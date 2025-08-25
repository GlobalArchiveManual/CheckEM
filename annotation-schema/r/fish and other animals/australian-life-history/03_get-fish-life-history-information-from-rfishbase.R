# This script uses the latest CAAB download from the CSIRO to:
# - validate names using rfishbase
# - Get information on the size of maturity, maximum sizes, IUCN categories and length-weight parameters

# Load the required libraries
library(tidyverse)
library(rfishbase)
library(openssl)
library(CheckEM)
library(taxize) # For IUCN
library(rredlist)
library(foreach)
library(doParallel)

# Use the list of Australian species created in the second script
caab <- readRDS("annotation-schema/data/staging/australia_fish_caab-with-regions.RDS") %>%
  dplyr::mutate(species = str_replace_all(.$species, c("[^[:alnum:]]" = "",
                                                       "cfizaspilotai" = "zaspilota",
                                                       "icfifilamentosa" = "filamentosa"))) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(!species == "spp") %>%
  dplyr::mutate(common_name = if_else(scientific_name %in% c("Lethrinus punctulatus"), "Bluespotted emperor", common_name)) %>%
  dplyr::mutate(common_name = if_else(scientific_name %in% c("Caranx heberi"), "Blacktip trevally", common_name))

# Validate scientific names to use in fishbase package ----
validated <- rfishbase::validate_names(caab$scientific_name)

codes <- rfishbase::common_names(validated) %>%
  clean_names() %>%
  distinct(species, speccode) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  filter(!is.na(fishbase_scientific)) %>%
  dplyr::mutate(speccode = as.character(speccode))

## This is slow but essential for figuring out what fishbase name = CAAB
# # Remove the hashes to run again
# code_crosswalk <- data.frame()
# 
# for (caab_name in unique(caab$scientific_name)) {
# 
#   validated_name <-  rfishbase::validate_names(caab_name)
# 
#   message(paste("validating: ", caab_name))
#   message(paste("validated name:", validated_name))
# 
#   temp_dat <- data.frame(fishbase_scientific = validated_name,
#                          caab_scientific = caab_name)
#   code_crosswalk <- bind_rows(code_crosswalk, temp_dat)
# 
# }
# mismatches <- code_crosswalk %>%
#   filter(!fishbase_scientific %in% caab_scientific)
# 
# code_crosswalk_codes <- caab %>%
#   dplyr::rename(caab_scientific = scientific_name) %>%
#   dplyr::full_join(code_crosswalk) %>%
#   dplyr::full_join(codes) %>%
#   dplyr::select(caab_code, caab_scientific, speccode, fishbase_scientific) %>%
#   dplyr::mutate(fishbase_scientific = str_replace_all(.$fishbase_scientific, c("Genus Species" = ""))) %>%
#   dplyr::mutate(speccode = if_else(speccode == "0", NA, speccode)) %>%
#   dplyr::mutate(across(c("fishbase_scientific", "speccode"), ~if_else(.=="", NA, as.character(.)))) %>%
#   glimpse()
# 
# saveRDS(code_crosswalk_codes, "annotation-schema/data/staging/code-crosswalk-codes.RDS")

code_crosswalk_codes <- readRDS("annotation-schema/data/staging/code-crosswalk-codes.RDS")

double_fb <- code_crosswalk_codes %>%
  dplyr::group_by(speccode) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) %>%
  ungroup() %>%
  dplyr::left_join(code_crosswalk_codes) %>%
  dplyr::filter(!is.na(speccode))

true_matches <- double_fb %>%
  dplyr::filter(caab_scientific %in% fishbase_scientific) %>%
  dplyr::select(caab_code, caab_scientific, speccode, fishbase_scientific)

still_missing <- anti_join(double_fb %>% dplyr::select(speccode), true_matches)

# Download maturity data ----
# Remove any that are NA
# Average the length at maturity (cm)
# Then change synonyms

# Load all length of maturity data, tidy and filter ----

## Summarise across all length of maturity studies
# maturity <- maturity(validated) %>%
#   clean_names() %>%
#   dplyr::filter(!is.na(lm)) %>%
#   dplyr::filter(!is.na(type1)) %>% # This loses species that don't specify a measurement type
#   # dplyr::mutate(test = if_else(type1 %in% "OT", comment, type1)) %>% # These are all wack
#   dplyr::filter(type1 %in% c("TL", "SL", "FL")) %>% # Remove non-standard length measures (131 entries removed)
#   dplyr::group_by(species, speccode, type1) %>%
#   dplyr::summarise(fb_length_at_maturity_cm = mean(lm)) %>% # Average length of maturity per measure
#   dplyr::rename(fishbase_scientific = species) %>%
#   dplyr::mutate(speccode = as.character(speccode)) %>%
#   dplyr::ungroup()

maturity <- maturity(validated) %>%
  clean_names() %>%
  dplyr::filter(!is.na(lm)) %>%
  dplyr::filter(!is.na(type1)) %>% # This loses species that don't specify a measurement type
  # dplyr::mutate(test = if_else(type1 %in% "OT", comment, type1)) %>% # These are all wack
  dplyr::filter(type1 %in% c("TL", "SL", "FL")) %>% # Remove non-standard length measures (131 entries removed)
  dplyr::rename(fishbase_scientific = species,
                fb_length_at_maturity_cm = lm) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  mutate(country_rank = case_when(c_code == "036" ~ 1,
    str_detect(locality, "New Zealand") ~ 2)) %>%
  mutate(number_rank = case_when(number >= 100 ~ 1,
    number >= 20 & number < 100 ~ 2,
    number >= 10 & number < 20 ~ 3,
    number >= 1 & number < 10 ~ 4)) %>%
  mutate(type_rank = case_when(type1 %in% "FL" ~ 1,
    type1 %in% "TL" ~ 2,
    type1 %in% c("SL","WD","OT","PC","NG","AF","LP") ~ 6)) %>%
  mutate(sex_rank = case_when(sex %in% c("mixed","unsexed", "Unsexed") ~ 1,
    sex %in% c(NA) ~ 2,
    sex %in% c("juvenile") ~ 8)) %>%
  replace_na(list(country_rank = 3, number_rank = 5, type_rank = 7, sex_rank = 2)) %>%
  dplyr::mutate(final_rank = type_rank + sex_rank + country_rank * 0.5 + number_rank) %>%
  # dplyr::select(fishbase_scientific, speccode, type1, fb_length_at_maturity_cm, final_rank) %>%
  glimpse()

maturity_ranked <- maturity %>%
  group_by(fishbase_scientific, speccode) %>%
  dplyr::slice_min(final_rank, n = 1) %>%
  ungroup() %>%
  glimpse()

test <- maturity %>%
  group_by(fishbase_scientific, speccode) %>%
  dplyr::summarise(n = n())

# Extract the species ----
maturity_species <- unique(maturity$fishbase_scientific) 

# Extract length-length relationships to convert non-FL measures ----
ll_eqs <- length_length(maturity_species) %>%
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

# Convert the length of maturity data into FL where possible ----
maturity_conv <- maturity %>%
  left_join(ll_eqs, relationship = "many-to-many") %>% 
  dplyr::mutate(lm_conv = case_when(type1 %in% "FL" ~ fb_length_at_maturity_cm,
                                    type1 %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ (fb_length_at_maturity_cm * b_ll) + a_ll,
                                    type1 %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ (fb_length_at_maturity_cm - a_ll)/b_ll),
                conversion_type = case_when(type1 %in% "FL" ~ "no-conversion",
                                    type1 %in% "TL" & unknown %in% "FL" & known %in% "TL" ~ "regular-eq",
                                    type1 %in% "TL" & unknown %in% "TL" & known %in% "FL" ~ "reversed-eq")) %>%
  dplyr::filter(!is.na(lm_conv)) %>%
  dplyr::select(fishbase_scientific, speccode, lm_conv, conversion_type) %>%
  dplyr::rename(fb_length_at_maturity_cm = lm_conv) %>%
  glimpse()

ts_species <- maturity %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  group_by(fishbase_scientific) %>%
  dplyr::filter(!is.na(unknown)) %>% # Remove anything that doesn't have a length-length
  summarise(has_SL_to_TL = any(type1 == "SL" & known == "SL" & unknown == "TL"),
            has_SL_to_TL_rev = any(type1 == "SL" & known == "TL" & unknown == "SL"),
            has_TL_to_FL = any(type1 == "SL" & known == "TL" & unknown == "FL"),
            has_TL_to_FL_rev = any(type1 == "SL" & known == "FL" & unknown == "TL")) %>%
  # dplyr::filter(has_SL_to_TL & has_TL_to_FL) %>%
  dplyr::filter((has_SL_to_TL | has_SL_to_TL_rev) &
      (has_TL_to_FL | has_TL_to_FL_rev)) %>%
  pull(fishbase_scientific)

maturity_ts <- maturity %>%
  left_join(ll_eqs, relationship = "many-to-many") %>%
  dplyr::filter(fishbase_scientific %in% ts_species) %>% # Only species with possible two-step conversions
  # Convert SL to TL
  dplyr::mutate(lm_tl = case_when(type1 %in% "SL" & 
                                         unknown %in% "TL" & 
                                         known %in% "SL" ~ 
                                         (fb_length_at_maturity_cm * b_ll) + a_ll)) %>%
  dplyr::mutate(lm_tl_rev = case_when(type1 %in% "SL" & 
                                    unknown %in% "SL" & 
                                    known %in% "TL" ~ 
                                    (fb_length_at_maturity_cm - a_ll)/b_ll)) %>%
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

# 1.
# direct_to_FL <- ll_eqs %>%
#   filter(unknown == "FL") %>%
#   distinct(species) %>%
#   pull(species)
# 
# # 2. For those without direct FL, check if there's a 2-step path
# two_step_candidates <- ll_eqs %>%
#   filter(!(species %in% direct_to_FL)) %>%
#   group_by(species) %>%
#   summarise(
#     step1 = list(known[unknown == "SL"]),
#     step2 = list(known[unknown == "FL"])
#   ) %>%
#   rowwise() %>%
#   mutate(has_path = any(step1 %in% step2)) %>%
#   filter(has_path) %>%
#   pull(species)
# 
# # 3. Final output: species that need 2-step conversion to FL
# two_step_only <- setdiff(two_step_candidates, direct_to_FL)
# 
# two_step_only

# Get fb_vulnerability, fb_length_max and fb_l_type_max, information from FishBase ----
info <- species(validated) %>% 
  clean_names() %>%
  dplyr::rename(fb_length_max = length, 
                fb_l_type_max = ltypemaxm,
                fb_vulnerability = vulnerability) %>% # Length metrics are in cm
  dplyr::select(species, speccode, fbname, fb_length_max, fb_l_type_max, fb_vulnerability, subfamily) %>%
  dplyr::rename(fishbase_scientific = species, 
                fb_common_name = fbname) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::filter(!fishbase_scientific %in% ("Genus Species")) %>%
  dplyr::glimpse()

not_fl <- info %>%
  dplyr::filter(!fb_l_type_max %in% "FL") # majority are not forklength

max_length_is_fl <- info %>%
  dplyr::filter(fb_l_type_max %in% "FL") # only 104 fish species max length are givne in forklength

# FB.countries and FB.Status ----
country_dat <- country(validated) %>%
  clean_names()%>%
  dplyr::mutate(speccode = as.character(speccode))

status <- country_dat %>%
  dplyr::filter(.$country %in% "Australia") %>%
  dplyr::select(species, speccode, status) %>%
  dplyr::rename(fishbase_scientific = species, 
                fb_status = status) 

countries <- country_dat %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(fb_countries = toString(country)) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  dplyr::ungroup()

# a and b values ----
ll <- length_length(validated) %>%
  clean_names() %>%
  dplyr::select(species, length1, length2, a, b)%>%
  dplyr::filter(length2 == "FL")%>%
  dplyr::rename(all = a)%>%
  dplyr::rename(bll = b)%>%
  dplyr::rename(type = length1)%>%
  dplyr::group_by(species, type)%>%
  dplyr::summarise(all = mean(all), bll = mean(bll)) %>%
  dplyr::filter(!type %in% c("SL", "Other"))

unique(ll$type)

# Length 2 is the known length
max_lengths_available <-  info %>%
  dplyr::filter(!is.na(fb_length_max))

summary(max_lengths_available)
summary_length_type <- max_lengths_available %>%
  dplyr::group_by(fb_l_type_max) %>%
  dplyr::summarise(n = n())

# 5087 species have a max length

unique(max_lengths_available$fb_l_type_max)

ll_for_max_lengths <- length_length(validated) %>%
  clean_names() %>%
  dplyr::select(species, length1, length2, a, b, lengthmin, lengthmax, r2, number) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  left_join(info %>% select(fishbase_scientific, fb_length_max, fb_l_type_max)) %>%
  glimpse()

length(unique(ll_for_max_lengths$fishbase_scientific)) # 2811 species

# Step 1: Identify max lengths already in FL
max_lengths_already_fl <- info %>%
  dplyr::filter(fb_l_type_max %in% "FL") %>%
  dplyr::select(fishbase_scientific, fb_length_max, fb_l_type_max) %>%
  dplyr::mutate(max_length_source = "FishBase")

length(unique(max_lengths_already_fl$fishbase_scientific)) # 104 species
  
# Step 2: Direct conversions (TL to FL or SL to FL)
max_lengths_conversion <- ll_for_max_lengths %>%
  dplyr::filter(length1 == "FL") %>%
  dplyr::mutate(conversion_possible = if_else(length2 == fb_l_type_max, TRUE, FALSE)) %>%
  dplyr::filter(!conversion_possible %in% FALSE) %>%
  dplyr::rename(known_length = length2,
               unknown_length = length1) %>%
  dplyr::mutate(fb_length_max = a + b * fb_length_max) %>%
  dplyr::group_by(fishbase_scientific) %>%
  dplyr::slice_max(fb_length_max, with_ties = FALSE) %>%
  dplyr::mutate(max_length_source = paste("FishBase:", known_length, "converted to FL using length-length equation"),
                fb_l_type_max = "FL") %>%
  dplyr::ungroup() %>%
  dplyr::filter(!fishbase_scientific %in% max_lengths_already_fl$fishbase_scientific) # don't include species where the max is actually given in forklength

# Step 3: Reverse conversions (FL obtained from conversions like FL -> TL)
max_lengths_conversion_reversed <- ll_for_max_lengths %>%
  dplyr::filter(length2 == "FL") %>%
  dplyr::mutate(conversion_possible = if_else(length1 == fb_l_type_max, TRUE, FALSE)) %>%
  dplyr::filter(!conversion_possible %in% FALSE) %>%
  dplyr::rename(known_length = length2,
                unknown_length = length1) %>%
  dplyr::mutate(fb_length_max = (fb_length_max - a)/b) %>%
  dplyr::group_by(fishbase_scientific) %>%
  dplyr::slice_max(fb_length_max, with_ties = FALSE) %>%
  dplyr::mutate(max_length_source = paste("FishBase:", known_length, "converted to Fl using inverse length-length equation"),
                fb_l_type_max = "FL") %>%
  dplyr::ungroup() %>%
  dplyr::filter(!fishbase_scientific %in% max_lengths_already_fl$fishbase_scientific) %>% # don't include species where the max is actually given in forklength
  dplyr::filter(!fishbase_scientific %in% max_lengths_conversion$fishbase_scientific) # don't include species where conversion equation was already given

# Step 4: Multi-step conversions (TL -> SL, then SL -> FL)
multi_step_conversion_1 <- ll_for_max_lengths %>%
  filter(length1 == "SL", length2 == "TL") %>%
  dplyr::rename(known_length = length2,
                unknown_length = length1) %>%
  dplyr::filter(known_length == fb_l_type_max) %>%
  left_join(ll_for_max_lengths %>% 
              filter(length1 == "FL", length2 == "SL") %>%
              dplyr::select(-c(fb_length_max, fb_l_type_max)) %>%
                              dplyr::rename(known_length = length2, unknown_length = length1),
            by = "fishbase_scientific", suffix = c("_tl_to_sl", "_sl_to_fl")) %>%
  mutate(intermediate_SL = a_tl_to_sl + b_tl_to_sl * fb_length_max,
         fb_length_max_FL = a_sl_to_fl + b_sl_to_fl * intermediate_SL) %>%
  dplyr::filter(!is.na(fb_length_max_FL)) %>%
  group_by(fishbase_scientific) %>%
  slice_max(fb_length_max_FL, with_ties = FALSE) %>%
  mutate(max_length_source = "FishBase, multi-step TL->SL->FL",
         fb_l_type_max = "FL") %>%
  ungroup() %>%
  filter(!fishbase_scientific %in% max_lengths_already_fl$fishbase_scientific &
           !fishbase_scientific %in% max_lengths_conversion$fishbase_scientific &
           !fishbase_scientific %in% max_lengths_conversion_reversed$fishbase_scientific)

# Step 4: Multi-step conversions (SL -> TL, then TL -> FL)
multi_step_conversion_2 <- ll_for_max_lengths %>%
  filter(length1 == "TL", length2 == "SL") %>%
  dplyr::rename(known_length = length2,
                unknown_length = length1) %>%
  dplyr::filter(known_length == fb_l_type_max) %>%
  left_join(ll_for_max_lengths %>% 
              filter(length1 == "FL", length2 == "TL") %>%
              dplyr::select(-c(fb_length_max, fb_l_type_max)) %>%
              dplyr::rename(known_length = length2, unknown_length = length1),
            by = "fishbase_scientific", suffix = c("_sl_to_tl", "_tl_to_fl")) %>%
  mutate(intermediate_TL = a_sl_to_tl + b_sl_to_tl * fb_length_max,
         fb_length_max = a_tl_to_fl + b_tl_to_fl * intermediate_TL) %>%
  dplyr::filter(!is.na(fb_length_max)) %>%
  dplyr::select(fishbase_scientific, fb_length_max) %>%
  group_by(fishbase_scientific) %>%
  slice_max(fb_length_max, with_ties = FALSE) %>%
  mutate(max_length_source = "FishBase, multi-step SL->TL->FL",
         fb_l_type_max = "FL") %>%
  ungroup() %>%
  filter(!fishbase_scientific %in% max_lengths_already_fl$fishbase_scientific &
           !fishbase_scientific %in% max_lengths_conversion$fishbase_scientific &
           !fishbase_scientific %in% max_lengths_conversion_reversed$fishbase_scientific)

all_max_lengths_fl <- bind_rows(max_lengths_already_fl,
                                max_lengths_conversion,
                                max_lengths_conversion_reversed,
                                multi_step_conversion_2) %>%
  dplyr::select(fishbase_scientific, fb_length_max, fb_l_type_max, max_length_source) %>%
  dplyr::mutate(fb_length_max = as.numeric(fb_length_max)) %>%
  glimpse()

length(unique(all_max_lengths_fl$fishbase_scientific)) # 1946 species with forklength information

# There are still 3152 species with forklength information missing
# however there are some species where we could use multiple equations to get to FL
# e.g. Abudefduf whitleyi the maximum length is given in SL, there is a and b values for SL -> TL and TL -> FL
fl_missing <- max_lengths_available %>%
  dplyr::filter(!fishbase_scientific %in% all_max_lengths_fl$fishbase_scientific) 

# Some of these don't have any length-length information though e.g. Bodianus solatus

fl_missing_but_has_length_length <- fl_missing %>%
  dplyr::filter(fishbase_scientific %in% ll_for_max_lengths$fishbase_scientific) 
# Only 880 fish that have length-length

# Now check whihc ones actually have some FL calc
length_length_to_fl <- ll_for_max_lengths %>%
  dplyr::filter(length1 %in% "FL" | length2 %in% "FL")
  
fl_missing_but_has_length_length <- fl_missing %>%
  dplyr::filter(fishbase_scientific %in% ll_for_max_lengths$fishbase_scientific) %>%
  dplyr::filter(fishbase_scientific %in% length_length_to_fl$fishbase_scientific) %>%
  dplyr::filter(!fb_l_type_max %in% c("NG", "OT"))

# only 421 fish species that have a/b for a FL calculation - still doesn't mean that I will be able to get them all but it is a much easier number to work with to see what I need to script

# Can remove ones where the type that is given does not occur in the length-length table as an option
# e.g. Urogymnus granulatus is given in WD but there is not WD in the table

fl_missing_but_has_length_length_and_type_exists <- fl_missing_but_has_length_length %>%
  left_join(ll_for_max_lengths) %>%
  dplyr::filter(length1 == fb_l_type_max | length2 == fb_l_type_max ) %>%
  dplyr::distinct(fishbase_scientific, speccode, fb_length_max, fb_l_type_max)

# Only left with SL and 404 species

# Need to do three more calcs
sl_to_tl_to_fl <- ll_for_max_lengths %>%
  filter(length1 == "TL", length2 == "SL") %>%
  dplyr::rename(known_length = length2,
                unknown_length = length1) %>%
  dplyr::filter(known_length == fb_l_type_max) %>%
  mutate(intermediate_TL = a + b * fb_length_max) %>%
  dplyr::filter(!is.na(intermediate_TL)) %>%
  dplyr::select(fishbase_scientific, fb_length_max, fb_l_type_max, intermediate_TL) %>%
  group_by(fishbase_scientific) %>%
  slice_max(intermediate_TL, with_ties = FALSE) %>%
  left_join(ll_for_max_lengths) %>%
  dplyr::filter(length2 == "FL") %>%
  dplyr::mutate(conversion_possible = if_else(length1 == "TL", TRUE, FALSE)) %>%
  dplyr::filter(!conversion_possible %in% FALSE) %>%
  dplyr::rename(known_length = length2,
                unknown_length = length1) %>%
  dplyr::mutate(fb_length_max = (intermediate_TL - a)/b) %>%
  dplyr::group_by(fishbase_scientific) %>%
  dplyr::slice_max(fb_length_max, with_ties = FALSE) %>%
  dplyr::mutate(max_length_source = paste("FishBase:", known_length, "converted to Fl using multi-step SL->TL->FL"),
                fb_l_type_max = "FL") %>%
  dplyr::ungroup() %>%
  dplyr::filter(!fishbase_scientific %in% all_max_lengths_fl$fishbase_scientific) %>%# don't include species where we have already worked it out
  dplyr::select(fishbase_scientific, fb_length_max, fb_l_type_max, max_length_source) 

test <- fl_missing_but_has_length_length %>%
  filter(!fishbase_scientific %in% sl_to_tl_to_fl$fishbase_scientific)

max_lengths_in_fl <- bind_rows(all_max_lengths_fl, sl_to_tl_to_fl)

other_max_lengths <- info %>%
  dplyr::select(fishbase_scientific, fb_length_max, fb_l_type_max) %>%
  filter(!fishbase_scientific %in% max_lengths_in_fl$fishbase_scientific) %>%
  dplyr::mutate(max_length_source = if_else(!is.na(fb_length_max), "Fishbase", NA))

all_max_lengths <- bind_rows(max_lengths_in_fl, other_max_lengths)

info <- info %>%
  dplyr::select(-c(fb_length_max, fb_l_type_max)) %>%
  left_join(all_max_lengths)

unique(ll$type)

test <- ll %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(n = n())

lwr <- length_weight(validated) %>%
  clean_names() %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::select(species, speccode, lengthmin, lengthmax, type, number, sex, a, b, sea, seb, method, c_code, locality, coeffdetermination)

tidy_lwr <- lwr %>%
  mutate(country_rank = case_when(
    c_code == "036" ~ 1,
    str_detect(locality, "New Zealand") ~ 2)
  ) %>%
  
  mutate(number_rank = case_when(
    number >= 100 ~ 1,
    number >= 20 & number < 100 ~ 2,
    number >= 10 & number < 20 ~ 3,
    number >= 1 & number < 10 ~ 4)
  ) %>%
  
  mutate(type_rank = case_when(
    type %in% "FL" ~ 1,
    type %in% "TL" ~ 2,
    type %in% c("SL","WD","OT","PC","NG","AF","LP") ~ 6)
  ) %>%
  
  mutate(sex_rank = case_when(
    sex %in% c("mixed","unsexed") ~ 1,
    sex %in% c(NA) ~ 2,
    type %in% c("juvenile") ~ 8)
  ) %>%
  
  dplyr::full_join(ll) %>%
  dplyr::mutate(conv_rank = if_else(!is.na(all) & !is.na(bll), 1, 4)) %>%
  replace_na(list(country_rank = 3, number_rank = 5, type_rank = 7, sex_rank = 2)) %>%
  dplyr::mutate(final_rank = type_rank + sex_rank + country_rank * 0.5 + number_rank + conv_rank) %>%
  arrange(species, final_rank) %>%
  dplyr::group_by(species) %>%
  dplyr::slice(which.min(final_rank)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(source_level = "Species specific") %>%
  dplyr::select(species, speccode, type, a, b, all, bll, source_level) %>%
  dplyr::filter(!is.na(a)) %>%
  mutate(all = case_when(type %in% c("FL") ~ 0,
                         !type %in% c("FL") ~ all)) %>%
  mutate(bll = case_when(type %in% c("FL") ~ 1,
                         !type %in% c("FL") ~ bll)) %>%
  dplyr::rename(fishbase_scientific = species)

# bay_lwrs <- data.frame() # turned off for now So i don't loose the data we have already downloaded
bay_lwrs <- read.csv("annotation-schema/data/staging/bayesian_length-weights.csv") %>% 
  distinct()

# Sys.time()
# 
# temp.validated <- as.data.frame(validated) %>%
#   filter(!validated %in% c(unique(bay_lwrs$scientific))) %>%
#   pull(validated)
# 
# for(species in seq(1:length(unique(temp.validated)))){
#   print(species)
# 
#   try(temp_lwr <- find_lw(temp.validated[species]))
#   nrow(temp_lwr)
# 
#   if(!is.null(nrow(temp_lwr))){
#   bay_lwrs <- bind_rows(bay_lwrs, temp_lwr)
#   }
# }
# 
# bay_lwrs <- bay_lwrs %>%
#   distinct()
# Sys.time()
# write.csv(bay_lwrs, "annotation-schema/data/staging/bayesian_length-weights.csv", row.names = FALSE)

bay_lwrs <- read.csv("annotation-schema/data/staging/bayesian_length-weights.csv") %>% 
  distinct() %>%
  dplyr::mutate(source_level = str_replace_all(.$metric, 
                                               c("\\(Sub\\)" = "Sub-",
                                                 " \\s*\\([^\\)]+\\)" = ""))) %>%
  tidyr::separate(source_level, into = c("delete", "keep"), sep = "ased on") %>%
  dplyr::mutate(source_level = paste0("Based on", keep)) %>%
  dplyr::rename(bayesian_a = lwa_m,
                bayesian_b = lwb_m,
                bayesian_source_level = source_level,
                fishbase_scientific = scientific) %>%
  dplyr::select(fishbase_scientific, bayesian_a, bayesian_b, bayesian_source_level) %>%
  dplyr::mutate(type = "TL") %>%
  dplyr::mutate(species = fishbase_scientific) %>%
  left_join(ll) %>%
  dplyr::rename(bayesian_length_measure = type,
                bayesian_all = all,
                bayesian_bll = bll) %>%
  select(-species)

is_not_na <- bay_lwrs %>%
  dplyr::filter(!is.na(bayesian_all))

# TODO need to add TL to the l-w length measure for all the bayesian length-weights, and join in the FL -> TL a_ll and b_ll for the bayesians where possible

complete_lw <- info %>%
  dplyr::select(fishbase_scientific) %>%
  full_join(tidy_lwr) %>%
  left_join(., bay_lwrs) %>%
  dplyr::filter(!is.na(fishbase_scientific)) %>%
  dplyr::mutate(type = if_else(is.na(a), bayesian_length_measure, type)) %>%
  dplyr::mutate(a = if_else(is.na(a), bayesian_a, a)) %>%
  dplyr::mutate(b = if_else(is.na(b), bayesian_b, b)) %>%
  dplyr::mutate(all = if_else(is.na(all), bayesian_all, all)) %>%
  dplyr::mutate(bll = if_else(is.na(bll), bayesian_bll, bll)) %>%
  dplyr::rename(a_ll = all, b_ll = bll) %>%
  dplyr::mutate(source_level = if_else(is.na(source_level), bayesian_source_level, source_level)) %>%
  dplyr::select(fishbase_scientific, type, a, b, a_ll, b_ll, source_level) %>%
  dplyr::rename(length_measure = type)

# Scrape trophic level ----
# trophic_levels <- data.frame() # only use this if you want to run all fish again
trophic_levels <- read_rds("annotation-schema/data/staging/trophic_levels.RDS")
# 
# test <- as.data.frame(validated) %>%
#   filter(!validated %in% c(unique(trophic_levels$species))) %>%
#   dplyr::filter(!validated %in% c("Genus Species", NA))
# 
# temp.validated <- as.data.frame(validated) %>%
#   filter(!validated %in% c(unique(trophic_levels$species))) %>%
#   dplyr::filter(!validated %in% c("Genus Species", NA)) %>%
#   pull(validated)
# 
# for(species in seq(1:length(unique(temp.validated)))){
#   message(paste("getting trophic level info for:", temp.validated[species]))
# 
#   try(temp_tl <- find_tl(temp.validated[species], "de"))
#   nrow(temp_tl)
# 
#   if(!is.null(nrow(temp_tl))){
#     trophic_levels <- bind_rows(trophic_levels, temp_tl) %>%
#       glimpse()
#   }
# }
# 
# trophic_levels <- trophic_levels %>%
#   distinct()

# Sys.time()
# write_rds(trophic_levels, "annotation-schema/data/staging/trophic_levels.RDS")

# trophic_levels <- readRDS( "annotation-schema/data/staging/trophic_levels.RDS")

unique(trophic_levels$trophic_level)
unique(trophic_levels$se)
unique(trophic_levels$metric)

hist(as.numeric(trophic_levels$trophic_level))

tidy_trophic_levels <- trophic_levels %>%
  dplyr::rename(fb_trophic_level = trophic_level,
                fb_trophic_level_se = se,
                fb_trophic_level_source = metric,
                fishbase_scientific = species)


# # Get IUCN status - takes roughly 40 minutes -----
# You will need an IUCN red list API key for this to work
# Go to https://apiv3.iucnredlist.org/api/v3/token
# After getting your key set it as IUCN_REDLIST_KEY in .Renviron.
# For that, use usethis::edit_r_environ()
# IUCN_REDLIST_KEY='youractualkeynotthisstring'
# Remove the hashes from this section to run again

# # Set up parallel backend to use multiple cores
# num_cores <- parallel::detectCores() - 1  # Use all cores except one to avoid overloading the system
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
# 
# # Placeholder for the final data frame
# iucn <- data.frame()
# 
# # Use foreach for parallel processing
# iucn_results <- foreach(species = validated, .combine = 'bind_rows', .packages = c('rredlist', 'dplyr')) %dopar% {
#   if (!species %in% c("NA", NA)) {
#     message(paste("accessing IUCN data for:", species))
#     
#     dat <- tryCatch({
#       rl_search(species)
#     }, error = function(e) {
#       NULL  # Return NULL if there's an error
#     })
#     
#     if (!is.null(dat) && !is.null(nrow(dat[[2]]))) {
#       return(dat[[2]])
#     }
#   }
#   return(NULL)  # Return NULL if no valid data
# }
# 
# # Combine results into the main data frame
# iucn <- bind_rows(iucn_results)
# 
# # Stop the cluster after processing
# stopCluster(cl)
# 
# # Print the final data frame or save it as needed
# print(iucn)


# iucn <- data.frame()
# 
# for (species in validated) {
#   message(paste("accessing IUCN data for:", species, "(species", nrow(iucn), "of", length(validated),")"))
# 
#   if(!species %in% c("NA", NA)){
# 
#     dat <- rl_search(species)
#     temp_dat <- dat[[2]]
# 
#     if(!is.null(nrow(temp_dat))){
#       iucn <- bind_rows(iucn, temp_dat)
#     }
#   }
# }
# 
# # # Format IUCN data
# final_iucn <- iucn %>%
#   dplyr::rename(fishbase_scientific = scientific_name) %>%
#   dplyr::mutate(iucn_ranking = str_replace_all(.$category, c("EX" = "Extinct",
#                                                              "EW" = "Extinct in the Wild",
#                                                              "CR" = "Critically Endangered",
#                                                              "EN" = "Endangered",
#                                                              "VU" = "Vulnerable",
#                                                              "NT" = "Near Threatened",
#                                                              "LC" = "Least Concern",
#                                                              "DD" = "Data Deficient"))) %>%
#   dplyr::select(fishbase_scientific, iucn_ranking) %>%
#   distinct()
# 
# saveRDS(final_iucn, "annotation-schema/data/staging/australia_fish_iucn-categories.RDS")
# 
final_iucn <- readRDS("annotation-schema/data/staging/australia_fish_iucn-categories.RDS")

# check which names don't match
iucn_not_match <- anti_join(final_iucn, code_crosswalk_codes) # none - woo!

# Combine all data ----
all_fishbase <- info %>%
  dplyr::full_join(countries) %>%
  dplyr::full_join(status) %>%
  dplyr::full_join(maturity_final) %>%
  dplyr::full_join(complete_lw) %>%
  dplyr::full_join(tidy_trophic_levels) %>%
  dplyr::select(fishbase_scientific, speccode, 
                fb_length_at_maturity_cm, 
                fb_length_max, 
                fb_l_type_max, 
                max_length_source,
                fb_vulnerability, 
                fb_countries, 
                fb_status, 
                length_measure,
                a,
                b,
                a_ll,
                b_ll,
                fb_trophic_level,
                fb_trophic_level_se,
                fb_trophic_level_source,
                source_level,
                subfamily) %>%
  dplyr::filter(!speccode %in% c("0", NA)) %>% # to remove blank fishbase
  distinct()

double_ups_fbcode <- all_fishbase %>%
  dplyr::group_by(speccode) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

double_ups_fbname <- all_fishbase %>%
  dplyr::group_by(fishbase_scientific) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

# Number of rows in all.fishbase does not match number of rows in info
extra <- anti_join(info %>% dplyr::select(speccode, fishbase_scientific), all_fishbase)

# Join in caab data
final_data <- dplyr::full_join(code_crosswalk_codes, all_fishbase) %>%
  dplyr::left_join(final_iucn) 

missing_trophic_level <- final_data %>%
  dplyr::filter(is.na(fb_trophic_level)) %>%
  dplyr::select(caab_code, caab_scientific, fb_trophic_level)

write.csv(missing_trophic_level, "annotation-schema/data/staging/missing-trophic-level.csv", row.names = F)

saveRDS(final_data, "annotation-schema/data/staging/australia_fish_fishbase-information-and-iucn-category.RDS")

