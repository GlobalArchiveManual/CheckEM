# This script uses the latest CAAB download from the CSIRO to:
# - validate names using rfishbase
# - Get information on the size of maturity, maximum sizes, IUCN categories and length-weight parameters

# Load the required libraries
library(tidyverse)
library(rfishbase)
library(openssl)
library(GlobalArchive)
library(taxize) # For IUCN
library(rredlist)

# Use the list of Australian species created in the second script
caab <- readRDS("annotation-schema/data/staging/caab-with-regions.RDS") %>%
  dplyr::mutate(species = str_replace_all(.$species, c("[^[:alnum:]]" = "",
                                                       "cfizaspilotai" = "zaspilota",
                                                       "icfifilamentosa" = "filamentosa"))) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(!species == "spp") 

# Validate scientific names to use in fishbase package ----
validated <- rfishbase::validate_names(caab$scientific_name)

codes <- rfishbase::common_names(validated) %>%
  clean_names() %>%
  distinct(species, speccode) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  filter(!is.na(fishbase_scientific)) %>%
  dplyr::mutate(speccode = as.character(speccode))

## This is slow but essential for figuring out what fishbase name = CAAB
## Remove the hashes to run again
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

still_missing <- anti_join(double_fb %>% select(speccode), true_matches)

# Download maturity data ----
# Filter to only Fork length measurements
# Remove any that are NA
# Average the length at maturity (cm)
# Then change synonyms

maturity <- maturity(validated) %>%
  clean_names() %>%
  # filter(type1 %in% "FL") %>% # Would be good to turn this on but it gets rid of a lot of species.
  filter(!is.na(lm)) %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(fb_length_at_maturity_cm = mean(lm)) %>%
  dplyr::rename(fishbase_scientific = species) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::ungroup()

# Get FB.Vulnerability, FB.Length_MAX and FB.LTypeMaxM, information from FishBase ----
info <- species(validated) %>% 
  clean_names() %>%
  dplyr::rename(FB.Length_MAX = length, 
                FB.LTypeMaxM = ltypemaxm,
                FB.Vulnerability = vulnerability) %>% # Length metrics are in cm
  dplyr::select(species, speccode, fbname, FB.Length_MAX, FB.LTypeMaxM, FB.Vulnerability, subfamily) %>%
  dplyr::rename(fishbase.scientific = species, 
                fb.common.name = fbname) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::filter(!fishbase.scientific %in% ("Genus Species"))

# FB.countries and FB.Status ----
country.dat <- country(validated) %>%
  ga.clean.names()%>%
  dplyr::mutate(speccode = as.character(speccode))

status <- country.dat %>%
  dplyr::filter(.$country %in% "Australia") %>%
  dplyr::select(species, speccode, status) %>%
  dplyr::rename(fishbase.scientific = species, 
                FB.Status = status) 

countries <- country.dat %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(FB.countries = toString(country)) %>%
  dplyr::rename(fishbase.scientific = species) %>%
  dplyr::ungroup()

# a and b values ----
ll <- length_length(validated) %>%
  ga.clean.names() %>%
  dplyr::select(species, length1, length2, a, b)%>%
  dplyr::filter(length2 == "FL")%>%
  dplyr::rename(all = a)%>%
  dplyr::rename(bll = b)%>%
  dplyr::rename(type = length1)%>%
  dplyr::group_by(species, type)%>%
  dplyr::summarise(all = mean(all), bll = mean(bll)) %>%
  dplyr::filter(!type %in% c("SL", "Other"))

unique(ll$type)

test <- ll %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(n = n())

lwr <- length_weight(validated) %>%
  ga.clean.names() %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::select(species, speccode, lengthmin, lengthmax, type, number, sex, a, b, sea, seb, method, c_code, locality, coeffdetermination)

tidy.lwr <- lwr %>%
  mutate(country.rank = case_when(
    c_code == "036" ~ 1,
    str_detect(locality, "New Zealand") ~ 2)
  ) %>%
  
  mutate(number.rank = case_when(
    number >= 100 ~ 1,
    number >= 20 & number < 100 ~ 2,
    number >= 10 & number < 20 ~ 3,
    number >= 1 & number < 10 ~ 4)
  ) %>%
  
  mutate(type.rank = case_when(
    type %in% "FL" ~ 1,
    type %in% "TL" ~ 2,
    type %in% c("SL","WD","OT","PC","NG","AF","LP") ~ 6)
  ) %>%
  
  mutate(sex.rank = case_when(
    sex %in% c("mixed","unsexed") ~ 1,
    sex %in% c(NA) ~ 2,
    type %in% c("juvenile") ~ 8)
  ) %>%
  
  dplyr::full_join(ll) %>%
  dplyr::mutate(conv.rank = if_else(!is.na(all) & !is.na(bll), 1, 4)) %>%
  replace_na(list(country.rank = 3, number.rank = 5, type.rank = 7, sex.rank = 2)) %>%
  dplyr::mutate(final.rank = type.rank + sex.rank + country.rank * 0.5 + number.rank + conv.rank) %>%
  arrange(species, final.rank) %>%
  dplyr::group_by(species) %>%
  dplyr::slice(which.min(final.rank)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(source.level = "Species specific") %>%
  dplyr::select(species, speccode, type, a, b, all, bll, source.level) %>%
  dplyr::filter(!is.na(a)) %>%
  mutate(all = case_when(type %in% c("FL") ~ 0,
                         !type %in% c("FL") ~ all)) %>%
  mutate(bll = case_when(type %in% c("FL") ~ 1,
                         !type %in% c("FL") ~ bll)) %>%
  dplyr::rename(fishbase.scientific = species)

bay_lwrs <- read.csv("data/bayesian_length-weights.csv") %>% distinct() %>%
  dplyr::mutate(source.level = str_replace_all(.$metric, 
                                               c("\\(Sub\\)" = "Sub-",
                                                 " \\s*\\([^\\)]+\\)" = ""))) %>%
  tidyr::separate(source.level, into = c("delete", "keep"), sep = "ased on") %>%
  dplyr::mutate(source.level = paste0("Based on", keep)) %>%
  dplyr::rename(bayesian.a = lwa_m,
                bayesian.b = lwb_m,
                bayesian.source.level = source.level,
                fishbase.scientific = scientific) %>%
  dplyr::select(fishbase.scientific, bayesian.a, bayesian.b, bayesian.source.level)

complete.lw <- info %>%
  dplyr::select(fishbase.scientific) %>%
  full_join(tidy.lwr) %>%
  left_join(., bay_lwrs) %>%
  dplyr::filter(!is.na(fishbase.scientific)) %>%
  dplyr::mutate(a = if_else(is.na(a), bayesian.a, a)) %>%
  dplyr::mutate(b = if_else(is.na(b), bayesian.b, b)) %>%
  dplyr::mutate(source.level = if_else(is.na(source.level), bayesian.source.level, source.level)) %>%
  dplyr::select(fishbase.scientific, type, a, b, all, bll, source.level) %>%
  dplyr::rename(aLL = all, bLL = bll, Length.measure = type, Source_Level = source.level)

# # # Get IUCN status - takes roughly 40 minutes
# iucn <- data.frame()
# 
# for (species in validated) {
#   message(paste("accessing IUCN data for:", species, "(species", nrow(iucn), "of", length(validated),")"))
# 
#   if(!species %in% c("NA", NA)){
# 
#   dat <- rl_search(species)
#   temp.dat <- dat[[2]]
# 
#   if(!is.null(nrow(temp.dat))){
#   iucn <- bind_rows(iucn, temp.dat)
#   }
#   }
# }
# 
# # Format IUCN data
# final.iucn <- iucn %>%
#   dplyr::rename(fishbase.scientific = scientific_name) %>%
#   dplyr::mutate(IUCN.ranking = str_replace_all(.$category, c("EX" = "Extinct",
#                                                              "EW" = "Extinct in the Wild",
#                                                              "CR" = "Critically Endangered",
#                                                              "EN" = "Endangered",
#                                                              "VU" = "Vulnerable",
#                                                              "NT" = "Near Threatened",
#                                                              "LC" = "Least Concern",
#                                                              "DD" = "Data Deficient"))) %>%
# 
#   dplyr::select(fishbase.scientific, IUCN.ranking) %>%
#   distinct()
# # 
# saveRDS(final.iucn, "data/final.iucn.RDS")
final.iucn <- readRDS("data/final.iucn.RDS")

# check which names don't match
iucn.not.match <- anti_join(final.iucn, code.crosswalk.codes) # none - woo!

# Combine all data ----
all.fishbase <- info %>%
  dplyr::full_join(countries) %>%
  dplyr::full_join(status) %>%
  dplyr::full_join(maturity) %>%
  dplyr::full_join(complete.lw) %>%
  dplyr::select(fishbase.scientific, speccode, 
                fb.length.at.maturity.cm, 
                FB.Length_MAX, 
                FB.LTypeMaxM, 
                FB.Vulnerability, 
                FB.countries, 
                FB.Status, 
                Length.measure,
                a,
                b,
                aLL,
                bLL,
                Source_Level,
                subfamily) %>%
  dplyr::filter(!speccode %in% c("0", NA)) # to remove blank fishbase

double.ups.fbcode <- all.fishbase %>%
  dplyr::group_by(speccode) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

double.ups.fbname <- all.fishbase %>%
  dplyr::group_by(fishbase.scientific) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

# Number of rows in all.fishbase does not match number of rows in info
extra <- anti_join(info %>% dplyr::select(speccode, fishbase.scientific), all.fishbase)

# Join in caab data
final.data <- dplyr::full_join(code.crosswalk.codes, all.fishbase) %>%
  dplyr::left_join(final.iucn) 

saveRDS(final.data, "data/fishbase.and.iucn.RDS")
