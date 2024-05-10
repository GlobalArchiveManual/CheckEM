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
library(stringr)

# Use the list of Australian species created in the second script
caab <- readRDS("annotation-schema/data/staging/australia_fish_caab-with-regions.RDS") %>%
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


# trophic_levels <- data.frame() # turned off for now So i don't loose the data we have already downloaded
trophic_levels <- read_rds("annotation-schema/data/staging/trophic_levels.RDS")

# Sys.time()
# 
# temp.validated <- as.data.frame(validated) %>%
#   filter(!validated %in% c(unique(trophic_levels$scientific))) %>%
#   pull(validated)
# 
# for(species in seq(1:length(unique(temp.validated)))){
#   message(paste("getting trophic level info for number: ", species))
# 
#   try(temp_tl <- find_tl(temp.validated[species]))
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
# 
# Sys.time()
# write_rds(trophic_levels, "annotation-schema/data/staging/trophic_levels.RDS")

unique(trophic_levels$trophic_level)
unique(trophic_levels$se)
unique(trophic_levels$metric)

hist(as.numeric(trophic_levels$trophic_level))
