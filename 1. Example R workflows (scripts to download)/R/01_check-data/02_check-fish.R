# Table of contents
# 1. Load and format metadata
# 2. Extract fishing status - optional?
# 3. Load MaxN data
# 4. Load length & 3D point data
# 5. Complete and tidy datasets, run data checks
# 6. Save datasets

rm(list=ls()) # Clear memory

# Load Libraries ----
library(devtools)
# TODO change this from GlobalArchive package to CheckEM
#install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)
# library(CheckEM)
library(tidyverse)
library(googlesheets4)
library(sf)
library(terra)
devtools::load_all("./") # TODO remove this once have changed to CheckEM package

# Set study name ----
# Change this to suit your project or campaign name. This will also be the prefix on your final saved files.
name <- "example-bruv-workflow"    

# 1. Load metadata ----
# If you have run the habitat script first then you can skip formatting the habitat
# If you have not run the habitat script first then you will need to remove the hashes from the following lines

# metadata <- read_metadata("1. Example R workflows (scripts to download)/data/raw/") %>%
#   dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location,  # Review columns to align with GlobalArchive - plus make metadata match
#                 site, depth, successful_count, successful_length,
#                 successful_habitat_forward, successful_habitat_backward) %>%
#   glimpse()
# 
# saveRDS(metadata, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
#                                 name, "_Metadata.rds"))

metadata <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                name, "_Metadata.rds"))

# 2. Extract fishing status ----
# Use this section if you need to work out which samples are inside areas closed to fishing
# TODO change to CAPAD file
marine.parks <- st_read("1. Example R workflows (scripts to download)/data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")  %>% 
  dplyr::select(geometry, ZONE_TYPE) %>%
  st_transform(4326) %>%
  st_make_valid()

metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), 
                      crs = 4326)

metadata <- metadata_sf %>%
  st_intersection(marine.parks) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
  dplyr::mutate(status = if_else(str_detect(ZONE_TYPE, "National|Sanctuary"), 
                                "No-take", "Fished")) %>%
  clean_names() %>%
  glimpse()

# 3. Load MaxN data ----
points <- CheckEM::read_files_txt("1. Example R workflows (scripts to download)/data/raw/") %>%
  glimpse()

maxn <- points %>% 
  dplyr::group_by(campaignid, sample, filename, periodtime, frame, family, genus, species) %>% # Add stage here if you would like to create MaxN by stage
  dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::summarise(maxn = sum(number)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::slice(which.max(maxn)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(maxn)) %>%
  dplyr::select(-frame) %>%
  tidyr::replace_na(list(maxn = 0)) %>%
  dplyr::mutate(maxn = as.numeric(maxn)) %>%
  dplyr::filter(maxn > 0) %>%
  dplyr::inner_join(metadata) %>%
  dplyr::filter(successful_count %in% c("Yes")) %>% # Remove any samples that were not marked in the metadata as successful for count
  dplyr::filter(maxn > 0) %>%
  glimpse()

# 4. Load length & 3D point data ----
length3dpoints <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",
                             recursive = T,
                             pattern = "_3DPoints.txt|_Lengths.txt",
                             full.names = T) %>%
  purrr::map(~CheckEM::read_files_txt(.)) %>%
  purrr::list_rbind() %>%
  dplyr::select(-c(comment))%>% # take comment out as there is also a comment column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful_length %in% "Yes") %>% # Remove any samples that were not marked in the metadata as successful for length measurements
  glimpse()

# 5. Complete and tidy datasets, run data checks
dat <- maxn %>%
  dplyr::select(campaignid, sample, family, genus, species, maxn) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  replace_na(list(maxn = 0)) %>%
  group_by(sample, family, genus, species) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  ungroup() %>% 
  mutate(scientific = paste(family, genus, species, sep = " "))%>%
  dplyr::select(sample, scientific, maxn)%>%
  spread(scientific, maxn, fill = 0)%>%
  glimpse()

maxn.families <- maxn %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  filter(!(family %in% "Unknown")) %>%
  dplyr::select(c(family, genus, species, scientific)) %>%
  distinct() %>%
  glimpse()

complete.maxn <- dat %>%
  pivot_longer(names_to = "scientific", values_to = "maxn",
               cols = 2:ncol(.)) %>%
  inner_join(maxn.families, by = c("scientific")) %>%
  full_join(metadata)%>%
  glimpse()

length.families <- length3dpoints %>%
  dplyr::filter(!(family %in% "Unknown")) %>%
  dplyr::select(family, genus, species)%>%
  distinct() %>%
  glimpse()

complete.length.number <- length3dpoints %>% 
  dplyr::filter(!family %in% "Unknown")%>%
  dplyr::right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  dplyr::select(campaignid, sample, family, genus, species, length, number, range) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  dplyr::mutate(number = as.numeric(number)) %>%
  replace_na(list(number = 0)) %>% # We add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup() %>%
  dplyr::filter(!is.na(number)) %>% # this should not do anything
  dplyr::mutate(length = as.numeric(length)) %>%
  left_join(.,metadata) %>%
  tidyr::uncount(number) %>%
  glimpse()

# 6. Save datasets ----
saveRDS(complete.maxn, 
          file = paste0("1. Example R workflows (scripts to download)/data/staging/",
                       name, "_Complete-maxn.rds"))

saveRDS(complete.length.number, 
          file = paste0("1. Example R workflows (scripts to download)/data/staging/",
                       name, "_Complete-length.rds"))