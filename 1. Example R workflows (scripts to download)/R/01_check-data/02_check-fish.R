rm(list=ls()) # Clear memory

# Load Libraries ----
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") # to check for updates
library(GlobalArchive)
library(tidyverse)
library(googlesheets4)
library(sf)
library(terra)

# Table of contents
# 1. Load and format metadata
# 2. Extract fishing status - optional?
# 3. Load MaxN data
# 4. Load length & 3D point data
# 5. Complete and tidy datasets, run data checks
# 6. Save datasets

# Set study name ----
# Change this to suit your name name. This will also be the prefix on your final saved files.
name <- "example-bruv-workflow"    

# New version of 'ga.read.files_em.csv
ga.read.files_em.csv <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c"))%>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    ga.clean.names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid, c("_Metadata.csv" = "")))
}

# 1. Load and format metadata ----
metadata <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",      # This replaces ga.list.files
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T) %>%
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::select(campaignid, sample, latitude, longitude, date.time, location,  # Review columns to align with GlobalArchive - plus make metadata match
                site, depth, observer.count, observer.length, successful.count, successful.length, 
                successful.habitat.forward, successful.habitat.backward) %>%
  glimpse()

# Only need to run this if you don't have habitat data!
# write.csv(metadata, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
#                                   name, "_Metadata.csv"), row.names = F)

# 2. Extract fishing status ----
marine.parks <- st_read("1. Example R workflows (scripts to download)/data/spatial/shapefiles/marine-parks-all.shp")  %>% 
  dplyr::select(geometry, ZoneName) %>%
  st_transform(4326) %>%
  st_make_valid()

metadata_sf <- st_as_sf(metadata, coords = c("longitude", "latitude"), 
                      crs = 4326)

metadata <- metadata_sf %>%
  st_intersection(marine.parks) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude = X, latitude = Y) %>%
  dplyr::mutate(status = if_else(ZoneName %in% c("National Park Zone", "Sanctuary Zone", "Marine National Park Zone"), 
                                "No-take", "Fished")) %>%
  glimpse()

# 3. Load MaxN data ----
ga.read.files_em.txt <- function(flnm) {
  read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    ga.clean.names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Points.txt"="","_3DPoints.txt"="","_Lengths.txt"=""))) %>%
    dplyr::rename(sample = opcode)
}

points <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",      # This replaces ga.list.files
                     recursive = T,
                     pattern = "_Points.txt",
                     full.names = T) %>%# list all files ending in "Lengths.txt"
  purrr::map(~ga.read.files_em.txt(.)) %>%
  purrr::list_rbind() %>%
  glimpse()

maxn <- points %>%
  dplyr::group_by(campaignid, sample, filename, periodtime, frame, family, genus, species)%>%
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
  dplyr::filter(successful.count %in% c("Yes"))%>%
  dplyr::filter(maxn > 0) %>%
  glimpse()

# 4. Load length & 3D point data ----
length3dpoints <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",
                             recursive = T,
                             pattern = "_3DPoints.txt|_Lengths.txt",
                             full.names = T) %>%
  purrr::map(~ga.read.files_em.txt(.)) %>%
  purrr::list_rbind() %>%
  dplyr::select(-c(time,comment))%>% # take time out as there is also a time column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.length %in% "Yes") %>%
  glimpse()

# 5. Complete and tidy datasets, run data checks
dat <- maxn %>%
  dplyr::select(campaignid,sample,family,genus,species,maxn) %>%
  tidyr::complete(nesting(campaignid,sample),nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0)) %>%
  group_by(sample,family,genus,species) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  ungroup() %>% 
  mutate(scientific = paste(family, genus, species, sep = " "))%>%
  dplyr::select(sample, scientific, maxn)%>%
  spread(scientific, maxn, fill = 0)%>%
  glimpse()

maxn.families <- maxn %>%
  dplyr::mutate(scientific = paste(family, genus, species,sep=" "))%>%
  filter(!(family %in% "Unknown"))%>%
  dplyr::select(c(family, genus, species, scientific))%>%
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
  dplyr::filter(successful.length %in% "Yes") %>%
  dplyr::select(campaignid, sample, family, genus, species, length, number, range) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  dplyr::mutate(number = as.numeric(number)) %>%
  replace_na(list(number = 0)) %>% # We add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup() %>%
  dplyr::filter(!is.na(number)) %>% # this should not do anything
  dplyr::mutate(length = as.numeric(length)) %>%
  left_join(.,metadata) %>%
  glimpse()

expanded.length <- complete.length.number %>%
  dplyr::filter(!is.na(length)) %>%
  tidyr::uncount(number) %>%
  glimpse()

# 6. Save datasets ----
write.csv(complete.maxn, 
          file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                       name, "_Complete-maxn.csv"), 
          row.names = FALSE)

write.csv(complete.length.number, 
          file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                       name, "_Complete-length.csv"), 
          row.names = FALSE)

write.csv(expanded.length, 
          file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                       name, "_Expanded-length.csv"), 
          row.names = FALSE)
