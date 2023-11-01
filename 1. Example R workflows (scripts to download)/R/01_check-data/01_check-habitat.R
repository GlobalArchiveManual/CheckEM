# Error checking of stereo-BRUV habitat data exported from TransectMeasure ----

# This script is designed to be used interactively to find basic annotation errors that should be made to original EventMeasure (.EMObs) or generic annotation files AND for subsequent data analysis

# NOTE: ERRORS SHOULD BE FIXED IN THE .TMObs AND THE SCRIPT RE-RAN!

# NOTE: This example dataset has been analysed using the standard 'TM schema_BROAD.MORPH.TYPE.txt' file
# Follow the notes to edit the script for use with the broad only 'TM schema_BROAD.txt' and fine 'TM schema_BROAD.MORPH.TYPE.FINE.txt' files 

### OBJECTIVES ###
# 1. Import data and run basic error reports
# 2. Run more thorough checks on the data against the metadata and images in the original directory
# 3. Tidy data into broad and detailed point-level and percent-cover data
# 4. Tidy the final data into organised dataframes
# 5. Inspect for tidy data for any errors
# 6. Export tidy datasets to a .csv format suitable for use in modelling and statistical testing
# 7. Visualise the data spatially

# Please forward any feedback and improvements to claude.spencer@uwa.edu.au or raise an issue in the "forward-facing-benthic-composition-annotation" GitHub repository

# Clear memory
rm(list = ls())

## Libraries required
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive")                                 # Run this once to install the GlobalArchive package
library(GlobalArchive)
devtools::load_all("./")

# To tidy data
library(tidyverse)

# To visualise data
library(ggplot2)
library(ggbeeswarm)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(todor)

# Set study name 
name <- "example-bruv-workflow"

### 1. Import data and run basic error reports ----
# New version of 'ga.read.files_em.csv
# Reads in metadata
ga.read.files_em.csv <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c"))%>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid, c("_Metadata.csv" = "")))
}

# 1. Load and format metadata ----
metadata <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",      # This replaces ga.list.files
                       recursive = F,
                       pattern = "_Metadata.csv",
                       full.names = T) %>%
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location,  # Review columns to align with GlobalArchive - plus make metadata match
                site, depth, successful_count, successful_length, 
                successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()                                                                   # Preview the data

saveRDS(metadata, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                  name, "_Metadata.rds"))

# Read in the raw habitat data
read_tm <- function(dir, sample) {
  if (sample %in% "opcode") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      # dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = opcode) %>%
      glimpse()
  }
  
  else if (sample %in% "period") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      # dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = period) %>%
      glimpse()
  }
  
  else {
    stop("Sample must be one of: c('opcode', 'period')")
  }}

points <- read_tm("1. Example R workflows (scripts to download)/data/raw/",
                               sample = "opcode")

habitat <- points %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample,                                         # Should I be keeping in relief_annotated?
                starts_with("level"), scientific, caab_code) %>%
  glimpse()

relief <- points %>%
  dplyr::filter(relief_annotated %in% "yes") %>%
  dplyr::select(campaignid, sample,                                         # Should I be keeping in relief_annotated?
                starts_with("level"), scientific, caab_code) %>%
  glimpse()

# Check to see if you have samples with points extra or missing points annotated
# Set the number of points per image
num.points <- 20

wrong.points.habitat <- habitat %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" &
                                       successful_habitat_backward %in% "Yes" ~ num.points * 2,
                                     successful_habitat_forward %in% "Yes" &
                                       successful_habitat_backward %in% "No" ~ num.points * 1,
                                     successful_habitat_forward %in% "No" &
                                       successful_habitat_backward %in% "Yes" ~ num.points * 1,
                                     successful_habitat_forward %in% "No" &
                                       successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

wrong.points.relief <- relief %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" &
                                       successful_habitat_backward %in% "Yes" ~ num.points * 2,
                                     successful_habitat_forward %in% "Yes" &
                                       successful_habitat_backward %in% "No" ~ num.points * 1,
                                     successful_habitat_forward %in% "No" &
                                       successful_habitat_backward %in% "Yes" ~ num.points * 1,
                                     successful_habitat_forward %in% "No" &
                                       successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()


# If you have samples with missing points, you need to rectify this in the original .TMObs file!
habitat.missing.metadata <- anti_join(habitat, metadata, by = c("campaignid", "sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat, by = c("campaignid", "sample")) %>%
  glimpse()

###   STOP     AND    READ      THE     NEXT      PART     ###


# We strongly encourage you to fix these errors at the source (i.e. TMObs)
# Now check through the files in your "Errors to check" folder and make corrections to .TMObs / generic files and then re-run this script

# TODO replace schema with file loaded in package 
schema <- read_csv("1. Example R workflows (scripts to download)/data/raw/benthic.annotation.schema.forward.facing.20230714.135113.csv",
                   col_types = "c", na = "") %>%
  ga.clean.names() %>%
  dplyr::select(-c(parent_caab, qualifiers)) %>%
  glimpse()

### 3. Tidy the final data into organised dataframes and save ----
tidy.habitat <- habitat %>%
  dplyr::mutate(number = 1) %>%                                                 # Add a count column to summarise the number of points
  left_join(schema) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species) %>%
  dplyr::tally(number, name = "number") %>%
  ungroup() %>%                                                                 # Ungroup
  # ga.clean.names() %>%                                                          # Clean names using GlobalArchive function
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()                                                      # Preview the data

saveRDS(tidy.habitat, file = paste0("1. Example R workflows (scripts to download)/data/staging/",
                                     name, "_Habitat.rds"))

# Create relief
tidy.relief <- relief %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%                       # Remove Open water and Unknown entries from broad
  dplyr::mutate(number = 1) %>%                   
  group_by(campaignid, sample, across(starts_with("level")), caab_code) %>%
  dplyr::tally(number, name = "number") %>%                                                    
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

saveRDS(tidy.relief, file = paste0("1. Example R workflows (scripts to download)/data/staging/",
                                      name, "_Relief.rds"))
