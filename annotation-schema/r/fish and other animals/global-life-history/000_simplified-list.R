# This script uses the taxon list from WORMS, to:
# - Create a list of marine animals in the world
# - Filter the list to species that are more commonly observed or annotated on stereo imagery
# Find their distribution using the worms package
# Map that distribution to the FAO major fishing areas

# Load the required libraries
library(tidyverse)
library(worrms)
library(mregions2)
library(CheckEM)
library(sf)
library(rfishbase)

# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fao <- st_read("annotation-schema/data/spatial/FAO_major_areas.shp")
st_crs(fao) <- wgs.84
fao <- st_as_sf(fao)

fao$NAME_EN <- str_replace_all(fao$NAME_EN, c(", " = "_", " " = "."))

unique(fao$NAME_EN)

# Read in species list 
worms_raw <- read_delim("annotation-schema/data/raw/taxon.txt") %>%
  dplyr::filter(taxonomicStatus %in% "accepted") %>%
  dplyr::filter(!is.na(genus)) %>%
  dplyr::filter(!is.na(order)) %>%
  dplyr::filter(taxonRank == "Species")

worms_all <- worms_raw %>%
  dplyr::filter(kingdom %in% "Animalia") %>%
  filter(!str_detect(order, "\\[unassigned\\] Caenogastropod")) %>%
  dplyr::filter(!is.na(class)) %>%
  clean_names() %>%
  dplyr::select(taxonid, scientificname, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename(scientific_name = scientificname) %>%
  dplyr::mutate(taxonid = str_replace_all(taxonid, "urn:lsid:marinespecies.org:taxname:", "")) %>%
  dplyr::filter(!is.na(taxonid)) %>%
  dplyr::rename(aphia_id = taxonid)

# Get a list of all fish species in the world from fishbase ----
all_species <- load_taxa() %>% 
  clean_names() %>%
  dplyr::mutate(scientific = species) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::select(speccode, superclass, class, order, family, genus, species, scientific) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

# Get a list of all fish species in the world from fishbase ----
all_species <- load_taxa() %>% 
  clean_names() %>%
  dplyr::mutate(scientific = species) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::select(speccode, superclass, class, order, family, genus, species, scientific) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

# Format basic info and min + max depths ----
info <- species(all_species$scientific) %>% 
  clean_names() %>%
  
  dplyr::rename(scientific_name = species, 
                #fb_common_name = fbname,
                fb_length_max = length, 
                fb_length_max_type = ltypemaxm,
                fb_speccode = speccode,
                
                fb_depth_range_shallow = depthrangeshallow,
                fb_depth_range_deep = depthrangedeep,
                fb_commerical_importance = importance
  ) %>% # Length metrics are in cm
  
  dplyr::mutate(fb_max_published_weight_kg = weight/1000) %>%
  dplyr::select(scientific_name, 
                fb_speccode, 
                #fb_common_name, 
                fb_length_max,  # TODO what is the unit
                fb_length_max_type, 
                fb_commerical_importance,
                fb_depth_range_deep,  # TODO what is the unit
                fb_depth_range_shallow,   # TODO what is the unit
                fb_max_published_weight_kg
  ) %>%
  
  dplyr::mutate(fb_speccode = as.character(fb_speccode)) %>%
  dplyr::mutate(fb_length_max_type = toupper(fb_length_max_type)) %>%# make them all capitals
  dplyr::filter(!grepl("[^a-zA-Z0-9 .]", scientific_name)) %>%
  glimpse()


unique(info$fb_length_max_type)


# Function to find values with special characters
find_special_values <- function(column) {
  column[grepl("[^a-zA-Z0-9 ]", column)]  # Extract values with special characters
}

# Apply to each column
special_values <- lapply(info, find_special_values)
special_values


combined <- full_join(worms_all, info)


missing_aphiaid <- combined %>%
  filter(is.na(aphia_id))

animalia_id <- wm_records_name(name = "Animalia")$AphiaID

animalia_species <- wm_children(2765220)

groups <- c("Pisces", "Mollusca", "Crustacea")  # Replace with your groups
species_list <- lapply(groups, function(g) {
  id <- wm_records_name(name = g)$AphiaID
  wm_children(id)
})

# Combine into one dataframe
all_species <- do.call(rbind, species_list)

head(all_species)
