# This script uses the latest CAAB download from the CSIRO to:
# - Create a list of animals in Australia, then
# - Finds the AphiaID using the taxize package, then
# - Gets the distribution by AphiaID from the worrms package
# - Converts that distribution into presence in Australia's marine bioregions

# Load the required libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(CheckEM)
library(readxl)
library(rvest)
library(mregions)
library(mregions2)
library(worrms)
library(sf)
library(beepr)
library(taxize)
library(progressr)   # progress bars for purrr/future loops
library(purrr)
sf_use_s2(FALSE)

# Spatial files ----
# set the projection
wgs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Read in Australia's marine regions ----
# aus_regions <- st_read("annotation-schema/data/spatial/marine_regions.shp") %>%
#   dplyr::select(-c(OBJECTID))
# 
# aus_regions$region <- as.character(aus_regions$REGION)
# st_crs(aus_regions) <- wgs_84
# aus_regions <- st_as_sf(aus_regions)

aus_regions <- st_read("annotation-schema/data/spatial/marine_regions_with_gbr.shp")

aus_regions$region <- as.character(aus_regions$FULL_NAME)
st_crs(aus_regions) <- wgs_84
aus_regions <- st_as_sf(aus_regions)

imcra_regions <- st_read("annotation-schema/data/spatial/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.shp")

imcra_regions$region <- as.character(imcra_regions$PB_NAME)
st_crs(imcra_regions) <- wgs_84
imcra_regions <- st_as_sf(imcra_regions)

# Read in the latest CAAB dump from CSIRO website ----
# Download is available here: https://www.marine.csiro.au/datacentre/caab/caab_dump_latest.xlsx
# We have removed some phylums and classes
caab_og <- read_excel("annotation-schema/data/raw/caab_dump_latest.xlsx")

caab <- read_excel("annotation-schema/data/raw/caab_dump_latest.xlsx") %>%
  clean_names() %>%
  dplyr::filter(kingdom %in% "Animalia") %>%
  dplyr::filter(phylum %in% c("Mollusca",
                              "Chordata",
                              "Arthropoda",
                              "Echinodermata")) %>%
  dplyr::filter(rank %in% c("Family", "Genus", "Species")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::filter(!non_current_flag %in% c("Y", "T")) %>%
  dplyr::filter(!class %in% c("Teleostei", # remove fish as have other list
                              "Chondrostei", # remove fish
                              "Cladistii", # remove fish
                              "Coelacanthi", # remove fish
                              "Holocephali", # remove fish
                              "Holostei", # remove fish
                              "Elasmobranchii", # remove sharks
                              "Myxini", # remove fish
                              "Anthozoa", # remove sea anemones and corals - need to talk to Tim
                              "Appendicularia", # remove free swimming tunicates
                              "Ascidiacea", # ascidians
                              "Branchiopoda", # small freshwater shrimps
                              "Cnidaria incertae sedis", # unknown
                              "Mollusca incertae sedis", # unknown
                              "Copepoda",
                              "Crinoidea", # crinoids
                              "Hydrozoa",
                              "Ostracoda", # tiny seed shrimp
                              "Symphyla",
                              "Pauropoda",
                              "Cephalocarida",
                              "Diplopoda",
                              "Remipedia",
                              "Tantulocarida",
                              "Chilopoda",
                              "Ichthyostraca",
                              "Thaliacea",
                              "Caudofoveata",
                              "Solenogastres",
                              "Scaphopoda",
                              "Maxillopoda",
                              "Actinopterygii", # fish
                              "Cephalaspidomorphi", #fish
                              "Sarcopterygii", # fish
                              "", NA)) %>%
  dplyr::rename(caab = spcode, order = order_name) %>%
  dplyr::select(caab, kingdom, phylum, class, order, family, genus, species, scientific_name, common_name) %>%
  filter(!grepl("[^A-Za-z]", order)) %>%
  tidyr::separate(family, into = c("family", sep = ":")) %>%
  filter(!grepl("[^A-Za-z]", family)) %>%
  filter(!grepl("[^A-Za-z]", genus)) %>%
  filter(!grepl("[^A-Za-z]", species))


unique(caab$rank)

# Look at the unique values
unique(caab$kingdom)
unique(caab$phylum)
unique(caab$class)
unique(caab$order)
unique(caab$family)
unique(caab$genus)

# Check for any duplicates for the same caab code ----
duplicates <- caab %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n >1)

# If there are duplicates we need to remove them
caab <- caab %>%
  filter(!scientific_name %in% duplicates$scientific_name)

# Check for any duplicates for the same caab code ----
duplicates <- caab %>%
  dplyr::group_by(caab) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n >1)

# Create a list of unique species to get AphiaIDs ----
species <- unique(caab$scientific_name) %>% sort()

get_wormsid(c("Octopodidae"))

# Get AphiaIDs using taxize package for every species ----
# takes just under 3 hours to run - unhash to run again
# system.time({ aphia_ids_list <- get_wormsid_(c(species), fuzzy = FALSE) })
# write_rds(aphia_ids_list, "annotation-schema/data/staging/australia_animals_aphia-ids-list.RDS")

aphia_ids_list <- readRDS("annotation-schema/data/staging/australia_animals_aphia-ids-list.RDS")

# remove tibbles that are null
aphia_ids_list <- aphia_ids_list[!sapply(aphia_ids_list, is.null)]

# Apply the standardization function to each tibble in the list
standardized_names <- map(aphia_ids_list, standardize_tibble)

# Combine the standardized tibbles into a single dataframe
aphia_ids <- bind_rows(standardized_names)

# Remove any duplicate IDs
duplicates_ids <- aphia_ids %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n >1)

aphia_ids <- aphia_ids %>%
  filter(!scientific_name %in% duplicates_ids$scientific_name) %>%
  dplyr::select(aphiaid, scientific_name)

# Combine caab and AphiaIDs
caab_worms <- left_join(caab, aphia_ids) 

duplicates_caab <- caab_worms %>%
  dplyr::group_by(caab) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n >1) # all good

test <- caab_worms %>%
  filter(caab %in% "25154911") %>%
  clean_names() %>%
  dplyr::filter(!class %in% c("Tentaculata", "Nuda")) %>%
  dplyr::filter(!family %in% c("", NA, NULL)) %>%
  tidyr::separate(scientific_name, into = c("genus", "species"), extra = "merge") %>%
  dplyr::mutate(species = str_replace_all(species, "spp.", "spp")) %>%
  dplyr::mutate(genus = if_else(family %in% genus, "Unknown", genus)) %>%
  dplyr::mutate(species = if_else(genus %in% "Unknown", "spp", species)) %>%
  dplyr::select(-x) %>%
  distinct() %>%
  filter(!apply(across(c(caab, kingdom, phylum, class, family, genus, species), as.character), 1, has_special_characters)) %>%
  glimpse()

clean_caab <- caab_worms %>%
  clean_names() %>%
  dplyr::filter(!class %in% c("Tentaculata", "Nuda")) %>%
  dplyr::filter(!family %in% c("", NA, NULL)) %>%
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, "Arctocephalus pusillus doriferus", "Arctocephalus pusillusdoriferus")) %>%
  tidyr::separate(scientific_name, into = c("genus", "species"), extra = "merge") %>%
  dplyr::mutate(species = str_replace_all(species, "spp.", "spp")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  dplyr::mutate(species = if_else(genus == "Unknown", "spp", species)) %>%
  dplyr::select(-x) %>%
  distinct() %>%
  filter(!apply(across(c(caab, kingdom, phylum, class, family, genus, species), as.character), 1, has_special_characters)) %>% # Remove all rows containing special characters
  dplyr::mutate(species = str_replace_all(species, "pusillusdoriferus", "pusillus doriferus"))

species_with_id <- clean_caab %>%
  filter(!aphiaid %in% c(NULL, "", NA)) 

# Get distribution of each aphiaID ----
# takes ~ 2 hours to run (unhash to rerun)
# ids <- unique(species_with_id$aphiaid)
# 
# system.time({distributions <- wm_distribution_(id = c(as.numeric(ids))) %>%
#   dplyr::mutate(mrgid = as.integer(str_replace_all(locationID, "http://marineregions.org/mrgid/", "")))})
# 
# saveRDS(distributions, "annotation-schema/data/staging/australia_animals_distributions-from-worms.RDS")

distributions <- readRDS("annotation-schema/data/staging/australia_animals_distributions-from-worms.RDS") %>%
  distinct()

# Get shapefiles for the marine regions using the mregion package ----
# mregions has now changed to mregions2

unique_mrgids <- distributions %>%
  distinct(mrgid)

test <- (unique(unique_mrgids$mrgid))[1:10]

polygons <- gaz_geometry(test[1], format = "sf")
plot(polygons)

aus_eez <- gaz_geometry(8323, format = "sf")

plot(aus_eez)

# -------------------------------------------------------------------------
# 1.  Prepare the list of unique mrgids
# -------------------------------------------------------------------------
distributions <- readRDS("annotation-schema/data/staging/australia_animals_distributions-from-worms.RDS") %>% 
  distinct()

unique_mrgids <- distributions %>% 
  distinct(mrgid) %>%               # keep one of each
  pull(mrgid)                   # get as a plain vector

# unique_mrgids <- unique_mrgids[1:10]   # for testing

# -------------------------------------------------------------------------
# 2.  Helper function: one call to gaz_geometry() with error handling
# -------------------------------------------------------------------------
fetch_geom <- function(id) {
  tryCatch(
    {
      gaz_geometry(id, format = "sf") %>% 
        mutate(mrgid = id)           # keep the id with the geometry
    },
    error = function(e) {
      message("⚠️  mrgid ", id, " failed: ", e$message)
      NULL                            # return nothing but keep loop alive
    }
  )
}

# -------------------------------------------------------------------------
# 3.  Set up a nice console progress bar and loop through ids
# -------------------------------------------------------------------------
handlers("txtprogressbar")            # pick your favourite handler; this is base‑R‑like

all_polygons <- with_progress({
  map_dfr(unique_mrgids, fetch_geom)  # one row (or more) per mrgid, combined as sf
})

# polygons is now a single sf object with all successful geometries
print(all_polygons)

# # Combine all polygons together ----
new_polygons <- all_polygons %>%
  dplyr::mutate(source = "lower") %>%
  dplyr::select(mrgid, the_geom, source)

codes <- st_set_geometry(new_polygons, NULL)

# See which shapefiles are missing ----
joined <- distributions %>%
  left_join(codes) %>%
  distinct()

missing_lower <- joined %>%
  dplyr::filter(is.na(source))

most_common <- missing_lower %>%
  group_by(locality, locationID, higherGeography, higherGeographyID) %>%
  dplyr::summarise(n = n())

# Some are missing that do not have available shapefiles
# If lower level region is missing, then use the higher geography shapefile.
missing_higher <- joined %>%
  dplyr::filter(is.na(source)) %>%
  dplyr::select(-c(mrgid, source)) %>%
  # distinct(locality, locationID, higherGeography, higherGeographyID) %>% # Turn this on to check number of unique combinations
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(codes) %>%
  dplyr::filter(is.na(source)) %>%
  dplyr::distinct(locality, locationID)

# Only very few that are still missing
# Some have been superseeded, some don't have shapefiles to access easily

# Get Australia Marine Regions based off worms distributions
polygons_with_aus_regions <- data.frame()

# Takes 2 mins to run ----
for (i in 1:nrow(all_polygons)) {
  
  polygon_to_test <- all_polygons %>% slice(i)

  mrgid <- unique(polygon_to_test$mrgid)

  try(
      
      dat <- st_intersection(polygon_to_test, aus_regions) %>%
        st_set_geometry(NULL)%>%
        dplyr::distinct(region) %>%
        dplyr::summarise(aus.region = toString(region)) %>%
        dplyr::mutate(mrgid = as.integer(mrgid))
      
      )

  polygons_with_aus_regions <- bind_rows(polygons_with_aus_regions, dat)
}


# Get IMCRA Regions based off worms distributions
polygons_with_imcra_regions <- data.frame()

# Takes 2 mins to run ----
for (i in 1:nrow(all_polygons)) {
  
  polygon_to_test <- all_polygons %>% slice(i)
  
  mrgid <- unique(polygon_to_test$mrgid)
  
  try(
    dat <- st_intersection(polygon_to_test, imcra_regions) %>%
      st_set_geometry(NULL)%>%
      dplyr::distinct(region) %>%
      dplyr::summarise(imcra.region = toString(region)) %>%
      dplyr::mutate(mrgid = as.integer(mrgid))
    
  )
  
  polygons_with_imcra_regions <- bind_rows(polygons_with_imcra_regions, dat)
}

# Create a dataframe of which Aus marine regions match the regions in the polygons ----
polygons_tidy_aus <- polygons_with_aus_regions %>%
  distinct()

polygons_tidy_imcra <- polygons_with_imcra_regions %>%
  distinct()

# Need to join this info to the distributions
distributions_lower_aus <- left_join(distributions, polygons_tidy_aus) %>%
  distinct(id, aus.region) %>%
  filter(!aus.region %in% c("", NA, NULL)) %>%
  mutate(aus.region = strsplit(as.character(aus.region), split = ", "))%>% # changed from "/" for old LH
  unnest(aus.region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(aus.region = toString(aus.region)) %>%
  dplyr::rename(lowest_aus_region = aus.region)

distributions_higher_aus <- distributions %>%
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(., polygons_tidy_aus) %>%
  distinct(id, aus.region) %>%
  filter(!aus.region %in% c("", NA, NULL))%>%
  mutate(aus.region = strsplit(as.character(aus.region), split = ", "))%>% # changed from "/" for old LH
  unnest(aus.region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(aus.region = toString(aus.region)) %>%
  dplyr::rename(highest_aus_region = aus.region)

# If lower distribution polygons available those were used, if not the higher geography ones were used
distributions_aus <- full_join(distributions_lower_aus, distributions_higher_aus) %>%
  dplyr::mutate(aus_region = if_else(!is.na(lowest_aus_region), lowest_aus_region, highest_aus_region)) %>%
  dplyr::distinct(id, aus_region) %>%
  dplyr::rename(aphiaid = id) %>%
  dplyr::mutate(aphiaid = as.numeric(aphiaid))

# Repeat for the IMCRA regions ----
distributions_lower_imcra <- left_join(distributions, polygons_tidy_imcra) %>%
  distinct(id, imcra.region) %>%
  filter(!imcra.region %in% c("", NA, NULL)) %>%
  mutate(imcra.region = strsplit(as.character(imcra.region), split = ", "))%>% # changed from "/" for old LH
  unnest(imcra.region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(imcra.region = toString(imcra.region)) %>%
  dplyr::rename(lowest_imcra_region = imcra.region)

distributions_higher_imcra <- distributions %>%
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(., polygons_tidy_imcra) %>%
  distinct(id, imcra.region) %>%
  filter(!imcra.region %in% c("", NA, NULL))%>%
  mutate(imcra.region = strsplit(as.character(imcra.region), split = ", "))%>% # changed from "/" for old LH
  unnest(imcra.region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(imcra.region = toString(imcra.region)) %>%
  dplyr::rename(highest_imcra_region = imcra.region)

# If lower distribution polygons available those were used, if not the higher geography ones were used
distributions_imcra <- full_join(distributions_lower_imcra, distributions_higher_imcra) %>%
  dplyr::mutate(imcra_region = if_else(!is.na(lowest_imcra_region), lowest_imcra_region, highest_imcra_region)) %>%
  dplyr::distinct(id, imcra_region) %>%
  dplyr::rename(aphiaid = id) %>%
  dplyr::mutate(aphiaid = as.numeric(aphiaid))


# CAAB with distributions ----
final <- left_join(clean_caab, distributions_aus) %>%
  dplyr::left_join(distributions_imcra) %>%
  dplyr::rename(australian_common_name = common_name) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::select(caab, class, order, family, genus, species, scientific_name, australian_common_name, aus_region, imcra_region) %>%
  dplyr::mutate(australian_common_name = str_to_sentence(australian_common_name)) %>%
  dplyr::mutate(australian_common_name = gsub("\\[", "", australian_common_name))%>%
  dplyr::mutate(australian_common_name = gsub("\\]", "", australian_common_name))

spps <- final %>%
  dplyr::filter(species %in% "spp")

non_spps <- final %>%
  dplyr::filter(!species %in% "spp")

genus_distributions_aus <- non_spps %>%
  dplyr::mutate(aus_region = strsplit(as.character(aus_region), split = ", "))%>%
  unnest(aus_region) %>%
  dplyr::filter(!is.na(aus_region)) %>%
  dplyr::distinct(family, genus, aus_region) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(new_aus_region = toString(aus_region)) %>%
  dplyr::mutate(species = "spp")

family_distributions_aus <- non_spps %>%
  dplyr::mutate(aus_region = strsplit(as.character(aus_region), split = ", "))%>%
  unnest(aus_region) %>%
  dplyr::filter(!is.na(aus_region)) %>%
  dplyr::distinct(family, aus_region) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(new_aus_region = toString(aus_region)) %>%
  dplyr::mutate(genus = "Unknown", species = "spp")

genus_distributions_imcra <- non_spps %>%
  dplyr::mutate(imcra_region = strsplit(as.character(imcra_region), split = ", "))%>%
  unnest(imcra_region) %>%
  dplyr::filter(!is.na(imcra_region)) %>%
  dplyr::distinct(family, genus, imcra_region) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(new_imcra_region = toString(imcra_region)) %>%
  dplyr::mutate(species = "spp")

family_distributions_imcra <- non_spps %>%
  dplyr::mutate(imcra_region = strsplit(as.character(imcra_region), split = ", "))%>%
  unnest(imcra_region) %>%
  dplyr::filter(!is.na(imcra_region)) %>%
  dplyr::distinct(family, imcra_region) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(new_imcra_region = toString(imcra_region)) %>%
  dplyr::mutate(genus = "Unknown", species = "spp")

extras_aus <- bind_rows(genus_distributions_aus, family_distributions_aus)
extras_imcra <- bind_rows(genus_distributions_imcra, family_distributions_imcra)

new_final <- final %>%
  left_join(extras_aus) %>%
  left_join(extras_imcra) %>%
  dplyr::mutate(aus_region = if_else(is.na(aus_region), new_aus_region, aus_region)) %>%
  dplyr::mutate(imcra_region = if_else(is.na(imcra_region), new_imcra_region, imcra_region)) %>%
  dplyr::select(-c(new_aus_region, new_imcra_region)) %>%
  distinct()

# family_distributions <- final %>%
#   select()

number.with.distributions <- new_final %>% filter(!is.na(aus_region))
nrow(number.with.distributions)/nrow(new_final) * 100 
# 40.18% with distribution info available from worms package

number.with.distributions <- new_final %>% filter(!is.na(imcra_region))
nrow(number.with.distributions)/nrow(new_final) * 100 
# 40.95% with distribution info available from worms package

duplicates <- new_final %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

new_final_without_duplicates <- new_final %>%
  dplyr::group_by(class, order, family, genus, species, scientific_name, australian_common_name, aus_region, imcra_region) %>%
  slice(1) %>%
  ungroup()

duplicates <- new_final_without_duplicates %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

new_final_without_duplicates_1 <- new_final_without_duplicates%>%
  dplyr::group_by(class, order, family, genus, species, scientific_name) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::filter(!caab %in% "23115000")

duplicates <- new_final_without_duplicates_1 %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) 

write_rds(new_final_without_duplicates_1, "annotation-schema/data/staging/australia_animals_caab-code-and-distributions.RDS")

