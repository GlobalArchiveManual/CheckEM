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
aus_regions <- st_read("annotation-schema/data/spatial/marine_regions.shp") %>%
  dplyr::select(-c(OBJECTID))

aus_regions$region <- as.character(aus_regions$REGION)
st_crs(aus_regions) <- wgs_84
aus_regions <- st_as_sf(aus_regions)

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
# plot(all_polygons["mrgid"])               # quick check

# method 1 233.23 seconds

# t <- gaz_search(8323, with_geometry = TRUE)
# 
# t8914 <- gaz_search(8914, with_geometry = TRUE)
# 
# plot(t)
# plot(t8914)
# 
# realm <- mrp_shp("Ecoregions:realm") %>%
#   dplyr::mutate(source = "realm") %>%
#   dplyr::rename(name = realm) %>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# realm <- mr_shp(key = "Ecoregions:realm") %>%
#   dplyr::mutate(source = "realm") %>%
#   dplyr::rename(name = realm) %>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# date <- mr_shp(key = "MarineRegions:cross_dateline_polygons") %>%
#   dplyr::mutate(source = "dateline") %>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# faoreg <- mr_shp(key = "MarineRegions:fao") %>%
#   dplyr::mutate(source = "FAO") %>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# tdwg <- mr_shp(key = "TDWG:level4") %>%
#   dplyr::mutate(source = "TDWG") %>%
#   dplyr::rename(name = level_4_na) %>%
#   dplyr::select(mrgid, name, geometry, source) %>%
#   dplyr::filter(!mrgid == 0)
# 
# erms <- mr_shp(key = "Europe:europeislands") %>%
#   dplyr::mutate(source = "europe") %>%
#   dplyr::select(mrgid, name, geometry, source) %>%
#   dplyr::filter(!mrgid == 0)
# 
# eco <- mr_shp(key = "Ecoregions:ecoregions") %>% # slow to run
#   dplyr::rename(name = ecoregion) %>%
#   dplyr::mutate(source = "eco") %>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# eez <- mr_shp(key = "MarineRegions:eez") %>% # slow to run
#   dplyr::rename(name = geoname) %>%
#   dplyr::mutate(source = "EEZ")%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# eez_iho <- mr_shp(key = "MarineRegions:eez_iho") %>% # very slow to run
#   dplyr::rename(name = marregion) %>%
#   dplyr::mutate(source = "EEZ IHO")%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# iho <- mr_shp(key = "MarineRegions:iho") %>%
#   dplyr::mutate(source = "IHO") %>%
#   dplyr::select(-c(id))%>%
#   dplyr::select(mrgid, name, geometry, source) # quick to run
# 
# iho_quad <- mr_shp(key = "MarineRegions:iho_quadrants_20150810") # quick to run
# 
# iho_q1 <- iho_quad %>%
#   dplyr::select(-c(name)) %>%
#   dplyr::rename(name = name_1, mrgid = mrgid_1) %>%
#   dplyr::mutate(source = "IHO Q1") %>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# iho_q2 <- iho_quad %>%
#   dplyr::select(-c(name)) %>%
#   dplyr::rename(name = name_2, mrgid = mrgid_2) %>%
#   dplyr::mutate(source = "IHO Q2")%>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# iho_q3 <- iho_quad %>%
#   dplyr::select(-c(name)) %>%
#   dplyr::rename(name = name_3, mrgid = mrgid_3) %>%
#   dplyr::mutate(source = "IHO Q1")%>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# nations <- mr_shp(key = "MarineRegions:worldcountries_esri_2014") # quick to run
# 
# nat_q1 <- nations %>%
#   # dplyr::select(territory1, mrgid_ter1) %>%
#   dplyr::rename(name = territory1, mrgid = mrgid_ter1) %>%
#   dplyr::mutate(source = "Nations Q1") %>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# nat_q2 <- nations %>%
#   # dplyr::select(territory2, mrgid_ter2) %>%
#   dplyr::rename(name = territory2, mrgid = mrgid_ter2) %>%
#   dplyr::mutate(source = "Nations Q2") %>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# nat_q3 <- nations %>%
#   # dplyr::select(territory3, mrgid_ter3) %>%
#   dplyr::rename(name = territory3, mrgid = mrgid_ter3) %>%
#   dplyr::mutate(source = "Nations Q3") %>%
#   dplyr::filter(!mrgid == 0)%>%
#   dplyr::select(mrgid, name, geometry, source)
# 
# # Combine all polygons together ----
# all_polygons <- bind_rows(eez, iho, iho_q1, iho_q2, iho_q3, nat_q1, nat_q2, nat_q3, eez_iho, eco, erms, tdwg, faoreg, realm, date) %>%
#   dplyr::select(mrgid, name, geometry, source)

codes <- st_set_geometry(all_polygons, NULL)

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
  dplyr::select(-c(mrgid, name, source)) %>%
  # distinct(locality, locationID, higherGeography, higherGeographyID) %>% # Turn this on to check number of unique combinations
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(codes) %>%
  dplyr::filter(is.na(source)) %>%
  dplyr::distinct(locality, locationID)

# Only very few that are still missing
# Some have been superseeded, some don't have shapefiles to access easily

# Get Australia Marine Regions based off worms distributions
polygons_with_regions <- data.frame()

# Takes 2 mins to run ----
for (i in 1:nrow(all_polygons)) {
  
  polygon_to_test <- all_polygons %>% slice(i)
  
  mrgid <- unique(polygon_to_test$mrgid)
  name <- unique(polygon_to_test$name)
  
  try(dat <- aus_regions %>%
        dplyr::slice(st_intersects(polygon_to_test, aus_regions)[[1]]) %>%
        st_set_geometry(NULL) %>%
        dplyr::distinct(Label) %>%
        dplyr::summarise(marine_region = toString(Label)) %>%
        dplyr::mutate(mrgid = as.integer(mrgid), name = name))
  
  polygons_with_regions <- bind_rows(polygons_with_regions, dat)
}

# Create a dataframe of which Aus marine regions match the regions in the polygons ----
polygons_tidy <- polygons_with_regions %>%
  distinct()

# Need to join this info to the distributions
distributions_lower <- left_join(distributions, polygons_tidy) %>%
  distinct(id, marine_region) %>%
  filter(!marine_region %in% c("", NA, NULL)) %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>% # changed from "/" for old LH
  unnest(marine_region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(marine_region = toString(marine_region)) %>%
  dplyr::rename(lowest_marine_region = marine_region)

distributions_higher <- distributions %>%
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(., polygons_tidy) %>%
  distinct(id, marine_region) %>%
  filter(!marine_region %in% c("", NA, NULL))%>%
  mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>% # changed from "/" for old LH
  unnest(marine_region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(marine_region = toString(marine_region)) %>%
  dplyr::rename(highest_marine_region = marine_region)

# If lower distribution polygons available those were used, if not the higher geography ones were used
distributions_aus <- full_join(distributions_lower, distributions_higher) %>%
  dplyr::mutate(marine_region = if_else(!is.na(lowest_marine_region), lowest_marine_region, highest_marine_region)) %>%
  dplyr::distinct(id, marine_region) %>%
  dplyr::rename(aphiaid = id) %>%
  dplyr::mutate(aphiaid = as.numeric(aphiaid))

# CAAB with distributions ----
final <- left_join(clean_caab, distributions_aus) %>%
  dplyr::rename(australian_common_name = common_name) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::select(caab, class, order, family, genus, species, scientific_name, australian_common_name, marine_region) %>%
  dplyr::mutate(australian_common_name = str_to_sentence(australian_common_name)) %>%
  dplyr::mutate(australian_common_name = gsub("\\[", "", australian_common_name))%>%
  dplyr::mutate(australian_common_name = gsub("\\]", "", australian_common_name))

spps <- final %>%
  dplyr::filter(species %in% "spp")

non_spps <- final %>%
  dplyr::filter(!species %in% "spp")

genus_distributions <- non_spps %>%
  dplyr::mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>%
  unnest(marine_region) %>%
  dplyr::filter(!is.na(marine_region)) %>%
  dplyr::distinct(family, genus, marine_region) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(new_marine_region = toString(marine_region)) %>%
  dplyr::mutate(species = "spp")

family_distributions <- non_spps %>%
  dplyr::mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>%
  unnest(marine_region) %>%
  dplyr::filter(!is.na(marine_region)) %>%
  dplyr::distinct(family, marine_region) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(new_marine_region = toString(marine_region)) %>%
  dplyr::mutate(genus = "Unknown", species = "spp")

extras <- bind_rows(genus_distributions, family_distributions)

new_final <- final %>%
  left_join(extras) %>%
  dplyr::mutate(marine_region = if_else(is.na(marine_region), new_marine_region, marine_region)) %>%
  dplyr::select(-c(new_marine_region))

# family_distributions <- final %>%
#   select()

number.with.distributions <- new_final %>% filter(!is.na(marine_region))
nrow(number.with.distributions)/nrow(new_final) * 100 
# 40.5% with distribution info available from worms package


duplicates <- new_final %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

new_final_without_duplicates <- new_final %>%
  dplyr::group_by(class, order, family, genus, species, scientific_name, australian_common_name, marine_region) %>%
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

