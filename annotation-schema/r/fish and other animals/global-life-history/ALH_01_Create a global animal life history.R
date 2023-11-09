library(tidyverse)
library(worrms)
library(mregions)
library(GlobalArchive)
library(sf)
library(rfishbase)

# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fao <- st_read("data/spatial/FAO_major_areas.shp")
st_crs(fao) <- wgs.84
fao <- st_as_sf(fao)

fao$NAME_EN <- str_replace_all(fao$NAME_EN, c(", " = "_", " " = "."))

unique(fao$NAME_EN)

# Read in species list 
worms <- read_delim("data/taxon.txt") %>%
  dplyr::filter(taxonomicStatus %in% "accepted") %>%
  dplyr::filter(!is.na(genus)) %>%
  dplyr::filter(!is.na(order)) %>%
  dplyr::filter(taxonRank == "Species")

names(worms)
unique(worms$kingdom) # we are only interested in animals
unique(worms$taxonomicStatus) # only accepted names
unique(worms$taxonRank) # only Species rank

animals <- worms %>%
  dplyr::filter(kingdom %in% "Animalia") %>%
  dplyr::filter(phylum %in% c("Mollusca",
                              "Arthropoda",
                              "Echinodermata",
                              "Chordata")) %>%
  filter(!str_detect(order, "\\[unassigned\\] Caenogastropod")) %>%
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
                              "")) %>%
  dplyr::filter(!is.na(class)) %>%
  ga.clean.names() %>%
  dplyr::select(taxonid, scientificname, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename(scientific_name = scientificname) %>%
  dplyr::mutate(taxonid = str_replace_all(taxonid, "urn:lsid:marinespecies.org:taxname:", "")) %>%
  dplyr::filter(!is.na(taxonid))

unique(animals$taxonid)
unique(animals$phylum)
unique(animals$class) %>% sort()

ids <- unique(animals$taxonid)

# takes ~ 14 hours to run
# Unhash to run again to update
distributions <- wm_distribution_(id = c(as.numeric(ids))) %>%
  dplyr::mutate(mrgid = as.integer(str_replace_all(locationID, "http://marineregions.org/mrgid/", ""))) #%>%
  # dplyr::filter(!is.na(higherGeographyID))

saveRDS(distributions, "data/distributions_worms_animals_global.RDS")

dist <- readRDS("data/distributions_worms_animals_global.RDS") %>%
  distinct()

# Get shapefiles for the marine regions using the mregion package ----

realm <- mr_shp(key = "Ecoregions:realm") %>%
  dplyr::mutate(source = "realm") %>%
  dplyr::rename(name = realm) %>%
  dplyr::select(mrgid, name, geometry, source)

date <- mr_shp(key = "MarineRegions:cross_dateline_polygons") %>%
  dplyr::mutate(source = "dateline") %>%
  dplyr::select(mrgid, name, geometry, source)

faoreg <- mr_shp(key = "MarineRegions:fao") %>%
  dplyr::mutate(source = "FAO") %>%
  dplyr::select(mrgid, name, geometry, source)

tdwg <- mr_shp(key = "TDWG:level4") %>%
  dplyr::mutate(source = "TDWG") %>%
  dplyr::rename(name = level_4_na) %>%
  dplyr::select(mrgid, name, geometry, source) %>%
  dplyr::filter(!mrgid == 0)

erms <- mr_shp(key = "Europe:europeislands") %>%
  dplyr::mutate(source = "europe") %>%
  dplyr::select(mrgid, name, geometry, source) %>%
  dplyr::filter(!mrgid == 0)

eco <- mr_shp(key = "Ecoregions:ecoregions") %>% # slow to run
  dplyr::rename(name = ecoregion) %>%
  dplyr::mutate(source = "eco") %>%
  dplyr::select(mrgid, name, geometry, source)

eez <- mr_shp(key = "MarineRegions:eez") %>% # slow to run
  dplyr::rename(name = geoname) %>%
  dplyr::mutate(source = "EEZ")%>%
  dplyr::select(mrgid, name, geometry, source)

eez_iho <- mr_shp(key = "MarineRegions:eez_iho") %>% # very slow to run
  dplyr::rename(name = marregion) %>%
  dplyr::mutate(source = "EEZ IHO")%>%
  dplyr::select(mrgid, name, geometry, source)

iho <- mr_shp(key = "MarineRegions:iho") %>%
  dplyr::mutate(source = "IHO") %>%
  dplyr::select(-c(id))%>%
  dplyr::select(mrgid, name, geometry, source) # quick to run

iho_quad <- mr_shp(key = "MarineRegions:iho_quadrants_20150810") # quick to run

iho_q1 <- iho_quad %>%
  dplyr::select(-c(name)) %>%
  dplyr::rename(name = name_1, mrgid = mrgid_1) %>%
  dplyr::mutate(source = "IHO Q1") %>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

iho_q2 <- iho_quad %>%
  dplyr::select(-c(name)) %>%
  dplyr::rename(name = name_2, mrgid = mrgid_2) %>%
  dplyr::mutate(source = "IHO Q2")%>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

iho_q3 <- iho_quad %>%
  dplyr::select(-c(name)) %>%
  dplyr::rename(name = name_3, mrgid = mrgid_3) %>%
  dplyr::mutate(source = "IHO Q1")%>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

nations <- mr_shp(key = "MarineRegions:worldcountries_esri_2014") # quick to run

nat_q1 <- nations %>%
  # dplyr::select(territory1, mrgid_ter1) %>%
  dplyr::rename(name = territory1, mrgid = mrgid_ter1) %>%
  dplyr::mutate(source = "Nations Q1") %>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

nat_q2 <- nations %>%
  # dplyr::select(territory2, mrgid_ter2) %>%
  dplyr::rename(name = territory2, mrgid = mrgid_ter2) %>%
  dplyr::mutate(source = "Nations Q2") %>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

nat_q3 <- nations %>%
  # dplyr::select(territory3, mrgid_ter3) %>%
  dplyr::rename(name = territory3, mrgid = mrgid_ter3) %>%
  dplyr::mutate(source = "Nations Q3") %>%
  dplyr::filter(!mrgid == 0)%>%
  dplyr::select(mrgid, name, geometry, source)

all_polygons <- bind_rows(eez, iho, iho_q1, iho_q2, iho_q3, nat_q1, nat_q2, nat_q3, eez_iho, eco, erms, tdwg, faoreg, realm, date) %>%
  dplyr::select(mrgid, name, geometry, source)

codes <- st_set_geometry(all_polygons, NULL)

names(codes)

# This is just for me to work out which shapefiles I am missing.
joined <- dist %>%
  left_join(codes) %>%
  distinct()

missing_lower <- joined %>%
  dplyr::filter(is.na(source))

most_common <- missing_lower %>%
  group_by(locality, locationID, higherGeography, higherGeographyID) %>%
  dplyr::summarise(n = n()) %>%
  arrange(-n)

# Get FAO major area based off worms distributions
polygons_with_regions <- data.frame()

# Takes 5 mins to run ----
for (i in 1:nrow(all_polygons)) {
  
  polygon_to_test <- all_polygons %>% slice(i)
  
  mrgid <- unique(polygon_to_test$mrgid)
  name <- unique(polygon_to_test$name)
  
  glimpse(name)
  
  try(dat <- fao %>%
        dplyr::slice(st_intersects(polygon_to_test, fao)[[1]]) %>%
        st_set_geometry(NULL) %>%
        dplyr::distinct(NAME_EN) %>%
        dplyr::summarise(marine_region = toString(NAME_EN)) %>%
        dplyr::mutate(mrgid = as.integer(mrgid), name = name))
  
  polygons_with_regions <- bind_rows(polygons_with_regions, dat)
}

# This is a dataframe of which FAO major fishing areas match the regions in the polygons 
polygons_tidy <- polygons_with_regions %>%
  distinct()

# Need to join this info to the distributions
distributions_lower <- left_join(dist, polygons_tidy) %>%
  distinct(id, marine_region) %>%
  filter(!marine_region %in% c("", NA, NULL)) %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = "; "))%>% # changed from "/" for old LH
  unnest(marine_region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(marine_region = toString(marine_region)) %>%
  dplyr::rename(lowest_marine_region = marine_region)

distributions_higher <- dist %>%
  dplyr::mutate(mrgid = as.integer(str_replace_all(higherGeographyID, "http://marineregions.org/mrgid/", ""))) %>%
  left_join(., polygons_tidy) %>%
  distinct(id, marine_region) %>%
  filter(!marine_region %in% c("", NA, NULL)) %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = "; "))%>% # changed from "/" for old LH
  unnest(marine_region) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(marine_region = toString(marine_region)) %>%
  dplyr::rename(highest_marine_region = marine_region)

# If lower distribution polygons available those were used, if not the higher geography ones were used
distributions_all <- full_join(distributions_lower, distributions_higher) %>%
  dplyr::mutate(marine_region = if_else(!is.na(lowest_marine_region), lowest_marine_region, highest_marine_region)) %>%
  dplyr::distinct(id, marine_region) %>%
  dplyr::rename(taxonid = id) 

# get common names from sealife base
common_names <- fb_tbl("species", "sealifebase") %>%
  ga.clean.names() %>%
  dplyr::filter(!is.na(fbname)) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::rename(common_name = fbname) %>%
  dplyr::select(scientific_name, common_name)

# CAAB with distributions ----
final <- left_join(animals, distributions_all) %>%
  left_join(common_names)

number.with.distributions <- final %>% filter(!is.na(marine_region))
39409/88271 # 44% with distribution info available from worms package

write_rds(final, "data/animals_global_with_dist.RDS")
final <- readRDS("data/animals_global_with_dist.RDS")
