# This script uses the latest CAAB download from the CSIRO to:
# - Create a list of fish species in Australia, then
# - Download the distribution shapefile from CAAB
# - Converts that distribution into presence in Australia's marine bioregions
# - Creates presence of each Genus in Australia's marine bioregions

# Load the required libraries
library(tidyverse)
library(readxl)
library(CheckEM)
library(sf)
sf_use_s2(TRUE)

# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

aus.regions <- st_read("annotation-schema/data/spatial/marine_regions.shp") %>%
  dplyr::select(-c(OBJECTID))

aus.regions$region <- as.character(aus.regions$REGION)
st_crs(aus.regions) <- wgs.84

# Read in the latest CAAB dump from CSIRO website ----
# Download is available here: https://www.marine.csiro.au/datacentre/caab/caab_dump_latest.xlsx
# We have manually edited some classes/phylums/kingdoms where missing
caab.og <- read_excel("annotation-schema/data/raw/caab_dump_latest.xlsx") %>%
  clean_names() %>%
  dplyr::mutate(class = if_else(family %in% "Trygonorrhinidae", "Elasmobranchii", class)) %>%
  dplyr::mutate(phylum = if_else(family %in% "Trygonorrhinidae", "Chordata", phylum)) %>%
  dplyr::mutate(order_name = if_else(family %in% "Trygonorrhinidae", "Rajiformes", order_name)) %>%
  dplyr::mutate(kingdom = if_else(family %in% "Trygonorrhinidae", "Animalia", kingdom)) %>%
  
  dplyr::mutate(class = if_else(family %in% "Myliobatidae", "Elasmobranchii", class)) %>%
  dplyr::mutate(phylum = if_else(family %in% "Myliobatidae", "Chordata", phylum)) %>%
  dplyr::mutate(order_name = if_else(family %in% "Myliobatidae", "Rajiformes", order_name)) %>%
  dplyr::mutate(kingdom = if_else(family %in% "Myliobatidae", "Animalia", kingdom)) %>%
  glimpse()

# Format caab codes ----
caab <- caab.og %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii", "Holocephali")) %>%
  dplyr::filter(!is.na(display_name)) %>%
  dplyr::filter(!is.na(parent_id)) %>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::select(spcode, kingdom, phylum, class, family, genus, species, common_name, order_name) %>%
  dplyr::rename(order = order_name)

list.to.download <- caab %>%
  filter(!species %in% "spp")

## Download distribution files from CSIRO - remove the hashes to run again
# for (caab in unique(list.to.download$spcode)){
#   print(caab)
# 
#   # Set the URL of the shapefile
#   url <- paste0("https://www.cmar.csiro.au/geoserver/caab/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=caab%3ACAAB_FISHMAP&maxFeatures=200&outputFormat=SHAPE-ZIP&CQL_FILTER=SPCODE%3D%27", caab, "%27")
# 
#   # Set the destination file path where the shapefile will be saved
#   destination_file <- paste0("annotation-schema/data/raw/distributions/", caab, "_shapefile.zip")
#   
#   # Check if the download was successful
#   if (file.exists(destination_file)) {
#     message("Shapefile downloaded successfully.")
#   } else {
#     # Download the shapefile
#     download.file(url, destfile = destination_file, mode = "wb")
#     
#     # message("Failed to download the shapefile.")
#   }
# }
# 
# # Need to unzip the shapefiles and combine into one file ----
# temp <- tempfile()
# 
# # Unzip the contents of the last caab from the loop above to get file format
# unzip(zipfile = paste0("annotation-schema/data/raw/distributions/", caab, "_shapefile.zip"), exdir = temp)
# 
# # Read the shapefile
# polygons <- sf::read_sf(temp) %>% glimpse()
# 
# for (caab in unique(list.to.download$spcode)){
#   # create a temp directory
#   temp <- tempfile()
# 
#   message(caab)
# 
#   # Unzip the contents of the temp and save unzipped content in 'temp2'
#   try(unzip(zipfile = paste0("annotation-schema/data/raw/distributions/", caab, "_shapefile.zip"), exdir = temp))
# 
#   # Read the shapefile
#   try(polygon <- sf::read_sf(temp) %>% glimpse())
# 
#   # Combine together
#   polygons <- bind_rows(polygons, polygon)
# 
# }
# 
# saveRDS(polygons, "annotation-schema/data/staging/distributions-polygons.RDS")

polygons <- readRDS("annotation-schema/data/staging/distributions-polygons.RDS")

# Get Marine Regions based off CAAB distributions
aus.regions <- st_as_sf(aus.regions)
temp.with.regions <- data.frame()

single <- st_cast(polygons, "POLYGON")

test <- aus.regions %>%
  dplyr::slice(st_nearest_feature(single, aus.regions)) %>%
  st_set_geometry(NULL)

# # Takes 30 minutes to run ----
for (CAAB in unique(polygons$SPCODE)) {

  polygons.to.test <- polygons %>% filter(SPCODE == CAAB)

  single <- st_cast(polygons.to.test, "POLYGON")
  
  dat <- aus.regions %>%
    dplyr::slice(st_nearest_feature(single, aus.regions)) %>%
    st_set_geometry(NULL)%>%
    dplyr::distinct(Label) %>%
    dplyr::summarise(marine.region = toString(Label)) %>%
    dplyr::mutate(spcode = CAAB)

  temp.with.regions <- bind_rows(temp.with.regions, dat)
  
}

caab <- caab.og %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii", "Holocephali")) %>%
  dplyr::filter(!is.na(display_name)) %>%
  dplyr::filter(!is.na(parent_id)) %>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::select(spcode, kingdom, phylum, class, family, genus, species, common_name, order_name) %>%
  dplyr::rename(order = order_name)

caab.with.regions <- full_join(temp.with.regions, caab)

# none missing
missing <- caab.with.regions %>%
  filter(is.na(marine.region))

spp.regions <- caab %>%
  dplyr::rename(caab = spcode) %>%
  dplyr::left_join(caab.with.regions) %>%
  dplyr::distinct(family, genus, marine.region) %>%
  dplyr::filter(!is.na(marine.region)) %>%
  dplyr::mutate(marine.region = strsplit(as.character(marine.region), split = ", "))%>%
  tidyr::unnest(marine.region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(marine.region = toString(marine.region)) %>%
  dplyr::left_join(caab)

caab.combined <- dplyr::bind_rows(caab.with.regions, spp.regions) %>%
  dplyr::rename(caab_code = spcode) %>%
  dplyr::filter(!caab_code %in% c(NA))

missing <- caab.combined %>%
  filter(is.na(marine.region))

saveRDS(caab.combined, "annotation-schema/data/staging/caab-with-regions.RDS")