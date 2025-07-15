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
wgs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# aus_regions <- st_read("annotation-schema/data/spatial/marine_regions.shp") %>%
#   dplyr::select(-c(OBJECTID))

aus_regions <- st_read("annotation-schema/data/spatial/marine_regions_with_gbr.shp")

aus_regions$region <- as.character(aus_regions$FULL_NAME)
st_crs(aus_regions) <- wgs_84

imcra_regions <- st_read("annotation-schema/data/spatial/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.shp")

imcra_regions$region <- as.character(imcra_regions$PB_NAME)
st_crs(imcra_regions) <- wgs_84

caab_og <- read_csv("annotation-schema/data/raw/caab_species_20250714.csv") %>%
  clean_names()

# Read in eread_csv_chunked()# Read in extra ones that are not included in CAAB dump that I have made by hand ----
caab_extra <- read_csv("annotation-schema/data/raw/caab_not_included_in_dump.csv") %>%
  dplyr::rename(display_name = scientific_name, order_name = order) %>%
  dplyr::mutate(scientific_name = display_name) %>%
  dplyr::rename(spcode = caab) %>%
  dplyr::filter(!spcode %in% unique(caab_og$spcode)) # appears these are now all here?

# Read in the latest CAAB dump from CSIRO website ----
# Download is available here: https://www.marine.csiro.au/datacentre/caab/caab_dump_latest.xlsx
# We have manually edited some classes/phylums/kingdoms where missing
caab_og <- read_csv("annotation-schema/data/raw/caab_species_20250714.csv") %>%
  clean_names() %>%
  dplyr::mutate(class = if_else(family %in% "Trygonorrhinidae", "Elasmobranchii", class)) %>%
  dplyr::mutate(phylum = if_else(family %in% "Trygonorrhinidae", "Chordata", phylum)) %>%
  dplyr::mutate(order_name = if_else(family %in% "Trygonorrhinidae", "Rajiformes", order_name)) %>%
  dplyr::mutate(kingdom = if_else(family %in% "Trygonorrhinidae", "Animalia", kingdom)) %>%
  
  dplyr::mutate(class = if_else(family %in% "Myliobatidae", "Elasmobranchii", class)) %>%
  dplyr::mutate(phylum = if_else(family %in% "Myliobatidae", "Chordata", phylum)) %>%
  dplyr::mutate(order_name = if_else(family %in% "Myliobatidae", "Rajiformes", order_name)) %>%
  dplyr::mutate(kingdom = if_else(family %in% "Myliobatidae", "Animalia", kingdom)) %>%
  # dplyr::bind_rows(caab_extra) %>%
  glimpse()

# Format caab codes ----
caab <- caab_og %>%
  dplyr::filter(class %in% c("Actinopterygii", 
                             "Elasmobranchii", 
                             "Holocephali", 
                             "Myxini"#,
                             # "Cephalaspidomorphi", # there are none of these in the list so I am leaving them out for now
                             # "Chondrichthyes",
                             # "Sarcopterygii"
                             )) %>%
  dplyr::filter(!is.na(display_name)) %>%
  # dplyr::filter(!is.na(parent_id)) %>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::select(spcode, kingdom, phylum, class, family, genus, species, common_name, order_name) %>%
  dplyr::rename(order = order_name)

list_to_download <- caab #%>%
  # filter(!species %in% "spp")

# # Download distribution files from CSIRO - remove the hashes to run again
# Sys.time() #1:09 PM
# 
# for (caab in unique(list_to_download$spcode)){
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
# for (caab in unique(list_to_download$spcode)){
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
# Sys.time()
# 
# saveRDS(polygons, "annotation-schema/data/staging/distributions-polygons.RDS")

polygons <- readRDS("annotation-schema/data/staging/distributions-polygons.RDS")

# Get Marine Regions based off CAAB distributions
aus_regions <- st_as_sf(aus_regions)
imcra_regions <- st_as_sf(imcra_regions)

temp_with_aus_regions <- data.frame()

# # Takes 30 minutes to run ----
for (CAAB in unique(polygons$SPCODE)) {

  polygons.to.test <- polygons %>% filter(SPCODE == CAAB)
  # single <- st_cast(polygons.to.test, "POLYGON")

  dat <- st_intersection(polygons.to.test, aus_regions) %>%
    st_set_geometry(NULL)%>%
    dplyr::distinct(region) %>%
    dplyr::summarise(marine.region = toString(region)) %>%
    dplyr::mutate(spcode = CAAB)

  temp_with_aus_regions <- bind_rows(temp_with_aus_regions, dat)

}

saveRDS(temp_with_aus_regions, "annotation-schema/data/staging/distributions-aus-regions-polygons.RDS")
temp_with_aus_regions <- readRDS("annotation-schema/data/staging/distributions-aus-regions-polygons.RDS") %>%
  dplyr::rename(aus_region = marine.region)

# # Reapeat again with IMCRA regions -----
# temp_with_imcra_regions <- data.frame()
# 
# # # Takes ~60 minutes to run ----
# # ----- prep for progress bar  ---------------------------------------------------------------
# spcodes <- unique(polygons$SPCODE)
# n_total <- length(spcodes)
# 
# temp_with_imcra_regions <- tibble()      # make sure it exists
# 
# pb <- utils::txtProgressBar(min = 0, max = n_total, style = 3)
# 
# # ----- loop ---------------------------------------------------------------
# # started at 2:52 PM
# for (i in seq_along(spcodes)) {
#   
#   CAAB <- spcodes[i]                 # current code
#   # message("Processing ", CAAB)
#   
#   polygons.to.test <- polygons %>%
#     filter(SPCODE == CAAB)
#   
#   dat <- st_intersection(polygons.to.test, imcra_regions) %>%
#     st_set_geometry(NULL) %>%
#     distinct(region) %>%
#     summarise(marine.region = toString(region)) %>%
#     mutate(spcode = CAAB)
#   
#   temp_with_imcra_regions <- bind_rows(temp_with_imcra_regions, dat)
#   
#   utils::setTxtProgressBar(pb, i)    # <‑‑ tick!
# }
# 
# close(pb)                             # tidy up
# 
# 
# for (CAAB in unique(polygons$SPCODE)) {
# 
#   message(CAAB)
# 
#   polygons.to.test <- polygons %>% filter(SPCODE == CAAB)
#   # single <- st_cast(polygons.to.test, "POLYGON")
# 
#   dat <- st_intersection(polygons.to.test, imcra_regions) %>%
#     st_set_geometry(NULL)%>%
#     dplyr::distinct(region) %>%
#     dplyr::summarise(marine.region = toString(region)) %>%
#     dplyr::mutate(spcode = CAAB)
# 
#   temp_with_imcra_regions <- bind_rows(temp_with_imcra_regions, dat)
# 
# }
# 
# saveRDS(temp_with_imcra_regions, "annotation-schema/data/staging/distributions-imcra-regions-polygons.RDS")
temp_with_imcra_regions <- readRDS("annotation-schema/data/staging/distributions-imcra-regions-polygons.RDS") %>%
  dplyr::rename(imcra_region = marine.region)

caab_format <- caab_og %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii", "Holocephali", "Myxini")) %>%
  dplyr::filter(!is.na(display_name)) %>%
  # dplyr::filter(!is.na(parent_id)) %>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::select(spcode, kingdom, phylum, class, order_name, family, genus, species, common_name) %>%
  dplyr::rename(order = order_name)

caab_with_regions <- full_join(caab_format, temp_with_imcra_regions) %>%
  full_join(temp_with_aus_regions) %>%
  dplyr::rename(caab_code = spcode)

# none missing
missing <- caab_with_regions %>%
  filter(is.na(imcra_region))

# Get IMCRA regions for spps ----
spps <- caab_with_regions %>%
  dplyr::filter(species %in% "spp") %>%
  dplyr::select(!imcra_region) 

spp_regions_genus_imcra <- caab_format %>%
  dplyr::rename(caab_code = spcode) %>%
  dplyr::left_join(caab_with_regions) %>%
  dplyr::distinct(family, genus, imcra_region) %>%
  dplyr::filter(!is.na(imcra_region)) %>%
  dplyr::mutate(imcra_region = strsplit(as.character(imcra_region), split = ", "))%>%
  tidyr::unnest(imcra_region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(new_imcra_region = toString(imcra_region))

spp_regions_family_imcra <- caab_format %>%
  dplyr::rename(caab_code = spcode) %>%
  dplyr::left_join(caab_with_regions) %>%
  dplyr::distinct(family, imcra_region) %>%
  dplyr::filter(!is.na(imcra_region)) %>%
  dplyr::mutate(imcra_region = strsplit(as.character(imcra_region), split = ", "))%>%
  tidyr::unnest(imcra_region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(genus = "Unknown", species = "spp") %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(new_imcra_region = toString(imcra_region))

spp_regions_genus_aus <- caab_format %>%
  dplyr::rename(caab_code = spcode) %>%
  dplyr::left_join(caab_with_regions) %>%
  dplyr::distinct(family, genus,aus_region) %>%
  dplyr::filter(!is.na(aus_region)) %>%
  dplyr::mutate(aus_region = strsplit(as.character(aus_region), split = ", "))%>%
  tidyr::unnest(aus_region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(new_aus_region = toString(aus_region))

spp_regions_family_aus <- caab_format %>%
  dplyr::rename(caab_code = spcode) %>%
  dplyr::left_join(caab_with_regions) %>%
  dplyr::distinct(family, aus_region) %>%
  dplyr::filter(!is.na(aus_region)) %>%
  dplyr::mutate(aus_region = strsplit(as.character(aus_region), split = ", "))%>%
  tidyr::unnest(aus_region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(genus = "Unknown", species = "spp") %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(new_aus_region = toString(aus_region))

extras_imcra <- bind_rows(spp_regions_genus_imcra, spp_regions_family_imcra)
extras_aus <- bind_rows(spp_regions_genus_aus, spp_regions_family_aus)

caab_combined <- caab_with_regions %>%
  left_join(extras_aus) %>%
  left_join(extras_imcra) %>%
  dplyr::mutate(aus_region = if_else(is.na(aus_region), new_aus_region, aus_region)) %>%
  dplyr::mutate(imcra_region = if_else(is.na(imcra_region), new_imcra_region, imcra_region)) %>%
  dplyr::select(-c(new_aus_region, new_imcra_region)) %>%
  dplyr::filter(!caab_code %in% c(NA)) %>%
  
  dplyr::filter(!caab_code %in% c("37337902", "37337910", "37018901", "37280000", "37384000", 
                                  "37385000", "37268900", "37096000", "37287901", "37311910")) %>%
  
  # temporary fixes to remove duplicate caabs
  dplyr::filter(!(caab_code %in% "37004001" & is.na(common_name))) %>%
  dplyr::filter(!(caab_code %in% "37004002" & is.na(common_name))) %>%
  dplyr::filter(!(caab_code %in% "37441924" & is.na(common_name))) %>%
  dplyr::filter(!(caab_code %in% "37026002" & species %in% "ancylostoma")) %>%
  dplyr::mutate(common_name = if_else(caab_code %in% "37026002", "Shark Ray", common_name)) %>%
  dplyr::mutate(family = if_else(family %in% "Labridae: Scarinae", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(family %in% "Labridae: Odacinae", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(family %in% "Labridae: Labrinae", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(family %in% "Centriscidae: Centriscinae", "Centriscidae", family)) %>%
  dplyr::mutate(family = if_else(family %in% "Centriscidae: Macroramphosinae", "Centriscidae", family)) %>%
  dplyr::mutate(family = if_else(family %in% "Scaridae", "Labridae", family)) %>%
  dplyr::filter(!(caab_code %in% "37386905" & is.na(imcra_region))) %>%
  dplyr::filter(!(caab_code %in% "37386907" & is.na(imcra_region))) %>%
  dplyr::mutate(family = if_else((family %in% "Scorpididae" & genus %in% "Microcanthus"), "Microcanthidae", family)) %>%
  dplyr::mutate(species = if_else((caab_code %in% "37361005"), "joyceae", species)) %>%
  dplyr::mutate(family = if_else(caab_code %in% "37386000", "Labridae: Scarinae", family)) %>%
  dplyr::mutate(common_name = gsub("[\\[\\]]", "", common_name)) %>%
  dplyr::mutate(common_name = gsub("\\[", "", common_name)) %>%
  dplyr::mutate(common_name = gsub("\\]", "", common_name)) %>%
  filter(!grepl("[^A-Za-z]", family)) %>%
  filter(!grepl("[^A-Za-z]", genus)) %>%
  filter(!grepl("[^A-Za-z]", species)) %>%
  distinct()

test <- caab_with_regions %>%
  filter(grepl("[^A-Za-z]", family)) %>%
  distinct(family)

test <- caab_with_regions %>%
  filter(grepl("[^A-Za-z]", genus)) %>%
  distinct(genus)

test <- caab_with_regions %>%
  filter(grepl("[^A-Za-z]", species)) %>%
  distinct(species)
  

unique(caab_combined$kingdom)
unique(caab_combined$phylum)
unique(caab_combined$class)
unique(caab_combined$family)
unique(caab_combined$genus)
unique(caab_combined$species)

37361028

test <- caab_combined %>%
  filter(grepl("[^A-Za-z]", family)) %>%
  distinct(family)

test <- caab_combined %>%
  filter(grepl("[^A-Za-z]", genus)) %>%
  distinct(genus)

test <- caab_combined %>%
  filter(grepl("[^A-Za-z]", species)) %>%
  distinct(species)

test <- caab_combined %>%
  filter(is.na(species))

test <- caab_combined %>%
  group_by(genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_combined %>%
  group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- caab_combined %>%
  group_by(caab_code) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

missing <- caab_combined %>%
  filter(is.na(imcra_region))

saveRDS(caab_combined, "annotation-schema/data/staging/australia_fish_caab-with-regions.RDS")
