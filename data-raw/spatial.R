# library(rgdal)
library(tidyverse)

# TODO - change this to use sf

# Spatial files ----
wgs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# aus_regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/marine_regions.shp")
# aus_regions$REGION <- as.character(aus_regions$REGION)
# # proj4string(aus_regions) <- CRS(wgs_84)
# 
# world_regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas.shp")
# world_regions$REGION <- str_replace_all(world_regions$NAME_EN, c(", " = "_", " " ="."))
# proj4string(world_regions) <- CRS(wgs_84)


aus_regions <- st_read("annotation-schema/data/spatial/marine_regions_with_gbr.shp")

aus_regions$REGION <- as.character(aus_regions$FULL_NAME)
st_crs(aus_regions) <- wgs_84


imcra_regions <- st_read("annotation-schema/data/spatial/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.shp")

imcra_regions$REGION <- as.character(imcra_regions$PB_NAME)
st_crs(imcra_regions) <- wgs_84


usethis::use_data(aus_regions, overwrite = TRUE)
usethis::use_data(imcra_regions, overwrite = TRUE)
usethis::use_data(world_regions, overwrite = TRUE)
