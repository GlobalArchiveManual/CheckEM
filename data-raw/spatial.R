library(rgdal)
library(tidyverse)

# Spatial files ----
wgs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

aus_regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/marine_regions.shp")
aus_regions$REGION <- as.character(aus_regions$REGION)
proj4string(aus_regions) <- CRS(wgs_84)

world_regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas.shp")
world_regions$REGION <- str_replace_all(world_regions$NAME_EN, c(", " = "_", " " ="."))
proj4string(world_regions) <- CRS(wgs_84)

usethis::use_data(aus_regions, overwrite = TRUE)
usethis::use_data(world_regions, overwrite = TRUE)
