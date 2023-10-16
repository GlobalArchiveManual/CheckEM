###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data & SwC multibeam bathymetry
# Task:    Extract bathymetry derivatives at sampled locations
# Author:  Claude Spencer
# Date:    August 2023
##

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(sf)
library(terra)
library(stars)
library(starsExtra)

# Set the study name ----
name <- '2021-2022_SwC_BOSS'

# Load bathymetry data ----
bathy <- rast("data/spatial/rasters/swc_multibeam_UTM50.tif")                   # UTM zone 50

# Create bathymetry derivatives ----
preds <- terrain(bathy, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),           # Remove here as necessary
                 unit = "degrees")

# Calculate detrended bathymetry ----
zstar <- st_as_stars(bathy)                                                     # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                               # Convert it to a terra raster
names(detre) <- c("detrended", "lineartrend")

preds <- rast(list(bathy, preds, detre[[1]]))                                   # Stack the derivatives with the bathymetry
names(preds)[1] <- "mbdepth"

# Save file for use later - too large so ignored from git
saveRDS(preds, 
        file = "data/spatial/rasters/2021-2022_SwC_bathymetry-derivatives.rds")

# Load metadata ----
metadata <- read.csv(paste0("data/tidy/", name, "_Metadata.csv")) %>%
  glimpse()

# Load habitat data and join with metadata ----
tidy.habitat <- read.csv(paste0("data/tidy/", name, "_Habitat.csv")) %>%
  left_join(metadata) %>%
  dplyr::filter(!level_2 %in% "Unscorable") %>%
  # Make broad habitat levels for modelling
  dplyr::mutate(habitat = case_when(level_2 %in% c("Sponges", "Cnidaria",
                                                   "Bryozoa", "Sessile invertebrates",
                                                   "Ascidians", "Echinoderms") ~ "sessile.invertebrates",
                                    level_2 %in% "Macroalgae" ~ "macroalgae",
                                    level_2 %in% "Seagrasses" ~ "seagrasses",
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)"~ "sand",
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)"~ "rock")) %>%
  group_by(campaignid, sample) %>%
  dplyr::mutate(total.points.annotated = sum(number)) %>%
  ungroup() %>%
  glimpse()

# Transform the habitat to a SpatVector ----
habitat.vect <- vect(tidy.habitat, geom = c("longitude", "latitude"), crs = "epsg:4326") %>%
  project(preds)                                                                # Project points to match crs of multibeam
plot(preds[[1]])
plot(habitat.vect, add = T)

# Run the extraction process ----
tidy.habitat_t   <- as.data.frame(habitat.vect, geom = "XY") %>%                # Make a dataframe of reprojected habitat
  left_join(tidy.habitat)                                                       # Lazy way of getting WGS84 lat longs back
habitat.bathy.derivatives   <- cbind(tidy.habitat_t, 
                                     terra::extract(preds, habitat.vect)) %>%   # Extract bathymetry derivatives for modelling
  dplyr::filter(!is.na(depth),                                                  # Remove samples outside of the study area
                !is.na(roughness)) %>%
  glimpse()

# Save the output ----
saveRDS(habitat.bathy.derivatives, paste0("data/tidy/", name, "_habitat-bathymetry.rds"))
