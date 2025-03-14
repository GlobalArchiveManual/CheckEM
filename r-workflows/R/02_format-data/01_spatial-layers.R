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
name <- 'example-bruv-workflow'

# Load bathymetry data ----
bathy <- rast("r-workflows/data/spatial/rasters/swc_ga250m.tif") %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()

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

# Save file for use later
preds_w <- wrap(preds)
saveRDS(preds_w, 
          file = paste0("r-workflows/data/spatial/rasters/", 
                        name, "_bathymetry_derivatives.rds"))

# Load metadata ----
metadata <- readRDS(paste0("r-workflows/data/tidy/", 
                            name, "_Metadata.rds")) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  glimpse()

# Transform the habitat to a SpatVector ----
metadata.vect <- vect(metadata, geom = c("longitude_dd", "latitude_dd"), crs = "epsg:4326")                                                               # Project points to match crs of multibeam
plot(preds[[1]])
plot(metadata.vect, add = T)

# Run the extraction process ----
tidy.metadata_t   <- as.data.frame(metadata.vect, geom = "XY") %>%                                                     # Lazy way of getting WGS84 lat longs back
  left_join(metadata)
metadata.bathy.derivatives   <- cbind(tidy.metadata_t, 
                                     terra::extract(preds, metadata.vect)) %>%   # Extract bathymetry derivatives for modelling
  dplyr::filter(!is.na(depth),                                                  # Remove samples outside of the study area
                !is.na(roughness)) %>%
  glimpse()

# Save the output ----
saveRDS(metadata.bathy.derivatives, paste0("r-workflows/data/tidy/", name, "_metadata-bathymetry-derivatives.rds"))
