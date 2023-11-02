###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data
# Task:    Predicting habitat
# Author:  Claude Spencer
# Date:    August 2023
##

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(mgcv)
library(ggplot2)
library(viridis)
library(terra)
library(tidyverse)
library(sf)

# Set the study name ----
name <- 'example-bruv-workflow'

# Load habitat data ----
count <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/", 
                      name, "_Tidy-count.rds")) %>%
  clean_names() %>%
  glimpse()

length <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/", 
                        name, "_Tidy-length.rds")) %>%
  clean_names() %>%
  dplyr::select(-status) %>%
  glimpse()

dat <- bind_rows(count, length) %>%
  glimpse()

# Load the bathy and derivatives ----
preds  <- readRDS(paste0("1. Example R workflows (scripts to download)/data/spatial/rasters/", 
                         name, "_Bathymetry_derivatives.rds"))
plot(preds)

# Transform bathy to a dataframe to predict onto ----
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE) %>%
  dplyr::mutate(depth = abs(mbdepth)) %>%                                       # Rename to match for predict.gam and swap direction to match
  clean_names() %>%
  glimpse()

# Set models using the formula from top model from '03_model-selection.R' ----
# Great than Length of 50% maturity
m_mature <- gam(number ~ 
                 s(detrended, k = 3, bs = "cr"), 
               data = dplyr::filter(dat, response %in% "greater than Lm"), 
               family = tw())
summary(m_mature)
plot(m_mature, pages = 1, residuals = T, cex = 5)

# Total abundance
m_totabund <- gam(number ~ 
                  s(depth, k = 3, bs = "cr"), 
                data = dplyr::filter(dat, response %in% "total_abundance"), 
                family = gaussian(link = "identity"))
summary(m_totabund)
plot(m_totabund, pages = 1, residuals = T, cex = 5)

# Species richness
m_specrich <- gam(number ~ 
                    s(depth, k = 3, bs = "cr"), 
                  data = dplyr::filter(dat, response %in% "species_richness"), 
                  family = gaussian(link = "identity"))
summary(m_specrich)
plot(m_specrich, pages = 1, residuals = T, cex = 5)

# Predict, rasterise and plot habitat predictions ----
preddf <- cbind(preddf, 
                "pmature" = predict(m_mature, preddf, type = "response"),
                "ptotabund" = predict(m_totabund, preddf, type = "response"),
                "pspecrich" = predict(m_specrich, preddf, type = "response")) %>%
  glimpse()

prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p")),
                        crs = crs(preds)) %>%
  aggregate(fact = 5, fun = "mean")                                             # Aggregate to speed up plots 
plot(prasts)
summary(prasts)

# Transform back to a dataframe for tidy plotting ----
preddf_a <- as.data.frame(prasts, xy = T, na.rm = T) %>%
  glimpse()

# Save final predictions ----
saveRDS(preddf_a, file = paste0("1. Example R workflows (scripts to download)/model output/habitat/", 
                              name, "_Habitat-prediction.RDS"))
