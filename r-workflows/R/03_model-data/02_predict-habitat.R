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
dat <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/", 
                      name, "_Tidy-habitat.rds")) %>%
  pivot_wider(values_from = "number", names_from = "habitat", values_fill = 0) %>%
  clean_names() %>%
  glimpse()

# Load the bathy and derivatives ----
preds  <- readRDS(paste0("1. Example R workflows (scripts to download)/data/spatial/rasters/", 
                         name, "_Bathymetry_derivatives.rds"))
plot(preds)

# Transform bathy to a dataframe to predict onto ----
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE) %>%
  dplyr::mutate(depth = abs(mbdepth)) %>%                                       # Rename to match for predict.gam
  clean_names() %>%
  glimpse()

# Set models using the formula from top model from '03_model-selection.R' ----
# Sessile invertebrates
m_inverts <- gam(cbind(sessile_invertebrates, total_points_annotated - sessile_invertebrates) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_inverts)
plot(m_inverts, pages = 1, residuals = T, cex = 5)

# Rock
m_rock <- gam(cbind(consolidated_hard, total_points_annotated - consolidated_hard) ~ 
                   s(depth,     k = 5, bs = "cr"), 
                 data = dat, method = "REML", family = binomial("logit"))
summary(m_rock)
plot(m_rock, pages = 1, residuals = T, cex = 5)

# Sand
m_sand <- gam(cbind(unconsolidated_soft, total_points_annotated - unconsolidated_soft) ~ 
                s(aspect,     k = 5, bs = "cr")  + 
                s(roughness, k = 5, bs = "cr") +
                s(tpi, k = 5, bs = "cr"), 
              data = dat, method = "REML", family = binomial("logit"))
summary(m_sand)
plot(m_sand, pages = 1, residuals = T, cex = 5)

# Seagrasses
m_seagrass <- gam(cbind(seagrasses, total_points_annotated - seagrasses) ~ 
                s(aspect,     k = 5, bs = "cr")  + 
                s(depth, k = 5, bs = "cr") + 
                s(tri, k = 5, bs = "cr"), 
              data = dat, method = "REML", family = binomial("logit"))
summary(m_seagrass)
plot(m_seagrass, pages = 1, residuals = T, cex = 5)

# Macroalgae
m_macro <- gam(cbind(macroalgae, total_points_annotated - macroalgae) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") +
                 s(tpi, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_macro)
plot(m_macro, pages = 1, residuals = T, cex = 5)

# Reef
m_reef <- gam(cbind(reef, total_points_annotated - reef) ~ 
                 s(aspect,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") +
                 s(tpi, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_reef)
plot(m_reef, pages = 1, residuals = T, cex = 5)

# Predict, rasterise and plot habitat predictions ----
preddf <- cbind(preddf, 
                "pmacro" = predict(m_macro, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "pseagrass" = predict(m_seagrass, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response"),
                "preef" = predict(m_reef, preddf, type = "response")) %>%
  glimpse()

prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p")),
                        crs = crs(preds)) %>%
  aggregate(fact = 5, fun = "mean")                                             # Aggregate to speed up plots 
plot(prasts)
summary(prasts)

# Transform back to a dataframe for tidy plotting ----
preddf_a <- as.data.frame(prasts, xy = T, na.rm = T) %>%
  glimpse()

# Categorise by dominant habitat ----
preddf_a$dom_tag <- apply(preddf_a %>% dplyr::select(pmacro, prock, psand, pseagrass, pinverts), 1,
                         FUN = function(x){names(which.max(x))})
preddf_a$dom_tag <- sub('.', '', preddf_a$dom_tag)
glimpse(preddf_a)

# Save final predictions ----
saveRDS(preddf_a, file = paste0("1. Example R workflows (scripts to download)/model output/habitat/", 
                              name, "_Habitat-prediction.RDS"))
