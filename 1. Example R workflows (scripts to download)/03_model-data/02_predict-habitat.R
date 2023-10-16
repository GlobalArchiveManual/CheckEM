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
name <- '2021-2022_SwC_BOSS'

# Load habitat data ----
dat   <- readRDS(paste0("data/tidy/", name, "_Habitat-bathymetry.rds")) %>%
  dplyr::mutate(mbdepth = abs(mbdepth)) %>%                                     # Transform to positive otherwise sqrt(mbdepth) will error
  pivot_wider(names_from = habitat, values_from = number, values_fill = 0) %>%
  glimpse()

# Load the bathy and derivatives ----
preds  <- readRDS(paste0("data/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
plot(preds)

# Transform bathy to a dataframe to predict onto ----
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE) %>%
  dplyr::mutate(mbdepth = abs(mbdepth))

# Set models using the formula from top model from '03_model-selection.R' ----
# Sessile invertebrates
m_inverts <- gam(cbind(sessile.invertebrates, total.points.annotated - sessile.invertebrates) ~ 
                 s(mbdepth,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") +
                 s(TPI, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_inverts)
plot(m_inverts, pages = 1, residuals = T)

# Rock
m_rock <- gam(cbind(rock, total.points.annotated - rock) ~ 
                   s(detrended,     k = 5, bs = "cr")  + 
                   s(mbdepth, k = 5, bs = "cr") + 
                   s(roughness, k = 5, bs = "cr"), 
                 data = dat, method = "REML", family = binomial("logit"))
summary(m_rock)
plot(m_rock, pages = 1, residuals = T)

# Sand
m_sand <- gam(cbind(sand, total.points.annotated - sand) ~ 
                s(detrended,     k = 5, bs = "cr")  + 
                s(mbdepth, k = 5, bs = "cr") +
                s(TPI, k = 5, bs = "cr"), 
              data = dat, method = "REML", family = binomial("logit"))
summary(m_sand)
plot(m_sand, pages = 1, residuals = T)

# Seagrasses
m_seagrass <- gam(cbind(seagrasses, total.points.annotated - seagrasses) ~ 
                s(detrended,     k = 5, bs = "cr")  + 
                s(mbdepth, k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr"), 
              data = dat, method = "REML", family = binomial("logit"))
summary(m_seagrass)
plot(m_seagrass, pages = 1, residuals = T)

# Macroalgae
m_macro <- gam(cbind(macroalgae, total.points.annotated - macroalgae) ~ 
                 s(detrended,     k = 5, bs = "cr")  + 
                 s(mbdepth, k = 5, bs = "cr") +
                 s(TPI, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_macro)
plot(m_macro, pages = 1, residuals = T)

# Predict, rasterise and plot habitat predictions ----
preddf <- cbind(preddf, 
                "pmacro" = predict(m_macro, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "pseagrass" = predict(m_seagrass, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response")) %>%
  glimpse()

prasts <- rast(preddf %>% dplyr::select(x, y, pmacro, prock, psand, pseagrass, pinverts),
                        crs = crs(preds)) %>%
  aggregate(fact = 5, fun = "mean")                                             # Aggregate to speed up plots 
plot(prasts)
summary(prasts)

# Transform back to a dataframe for tidy plotting ----
preddf <- as.data.frame(prasts, xy = T, na.rm = T) %>%
  glimpse()

# Categorise by dominant habitat ----
preddf$dom_tag <- apply(preddf %>% dplyr::select(pmacro, prock, psand, pseagrass, pinverts), 1,
                         FUN = function(x){names(which.max(x))})
preddf$dom_tag <- sub('.', '', preddf$dom_tag)
head(preddf)

# Save final predictions ----
saveRDS(preddf, file = paste0("model out/", name, "_habitat-prediction.RDS"))
