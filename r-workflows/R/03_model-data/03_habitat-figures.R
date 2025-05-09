###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data
# Task:    Preparing tidy plots of predicted and dominant habitat
# Author:  Claude Spencer
# Date:    August 2023
##

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(terra)
library(sf)
library(viridis)
library(patchwork)

# Set the study name ----
name <- 'example-bruv-workflow'

# Load the habitat predictions ----
# UTM Zone 50
preddf <- readRDS(paste0("1. Example R workflows (scripts to download)/model output/habitat/", 
                         name, "_Habitat-prediction.RDS")) %>%
  dplyr::mutate(dom_tag = case_when(dom_tag %in% "sand" ~ "Sand",
                                    dom_tag %in% "inverts" ~ "Sessile invertebrates",
                                    dom_tag %in% "macro" ~ "Macroalgae",
                                    dom_tag %in% "seagrass" ~ "Seagrasses")) %>%
  glimpse()

# Load marine park data from CAPAD 2022 ----
# sf_use_s2(F)
marine.parks <- st_read("1. Example R workflows (scripts to download)/data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_make_valid() %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, 
                                            "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::filter(ZONE_TYPE %in% c("National Park Zone", "Sanctuary Zone"),
                STATE %in% "WA") %>%                                            # Change here to speed up plotting for your location
  st_transform(32750) 

# Set colours for habitat plotting ----
unique(preddf$dom_tag)
hab_fills <- scale_fill_manual(values = c(
  "Sand" = "wheat",
  "Sessile invertebrates" = "plum",
  "Seagrasses" = "forestgreen",
  "Macroalgae" = "darkgoldenrod4"
), name = "Habitat")

# Build plot elements for dominant habitat ----
p1 <- ggplot() +
  geom_tile(data = preddf, aes(x, y, fill = dom_tag)) +
  hab_fills + 
  new_scale_fill() +
  geom_sf(data = marine.parks, fill = NA, aes(colour = ZONE_TYPE), 
          size = 0.2, show.legend = F) +
  scale_colour_manual(values = c("National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054"),
                    name = "Marine Parks") +
  coord_sf(xlim = c(min(preddf$x), max(preddf$x)),
           ylim = c(min(preddf$y), max(preddf$y))) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_minimal()
png(filename = paste0("1. Example R workflows (scripts to download)/plots/", name, "_dominant-habitat.png"),
    units = "in", res = 300, height = 4, width = 8)
p1
dev.off()

# Transform habitat predictions for easy plotting with ggplot::facet_wrap ----
indclass <- preddf %>%
  pivot_longer(cols = starts_with("p"), names_to = "habitat", 
               values_to = "Probability") %>%
  dplyr::mutate(habitat = case_when(habitat %in% "pinverts" ~ "Sessile invertebrates",
                                    habitat %in% "pmacro" ~ "Macroalgae",
                                    habitat %in% "prock" ~ "Rock",
                                    habitat %in% "psand" ~ "Sand",
                                    habitat %in% "pseagrass" ~ "Seagrass",
                                    habitat %in% "preef" ~ "Reef")) %>%
  glimpse()

# Build plot elements for individual habitat probabilities ----
p2 <- ggplot() +
  geom_tile(data = indclass, aes(x, y, fill = Probability)) +
  scale_fill_viridis(option = "D", direction = -1) +
  new_scale_fill() +                     
  geom_sf(data = marine.parks, fill = NA, aes(colour = ZONE_TYPE), 
          size = 0.2, show.legend = F) +
  scale_colour_manual(values = c("National Park Zone" = "#7bbc63",
                                 "Sanctuary Zone" = "#bfd054"),
                      name = "Marine Parks") +
  coord_sf(xlim = c(min(preddf$x), max(preddf$x)),
           ylim = c(min(preddf$y), max(preddf$y))) +
  labs(x = NULL, y = NULL, fill = "Probability") +
  theme_minimal()+
  facet_wrap(~habitat) +
  theme(axis.text.x = element_text(size = 7))
png(filename = paste0("1. Example R workflows (scripts to download)/plots/", name, "_individual-habitat.png"),
    units = "in", res = 300, height = 4, width = 8)
p2
dev.off()
