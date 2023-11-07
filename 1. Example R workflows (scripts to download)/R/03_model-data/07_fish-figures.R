# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(ggplot2)
library(tidyverse)
library(sf)

# Set the study name ----
name <- 'example-bruv-workflow'

dat <- readRDS(paste0("1. Example R workflows (scripts to download)/model output/habitat/", 
                      name, "_Habitat-prediction.RDS")) %>%
  pivot_longer(names_to = "response", values_to = "value",
               cols = !c(x, y)) %>%
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

resp.vars <- unique(dat$response)

for (i in 1:length(resp.vars)) {
  use.dat <- dat %>%
    dplyr::filter(response %in% resp.vars[i])
  
  p <- ggplot() +
    geom_tile(data = use.dat, aes(x, y, fill = value)) +
    scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"), na.value = "transparent") +
    labs(fill = resp.vars[i], x = NULL, y = NULL) +
    new_scale_fill() +                     
    geom_sf(data = marine.parks, fill = NA, aes(colour = ZONE_TYPE), 
            size = 0.2, show.legend = F) +
    coord_sf(xlim = c(min(dat$x), max(dat$x)),
             ylim = c(min(dat$y), max(dat$y))) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 7))
  assign(paste0("gg_", resp.vars[i]), p)
}

gg_grid <- gg_pmature + gg_ptotabund + gg_pspecrich +
  theme(legend.justification = "left") +
  plot_layout(ncol = 1, nrow = 3)

png(filename = paste(paste("1. Example R workflows (scripts to download)/plots", name, sep = "/"),               
                     "Fish-predictions.png", sep = "_"),
    width = 4.5, height = 6, res = 300, units = "in")                             # Change the dimensions here as necessary
gg_grid
dev.off()
