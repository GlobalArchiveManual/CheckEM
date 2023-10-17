###
# Project: Parks OMP Ningaloo
# Data:    Fish spatial prediction data
# Task:    Fish figures - predictions
# author:  Claude Spencer
# date:    November 2022
##

rm(list=ls())

# library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(cowplot)
library(ggnewscale)

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"                                          # Maybe wrong? 
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
sppcrs  <- "+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs"           # crs for sp objects

# read in outputs from 'R/habitat_fish_model_predict.R'
spreddf <- readRDS("output/fssgam-fish/site_fish_predictions.rds")              # site predictions only

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# Set cropping extent - larger than most zoomed out plot
e <- ext(112, 115, -23, -21)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
unique(mpa$ZoneName)
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
                                                "Recreational Use Zone",
                                                "Habitat Protection Zone",
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
sanc <- wampa %>%
  dplyr::filter(waname %in% "Sanctuary Zone")

cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

nmpa_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Recreational Use Zone" = "#ffb36b",
                                           "Habitat Protection Zone" = "#fff8a3"
))

wampa_cols <- scale_colour_manual(values = c("Marine Management Area" = "#b7cfe1",
                                            "Conservation Area" = "#b3a63d",
                                            "Sanctuary Zone" = "#bfd054",
                                            "General Use Zone" = "#bddde1",
                                            "Recreation Area" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9",
                                            "Marine Nature Reserve" = "#bfd054"
))

# plotting broad maps
#npz6
#total abundance
p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x = x, y = y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Total abundance", title = "Whole assemblage")+
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          
  # geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y)), crs = sppcrs) +
  theme_minimal() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7))

# p11

#species richness
p21 <- ggplot() +
  geom_tile(data = spreddf, aes(x = x, y = y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Species richness")+
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          
  # geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y)), crs = sppcrs) +
  theme_minimal() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7))

# p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = spreddf, aes(x = x, y = y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Legal", title = "Targeted assemblage")+
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          
  # geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y)), crs = sppcrs) +
  theme_minimal() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7))

# p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x = x, y = y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Sublegal")+
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          
  # geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  coord_sf(xlim = c(min(spreddf$x), max(spreddf$x)), ylim = c(min(spreddf$y), max(spreddf$y)), crs = sppcrs) +
  theme_minimal() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7))

# p41

gg.predictions <- (p11 + p21) / (p31 + p41) & theme(legend.justification = "left")    
gg.predictions

ggsave("figures/fish/site_fish_predictions.png", gg.predictions,width = 8, height = 7, dpi = 160)
