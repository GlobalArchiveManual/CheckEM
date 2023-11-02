rm(list = ls())

library(tidyverse)
devtools::load_all("./")

name <- "example-bruv-workflow"

metadata.bathy.derivatives <- readRDS(paste0("1. Example R workflows (scripts to download)/data/tidy/", 
                                                       name, "_Metadata-bathymetry-derivatives.rds")) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  glimpse()

habitat <- readRDS(paste0("1. Example R workflows (scripts to download)/data/staging/",
                           name, "_Habitat.rds")) %>%
  # USe your ecological brain pls when deciding habitat classes
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2,
                                    level_2 %in% "Seagrasses" ~ level_2,
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3,
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3, 
                                    level_2 %in% "Sponges" ~ "Sessile invertebrates",
                                    level_2 %in% "Sessile invertebrates" ~ level_2,
                                    level_2 %in% "Bryozoa" ~ "Sessile invertebrates",
                                    level_2 %in% "Cnidaria" ~ "Sessile invertebrates")) %>%
  dplyr::select(campaignid, sample, habitat, number) %>%
  group_by(campaignid, sample, habitat) %>%
  dplyr::tally(number, name = "number") %>%
  dplyr::mutate(total.points.annotated = sum(number)) %>%
  ungroup() %>%
  pivot_wider(names_from = "habitat", values_from = "number", values_fill = 0) %>%
  dplyr::mutate(reef = Macroalgae + Seagrasses + `Sessile invertebrates` + `Consolidated (hard)`) %>%
  pivot_longer(cols = c("Macroalgae", "Seagrasses", "Sessile invertebrates",
                        "Consolidated (hard)", "Unconsolidated (soft)", "reef"), 
               names_to = "habitat", values_to = "number") %>%
  glimpse()

tidy.relief <- readRDS(paste0("1. Example R workflows (scripts to download)/data/staging/",
                               name, "_Relief.rds")) %>%
  uncount(number) %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(mean.relief = mean(as.numeric(level_5)),
                   sd.relief = sd(as.numeric(level_5), na.rm = T)) %>%
  ungroup() %>%
  glimpse()

tidy.habitat <- metadata.bathy.derivatives %>%
  left_join(habitat) %>%
  left_join(tidy.relief) %>%
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude)) %>%
  glimpse()

saveRDS(tidy.habitat, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                      name, "_Tidy-habitat.rds"))

plot.relief <- readRDS(paste0("1. Example R workflows (scripts to download)/data/staging/",
                               name, "_Relief.rds")) %>%
  group_by(campaignid, sample, level_5) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  dplyr::mutate(class.relief = as.factor(level_5)) %>%
  glimpse()

# Plot and visualise the broad habitat dataset
gg.broad.hab <- ggplot() +
  geom_quasirandom(data = tidy.habitat,                                       # Create a dotplot - each point represents a sample
                   aes(x = total.points.annotated, y = habitat), 
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()
gg.broad.hab

# Plot and visualise the relief dataset
gg.relief <- ggplot() +
  geom_quasirandom(data = plot.relief,                                       # Create a dotplot - each point represents a sample
                   aes(x = number, y = class.relief), 
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.05) +
  labs(x = "Number of points", y = "Relief (0-5)") + 
  theme_classic()
gg.relief

# DIRECTORIES ARE ALL WRONG FOR SAVING OUT !!!!

# Save the plots to refer to later
# ggsave(paste(plot.dir, paste(name, "broad.habitat.png", sep = "."), sep = "/"),
#        gg.broad.hab,dpi = 600,width = 6.0, height = 3.0)
# 
# ggsave(paste(plot.dir, paste(name, "detailed.habitat.png", sep = "."), sep ="/"),
#        gg.detailed.hab,dpi = 600,width=8.0, height = 6.0)
# 
# ggsave(paste(plot.dir, paste(name, "relief.png", sep = "."), sep = "/"),
#        gg.relief,dpi = 600,width = 6.0, height = 3.0)

### 6. Export tidy datasets to a .csv format suitable for use in modelling and statistical testing ----
# Export point annotations
saveRDS(tidy.habitat, file =
            paste("1. Example R workflows (scripts to download)/data/tidy/", 
                  paste(name,"Habitat.rds", sep = "_"),
                  sep = "/"))

### 7. Spatially visualise the data ----

# This plot uses spatial pie charts to visualise the proportion of habitat classes in each sample
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Create a color palette to plot the scatterpies with using the 'RColorbrewer' palettes
cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(tidy.habitat$habitat))) # Expand the palette to the length of your unique habitat classes

min.lon <- min(as.numeric(tidy.habitat$longitude))
min.lat <- min(as.numeric(tidy.habitat$latitude))
max.lon <- max(as.numeric(tidy.habitat$longitude))
max.lat <- max(as.numeric(tidy.habitat$latitude))
plot.habitat <- tidy.habitat %>%
  pivot_wider(names_from = "habitat", values_from = "number",
              names_prefix = "broad.") %>%
  glimpse()

# Create the plot
pie.chart <- leaflet() %>%                                                      # Create a leaflet plot
  addTiles(group = "Open Street Map") %>%                                                                # Add the Open Street Map base layer
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%            # Add ESRI satellite imagery as a base layer
  addLayersControl(baseGroups = c("World Imagery", "Open Street Map"), 
                   options = layersControlOptions(collapsed = FALSE)) %>%       # Add controls to switch between layers
  addMinicharts(plot.habitat$longitude, plot.habitat$latitude,  # Add a spatial minichart using spatial information from the metadata
                type = "pie",                                                   # Make it a spatial pie chart
                colorPalette = cols,                                            # Color using the RColorbrewer palette
                chartdata = plot.habitat[grep("broad", names(plot.habitat))], # Select only columns starting with 'broad'
                width = 20, transitionTime = 0) %>%                                 # Set the size and transition time of the points
  setView(mean(as.numeric(plot.habitat$longitude)), 
          mean(as.numeric(plot.habitat$latitude)), zoom = 12)
pie.chart                                                                       # Display the plot

# This plot uses spatial bubble plots to frequency of occurrence of each habitat class
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Change the class below for each habitat class
hab.name <- 'Sessile invertebrates'

# Filter the data for plotting
overzero <-  tidy.habitat %>%                                                 # Any sample with a value greater than zero
  filter(habitat %in% hab.name & number > 0) 

equalzero <- tidy.habitat %>%                                                 # Any sample with a value equal to zero
  filter(habitat %in% hab.name & number == 0)

# Create the plot
bubble.plot <- leaflet(data = tidy.habitat) %>%                               # Create a leaflet plot
  addTiles() %>%                                                                # Add the Open Street Map base layer
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%            # Add ESRI satellite imagery as a base layer
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), 
                   options = layersControlOptions(collapsed = FALSE))           # Add controls to switch between layers 

if (nrow(overzero)) {                                                           # Add spatial bubble plots if the data is greater than zero
  bubble.plot <- bubble.plot %>%
    addCircleMarkers(data = overzero, lat = ~ latitude, lng = ~ longitude,      # Add the bubble plots
                     radius = ~ number + 3,                                                # Scale the size of the point by the data value
                     fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(sample))        # Format the points and add labels for sample code
}
if (nrow(equalzero)) {                                                          # Add spatial bubble plots if the data is equal to zero
  bubble.plot <- bubble.plot %>%
    addCircleMarkers(data = equalzero, lat = ~ latitude, lng = ~ longitude,     # Add the bubble plots
                     radius = 2,                                                               # Scale the points at a constant size 
                     fillOpacity = 0.5, color = "white", stroke = FALSE, 
                     label = ~ as.character(sample))                                           # Format the points and add labels for sample code
}
bubble.plot                                                                     # Display the plot 
