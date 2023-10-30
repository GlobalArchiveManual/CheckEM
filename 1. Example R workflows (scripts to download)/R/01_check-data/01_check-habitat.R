# Error checking of stereo-BRUV habitat data exported from TransectMeasure ----

# This script is designed to be used interactively to find basic annotation errors that should be made to original EventMeasure (.EMObs) or generic annotation files AND for subsequent data analysis

# NOTE: ERRORS SHOULD BE FIXED IN THE .TMObs AND THE SCRIPT RE-RAN!

# NOTE: This example dataset has been analysed using the standard 'TM schema_BROAD.MORPH.TYPE.txt' file
# Follow the notes to edit the script for use with the broad only 'TM schema_BROAD.txt' and fine 'TM schema_BROAD.MORPH.TYPE.FINE.txt' files 

### OBJECTIVES ###
# 1. Import data and run basic error reports
# 2. Run more thorough checks on the data against the metadata and images in the original directory
# 3. Tidy data into broad and detailed point-level and percent-cover data
# 4. Tidy the final data into organised dataframes
# 5. Inspect for tidy data for any errors
# 6. Export tidy datasets to a .csv format suitable for use in modelling and statistical testing
# 7. Visualise the data spatially

# Please forward any updates and improvements to tim.langlois@uwa.edu.au & claude.spencer@uwa.edu.au or raise an issue in the "forward-facing-benthic-composition-annotation" GitHub repository

# Clear memory
rm(list = ls())

## Libraries required
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive")                                 # Run this once to install the GlobalArchive package
library(GlobalArchive)

# To tidy data
library(tidyverse)

# To visualise data
library(ggplot2)
library(ggbeeswarm)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)

# Set study name 
name <- "example-bruv-workflow"

### 1. Import data and run basic error reports ----
# New version of 'ga.read.files_em.csv
# Reads in metadata
ga.read.files_em.csv <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c"))%>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    ga.clean.names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid, c("_Metadata.csv" = "")))
}

# 1. Load and format metadata ----
metadata <- list.files(path = "1. Example R workflows (scripts to download)/data/raw/",      # This replaces ga.list.files
                       recursive = F,
                       pattern = "_Metadata.csv",
                       full.names = T) %>%
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::select(campaignid, sample, latitude, longitude, date.time, location,  # Review columns to align with GlobalArchive - plus make metadata match
                site, depth, observer.count, observer.length, successful.count, successful.length, 
                successful.habitat.forward, successful.habitat.backward) %>%
  glimpse()                                                                   # Preview the data

write.csv(metadata, file = paste0("1. Example R workflows (scripts to download)/data/tidy/",
                                  name, "_Metadata.csv"), row.names = F)

# Read in the raw habitat data
ga.read.files_tm.txt <- function(dir, sample) {
  if (sample %in% "opcode") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character")) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = opcode) %>%
      glimpse()
  }
  
  else if (sample %in% "period") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character")) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = period) %>%
      glimpse()
  }
  
  else {
    stop("Sample must be one of: c('opcode', 'period')")
  }}

points <- ga.read.files_tm.txt("1. Example R workflows (scripts to download)/data/raw/",
                               sample = "opcode")

habitat <- points %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, id,                                         # Should I be keeping in relief_annotated?
                starts_with("level"), scientific, qualifiers, caab_code) %>%
  glimpse()

relief <- points %>%
  dplyr::filter(relief_annotated %in% "yes") %>%
  dplyr::select(campaignid, sample, id,                                         # Should I be keeping in relief_annotated?
                starts_with("level"), scientific, qualifiers, caab_code) %>%
  glimpse()

# Check to see if you have samples with points extra or missing points annotated
num.annotations.habitat <- habitat %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n())                                             

num.annotations.relief <- relief %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) 

# If you have samples with missing points, you need to rectify this in the original .TMObs file!

habitat.missing.metadata <- anti_join(habitat, metadata, by = c("campaignid", "sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat, by = c("campaignid", "sample")) %>%
  glimpse()

###   STOP     AND    READ      THE     NEXT      PART     ###


# We strongly encourage you to fix these errors at the source (i.e. TMObs)
# Now check through the files in your "Errors to check" folder and make corrections to .TMObs / generic files and then re-run this script

### 3. Tidy data into broad and detailed point-level and percent cover dataframes ----
# Create broad point annotations

# Function to make broad classes??? Its a bit shit
broad.points <- habitat %>%
  dplyr::mutate(count = 1) %>%                                                  # Add a count column to summarise the number of points
  dplyr::select(campaignid, sample, count, level_2, id) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample) %>%
  pivot_wider(names_from = level_2, values_from = count, 
              values_fill = 0, names_prefix = "broad.") %>%                     # Spread the data to wide format
  dplyr::select(-id) %>%
  summarise(across(starts_with("broad"), sum)) %>%                                                  # Add the points per sample across all broad habitat columns
  ungroup() %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%   # Take row sums of all data columns
  ga.clean.names() %>%                                                          # Clean names using GlobalArchive function
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data                                                                    # Preview the data

# Create relief
relief.grid <- relief %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%                       # Remove Open water and Unknown entries from broad
  dplyr::mutate(relief.rank = as.numeric(level_5)) %>%
  dplyr::select(campaignid, sample, relief.rank) %>%                            # Remove the original relief scores
  group_by(campaignid, sample) %>%
  summarise(mean.relief = mean (relief.rank), sd.relief = sd (relief.rank)) %>% # Create mean and standard deviation relief
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

### 4. Tidy the final data into organised dataframes ----

habitat.broad.points <- metadata %>%
  left_join(broad.points, by = c("campaignid", "sample")) %>% # Join metadata with habitat data
  left_join(relief.grid) %>% # And relief
  glimpse()

### 5. Inspect for tidy data for any errors ----
# Typical errors found could include samples where the wrong class has been assigned (ie. 20 point of octocoral instead of 20 points of sand)
# Or high cover of uncommon or rare classes

# Transform the data in a format suitable for use in ggplot 
# Broad habitat
broad.hab.plot <- habitat.broad.points %>%
  pivot_longer(cols = starts_with("broad"), 
               names_to = "biota", values_to = "num.points") %>%                # Pivots dataframes into long format to plot
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude))

# Broad relief
broad.rel.plot <- relief %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%                              # Remove Open water and Unknown entries from broad
  dplyr::mutate(relief.rank = as.numeric(level_5)) %>%
  dplyr::select(campaignid, sample, relief.rank) %>%
  dplyr::group_by(campaignid, sample, relief.rank) %>%
  dplyr::mutate(count = 1) %>%
  summarise(num.points = sum(count)) %>%
  ungroup() %>%
  glimpse()
  
# Plot and visualise the broad habitat dataset
gg.broad.hab <- ggplot() +
  geom_quasirandom(data = broad.hab.plot,                                       # Create a dotplot - each point represents a sample
                   aes(x = num.points, y = biota), groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()
gg.broad.hab

# Plot and visualise the relief dataset
gg.relief <- ggplot() +
  geom_quasirandom(data = broad.rel.plot,                                       # Create a dotplot - each point represents a sample
                   aes(x = num.points, y = relief.rank), 
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
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
write.csv(habitat.broad.points,file =
            paste("1. Example R workflows (scripts to download)/data/tidy/", 
                  paste(name,"Habitat.csv", sep = "_"),
                  sep = "/"), row.names = FALSE)

### 7. Spatially visualise the data ----

# This plot uses spatial pie charts to visualise the proportion of habitat classes in each sample
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Create a color palette to plot the scatterpies with using the 'RColorbrewer' palettes
cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(habitat.broad.points[grep("broad", names(habitat.broad.points))])) # Expand the palette to the length of your unique habitat classes

min.lon <- min(as.numeric(habitat.broad.points$longitude))
min.lat <- min(as.numeric(habitat.broad.points$latitude))
max.lon <- max(as.numeric(habitat.broad.points$longitude))
max.lat <- max(as.numeric(habitat.broad.points$latitude))

# Create the plot
pie.chart <- leaflet() %>%                                                      # Create a leaflet plot
  addTiles(group = "Open Street Map") %>%                                                                # Add the Open Street Map base layer
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%            # Add ESRI satellite imagery as a base layer
  addLayersControl(baseGroups = c("World Imagery", "Open Street Map"), 
                   options = layersControlOptions(collapsed = FALSE)) %>%       # Add controls to switch between layers
  addMinicharts(habitat.broad.points$longitude, habitat.broad.points$latitude,  # Add a spatial minichart using spatial information from the metadata
                type = "pie",                                                   # Make it a spatial pie chart
                colorPalette = cols,                                            # Color using the RColorbrewer palette
                chartdata = habitat.broad.points[grep("broad", names(habitat.broad.points))], # Select only columns starting with 'broad'
                width = 20, transitionTime = 0) %>%                                 # Set the size and transition time of the points
  setView(mean(as.numeric(habitat.broad.points$longitude)), 
          mean(as.numeric(habitat.broad.points$latitude)), zoom = 12)
pie.chart                                                                       # Display the plot

# This plot uses spatial bubble plots to frequency of occurrence of each habitat class
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Change the class below for each habitat class
hab.name <- 'broad.substrate'

# Filter the data for plotting
overzero <-  broad.hab.plot %>%                                                 # Any sample with a value greater than zero
  filter(biota %in% hab.name & num.points > 0) 

equalzero <- broad.hab.plot %>%                                                 # Any sample with a value equal to zero
  filter(biota %in% hab.name & num.points == 0)

# Create the plot
bubble.plot <- leaflet(data = broad.hab.plot) %>%                               # Create a leaflet plot
  addTiles() %>%                                                                # Add the Open Street Map base layer
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%            # Add ESRI satellite imagery as a base layer
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), 
                   options = layersControlOptions(collapsed = FALSE))           # Add controls to switch between layers 

if (nrow(overzero)) {                                                           # Add spatial bubble plots if the data is greater than zero
  bubble.plot <- bubble.plot %>%
    addCircleMarkers(data = overzero, lat = ~ latitude, lng = ~ longitude,      # Add the bubble plots
      radius = ~ num.points + 3,                                                # Scale the size of the point by the data value
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
