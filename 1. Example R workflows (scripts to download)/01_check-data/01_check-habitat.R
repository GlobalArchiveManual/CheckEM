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
install_github("UWAMEGFisheries/GlobalArchive")                                 # Run this once to install the GlobalArchive package
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)

# To visualise data
library(ggplot2)
library(ggbeeswarm)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)

# Study name 
study <- "2021-05_Abrolhos_stereo-BRUVs"                                        # Enter your study name (campaign ID) here

# Set your working directory 
working.dir <- getwd()                                                          # Run this line for GitHub projects, or set your working directory manually
setwd(working.dir)

# This script uses a file structure with a main folder for R scripts, plots and data
# Within the data folder there are nested folders for raw data, tidy data, errors to check and raw habitat images
# We recommend that you replicate our folder structure, 
# however if you decide that a different file structure is more suitable for your project then change the directories below
raw.dir   <- "data/raw"
tidy.dir  <- "data/tidy"
error.dir <- "data/errors to check"
image.dir <- paste(working.dir, "data/images/BRUV", sep = "/")
plot.dir  <- "plots"

### 1. Import data and run basic error reports ----
# Read in metadata
metadata  <- read_csv(paste(raw.dir, 
                            paste0(study, "_Metadata.csv"), sep = "/")) %>%     # Read in the file
  ga.clean.names() %>%                                                          # Tidy the column names using GlobalArchive function 
  # mutate(opcode = as.character(opcode)) %>%                                   # Turn this line on if you have numerical sample names 
  glimpse()                                                                     # Preview the data

# Read in the raw habitat data
points <- read.delim(paste(raw.dir, 
                           paste0(study, "_Dot Point Measurements.txt"), sep = "/"),
                     header = T,skip = 4,stringsAsFactors = F) %>%              # Read in the text file
  ga.clean.names() %>%                                                          # Tidy the column names using GlobalArchive function
  # mutate(opcode = as.character(opcode)) %>%                                   # Turn on if you have numerical sample names
  select(opcode, image.row, image.col, broad, morphology, type) %>%             # Select only these columns to keep - remove 'morphology' and 'type' for the broad only schema, add in 'fine' for the fine schema
  glimpse()                                                                     # Preview the data

# Check to see if you have samples with points extra or missing points annotated
num.annotations.habitat <- points %>%
  group_by(opcode) %>%
  summarise(points.annotated = n())                                             # All have 20 points annotated in this example dataset

# If you have samples with missing points, you should rectify this in the original .TMObs file!

# read in the relief gridded annotations
relief <- read.delim(paste(raw.dir, 
                           paste0(study, "_Relief_Dot Point Measurements.txt"), sep = "/"),
                     header = T,skip = 4,stringsAsFactors = FALSE) %>%          # Read in the file
  ga.clean.names() %>%                                                          # Tidy the column names using GlobalArchive function
  # mutate(opcode = as.character(opcode)) %>%                                   # Turn on if you have numerical sample names
  select(opcode,relief) %>%                                                     # Select only these columns to keep
  glimpse() # Preview the data

# Check to see if you have samples with points extra or missing points annotated
num.annotations.relief  <- relief %>%
  group_by(opcode) %>%
  summarise(relief.annotated = n())                                             # All have 20 points annotated in this example dataset

# If you have samples with missing points, you should rectify this in the original .TMObs file!

### 2. Run more thorough checks on the data against the metadata and images in the original directory ----
# Point to images folders
image.list <- dir(image.dir) %>%                                                # Selects the directory where your habitat images are stored
  as.data.frame() %>%                                                           # Convert to a dataframe
  rename(image.name = 1) %>%                                                    # Rename the column that contains image names
  separate(image.name, sep = "_", into = c("opcode"), 
                           extra = "drop", remove = F)

# Create a data frame to check habitat annotations against using original metadata
qaqc.habitat <- metadata %>%
  dplyr::select(opcode, date) %>%                                               # Select only these columns to keep
  dplyr::left_join(image.list) %>%                                              # Joins sample and date from metadata with the list of images
  dplyr::left_join(num.annotations.habitat) %>%                                 # And the number of annotation points per image
  glimpse()                                                                     # Preview the data

# Habitat point annotation checking
# Find samples where images and annotations are missing
habitat.missing.image.not.annotated <- qaqc.habitat %>%
  filter(image.name %in% c("NA",NA)) %>%
  filter(points.annotated %in% c("NA",NA))

# Find samples where image is exported but missing annotations
habitat.missing.annotation <- qaqc.habitat %>%
  filter(!image.name %in% c("NA",NA))%>%
  filter(points.annotated %in% c("NA",NA))

# Find samples annotated but missing images
habitat.missing.image <- qaqc.habitat %>%
  filter(image.name %in% c("NA",NA)) %>%
  filter(!points.annotated %in% c("NA",NA))

# Find samples missing points, or with extra points annotated
habitat.wrong.points <- num.annotations.habitat %>%
  filter(points.annotated!= 20)                                                 # Change here for your number of points (20 for single images, 80 for composite imagery)

# Relief grid annotation checking
# Create a data frame to check relief annotations against using original metadata
qaqc.relief <- metadata %>%
  dplyr::select(opcode, date) %>%                                               # Select only these columns to keep
  dplyr::left_join(image.list) %>%                                              # Joins sample and date from metadata with the list of images
  dplyr::left_join(num.annotations.relief) %>%                                  # And the number of annotation points per image
  glimpse()                                                                     # Preview the data

# Find samples where images and annotations are missing
relief.missing.image.not.annotated <- qaqc.relief %>%
  filter(image.name%in%c("NA",NA)) %>% 
  filter(relief.annotated %in% c("NA",NA))

# Find samples where image is exported but missing annotations
relief.missing.annotation <- qaqc.relief %>%
  filter(!image.name %in% c("NA",NA))%>%
  filter(relief.annotated %in% c("NA",NA))

# Find samples annotated but missing images
relief.missing.image <- qaqc.relief %>%
  filter(image.name %in% c("NA",NA)) %>%
  filter(!relief.annotated%in%c("NA",NA))

# Find samples missing points, or with extra points annotated
relief.wrong.points <- num.annotations.relief %>%
  filter(relief.annotated!= 20)                                                 # Change here for your number of points (20 for single images, 80 for composite imagery)

# Joins metadata with the list of habitat images and the number of relief and habitat annotations
qaqc.all <- metadata %>%
  select(opcode, date) %>%                                                      # Select only these columns
  left_join(image.list ) %>%                                                    # Joins sample and date from metadata with the list of images
  left_join(num.annotations.habitat) %>%                                        # The number of annotation points per habitat image
  left_join(num.annotations.relief) %>%                                         # And the number of annotation points per relief image
  glimpse()                                                                     # Preview the data

# Find samples that have been annotated for habitat but not for relief 
habitat.no.relief <- qaqc.all %>%
  filter(!points.annotated%in%c("NA",NA)) %>%
  filter(relief.annotated%in%c("NA",NA))

# Find samples that have been annotated for relief but not for habitat
relief.no.habitat <- qaqc.all %>%
  filter(points.annotated%in%c("NA",NA))%>%
  filter(!relief.annotated%in%c("NA",NA))

# Export errors to check and fix in TransectMeasure
write.csv(habitat.missing.image.not.annotated,
          paste(error.dir, paste(study,"habitat.missing.image.not.annotated.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

write.csv(habitat.missing.annotation,
          paste(error.dir, paste(study,"habitat.missing.annotation.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

write.csv(habitat.missing.image,
          paste(error.dir, paste(study,"habitat.missing.image.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

write.csv(habitat.wrong.points,
          paste(error.dir, paste(study,"habitat.wrong.points.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

write.csv(habitat.no.relief,
          paste(error.dir, paste(study,"habitat.no.relief.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

# Export errors in the relief annotation data
write.csv(relief.missing.image.not.annotated,
          paste(error.dir, paste(study,"relief.missing.image.not.annotated.csv", sep = "."), 
                sep = "/"),row.names = FALSE)

write.csv(relief.missing.annotation,
          paste(error.dir, paste(study,"relief.missing.annotation.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

write.csv(relief.missing.image,
          paste(error.dir, paste(study,"relief.missing.image.csv", sep = "."), 
                sep = "/"),row.names = FALSE)

write.csv(relief.wrong.points,
          paste(error.dir, paste(study,"relief.wrong.points.csv", sep = "."), 
                sep = "/"),row.names = FALSE)

write.csv(relief.no.habitat,
          paste(error.dir, paste(study,"relief.no.habitat.csv", sep = "."), 
                sep = "/"),row.names = FALSE) 

###   STOP     AND    READ      THE     NEXT      PART     ###     

# We strongly encourage you to fix these errors at the source (i.e. TMObs)
# Now check through the files in your "Errors to check" folder and make corrections to .TMObs / generic files and then re-run this script

# In this example dataset we have no errors

### 3. Tidy data into broad and detailed point-level and percent cover dataframes ----
# Join the habitat and relief annotations
habitat <- bind_rows(points, relief)                                            # Stack the habitat and relief data 

# Create broad point annotations
broad.points <- habitat %>%
  select(-c(morphology, type, relief)) %>%                                      # Remove these columns - turn off this line if using broad only schema, add in 'fine' if using the fine schema 
  filter(!broad %in% c("",NA,"Unknown","Open.Water","Open Water")) %>%          # Remove blank, NA, Unknown and Open water data
  mutate(broad = paste("broad",broad,sep = ".")) %>%                            # Add broad. to all entries
  mutate(count = 1) %>%                                                         # Add a count column to summarise the number of points
  group_by(opcode) %>%
  spread(key = broad,value = count,fill = 0) %>%                                # Spread the data to wide format
  select(-c(image.row,image.col)) %>%                                           # Remove image row and image col
  group_by(opcode) %>%
  summarise_all(list(sum)) %>%                                                  # Add the points per sample across all broad habitat columns
  mutate(total.points.annotated = rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>%   # Take row sums of all data columns
  ga.clean.names() %>%                                                          # Clean names using GlobalArchive function
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

# Create broad percent cover
broad.percent.cover <- broad.points %>%
  group_by(opcode) %>%
  mutate_at(vars(starts_with("broad")),list(~./total.points.annotated*100)) %>% # Create percent cover
  dplyr::select(-c(total.points.annotated)) %>%                                 # Remove this column
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

# Create  detailed point annotations - turn off this section if using the broad only schema
detailed.points <- habitat %>%
  select(-c(relief)) %>%                                                        # Remove this column
  filter(!morphology %in% c("",NA,"Unknown")) %>%                               # Remove blank, NA and Unknown entries from morphology
  filter(!broad%in%c("",NA,"Unknown","Open Water","Open.Water")) %>%            # Remove blank, NA, Unknown and Open water entries from broad
  mutate(morphology = paste("detailed",broad,morphology,type,sep = ".")) %>%    # Paste broad morphology and type and add detailed. - add 'fine' if using fine schema
  mutate(morphology = str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm."))) %>% # Tidy some entries in morphology
  select(-c(broad, type)) %>%                                                   # Remove these columns - add 'fine' if using the fine schema
  mutate(count = 1) %>%                                                         # Add a count column to summarise the number of points
  group_by(opcode) %>%
  spread(key = morphology,value = count,fill = 0) %>%                           # Spread the data to wide format
  select(-c(image.row,image.col)) %>%                                           # Remove these columns
  group_by(opcode) %>%
  summarise_all(list(sum)) %>%                                                  # Add the points per sample across all detailed habitat columns
  mutate(total.points.annotated = rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>%   # Get row sums of all data columns
  ga.clean.names() %>%                                                          # Clean names using GlobalArchive function
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

# Create detailed percent cover - turn off this section if using the broad only schema
detailed.percent.cover <- detailed.points %>%
  group_by(opcode) %>%
  mutate_at(vars(starts_with("detailed")),list(~./total.points.annotated*100)) %>% # Create percent cover
  select(-c(total.points.annotated)) %>%                                        # Remove this column
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

# Create relief
relief.grid <- habitat %>%
  filter(!broad %in% c("Open Water","Unknown")) %>%                             # Remove Open water and Unknown entries from broad
  filter(!relief %in% c("",NA)) %>%                                             # Remove blank and NA entries from relief
  mutate(relief.rank = ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0, # Create numerical relief ranks
                       ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                       ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                       ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,
                       ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                       ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  select(-c(relief))%>%                                                         # Remove the original relief scores
  mutate(relief.rank = as.numeric(relief.rank)) %>%                             # Mutate the relief ranks as a numerical columns
  group_by(opcode) %>%
  summarise(mean.relief = mean (relief.rank), sd.relief = sd (relief.rank)) %>% # Create mean and standard deviation relief
  ungroup() %>%                                                                 # Ungroup
  glimpse()                                                                     # Preview the data

### 4. Tidy the final data into organised dataframes ----

habitat.broad.points <- metadata %>%
  left_join(broad.points, by = "opcode") %>% # Join metadata with habitat data
  left_join(relief.grid) # And relief

habitat.detailed.points <- metadata %>%
  left_join(detailed.points, by = "opcode") %>%                                 # Join metadata with habitat data
  left_join(relief.grid)                                                        # And relief

habitat.broad.percent <- metadata %>%
  left_join(broad.percent.cover, by = "opcode") %>%                             # Join metadata with habitat data
  left_join(relief.grid)                                                        # And relief

habitat.detailed.percent <- metadata %>%
  left_join(detailed.percent.cover, by = "opcode") %>%                          # Join metadata with habitat data
  left_join(relief.grid)                                                        # And relief

### 5. Inspect for tidy data for any errors ----
# Typical errors found could include samples where the wrong class has been assigned (ie. 20 point of octocoral instead of 20 points of sand)
# Or high cover of uncommon or rare classes

# Transform the data in a format suitable for use in ggplot 
# Broad habitat
broad.hab.plot <- habitat.broad.points %>%
  pivot_longer(cols = starts_with("broad"), 
               names_to = "biota", values_to = "num.points")                    # Pivots dataframes into long format to plot

# Detailed habitat
detailed.hab.plot <- habitat.detailed.points %>%
  pivot_longer(cols = starts_with("detailed"),
               names_to = "biota", values_to = "num.points")                    # Pivots dataframes into long format to plot

# Relief
broad.rel.plot <- relief %>%
  mutate(relief.rank = ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0, # Transforms raw relief classes into relief ranks
                       ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                       ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                       ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,
                       ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                       ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief))))))) %>%
  dplyr::filter(!relief.rank %in% "") %>%                                       # Removes blank annotations (e.g. 'Open water')
  dplyr::mutate(num.points = 1) %>%                                             # Adds a count to summarise the relief at each rank and sample
  dplyr::group_by(opcode, relief.rank) %>%
  dplyr::summarise(num.points = sum(num.points)) %>%                            # Sums the relief scores by sample and relief rank
  glimpse()                                                                     # Preview the data

# Plot and visualise the broad habitat dataset
gg.broad.hab <- ggplot() +
  geom_quasirandom(data = broad.hab.plot,                                       # Create a dotplot - each point represents a sample
                   aes(x = num.points, y = biota), groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()
gg.broad.hab

# Plot and visualise the detailed habitat dataset
gg.detailed.hab <- ggplot() +
  geom_quasirandom(data = detailed.hab.plot,                                    # Create a dotplot - each point represents a sample
                   aes(x = num.points, y = biota), 
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.5, size = 1.8) +
  labs(x = "Number of points", y = "") +
  theme_classic()
gg.detailed.hab

# Plot and visualise the relief dataset
gg.relief <- ggplot() +
  geom_quasirandom(data = broad.rel.plot,                                       # Create a dotplot - each point represents a sample
                   aes(x = num.points, y = relief.rank), 
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "Relief (0-5)") + 
  theme_classic()
gg.relief

# Save the plots to refer to later
ggsave(paste(plot.dir, paste(study, "broad.habitat.png", sep = "."), sep = "/"),
       gg.broad.hab,dpi = 600,width = 6.0, height = 3.0)

ggsave(paste(plot.dir, paste(study, "detailed.habitat.png", sep = "."), sep ="/"),
       gg.detailed.hab,dpi = 600,width=8.0, height = 6.0)

ggsave(paste(plot.dir, paste(study, "relief.png", sep = "."), sep = "/"),
       gg.relief,dpi = 600,width = 6.0, height = 3.0)

### 6. Export tidy datasets to a .csv format suitable for use in modelling and statistical testing ----
# Export point annotations
write.csv(habitat.broad.points,file = 
            paste(tidy.dir, paste(study,"random-points_broad.habitat.csv",sep = "_"), 
                  sep = "/"), row.names = FALSE)
write.csv(habitat.detailed.points,file = 
            paste(tidy.dir,paste(study,"random-points_detailed.habitat.csv", sep = "_"), 
                  sep = "/"), row.names=FALSE)

# Export percent cover annotations
write.csv(habitat.broad.percent,file = 
            paste(tidy.dir, paste(study,"random-points_percent-cover_broad.habitat.csv",sep = "_"), 
                  sep = "/"), row.names = FALSE)
write.csv(habitat.detailed.percent,file = 
            paste(tidy.dir,paste(study,"random-points_percent-cover_detailed.habitat.csv",sep = "_"), 
                  sep = "/"), row.names = FALSE)

### 7. Spatially visualise the data ----

# This plot uses spatial pie charts to visualise the proportion of habitat classes in each sample
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Create a color palette to plot the scatterpies with using the 'RColorbrewer' palettes
cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(habitat.broad.points[grep("broad", names(habitat.broad.points))])) # Expand the palette to the length of your unique habitat classes

# Create the plot
pie.chart <- leaflet() %>%                                                      # Create a leaflet plot
  addTiles() %>%                                                                # Add the Open Street Map base layer
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%            # Add ESRI satellite imagery as a base layer
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), 
                   options = layersControlOptions(collapsed = FALSE)) %>%       # Add controls to switch between layers
  addMinicharts(habitat.broad.points$longitude, habitat.broad.points$latitude,  # Add a spatial minichart using spatial information from the metadata
                type = "pie",                                                   # Make it a spatial pie chart
                colorPalette = cols,                                            # Color using the RColorbrewer palette
                chartdata = habitat.broad.points[grep("broad", names(habitat.broad.points))], # Select only columns starting with 'broad'
                width = 20, transitionTime = 0)                                 # Set the size and transition time of the points
pie.chart                                                                       # Display the plot

# This plot uses spatial bubble plots to frequency of occurrence of each habitat class
# The plot can be scrolled through and zoomed, and has 2 choices of base layer imagery

# Change the class below for each habitat class
hab.name <- 'broad.unconsolidated'

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
      fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(opcode))        # Format the points and add labels for sample code
}
if (nrow(equalzero)) {                                                          # Add spatial bubble plots if the data is equal to zero
  bubble.plot <- bubble.plot %>%
    addCircleMarkers(data = equalzero, lat = ~ latitude, lng = ~ longitude,     # Add the bubble plots
      radius = 2,                                                               # Scale the points at a constant size 
      fillOpacity = 0.5, color = "white", stroke = FALSE, 
      label = ~ as.character(opcode))                                           # Format the points and add labels for sample code
}
bubble.plot                                                                     # Display the plot 
