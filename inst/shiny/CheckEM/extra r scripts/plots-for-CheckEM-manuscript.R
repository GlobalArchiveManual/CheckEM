# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)
# Load necessary library
library(wesanderson)


# Specify the path to the folder with CSV files (change to your GitHub local repo or file paths)
folder_path <- "inst/shiny/CheckEM/data/examples/Exmouth-Manuscript/CheckEM_exports/"

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read each CSV file, keeping the file name as a new column
data_list <- lapply(csv_files, function(file) {
  df <- read.csv(file)
  df$file_name <- basename(file)  # Add the file name as a new column
  return(df)
})

# Combine all data frames into one
combined_data <- bind_rows(data_list)

# Summarize the number of rows for each "error" type, grouped by file
summary_data <- combined_data %>%
  dplyr::mutate(error_recoded = case_when(
    error %in% c("period.with.no.end",
                 "period.wrong.length",
                 "point.outside.period",
                 "sample.without.period",
                 "length.or.3D.point.outside.period") ~ "Period",
    
    error %in% c("3D.point.measurement.over.rms",
                 "length.measurement.over.rms") ~ "Measurements over RMS",
    
    error %in% c("sample.in.lengths.without.metadata",
                 "sample.in.points.without.metadata") ~ "Missing Metadata",
    
    error %in% c("sample.without.length", 
                 "sample.without.points") ~ "Samples missing Fish",
    
    error %in% c("species.not.in.life.history.sheet",
                 "species.not.observed.in.region.before") ~ "Species identification",
    
    error %in% c("3D.point.without.a.number",
                 "point.without.a.number") ~ "Annotations missing number",
    
    error %in% c("stereo.maxn.does.not.equal.maxn") ~ "Measurements unequal to MaxN",
    
    error %in% c("too big", 
                 "too small") ~ "Species length outside known bounds"
    
    
  )) %>%
  
  
  group_by(file_name, error_recoded) %>%
  summarise(count = n(), .groups = 'drop') %>%
  dplyr::filter(!error_recoded %in% c(NA, "")) %>%
  separate(file_name, into = c("date", "extra"), sep = "_all.") %>%
  dplyr::mutate(date = str_replace_all(date, c("_" = " ", "00" = ":00")))

unique(summary_data$error_recoded)

# Generate a palette with 18 colors
palette_8_viridis <- viridis(8)

# Generate a palette with 18 colors (use the 'FantasticFox1' palette and extend it)
palette_8_wes <- wes_palette("FantasticFox1", 8, type = "continuous")

# Display the palette
print(palette_8_wes)

total_data <- summary_data %>%
  group_by(date) %>%
  summarise(total_count = sum(count), .groups = 'drop')

# Set2-8 color palette
palette_set2_8 <- c(
  "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", 
  "#a6d854", "#ff7f00", "#e5c494", "#b3b3b3"
)

# Define the color palette
custom_palette <- c(
  "#BE4174", "#DA86A5", "#F5C19E", "#F2AB60", 
  "#7BC6B4", "#4E9B9E", "#2A4F8C", "#79A0D0"
)

# Create a stacked bar plot
ggplot(summary_data, aes(x = date, y = count, fill = error_recoded)) +
  geom_bar(stat = "identity") +
  labs(
    # title = "Count of Error Types by File",
    x = "Iteration",
    y = "Count of Errors",
    fill = "Type of error"
  ) +
  # scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = custom_palette) +  # Use the Set2-8 color palette
  theme_classic() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Make x-axis labels readable
    axis.text.y = element_text(size = 12),  # Adjust y-axis label size
    axis.title.x = element_text(size = 14),  # Increase x-axis title size
    axis.title.y = element_text(size = 14),  # Increase y-axis title size
    plot.title = element_text(size = 16, hjust = 0.5),  # Title font size and centering
    legend.title = element_text(size = 12),  # Legend title size
    legend.text = element_text(size = 10)  # Legend text size
  )

# Create a bar plot
ggplot(summary_data, aes(x = date, y = count, fill = error_recoded)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +  # Set the bar width to 0.8
  labs(
    # title = "Count of Error Types by File",
    x = "Iteration",
    y = "Count of Errors",
    fill = "Type of error"
  ) +
  scale_fill_manual(values = custom_palette) +  # Use the viridis color palette
  theme_classic() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Make x-axis labels readable
    axis.text.y = element_text(size = 12),  # Adjust y-axis label size
    axis.title.x = element_text(size = 14),  # Increase x-axis title size
    axis.title.y = element_text(size = 14),  # Increase y-axis title size
    plot.title = element_text(size = 16, hjust = 0.5),  # Title font size and centering
    legend.title = element_text(size = 12),  # Legend title size
    legend.text = element_text(size = 10)  # Legend text size
  )
