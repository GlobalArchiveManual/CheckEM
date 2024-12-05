# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)
# Load necessary library
# library(wesanderson)
library(patchwork)


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

total_data <- summary_data %>%
  group_by(date) %>%
  summarise(total_count = sum(count), .groups = 'drop')

# Define the color palette
custom_palette <- c(
  "#BE4174", "#DA86A5", "#F5C19E", "#F2AB60", 
  "#7BC6B4", "#4E9B9E", "#79A0D0", "#2A4F8C", "#363635"
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



# Summarize the number of rows for each "error" type, grouped by file
detailed_data <- combined_data %>%
  dplyr::mutate(type_error = case_when(
    error %in% c("period.with.no.end",
                 "period.wrong.length",
                 "point.outside.period",
                 "sample.without.period",
                 "length.or.3D.point.outside.period") ~ "Metadata",
    
    error %in% c("3D.point.measurement.over.rms",
                 "length.measurement.over.rms") ~ "Measurements",
    
    error %in% c("sample.in.lengths.without.metadata",
                 "sample.in.points.without.metadata") ~ "Metadata",
    
    error %in% c("sample.without.length", 
                 "sample.without.points") ~ "Metadata",
    
    error %in% c("species.not.in.life.history.sheet",
                 "species.not.observed.in.region.before") ~ "Annotations",
    
    error %in% c("3D.point.without.a.number",
                 "point.without.a.number") ~ "Annotations",
    
    error %in% c("stereo.maxn.does.not.equal.maxn") ~ "Measurements",
    
    error %in% c("too big", 
                 "too small") ~ "Measurements"
    
    
  )) %>%
  
  
  group_by(file_name, error, type_error) %>%
  summarise(count = n(), .groups = 'drop') %>%
  dplyr::filter(!type_error %in% c(NA, "")) %>%
  separate(file_name, into = c("date", "extra"), sep = "_all.") %>%
  dplyr::mutate(date = str_replace_all(date, c("_" = " ", "00" = ":00"))) %>%
  arrange(date) %>%  # Ensure dates are in ascending order
  mutate(date_sequence = as.numeric(factor(date))) %>%  # Create sequence 1:5
  mutate(
    error = str_replace_all(error, "\\.", " ") %>%  # Replace dots with spaces
      str_to_sentence()) %>%                              # Capitalise first character of each word
  mutate(error = str_replace_all(error, c("rms" = "RMS",
                               "3d" = "3D")))


# Subset palettes for each plot
palette_metadata <- custom_palette  # Use all 9 colors
palette_annotations <- custom_palette[c(1, 3, 5, 7)]
palette_measurements <- custom_palette[c(1, 3, 5, 7, 9)]

# Theme for publication
publication_theme <- theme_classic() +
  theme(
    text = element_text(family = "serif", size = 12),  # Use sans-serif font
    plot.title = element_text(family = "serif", size = 14, hjust = 0.5),
    axis.title = element_text(family = "serif", size = 13),
    axis.text = element_text(family = "serif", size = 12),
    legend.position = "right",
    legend.title = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 12)
  )

# Plot for Metadata
plot_metadata <- ggplot(
  detailed_data %>% filter(type_error == "Metadata"), 
  aes(x = date_sequence, y = count, fill = error)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette_metadata) +
  labs(
    title = "Metadata",
    y = "Number of Flagged Errors",
    fill = "Type of Error"
  ) +
  publication_theme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()#,
    # axis.ticks.x = element_blank()
  )

# Plot for Annotations
plot_annotations <- ggplot(
  detailed_data %>% filter(type_error == "Annotations"), 
  aes(x = date_sequence, y = count, fill = error)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette_annotations) +
  labs(
    title = "Annotations",
    y = "Number of Flagged Errors",
    fill = "Type of Error"
  ) +
  publication_theme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()#,
    # axis.ticks.x = element_blank()
  )

# Plot for Measurements
plot_measurements <- ggplot(
  detailed_data %>% filter(type_error == "Measurements"), 
  aes(x = date_sequence, y = count, fill = error)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette_measurements) +
  labs(
    title = "Measurements",
    x = "Iteration",
    y = "Number of Flagged Errors",
    fill = "Type of Error"
  ) +
  publication_theme

plot_measurements

# Combine plots with patchwork
combined_plot <- (plot_metadata / plot_annotations / plot_measurements)

combined_plot


ggsave("manuscript/plots/plot-error-iterations.png", 
       plot = combined_plot, device = "png", 
       width = 18,                       # Width of A4 page in inches
       height = 22, 
       units = "cm", dpi = 600)
