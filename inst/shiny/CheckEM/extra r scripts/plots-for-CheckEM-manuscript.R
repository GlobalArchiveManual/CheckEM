# Load necessary libraries
library(tidyverse)

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
  group_by(file_name, error) %>%
  summarise(count = n(), .groups = 'drop')

# Create a bar plot
ggplot(summary_data, aes(x = file_name, y = count, fill = error)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Count of Error Types by File",
    x = "File Name",
    y = "Count of Errors",
    fill = "Error Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
