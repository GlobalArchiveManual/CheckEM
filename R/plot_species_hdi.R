#' Plot length frequency histogram with High Density Interval
#'
#' This function takes a data frame and standardises its column names by converting them to lower case, 
#' replacing special characters with underscores, and ensuring that names are syntactically valid R names.
#'
#' @param species_data A data frame with length information for one species
#' @param max_lengths A data frame with lmaximum length information
#' @param dir Directory where plots will be saved
#'
#' @return Saves plots into directory
#' 
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(ggdist)
#'
#' # Minimal example data for one species
#' species_data <- tibble::tibble(
#'   scientific_name = rep("Lutjanus sebae", 40),
#'   length_mm = c(
#'     rnorm(35, mean = 420, sd = 40),
#'     rnorm(5,  mean = 650, sd = 10)
#'   )
#' )
#'
#' # Maximum length lookup table
#' max_lengths <- tibble::tibble(
#'   scientific_name = "Lutjanus sebae",
#'   length_max_mm   = 1000
#' )
#'
#' # Temporary output directory
#' out_dir <- tempfile("hdi_plot_")
#' dir.create(out_dir)
#' out_dir <- paste0(out_dir, .Platform$file.sep)
#'
#' # Run the function
#' plot_species_hdi(
#'   species_data = species_data,
#'   max_lengths  = max_lengths,
#'   dir          = out_dir
#' )
#' }

plot_species_hdi <- function(species_data, max_lengths, dir) {
  
  # Get species-specific max length
  scientific_name <- unique(species_data$scientific_name)
  species_length_max_mm <- max_lengths[max_lengths$scientific_name == scientific_name, "length_max_mm"]
  
  # # adjust the upper bound for outliers by incorporating skewness (through the medcouple, MC), which improves upon the traditional IQR method by accounting for asymmetry in the distribution.
  # 
  # # IQR ----
  # # Calculate Q1, Q3, IQR, and Medcouple (MC)
  # Q1 <- quantile(species_data$length_mm, 0.25)
  # Q3 <- quantile(species_data$length_mm, 0.75)
  # IQR <- Q3 - Q1
  # MC <- mc(species_data$length_mm)  # Calculate medcouple
  # 
  # # Adjust the upper bound using the given formula
  # IQR_upper_bound <- Q3 + 1.5 * IQR * exp(3 * MC)
  # 
  # # Traditional lower bound or modified lower bound based on MC (optional)
  # IQR_lower_bound <- Q1 - 1.5 * IQR * exp(3 * (-MC))  # Modified lower bound, depends on skewness
  
  ## HDI ----
  hdi <- ggdist::median_hdci(species_data$length_mm, .width = 0.99) %>%
    dplyr::mutate(hdci_95 = ymax - ymin) %>%
    dplyr::rename(hdci_95_lower_bound = ymin, hdci_95_upper_bound = ymax)
  
  hdci_95_lower_bound <- unique(hdi$hdci_95_lower_bound)
  hdci_95_upper_bound <- unique(hdi$hdci_95_upper_bound)
  
  species_data <- species_data %>%
    mutate(is_outlier = length_mm < hdci_95_lower_bound | length_mm > hdci_95_upper_bound)
  
  # Number of individuals (rows in the dataset)
  num_measurements <- nrow(species_data)
  
  # Plot histogram with outliers and lines for IQR and max length
  p <- ggplot(species_data, aes(x = length_mm)) +
    geom_histogram(aes(fill = is_outlier), binwidth = 5, color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red"), guide = "none") +
    
    # geom_vline(xintercept = IQR_lower_bound, linetype = "dashed", color = "blue", size = 1) +
    # geom_vline(xintercept = IQR_upper_bound, linetype = "dashed", color = "blue", size = 1) +
    
    geom_vline(xintercept = hdci_95_lower_bound, linetype = "dashed", color = "purple", size = 1) +
    geom_vline(xintercept = hdci_95_upper_bound, linetype = "dashed", color = "purple", size = 1) +
    
    geom_vline(xintercept = species_length_max_mm, linetype = "solid", color = "green", size = 1) +
    labs(
      title = paste("Length Frequency for", scientific_name),
      x = "Body Size (mm)",
      y = "Frequency",
      subtitle = paste("HDI 99% bounds, Green line: Max length =", species_length_max_mm, "| Number of measurements =", num_measurements) # Blue dashed lines: IQR bounds, Purple: 
    ) 
  
  # Save the plot to a PNG file with species name
  file_name <- paste0(dir, "Length_Frequency_", gsub(" ", "_", scientific_name), ".png")
  ggsave(file_name, plot = p, width = 8, height = 6)
  
  print(p)  # Also print the plot to the console
}