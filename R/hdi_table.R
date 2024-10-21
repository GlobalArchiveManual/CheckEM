#' hdi_table
#'
#' Generates a summary table for species data with Highest Density Interval (HDI) statistics and comparisons 
#' against species-specific maximum lengths.
#'
#' This function calculates HDI statistics (99% interval) for each species' length data, compares these 
#' statistics with species-specific maximum lengths (provided separately), and creates a summary dataframe. 
#' The summary includes family, genus, species, scientific name, species length max, HDI bounds, number of 
#' individuals measured, and metrics like the number of lengths exceeding the max or HDI upper bound.
#'
#' @param species_data A dataframe with species data, including columns for `family`, `genus`, `species`, 
#' `scientific_name`, and `length_mm` (length measurements in millimeters).
#' @param max_lengths A dataframe with species-specific maximum lengths. It must include the column 
#' `scientific_name` to match the species and `length_max_mm` for the maximum length in millimeters.
#'
#' @return A dataframe summarizing the HDI statistics for each species, including:
#' \describe{
#'   \item{family}{Family of the species.}
#'   \item{genus}{Genus of the species.}
#'   \item{species}{Species name.}
#'   \item{scientific_name}{Scientific name of the species.}
#'   \item{species_length_max_mm}{Maximum length (in millimeters) from the provided species-specific data.}
#'   \item{hdci_99_lower_bound}{Lower bound of the 99% HDI for the species' length measurements.}
#'   \item{hdci_99_upper_bound}{Upper bound of the 99% HDI for the species' length measurements.}
#'   \item{num_measurements}{Number of individual length measurements (rows) for the species.}
#'   \item{highest_metric}{Indicates whether the maximum length (from FishBase) or the HDI upper bound is higher.}
#'   \item{num_lengths_over_max}{The count of lengths that exceed the species-specific maximum length.}
#'   \item{num_lengths_over_hdi_upper}{The count of lengths that exceed the 99% HDI upper bound.}
#'   \item{num_lengths_smaller_hdi_lower}{The count of lengths that are smaller than the 99% HDI lower bound.}
#' }
#'
#' @details
#' The function first loops through each species in the `species_data`, calculates the 99% HDI using the 
#' `ggdist::median_hdci()` function, and compares the species' maximum length (from `max_lengths`) with the 
#' HDI statistics. It then appends the results for each species into a final dataframe.
#'
#' @examples
#' # Example usage
#' species_data <- data.frame(family = c("Acanthuridae"), genus = c("Acanthurus"),
#'                            species = c("triostegus"), scientific_name = c("Acanthurus triostegus"),
#'                            length_mm = c(120, 130, 125, 140, 135, 290, 200, 260, 150, 190, 280))
#' max_lengths <- data.frame(scientific_name = c("Acanthurus triostegus"), length_max_mm = c(270))
#'
#' hdi_table(species_data, max_lengths)
#'
#' @import dplyr
#' @import ggdist
#' @export
hdi_table <- function(species_data, max_lengths) {
  
  # Initialize an empty dataframe to store results
  results <- data.frame(family = character(),
                        genus = character(),
                        species = character(),
                        scientific_name = character(),
                        species_length_max_mm = numeric(),
                        hdci_99_lower_bound = numeric(),
                        hdci_99_upper_bound = numeric(),
                        num_measurements = numeric(),
                        highest_metric = character(),
                        num_lengths_over_max = numeric(),
                        num_lengths_over_hdi_upper = numeric(),
                        num_lengths_smaller_hdi_lower = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through each species
  unique_species <- unique(species_data$scientific_name)
  
  for (species_name in unique_species) {
    
    # Filter data for the current species
    species_subset <- species_data[species_data$scientific_name == species_name, ]
    
    # Get species-specific max length
    family <- unique(species_subset$family)
    genus <- unique(species_subset$genus)
    species <- unique(species_subset$species)
    scientific_name <- unique(species_subset$scientific_name)
    
    species_length_max_mm <- max_lengths[max_lengths$scientific_name == species_name, "length_max_mm"]
    
    # Check if max length is found, if not assign NA
    if (length(species_length_max_mm) == 0) {
      species_length_max_mm <- NA
    }
    
    # Calculate HDI
    hdi <- ggdist::median_hdci(species_subset$length_mm, .width = 0.99) %>%
      dplyr::mutate(hdci_99 = ymax - ymin) %>%
      dplyr::rename(hdci_99_lower_bound = ymin, hdci_99_upper_bound = ymax)
    
    hdci_99_lower_bound <- unique(hdi$hdci_99_lower_bound)
    hdci_99_upper_bound <- unique(hdi$hdci_99_upper_bound)
    
    # Number of individuals
    num_measurements <- nrow(species_subset)
    
    # highest_metric: is species_length_max_mm greater than hdci_99_upper_bound
    highest_metric <- ifelse(species_length_max_mm > hdci_99_upper_bound, "FishBase", "HDI 99%")
    
    # Number of lengths greater than species_length_max_mm
    num_lengths_over_max <- sum(species_subset$length_mm > species_length_max_mm, na.rm = TRUE)
    
    # Number of lengths greater than hdci_99_upper_bound
    num_lengths_over_hdi_upper <- sum(species_subset$length_mm > hdci_99_upper_bound, na.rm = TRUE)
    
    # Number of lengths greater than hdci_99_upper_bound
    num_lengths_smaller_hdi_lower <- sum(species_subset$length_mm < hdci_99_lower_bound, na.rm = TRUE)
    
    # Append results to the dataframe
    results <- rbind(results, data.frame(
      family = family,
      genus = genus,
      species = species,
      scientific_name = scientific_name,
      species_length_max_mm = species_length_max_mm,
      hdci_99_lower_bound = hdci_99_lower_bound,
      hdci_99_upper_bound = hdci_99_upper_bound,
      num_measurements = num_measurements,
      highest_metric = highest_metric,
      num_lengths_over_max = num_lengths_over_max,
      num_lengths_over_hdi_upper = num_lengths_over_hdi_upper,
      num_lengths_smaller_hdi_lower = num_lengths_smaller_hdi_lower,
      stringsAsFactors = FALSE
    ))
  }
  
  # Return the results
  return(results)
}