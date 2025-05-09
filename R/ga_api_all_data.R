#' Retrieve all Data from a synthesis using the GlobalArchive API
#'
#' This function retrieves metadata, count, and length data from a CheckEM synthesis, processes the data,
#' and saves the results as RDS files in the specified directory with the given name. It also returns the
#' processed data in the environment and allows users to add in zeros where a species is not present
#' If the directory doesn't exist, it will be created, and the user will be informed.
#' 
#' @param token A character string representing your GlobalArchive token for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID 
#' for which the data should be retrieved.
#' @param dir A character string specifying the directory where the RDS files will be saved.
#' @param include_zeros Either TRUE or FALSE, if you would like the data to include zeros where a species is not observed, warning: this can create large datasets that use a lot og RAM
#'
#' @return A list containing `metadata`, `count`, and `length` data frames.
#'
#' @import dplyr
#' @import tidyr
#' @import arrow
#' @export
ga_api_all_data <- function(token, synthesis_id, dir, include_zeros = FALSE) {
  
  # Check if the directory exists, if not create it
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Directory ", dir, " does not exist. Creating directory...")
  }
  
  # Retrieve metadata from the CheckEM API
  metadata <- ga_api_metadata(synthesis_id = synthesis_id, token = token)
  
  # Retrieve and process count data
  count <- ga_api_count(synthesis_id = synthesis_id, token = token) %>%
    dplyr::semi_join(metadata, by = "sample_url") %>%
    dplyr::select(sample_url, family, genus, species, count) 
    
  # Retrieve and process length data
  length <- ga_api_length(synthesis_id = synthesis_id, token = token) %>%
    dplyr::semi_join(metadata, by = "sample_url") %>%
    dplyr::select(sample_url, family, genus, species, length_mm, count) %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) 
  
  # # Retrieve and process habitat data
  # habitat <- ga_api_habitat(synthesis_id = synthesis_id, token = token) %>%
  #   dplyr::semi_join(metadata, by = "sample_url") #%>%
  #   # dplyr::select(sample_url, family, genus, species, length_mm, number) %>%
  #   # dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) 
  
  assign("metadata", metadata, envir = .GlobalEnv)
  assign("count", count, envir = .GlobalEnv)
  assign("length", length, envir = .GlobalEnv)
  # assign("habitat", habitat, envir = .GlobalEnv)
  
  # Save processed data as RDS files
  saveRDS(metadata, file = file.path(dir, "metadata.RDS"))
  saveRDS(count, file = file.path(dir, "count.RDS"))
  saveRDS(length, file = file.path(dir, "length.RDS"))
  # saveRDS(habitat, file = file.path(dir, "habitat.RDS"))
  
  # Only complete the data is include_zeros == TRUE
  if (include_zeros) {
  
  # Filter metadata for successful counts
  count_metadata <- metadata %>%
    dplyr::filter(successful_count == TRUE)
  
  # Process count data into a wide format
  count_with_zeros <- count %>%
    dplyr::full_join(count_metadata, by = "sample_url") %>%
    dplyr::filter(successful_count == TRUE) %>%
    dplyr::select(campaignid, sample, family, genus, species, count) %>%
    tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
    tidyr::replace_na(list(count = 0)) %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
    dplyr::select(campaignid, sample, scientific_name, count) %>%
    dplyr::full_join(count_metadata, by = "sample_url") %>%
    dplyr::filter(successful_count == TRUE) 
  
  # Filter metadata for successful lengths
  length_metadata <- metadata %>%
    dplyr::filter(successful_length == TRUE)
  
  # Complete length data
  length_with_zeros <<- length %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::filter(!is.na(count)) %>%
    tidyr::uncount(count) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::full_join(length_metadata, by = "sample_url") %>%
    dplyr::filter(successful_length == TRUE) %>%
    dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
    tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
    tidyr::replace_na(list(count = 0)) %>%
    dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
    dplyr::full_join(length_metadata, by = "sample_url") %>%
    dplyr::filter(!is.na(count)) %>%
    dplyr::filter(successful_length == TRUE) %>%
    dplyr::glimpse()
  
  # Save additional processed data if complete_length was processed
  saveRDS(count_with_zeros, file = file.path(dir, "_count-with-zeros.RDS"))
  saveRDS(length_with_zeros, file = file.path(dir, "_length-with-zeros.RDS"))
  
  assign("count_with_zeros", count_with_zeros, envir = .GlobalEnv)
  assign("length_with_zeros", length_with_zeros, envir = .GlobalEnv)
  
  }
}
