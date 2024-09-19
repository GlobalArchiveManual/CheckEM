#' Retrieve all Data from a synthesis using the GlobalArchive API
#'
#' This function retrieves metadata, count, and length data from a CheckEM synthesis, processes the data,
#' and saves the results as RDS files in the specified directory with the given name. It also returns the
#' processed data in the environment and allows users to add in zeros where a species is not present
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
#' @export
ga_api_all_data <- function(token, synthesis_id, dir, include_zeros = FALSE) {
  
  # Retrieve metadata from the CheckEM API
  metadata <- ga_api_metadata(synthesis_id = synthesis_id, token = token)
  
  # Retrieve and process count data
  count <- ga_api_count(synthesis_id = synthesis_id, token = token) %>%
    dplyr::select(sample_url, family, genus, species, count) %>%
    dplyr::semi_join(metadata, by = "sample_url")
  
  # Retrieve and process length data
  length <- ga_api_length(synthesis_id = synthesis_id, token = token) %>%
    dplyr::semi_join(metadata, by = "sample_url") %>%
    dplyr::select(sample_url, family, genus, species, length_mm, number) %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) 
  
  assign("metadata", metadata, envir = .GlobalEnv)
  assign("count", count, envir = .GlobalEnv)
  assign("length", length, envir = .GlobalEnv)
  
  # Save processed data as RDS files
  saveRDS(metadata, file = file.path(dir, "metadata.RDS"))
  saveRDS(count, file = file.path(dir, "count.RDS"))
  saveRDS(length, file = file.path(dir, "length.RDS"))
  
  # Only complete the data is include_zeros == TRUE
  if (include_zeros) {
  
  # Filter metadata for successful counts
  count_metadata <- metadata %>%
    dplyr::filter(successful_count == TRUE)
  
  # Process count data into a wide format
  count_wide <- count %>%
    dplyr::full_join(count_metadata, by = "sample_url") %>%
    dplyr::filter(successful_count == TRUE) %>%
    dplyr::select(campaignid, sample, family, genus, species, count) %>%
    tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
    tidyr::replace_na(list(count = 0)) %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
    dplyr::select(campaignid, sample, scientific_name, count) %>%
    tidyr::spread(scientific_name, count, fill = 0) 
  
  # Create a table of distinct families
  count_families <- count %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(family, genus, species, scientific_name)) %>%
    dplyr::distinct()
  
  # Complete count data
  count_with_zeros <<- count_wide %>%
    tidyr::pivot_longer(names_to = "scientific_name", values_to = "count", cols = 3:ncol(.)) %>%
    dplyr::inner_join(count_families, by = "scientific_name") %>%
    dplyr::full_join(count_metadata, by = "sample_url") %>%
    dplyr::filter(successful_count == TRUE)
  
  # Filter metadata for successful lengths
  length_metadata <- metadata %>%
    dplyr::filter(successful_length == TRUE)
  
  # Complete length data
  length_with_zeros <<- length %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::filter(!is.na(number)) %>%
    tidyr::uncount(number) %>%
    dplyr::mutate(number = 1) %>%
    dplyr::full_join(length_metadata, by = "sample_url") %>%
    dplyr::filter(successful_length == TRUE) %>%
    dplyr::select(campaignid, sample, family, genus, species, length_mm, number) %>%
    tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
    tidyr::replace_na(list(number = 0)) %>%
    dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
    dplyr::full_join(length_metadata, by = "sample_url") %>%
    dplyr::filter(!is.na(number)) %>%
    dplyr::filter(successful_length == TRUE) %>%
    dplyr::glimpse()
  
  # Save additional processed data if complete_length was processed
  saveRDS(count_with_zeros, file = file.path(dir, "_count-with-zeros.RDS"))
  saveRDS(length_with_zeros, file = file.path(dir, "_length-with-zeros.RDS"))
  
  assign("count_with_zeros", count_with_zeros, envir = .GlobalEnv)
  assign("length_with_zeros", length_with_zeros, envir = .GlobalEnv)
  
  }
}
