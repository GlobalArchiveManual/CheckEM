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
ga_api_all_data <- function(token, synthesis_id, dir, include_zeros = FALSE, file_prefix = NULL) {
  
  # Check if the directory exists, if not create it
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Directory ", dir, " does not exist. Creating directory...")
  }
  
  # Retrieve metadata from the CheckEM API
  metadata <- ga_api_metadata(synthesis_id = synthesis_id, token = token) # Error handling from metadata function
  
  if (nrow(metadata) > 0) {
    
    # Retrieve and process count data
    count <- ga_api_count(synthesis_id = synthesis_id, token = token) %>%
      dplyr::semi_join(metadata, by = "sample_url") %>%
      dplyr::select(sample_url, family, genus, species, count) 
    
    # Retrieve and process length data
    length <- ga_api_length(synthesis_id = synthesis_id, token = token) %>%
      dplyr::semi_join(metadata, by = "sample_url") %>%
      dplyr::select(sample_url, family, genus, species, length_mm, count) %>%
      dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) 
    
    samples <- metadata %>%
      dplyr::select(sample_url, campaignid, sample)
    
    # # Retrieve and process habitat data
    benthos <- ga_api_habitat(synthesis_id = synthesis_id, token = token) %>%
      dplyr::semi_join(metadata, by = "sample_url")
    
    relief <- ga_api_relief(synthesis_id = synthesis_id, token = token) %>%
      dplyr::semi_join(metadata, by = "sample_url")
    
    benthos_summarised <- benthos %>%
      dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2, 
                                        level_2 %in% "Seagrasses" ~ level_2, 
                                        level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ "Consolidated", 
                                        level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ "Unconsolidated",  
                                        level_2 %in% "Sponges" ~ "Sessile invertebrates", 
                                        level_2 %in% "Sessile invertebrates" ~ level_2, 
                                        level_2 %in% "Bryozoa" ~ "Sessile invertebrates", 
                                        level_2 %in% "Cnidaria" ~ "Sessile invertebrates",
                                        level_2 %in% "Echinoderms" ~ "Sessile invertebrates",
                                        level_2 %in% "Ascidians" ~ "Sessile invertebrates",
                                        .default = level_2)) %>% 
      dplyr::left_join(samples) %>%
      dplyr::select(sample_url, campaignid, sample, habitat, count) %>%
      dplyr::group_by(sample_url, campaignid, sample, habitat) %>% 
      dplyr::tally(count, name = "count") %>% 
      dplyr::mutate(total_points_annotated = sum(count)) %>% 
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) %>%
      dplyr::select(-c(any_of("Fishes"))) %>%
      clean_names() %>%
      mutate(across(
        .cols = 5:ncol(.), 
        .fns = ~ .x / total_points_annotated,
        .names = "{.col}_percent"
      ))
    
    relief_summarised <- relief %>%
      uncount(count) %>%
      group_by(sample_url) %>%
      dplyr::summarise(mean_relief = mean(as.numeric(level_5)), sd_relief = sd(as.numeric(level_5), na.rm = T)) %>%
      ungroup() %>%
      dplyr::left_join(samples) %>%
      dplyr::select(sample_url, campaignid, sample, everything())
    
    if (nrow(metadata) > 0)  {assign("metadata", metadata, envir = .GlobalEnv)}
    if (nrow(count     > 0)) {assign("count", count, envir = .GlobalEnv)}
    if (nrow(length    > 0)) {assign("length", length, envir = .GlobalEnv)}
    if (nrow(benthos   > 0)) {assign("benthos_raw", benthos, envir = .GlobalEnv)}
    if (nrow(relief    > 0)) {assign("relief_raw", relief, envir = .GlobalEnv)}
    if (nrow(benthos_summarised > 0)) {assign("benthos_summarised", benthos_summarised, envir = .GlobalEnv)}
    if (nrow(relief_summarised) > 0)  {assign("relief_summarised", relief_summarised, envir = .GlobalEnv)}
    
    # Save processed data as RDS files
    
    save_with_prefix <- function(data, name, dir, prefix = NULL) {
      if (nrow(data) > 0) {
        filename <- if (!is.null(prefix)) paste0(prefix, "_", name, ".RDS") else paste0(name, ".RDS")
        saveRDS(data, file = file.path(dir, filename))
      }
    }
    
    save_with_prefix(metadata, "metadata", dir, file_prefix)
    save_with_prefix(count, "count", dir, file_prefix)
    save_with_prefix(length, "length", dir, file_prefix)
    save_with_prefix(benthos, "benthos_raw", dir, file_prefix)
    save_with_prefix(benthos_summarised, "benthos_summarised", dir, file_prefix)
    save_with_prefix(relief, "relief_raw", dir, file_prefix)
    save_with_prefix(relief_summarised, "relief_summarised", dir, file_prefix)
    
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
}
