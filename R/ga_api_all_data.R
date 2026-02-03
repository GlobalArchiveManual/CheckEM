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
#' @param include_zeros Either TRUE or FALSE, if you would like the data to include zeros where a species is not observed, warning: this can create large datasets that use a lot of RAM
#' @param file_prefix Optional character string. If supplied, it is used a prefix to output filenames
#' (e.g. "prefix_metadata.RDS"). Default NULL.
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
  # container for all outputs
  out <- list()
  # -------------------------
  # METADATA
  # -------------------------
  metadata <- ga_api_metadata(
    token = token,
    synthesis_id = synthesis_id
  )
  
  if(is.null(metadata)) {
    stop("Unable to return data.")
  } else if (nrow(metadata) > 0) {
    
    out$metadata <- metadata
    
    # -------------------------
    # COUNT DATA
    # -------------------------
    count <- ga_api_count(
      token = token,
      synthesis_id = synthesis_id) |>
      dplyr::semi_join(metadata, by = "sample_url") |>
      dplyr::select(sample_url, family, genus, species, count) 
    
    if (nrow(count) > 0)  {out$count <- count}
    
    # -------------------------
    # LENGTH DATA
    # -------------------------
    length <- ga_api_length(
      token = token,
      synthesis_id = synthesis_id) |>
      dplyr::semi_join(metadata, by = "sample_url") |>
      dplyr::select(sample_url, family, genus, species, length_mm, count) |>
      dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) 
    
    if (nrow(length) > 0)  {out$length <- length}
    
    
    ## CREATE LIST OF SAMPLES ----
    samples <- metadata |>
      dplyr::select(sample_url, campaignid, sample)
    
    # -------------------------
    # BENTHIC DATA
    # -------------------------
    # benthos_raw <- ga_api_benthic_list(
    #   token = token) |>
    #   dplyr::semi_join(metadata, by = "sample_url")
    
    benthos_raw <- ga_api_habitat(
      token = token,
      synthesis_id = synthesis_id) |>
      dplyr::semi_join(metadata, by = "sample_url")
    
    if (nrow(benthos_raw   > 0)) {
      benthos_summarised <- benthos_raw |>
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
                                          .default = level_2)) |> 
        dplyr::left_join(samples) |>
        dplyr::select(sample_url, campaignid, sample, habitat, count) |>
        dplyr::group_by(sample_url, campaignid, sample, habitat) |> 
        dplyr::tally(count, name = "count") |> 
        dplyr::mutate(total_points_annotated = sum(count)) |> 
        dplyr::ungroup() |> 
        tidyr::pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) |>
        dplyr::select(-c(any_of("Fishes"))) |>
        clean_names() |>
        mutate(across(
          # .cols = 5:ncol(.), 
          .cols = 5:last_col(), 
          .fns = ~ .x / total_points_annotated,
          .names = "{.col}_percent"
        ))
      
      # assign("benthos_summarised", benthos_summarised, envir = .GlobalEnv)
      out$benthos_raw <- benthos_raw
      out$benthos_summarised <- benthos_summarised
    }
    
    # -------------------------
    # RELIEF DATA
    # -------------------------
    relief_raw <- ga_api_relief(
      token = token,
      synthesis_id = synthesis_id) |>
      dplyr::semi_join(metadata, by = "sample_url")
    
    if (nrow(relief_raw   > 0)) {
      relief_summarised <- relief_raw |>
        uncount(count) |>
        group_by(sample_url) |>
        dplyr::summarise(mean_relief = mean(as.numeric(level_5)), sd_relief = stats::sd(as.numeric(level_5), na.rm = T)) |>
        ungroup() |>
        dplyr::left_join(samples) |>
        dplyr::select(sample_url, campaignid, sample, everything())
      
      # assign("relief_summarised", relief_summarised, envir = .GlobalEnv)
      out$relief_raw <- relief_raw
      out$relief_summarised <- relief_summarised
    }

    # -------------------------
    # ZERO-FILLING (OPTIONAL)
    # -------------------------
    if (isTRUE(include_zeros)) {
      
      # Filter metadata for successful counts
      count_metadata <- metadata |>
        dplyr::filter(successful_count == TRUE)
      
      # Process count data into a wide format
      count_with_zeros <- count |>
        dplyr::full_join(count_metadata, by = "sample_url") |>
        dplyr::filter(successful_count == TRUE) |>
        dplyr::select(campaignid, sample, family, genus, species, count) |>
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) |>
        tidyr::replace_na(list(count = 0)) |>
        dplyr::group_by(campaignid, sample, family, genus, species) |>
        dplyr::summarise(count = sum(count)) |>
        dplyr::ungroup() |>
        dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) |>
        dplyr::select(campaignid, sample, scientific_name, family, genus, species, count) |>
        dplyr::full_join(count_metadata) |>
        dplyr::filter(successful_count == TRUE) 
      
      if (nrow(count_with_zeros) > 0)  {out$count_with_zeros <- count_with_zeros}
      
      # Filter metadata for successful lengths
      length_metadata <- metadata |>
        dplyr::filter(successful_length == TRUE)
      
      # Complete length data
      length_with_zeros <- length |>
        dplyr::mutate(count = as.numeric(count)) |>
        dplyr::filter(!is.na(count)) |>
        tidyr::uncount(count) |>
        dplyr::mutate(count = 1) |>
        dplyr::full_join(length_metadata, by = "sample_url") |>
        dplyr::filter(successful_length == TRUE) |>
        dplyr::select(campaignid, sample, family, genus, species, length_mm, count) |>
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) |>
        tidyr::replace_na(list(count = 0)) |>
        dplyr::mutate(length_mm = as.numeric(length_mm)) |>
        dplyr::full_join(length_metadata) |>
        dplyr::filter(!is.na(count)) |>
        dplyr::filter(successful_length == TRUE) |>
        dplyr::glimpse()
      
      if (nrow(length_with_zeros) > 0)  {out$length_with_zeros <- length_with_zeros}
      
      # Save additional processed data if complete_length was processed
      saveRDS(count_with_zeros, file = file.path(dir, "_count-with-zeros.RDS"))
      saveRDS(length_with_zeros, file = file.path(dir, "_length-with-zeros.RDS"))
    }
    
    # Save processed data as RDS files
    save_with_prefix <- function(data, name, dir, prefix = NULL) {
      if (nrow(data) > 0) {
        filename <- if (!is.null(prefix)) paste0(prefix, "_", name, ".RDS") else paste0(name, ".RDS")
        saveRDS(data, file = file.path(dir, filename))
      }
    }
    
    if (nrow(metadata) > 0)  {save_with_prefix(metadata, "metadata", dir, file_prefix)}
    if (nrow(count) > 0)  {save_with_prefix(count, "count", dir, file_prefix)}
    if (nrow(length) > 0)  {save_with_prefix(length, "length", dir, file_prefix)}
    if (nrow(benthos_raw) > 0)  { save_with_prefix(benthos_raw, "benthos_raw", dir, file_prefix)}
    if (nrow(benthos_summarised) > 0)  {save_with_prefix(benthos_summarised, "benthos_summarised", dir, file_prefix)}
    if (nrow(relief_raw) > 0)  {save_with_prefix(relief_raw, "relief_raw", dir, file_prefix)}
    if (nrow(relief_summarised) > 0)  {save_with_prefix(relief_summarised, "relief_summarised", dir, file_prefix)}
    
    # -------------------------
    # RETURN EVERYTHING
    # -------------------------
    out
  }
}
