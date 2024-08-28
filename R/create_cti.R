#' Create Community Temperature Index as per Day et al. 2018
#'
#' This function creates a new dataframe containing Community Temperature Index (CTI) for fish species from abundance data.
#' The function is designed to work with data extracted from GlobalArchive using the 'ga_api_count' function, and processed using provided scripts.
#' The input dataframe must contain columns for 'campaignid', 'sample', 'count', 'family', 'genus' and 'species'.
#' CTI is calculated 
#'
#' @param data A dataframe with columns for 'campaignid', 'sample', 'count', 'family', 'genus' and 'species'
#' @param life_history A dataframe containing thermal niche data for fish species. For an example dataframe see CheckEM::australia_life_history.
#'
#' @return A data frame containing campaignid, sample and cti (the value for Community Temperature Index) 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch metadata for a specific synthesis
#' count <- readRDS(paste0("data/geographe/raw/", name, "_complete_count.RDS")) %>%
#'   dplyr::select(campaignid, sample, family, genus, species, count) %>%
#'   dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
#'   glimpse()
#'   
#' cti <- create_cti(data = count) %>%
#'   dplyr::rename(number = cti) %>% # Rename the column to number to match with other dataframes for FSSgam modelling
#'   dplyr::mutate(response = "cti") %>%
#'   glimpse()
#'   
#' }
create_cti <- function(data, life_history = CheckEM::australia_life_history) {
  # Transform 'count' dataframe into new dataframe for Community Temperature Index
  # 'count' dataframe must have campaignid, sample, and count columns
  require(CheckEM)
  require(tidyverse)
  
  # TODO Brooke - needs to add in RLS thermal niche col to global life history
  
  master <-  life_history %>%
    clean_names() %>%
    dplyr::select(family, genus, species, rls_thermal_niche) %>%
    dplyr::distinct()
  
  data %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 0) %>%
    left_join(master) %>%
    uncount(count) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::filter(!is.na(rls_thermal_niche)) %>%
    dplyr::mutate(log_count = log10(count + 1),
                  weightedsti = log_count*rls_thermal_niche) %>%
    dplyr::group_by(campaignid, sample) %>%
    dplyr::summarise(log_count = sum(log_count, na.rm = T),
                     w_sti = sum(weightedsti, na.rm = T),
                     cti = w_sti/log_count) %>%
    dplyr::ungroup()
}