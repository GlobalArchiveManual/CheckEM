#' Add in zeros for deployments not present in the length data
#' 
#' @param length_data 
#' @param metadata 
#'
#' @return A dataframe with a row for every species in the length data, in every sample in the metadata
#'
#' @import dplyr
#' @import tidyr
#' @export
complete_length <- function(length_data, metadata) {
  
  # Filter metadata for successful lengths
  length_metadata <- metadata %>%
    dplyr::filter(successful_length == TRUE)
  
  # Complete length data
  length_with_zeros <- length_data %>%
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
  
  return(length_with_zeros)
}
