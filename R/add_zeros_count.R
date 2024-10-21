#' Add in zeros for deployments not present in the count data
#' 
#' @param count_data 
#' @param metadata 
#'
#' @return A dataframe with a row for every species in the count data, in every sample in the metadata
#'
#' @import dplyr
#' @import tidyr
#' @export
add_zeros_count <- function(count_data, metadata) {
  
  # Filter metadata for successful counts
  count_metadata <- metadata %>%
    dplyr::filter(successful_count == TRUE)
  
  # Process count data into a wide format
  count_with_zeros <- count_data %>%
    dplyr::select(sample_url, family, genus, species, count) %>%
    dplyr::full_join(count_metadata, by = "sample_url") %>%
    dplyr::filter(successful_count == TRUE) %>%
    dplyr::select(campaignid, sample, family, genus, species, count) %>%
    tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
    tidyr::replace_na(list(count = 0)) %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
    dplyr::select(campaignid, sample, scientific_name, family, genus, species, count) %>%
    dplyr::full_join(count_metadata) %>%
    dplyr::filter(successful_count == TRUE) 
  
  return(count_with_zeros)
}