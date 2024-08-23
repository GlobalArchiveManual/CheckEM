#' Expand Life-History Data Frame for Species and Regions
#'
#' This function takes a life-history data frame where multiple marine regions for each species 
#' are listed in a single cell, separated by commas. It expands this data frame so that each
#' species-region combination gets its own row, with marine regions translated into their full names.
#'
#' @param dat A data frame containing life history information with a column for marine regions.
#' The `marine_region` column should contain comma-separated region codes (e.g., "NW, N, CS").
#' 
#' @return A data frame where each species-region combination is represented in a separate row.
#' The `marine_region` codes are expanded to their full names (e.g., "NW" to "North-west").
#' 
#' @export
#' @import dplyr tidyr
#'
#' @examples
#' # Create a sample data frame with species and comma-separated marine regions
#' dat <- data.frame(
#'   species = c("Species A", "Species B"),
#'   marine_region = c("NW, N, CS", "TE, SE")
#' )
#'
#' # Expand the data frame to have one row per species-region combination
#' expanded_dat <- expand_life_history(dat)
#'
#' # Check the result
#' print(expanded_dat)
#'
expand_life_history <- function(dat) {
  
  require(dplyr)
  require(tidyr)
  
  dat %>%
    dplyr::mutate(marine_region = strsplit(as.character(marine_region), split = ", ")) %>% # Split comma-separated regions into lists
    tidyr::unnest(marine_region) %>% # Expand lists into separate rows
    dplyr::mutate(marine_region = case_when(marine_region == "NW" ~ "North-west",
                                            marine_region == "N"  ~ "North",
                                            marine_region == "CS" ~ "Coral Sea",
                                            marine_region == "TE"  ~ "Temperate East",
                                            marine_region == "SE" ~ "South-east",
                                            marine_region == "SW"  ~ "South-west"
    )) # Replace region codes with full names
}





