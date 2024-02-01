#' A function to expand a life-history dataframe so there is one row per species per region
#'
#' @param dat 
#'
#' @return A life history dataframe, either australia_life_history or global_life_history
#' @export
#'
#' @examples
#' 
# dat <- as.data.frame(c("NAmE1", "name-2", "nAMe.-72")) %>%
#   clean_names()
#' 
#' 
expand_life_history <- function(dat) {
  
  dat %>%
    dplyr::mutate(marine_region = strsplit(as.character(marine_region), split = ", ")) %>% # changed from "/" for old LH
    tidyr::unnest(marine_region) %>%
    dplyr::mutate(marine_region = case_when(marine_region == "NW" ~ "North-west",
                                            marine_region == "N"  ~ "North",
                                            marine_region == "CS" ~ "Coral Sea",
                                            marine_region == "TE"  ~ "Temperate East",
                                            marine_region == "SE" ~ "South-east",
                                            marine_region == "SW"  ~ "South-west"
    ))
}





