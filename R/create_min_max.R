#' A function to create minimum and maximum size limits to check annotations against
#'
#' @param life_history 
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
create_min_max <- function(life_history, minimum, maximum) {
  
  family_max <- life_history %>%
    filter(!is.na(length_max_cm)) %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(famlength_max = mean(length_max_cm)) %>%
    ungroup()
  
  genus_max <- life_history %>%
    filter(!is.na(length_max_cm)) %>%
    dplyr::group_by(genus) %>%
    dplyr::summarise(genuslength_max = mean(length_max_cm)) %>%
    ungroup()
  
  left_join(life_history, family_max, by = c("family")) %>% # add in family values
    left_join(., genus_max) %>% # add in genus values
    dplyr::mutate(length_max_cm = ifelse((is.na(length_max_cm)), genuslength_max, length_max_cm)) %>%
    dplyr::mutate(length_max_cm = ifelse((is.na(length_max_cm)), famlength_max, length_max_cm)) %>%
    dplyr::select(-c(famlength_max, genuslength_max)) %>%
    dplyr::mutate(length_max_mm = length_max_cm * 10) %>%
    mutate(min_length_mm = minimum * length_max_mm) %>%
    mutate(max_length_mm = maximum * length_max_mm) %>% 
    dplyr::select(family, genus, species, min_length_mm, max_length_mm, length_max_mm) %>%
    dplyr::filter(!is.na(min_length_mm)) %>%
    distinct()
}





