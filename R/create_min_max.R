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
    filter(!is.na(fb_length_max)) %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(famlength_max = mean(fb_length_max)) %>%
    ungroup()
  
  genus_max <- life_history %>%
    filter(!is.na(fb_length_max)) %>%
    dplyr::group_by(genus) %>%
    dplyr::summarise(genuslength_max = mean(fb_length_max)) %>%
    ungroup()
  
  left_join(life_history, family_max, by = c("family")) %>% # add in family values
    left_join(., genus_max) %>% # add in genus values
    dplyr::mutate(fb_length_max = ifelse((is.na(fb_length_max)), genuslength_max, fb_length_max)) %>%
    dplyr::mutate(fb_length_max = ifelse((is.na(fb_length_max)), famlength_max, fb_length_max)) %>%
    dplyr::select(-c(famlength_max, genuslength_max)) %>%
    dplyr::mutate(fb_length_max = fb_length_max * 10) %>%
    mutate(min_length_mm = minimum * fb_length_max) %>%
    mutate(max_length_mm = maximum * fb_length_max) %>% 
    dplyr::select(family, genus, species, min_length_mm, max_length_mm, fb_length_max) %>%
    dplyr::filter(!is.na(min_length_mm)) %>%
    distinct()
}





