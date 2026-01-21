#' Calculate Minimum and Maximum Size Limits for Fish Species
#'
#' This function processes a data frame containing fish life history information to compute
#' minimum and maximum size limits for each species based on their maximum length to check your annotations against. 
#' It uses the maximum length data from genus and family levels to fill in any missing 
#' maximum lengths at the species level. The resulting data frame includes the computed
#' minimum and maximum size limits in millimeters.
#'
#' @param life_history A data frame containing fish life history information. This data frame
#' should include columns for `family`, `genus`, `species`, and `length_max_cm` (the maximum length
#' in centimeters). Missing values in `length_max_cm` will be filled using average lengths from 
#' genus or family levels if available.
#' 
#' @param minimum A numeric value representing the multiplier to calculate the minimum size limit.
#' The final minimum size limit in millimeters is computed as `minimum * length_max_mm`.
#' 
#' @param maximum A numeric value representing the multiplier to calculate the maximum size limit.
#' The final maximum size limit in millimeters is computed as `maximum * length_max_mm`.
#' 
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \strong{family}: Fish family.
#'   \item \strong{genus}: Fish genus.
#'   \item \strong{species}: Fish species.
#'   \item \strong{min_length_mm}: Calculated minimum size limit in millimeters.
#'   \item \strong{max_length_mm}: Calculated maximum size limit in millimeters.
#'   \item \strong{length_max_mm}: Maximum length of the fish in millimeters.
#' }
#' Rows with missing minimum length values are excluded from the result.
#' 
#' @examples
#' # Assume australia_life_history is a data frame with appropriate columns
#' life_history_with_min_max <- create_min_max(
#'   CheckEM::australia_life_history,
#'   minimum = 15,
#'   maximum = 85
#' )
#' 
#' @export
#' @import dplyr
create_min_max <- function(life_history, minimum, maximum) {
  
  family_max <- life_history |>
    dplyr::filter(!is.na(length_max_cm)) |>
    dplyr::group_by(family) |>
    dplyr::summarise(famlength_max = mean(length_max_cm), .groups = "drop")
  
  genus_max <- life_history |>
    dplyr::filter(!is.na(length_max_cm)) |>
    dplyr::group_by(genus) |>
    dplyr::summarise(genuslength_max = mean(length_max_cm), .groups = "drop")
  
  dplyr::left_join(life_history, family_max, by = "family") |>
    dplyr::left_join(genus_max, by = "genus") |>
    dplyr::mutate(length_max_cm = dplyr::if_else(is.na(length_max_cm), genuslength_max, length_max_cm)) |>
    dplyr::mutate(length_max_cm = dplyr::if_else(is.na(length_max_cm), famlength_max, length_max_cm)) |>
    dplyr::select(-famlength_max, -genuslength_max) |>
    dplyr::mutate(length_max_mm = length_max_cm * 10) |>
    dplyr::mutate(min_length_mm = minimum * length_max_mm) |>
    dplyr::mutate(max_length_mm = maximum * length_max_mm) |>
    dplyr::select(family, genus, species, min_length_mm, max_length_mm, length_max_mm) |>
    dplyr::filter(!is.na(min_length_mm)) |>
    dplyr::distinct()
}