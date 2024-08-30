#' Standardize Column Names and Types in a Tibble
#'
#' This function standardizes the column names and types in a given tibble. 
#' It renames the columns to a consistent set of names: "aphiaid", "scientific_name", 
#' "authority", and "status". If the tibble is not empty, it also ensures that the 
#' "scientific_name" column is of type character.
#'
#' @param tibble A tibble or data frame to standardize.
#'
#' @return A tibble with standardized column names and types, or \code{NULL} if the input tibble is empty.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample tibble
#' sample_data <- tibble::tibble(aphiaid = c(1, 2), sci_name = c("Species A", "Species B"), 
#'                               auth = c("Author A", "Author B"), stat = c("Valid", "Invalid"))
#' 
#' # Standardize the tibble
#' standardized_data <- standardize_tibble(sample_data)
#' print(standardized_data)
#' }
standardize_tibble <- function(tibble) {
  if (nrow(tibble) > 0) {
    # Rename columns to consistent names
    colnames(tibble) <- c("aphiaid", "scientific_name", "authority", "status")
    # Convert column types if needed
    tibble$scientific_name <- as.character(tibble$scientific_name)
    return(tibble)
  }
  return(NULL)  # Return NULL for empty tibbles
}