# 
#' Function to standardize column names and types
#'
#' @param tibble 
#'
#' @return
#' @export
#'
#' @examples
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