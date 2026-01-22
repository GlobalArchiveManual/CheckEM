#' Standardise Column Names in a Data Frame
#'
#' This function takes a data frame and standardises its column names by converting them to lower case, 
#' replacing special characters with underscores, and ensuring that names are syntactically valid R names.
#'
#' @param dat A data frame whose column names need to be standardized.
#'
#' @return A data frame with standardised column names. All column names are converted to lower case,
#' special characters are replaced with underscores, and redundant underscores at the end of names are removed.
#' 
#' @export
#' @import dplyr
#'
#' @examples
#' # Create a data frame with various column names
#' dat <- data.frame("NAmE1" = 1:3, "name-2" = 4:6, "nAMe.-72" = 7:9)
#'
#' # Clean the column names
#' cleaned_dat <- clean_names(dat)
#' 
#' # Check the result
#' print(cleaned_dat)
#'
clean_names <- function(dat) {
  
  old_names <- names(dat)
  
  new_names <- old_names |>
    gsub("%", "percent") |> 
    make.names() |>
    gsub("[.]+", "_") |>
    tolower() |>
    gsub("_$", "")
  
  stats::setNames(dat, new_names) 
}