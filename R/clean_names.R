#' A function to standardise names in a data frame
#'
#' @param dat 
#'
#' @return A data frame with all lower case column names. This function also replaces any special characters with an "_".
#' @export
#'
#' @examples
#' 
# dat <- as.data.frame(c("NAmE1", "name-2", "nAMe.-72")) %>%
#   clean_names()
#' 
#' 
clean_names <- function(dat) {
  
  old_names <- names(dat)
  
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  
  setNames(dat, new_names)
}