
#' Function to check if a string contains any special characters
#'
#' @param string 
#'
#' @return
#' @export
#'
#' @examples
has_special_characters <- function(string) {
  any(str_detect(string, "[^[:alnum:]]"))
}