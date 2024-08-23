#' Check if a String Contains Special Characters
#'
#' This function checks whether a given string contains any special characters. 
#' Special characters are defined as any character that is not alphanumeric (letters or numbers).
#'
#' @param string A character string to be checked for special characters.
#'
#' @return A logical value: \code{TRUE} if the string contains any special characters, 
#' \code{FALSE} otherwise.
#' @export
#'
#' @examples
#' # Check if a string contains special characters
#' has_special_characters("HelloWorld123") # Returns FALSE
#' has_special_characters("Hello@World!")  # Returns TRUE
has_special_characters <- function(string) {
  any(str_detect(string, "[^[:alnum:]]"))
}