#' Retrieve Benthic Species List from the Global Archive API
#'
#' This function retrieves a list of benthic species from the Global Archive API. It makes an API call to
#' the specified endpoint using basic authentication, reads the response in Feather format, and returns
#' the data as a data frame. The resulting data frame is cleaned by renaming and removing certain columns.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#'
#' @return A data frame containing the list of benthic species. The data frame includes columns for
#' species details, with some columns removed for clarity.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve the benthic species list using API credentials
#' species_list <- ga_api_benthic_list("your_username", "your_password")
#' print(species_list)
#' }
ga_api_benthic_list <- function(token) {
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/AustralianBenthicBiotaAndSubstrateSubject/?format=feather")
  
  # Include the token in the request headers
  headers <- add_headers(Authorization = paste("Token", token))
  
  # Send GET request with token-based authentication
  response <- GET(url, headers)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    species_list <- arrow::read_feather(raw_connection) %>%
      as.data.frame() %>%
      dplyr::rename(subject = url) %>%
      dplyr::select(-c(row, annotation_list, native_id_in_list, is_benthic_subject, subject_common_name, qualifiers))
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(species_list)
  
}
