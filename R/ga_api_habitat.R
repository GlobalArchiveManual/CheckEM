#' Retrieve Habitat Data from the GlobalArchive API
#'
#' This function retrieves habitat data from a GlobalArchive synthesis using an API call. It processes
#' the data to include relevant species information by merging with a benthic species list.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID for which the habitat data should be retrieved.
#'
#' @return A data frame containing habitat data retrieved from the GlobalArchive API. The data frame
#' includes species information merged from the benthic species list.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve habitat metadata from a synthesis
#' habitat <- ga_api_habitat(username = "your_username", password = "your_password", 
#'                                    synthesis_id = "your_synthesis_id")
#' print(habitat)
#' }
ga_api_habitat <- function(token, synthesis_id) {
  
  # Retrieve the benthic species list
  species_list <- ga_api_benthic_list(token)
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisBenthosCountEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
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
    habitat <- arrow::read_feather(raw_connection) %>%
      dplyr::rename(sample_url = sample) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianBenthicBiotaAndSubstrateSubject")) %>%
      dplyr::select(-c(row)) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::select(-c(subject))
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(habitat)
  
}
