#' Retrieve Count Data from the GlobalArchive API
#'
#' This function retrieves count data from a GlobalArchive synthesis using an API call. It allows you to 
#' include or exclude life history information in the retrieved data and processes the data accordingly.
#'
#' @param username A character string representing your GlobalArchive username for API authentication.
#' @param password A character string representing your GlobalArchive password for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID 
#' for which the count data should be retrieved.
#' @param include_life_history A logical value indicating whether life history information should 
#' be included in the retrieved data. Defaults to `TRUE`. If `FALSE`, only basic species information 
#' is included.
#'
#' @return A data frame containing the count data retrieved from the GlobalArchive API. The data frame
#' includes count data for various subjects, with optional life history details depending on the 
#' `include_life_history` parameter.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve count data including life history information
#' count_data <- ga_api_count(username = "your_username", password = "your_password", 
#'                            synthesis_id = "your_synthesis_id", include_life_history = TRUE)
#' 
#' # Retrieve count data without life history information
#' count_data <- ga_api_count(username = "your_username", password = "your_password", 
#'                            synthesis_id = "your_synthesis_id", include_life_history = FALSE)
#' }
ga_api_count <- function(username, password, synthesis_id, include_life_history = TRUE) {
  
  # Retrieve the species list
  species_list <- CheckEM::ga_api_species_list(username, password)
  
  # Conditionally modify the species list based on include_life_history parameter
  if (!include_life_history) {
    species_list <- species_list %>%
      dplyr::select(subject, australian_common_name, family, genus, species, caab)
  }
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    count <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianAquaticFaunaSubject")) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::rename(sample_url = sample) %>%
      dplyr::select(-c(subject, row))
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(count)
  
}