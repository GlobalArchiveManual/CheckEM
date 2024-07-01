# 
#' Function to API call to the GlobalArchive species list
#'
#'
#' @return
#' @export
#'
#' @examples
ga_api_species_list <- function(username, password) {
  
  # URL
  url <- paste0("https://dev.globalarchive.org/api/data/AustralianAquaticFaunaSubject/?format=feather")
  
  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))
  
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
      dplyr::select(-c(row, annotation_list, native_id_in_list, is_benthic_subject))
    # left_join(lm) %>%
    #dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(species_lm), fb_length_at_maturity_cm, species_lm))
    
    names(species_list)
    
  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }
  
  return(species_list)
  
}