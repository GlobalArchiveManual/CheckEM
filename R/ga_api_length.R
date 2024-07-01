# 
#' Function to API call length data from a synthesis in GlobalArchive
#'
#'
#' @return
#' @export
#'
#' @examples
ga_api_length <- function(username, password, synthesis_id, include_life_history = TRUE) {
  
  species_list <- CheckEM::ga_api_species_list("public", "sharedaccess")
  
  if(include_life_history %in% TRUE){
    
    species_list <- species_list
    
  } else{
    
    species_list <- species_list %>%
      dplyr::select(subject, australian_common_name, family, genus, species, caab)
    
  }
  
  # URL
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    length <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianAquaticFaunaSubject")) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::rename(sample_url = url) %>%
      dplyr::select(-subject)
    
  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }
  
  return(length)
  
}