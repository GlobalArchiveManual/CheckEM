# Retrieve count data from GlobalArchive
#' @description
#' Function to retrieve count data from a GlobalArchive synthesis using an API call 
#'
#' @param username your GlobalArchive username as a string. 
#' @param password your GlobalArchive password as a string. 
#' @param synthesis_id the GlobalArchive synthesis ID number.
#' @param include_life_history if life history information should be included in the download.
#'
#' @returns A data frame of count data retrieved using the GlobalArchive API. 
#' @export
#'
#' @examples
#' \dontrun{
#' count <- ga_api_count(username = "your_username", password = "your_password", synthesis_id = "your_synthesis_id, include_life_history = TRUE)
#' }
ga_api_count <- function(username, password, synthesis_id, include_life_history = TRUE) {
  
  # TODO brooke to add functionality life_history = TRUE or FALSE
  
  species_list <- CheckEM::ga_api_species_list(username, password)
  
  if(include_life_history %in% TRUE){
    
    species_list <- species_list
    
  } else{
    
    species_list <- species_list %>%
      dplyr::select(subject, australian_common_name, family, genus, species, caab)
      
  }
  
  # URL
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
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }
  
  return(count)
  
}