# 
#' Function to API call metadata from a synthesis in GlobalArchive
#'
#'
#' @return
#' @export
#'
#' @examples
ga_api_habitat <- function(username, password, synthesis_id) {
  
  # URL
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisBenthosEntry/?synthesis=", synthesis_id, "&format=feather")
  
  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    habitat <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(sample = url) 
  

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }
  
  return(habitat)
  
}
