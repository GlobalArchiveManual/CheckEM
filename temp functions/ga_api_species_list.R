#' Retrieve Species List from the Global Archive API
#' 
#' This function retrieves the species list from GlobalArchive by making an API call. The data 
#' is returned as a data frame with relevant species information, excluding some columns that 
#' are not needed for further analysis.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#'
#' @return A data frame containing the species list from GlobalArchive. 
#' The data frame includes the following columns:
#' \itemize{
#'   \item \code{subject}: The URL or identifier of the species in GlobalArchive.
#'   \item \code{australian_common_name}: Common name of the species.
#'   \item \code{family}: The family of the species.
#'   \item \code{genus}: The genus of the species.
#'   \item \code{species}: The species name.
#'   \item \code{caab}: The CAAB (Codes for Australian Aquatic Biota) code.
#'   \item \code{other_columns}: Any other relevant columns included in the original dataset.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch the species list from GlobalArchive
#' species_list <- ga_api_species_list("your_username", "your_password")
#' 
#' # Display the first few rows of the species list
#' head(species_list)
#' }
ga_api_species_list <- function(token) {
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/AustralianAquaticFaunaSubject/?format=feather")
  
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
      dplyr::select(-c(row, annotation_list, 
                       native_id_in_list, 
                       is_benthic_subject))
    
    names(species_list)
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(species_list)
  
}
