#' Retrieve Length Data from the GlobalArchive API
#'
#' This function retrieves length data associated with a specific synthesis in GlobalArchive 
#' by making an API call. The data can optionally include life history information for species. 
#' The data is returned in a processed format with species information joined based on the 
#' synthesis data.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID for which the length data should be retrieved.
#' @param include_life_history A logical value indicating whether to include life history data 
#' (default is TRUE). If FALSE, only basic species information is returned.
#'
#' @return A data frame containing length data for the synthesis, with optional life history 
#' information and species data joined from GlobalArchive.
#' 
#' The data frame includes the following columns:
#' \itemize{
#'   \item \code{australian_common_name}: Common name of the species.
#'   \item \code{family}: The family of the species.
#'   \item \code{genus}: The genus of the species.
#'   \item \code{species}: The species name.
#'   \item \code{caab}: The CAAB (Codes for Australian Aquatic Biota) code.
#'   \item \code{length}: The recorded length.
#'   \item \code{sample_url}: The URL of the sample in GlobalArchive.
#'   \item \code{other_columns}: Any other columns present in the Feather file that were not removed.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch length data including life history
#' length <- ga_api_length("your_username", "your_password", synthesis_id = 1234)
#' 
#' # Fetch length data without life history
#' length <- ga_api_length("your_username", "your_password", synthesis_id = 1234, include_life_history = FALSE)
#' }
ga_api_length <- function(token, synthesis_id, include_life_history = TRUE) {
  
  # Retrieve the species list
  species_list <- CheckEM::ga_api_species_list(token)
  
  # Conditionally modify the species list based on include_life_history parameter
  if (!include_life_history) {
    species_list <- species_list %>%
      dplyr::select(subject, australian_common_name, family, genus, species, caab)
  }
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
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
    length <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianAquaticFaunaSubject")) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::rename(sample_url = sample) %>%
      dplyr::select(-c(subject, row))
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(length)
  
}