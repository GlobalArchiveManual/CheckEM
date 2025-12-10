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
    
    cat("Request succeeded: Species list.\n")
    return(species_list)
    
  } 
  
    else if (status_code(response) == 400) {
    cat("Request failed with status code 400: Bad Request – Your request is malformed.\n")
  } else if (status_code(response) == 401) {
    cat("Request failed with status code 401: Unauthorized – Your API token is likely expired or invalid.\n")
  } else if (status_code(response) == 403) {
    cat("Request failed with status code 403: Forbidden – Your API token is correct but you don't have permission to access this synthesis.\n")
  } else if (status_code(response) == 404) {
    cat("Request failed with status code 404: Not Found – Check the synthesis ID.\n")
  } else if (status_code(response) == 405) {
    cat("Request failed with status code 405: Method Not Allowed – Check the HTTP method (GET, POST, etc.).\n")
  } else if (status_code(response) == 408) {
    cat("Request failed with status code 408: Request Timeout – The server timed out waiting for the request.\n")
  } else if (status_code(response) == 429) {
    cat("Request failed with status code 429: Too Many Requests – You have hit the rate limit.\n")
  } else if (status_code(response) == 500) {
    cat("Request failed with status code 500: Internal Server Error – Something went wrong on the server side.\n")
  } else if (status_code(response) == 502) {
    cat("Request failed with status code 502: Bad Gateway – The server received an invalid response from the upstream server.\n")
  } else if (status_code(response) == 503) {
    cat("Request failed with status code 503: Service Unavailable – The server is temporarily unavailable.\n")
  } else if (status_code(response) == 504) {
    cat("Request failed with status code 504: Gateway Timeout – The server didn't get a response in time.\n")
  } else {
    cat("Request failed with status code", status_code(response), ": Unknown error.\n")
  }
  
  
  
}
