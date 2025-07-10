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
    cat("Request succeeded.\n")
    
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
