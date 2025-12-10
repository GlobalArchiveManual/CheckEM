#' Retrieve Count Data from the GlobalArchive API
#'
#' This function retrieves count data from a GlobalArchive synthesis using an API call. It allows you to 
#' include or exclude life history information in the retrieved data and processes the data accordingly.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
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
ga_api_count <- function(token, synthesis_id, include_life_history = TRUE) {
  
  # Retrieve the species list
  species_list <- ga_api_species_list(token)
  
  # Conditionally modify the species list based on include_life_history parameter
  if (!include_life_history) {
    species_list <- species_list %>%
      dplyr::select(subject, australian_common_name, family, genus, species, caab)
  }
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
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
    count <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianAquaticFaunaSubject")) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::rename(sample_url = url) %>%
      dplyr::mutate(sample = case_when(
        period %in% "nan" ~ opcode,
        opcode %in% "nan" ~ period,
        .default = paste(opcode, period, sep = "_")
      )) %>%
      dplyr::select(-c(subject, row)) %>%
      glimpse()
    
    cat("Request succeeded.\n")
    return(count)
    
  } else if (status_code(response) == 400) {
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