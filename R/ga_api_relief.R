#' Retrieve Relief Data from the GlobalArchive API
#'
#' This function retrieves habitat data from a GlobalArchive synthesis using an API call. It processes
#' the data to include relevant species information by merging with a benthic species list.
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID for which the habitat data should be retrieved.
#'
#' @return A data frame containing habitat data retrieved from the GlobalArchive API. The data frame
#' includes species information merged from the benthic species list.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve relief metadata from a synthesis
#' relief <- ga_api_relief(username = "your_username", password = "your_password", 
#'                                    synthesis_id = "your_synthesis_id")
#' print(relief)
#' }
ga_api_relief <- function(token, synthesis_id) {
  
  # Retrieve the benthic species list
  species_list <- ga_api_benthic_list(token)
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisBenthosReliefEntry/?sample__synthesis=", synthesis_id, "&format=feather")
  
  # Include the token in the request headers
  headers <- add_headers(Authorization = paste("Token", token))
  
  # Send GET request with token-based authentication
  response <- httr::GET(url, headers)
  
  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    relief <- arrow::read_feather(raw_connection) %>%
      dplyr::rename(sample_url = sample) %>%
      # dplyr::mutate(sample = case_when(
      #   period %in% "nan" ~ opcode,
      #   opcode %in% "nan" ~ period,
      #   .default = paste(opcode, period, sep = "_")
      # )) %>%
      dplyr::mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "AustralianBenthicBiotaAndSubstrateSubject")) %>%
      dplyr::select(-c(row)) %>%
      dplyr::left_join(., species_list, by = "subject") %>%
      dplyr::select(-c(subject))
    
    cat("Request succeeded: Benthos relief.\n")
    return(relief)
    
  } 
  
    else if (httr::status_code(response) == 400) {
    cat("Request failed with status code 400: Bad Request - Your request is malformed.\n")
  } else if (httr::status_code(response) == 401) {
    cat("Request failed with status code 401: Unauthorized - Your API token is likely expired or invalid.\n")
  } else if (httr::status_code(response) == 403) {
    cat("Request failed with status code 403: Forbidden - Your API token is correct but you don't have permission to access this synthesis.\n")
  } else if (httr::status_code(response) == 404) {
    cat("Request failed with status code 404: Not Found - Check the synthesis ID.\n")
  } else if (httr::status_code(response) == 405) {
    cat("Request failed with status code 405: Method Not Allowed - Check the HTTP method (GET, POST, etc.).\n")
  } else if (httr::status_code(response) == 408) {
    cat("Request failed with status code 408: Request Timeout - The server timed out waiting for the request.\n")
  } else if (httr::status_code(response) == 429) {
    cat("Request failed with status code 429: Too Many Requests - You have hit the rate limit.\n")
  } else if (httr::status_code(response) == 500) {
    cat("Request failed with status code 500: Internal Server Error - Something went wrong on the server side.\n")
  } else if (httr::status_code(response) == 502) {
    cat("Request failed with status code 502: Bad Gateway - The server received an invalid response from the upstream server.\n")
  } else if (httr::status_code(response) == 503) {
    cat("Request failed with status code 503: Service Unavailable - The server is temporarily unavailable.\n")
  } else if (httr::status_code(response) == 504) {
    cat("Request failed with status code 504: Gateway Timeout - The server didn't get a response in time.\n")
  } else {
    cat("Request failed with status code", httr::status_code(response), ": Unknown error.\n")
  }
  

  
}
