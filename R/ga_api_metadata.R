#' Retrieve Metadata from the GlobalArchive API
#'
#' This function retrieves metadata associated with a specific synthesis in GlobalArchive 
#' by making an API call. The metadata includes geographic coordinates, which are processed 
#' into separate latitude and longitude columns. 
#'
#' @param token A character string representing your GlobalArchive token for API authentication.
#' @param synthesis_id A character string or numeric value representing the GlobalArchive synthesis ID for which the metadata should be retrieved.
#'
#' @return A data frame containing metadata for the synthesis, including processed coordinates and other relevant information. 
#' If the API request fails, the function returns NULL and prints the status code.
#' 
#' The data frame includes the following columns:
#' TODO - brooke to add all columns
#' \itemize{
#'   \item \code{latitude_dd}: The latitude in decimal degrees.
#'   \item \code{longitude_dd}: The longitude in decimal degrees.
#'   \item \code{sample_url}: The URL of the sample in GlobalArchive.
#'   \item \code{other_columns}: Any other columns present in the Feather file that were not removed.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch metadata for a specific synthesis
#' metadata <- ga_api_metadata("your_username", "your_password", synthesis_id = 1234)
#' 
#' # Display the first few rows of metadata
#' head(metadata)
#' }
ga_api_metadata <- function(token, synthesis_id) {
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&format=feather")
  
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
    metadata_raw <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT" = "", "[()]" = ""))) %>%
      tidyr::separate(coordinates, into = c("longitude_dd", "latitude_dd"), sep = " ") %>%
      dplyr::mutate(latitude_dd = as.numeric(latitude_dd), longitude_dd = as.numeric(longitude_dd)) %>%
      dplyr::mutate(sample = case_when(
        period %in% "nan" ~ opcode,
        opcode %in% "nan" ~ period,
        .default = paste(opcode, period, sep = "_")
      )) %>%
      dplyr::rename(sample_url = url) %>%
      dplyr::select(-c(row))
    
    # Add marine parks to metadata ----
    metadata <- metadata_raw
  
  } 
  
  if (httr::status_code(response) == 200) {
    cat("Request succeeded: Sample Metadata.\n")
    return(metadata)
    
  } else if (httr::status_code(response) == 400) {
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
