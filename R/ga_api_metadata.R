#' Retrieve Metadata from the GlobalArchive API
#'
#' This function retrieves metadata associated with a specific synthesis in GlobalArchive 
#' by making an API call. The metadata includes geographic coordinates, which are processed 
#' into separate latitude and longitude columns. 
#'
#' @param username A character string representing your GlobalArchive username for API authentication.
#' @param password A character string representing your GlobalArchive password for API authentication.
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
ga_api_metadata <- function(username, password, synthesis_id) {
  
  # URL for the API endpoint
  url <- paste0("https://dev.globalarchive.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&format=feather")
  
  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")
    
    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")
    
    # Read the Feather file from the input stream
    metadata_raw <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT " = "", "[()]" = ""))) %>%
      tidyr::separate(coordinates, into = c("longitude_dd", "latitude_dd"), sep = " ") %>%
      dplyr::mutate(latitude_dd = as.numeric(latitude_dd), longitude_dd = as.numeric(longitude_dd)) %>%
      dplyr::rename(sample_url = url) %>%
      dplyr::select(-c(row, derived_from_sample, map_marker_colour_rgb, annotation_parameters))
    
    # Add marine parks to metadata ----
    metadata <- metadata_raw
    
  } else {
    # Handle request failure
    cat("Request failed with status code:", status_code(response))
  }
  
  return(metadata)
  
}
