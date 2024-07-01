# 
#' Function to API call metadata from a synthesis in GlobalArchive
#'
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 

ga_api_metadata <- function(username, password, synthesis_id) {
  # TODO brooke rename sample
  # URL
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
      dplyr::rename(sample_url = url) #%>%
    #dplyr::select(-c(status))
    
    # Add marine parks to metadata ----
    metadata <- metadata_raw
    
    # # Add spatial
    # coordinates(metadata_spatial) <- c('longitude', 'latitude')
    # proj4string(metadata_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # # Add in marine spatial zoning information ----
    # metadata <- bind_cols(metadata_raw, over(metadata_spatial, marineparks)) %>%
    #   dplyr::rename(zone = ZONE_TYPE) %>%
    #   tidyr::replace_na(list(status = "Fished"))
    #
    # metadata$zone <- fct_relevel(metadata$zone,
    #                              "National Park Zone",
    #                              "Sanctuary Zone",
    #                              "General Use",
    #                              "General Use Zone",
    #                              "Habitat Protection Zone",
    #                              "Multiple Use Zone",
    #                              "Recreational Use Zone",
    #                              "Special Purpose Zone (Mining Exclusion)")
  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }
  
  return(metadata)
  
}
