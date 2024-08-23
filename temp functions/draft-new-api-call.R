synthesis_id <- 14
token <- "22b3011d61728e614822fa9537863326a30ec7cb2c77a98adc8d360e0cbb1b2c"

library(httr)
library(tidyverse)

# URL for the API endpoint
url <- paste0("https://dev.globalarchive.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&format=feather")

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
  metadata_raw <- arrow::read_feather(raw_connection) %>%
    dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT " = "", "[()]" = ""))) %>%
    tidyr::separate(coordinates, into = c("longitude_dd", "latitude_dd"), sep = " ") %>%
    dplyr::mutate(latitude_dd = as.numeric(latitude_dd), longitude_dd = as.numeric(longitude_dd)) %>%
    dplyr::rename(sample_url = url) %>%
    dplyr::select(-c(row))
  
  # Add marine parks to metadata ----
  metadata <- metadata_raw
}
