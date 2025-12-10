# Retrieve Metadata from the GlobalArchive API

This function retrieves metadata associated with a specific synthesis in
GlobalArchive by making an API call. The metadata includes geographic
coordinates, which are processed into separate latitude and longitude
columns.

## Usage

``` r
ga_api_metadata(token, synthesis_id)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

- synthesis_id:

  A character string or numeric value representing the GlobalArchive
  synthesis ID for which the metadata should be retrieved.

## Value

A data frame containing metadata for the synthesis, including processed
coordinates and other relevant information. If the API request fails,
the function returns NULL and prints the status code.

The data frame includes the following columns: TODO - brooke to add all
columns

- `latitude_dd`: The latitude in decimal degrees.

- `longitude_dd`: The longitude in decimal degrees.

- `sample_url`: The URL of the sample in GlobalArchive.

- `other_columns`: Any other columns present in the Feather file that
  were not removed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch metadata for a specific synthesis
metadata <- ga_api_metadata("your_username", "your_password", synthesis_id = 1234)

# Display the first few rows of metadata
head(metadata)
} # }
```
