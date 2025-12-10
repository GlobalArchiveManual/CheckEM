# Retrieve Relief Data from the GlobalArchive API

This function retrieves habitat data from a GlobalArchive synthesis
using an API call. It processes the data to include relevant species
information by merging with a benthic species list.

## Usage

``` r
ga_api_relief(token, synthesis_id)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

- synthesis_id:

  A character string or numeric value representing the GlobalArchive
  synthesis ID for which the habitat data should be retrieved.

## Value

A data frame containing habitat data retrieved from the GlobalArchive
API. The data frame includes species information merged from the benthic
species list.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve relief metadata from a synthesis
relief <- ga_api_relief(username = "your_username", password = "your_password", 
                                   synthesis_id = "your_synthesis_id")
print(relief)
} # }
```
