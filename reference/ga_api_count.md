# Retrieve Count Data from the GlobalArchive API

This function retrieves count data from a GlobalArchive synthesis using
an API call. It allows you to include or exclude life history
information in the retrieved data and processes the data accordingly.

## Usage

``` r
ga_api_count(token, synthesis_id, include_life_history = TRUE)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

- synthesis_id:

  A character string or numeric value representing the GlobalArchive
  synthesis ID for which the count data should be retrieved.

- include_life_history:

  A logical value indicating whether life history information should be
  included in the retrieved data. Defaults to `TRUE`. If `FALSE`, only
  basic species information is included.

## Value

A data frame containing the count data retrieved from the GlobalArchive
API. The data frame includes count data for various subjects, with
optional life history details depending on the `include_life_history`
parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve count data including life history information
count_data <- ga_api_count(username = "your_username", password = "your_password", 
                           synthesis_id = "your_synthesis_id", include_life_history = TRUE)

# Retrieve count data without life history information
count_data <- ga_api_count(username = "your_username", password = "your_password", 
                           synthesis_id = "your_synthesis_id", include_life_history = FALSE)
} # }
```
