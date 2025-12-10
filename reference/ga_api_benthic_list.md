# Retrieve Benthic Species List from the Global Archive API

This function retrieves a list of benthic species from the Global
Archive API. It makes an API call to the specified endpoint using basic
authentication, reads the response in Feather format, and returns the
data as a data frame. The resulting data frame is cleaned by renaming
and removing certain columns.

## Usage

``` r
ga_api_benthic_list(token)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

## Value

A data frame containing the list of benthic species. The data frame
includes columns for species details, with some columns removed for
clarity.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve the benthic species list using API credentials
species_list <- ga_api_benthic_list("your_username", "your_password")
print(species_list)
} # }
```
