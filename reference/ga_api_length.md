# Retrieve Length Data from the GlobalArchive API

This function retrieves length data associated with a specific synthesis
in GlobalArchive by making an API call. The data can optionally include
life history information for species. The data is returned in a
processed format with species information joined based on the synthesis
data.

## Usage

``` r
ga_api_length(token, synthesis_id, include_life_history = TRUE)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

- synthesis_id:

  A character string or numeric value representing the GlobalArchive
  synthesis ID for which the length data should be retrieved.

- include_life_history:

  A logical value indicating whether to include life history data
  (default is TRUE). If FALSE, only basic species information is
  returned.

## Value

A data frame containing length data for the synthesis, with optional
life history information and species data joined from GlobalArchive.

The data frame includes the following columns:

- `australian_common_name`: Common name of the species.

- `family`: The family of the species.

- `genus`: The genus of the species.

- `species`: The species name.

- `caab`: The CAAB (Codes for Australian Aquatic Biota) code.

- `length`: The recorded length.

- `sample_url`: The URL of the sample in GlobalArchive.

- `other_columns`: Any other columns present in the Feather file that
  were not removed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch length data including life history
length <- ga_api_length("your_username", "your_password", synthesis_id = 1234)

# Fetch length data without life history
length <- ga_api_length("your_username", "your_password", synthesis_id = 1234, include_life_history = FALSE)
} # }
```
