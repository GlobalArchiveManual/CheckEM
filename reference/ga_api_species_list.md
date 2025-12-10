# Retrieve Species List from the Global Archive API

This function retrieves the species list from GlobalArchive by making an
API call. The data is returned as a data frame with relevant species
information, excluding some columns that are not needed for further
analysis.

## Usage

``` r
ga_api_species_list(token)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

## Value

A data frame containing the species list from GlobalArchive. The data
frame includes the following columns:

- `subject`: The URL or identifier of the species in GlobalArchive.

- `australian_common_name`: Common name of the species.

- `family`: The family of the species.

- `genus`: The genus of the species.

- `species`: The species name.

- `caab`: The CAAB (Codes for Australian Aquatic Biota) code.

- `other_columns`: Any other relevant columns included in the original
  dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch the species list from GlobalArchive
species_list <- ga_api_species_list("your_username", "your_password")

# Display the first few rows of the species list
head(species_list)
} # }
```
