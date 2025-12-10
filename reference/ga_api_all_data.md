# Retrieve all Data from a synthesis using the GlobalArchive API

This function retrieves metadata, count, and length data from a CheckEM
synthesis, processes the data, and saves the results as RDS files in the
specified directory with the given name. It also returns the processed
data in the environment and allows users to add in zeros where a species
is not present If the directory doesn't exist, it will be created, and
the user will be informed.

## Usage

``` r
ga_api_all_data(
  token,
  synthesis_id,
  dir,
  include_zeros = FALSE,
  file_prefix = NULL
)
```

## Arguments

- token:

  A character string representing your GlobalArchive token for API
  authentication.

- synthesis_id:

  A character string or numeric value representing the GlobalArchive
  synthesis ID for which the data should be retrieved.

- dir:

  A character string specifying the directory where the RDS files will
  be saved.

- include_zeros:

  Either TRUE or FALSE, if you would like the data to include zeros
  where a species is not observed, warning: this can create large
  datasets that use a lot og RAM

## Value

A list containing `metadata`, `count`, and `length` data frames.
