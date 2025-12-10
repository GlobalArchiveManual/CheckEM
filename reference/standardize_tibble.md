# Standardize Column Names and Types in a Tibble

This function standardizes the column names and types in a given tibble.
It renames the columns to a consistent set of names: "aphiaid",
"scientific_name", "authority", and "status". If the tibble is not
empty, it also ensures that the "scientific_name" column is of type
character.

## Usage

``` r
standardize_tibble(tibble)
```

## Arguments

- tibble:

  A tibble or data frame to standardize.

## Value

A tibble with standardized column names and types, or `NULL` if the
input tibble is empty.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a sample tibble
sample_data <- tibble::tibble(aphiaid = c(1, 2), sci_name = c("Species A", "Species B"), 
                              auth = c("Author A", "Author B"), stat = c("Valid", "Invalid"))

# Standardize the tibble
standardized_data <- standardize_tibble(sample_data)
print(standardized_data)
} # }
```
