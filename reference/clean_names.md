# Standardise Column Names in a Data Frame

This function takes a data frame and standardises its column names by
converting them to lower case, replacing special characters with
underscores, and ensuring that names are syntactically valid R names.

## Usage

``` r
clean_names(dat)
```

## Arguments

- dat:

  A data frame whose column names need to be standardized.

## Value

A data frame with standardised column names. All column names are
converted to lower case, special characters are replaced with
underscores, and redundant underscores at the end of names are removed.

## Examples

``` r
# Create a data frame with various column names
dat <- data.frame("NAmE1" = 1:3, "name-2" = 4:6, "nAMe.-72" = 7:9)

# Clean the column names
cleaned_dat <- clean_names(dat)
#> Warning: argument 'pattern' has length > 1 and only the first element will be used

# Check the result
print(cleaned_dat)
#> # A tibble: 3 × 3
#>      ``    ``    ``
#>   <int> <int> <int>
#> 1     1     4     7
#> 2     2     5     8
#> 3     3     6     9
```
