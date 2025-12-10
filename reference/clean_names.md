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
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Check the result
print(cleaned_dat)
#>   name1 name_2 name_72
#> 1     1      4       7
#> 2     2      5       8
#> 3     3      6       9
```
