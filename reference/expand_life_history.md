# Expand Life-History Data Frame for Species and Regions

This function takes a life-history data frame where multiple marine
regions for each species are listed in a single cell, separated by
commas. It expands this data frame so that each species-region
combination gets its own row, with marine regions translated into their
full names.

## Usage

``` r
expand_life_history(dat)
```

## Arguments

- dat:

  A data frame containing life history information with a column for
  marine regions. The `marine_region` column should contain
  comma-separated region codes (e.g., "NW, N, CS").

## Value

A data frame where each species-region combination is represented in a
separate row. The `marine_region` codes are expanded to their full names
(e.g., "NW" to "North-west").

## Examples

``` r
# Create a sample data frame with species and comma-separated marine regions
dat <- data.frame(
  species = c("Species A", "Species B"),
  marine_region = c("NW, N, CS", "TE, SE")
)

# Expand the data frame to have one row per species-region combination
expanded_dat <- expand_life_history(dat)

# Check the result
print(expanded_dat)
#> # A tibble: 5 × 2
#>   species   marine_region 
#>   <chr>     <chr>         
#> 1 Species A North-west    
#> 2 Species A North         
#> 3 Species A Coral Sea     
#> 4 Species B Temperate East
#> 5 Species B South-east    
```
