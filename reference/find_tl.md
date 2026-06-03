# Retrieve Trophic Level and Standard Error from FishBase

This function retrieves the trophic level and its standard error for a
given species from FishBase. It fetches data from the FishBase website,
extracts the trophic level information, and formats it into a data
frame.

## Usage

``` r
find_tl(sp, mirror = "us")
```

## Arguments

- sp:

  A character string representing the species name. The species name
  should be provided as it appears on FishBase, but spaces will be
  replaced with hyphens.

- mirror:

  A character string specifying the FishBase mirror site to use.
  Defaults to "us" (FishBase USA). Other mirrors can be specified if
  needed.

## Value

A data frame containing the trophic level information for the species.
The data frame includes the following columns:

- **species**: The species name.

- **trophic_level**: The trophic level of the species.

- **se**: The standard error of the trophic level.

- **metric**: The source used (if provided) e.g. "based on food items".

If there is no internet connection, or if the species page cannot be
retrieved or parsed, the function returns `NULL`.

## Examples

``` r
# Retrieve trophic level and standard error for a species
tl_info <- find_tl("Lutjanus campechanus")
#> Forbidden (HTTP 403).
print(tl_info)
#> NULL
```
