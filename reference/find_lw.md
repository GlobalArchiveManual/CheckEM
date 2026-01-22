# Retrieve Bayesian Length-Weight Equation from FishBase

This function retrieves the Bayesian length-weight equation parameters
for a given species from FishBase. It fetches data from the FishBase
website and parses the Bayesian length-weight parameters including the
mean (m), standard deviation (sd), and the metric used.

## Usage

``` r
find_lw(sp, mirror = "us")
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

A data frame containing the Bayesian length-weight parameters for the
species. The data frame includes the following columns:

- **species**: The species name.

- **lwa_m**: The parameter for the length-weight equation (a).

- **lwa_sd**: The standard deviation of the parameter for the
  length-weight equation (a).

- **lwb_m**: The parameter for the length-weight equation (b).

- **lwb_sd**: The standard deviation of the parameter for the
  length-weight equation (b).

- **metric**: The metric used for length measurement (e.g., "based on
  LWR estimates for this species & (Sub)family-body").

If there is no internet connection or if the species page cannot be
retrieved, the function returns `NULL`.

## Examples

``` r
# Retrieve Bayesian length-weight parameters for a species
lw_params <- find_lw("Lutjanus campechanus")
#> Forbidden (HTTP 403).
print(lw_params)
#> NULL
```
