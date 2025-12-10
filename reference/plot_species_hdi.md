# Plot length frequency histogram with High Density Interval

This function takes a data frame and standardises its column names by
converting them to lower case, replacing special characters with
underscores, and ensuring that names are syntactically valid R names.

## Usage

``` r
plot_species_hdi(species_data, max_lengths, dir)
```

## Arguments

- species_data:

  A data frame with length information for one species

- max_lengths:

  A data frame with lmaximum length information

- dir:

  Directory where plots will be saved

## Value

Saves plots into directory
