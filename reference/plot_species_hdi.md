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

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ggplot2)
library(ggdist)

# Minimal example data for one species
species_data <- tibble::tibble(
  scientific_name = rep("Lutjanus sebae", 40),
  length_mm = c(
    rnorm(35, mean = 420, sd = 40),
    rnorm(5,  mean = 650, sd = 10)
  )
)

# Maximum length lookup table
max_lengths <- tibble::tibble(
  scientific_name = "Lutjanus sebae",
  length_max_mm   = 1000
)

# Temporary output directory
out_dir <- tempfile("hdi_plot_")
dir.create(out_dir)
out_dir <- paste0(out_dir, .Platform$file.sep)

# Run the function
plot_species_hdi(
  species_data = species_data,
  max_lengths  = max_lengths,
  dir          = out_dir
)
} # }
```
