# hdi_table

Generates a summary table for species data with Highest Density Interval
(HDI) statistics and comparisons against species-specific maximum
lengths.

## Usage

``` r
hdi_table(species_data, max_lengths)
```

## Arguments

- species_data:

  A dataframe with species data, including columns for `family`,
  `genus`, `species`, `scientific_name`, and `length_mm` (length
  measurements in millimeters).

- max_lengths:

  A dataframe with species-specific maximum lengths. It must include the
  column `scientific_name` to match the species and `length_max_mm` for
  the maximum length in millimeters.

## Value

A dataframe summarizing the HDI statistics for each species, including:

- family:

  Family of the species.

- genus:

  Genus of the species.

- species:

  Species name.

- scientific_name:

  Scientific name of the species.

- species_length_max_mm:

  Maximum length (in millimeters) from the provided species-specific
  data.

- hdci_99_lower_bound:

  Lower bound of the 99% HDI for the species' length measurements.

- hdci_99_upper_bound:

  Upper bound of the 99% HDI for the species' length measurements.

- num_measurements:

  Number of individual length measurements (rows) for the species.

- highest_metric:

  Indicates whether the maximum length (from FishBase) or the HDI upper
  bound is higher.

- num_lengths_over_max:

  The count of lengths that exceed the species-specific maximum length.

- num_lengths_over_hdi_upper:

  The count of lengths that exceed the 99% HDI upper bound.

- num_lengths_smaller_hdi_lower:

  The count of lengths that are smaller than the 99% HDI lower bound.

## Details

This function calculates HDI statistics (99% interval) for each species'
length data, compares these statistics with species-specific maximum
lengths (provided separately), and creates a summary dataframe. The
summary includes family, genus, species, scientific name, species length
max, HDI bounds, number of individuals measured, and metrics like the
number of lengths exceeding the max or HDI upper bound.

The function first loops through each species in the `species_data`,
calculates the 99% HDI using the
[`ggdist::median_hdci()`](https://mjskay.github.io/ggdist/reference/point_interval.html)
function, and compares the species' maximum length (from `max_lengths`)
with the HDI statistics. It then appends the results for each species
into a final dataframe.

## Examples

``` r
# Example usage
species_data <- data.frame(family = c("Acanthuridae"), genus = c("Acanthurus"),
                           species = c("triostegus"), scientific_name = c("Acanthurus triostegus"),
                           length_mm = c(120, 130, 125, 140, 135, 290, 200, 260, 150, 190, 280))
max_lengths <- data.frame(scientific_name = c("Acanthurus triostegus"), length_max_mm = c(270))

hdi_table(species_data, max_lengths)
#>         family      genus    species       scientific_name
#> 1 Acanthuridae Acanthurus triostegus Acanthurus triostegus
#>   species_length_max_mm hdci_99_lower_bound hdci_99_upper_bound
#> 1                   270                 120                 290
#>   num_measurements highest_metric num_lengths_over_max
#> 1               11        HDI 99%                    2
#>   num_lengths_over_hdi_upper num_lengths_smaller_hdi_lower
#> 1                          0                             0
```
