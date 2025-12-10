# Calculate Minimum and Maximum Size Limits for Fish Species

This function processes a data frame containing fish life history
information to compute minimum and maximum size limits for each species
based on their maximum length to check your annotations against. It uses
the maximum length data from genus and family levels to fill in any
missing maximum lengths at the species level. The resulting data frame
includes the computed minimum and maximum size limits in millimeters.

## Usage

``` r
create_min_max(life_history, minimum, maximum)
```

## Arguments

- life_history:

  A data frame containing fish life history information. This data frame
  should include columns for `family`, `genus`, `species`, and
  `length_max_cm` (the maximum length in centimeters). Missing values in
  `length_max_cm` will be filled using average lengths from genus or
  family levels if available.

- minimum:

  A numeric value representing the multiplier to calculate the minimum
  size limit. The final minimum size limit in millimeters is computed as
  `minimum * length_max_mm`.

- maximum:

  A numeric value representing the multiplier to calculate the maximum
  size limit. The final maximum size limit in millimeters is computed as
  `maximum * length_max_mm`.

## Value

A data frame with the following columns:

- **family**: Fish family.

- **genus**: Fish genus.

- **species**: Fish species.

- **min_length_mm**: Calculated minimum size limit in millimeters.

- **max_length_mm**: Calculated maximum size limit in millimeters.

- **length_max_mm**: Maximum length of the fish in millimeters.

Rows with missing minimum length values are excluded from the result.

## Examples

``` r
# Assume australia_life_history is a data frame with appropriate columns
life_history_with_min_max <- create_min_max(CheckEM::australia_life_history, minimum = 15, maximum = 85)
#> Joining with `by = join_by(genus)`
```
