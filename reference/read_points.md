# Read Point Data from EventMeasure files (\_Points.txt)

This function reads EventMeasure point data files (\_Points.txt) from a
specified directory and processes them into a single data frame. The
data frame contains point annotations, including columns for the
campaignID, sample, and other relevant information.

## Usage

``` r
read_points(dir, recursive = FALSE, method = "BRUVs")
```

## Arguments

- dir:

  The directory where the .txt files are saved.

- recursive:

  Logical, whether to search for files recursively in subdirectories.
  Default is `FALSE`.

- method:

  A character string indicating the method used ("BRUVs" or "DOVs").
  Default is "BRUVs".

## Value

A data frame containing all point annotations from the specified files,
with standardised columns for campaign ID, sample, family, genus,
species, number, and period.

## Examples

``` r
if (FALSE) { # \dontrun{
# To read point data from a directory using the BRUVs method
points_data <- read_points("path/to/directory")

# To read point data from a directory recursively using the DOVs method
points_data <- read_points("path/to/directory", recursive = TRUE, method = "DOVs")
} # }
```
