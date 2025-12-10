# Read "Generic" length data

This function reads "Generic" length data files (either .csv or .txt)
from a specified directory and processes them into a single data frame.
The data frame contains information such as campaignID, sample, length
(mm) and species information.

## Usage

``` r
read_gen_length(dir, method = "BRUVs", recursive = FALSE)
```

## Arguments

- dir:

  The directory where the length data files are saved.

- method:

  A character string indicating the method used ("BRUVs" or "DOVs").
  Default is "BRUVs".

- recursive:

  Logical, whether to search for files recursively in subdirectories.
  Default is `FALSE`.

## Value

A data frame containing all length data from the specified files, with
standardised column names and formats.

## Examples

``` r
if (FALSE) { # \dontrun{
# To read length data from a directory using the BRUVs method
length_data <- read_gen_length("path/to/directory")

# To read length data from a directory recursively using the DOVs method
length_data <- read_gen_length("path/to/directory", method = "DOVs", recursive = TRUE)
} # }
```
