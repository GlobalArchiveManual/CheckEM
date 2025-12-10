# Read EventMeasure length and 3D point data (\_3DPoints.txt & \_Lengths.txt)

This function reads length and 3D point information from EventMeasure
export files (`_3DPoints.txt` and `_Lengths.txt`) from a specified
directory and processes them into a single dataframe. The dataframe
includes details such as length, species, sample, and additional
metadata.

## Usage

``` r
read_em_length(dir, method = "BRUVs", recursive = FALSE)
```

## Arguments

- dir:

  The directory where the EventMeasure length data files are located.

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
# To read EventMeasure length data from a directory using the BRUVs method
length_data <- read_em_length("path/to/directory")

# To read length data from a directory recursively using the DOVs method
length_data <- read_em_length("path/to/directory", method = "DOVs", recursive = TRUE)
} # }
```
