# Read Metadata

This function reads Metadata in a GlobalArchive format (as .csv files)
from a specified directory and processes them into a single dataframe.
The dataframe includes campaignID, sample, depth_m, time, coordinates
and other important information collected when deploying and annotating
stere-video imagery.

## Usage

``` r
read_metadata(dir, method = "BRUVs", recursive = FALSE)
```

## Arguments

- dir:

  The directory where the .csv files are saved.

- method:

  A character string indicating the method used ("BRUVs" or "DOVs").
  Default is "BRUVs".

- recursive:

  Logical, whether to search for files recursively in subdirectories.
  Default is `FALSE`.

## Value

A data frame containing all metadata from the specified files, with
standardised columns for campaignID, sample, and various other metadata
details.

## Examples

``` r
if (FALSE) { # \dontrun{
# To read metadata from a directory using the BRUVs method
metadata <- read_metadata("path/to/directory")

# To read metadata from a directory recursively using the DOVs method
metadata <- read_metadata("path/to/directory", method = "DOVs", recursive = TRUE)
} # }
```
