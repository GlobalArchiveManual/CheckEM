# Read Period Data from EventMeasure files (\_Period.txt)

This function reads EventMeasure period data files (\_Period.txt) from a
specified directory and processes them into a single dataframe. The
dataframe contains period annotations, including columns for the
campaignID, sample, and other relevant information.

## Usage

``` r
read_periods(dir, method = "BRUVs", recursive = FALSE)
```

## Arguments

- dir:

  The directory where the .txt files are saved.

- method:

  A character string indicating the method used ("BRUVs" or "DOVs").
  Default is "BRUVs".

- recursive:

  Logical, whether to search for files recursively in subdirectories.
  Default is `FALSE`.

## Value

A data frame containing all period annotations from the specified files,
with standardised columns for campaign ID, sample, opcode, period, start
time, end time, and whether there is an end time.

## Examples

``` r
if (FALSE) { # \dontrun{
# To read period data from a directory using the BRUVs method
periods_data <- read_periods("path/to/directory")

# To read period data from a directory recursively using the DOVs method
periods_data <- read_periods("path/to/directory", method = "DOVs", recursive = TRUE)
} # }
```
