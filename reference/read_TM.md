# Read .txt Exports from SeaGIS TransectMeasure

This function reads and processes .txt files exported from SeaGIS
TransectMeasure located in the specified directory. The files contain
data from dot point measurements, which are read into a single dataframe
with a column for the campaign ID and a specified sample type (either
"opcode" or "period").

## Usage

``` r
read_TM(dir, sample)
```

## Arguments

- dir:

  The directory where the .txt files are saved.

- sample:

  A character string indicating the sample type; either "opcode" or
  "period".

## Value

A dataframe containing combined data from all .txt files in the
specified directory, including a column for the sample type.

## Examples

``` r
if (FALSE) { # \dontrun{
# To read .txt files with sample recorded as "opcode"
data <- read_TM("path/to/directory", sample = "opcode")

# To read .txt files with sample recorded as "period"
data <- read_TM("path/to/directory", sample = "period")
} # }
```
