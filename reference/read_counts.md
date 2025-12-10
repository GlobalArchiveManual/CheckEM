# Read Count Data from Generic Count Data (\_Count.csv)

This function reads and processes point count data from Generic files in
either CSV or TXT format. It can handle both BRUVs (Baited Remote
Underwater Video Systems) and DOVs (Diver Operated Video) methods,
ensuring that the data is correctly cleaned and formatted for further
analysis.

## Usage

``` r
read_counts(dir, method = "BRUVs", recursive = FALSE)
```

## Arguments

- dir:

  A character string specifying the directory containing the count
  files.

- method:

  A character string specifying the method used: either `"BRUVs"` or
  `"DOVs"`. Defaults to `"BRUVs"`.

- recursive:

  A logical value indicating whether to read files recursively from the
  directory. Defaults to `FALSE`.

## Value

A data frame containing the cleaned and formatted count data from the
specified files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read all count files from the directory for BRUVs method
counts_data <- read_counts(dir = "path/to/directory", method = "BRUVs", recursive = TRUE)

# Read all count files from the directory for DOVs method
counts_data <- read_counts(dir = "path/to/directory", method = "DOVs", recursive = TRUE)
} # }
```
