# Load an RDS File from a Private GitHub Repository

Load an RDS File from a Private GitHub Repository

## Usage

``` r
load_rds_from_github(url, pat = NULL)
```

## Arguments

- url:

  Raw URL to the file (raw.githubusercontent.com/...)

- pat:

  GitHub Personal Access Token. If NULL, tries env vars then gitcreds.

## Value

R object, or NULL if not available (non-interactive w/out PAT) / access
denied.

## Examples

``` r
if (FALSE) { # \dontrun{
rds_url <- "https://raw.githubusercontent.com/OWNER/REPO/branch/path/file.RDS"
data <- load_rds_from_github(rds_url)
} # }
```
