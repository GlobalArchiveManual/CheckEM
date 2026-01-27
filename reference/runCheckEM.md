# Run the CheckEM Shiny App Locally

Launches the CheckEM Shiny app in the default web browser. Optionally
installs required packages if they are missing.

## Usage

``` r
runCheckEM(
  install_missing = TRUE,
  ask = interactive(),
  repos = getOption("repos")
)
```

## Arguments

- install_missing:

  Logical. If TRUE, install missing packages automatically.

- ask:

  Logical. If TRUE, ask before installing packages.

- repos:

  Character. CRAN repo to use.

## Value

Launches the CheckEM Shiny app.

## Examples

``` r
if (FALSE) { # \dontrun{
CheckEM::runCheckEM()
} # }
```
