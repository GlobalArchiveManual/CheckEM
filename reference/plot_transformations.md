# Plot Basic Transformations of Predictor Variables for GAM Modelling

This function generates diagnostic plots to visualise the distribution
of predictor variables and the effects of basic transformations (square
root and log transformations) on these variables. It creates scatter
plots and histograms for the raw, square root, and log-transformed data.
The function is useful for exploring the suitability of different
transformations before fitting a Generalized Additive Model (GAM).

## Usage

``` r
plot_transformations(pred.vars, dat)
```

## Arguments

- pred.vars:

  A character vector of predictor variable names to be plotted.

- dat:

  A data frame containing the predictor variables and a grouping
  variable, either 'opcode' or 'sample', which is used for the scatter
  plots.

## Value

The function generates and prints diagnostic plots but does not return a
value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat' is a data frame containing the predictor variables and 'opcode' or 'sample' column
plot_transformations(pred.vars = c("var1", "var2"), dat = dat)
} # }
```
