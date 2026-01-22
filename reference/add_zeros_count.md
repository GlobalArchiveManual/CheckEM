# Add in zeros for deployments not present in the count data

Add in zeros for deployments not present in the count data

## Usage

``` r
add_zeros_count(count_data, metadata)
```

## Arguments

- count_data:

  A data frame of species counts.

- metadata:

  A data frame containing deployment-level metadata.

## Value

A dataframe with a row for every species in the count data, in every
sample in the metadata
