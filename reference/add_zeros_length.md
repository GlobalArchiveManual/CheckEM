# Add in zeros for deployments not present in the length data

Add in zeros for deployments not present in the length data

## Usage

``` r
add_zeros_length(length_data, metadata)
```

## Arguments

- length_data:

  A data frame of species lengths.

- metadata:

  A data frame containing deployment-level metadata.

## Value

A dataframe with a row for every species in the length data, in every
sample in the metadata
