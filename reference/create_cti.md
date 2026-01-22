# Create Community Temperature Index as per Day et al. 2018

This function creates a new dataframe containing Community Temperature
Index (CTI) for fish species from abundance data. The function is
designed to work with data extracted from GlobalArchive using the
'ga_api_count' function, and processed using provided scripts. The input
dataframe must contain columns for 'campaignid', 'sample', 'count',
'family', 'genus' and 'species'. CTI is calculated

## Usage

``` r
create_cti(data, life_history = CheckEM::australia_life_history)
```

## Arguments

- data:

  A dataframe with columns for 'campaignid', 'sample', 'count',
  'family', 'genus' and 'species'

- life_history:

  A dataframe containing thermal niche data for fish species. For an
  example dataframe see CheckEM::australia_life_history.

## Value

A data frame containing campaignid, sample and cti (the value for
Community Temperature Index). Note that this dataframe may have less
rows than your number of samples, as you may have samples that have no
fish with valid thermal niche data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch metadata for a specific synthesis
count <- readRDS(paste0("data/geographe/raw/", name, "_complete_count.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
  glimpse()
  
cti <- create_cti(data = count) %>%
  dplyr::rename(number = cti) %>%
  # Rename the column to number to match with other dataframes for FSSgam modelling
  dplyr::mutate(response = "cti") %>%
  glimpse()
  
} # }
```
