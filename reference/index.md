# Package index

## Functions for data formatting

Essential functionality for formatting stereo-video annotation

- [`clean_names()`](https://globalarchivemanual.github.io/CheckEM/reference/clean_names.md)
  : Standardise Column Names in a Data Frame
- [`create_cti()`](https://globalarchivemanual.github.io/CheckEM/reference/create_cti.md)
  : Create Community Temperature Index as per Day et al. 2018
- [`standardize_tibble()`](https://globalarchivemanual.github.io/CheckEM/reference/standardize_tibble.md)
  : Standardize Column Names and Types in a Tibble
- [`has_special_characters()`](https://globalarchivemanual.github.io/CheckEM/reference/has_special_characters.md)
  : Check if a String Contains Special Characters
- [`read_metadata()`](https://globalarchivemanual.github.io/CheckEM/reference/read_metadata.md)
  : Read Metadata
- [`read_points()`](https://globalarchivemanual.github.io/CheckEM/reference/read_points.md)
  : Read Point Data from EventMeasure files (\_Points.txt)
- [`read_counts()`](https://globalarchivemanual.github.io/CheckEM/reference/read_counts.md)
  : Read Count Data from Generic Count Data (\_Count.csv)
- [`read_TM()`](https://globalarchivemanual.github.io/CheckEM/reference/read_TM.md)
  : Read .txt Exports from SeaGIS TransectMeasure
- [`find_lw()`](https://globalarchivemanual.github.io/CheckEM/reference/find_lw.md)
  : Retrieve Bayesian Length-Weight Equation from FishBase
- [`find_tl()`](https://globalarchivemanual.github.io/CheckEM/reference/find_tl.md)
  : Retrieve Trophic Level and Standard Error from FishBase
- [`plot_transformations()`](https://globalarchivemanual.github.io/CheckEM/reference/plot_transformations.md)
  : Plot Basic Transformations of Predictor Variables for GAM Modelling
- [`read_em_length()`](https://globalarchivemanual.github.io/CheckEM/reference/read_em_length.md)
  : Read EventMeasure length and 3D point data (\_3DPoints.txt &
  \_Lengths.txt)
- [`read_gen_length()`](https://globalarchivemanual.github.io/CheckEM/reference/read_gen_length.md)
  : Read "Generic" length data
- [`read_periods()`](https://globalarchivemanual.github.io/CheckEM/reference/read_periods.md)
  : Read Period Data from EventMeasure files (\_Period.txt)
- [`expand_life_history()`](https://globalarchivemanual.github.io/CheckEM/reference/expand_life_history.md)
  : Expand Life-History Data Frame for Species and Regions
- [`create_min_max()`](https://globalarchivemanual.github.io/CheckEM/reference/create_min_max.md)
  : Calculate Minimum and Maximum Size Limits for Fish Species
- [`load_rds_from_github()`](https://globalarchivemanual.github.io/CheckEM/reference/load_rds_from_github.md)
  : Load an RDS File from a Private GitHub Repository
- [`add_zeros_count()`](https://globalarchivemanual.github.io/CheckEM/reference/add_zeros_count.md)
  : Add in zeros for deployments not present in the count data
- [`add_zeros_length()`](https://globalarchivemanual.github.io/CheckEM/reference/add_zeros_length.md)
  : Add in zeros for deployments not present in the length data

## Shiny App

Functions for shiny app

- [`runCheckEM()`](https://globalarchivemanual.github.io/CheckEM/reference/runCheckEM.md)
  : Run the CheckEM Shiny App Locally

## GlobalArchive API

Essential functionality for accessing data through the GlobalArchive API

- [`ga_api_set_token()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_set_token.md)
  : Set GlobalArchive API Token
- [`ga_api_all_data()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_all_data.md)
  : Retrieve all Data from a synthesis using the GlobalArchive API
- [`ga_api_count()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_count.md)
  : Retrieve Count Data from the GlobalArchive API
- [`ga_api_habitat()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_habitat.md)
  : Retrieve Habitat Data from the GlobalArchive API
- [`ga_api_relief()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_relief.md)
  : Retrieve Relief Data from the GlobalArchive API
- [`ga_api_length()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_length.md)
  : Retrieve Length Data from the GlobalArchive API
- [`ga_api_metadata()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_metadata.md)
  : Retrieve Metadata from the GlobalArchive API
- [`ga_api_species_list()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_species_list.md)
  : Retrieve Species List from the Global Archive API
- [`ga_api_benthic_list()`](https://globalarchivemanual.github.io/CheckEM/reference/ga_api_benthic_list.md)
  : Retrieve Benthic Species List from the Global Archive API

## Investigating maximum sizes of fish

Functions to plot and investigate size frequency and maximum sizes of
fish

- [`plot_species_hdi()`](https://globalarchivemanual.github.io/CheckEM/reference/plot_species_hdi.md)
  : Plot length frequency histogram with High Density Interval
- [`hdi_table()`](https://globalarchivemanual.github.io/CheckEM/reference/hdi_table.md)
  : hdi_table
