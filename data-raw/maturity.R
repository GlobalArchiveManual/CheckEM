## Code to prepare `maturity` dataset ----
# This dataset contains length of maturity data for common West Australian recreationally and commercially targeted species
# The vast majority of this data has been obtained from Department of Fisheries public reports
# See here - https://www.fish.wa.gov.au/Documents/management_papers/fmp280.pdf

library(googlesheets4)
library(usethis)
library(tidyverse)
library(CheckEM)

url <- "https://docs.google.com/spreadsheets/d/176genWqd_pc3NDVQtP2CmfMh66Ui6uwWrLoJU1Ny_qw/edit?usp=sharing"

maturity <- googlesheets4::read_sheet(url, sheet = "fisheries Lm") %>%
  CheckEM::clean_names() %>%
  dplyr::select(family, genus, species, sex, l50_mm, measurement_type, marine_region,
                hermaphrodite, source) %>%
  glimpse()

usethis::use_data(maturity, overwrite = TRUE)
