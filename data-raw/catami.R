## Code to prepare `catami` dataset ----
# This dataset contains a tidy version of the CATAMI Classification Scheme for scoring marine biota and substrata in underwater imagery
# See here - https://catami.org/

library(googlesheets4)
library(usethis)
library(tidyverse)
library(CheckEM)

url <- "https://docs.google.com/spreadsheets/d/1tcvHnD8LtPmjro8eOMdOS6k6HKdND86mMAOk6AS_gfc/edit?usp=sharing"

catami <- googlesheets4::read_sheet(url, sheet = "code crosswalk") %>%
  CheckEM::clean_names() %>%
  dplyr::select(starts_with("level"), family, genus, species, qualifiers, caab_code) %>%
  glimpse()

usethis::use_data(catami, overwrite = TRUE)
