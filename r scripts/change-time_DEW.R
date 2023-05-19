# Script to turn old GA format of date and time into UTC
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(GlobalArchive)

# Function to read in the data from csv files
read_files_csv <- function(flnm, data_dir = here::here("data")) {
  flnm %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) |>
    dplyr::mutate(folder.structure = stringr::str_replace_all(flnm, paste(data_dir, "/", sep = ""), "")) |>
    tidyr::separate(folder.structure, into = c("method", "campaignid"), sep = "/", extra = "drop", fill = "right") |>
    GlobalArchive::ga.clean.names()
}

time.zone <- "+10:30"

# Read in data ----
# Directory contains multiple metadata files ----
# Change data directory to where your data is saved
data.dir <- here::here("data/DEW")

# If Date is in YYYYMMDD then:
metadata <- list.files(path = data.dir, recursive = F, pattern = "_Metadata.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ read_files_csv(.)) %>%
  dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Metadata.csv" = ""))) %>%
  dplyr::mutate(year = as.numeric(substr(date, 1, 4))) %>%
  dplyr::mutate(month = substr(date, 5, 6)) %>%
  dplyr::mutate(day = substr(date, 7, 8)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 4, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 5, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 7, paste0("0", time), time)) %>%
  dplyr::mutate(date.time = paste0(year, "-", month, "-", day, "T", time, time.zone)) %>%
  glimpse()

# Check these all make sense
unique(metadata$year) %>% sort()
unique(metadata$month) %>% sort() # shouldn't be above 12
unique(metadata$day) %>% sort() # shouldn't be greater than 31
unique(metadata$time) %>% sort() # time needs to be in 24HR hh:mm:ss

# If Date is in DDMMYYYY then:
metadata <- list.files(path = data.dir, recursive = F, pattern = "_Metadata.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ read_files_csv(.)) %>%
  dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Metadata.csv" = ""))) %>%
  dplyr::mutate(year = as.numeric(substr(date, 5, 8))) %>%
  dplyr::mutate(month = substr(date, 3, 4)) %>%
  dplyr::mutate(day = substr(date, 1, 2)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 4, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 5, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 7, paste0("0", time), time)) %>%
  dplyr::mutate(date.time = paste0(year, "-", month, "-", day, "T", time, time.zone)) %>%
  glimpse()

# Check these all make sense
unique(metadata$year) %>% sort()
unique(metadata$month) %>% sort() # shouldn't be above 12
unique(metadata$day) %>% sort() # shouldn't be greater than 31
unique(metadata$time) %>% sort() # time needs to be in 24HR hh:mm:ss

names(metadata) <- ga.capitalise(names(metadata))

names(metadata)

for (i in unique(metadata$Campaignid)) {
  
  print(i)
  
  dat <- metadata %>% 
    dplyr::filter(Campaignid %in% i) %>% 
    dplyr::select(-c(Campaignid, Date, Time, Year, Month, Day)) %>%
    glimpse()
  
  write.csv(dat, paste0("data/", i, "_Metadata.csv"), row.names = FALSE)
  
}
