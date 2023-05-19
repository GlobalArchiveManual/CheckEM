# Script to turn old GA format of date and time into UTC
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(lutz)
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

# Read in data ----
# Directory contains multiple metadata files ----
data.dir <- here::here("data/examples for synthesis")

metadata <- list.files(path = data.dir, recursive = T, pattern = "_Metadata.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ read_files_csv(.)) %>%
  dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Metadata.csv" = ""))) %>%
  dplyr::mutate(year = as.numeric(substr(date, 1, 4))) %>%
  dplyr::mutate(month = substr(date, 5, 6)) %>%
  dplyr::mutate(day = substr(date, 7, 8)) %>%
  dplyr::mutate(latitude = as.numeric(latitude)) %>%
  dplyr::mutate(longitude = as.numeric(longitude)) %>%
  dplyr::mutate(timezone = tz_lookup_coords(lat = latitude, lon = longitude, method = "fast"))%>% # 'fast' method can cause inaccuracies in time zones near boundaries away from populated ares. Use the 'accurate' method if accuracy is more important than speed.
  dplyr::mutate(time = if_else(str_length(time) %in% 4, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 5, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = if_else(str_length(time) %in% 7, paste0("0", time), time)) %>%
  dplyr::mutate(date.time = paste0(year, "-", month, "-", day, " ", time)) %>%
  dplyr::mutate(local.date.time = map2(.x = date.time, .y = timezone, .f = function(x, y) {ymd_hms(time = x, tz = y)})) %>% 
  unnest(local.date.time) %>%
  dplyr::mutate(utc.date.time = with_tz(local.date.time, tz = "UTC")) %>%
  glimpse()

# Check these all make sense
unique(metadata$year) %>% sort()
unique(metadata$month) %>% sort() # shouldn't be above 12
unique(metadata$day) %>% sort() # shouldn't be greater than 31
unique(metadata$time) %>% sort() # time needs to be in 24HR hh:mm:ss
unique(metadata$local.date.time) %>% sort()
unique(metadata$utc.date.time) %>% sort()

# Select only the columns to keep
metadata.utc <- metadata %>%
  dplyr::select(-c(date, time, method, year, month, day, timezone, date.time, local.date.time)) %>% # remove campaignid in a minute
  dplyr::rename(date.time.utc = utc.date.time) %>%
  dplyr::mutate(date.time.utc = paste0(str_replace_all(.$date.time.utc, " ", "T"), "Z"))

names(metadata.utc) <- ga.capitalise(names(metadata.utc))

for (i in unique(metadata.utc$Campaignid)) {
  
  print(i)
  
  dat <- metadata.utc %>% 
    dplyr::filter(Campaignid %in% i) %>% 
    dplyr::select(-c(Campaignid)) %>%
    glimpse()
  
  write.csv(dat, paste0("data/new metadata/", i, "_Metadata.csv"), row.names = FALSE)
  
}
