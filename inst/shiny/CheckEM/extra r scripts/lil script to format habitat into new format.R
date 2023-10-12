# An example script to format habitat into GA acceptable format ----
library(tidyverse)
library(googlesheets4)
library(GlobalArchive)

# Google Sheets Auth
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()
# 1

# read in catami/CAAB key ----
url <- "https://docs.google.com/spreadsheets/d/1tcvHnD8LtPmjro8eOMdOS6k6HKdND86mMAOk6AS_gfc/edit#gid=1972721984"

code.key <- read_sheet(url, sheet = "code crosswalk") %>%
  ga.clean.names() %>%
  dplyr::mutate(caab_code = as.integer(caab_code)) %>%
  dplyr::mutate(caab_code = as.character(caab_code))
1

unique(code.key$caab_code)

# read in example dot points measurements ----
data <- read_tsv("data/habitat examples/CHECKEMTEST_2021-05_Abrolhos_stereo-BRUVs_Forwards_Dot Point Measurements.txt", skip = 4) %>%
  ga.clean.names() %>%
  dplyr::mutate(sample = str_replace_all(.$filename,c(".png" = "", ".jpg" = "", ".JPG" = "", ".PNG" = ""))) %>%
  dplyr::select(sample, image.row, image.col, broad, morphology, type, caab_code) %>%
  filter(!broad %in% c("Invertebrate Complex")) %>%
  # dplyr::mutate(caab_code = as.numeric(caab_code, scipen=999)) %>%
  glimpse()

relief <- read_tsv("data/habitat examples/2021-05_Abrolhos_stereo-BRUVs_Forwards_Relief_Dot Point Measurements.txt", skip = 4) %>%
  ga.clean.names() %>%
  dplyr::mutate(sample = str_replace_all(.$filename,c(".png" = "", ".jpg" = "", ".JPG" = "", ".PNG" = ""))) %>%
  dplyr::select(sample, image.row, image.col, relief) %>%
  glimpse()

names(data)

# Step 1 - format habitat data into count
unique(data$broad)

count <- data %>%
  filter(!broad %in% c("", NA, "Unknown", "Open.Water", "Open Water")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample, broad, morphology, type, caab_code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup()

relief.count <- relief %>%
  filter(!relief %in% c("", NA, "Unknown")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample, relief) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup()
  
# Step 2 - add catami codes using CAAB codes from LH sheet
combined <- left_join(count, code.key)

unique(relief.count$relief) %>% sort()

relief.caab <- relief.count %>%
  dplyr::mutate(caab_code = case_when(
    relief %in% ".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope."                        ~ "82003001",
    relief %in% ".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope." ~ "82003003",
    relief %in% ".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope."          ~ "82003004",
    relief %in% ".3. Good relief structure with some overhangs. >45 substrate slope."                             ~ "82003006",
    relief %in% ".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope."         ~ "82003007",
  )) %>%
  left_join(., code.key)
  

# There are a few with NAs (CAAB code isn't the same), Need to find out where the error is from
test <-anti_join(data, code.key)

unique(test$caab_code)
unique(code.key$caab_code)

test <- combined %>%
  filter(is.na(level_1)) %>%
  distinct(broad, morphology, type, caab_code)

names(combined)

tidy <- combined %>%
  dplyr::mutate(campaignid = "2021-05_Abrolhos_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, caab_code, level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, family,genus, species, count) %>%
  dplyr::filter(!is.na(level_1)) # this should not do anything if the data is ok and all the codes are match up (it does get rid of them atm)

tidy.relief <- relief.caab %>%
  dplyr::mutate(campaignid = "2021-05_Abrolhos_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, caab_code, level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, family, genus, species, count) %>%
  dplyr::filter(!is.na(level_1)) 
  
# If annotater has included qualifiers how will this be included in the final data.

write.csv(tidy, "2021-05_Abrolhos_stereo-BRUVs_benthos.csv", na = "", row.names = FALSE)
write.csv(tidy.relief, "2021-05_Abrolhos_stereo-BRUVs_relief.csv", na = "", row.names = FALSE)
