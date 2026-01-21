# library(tidyverse)
# devtools::load_all()
# 
# read_old_data <- function(flnm) {
#   flnm %>%
#     read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
#                colClasses = "character", na.strings = "") %>%
#     dplyr::mutate(campaignid = basename(flnm))
# }
# 
# points <- list.files(path = "1. Example R workflows (scripts to download)/data/old raw/",    
#                      recursive = F,
#                      pattern = "_Dot Point Measurements.txt",
#                      full.names = T) %>%
#   purrr::map(~read_old_data(.)) %>%
#   purrr::list_rbind() %>%
#   clean_names() %>%
#   dplyr::select(campaignid, filename, broad, morphology, type, relief) %>%
#   dplyr::mutate(relief_file = if_else(str_detect(campaignid, "relief|Relief"), TRUE, FALSE),
#                 relief_score = str_extract(relief, "[:digit:]{1}"),
#                 sample = str_extract(filename, )) %>%
#   glimpse()
