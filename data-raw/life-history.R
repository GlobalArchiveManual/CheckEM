## Code to prepare australia and global life history datasets ----
library(usethis)
library(tidyverse)
library(CheckEM)

australia_life_history <- readRDS("annotation-schema/data/tidy/simple-life-history.RDS")

usethis::use_data(australia_life_history, overwrite = TRUE)

global_life_history <- readRDS("annotation-schema/data/tidy/global_fish.life.history.RDS")

usethis::use_data(global_life_history, overwrite = TRUE)
