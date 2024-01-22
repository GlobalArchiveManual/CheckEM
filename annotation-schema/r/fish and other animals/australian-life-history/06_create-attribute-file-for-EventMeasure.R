# This script turns the generated life history information into a text file 
# The text file can be used in EventMeasure as an attribute file

# load libraries required
library(tidyverse)

# Create a dataframe of some extra fish that are missing from the life history information
extras <- data.frame(FAMILY = c("Unknown", "Larval", "SUS", "Baitfish", "Sparidae"),
                     GENUS = c("Unknown", "Larval", "SUS", "Baitfish", "Dentex"),
                     SPECIES = c("Unknown", "Larval", "SUS", "Baitfish", "carpenteri"),
                     'CAAB CODE' = c("0", "1", "2", "3", "4")) %>%
  dplyr::rename("CAAB CODE" = CAAB.CODE)

lh <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS") %>%
  glimpse()
  
all <- lh %>%  
  dplyr::rename("CAAB CODE" = caab, 
                FAMILY = family,
                GENUS = genus, 
                SPECIES = species) %>%
  dplyr::select(FAMILY, GENUS, SPECIES, "CAAB CODE") %>%
  dplyr::filter(!SPECIES %in% "spp") %>% # EventMeasure creates spp, sp1 etc automatically
  dplyr::bind_rows(., extras) %>%
  distinct()

date <- str_sub(str_remove_all(Sys.time(), "[^[:alnum:] ]"), 1, 8)

write_tsv(all, paste0("annotation-schema/output/fish/eventMeasure-attribute-files/CAAB_", date, ".txt"))
