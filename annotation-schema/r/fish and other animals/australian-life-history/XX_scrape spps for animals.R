# CAAB categories to scrape

# Aves	40 (can get from caab codes)
# Ctenophora	12 (can get from caab codes)
# Mammalia	41 (can get from caab codes)
# Reptilia	39 (can get from caab codes)
# Mollusca - Non-Gastropod groups	23 (over 1000 total, 782 spps)
# Crustacea - non-Malacostraca	27 (over 1000, 629 spps)
# Echinodermata	25 (over 1000, 693 spps)


# Crustacea - Malacostraca	28 (over 1000, over 1000 spps)
# Mollusca - Gastropoda	24 (over 1000, over 1000 spps)
# Pisces	37 (over 1000)



# This script uses the CAAB code cateogires to scrape all the caab codes within that category

# Load libraries required
library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(CheckEM)
library(stringr)
library(GlobalArchive)

caab_og <- read_excel("annotation-schema/data/raw/caab_dump_latest.xlsx")

aves_caabs <- caab_og %>%
  filter(SPCODE > 40000000) %>%
  filter(SPCODE < 40999999)

crustacea1_caabs <- caab_og %>%
  filter(SPCODE > 28000000) %>%
  filter(SPCODE < 28999999)

crustacea2_caabs <- caab_og %>%
  filter(SPCODE > 27000000) %>%
  filter(SPCODE < 27999999)

ctenophora_caabs <- caab_og %>%
  filter(SPCODE > 12000000) %>%
  filter(SPCODE < 12999999)

echinodermata_caabs <- caab_og %>%
  filter(SPCODE > 25000000) %>%
  filter(SPCODE < 25999999)

mammalia_caabs <- caab_og %>%
  filter(SPCODE > 41000000) %>%
  filter(SPCODE < 41999999)

mollusca1_caabs <- caab_og %>%
  filter(SPCODE > 24000000) %>%
  filter(SPCODE < 24999999)

mollusca2_caabs <- caab_og %>%
  filter(SPCODE > 23000000) %>%
  filter(SPCODE < 23999999)

pisces_caabs <- caab_og %>%
  filter(SPCODE > 37000000) %>%
  filter(SPCODE < 37999999)

reptilia_caabs <- caab_og %>%
  filter(SPCODE > 39000000) %>%
  filter(SPCODE < 39999999)


spps1 <- read.delim("annotation-schema/data/raw/spps/caab_extract (1).csv")
spps2 <- read.delim("annotation-schema/data/raw/spps/caab_extract (2).csv")
spps3 <- read.delim("annotation-schema/data/raw/spps/caab_extract (3).csv")
spps4 <- read.delim("annotation-schema/data/raw/spps/caab_extract (4).csv")
spps5 <- read.delim("annotation-schema/data/raw/spps/caab_extract (5).csv")
spps6 <- read.delim("annotation-schema/data/raw/spps/caab_extract (6).csv")
spps7 <- read.delim("annotation-schema/data/raw/spps/caab_extract (7).csv")
spps8 <- read.delim("annotation-schema/data/raw/spps/caab_extract.csv")

spps <- bind_rows(spps1, spps2, spps3, spps4, spps5, spps6, spps7, spps8) %>%
  clean_names() %>%
  separate(scientificname, into = c("genus", "species"), extra = "merge") %>%
  dplyr::filter(species %in% c("spp.", "", NA)) %>%
  dplyr::mutate(family = if_else(species %in% NA, genus, NA)) %>%
  dplyr::mutate(genus = if_else(!species %in% "spp.", "Unknown", genus)) %>%
  dplyr::select(caab, family, genus, species, commonname) %>%
  dplyr::mutate(species = str_replace_all(species, "\\.", ""))
  


# Codes to scrape
aves <- min(aves_caabs$SPCODE):max(aves_caabs$SPCODE)
crustacea1 <- min(crustacea1_caabs$SPCODE):max(crustacea1_caabs$SPCODE)
crustacea2 <- min(crustacea2_caabs$SPCODE):max(crustacea2_caabs$SPCODE)
ctenophora <- min(ctenophora_caabs$SPCODE):max(ctenophora_caabs$SPCODE)
echinodermata <- min(echinodermata_caabs$SPCODE):max(echinodermata_caabs$SPCODE)
mammalia <- min(mammalia_caabs$SPCODE):max(mammalia_caabs$SPCODE)
mollusca1 <- min(mollusca1_caabs$SPCODE):max(mollusca1_caabs$SPCODE)
mollusca2 <- min(mollusca2_caabs$SPCODE):max(mollusca2_caabs$SPCODE)
pisces <- min(pisces_caabs$SPCODE):max(pisces_caabs$SPCODE)
reptilia <- min(reptilia_caabs$SPCODE):max(reptilia_caabs$SPCODE)

codes <- c(crustacea1, mollusca1)
length(codes)


# # # Scrape information from the website by looping through all CAAB codes (this takes roughly an hour)
# # # Remove the hashes in this section to run again
# temp.info <- data.frame()
# 
# system.time(
#   # Test with 100 first
#   for (caab.code in unique(spps$caab)) {
#     
#     url <- paste0("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", caab.code)
#     
#     # Read the webpage content as lines
#     try(webpage <- readLines(url))
#     
#     # Send a GET request to the webpage and parse the HTML content
#     try(page <- read_html(url))
#     
#     # Find the second table on the webpage
#     try(table <- page %>% html_nodes("table"))
#     
#     # Convert the table to a data frame
#     try(table_df <- html_table(table, fill = TRUE))
#     
#     try(caab.info <- table_df[[1]] %>%
#           pivot_wider(names_from = X1, values_from = X2) %>%
#           ga.clean.names())
#     
#     try(names(caab.info) <- (sub("[.]$", "", names(caab.info)))) # Remove full stops from end of names
#     names(caab.info)
#     
#     try(species.info <- table_df[[2]] %>%
#           dplyr::select(X1, X2) %>%
#           dplyr::filter(X1 %in% c("Family:",
#                                   "Standard Name (AS5300):",
#                                   "Standard Name:",
#                                   "Synonyms:",
#                                   "WoRMS")) %>%
#           pivot_wider(names_from = X1, values_from = X2) %>%
#           ga.clean.names())
#     
#     try(names(species.info) <- (sub("[.]$", "", names(species.info)))) # Remove full stops from end of names
#     names(species.info)
#     
#     try(info <- bind_cols(caab.info, species.info))
#     
#     try(info$caab <- caab.code)
#     
#     try(temp.info <- bind_rows(temp.info, info))
#     
#     message(paste("up to ", nrow(temp.info), "of ", length(unique(spps$caab))))
#     
#   }
# )



saveRDS(temp.info, "annotation-schema/data/staging/scraped-caab_spps.RDS")
Sys.time() # started at 8:30

info <- readRDS("annotation-schema/data/staging/scraped-caab_spps.RDS") %>%
  clean_names()

# Remove spaces from CAAB
info$caab_code <- gsub(" \nshow as JSON", "", info$caab_code)
info$caab_code <- gsub("[^[:alnum:]]", "", info$caab_code)
info$caab_code <- gsub("showasJSON", "", info$caab_code)

# Need to split name into scientific and authority
clean_info <- info %>%
  tidyr::separate(scientific_nameand_authority, into = c("scientific_name"), sep = " \n", remove = FALSE) %>%
  dplyr::mutate(scientific_name = trimws(scientific_name)) %>%
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, c("^(\\S+\\s+\\S+)\\s+" = "\\1"))) %>%
  tidyr::separate(scientific_name, into = c("genus", "species"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(family = str_replace_all(family, c('[[:digit:]]+' = "", "  " = "", "Rock Whitings" = "Labridae", "\\(|\\)" = ""))) %>%
  tidyr::separate(family, into = c("family"), sep = " ") %>%
  dplyr::mutate(common_name = if_else(is.na(standard_name_as5300), standard_name, standard_name_as5300)) %>%
  dplyr::mutate(worms = str_replace_all(worms, c("urn:lsid:marinespecies.org:taxname:" = ""))) %>%
  tidyr::separate(worms, into = c("worms_id", "worms_valid"), sep = "\n") %>%
  dplyr::select(caab_code, family, genus, species, scientific_name, common_name, worms_id, worms_valid, synonyms) %>%
  dplyr::mutate(species = str_replace_all(species, "\\.", "")) %>%
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, "\\.", "")) %>%
  dplyr::mutate(family = if_else(genus %in% "Aipysurus", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Abantennarius", "Antennariidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Acanthopleura", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Chiton", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Ctenophora", "Fragilariaceae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Ctenophora", "Fragilariaceae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Dampierosa", "Synanceiidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Dasyscopelus", "Myctophidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Emydocephalus", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Ephalophis", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Hippopus", "Cardiidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Hydrelaps", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Hydrophis", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Laticauda", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Liolophura", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Lucilina", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Lythrichthys", "Setarchidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Microcephalophis", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Mucrosquama", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Onithochiton", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Parahydrophis", "Elapidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Perlophiura", "Ophiosphalmidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Pyronotanthias", "Serranidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Rhyssoplax", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Squamopleura", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Sypharochiton", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Tegulaplax", "Chitonidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Waldo", "Lasaeidae", family)) %>%
  dplyr::filter(!genus %in% "Dasyatis") %>%
  dplyr::filter(!is.na(family)) %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  dplyr::mutate(scientific_name = paste(genus, species)) %>%
  dplyr::mutate(common_name = tolower(str_remove_all(common_name, "[[:punct:]]"))) %>%
  dplyr::mutate(worms_id = parse_number(worms_id)) %>%
  glimpse()

  
  
  
unique(clean_info$scientific_name)
unique(clean_info$synonyms) # a mess - will need to do separately
unique(clean_info$family)# looks mostly ok
unique(clean_info$genus)# looks mostly ok
unique(clean_info$species)# looks mostly ok
unique(clean_info$worms_id)# looks mostly ok
unique(clean_info$worms_valid)

# Have added the familys manually
familys_not_containing_dae <- clean_info[!grepl("dae", clean_info$family), ]

# Need higher taxanomic levels

caab_classes <- read_excel("annotation-schema/data/raw/caab_dump_latest.xlsx") %>%
  clean_names() %>%
  distinct(family, kingdom, phylum, class, order_name) %>%
  dplyr::filter(!is.na(family))

names(clean_info)

final_caab_codes <- clean_info %>%
  dplyr::select(-c(synonyms, worms_valid)) %>%
  left_join(caab_classes) %>%
  distinct() %>%
  dplyr::rename(caab = caab_code) %>%
  dplyr::mutate(caab = as.character(caab))

blank_class <- final_caab_codes %>%
  filter(is.na(class)) %>%
  distinct(family)

saveRDS(final_caab_codes, "annotation-schema/data/staging/australia_fish_caab-codes_spps.RDS")
