library(tidyverse)
library(googlesheets4)
library(openxlsx)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"
lh <- read_sheet(url)

saveRDS(lh, "data/life.history.RDS")

synonyms <- read_sheet(url, sheet = 2) %>% distinct()
saveRDS(synonyms, "data/synonyms.RDS")

fam.common.names <- read_sheet(url, sheet = 3)
lumped.common.names <- read_sheet(url, sheet = 4)

# Get columns to keep from original life history
keep <- lh %>%
  dplyr::select(Family, Genus, Species, 
                
                RLS.trophic.level,
                RLS.trophic.breadth,
                RLS.trophic.group,
                RLS.water.column,
                RLS.substrate.type,
                RLS.complexity,
                RLS.night.day,
                RLS.gregariousness,
                RLS.thermal.niche,
                RLS.vulnerability,
                
                Fishing.mortality,
                Fishing.type,
                Fishing.intensity,
                
                MinLegal.NT,
                MaxLegal.NT,
                Bag.NT,
                MinLegal.WA,
                MaxLegal.WA,
                Bag.WA,
                MinLegal.QLD,
                MaxLegal.QLD,
                Bag.QLD,
                MinLegal.NSW,
                MaxLegal.NSW,
                Bag.NSW,
                MinLegal.Vic,
                MaxLegal.Vic,
                Bag.Vic,
                MinLegal.SA,
                MaxLegal.SA,
                Bag.SA,
                MinLegal.Tas,
                MaxLegal.Tas,
                Bag.Tas,
                
                EPBC.Threat.Status) %>%
  
  dplyr::rename(family = Family,
                genus = Genus,
                species = Species,
                
                rls_trophic_level = RLS.trophic.level,
                rls_trophic_breadth = RLS.trophic.breadth,
                rls_trophic_group = RLS.trophic.group,
                rls_water_column = RLS.water.column,
                rls_substrate_type = RLS.substrate.type,
                rls_complexity = RLS.complexity,
                rls_night_day = RLS.night.day,
                rls_gregariousness = RLS.gregariousness,
                rls_thermal_niche = RLS.thermal.niche,
                rls_vulnerability = RLS.vulnerability,
                
                fishing_mortality = Fishing.mortality,
                fishing_type = Fishing.type,
                fishing_intensity = Fishing.intensity,
                
                min_legal_nt = MinLegal.NT,
                max_legal_nt = MaxLegal.NT,
                bag_nt = Bag.NT,
                min_legal_wa = MinLegal.WA,
                max_legal_wa = MaxLegal.WA,
                bag_wa = Bag.WA,
                min_legal_qld = MinLegal.QLD,
                max_legal_qld = MaxLegal.QLD,
                bag_qld = Bag.QLD,
                min_legal_nsw = MinLegal.NSW,
                max_legal_nsw = MaxLegal.NSW,
                bag_nsw = Bag.NSW,
                min_legal_vic = MinLegal.Vic,
                max_legal_vic = MaxLegal.Vic,
                bag_vic = Bag.Vic,
                min_legal_sa = MinLegal.SA,
                max_legal_sa = MaxLegal.SA,
                bag_sa = Bag.SA,
                min_legal_tas = MinLegal.Tas,
                max_legal_tas = MaxLegal.Tas,
                bag_tas = Bag.Tas,
                
                epbc_threat_status = EPBC.Threat.Status)
               
names(keep)


# Other data to read in from script 1 & 
caab.regions <- readRDS("data/caab.with.regions.RDS") %>%
  dplyr::filter(!is.na(caab_code)) %>%
  dplyr::mutate(caab_code = as.character(caab_code))

fishbase <- readRDS("data/fishbase.and.iucn.RDS") %>%
  dplyr::filter(!(caab.scientific %in% "Epigonus macrops" & speccode %in% "14365"))%>%
  dplyr::mutate(caab_code = as.character(caab_code)) 

caab.scraped <- readRDS("data/caab_scraped_codes_common-names.RDS") %>%
  dplyr::mutate(family = str_replace_all(.$family, "[^[:alnum:]]", "")) %>%
  dplyr::mutate(species = str_replace_all(.$species, "cffilamentosa", "filamentosa")) %>%
  
  dplyr::mutate(scraped.name = paste(family, genus, species)) %>%
  dplyr::rename(scraped.family = family,
                scraped.genus = genus, 
                scraped.species = species,
                caab_code = caab.code) %>%
  dplyr::select(caab_code, scraped.name, scraped.family, scraped.genus, scraped.species, common.name) 

caab.combined <- full_join(caab.regions, caab.scraped) %>%
  dplyr::mutate(name = paste(family, genus, species)) %>%
  # dplyr::filter(!name %in% scraped.name) %>% # for testing
  # dplyr::filter(!is.na(scraped.name)) %>%# for testing 
  dplyr::mutate(scraped.family = if_else(is.na(scraped.family), family, scraped.family)) %>%
  dplyr::mutate(scraped.genus = if_else(is.na(scraped.genus), genus, scraped.genus)) %>%
  dplyr::mutate(scraped.species = if_else(is.na(scraped.species), species, scraped.species)) %>%
  
  dplyr::mutate(family = if_else(name %in% c(scraped.name), family, scraped.family)) %>%
  dplyr::mutate(genus = if_else(name %in% scraped.name, genus, scraped.genus)) %>%
  dplyr::mutate(species = if_else(name %in% scraped.name, species, scraped.species)) %>%
  dplyr::select(-c(scraped.name, scraped.family, scraped.genus, scraped.species, name)) %>%
  dplyr::mutate(common.name = str_replace_all(.$common.name, "\\[|\\]", "")) %>%
  dplyr::filter(!is.na(species))

names(caab.combined)

names(fishbase)

iucn.all <- readRDS("data/iucn.RDS") %>%
  dplyr::rename(iucn_ranking = category) %>%
  dplyr::mutate(iucn_ranking = case_when(
    iucn_ranking %in% "DD" ~ "Data Deficient",
    iucn_ranking %in% "LC" ~ "Least Concern",
    iucn_ranking %in% "NT" ~ "Near Threatened",
    iucn_ranking %in% "VU" ~ "Vulnerable",
    iucn_ranking %in% "EN" ~ "Endangered",
    iucn_ranking %in% "CR" ~ "Critically Endangered",
    iucn_ranking %in% "EW" ~ "Extinct in the Wild",
    iucn_ranking %in% "EX" ~ "Extinct"
  )) %>%
  glimpse()

animals <- readRDS("data/animals_australia_with_dist.RDS") %>%
  dplyr::mutate(caab = as.character(caab)) %>%
  left_join(iucn.all) %>%
  glimpse()

unique(animals$iucn_ranking)

# Make it simpler
simple.lh <- caab.combined %>%
  dplyr::left_join(fishbase) %>%
  dplyr::rename(caab = caab_code,
                marine_region = marine.region,
                australian_common_name = common.name,
                fb_code = speccode,
                fb_length_at_maturity_cm = fb.length.at.maturity.cm,
                iucn_ranking = IUCN.ranking,
                fb_length_max = FB.Length_MAX,
                fb_length_max_type = FB.LTypeMaxM,
                fb_vulnerability = FB.Vulnerability,
                fb_countries = FB.countries,
                fb_status = FB.Status,
                fb_length_weight_measure = Length.measure,
                fb_a = a,
                fb_b = b,
                fb_a_ll = aLL,
                fb_b_ll = bLL,
                fb_length_weight_source = Source_Level) %>%
  
  dplyr::left_join(keep) %>%
  
  dplyr::mutate(australian_source = "CAAB",
                global_source = "FishBase",
                local_source = "Harvey et al 2020") %>%
  
  dplyr::mutate(scientific_name = paste(genus, species)) %>%

  dplyr::mutate(global_region = "Australia") %>%
  
  dplyr::select(c(australian_source,
                caab,
                class,
                order,
                family,
                genus,
                species,
                scientific_name,
                australian_common_name, # TODO need to add in 1st script or use the scraping
                marine_region,
                
                global_source,
                fb_code,
                fb_length_at_maturity_cm,
                # TODO need to add in 1st script
                subfamily,
                global_region,
                
                fb_length_weight_measure,
                fb_a,
                fb_b,
                fb_a_ll,
                fb_b_ll,
                fb_length_weight_source,
                
                fb_vulnerability,
                fb_countries,
                fb_status,
                fb_length_max,
                fb_length_max_type,
                rls_trophic_group,
                rls_water_column,
                rls_substrate_type,
                rls_thermal_niche,
                
                local_source,
                epbc_threat_status,
                iucn_ranking,
                
                fishing_mortality,
                fishing_type,
                min_legal_nt,
                max_legal_nt,
                min_legal_wa,
                max_legal_wa,
                min_legal_qld,
                max_legal_qld,
                min_legal_nsw,
                max_legal_nsw,
                min_legal_vic,
                max_legal_vic,
                min_legal_sa,
                max_legal_sa,
                min_legal_tas,
                max_legal_tas
                )) %>%
  bind_rows(animals)
  
unique(simple.lh$class)

names(simple.lh) %>% sort()

saveRDS(simple.lh, "data/simple.life.history.RDS")
saveRDS(simple.lh, "C:/GitHub/CheckEMV2/data/simple.life.history.RDS") # to update CheckEM

# Define styles for excel
aus = createStyle(fontColour = "black", bgFill = "#D9EAD3")
global = createStyle(fontColour = "black", bgFill = "#FCE5CD")
local = createStyle(fontColour = "black", bgFill = "#FFF2CC")

# Create new workbook
wb = createWorkbook(title = "life.history")

# Add Sheet 1 - Information -----
addWorksheet(wb, "information")

info <- data.frame(Source = c("Australian.source", "Global.source", "Local.source"),
                 From = c("CAAB", "FishBase", "Harvey et al. 2020"),
                 Description = c("Species list", "Life history information", "Fisheries and TEPS information"),
                 Use = c("Region and ID QAQC", "life history and body size QAQC", "Metric calculations"),
                 Comments = c("","includes L/W formula", "Need to add in maturity etc metrics"))

# Add the data 
header = createStyle(textDecoration = "Bold")
writeData(wb, "information", info, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "information", cols = 1:ncol(info), widths = "auto")

# Colour the cells
conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Australian.source", 
                      type = "contains",
                      style = aus)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Global.source", 
                      type = "contains",
                      style = global)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Local.source", 
                      type = "contains",
                      style = local)

# Add Sheet 2 - fish.life.history ----
addWorksheet(wb, "fish.life.history")

# Add the data
writeData(wb, "fish.life.history", simple.lh, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "fish.life.history", cols = 1:ncol(simple.lh), widths = "auto")

# Colour the cells
conditionalFormatting(wb, "fish.life.history", 
                      cols = 1:10, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule= "!=0", 
                      style = aus)

conditionalFormatting(wb, "fish.life.history", 
                      cols = 1:10, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule= "=0", 
                      style = aus)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 11:30, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = global)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 11:30, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = global)

conditionalFormatting(wb, "fish.life.history", 
                      cols = 31:49, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = local)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 31:49, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = local)

# Add Sheet 3 - Synonyms ----
addWorksheet(wb, "synonyms")

# Add the data 
writeData(wb, "synonyms", synonyms, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "synonyms", cols = 1:ncol(synonyms), widths = "auto")

# Add Sheet 4 - Family common names ----
addWorksheet(wb, "family.common.names")

# Add the data 
writeData(wb, "family.common.names", fam.common.names, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "family.common.names", cols = 1:ncol(fam.common.names), widths = "auto")

# Add Sheet 5 - Lumped common names ----
addWorksheet(wb, "lumped.common.names")

# Add the data 
writeData(wb, "lumped.common.names", lumped.common.names, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "lumped.common.names", cols = 1:ncol(lumped.common.names), widths = "auto")

# Show the workbook
openXL(wb)

time <- str_remove_all(Sys.time(), "[^[:alnum:] ]") # remove spaces for Nik
glimpse(time)

date <- str_sub(time, 1, 8)
hour <- str_sub(time, 10, 15)

# Save the workbook
saveWorkbook(wb, paste("output/fish/fish.life.history", date, hour, "xlsx", sep = "."), overwrite = TRUE)

