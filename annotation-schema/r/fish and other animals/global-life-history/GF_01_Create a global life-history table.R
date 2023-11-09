library(rfishbase)
library(tidyverse)
# devtools::install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE)
library(GlobalArchive)
library(worrms)
library(rredlist)
library(openxlsx)

# Get a list of all fish species in the world from fishbase ----
all.species <- load_taxa() %>% 
  ga.clean.names() %>%
  dplyr::mutate(scientific = species) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::select(speccode, superclass, class, order, family, genus, species, scientific) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

# Format basic info and min + max depths ----
info <- species(all.species$scientific) %>% 
  ga.clean.names() %>%
  dplyr::rename(length.max = length, length.max.type = ltypemaxm) %>% # Length metrics are in cm
  dplyr::select(species, speccode, fbname, length.max, length.max.type, importance,
                depthrangecomdeep, depthrangecomshallow, depthrangedeep,depthrangeshallow) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::rename(scientific = species, 
                common.name = fbname) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

names(info) %>% sort()

# Validate species names ----
validated <- validate_names(all.species$scientific)

# Distribution (Which FAO major fishing areas are species present in) -----
distribution <- distribution(validated) %>%
  ga.clean.names() %>%
  dplyr::select(species, speccode, status, fao) %>% 
  # dpl
  dplyr::filter(!status %in% c("stray", "extirpated", "unclear", "questionable", "NA", NA)) %>%
  dplyr::mutate(fao = str_replace_all(.$fao, c(", " = "_", 
                                               " - " = "_",
                                               "-" = "_",
                                               " " = "."))) %>%
  nest(fao = c(fao)) %>%
  dplyr::mutate(fao = paste(fao)) %>%
  dplyr::mutate(fao = str_replace_all(.$fao, c("[()]" = "", 
                                               "listfao = c" = "", 
                                               "listfao = " = "", 
                                               '"' = ""))) %>%
  tidyr::pivot_wider(names_from = status, values_from = fao) %>%
  dplyr::rename(fao.fishing.area.endemic = endemic, 
                fao.fishing.area.native = native, 
                fao.fishing.area.introduced = introduced,
                scientific = species)%>%
  dplyr::mutate(speccode = as.character(speccode))

# Get size of maturity -----
maturity <- maturity(validated) %>%
  ga.clean.names() %>%
  # filter(type1 %in% "FL") %>% # Would be good to turn this on but it gets rid of a lot of species.
  filter(!is.na(lm)) %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(length.at.maturity.cm = mean(lm)) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::mutate(speccode = as.character(speccode)) 
  
# Get length-weight relationship
# TODO come back and make this for only one species

lwr <- length_weight(validated) %>%
  ga.clean.names() %>%
  dplyr::mutate(speccode = as.character(speccode))

# bay_lwrs <- data.frame() # turned off for now So i don't loose the data we have already downloaded
bay_lwrs <- read.csv("data/bayesian_length-weights.csv") %>% distinct()

# Sys.time() 
# 
# temp.validated <- as.data.frame(validated) %>%
#   filter(!validated %in% c(unique(bay_lwrs$species))) %>%
#   pull(validated)

# # started next ones (24799) @ 8:22 AM (estimate 10 AM next day)
# # have length weights for 34671 species, running once more to double check I don't get any extras
# for(species in seq(1:length(unique(temp.validated)))){ 
#   print(species)
#   
#   try(temp_lwr <- find_lw(temp.validated[species]))
#   nrow(temp_lwr)
#   
#   if(!is.null(nrow(temp_lwr))){
#   bay_lwrs <- bind_rows(bay_lwrs, temp_lwr)
#   }
# }
# 
# bay_lwrs <- bay_lwrs %>% distinct() %>%
#   dplyr::rename(scientific = species)

# Sys.time()

# write.csv(bay_lwrs, "data/bayesian_length-weights.csv", row.names = FALSE)


# Get list of synyonms
synonyms <- synonyms(all.species$scientific) %>% 
  ga.clean.names() %>%
  dplyr::filter(!status %in% "accepted name") %>% # Don't care about accepted names
  dplyr::filter(!synonym == species) # this is silly, but some lines had the same fish name as correct as a synonym. So had to remove

unique(synonyms$status)

## Get worms code ----
## The wm_records_names function can only handle 170 names at a time (have chopped in 150 chunks as it is an easier number)
## 150 names takes around ~ 6 seconds to run
# 
# # Create a list of all species
# species.to.use <- unique(all.species$scientific)
# 
# # Break into chunks of 150
# species.lists <- split(species.to.use, ceiling(seq_along(species.to.use)/150)) # 234 lists
# 
# worms <- data.frame()

# # Time to run = 35 (Have written as a csv to re-read back in to save time, but left the original code to be re-run if needed)
# for(id in seq(1:length(species.lists))){
#   
#   dat <- species.lists[id][[1]] #%>% glimpse()
#   temp <- wm_records_names(c(dat), marine_only = FALSE)
#   
#   temp.worms <- do.call("rbind", temp)
#   
#   worms <- bind_rows(worms, temp.worms)
#   
# }
# 
# worms.final <- worms %>%
#   distinct() %>%
#   ga.clean.names() %>%
#   dplyr::select(aphiaid, scientificname, status, kingdom, phylum, class, order, family, genus, ismarine, isbrackish, isfreshwater) %>%
#   dplyr::rename(scientific = scientificname)
# write.csv(worms.final, "data/worms.list.csv", row.names = FALSE)
# 
# worms.final <- read.csv("data/worms.list.csv")%>%
#   filter(!is.na(scientific)) %>%
#   filter(status == "accepted")
# 
# simple.worms <- worms.final %>%
#   dplyr::select(-c(ismarine, isbrackish, isfreshwater))
# 
# missing.some.info <- (simple.worms[!complete.cases(simple.worms), ])
# species.to.use <- unique(missing.some.info$scientific)
# 
# # Break into chunks of 150
# species.lists <- split(species.to.use, ceiling(seq_along(species.to.use)/150)) # only 6 lists this time
# missing.worms <- data.frame()
# 
# for(id in seq(1:length(species.lists))){
#   dat <- species.lists[id][[1]] #%>% glimpse()
#   temp <- wm_records_names(c(dat), marine_only = FALSE)
#   temp.worms <- do.call("rbind", temp)
#   missing.worms <- bind_rows(missing.worms, temp.worms)
# }
# 
# new.worms <- missing.worms %>%
#   distinct() %>%
#   ga.clean.names() %>%
#   dplyr::select(aphiaid, scientificname, status, kingdom, phylum, class, order, family, genus, ismarine, isbrackish, isfreshwater) %>%
#   dplyr::rename(scientific = scientificname) %>%
#   filter(status == "accepted") %>%
#   filter(!is.na(family)) 
# 
# all.worms <- worms.final %>%
#   filter(!is.na(family)) %>%
#   bind_rows(new.worms) %>%
#   filter(status == "accepted")

# simple.worms <- all.worms %>%
#   dplyr::select(-c(ismarine, isbrackish, isfreshwater))
# 
# test.for.nas <- (simple.worms[!complete.cases(simple.worms), ])

# write.csv(all.worms, "data/worms.list.csv", row.names = FALSE)
all.worms <- read.csv("data/worms.list.csv") %>%
  group_by(scientific) %>% 
  slice(1) %>% # WORMS has duplicate rows but different AphiaIDs for some species e.g. Anabarilius liui (1007204 & 1012093)
  ungroup()

# duplicates <- all.worms %>% 
#   group_by(scientific) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) # No duplicates!

# # Get synonyms from worms using AphiaID ----
# ids.to.use <- unique(all.worms$aphiaid)
# 
# # Break into chunks of 150
# id.lists <- split(ids.to.use, ceiling(seq_along(ids.to.use)/150))[1:3] # 234 lists
# 
# syns <- data.frame()
# 
# for(id in seq(1:length(id.lists))){
#   dat <- id.lists[id][[1]] #%>% glimpse()
#   temp <- wm_synonyms_(c(dat)) %>%
#     glimpse()
#   
#   syns <- bind_rows(syns, temp)
# }
# 
# syn.tidy <- syns %>%
#   distinct() %>%
#   ga.clean.names() %>%
#   dplyr::select(scientificname, unacceptreason, valid_aphiaid, valid_name) %>%
#   dplyr::filter(!scientificname == valid_name) %>% # a check to make sure the valid name isn't the same as the synonym
#   dplyr::rename(scientific = valid_name, 
#                 aphiaid = valid_aphiaid,
#                 synonym = scientificname)
# 
# write.csv(syn.tidy, "data/worms.synonyms.list.csv", row.names = FALSE)
worms.synonyms <- read.csv("data/worms.synonyms.list.csv") %>%
  left_join(., all.worms) %>%
  dplyr::select(-c(match_type)) %>% #TODO remove this if i have to run the above code again
  tidyr::separate(scientific, into = c("genus_correct", "species_correct"), sep = " ", extra = "merge", remove = FALSE) %>%
  tidyr::separate(synonym, into = c("genus", "species"), sep = " ", extra = "merge", remove = FALSE) %>%
  dplyr::select(-c(aphiaid, status, kingdom, phylum, class, order, ismarine, isbrackish, isfreshwater)) %>%
  dplyr::mutate(family_correct = family)
  
  
list.for.checking <- worms.synonyms %>%
  rename(correct.name = scientific, scientific = synonym)

# TODO need to check that the list of unaccepted names isn't a existing synonym for something else
# Could somehow flag this in CheckEM if there are some e.g. Pagrus auratus is an unaccepted for Sparus aurata Linnaeus, 1758
# Another example is Alectis indica (Rüppell, 1830) (accepted name), but Alectis indica is also a synonym for Alectis ciliaris (Bloch, 1787).
species.also.a.synonym <- worms.synonyms %>%
  dplyr::rename(correct_name = scientific,
                scientific = synonym) %>%
  dplyr::select(correct_name, scientific) %>%
  dplyr::semi_join(info) %>%
  dplyr::rename(valid.name.too.but.also.a.synonym.for.correct.name = scientific)

# Get IUCN ranking ----
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


# Get vulnerability ----
# Also going to keep body shape and dermersal/pelagic from this function
fb.vul <- fb_tbl("species") %>%
  ga.clean.names() %>%
  dplyr::select(speccode, genus, species, vulnerability, bodyshapei, demerspelag) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

names(fb.vul) %>% sort()

unique(fb.vul$bodyshapei) # TODO make body shape uniform
unique(fb.vul$demerspelag)

# Final life history sheet should have ----
# - Y Scientific name
# - Y Class, Order, Family, Genus, Species
# - Y Common name
# - Y Regions were present
# - Length-weight 
# - Y Vulnerability
# - IUCN threat status
# - Y Fishing
# - Y Size at maturity

# And a Synonym list

fblh <- all.species %>% # has 35,024 species
  dplyr::select(-c(superclass, class, order, family)) %>%
  full_join(info) %>% # has 35,024 species
  full_join(distribution) %>% # has 34,850 species (less)
  full_join(maturity) %>% # only available for 1913 species (much less)
  full_join((all.worms)) %>% # Has 103 missing # the join increases the number of rows WHY?  # TODO
  full_join(fb.vul) %>%
  dplyr::select(-speccode) %>%
  full_join(bay_lwrs) %>%
  dplyr::rename(aphia_id = aphiaid,
                fb_body_shape = bodyshapei,
                common_name = common.name,
                scientific_name = scientific,
                fb_environment = demerspelag,
                fb_depth_range_shallow = depthrangeshallow,
                fb_depth_range_deep = depthrangedeep,
                fb_fao_fishing_area_endemic = fao.fishing.area.endemic,
                fb_fao_fishing_area_introduced = fao.fishing.area.introduced,
                fb_fao_fishing_area_native = fao.fishing.area.native,
                fb_commerical_importance = importance,
                fb_brackish = isbrackish,
                fb_freshwater = isfreshwater,
                fb_marine = ismarine,
                fb_length_at_maturity_cm = length.at.maturity.cm,
                fb_length_max = length.max,
                fb_length_max_type = length.max.type,
                fb_a = lwa_m,
                fb_a_sd = lwa_sd,
                fb_b = lwb_m,
                fb_b_sd = lwb_sd,
                fb_metric = metric,
                fb_vulnerability = vulnerability) %>%
  dplyr::select(-c(status, depthrangecomdeep, depthrangecomshallow)) %>% # remove 
  
  dplyr::mutate(marine_region = paste(fb_fao_fishing_area_endemic, fb_fao_fishing_area_introduced, fb_fao_fishing_area_native, sep = ", ")) %>%
  dplyr::mutate(marine_region = str_replace_all(marine_region, c("NA, NA, " = "", ", NA, NA" = ""))) %>%
  dplyr::select(aphia_id, scientific_name, kingdom, phylum, class, order, family, genus, species, common_name,
                fb_length_at_maturity_cm, fb_length_max, fb_length_max_type,
                fb_a, fb_a_sd, fb_b, fb_b_sd, fb_metric, fb_body_shape, 
                fb_commerical_importance,
                fb_depth_range_shallow, fb_depth_range_deep,
                marine_region, fb_fao_fishing_area_endemic, fb_fao_fishing_area_introduced, fb_fao_fishing_area_native, 
                fb_environment, fb_marine, fb_freshwater, fb_brackish, fb_vulnerability)%>%
  dplyr::mutate(aphia_id = as.character(aphia_id)) %>%
  filter(!is.na(kingdom))

names(fblh)
  
test <- fblh %>%
  select(fb_all_fao_fishing_areas) %>%
  mutate(marine.region = strsplit(as.character(fb_all_fao_fishing_areas), split = ", ")) %>%
  unnest(marine.region) %>%
  distinct(marine.region)

animals <- readRDS("data/animals_global_with_dist.RDS") %>%
  dplyr::rename(aphia_id = taxonid) %>%
  tidyr::separate(scientific_name, into = c("genus", "species"), remove = FALSE, extra = "merge") 

names(animals)
  
# Bind with IUCN
life_history <- bind_rows(fblh, animals) %>%
  left_join(iucn.all)
  
names(life_history)

missing <- life_history %>%
  filter(is.na(phylum))

write.csv(life_history, "output/fish/global_fish.life.history.csv")
write.csv(worms.synonyms, "output/fish/global_fish.synonyms.csv")


# Write files for CheckEM
write.csv(life_history, "output/CheckEM/global_fish.life.history.csv")
write.csv(worms.synonyms, "output/CheckEM/global_fish.synonyms.csv")
write.csv(species.also.a.synonym, "output/CheckEM/global_fish.ambiguous.synonyms.csv")

saveRDS(life_history, "C:/GitHub/CheckEMV2/data/global_fish.life.history.RDS")
saveRDS(worms.synonyms, "C:/GitHub/CheckEMV2/data/global_fish.synonyms.RDS")
saveRDS(species.also.a.synonym, "C:/GitHub/CheckEMV2/data/global_fish.ambiguous.synonyms.RDS")




# species.missing.distribution <- anti_join(all.species, distribution) #  174 missing distribution
# species.missing.maturity <- anti_join(all.species, maturity) #  33,111 missing maturity

# FISHBASE HAS SOME VALID NAMES THAT ARE SYNONYMS HECK
# TODO once the worms synyonms have finished running - need to check which species are synonyms 
# (this will be confusing because I will also need to know which ones aren't ambiguous synonyms
species.missing.worms <- anti_join(all.species %>% dplyr::select(-c(superclass, class, order, family)), all.worms) # only 109
are.they.valid <- validate_names(c(species.missing.worms$scientific))

are.they.synonyms <- left_join(species.missing.worms, list.for.checking) %>%
  filter(!is.na(correct.name))

# Have found 24 that could be synonyms
# Need to check if these species exist in the data before though.
do.they.exist <- all.species %>%
  filter(scientific %in% c(unique(are.they.synonyms$correct.name)))
# 20 are already in the fishbase data - this is very confusing.

# I think it is best to remove these species

worms.missing.fishbase <- anti_join(all.worms, all.species %>% dplyr::select(-c(superclass, class, order, family))) # None - woo

# Save information as an excel workbook ----
# Define styles for excel
# TODO update this for global context (FishBase, WORMS or IUCN)
fb = createStyle(fontColour = "black", bgFill = "#d4a8f0")
worms = createStyle(fontColour = "black", bgFill = "#a8cdf0")
iucn = createStyle(fontColour = "black", bgFill = "#f0a8a8")

# Create new workbook
wb = createWorkbook(title = "global.fish.life.history")

# Add Sheet 1 - Information -----
addWorksheet(wb, "information")

# Create the info data 
info <- data.frame(Source = c("FishBase", "WORMS", "IUCN red list"),
                   Package = c("rfishbase", "worrms", "rredlist"),
                   Citation = c("X", "X", "X"),
                   Information = c("X", "X", "X"),
                   Comments = c("X", "X", "X"))
# Add the data 
header = createStyle(textDecoration = "Bold")
writeData(wb, "information", info, headerStyle = header)

# Colour the cells
conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "FishBase", 
                      type = "contains",
                      style = fb)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "WORMS", 
                      type = "contains",
                      style = worms)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "IUCN red list", 
                      type = "contains",
                      style = iucn)

# Add Sheet 2 - fish.life.history ----
addWorksheet(wb, "lifehistory")

fblh.chop <- fblh %>% slice(5)

# Add the data
writeData(wb, "lifehistory", fblh, headerStyle = header)

write.csv(fblh, "output/fish/global_fish.life.history.csv", row.names = FALSE)
write.csv(worms.synonyms, "output/fish/global_fish.synonyms.csv", row.names = FALSE)

# # Colour the cells
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 1:10, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule= "!=0", 
#                       style = aus)
# 
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 1:10, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule= "=0", 
#                       style = aus)
# 
# 
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 11:30, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule = "!=0", 
#                       style = global)
# 
# 
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 11:30, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule = "=0", 
#                       style = global)
# 
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 31:47, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule = "!=0", 
#                       style = local)
# 
# 
# conditionalFormatting(wb, "fish.life.history", 
#                       cols = 31:47, 
#                       rows = 1:(nrow(simple.lh)+1), 
#                       rule = "=0", 
#                       style = local)

# Add Sheet 3 - Synonyms ----
addWorksheet(wb, "synonyms")

# Add the data 
writeData(wb, "synonyms", worms.synonyms, headerStyle = header)

# Show the workbook
openXL(wb)

time <- str_remove_all(Sys.time(), "[^[:alnum:] ]") # remove spaces for Nik
glimpse(time)

date <- str_sub(time, 1, 8)
hour <- str_sub(time, 10, 15)

# Save the workbook
saveWorkbook(wb, paste("output/fish/global_fish.life.history", date, hour, "xlsx", sep = "."), overwrite = TRUE)

