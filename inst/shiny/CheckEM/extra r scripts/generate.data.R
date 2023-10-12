library(tidyverse)
library(googlesheets4)
library(GlobalArchive)
library(rgdal)
library(sf)
library(RCurl)
require(rgdal)
library(mregions)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth()
2

# TODO change these to github repo for Australia LH and synonyms 
# TODO change australian.common.name to common.name
# Read in sheet from googledrive ----
aus.url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"

# # Read in life history sheet ----
# lh.aus <- read_sheet(aus.url) %>%
#   ga.clean.names() %>%
#   filter(grepl('Australia', global.region)) %>%
#   dplyr::mutate(all=as.numeric(all)) %>%
#   dplyr::mutate(bll=as.numeric(bll)) %>%
#   dplyr::mutate(a=as.numeric(a)) %>%
#   dplyr::mutate(b=as.numeric(b)) %>%
#   dplyr::rename(code = caab) %>%
#   dplyr::select(code, family, genus, species, marine.region, length.measure, a, b, all, bll, fb.length_max, fb.ltypemaxm, australian.common.name) %>%
#   distinct() %>%
#   dplyr::mutate(marine.region = str_replace_all(.$marine.region,c("N/" = "North/",
#                                                                   "NW" = "North-west",
#                                                                   "CS" = "Coral Sea",
#                                                                   "TE" = "Temperate East",
#                                                                   "SE" = "South-east",
#                                                                   "SW" = "South-west",
#                                                                   "N" = "North",
#                                                                   "Northorth" = "North",
#                                                                   "Northor" = "North")))
# classes <- read_sheet(aus.url) %>%
#   ga.clean.names() %>%
#   filter(grepl('Australia', global.region)) %>%
#   distinct(class, order, family, genus, species)

# test <- readRDS(gzcon(url("https://github.com/biohacker/nomogram/blob/main/rg_quantiles.RDS")))
# 

extra.lengths <- read_sheet("https://docs.google.com/spreadsheets/d/1H7EXoTlpeg48LrNVszIa8irwIAh_SqYBNqMYI1WVce8/edit?usp=sharing", sheet = "Responses") %>%
  ga.clean.names() %>%
  dplyr::filter(reviewed %in% "Yes") %>%
  dplyr::rename(new.maximum.length = new.maximum.length.in.cm.) %>%
  dplyr::select(family, genus, species, new.maximum.length)

lh.aus <- readRDS("data/simple.life.history.RDS") %>%
  ga.clean.names() %>%
  dplyr::mutate(all = as.numeric(fb_a_ll)) %>%
  dplyr::mutate(bll = as.numeric(fb_b_ll)) %>%
  dplyr::mutate(a = as.numeric(fb_a)) %>%
  dplyr::mutate(b = as.numeric(fb_b)) %>%
  dplyr::rename(code = caab) %>%
  
  #TODO change all the app to be the new naming. This is just a temp fix
  dplyr::rename(#marine.region = marine_region,
                length.measure = fb_length_weight_measure,
                fb.ltypemaxm = fb_length_max_type,
                # fb.length_max = fb_length_max,
                australian.common.name = australian_common_name) %>%
  
  
  dplyr::select(code, family, genus, species, marine_region, length.measure, a, b, all, bll, fb_length_max, fb.ltypemaxm, australian.common.name) %>%
  distinct() %>%
  dplyr::mutate(marine_region = str_replace_all(.$marine_region, c("N/" = "North/",
                                                                   "NW" = "North-west",
                                                                   "CS" = "Coral Sea",
                                                                   "TE" = "Temperate East",
                                                                   "SE" = "South-east",
                                                                   "SW" = "South-west",
                                                                   "N" = "North",
                                                                   "Northorth" = "North",
                                                                   "Northor" = "North"))) %>%
  
  dplyr::left_join(extra.lengths) %>%
  tidyr::replace_na(list(new.maximum.length = 0)) %>%
  dplyr::mutate(fb_length_max = if_else(new.maximum.length > fb_length_max, new.maximum.length, fb_length_max)) %>%
  dplyr::mutate(fb_length_max = fb_length_max * 10) %>%
  dplyr::select(-new.maximum.length)

unique(lh.aus$fb.ltypemaxm)

spp.lengths <- lh.aus %>%
  dplyr::filter(!is.na(fb_length_max)) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(average.max = mean(fb_length_max)) %>%
  ungroup()

lh.aus <- left_join(lh.aus, spp.lengths) %>%
  dplyr::mutate(fb_length_max = if_else(is.na(fb_length_max), average.max, fb_length_max))

classes <- readRDS("data/simple.life.history.RDS") %>%
  ga.clean.names() %>%
  distinct(class, order, family, genus, species)

# Expand life history for checking regions ----
lh.aus.expanded <- lh.aus %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>% # changed from "/" for old LH
  unnest(marine_region)

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
lh.aus.family.max.length <- lh.aus %>%
  filter(!is.na(fb_length_max)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max = mean(fb_length_max)) %>%
  ungroup()

lh.aus.genus.max.length <- lh.aus %>%
  filter(!is.na(fb_length_max)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max = mean(fb_length_max)) %>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
lh.aus.min.max <- left_join(lh.aus, lh.aus.family.max.length, by = c("family")) %>% # add in family values
  left_join(., lh.aus.genus.max.length) %>% # add in genus values
  dplyr::mutate(fb_length_max = ifelse((is.na(fb_length_max)), genuslength_max, fb_length_max)) %>%
  dplyr::mutate(fb_length_max = ifelse((is.na(fb_length_max)), famlength_max, fb_length_max)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min_length = 0.15 * fb_length_max) %>% # change values here
  mutate(max_length = 0.85 * fb_length_max) %>% # change values here
  glimpse()

# Read in global life history list ----
lh.glo <- readRDS("data/global_fish.life.history.RDS") %>%
  # dplyr::filter(fb_marine %in% 1) %>%
  
  # dplyr::mutate(all = as.numeric(fb_a_ll)) %>%
  # dplyr::mutate(bll = as.numeric(fb_b_ll)) %>%
  dplyr::mutate(a = as.numeric(fb_a)) %>%
  dplyr::mutate(b = as.numeric(fb_b)) %>%
  dplyr::rename(code = aphia_id) %>%
  
  #TODO change all the app to be the new naming. This is just a temp fix
  dplyr::rename(#marine.region = marine_region,
                # length.measure = fb_length_weight_measure,
                fb.ltypemaxm = fb_length_max_type,
                length.max = fb_length_max,
                common.name = common_name,
                fao.fishing.area.endemic = fb_fao_fishing_area_endemic,
                fao.fishing.area.introduced = fb_fao_fishing_area_introduced,
                fao.fishing.area.native = fb_fao_fishing_area_native,
                length.at.maturity.cm = fb_length_at_maturity_cm) %>%
  
  
  
  dplyr::select(class, order, family, genus, species, common.name, fao.fishing.area.endemic, fao.fishing.area.introduced, fao.fishing.area.native, length.max, length.at.maturity.cm) %>%
  dplyr::mutate(marine_region = paste(fao.fishing.area.endemic, fao.fishing.area.introduced, fao.fishing.area.native, sep = ", ")) %>%
  dplyr::mutate(marine_region = str_replace_all(.$marine_region, c("NA, " = "", "NA" = "", "\n" = ""))) %>%
  dplyr::left_join(extra.lengths) %>%
  tidyr::replace_na(list(new.maximum.length = 0)) %>%
  dplyr::mutate(length.max = if_else(new.maximum.length > length.max, new.maximum.length, length.max)) %>%
  dplyr::mutate(length.max = length.max * 10) %>%
  dplyr::select(-new.maximum.length) %>%
  glimpse()

names(lh.aus) %>% sort()
names(lh.glo) %>% sort()

unique(lh.glo$marine_region)

# Expand life history for checking regions ----
lh.glo.expanded <- lh.glo %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>%
  unnest(marine_region)

unique(lh.glo.expanded$marine_region) %>% sort()

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
lh.glo.family.max.length <- lh.glo %>%
  filter(!is.na(length.max)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max = mean(length.max)) %>%
  ungroup()

lh.glo.genus.max.length <- lh.glo %>%
  filter(!is.na(length.max)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max = mean(length.max)) %>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
lh.glo.min.max <- left_join(lh.glo, lh.glo.family.max.length, by = c("family")) %>% # add in family values
  left_join(., lh.glo.genus.max.length) %>% # add in genus values
  dplyr::mutate(fb_length_max = ifelse((is.na(length.max)), genuslength_max, length.max)) %>%
  dplyr::mutate(fb_length_max = ifelse((is.na(length.max)), famlength_max, fb_length_max)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min_length = 0.15 * fb_length_max) %>% # change values here
  mutate(max_length = 0.85 * fb_length_max) %>% # change values here
  glimpse()


# Synonyms ----
lh.aus.synonyms <- read_sheet(aus.url, sheet = "synonyms_updated") %>%
  distinct() %>%
  ga.clean.names() %>%
  dplyr::filter(!keep %in% "No") %>%
  dplyr::select(-c(comment, keep, reason, source, last_checked)) %>%
  glimpse()

names(lh.aus.synonyms)

caab.aus.synonyms <- readRDS("data/caab_synonyms.RDS") %>%
  dplyr::rename(family_correct = family,
                genus_correct = genus, 
                species_correct = species,
                family = synonym.family,
                genus = synonym.genus,
                species = synonym.species) %>%
  dplyr::select(family, genus, species, family_correct, genus_correct, species_correct)

all.synonyms <- bind_rows(lh.aus.synonyms, caab.aus.synonyms)

# Need to remove these from the synonym list
lh.glo.ambiguous.synonyms <- readRDS("data/global_fish.ambiguous.synonyms.RDS") %>%
  dplyr::rename(synonym = valid.name.too.but.also.a.synonym.for.correct.name) %>%
  dplyr::rename(scientific = correct_name) %>%
  dplyr::mutate(remove = TRUE)

regions.for.synonyms <- lh.glo.expanded %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct(scientific, marine_region)

lh.glo.synonyms <- readRDS("data/global_fish.synonyms.RDS") %>%
  full_join(regions.for.synonyms) %>%
  distinct() %>%
  dplyr::filter(!is.na(marine_region)) %>%
  # full_join(lh.glo.ambiguous.synonyms) %>%
  # dplyr::filter(!remove %in% TRUE) %>%
  glimpse()


# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

aus.regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/marine_regions.shp")
aus.regions$REGION <- as.character(aus.regions$REGION)
proj4string(aus.regions) <- CRS(wgs.84)

# New CAPAD 2022 ----
marineparks <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/CAPAD2022_single_fixed.shp")
proj4string(marineparks) <- CRS(wgs.84)

names(marineparks)

unique(marineparks$NAME) %>% sort()
# plot(marineparks)

# Buffer in QLD - has some fishing allowed (Trolling)
# These allow fishing:
# - Habitat Protection
# - General Use

marineparks$status <- if_else(str_detect(pattern = "No take|Sanctuary|Conservation Area|Marine National Park|Preservation|National Park|	
Scientific Research|Scientific Reference", string = marineparks$ZONE_TYPE),
"No-take",
"Fished")

marineparks.single <- st_read("C:/GitHub/Files to big for CheckEM/spatial/CAPAD2022_single_fixed.shp")
marineparks.single <- st_as_sf(marineparks)

marineparks.single <- marineparks.single %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = ZONE_TYPE) ~ "Sanctuary",
    str_detect(pattern = "IUCN II", string = ZONE_TYPE) ~ "Marine National Park",
    str_detect(pattern = "National Park", string = ZONE_TYPE) ~ "Marine National Park",
    str_detect(pattern = "Recreational|Recreation", string = ZONE_TYPE) ~ "Recreational Use",
    str_detect(pattern = "Habitat Protection", string = ZONE_TYPE) ~ "Habitat Protection",
    str_detect(pattern = "Special Purpose", string = ZONE_TYPE) ~ "Special Purpose",
    str_detect(pattern = "Multiple Use", string = ZONE_TYPE) ~ "Multiple Use",
    str_detect(pattern = "General", string = ZONE_TYPE) ~ "General Use"))#%>% 
#filter(!is.na(zone)) # TODO work out if I should be doing this or not. I think probably not

unique(marineparks.single$NAME) %>% sort()

comm.pal <- 
  leaflet::colorFactor(palette = c("#F7C0D8", "#7BBC63", "#FDBA33", "#FFF8A3", "#6DAFE0", "#447191", "#B9E6FB", "#828282"), 
                       levels = c("Sanctuary", "Marine National Park", "Recreational Use", "Habitat Protection", "Special Purpose", "Multiple Use", "General Use"))



# Testing Global Marine parks ----
# takes a while to read in ----
# world_marineparks_raw <- st_read(dsn = "C:/GitHub/Files to big for CheckEM/WDPA_WDOECM_Jul2023_Public_marine.shp")
# 
# world_marineparks <- world_marineparks_raw %>%
#   # st_cast("POLYGON") %>%
#   # st_make_valid() %>%
#   st_as_sf() 

# saveRDS(world_marineparks, "data/spatial/WDPA_WDOECM_Jul2023_Public_marine.RDS")
world_marineparks <- readRDS("C:/GitHub/Files to big for CheckEM/spatial/WDPA_WDOECM_Jul2023_Public_marine.RDS") %>%
  dplyr::filter(MARINE %in% 2) %>%
  dplyr::rename(status = NO_TAKE, zone = IUCN_CAT) %>%
  dplyr::select(NAME, zone, status, geometry) %>%
  dplyr::mutate(status = str_replace_all(status, c("All" = "No-take", "Part" = "Part No-take", "None" = "Fished"))) %>%
  glimpse()

# eez <- mr_shp(key = "MarineRegions:eez")

oceans <- st_read(dsn = "C:/GitHub/Files to big for CheckEM/goas_v01.shp")

oceans_sf <- oceans %>%
  st_as_sf()

names(world_marineparks)
unique(world_marineparks$NAME)
unique(world_marineparks$status)

test <- world_marineparks %>% filter(status == "Part")
unique(world_marineparks$zone)

world_marineparks_single <- st_cast(world_marineparks, "POLYGON")
# 
# sf_use_s2(FALSE)
# world_marineparks_oceans <- st_intersection(world_marineparks, oceans_sf) %>%
#   st_set_geometry(NULL)
# 
# world_marineparks_oceans <- world_marineparks_oceans %>%
#   dplyr::rename(ocean_name = name)
# 
# saveRDS(world_marineparks_oceans, "data/world_marineparks_oceans.RDS")

world_marineparks_oceans <- readRDS("data/world_marineparks_oceans.RDS")

iucn.pal <- 
  leaflet::colorFactor(palette = c("#F7C0D8", "#b0d1a3", "#7BBC63", "#FDBA33", "#FFF8A3", "#6DAFE0", "#447191", 
                                   "#828282", "#828282", "#828282"), 
                       levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned"))

# FAO Major fishing regions ----

# TODO make these region names match the region names in the life history sheet
world.regions <- st_read(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas.shp") %>%
  st_transform(crs="+init=epsg:4326") %>%
  dplyr::mutate(REGION = str_replace_all(.$NAME_EN, c(", " = "_", " " =".")))

world.regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas.shp")
world.regions$REGION <- str_replace_all(world.regions$NAME_EN, c(", " = "_", " " ="."))
proj4string(world.regions) <- CRS(wgs.84)



unique(world.regions$NAME_EN) %>% sort()
unique(world.regions$REGION) %>% sort()

world.regions.display <- st_read(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas_with_coastline_single.shp") %>%
  st_transform(crs="+init=epsg:4326") 


# Schemas ----
schema.fish <- readxl::read_xlsx("data/schemas/fish.life.history.xlsx", 2)
schema.habitat <- read_tsv("data/schemas/benthic.habitat.annotation.schema.forward.facing.txt")
schema.relief <- read_tsv("data/schemas/benthic.relief.annotation.schema.forward.facing.txt")

all_data <- structure(
  list(
    wgs.84 = wgs.84,
    lh.aus = lh.aus,
    lh.aus.synonyms = all.synonyms,
    lh.aus.expanded = lh.aus.expanded,
    lh.aus.min.max = lh.aus.min.max,
    lh.glo = lh.glo,
    lh.glo.synonyms = lh.glo.synonyms,
    lh.glo.expanded = lh.glo.expanded,
    lh.glo.min.max = lh.glo.min.max,
    classes = classes,
    marineparks = marineparks,
    marineparks.single = marineparks.single,
    aus.regions = aus.regions,
    world.regions = world.regions,
    world.regions.display = world.regions.display,
    schema.fish = schema.fish,
    schema.habitat = schema.habitat,
    schema.relief = schema.relief,
    world_marineparks = world_marineparks,
    world_marineparks_oceans = world_marineparks_oceans,
    comm.pal = comm.pal,
    iucn.pal = iucn.pal
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))


# FOR WORKSHOP
# setwd("C:/GitHub/API-query/data/spatial")
# saveRDS(marineparks, "marineparks.RDS")

