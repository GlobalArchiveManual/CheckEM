library(tidyverse)
library(googlesheets4)
library(GlobalArchive)
# library(rgdal)
library(sf)
library(RCurl)
library(mregions2)
library(rnaturalearth)

options(timeout=9999999)
remotes::install_github("GlobalArchiveManual/CheckEM")

library(CheckEM)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth()
2

# TODO change these to github repo for Australia LH and synonyms 
# TODO change australian.common.name to common.name
# Read in sheet from googledrive ----
aus.url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"

lh.aus <- CheckEM::australia_life_history %>%
  # dplyr::mutate(marine_region = str_replace_all(.$marine_region, c("N/" = "North/",
  #                                                                  "NW" = "North-west",
  #                                                                  "CS" = "Coral Sea",
  #                                                                  "TE" = "Temperate East",
  #                                                                  "SE" = "South-east",
  #                                                                  "SW" = "South-west",
  #                                                                  "N" = "North",
  #                                                                  "Northorth" = "North",
  #                                                                  "Northor" = "North"))) %>%
  dplyr::mutate(all = as.numeric(fb_a_ll)) %>%
  dplyr::mutate(bll = as.numeric(fb_b_ll)) %>%
  dplyr::mutate(a = as.numeric(fb_a)) %>%
  dplyr::mutate(b = as.numeric(fb_b)) %>%
  dplyr::mutate(length_max_mm = length_max_cm * 10) %>%
  dplyr::rename(code = caab_code) 


spp.lengths <- lh.aus %>%
  dplyr::filter(!is.na(length_max_mm)) %>%
  dplyr::group_by(family, genus) %>%
  dplyr::summarise(average.max = mean(length_max_mm)) %>%
  ungroup()

lh.aus <- left_join(lh.aus, spp.lengths) %>%
  dplyr::mutate(length_max_mm = if_else(is.na(length_max_mm), average.max, length_max_mm))

classes <- lh.aus %>%
  ga.clean.names() %>%
  distinct(class, order, family, genus, species)

# Expand life history for checking regions ----
lh.aus.expanded <- lh.aus %>%
  mutate(marine_region = strsplit(as.character(marine_region), split = ", "))%>% # changed from "/" for old LH
  unnest(marine_region)

lh.imcra.expanded <- lh.aus %>%
  mutate(marine_region = strsplit(as.character(imcra_region), split = ", "))%>% # changed from "/" for old LH
  unnest(marine_region)

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
lh.aus.family.max.length <- lh.aus %>%
  filter(!is.na(length_max_mm)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max = mean(length_max_mm)) %>%
  ungroup()

lh.aus.genus.max.length <- lh.aus %>%
  filter(!is.na(length_max_mm)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max = mean(length_max_mm)) %>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
lh.aus.min.max <- left_join(lh.aus, lh.aus.family.max.length, by = c("family")) %>% # add in family values
  left_join(., lh.aus.genus.max.length) %>% # add in genus values
  dplyr::mutate(length_max_mm = ifelse((is.na(length_max_mm)), genuslength_max, length_max_mm)) %>%
  dplyr::mutate(length_max_mm = ifelse((is.na(length_max_mm)), famlength_max, length_max_mm)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min_length = 0.15 * length_max_mm) %>% # change values here
  mutate(max_length = 0.85 * length_max_mm) %>% # change values here
  glimpse()

load("data/global_life_history.rda")

# Read in global life history list ----
lh.glo <- global_life_history %>%
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
  # dplyr::left_join(extra.lengths) %>%
  # tidyr::replace_na(list(new.maximum.length = 0)) %>%
  # dplyr::mutate(length.max = if_else(new.maximum.length > length.max, new.maximum.length, length.max)) %>%
  # dplyr::mutate(length.max = length.max * 10) %>%
  # dplyr::select(-new.maximum.length) %>%
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
  mutate(min_length = 0.15 * fb_length_max * 10) %>% # change values here
  mutate(max_length = 0.85 * fb_length_max * 10) %>% # change values here
  glimpse()


# Synonyms ----


lh.aus.synonyms <- CheckEM::aus_synonyms
all.synonyms <- CheckEM::aus_synonyms

# Need to remove these from the synonym list
# lh.glo.ambiguous.synonyms <- readRDS("data/global_fish.ambiguous.synonyms.RDS") %>%
#   dplyr::rename(synonym = valid.name.too.but.also.a.synonym.for.correct.name) %>%
#   dplyr::rename(scientific = correct_name) %>%
#   dplyr::mutate(remove = TRUE)

regions.for.synonyms <- lh.glo.expanded %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct(scientific, marine_region)

lh.glo.synonyms <- readRDS("inst/shiny/CheckEM/data/global_fish.synonyms.RDS") %>%
  full_join(regions.for.synonyms) %>%
  distinct() %>%
  dplyr::filter(!is.na(marine_region)) %>%
  # full_join(lh.glo.ambiguous.synonyms) %>%
  # dplyr::filter(!remove %in% TRUE) %>%
  glimpse()


# Spatial files ----

# wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# aus.regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/marine_regions.shp")
# aus.regions$REGION <- as.character(aus.regions$REGION)
# proj4string(aus.regions) <- CRS(wgs.84)
# 
# aus.regions <- C

aus.regions <- CheckEM::aus_regions

world.regions <- CheckEM::world_regions %>% 
  st_as_sf() %>%
  st_cast("POLYGON")

imcra.regions <- CheckEM::imcra_regions

# New CAPAD 2022 ----
iucn_levels <- c("Ia", "II", "III", "IV", "VI", "V", "NA")


marineparks <- st_read(here::here("r-workflows/data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"))  %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  dplyr::mutate(IUCN = fct_relevel(IUCN, iucn_levels))

names(marineparks)

unique(marineparks$IUCN)

marineparks$status <- if_else(str_detect(pattern = "No take|Sanctuary|Conservation Area|Marine National Park|Preservation|National Park|	
Scientific Research|Scientific Reference", string = marineparks$ZONE_TYPE),
"No-take",
"Fished")

marineparks.single <- st_read(here::here("r-workflows/data/spatial/shapefiles/CAPAD2022_single_fixed.shp"))
marineparks.single <- st_as_sf(marineparks.single)

marineparks.single <- marineparks.single %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = ZONE_TYPE) ~ "Sanctuary",
    str_detect(pattern = "IUCN II", string = ZONE_TYPE) ~ "Marine National Park",
    str_detect(pattern = "National Park", string = ZONE_TYPE) ~ "Marine National Park",
    str_detect(pattern = "Recreational|Recreation", string = ZONE_TYPE) ~ "Recreational Use",
    str_detect(pattern = "Habitat Protection", string = ZONE_TYPE) ~ "Habitat Protection",
    str_detect(pattern = "Special Purpose", string = ZONE_TYPE) ~ "Special Purpose",
    str_detect(pattern = "Multiple Use", string = ZONE_TYPE) ~ "Multiple Use",
    str_detect(pattern = "General", string = ZONE_TYPE) ~ "General Use"))%>%
  dplyr::mutate(IUCN = fct_relevel(IUCN, iucn_levels))
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
world_marineparks <- readRDS(here::here("r-workflows/data/spatial/shapefiles/WDPA_WDOECM_Jul2023_Public_marine.RDS")) %>%
  dplyr::filter(MARINE %in% 2) %>%
  dplyr::rename(status = NO_TAKE, zone = IUCN_CAT) %>%
  dplyr::select(NAME, zone, status, geometry) %>%
  dplyr::mutate(status = str_replace_all(status, c("All" = "No-take", "Part" = "Part No-take", "None" = "Fished"))) %>%
  glimpse()

# eez <- mr_shp(key = "MarineRegions:eez")

oceans <- st_read(here::here("r-workflows/data/spatial/shapefiles/goas_v01.shp"))

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
# saveRDS(world_marineparks_oceans, here::here("r-workflows/data/spatial/shapefiles/world_marineparks_oceans.RDS"))

world_marineparks_oceans <- readRDS("r-workflows/data/spatial/shapefiles/world_marineparks_oceans.RDS")

iucn.pal <- 
  leaflet::colorFactor(palette = c("#F7C0D8", "#b0d1a3", "#7BBC63", "#FDBA33", "#FFF8A3", "#6DAFE0", "#447191", 
                                   "#828282", "#828282", "#828282"), 
                       levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned"))

# FAO Major fishing regions ----

# TODO make these region names match the region names in the life history sheet
world.regions <- st_read(here::here("r-workflows/data/spatial/shapefiles/FAO_major_areas.shp")) %>%
  st_transform(crs="+init=epsg:4326") %>%
  dplyr::mutate(REGION = str_replace_all(.$NAME_EN, c(", " = "_", " " =".")))

# world.regions <- readOGR(dsn = "C:/GitHub/Files to big for CheckEM/spatial/FAO_major_areas.shp")
# world.regions$REGION <- str_replace_all(world.regions$NAME_EN, c(", " = "_", " " ="."))
# proj4string(world.regions) <- CRS(wgs.84)

unique(world.regions$NAME_EN) %>% sort()
unique(world.regions$REGION) %>% sort()

world.regions.display <- st_read(here::here("r-workflows/data/spatial/shapefiles/FAO_major_areas_with_coastline_single.shp")) %>%
  st_transform(crs="+init=epsg:4326") 


# Schemas ----
schema.fish <- readxl::read_xlsx("inst/shiny/CheckEM/data/schemas/fish.life.history.xlsx", 2)
schema.habitat <- read_tsv("inst/shiny/CheckEM/data/schemas/benthic.habitat.annotation.schema.forward.facing.txt")
schema.relief <- read_tsv("inst/shiny/CheckEM/data/schemas/benthic.relief.annotation.schema.forward.facing.txt")

# Australia land shapefile ----
aus <- st_read(here::here("H:/Files to big for CheckEM/spatial/aus-shapefile-w-investigator-stokes.shp")) %>%
  st_transform(crs="+init=epsg:4326") %>%
  dplyr::mutate(land = "land")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::mutate(land = "land")

# plot(aus)
# plot(world)

all_data <- structure(
  list(
    # wgs.84 = wgs.84,
    lh.aus = lh.aus,
    lh.aus.synonyms = all.synonyms,
    lh.aus.expanded = lh.aus.expanded,
    lh.imcra.expanded = lh.imcra.expanded,
    lh.aus.min.max = lh.aus.min.max,
    lh.glo = lh.glo,
    lh.glo.synonyms = lh.glo.synonyms,
    lh.glo.expanded = lh.glo.expanded,
    lh.glo.min.max = lh.glo.min.max,
    classes = classes,
    marineparks = marineparks,
    # marineparks.single = marineparks.single,
    aus.regions = aus.regions,
    world.regions = world.regions,
    imcra.regions = imcra.regions,
    # world.regions.display = world.regions.display,
    schema.fish = schema.fish,
    schema.habitat = schema.habitat,
    schema.relief = schema.relief,
    # world_marineparks = world_marineparks,
    # world_marineparks_oceans = world_marineparks_oceans,
    # comm.pal = comm.pal,
    iucn.pal = iucn.pal,
    world = world#,
    # aus = aus
    
  ),
  class = "data"
)

save(all_data, file = here::here("inst/shiny/CheckEM/data/all_data.Rdata"))


# FOR WORKSHOP
# setwd("C:/GitHub/API-query/data/spatial")
# saveRDS(marineparks, "marineparks.RDS")

