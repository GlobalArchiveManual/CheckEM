library(tidyverse)
library(googlesheets4)
library(GlobalArchive)
library(rgdal)
library(sf)
library(RCurl)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth()
2

# TODO change these to github repo for Australia LH and synonyms 
# TODO change australian.common.name to common.name
# Read in sheet from googledrive ----
aus.url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"

# Read in life history sheet ----
lh.aus <- read_sheet(aus.url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>%
  dplyr::mutate(all=as.numeric(all)) %>%
  dplyr::mutate(bll=as.numeric(bll)) %>%
  dplyr::mutate(a=as.numeric(a)) %>%
  dplyr::mutate(b=as.numeric(b)) %>%
  dplyr::rename(code = caab) %>%
  dplyr::select(code, family, genus, species, marine.region, length.measure, a, b, all, bll, fb.length_max, fb.ltypemaxm, australian.common.name) %>%
  distinct() %>%
  dplyr::mutate(marine.region = str_replace_all(.$marine.region,c("N/" = "North/",
                                                                  "NW" = "North-west",
                                                                  "CS" = "Coral Sea",
                                                                  "TE" = "Temperate East",
                                                                  "SE" = "South-east",
                                                                  "SW" = "South-west",
                                                                  "N" = "North",
                                                                  "Northorth" = "North",
                                                                  "Northor" = "North")))
classes <- read_sheet(aus.url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>%
  distinct(class, order, family, genus, species)

# Expand life history for checking regions ----
lh.aus.expanded <- lh.aus %>%
  mutate(marine.region = strsplit(as.character(marine.region), split = "/"))%>%
  unnest(marine.region)

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
lh.aus.family.max.length <- lh.aus %>%
  filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max = mean(fb.length_max)) %>%
  ungroup()

lh.aus.genus.max.length <- lh.aus %>%
  filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max = mean(fb.length_max)) %>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
lh.aus.min.max <- left_join(lh.aus, lh.aus.family.max.length, by = c("family")) %>% # add in family values
  left_join(., lh.aus.genus.max.length) %>% # add in genus values
  dplyr::mutate(fb.length_max = ifelse((is.na(fb.length_max)), genuslength_max, fb.length_max)) %>%
  dplyr::mutate(fb.length_max = ifelse((is.na(fb.length_max)), famlength_max, fb.length_max)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min.length = 0.15 * fb.length_max) %>% # change values here
  mutate(max.length = 0.85 * fb.length_max) %>% # change values here
  glimpse()

# Read in global life history list ----
lh.glo <- read.csv("https://raw.githubusercontent.com/GlobalArchiveManual/annotation-schema/main/output/CheckEM/global_fish.life.history.csv") %>%
  dplyr::filter(ismarine %in% 1) %>%
  dplyr::select(class, order, family, genus, species, common.name, fao.fishing.area.endemic, fao.fishing.area.introduced, fao.fishing.area.native, length.max, length.at.maturity.cm) %>%
  dplyr::mutate(marine.region = paste(fao.fishing.area.endemic, fao.fishing.area.introduced, fao.fishing.area.native, sep = ", ")) %>%
  dplyr::mutate(marine.region = str_replace_all(.$marine.region, c("NA, " = "", "NA" = "", "\n" = ""))) %>%
  glimpse()

names(lh.aus) %>% sort()
names(lh.glo) %>% sort()

unique(lh.glo$marine.region)

# Expand life history for checking regions ----
lh.glo.expanded <- lh.glo %>%
  mutate(marine.region = strsplit(as.character(marine.region), split = ", "))%>%
  unnest(marine.region)

unique(lh.glo.expanded$marine.region) %>% sort()

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
  dplyr::mutate(fb.length_max = ifelse((is.na(length.max)), genuslength_max, length.max)) %>%
  dplyr::mutate(fb.length_max = ifelse((is.na(length.max)), famlength_max, fb.length_max)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min.length = 0.15 * fb.length_max) %>% # change values here
  mutate(max.length = 0.85 * fb.length_max) %>% # change values here
  glimpse()

# Synonyms ----
lh.aus.synonyms <- read_sheet(aus.url, sheet = "synonyms") %>%
  distinct() %>%
  ga.clean.names() %>%
  dplyr::select(-comment) %>%
  glimpse()

# Need to remove these from the synonym list
lh.glo.ambiguous.synonyms <- read.csv("https://raw.githubusercontent.com/GlobalArchiveManual/annotation-schema/main/output/CheckEM/global_fish.ambiguous.synonyms.csv") %>%
  dplyr::rename(synonym = valid.name.too.but.also.a.synonym.for.correct.name) %>%
  dplyr::rename(scientific = correct_name) %>%
  dplyr::select(-X) %>%
  dplyr::mutate(remove = TRUE)

regions.for.synonyms <- lh.glo.expanded %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct(scientific, marine.region)

lh.glo.synonyms <- read.csv("https://raw.githubusercontent.com/GlobalArchiveManual/annotation-schema/main/output/CheckEM/global_fish.synonyms.csv") %>%
  full_join(regions.for.synonyms) %>%
  dplyr::select(-X) %>%
  distinct() %>%
  dplyr::filter(!is.na(marine.region)) %>%
  # full_join(lh.glo.ambiguous.synonyms) %>%
  # dplyr::filter(!remove %in% TRUE) %>%
  glimpse()


# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

aus.regions <- readOGR(dsn = "data/spatial/marine_regions.shp")
aus.regions$REGION <- as.character(aus.regions$REGION)
proj4string(aus.regions) <- CRS(wgs.84)

# New CAPAD 2020 ----
marineparks <- readOGR(dsn = "data/spatial/CAPAD2020_marine_single_fixed2.shp")
proj4string(marineparks) <- CRS(wgs.84)

# Buffer in QLD - has some fishing allowed (Trolling)
# These allow fishing:
# - Habitat Protection
# - General Use

marineparks$status <- if_else(str_detect(pattern = "No take|Sanctuary|Conservation Area|Marine National Park|Preservation|National Park|	
Scientific Research|Scientific Reference", string = marineparks$ZONE_TYPE),
                              "No-take",
                              "Fished")
marineparks.single <- st_read("data/spatial/CAPAD2020_marine_single_fixed2.shp")
marineparks.single <- st_as_sf(marineparks)

# FAO Major fishing regions ----

# TODO make these region names match the region names in the life history sheet
world.regions <- st_read(dsn = "data/spatial/FAO_major_areas.shp") %>%
  st_transform(crs="+init=epsg:4326") %>%
  dplyr::mutate(REGION = str_replace_all(.$NAME_EN, c(", " = "_", " " =".")))

world.regions <- readOGR(dsn = "data/spatial/FAO_major_areas.shp")
world.regions$REGION <- str_replace_all(world.regions$NAME_EN, c(", " = "_", " " ="."))
proj4string(world.regions) <- CRS(wgs.84)



unique(world.regions$NAME_EN) %>% sort()
unique(world.regions$REGION) %>% sort()

world.regions.display <- st_read(dsn = "data/spatial/FAO_major_areas_with_coastline_single.shp") %>%
  st_transform(crs="+init=epsg:4326") 


# Schemas ----
schema.fish <- readxl::read_xlsx("data/schemas/fish.life.history.xlsx", 2)
schema.habitat <- read_tsv("data/schemas/benthic.habitat.annotation.schema.forward.facing.txt")
schema.relief <- read_tsv("data/schemas/benthic.relief.annotation.schema.forward.facing.txt")

all_data <- structure(
  list(
    wgs.84 = wgs.84,
    lh.aus = lh.aus,
    lh.aus.synonyms = lh.aus.synonyms,
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
    schema.relief = schema.relief
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))


# FOR WORKSHOP
# setwd("C:/GitHub/API-query/data/spatial")
# saveRDS(marineparks, "marineparks.RDS")
