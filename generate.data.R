library(tidyverse)
library(googlesheets4)
library(GlobalArchive)
library(rgdal)
library(sf)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?usp=sharing"

lh <- read_sheet(url)

# Read in life history sheet ----
master <- lh %>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region)) %>%
  dplyr::mutate(all=as.numeric(all)) %>%
  dplyr::mutate(bll=as.numeric(bll)) %>%
  dplyr::mutate(a=as.numeric(a)) %>%
  dplyr::mutate(b=as.numeric(b)) %>%
  dplyr::select(family, genus, species, marine.region, length.measure, a, b, all, bll, fb.length_max, fb.ltypemaxm, australian.common.name) %>%
  distinct() %>%
  dplyr::mutate(marine.region = str_replace_all(.$marine.region,c("N/" = "North/",
                                                                  "NW" = "North-west",
                                                                  "CS" = "Coral Sea",
                                                                  "TE" = "Temperate East",
                                                                  "SE" = "South-east",
                                                                  "SW" = "South-west",
                                                                  "N" = "North",
                                                                  "Northorth" = "North",
                                                                  "Northor" = "North"))) %>%
  glimpse()

classes <- lh %>%
  ga.clean.names() %>%
  distinct(class, order, family, genus, species)

# Expand life history for checking regions ----
master.expanded <- master %>%
  mutate(marine.region = strsplit(as.character(marine.region), split = "/"))%>%
  unnest(marine.region)

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
family.max.length <- master %>%
  filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max = mean(fb.length_max)) %>%
  ungroup()

genus.max.length <- master %>%
  filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max = mean(fb.length_max)) %>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
master.min.max <- left_join(master, family.max.length, by = c("family")) %>% # add in family values
  left_join(., genus.max.length) %>% # add in genus values
  dplyr::mutate(fb.length_max = ifelse((is.na(fb.length_max)), genuslength_max, fb.length_max)) %>%
  dplyr::mutate(fb.length_max = ifelse((is.na(fb.length_max)), famlength_max, fb.length_max)) %>%
  dplyr::select(-c(famlength_max, genuslength_max)) %>%
  mutate(min.length = 0.15 * fb.length_max) %>% # change values here
  mutate(max.length = 0.85 * fb.length_max) %>% # change values here
  glimpse()

# Synonyms ----
synonyms <- read_sheet(url, sheet = "synonyms") %>%
  distinct() %>%
  ga.clean.names() %>%
  dplyr::select(-comment) %>%
  glimpse()

# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

marine.regions <- readOGR(dsn = "data/spatial/marine_regions.shp")
marine.regions$REGION <- as.character(marine.regions$REGION)
proj4string(marine.regions) <- CRS(wgs.84)

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

# TODO change to FAO major fishing areas
world.regions <- st_read(dsn = "data/spatial/MEOW.shp") %>%
  st_transform(crs="+init=epsg:4326") 

all_data <- structure(
  list(
    wgs.84 = wgs.84,
    master = master,
    synonyms = synonyms,
    master.expanded = master.expanded,
    master.min.max = master.min.max,
    classes = classes,
    marineparks = marineparks,
    marineparks.single = marineparks.single,
    marine.regions = marine.regions,
    world.regions = world.regions
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))
