rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to GitHub
library(RCurl)
library(R.utils)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# to connect to googlesheets
library(googlesheets4)
library(sf)
library(terra)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study <- "Parks-Ningaloo-synthesis"                                             # Change here

## Set your working directory ----
working.dir <- getwd()

## Save these directory names to use later----
staging.dir <- paste(working.dir,"data/staging",sep="/") 
download.dir <- paste(working.dir,"data/raw/EM Export",sep="/")
tidy.dir <- paste(working.dir,"data/tidy",sep="/")

setwd(working.dir)

# Metadata ----
metadata <- ga.list.files("_Metadata.csv") %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::select(campaignid,sample,latitude,longitude,date,time,location,status,
                site,depth,observer,successful.count,successful.length) %>%
  dplyr::filter(campaignid %in% c("2019-08_Ningaloo_stereo-BRUVs",
                                  "2022-05_PtCloates_stereo-BRUVS")) %>%    # Add new campaignids when data gets finished
  glimpse()

# Join with status
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
wgscrs <- "+proj=longlat +datum=WGS84"
# Load maxn and length as sf
metadatav <- st_as_sf(metadata, coords = c("longitude", "latitude"), crs = wgscrs)

# Load in sanctuary zones as sf
# State SZs
# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
wampa <- st_transform(wampa, wgscrs)
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
aumpa <- st_transform(aumpa, wgscrs)

metadata.commonwealth <- metadatav %>%
  st_intersection(aumpa %>% dplyr::select(geometry, ZoneName)) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude = X, latitude = Y) %>%
  dplyr::mutate(status = ifelse(ZoneName %in% "National Park Zone", "No-take", "Fished"),
                commonwealth = "state") %>%
  glimpse()

metadata.state <- metadatav %>%
  st_intersection(wampa %>% dplyr::select(geometry, waname)) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude = X, latitude = Y) %>%
  dplyr::mutate(status = ifelse(waname %in% "Sanctuary Zone", "No-take", "Fished"),
                commonwealth = "State") %>%
  dplyr::filter(!status %in% "No-take") %>%                                     # Remove state SZs                                 
  glimpse()

metadata <- bind_rows(metadata.commonwealth, metadata.state) %>% 
  glimpse()

unique(metadata$campaignid) # check the number of campaigns in metadata, and the campaign name

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
points.files <- ga.list.files("_Points.txt") # list all files ending in "Lengths.txt"
points.files$lines <- sapply(points.files,countLines) # Count lines in files (to avoid empty files breaking the script)
points <- as.data.frame(points.files) %>%
  dplyr::mutate(campaign=row.names(.))%>%
  filter(lines>1)%>% # filter out all empty text files
  dplyr::select(campaign)%>%
  as_vector(.)%>% # remove all empty files
  purrr::map_df(~ga.read.files_em.txt(.)) %>%
  dplyr::filter(campaignid %in% c("2019-08_Ningaloo_stereo-BRUVs",
                                  "2022-05_PtCloates_stereo-BRUVS")) 

maxn <- points %>%
  dplyr::group_by(campaignid,sample,filename,periodtime,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.count %in% c("Y", "Yes", "yes", "y"))%>%
  dplyr::filter(maxn>0)%>%
  glimpse()

test <- metadata %>%
  anti_join(maxn) %>%
  glimpse() # Only ones left are successful.count = "No"

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

unique(maxn$sample)

## Combine Length, Lengths and 3D point files into length3dpoints----
length3dpoints <- ga.create.em.length3dpoints()%>%
  dplyr::select(-c(time,comment))%>% # take time out as there is also a time column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.length %in% c("Y", "Yes", "yes", "y"))%>%
  glimpse()

unique(length3dpoints$sample)

## Save length files ----
setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

setwd(working.dir)
