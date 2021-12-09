# Load shiny packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(profvis)

# Tables
library(rpivotTable)

# Data manipulation
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(fuzzyjoin)
library(forcats)
library(fst)

# Plotting
library(ggplot2)
library(grid)

# Maps
library(rgdal)
library(rgeos)
library(raster)
library(leaflet)
library(leaflet.minicharts)

# Global archive functions
library(devtools)
# devtools::install_github('UWAMEGFisheries/GlobalArchive')
library(GlobalArchive)

# Googlesheets (for now)
library(googlesheets4)

# R markdown
library(rmarkdown)
library(sf)

# https://geospatial.tnc.org/datasets/ed2be4cf8b7a425f84fd093c2e7660e3_0/explore?location=-1.040302%2C0.000000%2C1.57 # the MEOW
# https://academic.oup.com/bioscience/article/57/7/573/238419
# https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions

dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                           tags$img(src='https://www.nespmarine.edu.au/sites/default/themes/nespmarine/logo.png',height='60',width='200'))

dbHeader <- dashboardHeader(title = "CheckEM",
                            tags$li(a(href = 'https://marineecology.io/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
                                          title = "Marine Ecology Group", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.nespmarine.edu.au/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/mbh-logo-white-cropped.png?raw=true',
                                          title = "Marine Biodiversity Hub", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))





# Read in life history sheet ----
master<-read_csv("data/australia.life.history_211021.csv")%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  #filter(grepl('NW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm,australian.common.name)%>%
  distinct()%>%
  dplyr::mutate(marine.region=str_replace_all(.$marine.region,c("N/"="North/",
                                                                "NW"="North-west",
                                                                "CS"="Coral Sea",
                                                                "TE"="Temperate East",
                                                                "SE"="South-east",
                                                                "SW"="South-west",
                                                                "N"="North",
                                                                "Northorth"="North",
                                                                "Northor"="North")))%>%
  glimpse()

classes<-read_csv("data/australia.life.history_211021.csv")%>%
  ga.clean.names()%>%
  distinct(class,order,family,genus,species)
  

# Expand life history for checking regions ----
master.expanded<-master%>%
  mutate(marine.region = strsplit(as.character(marine.region), split = "/"))%>%
  unnest(marine.region)

unique(master.expanded$marine.region)%>%sort()

# Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---
family.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max))%>%
  ungroup()

genus.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(genus)%>%
  dplyr::summarise(genuslength_max=mean(fb.length_max))%>%
  ungroup()

# Create a new master list with family and genus average maximum length where missing species max.length ----
master.min.max<-left_join(master,family.max.length,by=c("family"))%>% # add in family values
  left_join(.,genus.max.length)%>% # add in genus values
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),genuslength_max,fb.length_max))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max,genuslength_max))%>%
  mutate(min.length=0.15*fb.length_max)%>% # change values here
  mutate(max.length=0.85*fb.length_max)%>% # change values here
  glimpse()

# Synonyms ----
synonyms <- read_csv("data/synonyms_200813.csv")%>%
  distinct()%>%
  ga.clean.names()%>%
  dplyr::select(-comment)%>%
  glimpse()

# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

marine.regions <- readOGR(dsn="data/spatial/marine_regions.shp")
proj4string(marine.regions)
marine.regions$REGION<-as.character(marine.regions$REGION)

commonwealth.marineparks <- readOGR(dsn="data/spatial/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)

wa.marineparks <- readOGR(dsn="data/spatial/WA_MPA_2018.shp")
proj4string(wa.marineparks)

proj4string(marine.regions)<-CRS(wgs.84)
proj4string(commonwealth.marineparks)<-CRS(wgs.84)
proj4string(wa.marineparks)<-CRS(wgs.84)

# Theme for plotting ----
Theme1 <-    theme_bw()+
  theme( # use theme_get() to see available options
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))

theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major=element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

#australia.regions <- SpatialPolygons(marine.regions@polygons)
#test <- SpatialPolygonsDataFrame(australia.regions, data = data.frame(australia.regions$polygons))

world.regions<-st_read(dsn = "data/spatial/MEOW.shp") %>%
  st_transform(crs="+init=epsg:4326") 

#world.regions <- st_transform(world.regions,)
#proj4string(world.regions)<-CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#world.regions <- SpatialPolygons(world@polygons)
#
# length.sample <- length%>%distinct(campaignid,sample) # only examine samples where lengths were possible
# 
# summarise length and then compare to maxn
# taxa.maxn.vs.stereo.summary<-length%>%
#   group_by(campaignid,sample,family,genus,species)%>%
#   dplyr::summarise(stereo.maxn=sum(number))%>%
#   full_join(maxn)%>%
#   replace_na(list(maxn=0))%>%
#   filter(!stereo.maxn==maxn)%>%
#   mutate(percent.difference = (maxn-stereo.maxn)/maxn*100)%>%
#   semi_join(length.sample)%>% # only keep ones where length was possible
#   replace_na(list(percent.difference=1))%>%
#   filter(!percent.difference%in%c(0))%>% #only for those that have missing lengths
#   mutate(difference=(maxn-stereo.maxn))%>%
#   mutate(difference=abs(difference))%>%
#   mutate(percent.difference=abs(percent.difference))%>%
#   select(campaignid,sample,family,genus,species,maxn,stereo.maxn,difference,percent.difference)%>%
#   arrange(-difference)%>%
#   glimpse()

# profvis(shiny::runApp())
