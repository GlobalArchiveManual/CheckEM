# Load shiny packages
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(profvis)

# Tables
library(rpivotTable)
library(data.table)

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
library(ggbeeswarm) # NEW 15/07/2022
library(RColorBrewer)# NEW 15/07/2022

# Maps
library(rgdal)
library(rgeos)
library(raster)
library(leaflet)
library(leaflet.minicharts)
library(leafgl)

# Global archive functions
library(devtools)
# devtools::install_github('UWAMEGFisheries/GlobalArchive')
library(GlobalArchive)

# Googlesheets (for now)
library(googlesheets4)

# R markdown
library(rmarkdown)
library(sf)

library(reactlog) # reactlog::reactlog_enable()

# Load data
load("data/all_data.Rdata")

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
                            
                            tags$li(a(href = 'https://ardc.edu.au/',
                                      img(src = 'ardc.png',
                                          title = "ARDC", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            
                            tags$li(a(href = 'https://www.nespmarine.edu.au/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/mbh-logo-white-cropped.png?raw=true',
                                          title = "Marine Biodiversity Hub", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

# Theme for plotting ----
Theme1 <-    theme_bw() +
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
    axis.title.x = element_text(vjust = 0.3, size = 12),
    axis.title.y = element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.x = element_line(colour = "black", size = 0.5,linetype = 'solid'),
    axis.line.y = element_line(colour = "black", size = 0.5,linetype = 'solid'),
    strip.background = element_blank(),
    plot.title = element_text(color = "black", size = 12, face = "bold.italic"))

theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major = element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor = element_line(colour = "white", size = 0.25), 
  plot.margin = grid::unit(c(0, 0, 0, 0), "in"))

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

all_data$marine.regions

iconSet <- awesomeIconList(
  Fished = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'blue'
  ),
  'No-take' = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'green'
  )
)