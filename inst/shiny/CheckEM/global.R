# Load shiny packages
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)
# library(profvis)
library(shinycssloaders)

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
# library(rgdal)
# library(rgeos)
library(raster)
library(leaflet)
library(leaflet.minicharts)
library(leafgl)
library(rnaturalearth)

# Global archive functions
library(devtools)
# devtools::install_github('UWAMEGFisheries/GlobalArchive')
library(GlobalArchive)

# Googlesheets (for now)
library(googlesheets4)

# R markdown
library(rmarkdown)
library(markdown)
library(sf)

library(reactlog) # reactlog::reactlog_enable()

library(beepr)

# remotes::install_github("AllanCameron/geomtextpath")
library(geomtextpath)

library(png)
library(cowplot)

library(CheckEM)
sf_use_s2(FALSE)

# Load data
load("data/all_data.Rdata")

# TODO update once I have moved the LH workflow into CHECKEM
schema <- read_csv("data/benthic-annotation-schema-forward-facing.csv",
                   col_types = "c", na = "") %>%
  ga.clean.names() %>%
  dplyr::select(-c(qualifiers)) %>% #parent_caab, 
  dplyr::mutate(scientific = dplyr::case_when(
    !is.na(genus) ~ paste(genus, species)
  ))

dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                           tags$img(src='https://www.nespmarine.edu.au/sites/default/themes/nespmarine/logo.png',height='60',width='200'))

dbHeader <- dashboardHeader(title = "CheckEM",
                            
                            ## DOWNLOAD
                            tags$li(a(img(src = 'user-guide.png',
                                          title = "Download User Guide", height = "60px"),
                                      href = "CheckEM_user_guide.pdf", download = "CheckEM_user_guide.pdf",
                                    style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown"),
                            
                            tags$li(a(href = 'https://marineecology.io/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
                                          title = "Marine Ecology Group", height = "60px"),
                                     style = "padding-top:10px; padding-bottom:10px;"
                                     ),
                                    class = "dropdown"),
                            
                            tags$li(a(href = 'https://ardc.edu.au/',
                                      img(src = 'ardc.png',
                                          title = "ARDC", height = "60px"),
                                      style = "padding-top:10px; padding-bottom:10px;"
                                      ),
                                    class = "dropdown"),
                            
                            tags$li(a(href = 'https://www.nespmarine.edu.au/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/mac-logo-white-cropped.png?raw=true',
                                          title = "Marine and Coastal Hub", height = "60px"),
                                      style = "padding-top:10px; padding-bottom:10px;"
                                      ),
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

# Leaflet basemap

leaflet_basemap <- function(data = NULL, l_width = NULL, l_height = NULL) {
  leaflet::leaflet(data = data, width = l_width, height = l_height) %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
    leaflet::addProviderTiles(
      "OpenStreetMap.Mapnik",
      group = "Basemap",
      options = leaflet::providerTileOptions(opacity = 0.35)
    ) %>%
    # leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE, position = "bottomright") %>%
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(
        imperial = FALSE,
        maxWidth = 200
      )
    ) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
}

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

iconSet <- awesomeIconList(
  'Fished' = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'blue'
  ),
  'No-take' = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'green'
  ),
  'Part No-take' = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'yellow'
  )
)

icon.on.land <- awesomeIconList(
  'Land' = makeAwesomeIcon(
    icon = 'surf',
    iconColor = 'white',
    markerColor = 'orange'
  )
)
