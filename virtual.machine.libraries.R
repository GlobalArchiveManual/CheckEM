# List of all libraries used in shiny apps so far
devtools
dplyr
DT
forcats
fst
fuzzyjoin
ggplot2
GlobalArchive
googlesheets4
grid
htmlwidgets
leaflet
leaflet.minicharts
leafpop
mapview
naniar
plyr
profvis
raster
readr
rgdal
rgeos
rmapshaper
rpivotTable
sf
shiny
shinyBS
shinybusy
shinydashboard
shinyjs
shinythemes
stringr
tidyr

# To add libraries to VM shiny server. Use Ctrl + Alt + Enter to run in terminal
# For GlobalArchive
sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"devtools::install_github('UWAMEGFisheries/GlobalArchive')\""

# For all others
sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('DT', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('forcats', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('fst', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('fuzzyjoin', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('googlesheets4', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('grid', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('htmlwidgets', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('leaflet', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('leaflet.minicharts', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('leafpop', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('mapview', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('naniar', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('plyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('profvis', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('raster', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('readr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rgdal', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rgeos', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rmapshaper', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rpivotTable', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('sf', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinyBS', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinybusy', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinydashboard', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinyjs', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinythemes', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('stringr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyr', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('shinyMobile', repos='http://cran.rstudio.com/')\""

