[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Hits](hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2FGlobalArchiveManual%2FCheckEM&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=views&edge_flat=false)](https://hits.seeyoufarm.com)

# CheckEM
CheckEM is an R package and [shiny app](marine-ecology.shinyapps.io/CheckEM/) for checking, visualizing and analysing stereo-video annotation data.

The idea behind CheckEM is to provide fish and benthic ecologists with a set of tools to quickly and effectively quality control, format and analyse their data collected using stereo-video methods. 

Here we have provided:

* R workflows for fish and benthic annotation data sets using the functions in CheckEM.
* A web based app hosted on shinyapps.io to enable users of stereo-video who aren't familiar with R to quality control and format their annotations
* A [user guide for the CheckEM shiny app](globalarchivemanual.github.io/CheckEM/docs/CheckEM_user_guide.pdf)
* Annotation guides for [EventMeasure](globalarchivemanual.github.io/CheckEM/docs/EventMeasure_annotation_guide.pdf) and [TransectMeasure](globalarchivemanual.github.io/CheckEM/docs/TransectMeasure_annotation_guide.pdf). 

We hope that our efforts to standardise the annotation and QC of stereo-video data will result in data which meet the [FAIR principles](ardc.edu.au/resource/fair-data/) of interoperability and reusability.

## To install in R
```
install.packages('devtools')
library('devtools')

devtools::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
```

## CheckEM is available as a shiny app
CheckEM is available as a web based app hosted on shinyapps.io and can be accessed through [this link](https://marine-ecology.shinyapps.io/CheckEM/)
You can run a local version of CheckEM (without the internet) using R via:

```
CheckEM::runCheckEM()
```
