[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2FGlobalArchiveManual%2FCheckEM&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=views&edge_flat=false)](https://hits.seeyoufarm.com)

# [CheckEM](https://marine-ecology.shinyapps.io/CheckEM/)
A package for checking, visualizing, formatting and analysing stereo-video annotation data.

## To install in r 
```
install.packages('devtools')
library('devtools')

devtools::install_github("GlobalArchiveManual/CheckEM")
```

## CheckEM is available as a shiny app
To enable users of stereo-video who aren't familiar with R to check their annotations CheckEM is avilable as a web based app hosted on shinyapps.io and can be accessed through [this link](https://marine-ecology.shinyapps.io/CheckEM/)

You can run a local version of CheckEM (without the internet) using R via:
```
CheckEM::runCheckEM()
```
