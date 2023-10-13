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
