# Check fish data from single point based methods

## Introduction

This script will take fish annotation data from single point based
methods (e.g. stereo-BRUVs or stereo-BOSS), either exported from
EventMeasure or “Generic” format (see format requirements
[here](https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html)),
and check for any errors in the annotation. The script will then format
the annotation data into a tidy format.

You also need a formatted sample metadata file “\*\_metadata.csv” (see
format requirements
[here](https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html))

## R set up

First you will need to load the necessary libraries. If you haven’t
installed CheckEM before you will need to install CheckEM using the
install_github function.

``` r
# install.packages('remotes')
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(googlesheets4)
library(sf)
library(terra)
library(here)
```

Next set the study name. This can be any name you like, all files saved
using this script will be prefixed with this name. Avoid having a name
that is too long. We recommend using a short project name that includes
the method e.g. “2020_ningaloo_stereo-BRUVs”.

``` r
name <- "example-bruv-workflow"
```

## Metadata

Now we load and tidy the metadata. If you have already completed this
step while checking your habitat data, then you can skip these next two
chunks of code and simply read in the metadata (see below).

``` r
dir(here::here("r-workflows/data/raw/"))
```

    ##  [1] "2022-05_PtCloates_stereo-BRUVS_count.csv"                            
    ##  [2] "2022-05_PtCloates_stereo-BRUVS_length.csv"                           
    ##  [3] "2022-05_PtCloates_stereo-BRUVS_metadata.csv"                         
    ##  [4] "2023-03_SwC_stereo-BRUVs_3DPoints.txt"                               
    ##  [5] "2023-03_SwC_stereo-BRUVs_Backwards_Dot Point Measurements.txt"       
    ##  [6] "2023-03_SwC_stereo-BRUVs_Backwards_Relief_Dot Point Measurements.txt"
    ##  [7] "2023-03_SwC_stereo-BRUVs_Forwards_Dot Point Measurements.txt"        
    ##  [8] "2023-03_SwC_stereo-BRUVs_Forwards_Relief_Dot Point Measurements.txt" 
    ##  [9] "2023-03_SwC_stereo-BRUVs_ImagePtPair.txt"                            
    ## [10] "2023-03_SwC_stereo-BRUVs_Info.txt"                                   
    ## [11] "2023-03_SwC_stereo-BRUVs_Lengths.txt"                                
    ## [12] "2023-03_SwC_stereo-BRUVs_Metadata.csv"                               
    ## [13] "2023-03_SwC_stereo-BRUVs_MovieSeq.txt"                               
    ## [14] "2023-03_SwC_stereo-BRUVs_Period.txt"                                 
    ## [15] "2023-03_SwC_stereo-BRUVs_Points.txt"                                 
    ## [16] "2023-03_SwC_stereo-BRUVs_Source.txt"                                 
    ## [17] "benthic.annotation.schema.forward.facing.20230714.135113.csv"        
    ## [18] "benthic.habitat.annotation.schema.forward.facing_2023-10-30.txt"     
    ## [19] "benthic.relief.annotation.schema.forward.facing_2023-10-17.txt"      
    ## [20] "broad-categories.csv"

``` r
# x <- read_metadata(here::here("r-workflows/data/raw/"), method = "BRUVs")
# 
# glimpse(x)


# bad <- is.na(names(x)) | names(x) == ""
# if (any(bad)) {
#   stop("Bad column names in: ", basename(file), "\nNames: ", paste(names(x), collapse = " | "))
# }

# metadata <- read_metadata(here::here("r-workflows/data/raw/"), method = "BRUVs") %>% # Change here to "DOVs"
#   dplyr::select(campaignid, sample, status, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
#   glimpse()
```
