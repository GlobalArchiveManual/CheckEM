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
metadata <- read_metadata(here::here("r-workflows/data/raw/"), method = "BRUVs") %>% # Change here to "DOVs"
  dplyr::select(campaignid, sample, status, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()
```

    ## reading metadata file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw//2022-05_PtCloates_stereo-BRUVS_metadata.csv

    ## Rows: 62
    ## Columns: 16
    ## $ campaignid            <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtClo…
    ## $ sample                <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "11…
    ## $ opcode                <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "11…
    ## $ latitude_dd           <chr> "-22.7221", "-22.6957", "-22.7379", "-22.7337", …
    ## $ longitude_dd          <chr> "113.5447", "113.5628", "113.5515", "113.5555", …
    ## $ date_time             <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T14:16:2…
    ## $ site                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ location              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ status                <chr> "No-take", "No-take", "No-take", "No-take", "No-…
    ## $ depth_m               <chr> "93.9", "77.3", "78.3", "73.9", "81.9", "74.3", …
    ## $ successful_count      <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",…
    ## $ successful_length     <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",…
    ## $ observer_count        <chr> "Kaitlin McCloghry", "Kaitlin McCloghry", "Kaitl…
    ## $ observer_length       <chr> "Henry Evans", "Henry Evans", "Henry Evans", "He…
    ## $ inclusion_probability <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ visibility_m          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

    ## reading metadata file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw//2023-03_SwC_stereo-BRUVs_Metadata.csv

    ## Rows: 32
    ## Columns: 34
    ## $ system_number                              <chr> "1", "2", "3", "4", "5", "6…
    ## $ sample                                     <chr> "35", "5", "26", "23", "29"…
    ## $ latitude_dd                                <chr> "-34.13155", "-34.12952667"…
    ## $ longitude_dd                               <chr> "114.923645", "114.9292367"…
    ## $ date_time                                  <chr> "14/03/2023 23:36", "14/03/…
    ## $ site                                       <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ location                                   <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ status                                     <chr> "No-take", "No-take", "No-t…
    ## $ depth_m                                    <chr> "39.6", "42.7", "36", "41",…
    ## $ successful_count                           <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ successful_length                          <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ observer_count                             <chr> "Hannah Williams", "Hannah …
    ## $ observer_length                            <chr> "Gidget Mirrabelle", "Gidge…
    ## $ observer_new_measurement_rules             <chr> "Gidget Mirrabelle", NA, "G…
    ## $ observer_labridaesp1                       <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ left_cam                                   <chr> "27", "30", "31", "33", "35…
    ## $ right_cam                                  <chr> "28", "30", "32", "34", "36…
    ## $ rear_cam                                   <chr> "19", "20", "21", "22", "23…
    ## $ video_notes                                <chr> NA, NA, "struggled with sha…
    ## $ raw_hdd_number                             <chr> "1", "1", "1", "1", "1", "1…
    ## $ fishnclips_saved                           <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ forwards_habitat_image_saved               <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ backwards_habitat_image_saved              <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ successful_habitat_forward                 <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ successful_habitat_backward                <chr> "Yes", "Yes", "Yes", "Yes",…
    ## $ observer_habitat_forward                   <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ observer_habitat_backward                  <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ maxn_complete_date                         <chr> NA, NA, "11/07/2023", "7/07…
    ## $ maxn_checker                               <chr> "Gidget Neunuebel", "Gidget…
    ## $ length_complete_date                       <chr> "31/07/2023", "31/07/2023",…
    ## $ length_new_measurement_rules_complete_date <chr> "1/08/2023", NA, "2/08/2023…
    ## $ labridaesp1_complete_date                  <chr> NA, NA, NA, NA, NA, NA, NA,…
    ## $ comment                                    <chr> NA, "need to check feeding.…
    ## $ campaignid                                 <chr> "2023-03_SwC_stereo-BRUVs",…
    ## Rows: 94
    ## Columns: 13
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ sample                      <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9…
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ longitude_dd                <chr> "113.5447", "113.5628", "113.5515", "113.5…
    ## $ latitude_dd                 <chr> "-22.7221", "-22.6957", "-22.7379", "-22.7…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "77.3", "78.3", "73.9", "81.9", "7…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

Save the metadata as an R data file (this creates a lighter file than
saving as a .csv or similar, it also maintains any column formatting).

``` r
saveRDS(metadata, file = here::here(paste0("r-workflows/data/tidy/",
                                name, "_metadata.rds")))
```

If you have already exported in the ‘check-habitat’ script then load the
metadata. If you have just loaded and saved the metadata, then you can
skip this chunk of code.

``` r
metadata <- readRDS(here::here(paste0("r-workflows/data/tidy/",
                                name, "_metadata.rds")))
```

### Marine Parks

Load marine park shape files, and extract fishing status (e.g. ‘Fished’
or ‘No-take’) for use in modelling. The data set used here is the 2022
Collaborative Australian Protected Areas Database, which you can
download for free
[here](https://fed.dcceew.gov.au/datasets/782c02c691014efe8ffbd27445fe41d7_0/explore).

You may change this shape file to any suitable data set that is
available for your study area.

#### NOTE: You should only use this code if the Status column is not filled out

``` r
marine_parks <- st_read(here::here("r-workflows/data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"))  %>%
  dplyr::select(geometry, ZONE_TYPE) %>%
  st_transform(4326) %>%
  st_make_valid()
```

    ## Reading layer `Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine' from data source `/home/runner/work/CheckEM/CheckEM/r-workflows/data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 3775 features and 26 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 70.71702 ymin: -58.44947 xmax: 170.3667 ymax: -8.473407
    ## Geodetic CRS:  WGS 84

``` r
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# metadata <- metadata_sf %>%
#   st_join(marine_parks) %>%
#   bind_cols(st_coordinates(.)) %>%
#   as.data.frame() %>%
#   dplyr::select(-c(geometry)) %>%
#   dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
#   dplyr::mutate(status = if_else(str_detect(ZONE_TYPE, "National|Sanctuary"),
#                                 "No-take", "Fished")) %>%
#   tidyr::replace_na(list(status = "Fished")) %>%
#   clean_names() %>%
#   dplyr::glimpse()
```

### Find nearest Marine Region

Now we need to find the nearest marine region for each sample in the
metadata. Then we can use the life history lists to find species that
have not been observed in that marine region before.

``` r
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)
regions <- st_as_sf(CheckEM::aus_regions, crs = st_crs(4326))
regions <- st_transform(regions, 4326) %>%
  dplyr::select(REGION)

metadata <- st_join(metadata_sf, regions, join = st_nearest_feature) %>%
  dplyr::rename(marine_region = REGION) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  glimpse()
```

    ## Rows: 94
    ## Columns: 12
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ sample                      <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9…
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "77.3", "78.3", "73.9", "81.9", "7…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ marine_region               <chr> "North-west", "North-west", "North-west", …

## Fish annotation data

There are two types of fish annotation data formats that you can check
and format using this script: *EventMeasure database outputs* or
*Generic* files.

- For *EventMeasure database outputs* you will need to export the
  database outputs using the EventMeasure software and the .EMObs files.
  This will give you the ’\_Points.txt’, ’\_Lengths.txt’ and
  ’\_3DPoints.txt’ files.

- *Generic* data is a much simpler format and allows users who haven’t
  used EventMeasure to format QC their annotation data. You will need a
  \_Count.csv file and a \_Length.csv file.

For more information on the format of these files please see the
[CheckEM user
guide](https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html)

We recommend using the *EventMeasure database outputs* if they are
available and up to date. There are more checks possible with
EventMeasure data than *Generic* data. **Note:** If you have used
EventMeasure software to annotate your samples BUT have made corrections
on the exported data (e.g. in Excel), this corrected data is now the
true copy of the data and you should import your data as *Generic*
annotation files (e.g. count and length).

### Load any EventMeasure Points.txt files

This section of code will read in any Points.txt files that you have
saved in the directory you set. It will combine all of the files into
one data-frame, and get the campaignid name from the name of the file.
It is important that you consistently name your files with the same
campaignid (look out for different separators e.g. ‘-’, ’\_‘, or’.’)

``` r
points <- read_points(here::here("r-workflows/data/raw/")) %>%
  glimpse()
```

    ## Rows: 17,425
    ## Columns: 23
    ## $ opcode      <chr> "10", "10", "10", "10", "10", "10", "10", "10", "10", "10"…
    ## $ pointindex  <chr> "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "1…
    ## $ filename    <chr> "Left_MAH00355.MP4", "Left_MAH00355.MP4", "Left_MAH00355.M…
    ## $ frame       <chr> "11347", "11403", "11403", "11473", "11473", "11473", "127…
    ## $ time        <chr> "25.71152", "25.72709", "25.72709", "25.74655", "25.74655"…
    ## $ period      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
    ## $ periodtime  <chr> "0.33422", "0.34979", "0.34979", "0.36926", "0.36926", "0.…
    ## $ imagecol    <chr> "463.20000", "474.00000", "1495.20000", "1562.40000", "157…
    ## $ imagerow    <chr> "669.60000", "655.20000", "541.20000", "543.60000", "571.2…
    ## $ rectwidth   <chr> "0.00000", "0.00000", "0.00000", "0.00000", "0.00000", "0.…
    ## $ rectheight  <chr> "0.00000", "0.00000", "0.00000", "0.00000", "0.00000", "0.…
    ## $ family      <chr> "Labridae", "Labridae", "Labridae", "Labridae", "Labridae"…
    ## $ genus       <chr> "Ophthalmolepis", "Ophthalmolepis", "Ophthalmolepis", "Pse…
    ## $ species     <chr> "lineolata", "lineolata", "lineolata", "biserialis", "bise…
    ## $ code        <chr> "37384040", "37384040", "37384040", "37384149", "37384149"…
    ## $ number      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ stage       <chr> "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AD"…
    ## $ activity    <chr> "Passing", "Passing", "Passing", "Passing", "Passing", "Pa…
    ## $ comment     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ attribute9  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ attribute10 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ campaignid  <chr> "2023-03_SwC_stereo-BRUVs", "2023-03_SwC_stereo-BRUVs", "2…
    ## $ sample      <chr> "10", "10", "10", "10", "10", "10", "10", "10", "10", "10"…

### Load any *Generic* count files

This section of code will read in any Count.csv files that you have
saved in the directory you set. It will combine all of the files into
one data frame, and get the campaignid name from the name of the file.

It is important that you consistently name your files with the same
campaignid (look out for different separators e.g. ‘-’, ’\_‘, or’.’)

``` r
counts <- read_counts(here::here("r-workflows/data/raw/")) %>%
  glimpse()
```

    ## reading count file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw//2022-05_PtCloates_stereo-BRUVS_count.csv

    ## file is a csv

    ## Rows: 802
    ## Columns: 7
    ## $ campaignid <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates_stereo…
    ## $ opcode     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2",…
    ## $ family     <chr> "Balistidae", "Carangidae", "Carangidae", "Lethrinidae", "L…
    ## $ genus      <chr> "Abalistes", "Decapterus", "Turrum", "Gymnocranius", "Lethr…
    ## $ species    <chr> "filamentosus", "spp", "fulvoguttatum", "sp1", "rubriopercu…
    ## $ count      <dbl> 3, 4, 18, 2, 1, 9, 3, 2, 20, 1, 1, 4, 1, 1, 2, 2, 1, 2, 1, …
    ## $ sample     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2",…

### Combine EventMeasure and *Generic* count together

Create and tidy the MaxN file.

``` r
# Only run this if there is data in the counts data frame
if(nrow(points) > 1){
  maxn_points <- points %>%
    dplyr::group_by(campaignid, sample, filename, periodtime, frame, family, genus, species) %>% # If you have MaxN'd by stage (e.g. Adult, Juvenile) add stage here
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::slice(which.max(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(maxn)) %>%
    dplyr::select(-frame) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::mutate(maxn = as.numeric(maxn)) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::inner_join(metadata, by = join_by(campaignid, sample)) %>%
    dplyr::filter(successful_count %in% c("Yes")) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(campaignid, sample, family, genus, species, maxn) %>%
    dplyr::glimpse()
}
```

    ## `summarise()` has grouped output by 'campaignid', 'sample', 'filename',
    ## 'periodtime', 'frame', 'family', 'genus'. You can override using the `.groups`
    ## argument.

    ## Rows: 462
    ## Columns: 6
    ## $ campaignid <chr> "2023-03_SwC_stereo-BRUVs", "2023-03_SwC_stereo-BRUVs", "20…
    ## $ sample     <chr> "10", "10", "10", "10", "10", "10", "10", "10", "10", "10",…
    ## $ family     <chr> "Dasyatidae", "Gerreidae", "Heterodontidae", "Labridae", "L…
    ## $ genus      <chr> "Dasyatis", "Parequula", "Heterodontus", "Austrolabrus", "C…
    ## $ species    <chr> "brevicaudata", "melbournensis", "portusjacksoni", "maculat…
    ## $ maxn       <dbl> 1, 1, 1, 1, 71, 9, 8, 1, 5, 2, 9, 1, 1, 2, 2, 1, 5, 12, 3, …

``` r
# Only run this if there is data in the counts data frame
if(nrow(counts) > 1){
  maxn_counts <- counts %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::mutate(number = as.numeric(count)) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(campaignid, sample, family, genus, species) %>%
    dplyr::slice(which.max(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(maxn)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::mutate(maxn = as.numeric(maxn)) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::inner_join(metadata, by = join_by(campaignid, sample)) %>%
    dplyr::filter(successful_count %in% c("Yes")) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(campaignid, sample, family, genus, species, maxn) %>%
    dplyr::glimpse()
}
```

    ## `summarise()` has grouped output by 'campaignid', 'sample', 'family', 'genus'.
    ## You can override using the `.groups` argument.

    ## Rows: 802
    ## Columns: 6
    ## $ campaignid <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates_stereo…
    ## $ sample     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "11", "11", "11", "…
    ## $ family     <chr> "Balistidae", "Carangidae", "Carangidae", "Lethrinidae", "L…
    ## $ genus      <chr> "Abalistes", "Decapterus", "Turrum", "Gymnocranius", "Lethr…
    ## $ species    <chr> "filamentosus", "spp", "fulvoguttatum", "sp1", "rubriopercu…
    ## $ maxn       <dbl> 3, 4, 18, 2, 1, 9, 3, 2, 2, 1, 6, 3, 1, 3, 1, 2, 1, 1, 1, 1…

``` r
# If only EventMeasure data then MaxN only includes Points data
# If only Generic data then MaxN only includes Count data
# If both exist, then MaxN includes both Points and Count data
maxn <- bind_rows(get0("maxn_points"), get0("maxn_counts")) # this works even if you only have one type of data
```

### Load any EventMeasure Lengths.txt and/or 3DPoints.txt files

This section of code will read in any Lengths.txt and/or 3DPoints.txt
files that you have saved in the directory you set. It will combine all
of the files into one data-frame, and get the campaignid name from the
name of the file. It is important that you consistently name your files
with the same campaignid (look out for different separators e.g. ‘-’,
’\_‘, or’.’)

``` r
em_length3dpoints <- read_em_length(here::here("r-workflows/data/raw/")) %>%
  dplyr::select(-c(comment))%>% # there is a comment column in metadata, so you will need to remove this column from EM data
  dplyr::inner_join(metadata, by = join_by(sample, campaignid)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  # dplyr::rename(length_mm = length) %>%
  glimpse()
```

    ## Rows: 2,428
    ## Columns: 46
    ## $ opcode                      <chr> "10", "10", "10", "10", "10", "12", "12", …
    ## $ imageptpair                 <chr> "72", "73", "75", "81", "93", "0", "11", "…
    ## $ filenameleft                <chr> "Left_MAH00355.MP4", "Left_MAH00355.MP4", …
    ## $ frameleft                   <chr> "65608", "74525", "21436", "33446", "46833…
    ## $ filenameright               <chr> "Right_MAH00344.MP4", "Right_MAH00344.MP4"…
    ## $ frameright                  <chr> "64140", "73057", "20028", "32038", "45425…
    ## $ time                        <chr> "40.79909", "43.27851", "51.07269", "54.41…
    ## $ period                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ periodtime                  <chr> "15.42180", "17.90122", "25.69539", "29.03…
    ## $ x                           <dbl> -430.67367, -278.33750, -997.34125, -187.9…
    ## $ y                           <dbl> -140.98709, -257.03677, -186.86483, -238.3…
    ## $ z                           <dbl> -868.5349, -762.9243, -1522.4532, -730.307…
    ## $ sx                          <chr> "0.64880", "0.48001", "1.91708", "0.42177"…
    ## $ sy                          <chr> "0.46100", "0.47613", "0.79836", "0.44960"…
    ## $ sz                          <chr> "1.11439", "0.88751", "3.13793", "0.83996"…
    ## $ rms                         <dbl> 0.67020, 0.47105, 0.63932, 0.10770, 0.3492…
    ## $ range                       <dbl> 979.6479, 851.8175, 1829.6098, 790.8665, 9…
    ## $ direction                   <chr> "27.65589", "26.62953", "33.74391", "22.82…
    ## $ family                      <chr> "Labridae", "Urolophidae", "Labridae", "Tr…
    ## $ genus                       <chr> "Austrolabrus", "Trygonoptera", "Ophthalmo…
    ## $ species                     <chr> "maculatus", "ovalis", "lineolata", "dumer…
    ## $ code                        <chr> "37384025", "37038016", "37384040", "37027…
    ## $ number                      <dbl> 1, 1, 1, 1, 1, 40, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ stage                       <chr> "AD", "AD", "AD", "AD", "AD", "AD", "AD", …
    ## $ activity                    <chr> "Passing", "Passing", "Passing", "Passing"…
    ## $ attribute9                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ attribute10                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ campaignid                  <chr> "2023-03_SwC_stereo-BRUVs", "2023-03_SwC_s…
    ## $ length_mm                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ precision                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ horzdir                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ vertdir                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ midx                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ midy                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ midz                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ sample                      <chr> "10", "10", "10", "10", "10", "12", "12", …
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "18/03/2023 2:44", "18/03/2023 2:44", "18/…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "44.3", "44.3", "44.3", "44.3", "44.3", "4…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_backward <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ marine_region               <chr> "South-west", "South-west", "South-west", …

### Load any *Generic* length files

This section of code will read in any *Length.csv files that you have
saved in the directory you set. It will combine all of the files into
one data frame, and get the campaignid name from the name of the file.
It is important that you consistently name your files with the same
campaignid (look out for different separators e.g. ‘-’, ’*’, or ‘.’)

``` r
gen_length <- read_gen_length(here::here("r-workflows/data/raw/")) %>%
  dplyr::full_join(metadata, by = join_by(campaignid, sample)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse()
```

    ## reading length file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw//2022-05_PtCloates_stereo-BRUVS_length.csv

    ## file is a csv

    ## Rows: 2,162
    ## Columns: 22
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ opcode                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ family                      <chr> "Balistidae", "Balistidae", "Balistidae", …
    ## $ genus                       <chr> "Abalistes", "Abalistes", "Abalistes", "De…
    ## $ species                     <chr> "filamentosus", "filamentosus", "filamento…
    ## $ length_mm                   <dbl> 336.7789, 324.9078, 281.1703, 310.8369, 33…
    ## $ number                      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ range                       <dbl> 3775.308, 3918.720, 2559.820, 3356.762, 35…
    ## $ rms                         <dbl> 11.10507, 14.14936, 7.13303, 16.32680, 15.…
    ## $ precision                   <dbl> 21.13063, 9.07687, 5.16125, 13.59407, 18.2…
    ## $ code                        <chr> "37465089", "37465089", "37465089", NA, NA…
    ## $ sample                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "93.9", "93.9", "93.9", "93.9", "9…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ marine_region               <chr> "North-west", "North-west", "North-west", …

### Combine EventMeasure and *Generic* length data

``` r
# If only EventMeasure data then length only includes Length and 3D points data
# If only Generic data then length only includes generic length data
# If both exist, then length includes both Length and 3D points and generic length data
length <- bind_rows(get0("em_length3dpoints"), get0("gen_length")) # this works even if you only have one type of data
```

### Format and add zeros where a species isn’t present

#### In the count data

Tidy and “complete” MaxN data (e.g. add zeros in the data where a
species wasn’t observed). The final data set will have a row for each
species in every sample (deployment).

``` r
count <- maxn %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
  dplyr::select(campaignid, sample, family, genus, species, maxn) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  tidyr::replace_na(list(maxn = 0)) %>%
  group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(count = sum(maxn)) %>%
  ungroup() %>%
  mutate(scientific = paste(family, genus, species, sep = " "))%>%
  dplyr::select(campaignid, sample, scientific, count)%>%
  spread(scientific, count, fill = 0)
```

    ## `summarise()` has grouped output by 'campaignid', 'sample', 'family', 'genus'.
    ## You can override using the `.groups` argument.

``` r
count_families <- maxn %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  filter(!(family %in% "Unknown")) %>%
  dplyr::select(c(family, genus, species, scientific)) %>%
  distinct()

complete_count <- count %>%
  pivot_longer(names_to = "scientific", values_to = "count",
               cols = 3:ncol(.)) %>%
  inner_join(count_families, by = c("scientific")) %>%
  full_join(metadata)%>%
  glimpse()
```

    ## Joining with `by = join_by(campaignid, sample)`

    ## Rows: 17,766
    ## Columns: 17
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ sample                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ scientific                  <chr> "Acanthuridae Naso brachycentron", "Acanth…
    ## $ count                       <dbl> 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, …
    ## $ family                      <chr> "Acanthuridae", "Acanthuridae", "Acanthuri…
    ## $ genus                       <chr> "Naso", "Naso", "Naso", "Albula", "Aplodac…
    ## $ species                     <chr> "brachycentron", "fageni", "hexacanthus", …
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "93.9", "93.9", "93.9", "93.9", "9…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ marine_region               <chr> "North-west", "North-west", "North-west", …

#### In the length data

Tidy and complete length data (e.g. complete zeroes in the data). The
final data set will have a row for each species in every sample
(deployment).

``` r
complete_length <- length %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
  dplyr::filter(!family %in% "Unknown")%>%
  # First make one row for every length measurement
  dplyr::mutate(number = as.numeric(number)) %>%
  tidyr::uncount(number) %>%
  dplyr::mutate(number = 1) %>%
  # Add in missing samples
  dplyr::right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  # Complete the data (add in zeros for every species)
  dplyr::select(campaignid, sample, family, genus, species, length_mm, number, any_of(c("range", "rms", "precision"))) %>% # this will keep EM only columns
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(number)) %>%
  dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
  left_join(., metadata) %>%
  glimpse()
```

    ## Joining with `by = join_by(campaignid, sample, status, date_time, location,
    ## site, depth_m, successful_count, successful_length, successful_habitat_forward,
    ## successful_habitat_backward, marine_region)`
    ## Joining with `by = join_by(campaignid, sample)`

    ## Rows: 23,119
    ## Columns: 20
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ sample                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ family                      <chr> "Acanthuridae", "Acanthuridae", "Acanthuri…
    ## $ genus                       <chr> "Naso", "Naso", "Naso", "Albula", "Aplodac…
    ## $ species                     <chr> "brachycentron", "fageni", "hexacanthus", …
    ## $ length_mm                   <dbl> NA, NA, NA, NA, NA, NA, NA, 336.7789, 324.…
    ## $ number                      <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, …
    ## $ range                       <dbl> NA, NA, NA, NA, NA, NA, NA, 3775.308, 3918…
    ## $ rms                         <dbl> NA, NA, NA, NA, NA, NA, NA, 11.10507, 14.1…
    ## $ precision                   <dbl> NA, NA, NA, NA, NA, NA, NA, 21.13063, 9.07…
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "93.9", "93.9", "93.9", "93.9", "9…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ marine_region               <chr> "North-west", "North-west", "North-west", …

## Quality Control Checks

Now we have some-what tidy data we can begin to run some checks.

### Number of unique samples in the metadata

This is the total number of unique samples in the sample metadata (it
should also be the number of rows in the metadata data frame)

``` r
number_of_samples <- metadata %>%
  dplyr::distinct(campaignid, sample)

message(paste(nrow(number_of_samples), "unique samples in the metadata"))
```

    ## 94 unique samples in the metadata

### Check for duplicate sample names

If you have any duplicate samples within a campaign they will be
displayed here

``` r
duplicate_samples <- metadata %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1)
```

    ## `summarise()` has grouped output by 'campaignid'. You can override using the
    ## `.groups` argument.

``` r
message(paste(nrow(duplicate_samples), "samples duplicated in the metadata"))
```

    ## 0 samples duplicated in the metadata

### Number of sample(s) without points or count data

This could be due to samples not observing fish (not an error) or a
sample that should be marked as successful_count = No. It could also be
due to a sample name spelt incorrectly in the count/points or sample
metadata file.

``` r
metadata_samples <- metadata %>%
  dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")),
                successful_count, successful_length) %>%
  distinct()

samples <- maxn %>%
  distinct(campaignid, sample)

missing_count <- anti_join(metadata_samples, samples, by = join_by(campaignid, sample))

message(paste(nrow(missing_count), "samples in the metadata missing count data"))
```

    ## 0 samples in the metadata missing count data

### Samples in the count data missing metadata

This next chunk checks for any samples that are in the count data but do
not have a match in the sample metadata.

``` r
missing_metadata <- anti_join(samples, metadata_samples, by = join_by(campaignid, sample))
message(paste(nrow(missing_metadata), "samples in count data missing metadata"))
```

    ## 0 samples in count data missing metadata

### Number of sample(s) without length or 3D point data

This could be due to samples not observing fish (not an error) or a
sample that should be marked as successful_length = No. It could also be
due to a sample name spelt incorrectly in the EMObs or Length file or
the sample metadata file.

``` r
metadata_samples <- metadata %>%
  dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")),
                successful_count, successful_length) %>%
  distinct()

samples <- length %>%
  distinct(campaignid, sample)

missing_length <- anti_join(metadata_samples, samples, by = join_by(campaignid, sample))

message(paste(nrow(missing_length), "samples in metadata missing length data"))
```

    ## 0 samples in metadata missing length data

### Samples in the length data missing metadata

This next chunk checks for any samples that are in the length data but
do not have a match in the sample metadata.

``` r
missing_metadata <- anti_join(samples, metadata_samples, by = join_by(campaignid, sample))

message(paste(nrow(missing_metadata), "samples in length data missing metadata"))
```

    ## 0 samples in length data missing metadata

### Periods without an end (EM only)

This check is only important if you have used periods to define your
sampling duration. It looks for any periods in the EMObs that do not
have an end time. This is important if you want to check the duration of
each period.

``` r
periods <- read_periods(here::here("r-workflows/data/raw/")) %>%
  glimpse()
```

    ## Rows: 32
    ## Columns: 12
    ## $ opcode        <chr> "10", "12", "14", "15", "16", "17", "19", "2", "21", "22…
    ## $ periodindex   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ period        <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
    ## $ filenamestart <chr> "Left_MAH00355.MP4", "L031_MAH01383.MP4", "L031_14_MAH01…
    ## $ framestart    <chr> "10145", "3746", "24760", "76519", "67465", "71918", "46…
    ## $ time_start    <dbl> 25.37730, 23.59802, 29.42440, 21.27653, 18.75902, 19.997…
    ## $ filenameend   <chr> "Left_MAH00357.MP4", "L031_MAH01385.MP4", "L031_14_MAH01…
    ## $ frameend      <chr> "63748", "57409", "78422", "49060", "40006", "44459", "1…
    ## $ time_end      <dbl> 85.37724, 83.59796, 89.42433, 81.27647, 78.75896, 79.997…
    ## $ has_end       <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
    ## $ campaignid    <chr> "2023-03_SwC_stereo-BRUVs", "2023-03_SwC_stereo-BRUVs", …
    ## $ sample        <chr> "10", "12", "14", "15", "16", "17", "19", "2", "21", "22…

``` r
periods_without_end <- periods %>%
  dplyr::filter(has_end == 0)

message(paste(nrow(periods_without_end), "periods without an end"))
```

    ## 0 periods without an end

``` r
glimpse(periods_without_end)
```

    ## Rows: 0
    ## Columns: 12
    ## $ opcode        <chr> 
    ## $ periodindex   <chr> 
    ## $ period        <chr> 
    ## $ filenamestart <chr> 
    ## $ framestart    <chr> 
    ## $ time_start    <dbl> 
    ## $ filenameend   <chr> 
    ## $ frameend      <chr> 
    ## $ time_end      <dbl> 
    ## $ has_end       <chr> 
    ## $ campaignid    <chr> 
    ## $ sample        <chr>

### Samples without periods (EM only)

This check is only important if you have used periods to define your
sampling duration. You can use it to find any samples that are missing
periods.

``` r
metadata_samples <- metadata %>%
  dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
  dplyr::distinct() %>%
  dplyr::mutate(sample = as.factor(sample))

periods_samples <- periods %>%
  dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
  distinct()

missing_periods <- anti_join(metadata_samples, periods_samples) %>%
  dplyr::select(!sample)
```

    ## Joining with `by = join_by(campaignid, sample)`

``` r
message(paste(nrow(missing_periods), "samples missing period"))
```

    ## 62 samples missing period

``` r
glimpse(missing_periods)
```

    ## Rows: 62
    ## Columns: 3
    ## $ campaignid        <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates…
    ## $ successful_count  <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ successful_length <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…

### Points outside Periods (EM only)

This check identifies any points that have been annotated outside of a
period.

``` r
points_outside_periods <- points %>%
  dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, frame)

message(paste(nrow(points_outside_periods), "points outside a period"))
```

    ## 0 points outside a period

``` r
glimpse(points_outside_periods)
```

    ## Rows: 0
    ## Columns: 8
    ## $ campaignid <chr> 
    ## $ opcode     <chr> 
    ## $ period     <chr> 
    ## $ family     <chr> 
    ## $ genus      <chr> 
    ## $ species    <chr> 
    ## $ number     <dbl> 
    ## $ frame      <chr>

### Length measurement(s) or 3D point(s) outside periods (EM only)

This check identifies any length measurements or 3D points that have
been annotated outside of a period.

``` r
lengths_outside_periods <- em_length3dpoints %>%
  dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number)

message(paste(nrow(lengths_outside_periods), "lengths/3D points outside period"))
```

    ## 1 lengths/3D points outside period

``` r
glimpse(lengths_outside_periods)
```

    ## Rows: 1
    ## Columns: 7
    ## $ campaignid <chr> "2023-03_SwC_stereo-BRUVs"
    ## $ opcode     <chr> "32"
    ## $ period     <chr> NA
    ## $ family     <chr> NA
    ## $ genus      <chr> NA
    ## $ species    <chr> NA
    ## $ number     <dbl> NA

### Period(s) that are not the correct duration (EM only)

In this check you define the correct sampling duration (e.g. 60 minutes
for stereo-BRUVs) and then identify any periods that are not that
length.

``` r
period_length <- 60 # in minutes

periods_wrong <- periods %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), time_start, time_end, has_end) %>%
        dplyr::distinct() %>%
        dplyr::mutate(period_time = round(time_end - time_start)) %>%
        dplyr::filter(!period_time %in% period_length)

message(paste(nrow(periods_wrong), "periods not", period_length, "minutes long"))
```

    ## 1 periods not 60 minutes long

``` r
glimpse(periods_wrong)
```

    ## Rows: 1
    ## Columns: 7
    ## $ campaignid  <chr> "2023-03_SwC_stereo-BRUVs"
    ## $ opcode      <chr> "31"
    ## $ period      <chr> "1"
    ## $ time_start  <dbl> 12.19858
    ## $ time_end    <dbl> 65.25241
    ## $ has_end     <chr> "1"
    ## $ period_time <dbl> 53

### Total number of individuals observed

This is the total number of individuals observed in the count data:

``` r
total_count <- sum(complete_count$count)
message(paste(total_count, "fish counted in the count data"))
```

    ## 6411 fish counted in the count data

This is the total number of individuals observed in the length data:

``` r
total_length <- sum(complete_length$number)
message(paste(total_length, "fish counted in the length data"))
```

    ## 6421 fish counted in the length data

### Points without a number (EM only)

This is a check for EventMeasure data. Sometimes analysts will add
points of interest that are not fish/sharks and remove the number so
they are not summed in total abundance metrics. You should check to make
sure no fish species accidentally had their number deleted.

``` r
points_without_number <- points %>%
  filter(number %in% c("NA", NA, 0, NULL, "", " "))

message(paste(nrow(points_without_number), "points in the _Points.txt file that do not have a number"))
```

    ## 11 points in the _Points.txt file that do not have a number

``` r
glimpse(points_without_number)
```

    ## Rows: 11
    ## Columns: 23
    ## $ opcode      <chr> "10", "10", "10", "21", "21", "21", "26", "26", "26", "34"…
    ## $ pointindex  <chr> "166", "485", "486", "238", "254", "256", "66", "67", "68"…
    ## $ filename    <chr> "Left_MAH00355.MP4", "Left_MAH00356.MP4", "Left_MAH00356.M…
    ## $ frame       <chr> "17138", "31042", "31042", "32458", "35214", "45239", "311…
    ## $ time        <chr> "27.32174", "53.74369", "53.74369", "54.10433", "54.87065"…
    ## $ period      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"
    ## $ periodtime  <chr> "1.94444", "28.36639", "28.36639", "24.85844", "25.62477",…
    ## $ imagecol    <chr> "1861.20000", "289.20000", "408.00000", "973.20000", "728.…
    ## $ imagerow    <chr> "243.60000", "292.80000", "414.00000", "67.20000", "50.400…
    ## $ rectwidth   <chr> "0.00000", "0.00000", "0.00000", "0.00000", "0.00000", "0.…
    ## $ rectheight  <chr> "0.00000", "0.00000", "0.00000", "0.00000", "0.00000", "0.…
    ## $ family      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ genus       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ species     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ code        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ number      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ stage       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ activity    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ comment     <chr> "squid", "squid", "squid", "Squid", "Squid", "Squid", "squ…
    ## $ attribute9  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ attribute10 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ## $ campaignid  <chr> "2023-03_SwC_stereo-BRUVs", "2023-03_SwC_stereo-BRUVs", "2…
    ## $ sample      <chr> "10", "10", "10", "21", "21", "21", "26", "26", "26", "34"…

### Length measurements or 3D points without a number (EM only)

This is a check for EventMeasure data. Sometimes analysts will add 3D
points to record the sync point. These can remain in the data but you
should double check that no fish are accidentally missing a number.

``` r
lengths_without_number <- em_length3dpoints %>%
  filter(number %in% c("NA", NA, 0, NULL, "", " "))

message(paste(nrow(lengths_without_number), "lengths or 3D points in the EMObs that do not have a number"))
```

    ## 1 lengths or 3D points in the EMObs that do not have a number

``` r
glimpse(lengths_without_number)
```

    ## Rows: 1
    ## Columns: 46
    ## $ opcode                      <chr> "32"
    ## $ imageptpair                 <chr> "0"
    ## $ filenameleft                <chr> "L039_MAH00349.MP4"
    ## $ frameleft                   <chr> "58009"
    ## $ filenameright               <chr> "R040_MAH00356.MP4"
    ## $ frameright                  <chr> "57032"
    ## $ time                        <chr> "16.12972"
    ## $ period                      <chr> NA
    ## $ periodtime                  <chr> "-1.00000"
    ## $ x                           <dbl> 1.53837
    ## $ y                           <dbl> -121.0145
    ## $ z                           <dbl> -714.4317
    ## $ sx                          <chr> "0.38485"
    ## $ sy                          <chr> "0.38601"
    ## $ sz                          <chr> "0.84527"
    ## $ rms                         <dbl> 0.01491
    ## $ range                       <dbl> 724.6099
    ## $ direction                   <chr> "10.04716"
    ## $ family                      <chr> NA
    ## $ genus                       <chr> NA
    ## $ species                     <chr> NA
    ## $ code                        <chr> NA
    ## $ number                      <dbl> NA
    ## $ stage                       <chr> NA
    ## $ activity                    <chr> NA
    ## $ attribute9                  <chr> NA
    ## $ attribute10                 <chr> NA
    ## $ campaignid                  <chr> "2023-03_SwC_stereo-BRUVs"
    ## $ length_mm                   <dbl> NA
    ## $ precision                   <dbl> NA
    ## $ horzdir                     <chr> NA
    ## $ vertdir                     <chr> NA
    ## $ midx                        <dbl> NA
    ## $ midy                        <dbl> NA
    ## $ midz                        <dbl> NA
    ## $ sample                      <chr> "32"
    ## $ status                      <chr> "No-take"
    ## $ date_time                   <chr> "15/03/2023 0:23"
    ## $ location                    <chr> NA
    ## $ site                        <chr> NA
    ## $ depth_m                     <chr> "44.4"
    ## $ successful_count            <chr> "Yes"
    ## $ successful_length           <chr> "Yes"
    ## $ successful_habitat_forward  <chr> "Yes"
    ## $ successful_habitat_backward <chr> "Yes"
    ## $ marine_region               <chr> "South-west"

### Species names that have been updated

Taxanomic advances means that species names are updated all the time,
and analysts sometimes spell species names incorrectly. This check uses
a data-frame saved inside the CheckEM package to identifies species
names that have been updated or spelt incorrectly.

You can choose if you would like to find the synonyms using an Australia
specific list (*CheckEM::aus_synonyms*) or a Global list (*TO ADD*).

#### Synonyms in the count data

``` r
synonyms_in_count <- dplyr::left_join(complete_count, CheckEM::aus_synonyms) %>%
      dplyr::filter(!is.na(genus_correct)) %>%
      dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
      dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
      dplyr::select('old name', 'new name') %>%
      dplyr::distinct()
```

    ## Joining with `by = join_by(family, genus, species)`

``` r
message(paste(nrow(synonyms_in_count), "synonyms used in the count data"))
```

    ## 13 synonyms used in the count data

``` r
glimpse(synonyms_in_count)
```

    ## Rows: 13
    ## Columns: 2
    ## $ `old name` <chr> "Carangidae Carangoides spp", "Dasyatidae Dasyatis brevicau…
    ## $ `new name` <chr> "Carangidae Unknown spp", "Dasyatidae Bathytoshia brevicaud…

#### Synonyms in the length data

``` r
synonyms_in_length <- dplyr::left_join(complete_length, CheckEM::aus_synonyms) %>%
      dplyr::filter(!is.na(genus_correct)) %>%
      dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
      dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
      dplyr::select('old name', 'new name') %>%
      dplyr::distinct()
```

    ## Joining with `by = join_by(family, genus, species)`

``` r
message(paste(nrow(synonyms_in_length), "synonyms used in the length data"))
```

    ## 13 synonyms used in the length data

``` r
glimpse(synonyms_in_length)
```

    ## Rows: 13
    ## Columns: 2
    ## $ `old name` <chr> "Carangidae Carangoides spp", "Dasyatidae Dasyatis brevicau…
    ## $ `new name` <chr> "Carangidae Unknown spp", "Dasyatidae Bathytoshia brevicaud…

#### Change synonyms names in data

Now that we have identified species names that have been updated or
spelt wrong you need to decide if you want to change the names in your
data or continue using the old names. If you want to update the names
use the next two chunks. If you would like to retain the old names skip
the next two chunks.

**NOTE:** this does not change your original annotation files, only the
data you save at the end of the script.

``` r
complete_count <- dplyr::left_join(complete_count, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species))
```

    ## Joining with `by = join_by(family, genus, species)`

``` r
complete_length <- dplyr::left_join(complete_length, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species))
```

    ## Joining with `by = join_by(family, genus, species)`

### Species not observed in the region before

In this check you use a list of species and their known geographical
ranges to check your data against to identify any species that are
outside of that range. There are two life history data sets to choose
from to check any species that are outside of their known geographical
range. They are the Australia and Global lists.

To use the Australia life history list use
*CheckEM::australia_life_history* to use the Global life history list
use *CheckEM::global_life_history*.

We run the check on both the count and length data, as sometimes a
species will be in the count and not the length. All species identified
in these checks are present in your chosen life-history list. Sometimes
range data is limited, so it is possible that a species flagged in this
check is actually present in that area. You need to critically think
about the species flagged by this check.

#### Species out of range in the count data

Check for any species that are out of range in the count data.

``` r
count_species_not_observed_region <- complete_count %>%
  dplyr::distinct(campaignid, sample, family, genus, species, marine_region, count) %>%
  dplyr::anti_join(., expand_life_history(CheckEM::australia_life_history), by = c("family", "genus", "species", "marine_region")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::left_join(metadata) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region) %>%
  dplyr::distinct() %>%
  dplyr::rename('marine region not observed in' = marine_region) %>%
  dplyr::semi_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))
```

    ## Joining with `by = join_by(campaignid, sample, marine_region)`

``` r
message(paste(nrow(count_species_not_observed_region), "species not observed in the region before"))
```

    ## 0 species not observed in the region before

``` r
glimpse(count_species_not_observed_region)
```

    ## Rows: 0
    ## Columns: 5
    ## $ campaignid                      <chr> 
    ## $ family                          <chr> 
    ## $ genus                           <chr> 
    ## $ species                         <chr> 
    ## $ `marine region not observed in` <chr>

#### Species out of range in the length data

Check for any species that are out of range in the length data.

``` r
length_species_not_observed_region <- complete_length %>%
  dplyr::distinct(campaignid, sample, family, genus, species, marine_region, number) %>%
  dplyr::anti_join(., expand_life_history(CheckEM::australia_life_history), by = c("family", "genus", "species", "marine_region")) %>%
  dplyr::filter(number > 0) %>%
  dplyr::left_join(metadata) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region) %>%
  dplyr::distinct() %>%
  dplyr::rename('marine region not observed in' = marine_region) %>%
  dplyr::semi_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))
```

    ## Joining with `by = join_by(campaignid, sample, marine_region)`

``` r
message(paste(nrow(length_species_not_observed_region), "species not observed in the region before"))
```

    ## 0 species not observed in the region before

``` r
glimpse(length_species_not_observed_region)
```

    ## Rows: 0
    ## Columns: 5
    ## $ campaignid                      <chr> 
    ## $ family                          <chr> 
    ## $ genus                           <chr> 
    ## $ species                         <chr> 
    ## $ `marine region not observed in` <chr>

### Species not listed in the life history list

This next check identifies any species that are not listed in your
chosen life history list. It could be that you have misspelt the
family/genus/species name, the species name is invalid, the name has
been updated or that the species name should be included in the life
history list but is missing. Again you will need to critically think
about the species that are flagged to determine if this is an error or
not.

To use the Australia life history list use
*CheckEM::australia_life_history* to use the Global life history list
use *CheckEM::global_life_history*.

*NOTE.* If you believe that a species flagged by this check should be
included in the life history list please email
<brooke.gibbons@uwa.edu.au> with the full species name and which list it
should be added to (Global or Australia).

#### Species in the count data that are not listed

If you chose to update the names that have changed (synonyms) then this
check won’t include the previously used names.

``` r
count_species_not_in_list <- complete_count %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::left_join(metadata) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species) %>%
  dplyr::distinct()
```

    ## Joining with `by = join_by(campaignid, sample, status, date_time, location,
    ## site, depth_m, successful_count, successful_length, successful_habitat_forward,
    ## successful_habitat_backward, marine_region)`

``` r
message(paste(nrow(count_species_not_in_list), "species not in chosen life history list"))
```

    ## 11 species not in chosen life history list

``` r
glimpse(count_species_not_in_list)
```

    ## Rows: 11
    ## Columns: 4
    ## $ campaignid <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates_stereo…
    ## $ family     <chr> "Lethrinidae", "Lutjanidae", "Sus", "Rhinidae", "Scaridae",…
    ## $ genus      <chr> "Gymnocranius", "Pristipomoides", "Sus", "Rhynchobatus", "S…
    ## $ species    <chr> "sp1", "sp1", "sus", "laevis", "sp3", "sp10", "sp1", "SUS",…

#### Species in the length data that are not listed

``` r
length_species_not_in_list <- complete_length %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species")) %>%
  dplyr::filter(number > 0) %>%
  dplyr::left_join(metadata) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species) %>%
  dplyr::distinct()
```

    ## Joining with `by = join_by(campaignid, sample, status, date_time, location,
    ## site, depth_m, successful_count, successful_length, successful_habitat_forward,
    ## successful_habitat_backward, marine_region)`

``` r
message(paste(nrow(length_species_not_in_list), "species not in chosen life history list"))
```

    ## 12 species not in chosen life history list

``` r
glimpse(length_species_not_in_list)
```

    ## Rows: 12
    ## Columns: 4
    ## $ campaignid <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates_stereo…
    ## $ family     <chr> "Lethrinidae", "Lutjanidae", "Sus", "Rhinidae", "Scaridae",…
    ## $ genus      <chr> "Gymnocranius", "Pristipomoides", "Sus", "Rhynchobatus", "S…
    ## $ species    <chr> "sp1", "sp1", "sus", "laevis", "sp3", "sp10", "sp1", "SUS",…

### Length measurements smaller or bigger than the fishbase minimum and maximums

Length data is extremely valuable so it is important that fish are
precisely and accurately measured. You can use this check to identify
any measurements that are bigger than the maximum size listed on
fishbase. However, we think it is important that you check any species
that are nearing their maximum size (e.g 85% of maximum). We also think
it is important that you check for any species that are very small.
Fishbase does not list a minimum size, so we recommend checking the
length measurements against 15% of the maximum size.

In the function below you can edit these cut offs (15% and 85%) as you
see fit. If you believe the size limits for a particular species is
incorrect you fill out the feedback form on the
[CheckEM](https://marine-ecology.shinyapps.io/CheckEM/) web based app
(see tab *Edit maximum lengths*) to supply a new maximum size limit.

``` r
incorrect_lengths <- left_join(complete_length, create_min_max(CheckEM::australia_life_history, minimum = 0.15, maximum = 0.85)) %>%
  dplyr::filter(length_mm < min_length_mm | length_mm > max_length_mm) %>%
  mutate(reason = ifelse(length_mm < min_length_mm, "too small", "too big")) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length_mm, max_length_mm, length_max_mm, reason, any_of(c("em_comment", "frame_left")), length_max_mm) %>%
  mutate(difference = ifelse(reason %in% c("too small"), (min_length_mm - length_mm), (length_mm - max_length_mm))) %>%
  dplyr::mutate(percent_of_fb_max = (length_mm/length_max_mm)*100) %>%
  dplyr::left_join(metadata) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length_mm, max_length_mm, length_max_mm, reason, any_of(c("em_comment", "frame_left")), difference, percent_of_fb_max)
```

    ## Joining with `by = join_by(family, genus, species)`
    ## Joining with `by = join_by(campaignid, sample)`

``` r
too_small <- incorrect_lengths %>%
  dplyr::filter(reason %in% "too small")

too_big <- incorrect_lengths %>%
  dplyr::filter(reason %in% "too big")

message(paste(nrow(too_small), "lengths are too small"))
```

    ## 172 lengths are too small

``` r
glimpse(too_small)
```

    ## Rows: 172
    ## Columns: 11
    ## $ campaignid        <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates…
    ## $ family            <chr> "Monacanthidae", "Echeneidae", "Echeneidae", "Echene…
    ## $ genus             <chr> "Unknown", "Echeneis", "Echeneis", "Echeneis", "Eche…
    ## $ species           <chr> "spp", "naucrates", "naucrates", "naucrates", "naucr…
    ## $ length_mm         <dbl> 32.95594, 116.45890, 141.68877, 133.69953, 70.35057,…
    ## $ min_length_mm     <dbl> 44.12251, 165.00000, 165.00000, 165.00000, 165.00000…
    ## $ max_length_mm     <dbl> 250.0276, 935.0000, 935.0000, 935.0000, 935.0000, 38…
    ## $ length_max_mm     <dbl> 294.1501, 1100.0000, 1100.0000, 1100.0000, 1100.0000…
    ## $ reason            <chr> "too small", "too small", "too small", "too small", …
    ## $ difference        <dbl> 11.166572, 48.541100, 23.311230, 31.300470, 94.64943…
    ## $ percent_of_fb_max <dbl> 11.203784, 10.587173, 12.880797, 12.154503, 6.395506…

``` r
message(paste(nrow(too_big), "lengths are too big"))
```

    ## 488 lengths are too big

``` r
glimpse(too_big)
```

    ## Rows: 488
    ## Columns: 11
    ## $ campaignid        <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05_PtCloates…
    ## $ family            <chr> "Balistidae", "Balistidae", "Balistidae", "Carangida…
    ## $ genus             <chr> "Abalistes", "Abalistes", "Abalistes", "Decapterus",…
    ## $ species           <chr> "filamentosus", "filamentosus", "filamentosus", "spp…
    ## $ length_mm         <dbl> 336.7789, 324.9078, 281.1703, 366.6616, 712.5285, 36…
    ## $ min_length_mm     <dbl> 49.50000, 49.50000, 49.50000, 63.29100, 103.21126, 5…
    ## $ max_length_mm     <dbl> 280.5000, 280.5000, 280.5000, 358.6490, 584.8638, 32…
    ## $ length_max_mm     <dbl> 330.0000, 330.0000, 330.0000, 421.9400, 688.0751, 38…
    ## $ reason            <chr> "too big", "too big", "too big", "too big", "too big…
    ## $ difference        <dbl> 56.278880, 44.407750, 0.670350, 8.012586, 127.664692…
    ## $ percent_of_fb_max <dbl> 102.05421, 98.45689, 85.20314, 86.89899, 103.55389, …

### Number of 3D points and length measurements over the RMS limit (EM only)

In this check you can set the RMS limit, and then identify any
measurements that have a larger RMS.

``` r
rms_limit <- 20 # in mm

over_rms <- complete_length %>%
  dplyr::filter(as.numeric(rms) > rms_limit)

message(paste(nrow(over_rms), "lengths over RMS limit"))
```

    ## 0 lengths over RMS limit

``` r
glimpse(over_rms)
```

    ## Rows: 0
    ## Columns: 21
    ## $ campaignid                  <chr> 
    ## $ sample                      <chr> 
    ## $ family                      <chr> 
    ## $ genus                       <chr> 
    ## $ species                     <chr> 
    ## $ length_mm                   <dbl> 
    ## $ number                      <dbl> 
    ## $ range                       <dbl> 
    ## $ rms                         <dbl> 
    ## $ precision                   <dbl> 
    ## $ status                      <chr> 
    ## $ date_time                   <chr> 
    ## $ location                    <chr> 
    ## $ site                        <chr> 
    ## $ depth_m                     <chr> 
    ## $ successful_count            <chr> 
    ## $ successful_length           <chr> 
    ## $ successful_habitat_forward  <chr> 
    ## $ successful_habitat_backward <chr> 
    ## $ marine_region               <chr> 
    ## $ scientific                  <chr>

### Number of length measurements over the precision limit (EM only)

In this check you can set the precision limit, and then identify any
measurements that have a larger precision.

``` r
precision_limit <- 10 # in %

over_precision <- complete_length %>%
  dplyr::filter(as.numeric(precision) > precision_limit)

message(paste(nrow(over_precision), "lengths over precision limit"))
```

    ## 430 lengths over precision limit

``` r
glimpse(over_precision)
```

    ## Rows: 430
    ## Columns: 21
    ## $ campaignid                  <chr> "2022-05_PtCloates_stereo-BRUVS", "2022-05…
    ## $ sample                      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ family                      <chr> "Balistidae", "Carangidae", "Carangidae", …
    ## $ genus                       <chr> "Abalistes", "Decapterus", "Decapterus", "…
    ## $ species                     <chr> "filamentosus", "spp", "spp", "spp", "fulv…
    ## $ length_mm                   <dbl> 336.7789, 310.8369, 336.7015, 303.8198, 26…
    ## $ number                      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ range                       <dbl> 3775.308, 3356.762, 3562.630, 3514.671, 31…
    ## $ rms                         <dbl> 11.10507, 16.32680, 15.62525, 11.23263, 11…
    ## $ precision                   <dbl> 21.13063, 13.59407, 18.23430, 16.22170, 15…
    ## $ status                      <chr> "No-take", "No-take", "No-take", "No-take"…
    ## $ date_time                   <chr> "2022-05-22T10:03:24+08:00", "2022-05-22T1…
    ## $ location                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ site                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ depth_m                     <chr> "93.9", "93.9", "93.9", "93.9", "93.9", "9…
    ## $ successful_count            <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_length           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", …
    ## $ successful_habitat_forward  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ successful_habitat_backward <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ marine_region               <chr> "North-west", "North-west", "North-west", …
    ## $ scientific                  <chr> "Balistidae Abalistes filamentosus", "Cara…

### Number of 3D points and length measurements over the range limit (EM only)

In this check you can set the range limit, and then identify any
measurements that have a larger range.

``` r
range_limit <- 10 # in metres

over_range <- complete_length %>%
  dplyr::filter(as.numeric(range) > (range_limit* 1000))

message(paste(nrow(over_range), "lengths over range limit"))
```

    ## 0 lengths over range limit

``` r
glimpse(over_range)
```

    ## Rows: 0
    ## Columns: 21
    ## $ campaignid                  <chr> 
    ## $ sample                      <chr> 
    ## $ family                      <chr> 
    ## $ genus                       <chr> 
    ## $ species                     <chr> 
    ## $ length_mm                   <dbl> 
    ## $ number                      <dbl> 
    ## $ range                       <dbl> 
    ## $ rms                         <dbl> 
    ## $ precision                   <dbl> 
    ## $ status                      <chr> 
    ## $ date_time                   <chr> 
    ## $ location                    <chr> 
    ## $ site                        <chr> 
    ## $ depth_m                     <chr> 
    ## $ successful_count            <chr> 
    ## $ successful_length           <chr> 
    ## $ successful_habitat_forward  <chr> 
    ## $ successful_habitat_backward <chr> 
    ## $ marine_region               <chr> 
    ## $ scientific                  <chr>

### Samples where the MaxN does not equal the number of length measurements

### Percent of MaxN Measured

## Save the checked data

Save MaxN as an R data file.

``` r
saveRDS(complete_count,
          file = here::here(paste0("r-workflows/data/staging/",
                       name, "_complete-count.rds")))
```

Save lengths as an R data file.

``` r
saveRDS(complete_length,
          file = here::here(paste0("r-workflows/data/staging/",
                       name, "_complete-length.rds")))
```
