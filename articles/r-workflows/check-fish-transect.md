# Check fish data from transect based methods

## Introduction

This script will take fish annotation data from transect based methods
(e.g. stereo-DOVs or ROVs), either exported from EventMeasure or
“Generic” format (see format requirements
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
library(lutz)
```

Next set the study name. This can be any name you like, all files saved
using this script will be prefixed with this name. Avoid having a name
that is too long. We recommend using a short project name that includes
the method e.g. “2020_ningaloo_stereo-BRUVs”.

``` r
name <- "example-dov-workflow"
```

## Metadata

Now we load and tidy the metadata. If you have already completed this
step while checking your habitat data, then you can skip these next two
chunks of code and simply read in the metadata (see below).

``` r
# TODO remove all non-numeric from the period column in metadata, will also need to remove in EMOBs - make it optional
# Do people call periods, a, b, c

# TODO add a check to say how many files there are of a certain type
# TODO flag if file type is different for all files
# TODO fuzzy join naming to see if I can find the potential file it is meant to be
# TODO check for missing metadata
# TODO Have a think about generic count data - how to include this (e.g. JCU missing length data but having abundance)
# TODO Check to see if the metadata has a fuzzy join if there is no matching count or length
# TODO - Add examples of completing by location, campaign, marine_region - don't want zeros for australian species in Fiji
# TODO - add biomass

metadata <- read_metadata(here::here("r-workflows/data/raw/DOVs/"), method = "DOVs") %>% # Change here to "DOVs"
  dplyr::select(campaignid, sample, status, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length) %>%
  dplyr::glimpse()
```

    ## reading metadata file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Marmion.MP.Monitoring_stereoDOVs_Metadata.csv

    ## Rows: 1
    ## Columns: 14
    ## $ sample            <chr> "3 Mile-01"
    ## $ latitude_dd       <chr> "-31.78333"
    ## $ longitude_dd      <chr> "115.67861"
    ## $ date              <chr> "20170401"
    ## $ time              <chr> "11:00:00"
    ## $ location          <chr> "Marmion Marine Park"
    ## $ status            <chr> "Fished"
    ## $ site              <chr> "3 Mile"
    ## $ successful_count  <chr> "Yes"
    ## $ successful_length <chr> "Yes"
    ## $ depth_m           <chr> "11.5"
    ## $ observer_count    <chr> "Caprice Hyde"
    ## $ transect_length   <chr> "25"
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs"

    ## the date_time column is missing from: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Marmion.MP.Monitoring_stereoDOVs_Metadata.csv. Creating column now....

    ## Joining with `by = join_by(sample, campaignid)`
    ## Joining with `by = join_by(date, timezone)`
    ## reading metadata file:
    ## /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Shoalwater.MP.Monitoring_stereoDOVs_Metadata.csv

    ## Rows: 1
    ## Columns: 14
    ## $ opcode            <chr> "BP_SZ_20170428_DOV"
    ## $ period            <chr> "T1"
    ## $ status            <chr> "No-take"
    ## $ latitude_dd       <chr> "-32.3643"
    ## $ longitude_dd      <chr> "115.6915"
    ## $ date              <chr> "20170428"
    ## $ time              <chr> "11:30:00"
    ## $ location          <chr> "Shoalwater Islands"
    ## $ site              <chr> "Becher Point SZ 1"
    ## $ successful_count  <chr> "Yes"
    ## $ successful_length <chr> "Yes"
    ## $ depth_m           <chr> "5"
    ## $ observer_count    <chr> "Caprice"
    ## $ campaignid        <chr> "2017-04_Shoalwater.MP.Monitoring_stereoDOVs"

    ## the date_time column is missing from: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Shoalwater.MP.Monitoring_stereoDOVs_Metadata.csv. Creating column now....
    ## Joining with `by = join_by(campaignid, sample)`Joining with `by = join_by(date, timezone)`

    ## Rows: 2
    ## Columns: 11
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs", "2017-04…
    ## $ sample            <chr> "3 Mile-01", "BP_SZ_20170428_DOV-T1"
    ## $ status            <chr> "Fished", "No-take"
    ## $ longitude_dd      <chr> "115.67861", "115.6915"
    ## $ latitude_dd       <chr> "-31.78333", "-32.3643"
    ## $ date_time         <chr> "2017-04-01T11:00:00+08:00", "2017-04-28T11:30:00+08…
    ## $ location          <chr> "Marmion Marine Park", "Shoalwater Islands"
    ## $ site              <chr> "3 Mile", "Becher Point SZ 1"
    ## $ depth_m           <chr> "11.5", "5"
    ## $ successful_count  <chr> "Yes", "Yes"
    ## $ successful_length <chr> "Yes", "Yes"

Save the metadata as an R data file (this creates a lighter file than
saving as a .csv or similar, it also maintains any column formatting).

``` r
saveRDS(metadata, file = here::here(paste0("r-workflows/data/tidy/",
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

    ## Rows: 2
    ## Columns: 10
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs", "2017-04…
    ## $ sample            <chr> "3 Mile-01", "BP_SZ_20170428_DOV-T1"
    ## $ status            <chr> "Fished", "No-take"
    ## $ date_time         <chr> "2017-04-01T11:00:00+08:00", "2017-04-28T11:30:00+08…
    ## $ location          <chr> "Marmion Marine Park", "Shoalwater Islands"
    ## $ site              <chr> "3 Mile", "Becher Point SZ 1"
    ## $ depth_m           <chr> "11.5", "5"
    ## $ successful_count  <chr> "Yes", "Yes"
    ## $ successful_length <chr> "Yes", "Yes"
    ## $ marine_region     <chr> "South-west", "South-west"

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
points <- read_points(here::here("r-workflows/data/raw/DOVs/")) %>%
  glimpse()
```

    ## Rows: 0
    ## Columns: 23
    ## $ opcode      <chr> 
    ## $ pointindex  <chr> 
    ## $ filename    <chr> 
    ## $ frame       <chr> 
    ## $ time        <chr> 
    ## $ period      <chr> 
    ## $ periodtime  <chr> 
    ## $ imagecol    <chr> 
    ## $ imagerow    <chr> 
    ## $ rectwidth   <chr> 
    ## $ rectheight  <chr> 
    ## $ family      <chr> 
    ## $ genus       <chr> 
    ## $ species     <chr> 
    ## $ code        <chr> 
    ## $ number      <dbl> 
    ## $ stage       <chr> 
    ## $ activity    <chr> 
    ## $ comment     <chr> 
    ## $ attribute9  <chr> 
    ## $ attribute10 <chr> 
    ## $ campaignid  <chr> 
    ## $ sample      <chr>

### Load any *Generic* count files

This section of code will read in any Count.csv files that you have
saved in the directory you set. It will combine all of the files into
one data frame, and get the campaignid name from the name of the file.

It is important that you consistently name your files with the same
campaignid (look out for different separators e.g. ‘-’, ’\_‘, or’.’)

``` r
counts <- read_counts(here::here("r-workflows/data/raw/DOVs/"), method = "DOVs") %>% # Change here to "DOVs"
  glimpse()
```

    ## reading count file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Marmion.MP.Monitoring_stereoDOVs_Count.csv

    ## file is a csv

    ## Rows: 78
    ## Columns: 7
    ## $ sample     <chr> "3 Mile-01", "3 Mile-01", "3 Mile-01", "3 Mile-01", "3 Mile…
    ## $ family     <chr> "Labridae", "Labridae", "Labridae", "Labridae", "Labridae",…
    ## $ genus      <chr> "Coris", "Coris", "Coris", "Coris", "Coris", "Coris", "Parm…
    ## $ species    <chr> "auricularis", "auricularis", "auricularis", "auricularis",…
    ## $ count      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ transect   <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
    ## $ campaignid <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs", "2017-04_Marmio…

### Load any EventMeasure Lengths.txt and/or 3DPoints.txt files

This section of code will read in any Lengths.txt and/or 3DPoints.txt
files that you have saved in the directory you set. It will combine all
of the files into one data-frame, and get the campaignid name from the
name of the file. It is important that you consistently name your files
with the same campaignid (look out for different separators e.g. ‘-’,
’\_‘, or’.’)

``` r
em_length3dpoints <- read_em_length(here::here("r-workflows/data/raw/DOVs/"), method = "DOVs") %>% # Change here to "DOVs"
  dplyr::select(-c(any_of("comment"))) %>% # there is a comment column in metadata, so you will need to remove this column from EM data
  dplyr::inner_join(metadata, by = join_by(sample, campaignid)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  # dplyr::rename(length_mm = length) %>%
  glimpse()
```

    ## Rows: 27
    ## Columns: 44
    ## $ opcode            <chr> "BP_SZ_20170428_DOV", "BP_SZ_20170428_DOV", "BP_SZ_2…
    ## $ imageptpair       <chr> "3", "4", "5", "6", "7", "8", "11", "12", "14", "19"…
    ## $ filenameleft      <chr> "Becher Point SZ_20170428_LEFT110.avi", "Becher Poin…
    ## $ frameleft         <chr> "17390", "17393", "17393", "17600", "17682", "17692"…
    ## $ filenameright     <chr> "Becher Point SZ_20170428_Right111.avi", "Becher Poi…
    ## $ frameright        <chr> "18111", "18114", "18114", "18321", "18403", "18413"…
    ## $ time              <chr> "11.59333", "11.59533", "11.59533", "11.73333", "11.…
    ## $ period            <chr> "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1"…
    ## $ periodtime        <chr> "0.338", "0.34", "0.34", "0.478", "0.53267", "0.5393…
    ## $ x                 <dbl> 2134.6941, 1376.5557, 779.0388, -456.7284, -643.7255…
    ## $ y                 <dbl> 414.20359, 558.91309, 297.44439, -39.05379, -691.681…
    ## $ z                 <dbl> -4598.665, -3416.569, -2294.869, -4340.940, -2352.72…
    ## $ sx                <chr> "9.83", "4.93609", "2.05508", "2.68764", "1.81621", …
    ## $ sy                <chr> "2.40788", "2.22716", "1.12553", "1.90913", "2.08107…
    ## $ sz                <chr> "21.82321", "12.38573", "5.7018", "19.84829", "5.801…
    ## $ rms               <dbl> 9.01207, 4.47500, 2.66084, 1.05090, 0.77775, 0.25224…
    ## $ range             <dbl> 5086.866, 3725.619, 2441.679, 4365.076, 2535.372, 20…
    ## $ direction         <chr> "25.56772", "23.98894", "20.43836", "6.04787", "20.9…
    ## $ family            <chr> "Kyphosidae", "Kyphosidae", "Kyphosidae", "Pomacentr…
    ## $ genus             <chr> "Kyphosus", "Kyphosus", "Kyphosus", "Parma", "Notola…
    ## $ species           <chr> "sydneyanus", "sydneyanus", "sydneyanus", "mcculloch…
    ## $ code              <chr> "37361001", "37361001", "37361001", "37372093", "373…
    ## $ number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ stage             <chr> "AD", "AD", "AD", "AD", "J", "AD", "AD", "AD", "AD",…
    ## $ activity          <chr> "Passing", "Passing", "Passing", "Passing", "Passing…
    ## $ attribute9        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ attribute10       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ campaignid        <chr> "2017-04_Shoalwater.MP.Monitoring_stereoDOVs", "2017…
    ## $ length_mm         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 496.…
    ## $ precision         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3.66…
    ## $ horzdir           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "12.…
    ## $ vertdir           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "14.…
    ## $ midx              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -348…
    ## $ midy              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 80.6…
    ## $ midz              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -297…
    ## $ sample            <chr> "BP_SZ_20170428_DOV-T1", "BP_SZ_20170428_DOV-T1", "B…
    ## $ status            <chr> "No-take", "No-take", "No-take", "No-take", "No-take…
    ## $ date_time         <chr> "2017-04-28T11:30:00+08:00", "2017-04-28T11:30:00+08…
    ## $ location          <chr> "Shoalwater Islands", "Shoalwater Islands", "Shoalwa…
    ## $ site              <chr> "Becher Point SZ 1", "Becher Point SZ 1", "Becher Po…
    ## $ depth_m           <chr> "5", "5", "5", "5", "5", "5", "5", "5", "5", "5", "5…
    ## $ successful_count  <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ successful_length <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ marine_region     <chr> "South-west", "South-west", "South-west", "South-wes…

### Load any *Generic* length files

This section of code will read in any *Length.csv files that you have
saved in the directory you set. It will combine all of the files into
one data frame, and get the campaignid name from the name of the file.
It is important that you consistently name your files with the same
campaignid (look out for different separators e.g. ‘-’, ’*’, or ‘.’)

##### Note: In this section we also add any extra counts that were not measured.

For example, if 10 of Species X were counted in a transect, but only 6
of them were measured, we add an extra 4 of Species X to the gen_length
dataframe with length = NA (like a 3D point in EM data)

``` r
gen_length <- read_gen_length(here::here("r-workflows/data/raw/DOVs/"), method = "DOVs") %>% # Change here to "DOVs"
  dplyr::full_join(metadata, by = join_by(campaignid, sample)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse()
```

    ## reading length file: /home/runner/work/CheckEM/CheckEM/r-workflows/data/raw/DOVs//2017-04_Marmion.MP.Monitoring_stereoDOVs_Length.csv

    ## file is a csv

    ## Rows: 47
    ## Columns: 20
    ## $ sample            <chr> "3 Mile-01", "3 Mile-01", "3 Mile-01", "3 Mile-01", …
    ## $ family            <chr> "Labridae", "Labridae", "Labridae", "Labridae", "Lab…
    ## $ genus             <chr> "Austrolabrus", "Coris", "Coris", "Coris", "Coris", …
    ## $ species           <chr> "maculatus", "auricularis", "auricularis", "auricula…
    ## $ count             <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ length_mm         <dbl> 132.757, 134.102, 120.772, 118.868, 104.315, 195.797…
    ## $ transect          <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1…
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs", "2017-04…
    ## $ number            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ range             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ rms               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ precision         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ status            <chr> "Fished", "Fished", "Fished", "Fished", "Fished", "F…
    ## $ date_time         <chr> "2017-04-01T11:00:00+08:00", "2017-04-01T11:00:00+08…
    ## $ location          <chr> "Marmion Marine Park", "Marmion Marine Park", "Marmi…
    ## $ site              <chr> "3 Mile", "3 Mile", "3 Mile", "3 Mile", "3 Mile", "3…
    ## $ depth_m           <chr> "11.5", "11.5", "11.5", "11.5", "11.5", "11.5", "11.…
    ## $ successful_count  <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ successful_length <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ marine_region     <chr> "South-west", "South-west", "South-west", "South-wes…

``` r
counts_summary <- counts %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(total_count = sum(count)) %>%
  dplyr::ungroup()
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by campaignid, sample, family, genus, and
    ##   species.
    ## ℹ Output is grouped by campaignid, sample, family, and genus.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(campaignid, sample, family, genus, species))` for
    ##   per-operation grouping (`?dplyr::dplyr_by`) instead.

``` r
gen_length_missing <- gen_length %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(total_length = sum(number)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(counts_summary) %>%
  dplyr::mutate(difference = total_count - total_length) %>%
  dplyr::filter(difference > 0) %>% # remove rows where there are more lengths than counts
  tidyr::uncount(difference) %>%
  dplyr::select(-c(total_length, total_count)) %>%
  dplyr::mutate(number = 1, length_mm = NA) %>%
  dplyr::left_join(metadata)
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by campaignid, sample, family, genus, and
    ##   species.
    ## ℹ Output is grouped by campaignid, sample, family, and genus.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(campaignid, sample, family, genus, species))` for
    ##   per-operation grouping (`?dplyr::dplyr_by`) instead.

    ## Joining with `by = join_by(campaignid, sample, family, genus, species)`
    ## Joining with `by = join_by(campaignid, sample)`

``` r
gen_length_combined <- bind_rows(gen_length, gen_length_missing)
```

### Combine EventMeasure and *Generic* length data

``` r
# If only EventMeasure data then length only includes Length and 3D points data
# If only Generic data then length only includes generic length data
# If both exist, then length includes both Length and 3D points and generic length data
length <- bind_rows(get0("em_length3dpoints"), get0("gen_length_combined")) # this works even if you only have one type of data
```

### Format and add zeros where a species isn’t present

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
    ## site, depth_m, successful_count, successful_length, marine_region)`
    ## Joining with `by = join_by(campaignid, sample)`

    ## Rows: 113
    ## Columns: 18
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs", "2017-04…
    ## $ sample            <chr> "3 Mile-01", "3 Mile-01", "3 Mile-01", "3 Mile-01", …
    ## $ family            <chr> "Blenniidae", "Kyphosidae", "Labridae", "Labridae", …
    ## $ genus             <chr> "Plagiotremus", "Kyphosus", "Austrolabrus", "Austrol…
    ## $ species           <chr> "tapeinosoma", "sydneyanus", "maculatus", "maculatus…
    ## $ length_mm         <dbl> NA, NA, 132.757, NA, 134.102, 120.772, 118.868, 104.…
    ## $ number            <dbl> 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ range             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ rms               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ precision         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ status            <chr> "Fished", "Fished", "Fished", "Fished", "Fished", "F…
    ## $ date_time         <chr> "2017-04-01T11:00:00+08:00", "2017-04-01T11:00:00+08…
    ## $ location          <chr> "Marmion Marine Park", "Marmion Marine Park", "Marmi…
    ## $ site              <chr> "3 Mile", "3 Mile", "3 Mile", "3 Mile", "3 Mile", "3…
    ## $ depth_m           <chr> "11.5", "11.5", "11.5", "11.5", "11.5", "11.5", "11.…
    ## $ successful_count  <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ successful_length <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Ye…
    ## $ marine_region     <chr> "South-west", "South-west", "South-west", "South-wes…

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

    ## 2 unique samples in the metadata

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

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by campaignid and sample.
    ## ℹ Output is grouped by campaignid.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(campaignid, sample))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

``` r
message(paste(nrow(duplicate_samples), "samples duplicated in the metadata"))
```

    ## 0 samples duplicated in the metadata

### Number of sample(s) without length or 3D point data

This could be due to samples not observing fish (not an error) or a
sample that should be marked as successful_length = No. It could also be
due to a sample name spelt incorrectly in the EMObs or Length file or
the sample metadata file.

``` r
# TODO Check to see if any samples without abundance data
# TODO check to see if any samples without length data (not including 3D points)
# Do that by changing
# samples <- length %>%
# dplyr::filter(length_mm >0) %>%
#   distinct(campaignid, sample)
# TODO - include location, site
# TODO - summarise % transects with zero by opcode
# IDEA - if last or consective transects at the end of an opcode are empty but marked as successful
# lead or lag - flag if the leading row is also zero.

metadata_samples <- metadata %>%
  dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")),
                successful_count, successful_length, location, site) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
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
# TODO - only check these if both count and length are not successful

periods <- read_periods(here::here("r-workflows/data/raw/DOVs/"), method = "DOVs") %>%
  glimpse()
```

    ## Rows: 1
    ## Columns: 12
    ## $ opcode        <chr> "BP_SZ_20170428_DOV"
    ## $ periodindex   <chr> "0"
    ## $ period        <chr> "T1"
    ## $ filenamestart <chr> "Becher Point SZ_20170428_LEFT110.avi"
    ## $ framestart    <chr> "16883"
    ## $ time_start    <dbl> 11.25533
    ## $ filenameend   <chr> "Becher Point SZ_20170428_LEFT110.avi"
    ## $ frameend      <chr> "18942"
    ## $ time_end      <dbl> 12.628
    ## $ has_end       <chr> "1"
    ## $ campaignid    <chr> "2017-04_Shoalwater.MP.Monitoring_stereoDOVs"
    ## $ sample        <chr> "BP_SZ_20170428_DOV-T1"

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

    ## 1 samples missing period

``` r
glimpse(missing_periods)
```

    ## Rows: 1
    ## Columns: 3
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs"
    ## $ successful_count  <chr> "Yes"
    ## $ successful_length <chr> "Yes"

``` r
lengths_outside_periods <- em_length3dpoints %>%
  dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
  dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number)

message(paste(nrow(lengths_outside_periods), "lengths/3D points outside period"))
```

    ## 0 lengths/3D points outside period

``` r
glimpse(lengths_outside_periods)
```

    ## Rows: 0
    ## Columns: 7
    ## $ campaignid <chr> 
    ## $ opcode     <chr> 
    ## $ period     <chr> 
    ## $ family     <chr> 
    ## $ genus      <chr> 
    ## $ species    <chr> 
    ## $ number     <dbl>

This is the total number of individuals observed in the length data:

``` r
total_length <- sum(complete_length$number)
message(paste(total_length, "fish counted in the length data"))
```

    ## 107 fish counted in the length data

### Length measurements or 3D points without a number (EM only)

This is a check for EventMeasure data. Sometimes analysts will add 3D
points to record the sync point. These can remain in the data but you
should double check that no fish are accidentally missing a number.

``` r
lengths_without_number <- em_length3dpoints %>%
  filter(number %in% c("NA", NA, 0, NULL, "", " "))

message(paste(nrow(lengths_without_number), "lengths or 3D points in the EMObs that do not have a number"))
```

    ## 0 lengths or 3D points in the EMObs that do not have a number

``` r
glimpse(lengths_without_number)
```

    ## Rows: 0
    ## Columns: 44
    ## $ opcode            <chr> 
    ## $ imageptpair       <chr> 
    ## $ filenameleft      <chr> 
    ## $ frameleft         <chr> 
    ## $ filenameright     <chr> 
    ## $ frameright        <chr> 
    ## $ time              <chr> 
    ## $ period            <chr> 
    ## $ periodtime        <chr> 
    ## $ x                 <dbl> 
    ## $ y                 <dbl> 
    ## $ z                 <dbl> 
    ## $ sx                <chr> 
    ## $ sy                <chr> 
    ## $ sz                <chr> 
    ## $ rms               <dbl> 
    ## $ range             <dbl> 
    ## $ direction         <chr> 
    ## $ family            <chr> 
    ## $ genus             <chr> 
    ## $ species           <chr> 
    ## $ code              <chr> 
    ## $ number            <dbl> 
    ## $ stage             <chr> 
    ## $ activity          <chr> 
    ## $ attribute9        <chr> 
    ## $ attribute10       <chr> 
    ## $ campaignid        <chr> 
    ## $ length_mm         <dbl> 
    ## $ precision         <dbl> 
    ## $ horzdir           <chr> 
    ## $ vertdir           <chr> 
    ## $ midx              <dbl> 
    ## $ midy              <dbl> 
    ## $ midz              <dbl> 
    ## $ sample            <chr> 
    ## $ status            <chr> 
    ## $ date_time         <chr> 
    ## $ location          <chr> 
    ## $ site              <chr> 
    ## $ depth_m           <chr> 
    ## $ successful_count  <chr> 
    ## $ successful_length <chr> 
    ## $ marine_region     <chr>

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

    ## 0 synonyms used in the length data

``` r
glimpse(synonyms_in_length)
```

    ## Rows: 0
    ## Columns: 2
    ## $ `old name` <chr> 
    ## $ `new name` <chr>

#### Change synonyms names in data

Now that we have identified species names that have been updated or
spelt wrong you need to decide if you want to change the names in your
data or continue using the old names. If you want to update the names
use the next two chunks. If you would like to retain the old names skip
the next two chunks.

**NOTE:** this does not change your original annotation files, only the
data you save at the end of the script.

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
    ## site, depth_m, successful_count, successful_length, marine_region)`

``` r
message(paste(nrow(length_species_not_in_list), "species not in chosen life history list"))
```

    ## 0 species not in chosen life history list

``` r
glimpse(length_species_not_in_list)
```

    ## Rows: 0
    ## Columns: 4
    ## $ campaignid <chr> 
    ## $ family     <chr> 
    ## $ genus      <chr> 
    ## $ species    <chr>

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

    ## 0 lengths are too small

``` r
glimpse(too_small)
```

    ## Rows: 0
    ## Columns: 11
    ## $ campaignid        <chr> 
    ## $ family            <chr> 
    ## $ genus             <chr> 
    ## $ species           <chr> 
    ## $ length_mm         <dbl> 
    ## $ min_length_mm     <dbl> 
    ## $ max_length_mm     <dbl> 
    ## $ length_max_mm     <dbl> 
    ## $ reason            <chr> 
    ## $ difference        <dbl> 
    ## $ percent_of_fb_max <dbl>

``` r
message(paste(nrow(too_big), "lengths are too big"))
```

    ## 1 lengths are too big

``` r
glimpse(too_big)
```

    ## Rows: 1
    ## Columns: 11
    ## $ campaignid        <chr> "2017-04_Marmion.MP.Monitoring_stereoDOVs"
    ## $ family            <chr> "Pomacentridae"
    ## $ genus             <chr> "Parma"
    ## $ species           <chr> "occidentalis"
    ## $ length_mm         <dbl> 283.05
    ## $ min_length_mm     <dbl> 45
    ## $ max_length_mm     <dbl> 255
    ## $ length_max_mm     <dbl> 300
    ## $ reason            <chr> "too big"
    ## $ difference        <dbl> 28.05
    ## $ percent_of_fb_max <dbl> 94.35

### Number of 3D points and length measurements over the RMS limit (EM only)

In this check you can set the RMS limit, and then identify any
measurements that have a larger RMS.

``` r
# TODO split this by lengths and 3D points

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
    ## Columns: 19
    ## $ campaignid        <chr> 
    ## $ sample            <chr> 
    ## $ family            <chr> 
    ## $ genus             <chr> 
    ## $ species           <chr> 
    ## $ length_mm         <dbl> 
    ## $ number            <dbl> 
    ## $ range             <dbl> 
    ## $ rms               <dbl> 
    ## $ precision         <dbl> 
    ## $ status            <chr> 
    ## $ date_time         <chr> 
    ## $ location          <chr> 
    ## $ site              <chr> 
    ## $ depth_m           <chr> 
    ## $ successful_count  <chr> 
    ## $ successful_length <chr> 
    ## $ marine_region     <chr> 
    ## $ scientific        <chr>

### Number of length measurements over the precision limit (EM only)

In this check you can set the precision limit, and then identify any
measurements that have a larger precision.

``` r
# TODO - change to be a % precision/length_mm * 100

precision_limit <- 10 # in %

over_precision <- complete_length %>%
  dplyr::filter(as.numeric(precision) > precision_limit)

message(paste(nrow(over_precision), "lengths over precision limit"))
```

    ## 2 lengths over precision limit

``` r
glimpse(over_precision)
```

    ## Rows: 2
    ## Columns: 19
    ## $ campaignid        <chr> "2017-04_Shoalwater.MP.Monitoring_stereoDOVs", "2017…
    ## $ sample            <chr> "BP_SZ_20170428_DOV-T1", "BP_SZ_20170428_DOV-T1"
    ## $ family            <chr> "Kyphosidae", "Kyphosidae"
    ## $ genus             <chr> "Kyphosus", "Kyphosus"
    ## $ species           <chr> "sydneyanus", "sydneyanus"
    ## $ length_mm         <dbl> 631.3505, 402.7545
    ## $ number            <dbl> 1, 1
    ## $ range             <dbl> 4077.000, 4166.826
    ## $ rms               <dbl> 12.28661, 10.75454
    ## $ precision         <dbl> 19.80021, 10.31061
    ## $ status            <chr> "No-take", "No-take"
    ## $ date_time         <chr> "2017-04-28T11:30:00+08:00", "2017-04-28T11:30:00+08…
    ## $ location          <chr> "Shoalwater Islands", "Shoalwater Islands"
    ## $ site              <chr> "Becher Point SZ 1", "Becher Point SZ 1"
    ## $ depth_m           <chr> "5", "5"
    ## $ successful_count  <chr> "Yes", "Yes"
    ## $ successful_length <chr> "Yes", "Yes"
    ## $ marine_region     <chr> "South-west", "South-west"
    ## $ scientific        <chr> "Kyphosidae Kyphosus sydneyanus", "Kyphosidae Kyphos…

### Number of 3D points and length measurements over the range limit (EM only)

In this check you can set the range limit, and then identify any
measurements that have a larger range.

``` r
range_limit <- 10 # in metres

over_range <- complete_length %>%
  dplyr::filter(as.numeric(range) > (range_limit * 1000))

message(paste(nrow(over_range), "lengths over range limit"))
```

    ## 0 lengths over range limit

``` r
glimpse(over_range)
```

    ## Rows: 0
    ## Columns: 19
    ## $ campaignid        <chr> 
    ## $ sample            <chr> 
    ## $ family            <chr> 
    ## $ genus             <chr> 
    ## $ species           <chr> 
    ## $ length_mm         <dbl> 
    ## $ number            <dbl> 
    ## $ range             <dbl> 
    ## $ rms               <dbl> 
    ## $ precision         <dbl> 
    ## $ status            <chr> 
    ## $ date_time         <chr> 
    ## $ location          <chr> 
    ## $ site              <chr> 
    ## $ depth_m           <chr> 
    ## $ successful_count  <chr> 
    ## $ successful_length <chr> 
    ## $ marine_region     <chr> 
    ## $ scientific        <chr>

### Number of 3D points and length measurements outside transect (EM only)

In this check you can set the transect belt width and then identify any
measurements that are outside that transect.

``` r
transect_belt_width <- 5 # in metres

transect_limit <- (transect_belt_width*1000)/2

length_outside_transect <- length %>%
  dplyr::mutate(x = as.numeric(x),
                y = as.numeric(y),
                z = as.numeric(z),
                midx = as.numeric(midx),
                midy = as.numeric(midy),
                midz = as.numeric(midz)) %>%
  tidyr::replace_na(list(x = 0, y = 0, midx = 0, midy = 0)) %>%
  dplyr::filter(c(midx > transect_limit | midx < -transect_limit | midy > transect_limit | midy < -transect_limit | x > transect_limit | x < -transect_limit | y > transect_limit | y < -transect_limit))

message(paste(nrow(length_outside_transect), "lengths outside transect limit"))
```

    ## 0 lengths outside transect limit

``` r
glimpse(length_outside_transect)
```

    ## Rows: 0
    ## Columns: 46
    ## $ opcode            <chr> 
    ## $ imageptpair       <chr> 
    ## $ filenameleft      <chr> 
    ## $ frameleft         <chr> 
    ## $ filenameright     <chr> 
    ## $ frameright        <chr> 
    ## $ time              <chr> 
    ## $ period            <chr> 
    ## $ periodtime        <chr> 
    ## $ x                 <dbl> 
    ## $ y                 <dbl> 
    ## $ z                 <dbl> 
    ## $ sx                <chr> 
    ## $ sy                <chr> 
    ## $ sz                <chr> 
    ## $ rms               <dbl> 
    ## $ range             <dbl> 
    ## $ direction         <chr> 
    ## $ family            <chr> 
    ## $ genus             <chr> 
    ## $ species           <chr> 
    ## $ code              <chr> 
    ## $ number            <dbl> 
    ## $ stage             <chr> 
    ## $ activity          <chr> 
    ## $ attribute9        <chr> 
    ## $ attribute10       <chr> 
    ## $ campaignid        <chr> 
    ## $ length_mm         <dbl> 
    ## $ precision         <dbl> 
    ## $ horzdir           <chr> 
    ## $ vertdir           <chr> 
    ## $ midx              <dbl> 
    ## $ midy              <dbl> 
    ## $ midz              <dbl> 
    ## $ sample            <chr> 
    ## $ status            <chr> 
    ## $ date_time         <chr> 
    ## $ location          <chr> 
    ## $ site              <chr> 
    ## $ depth_m           <chr> 
    ## $ successful_count  <chr> 
    ## $ successful_length <chr> 
    ## $ marine_region     <chr> 
    ## $ count             <chr> 
    ## $ transect          <chr>

``` r
# 1. Check for missing length weight relationship
# taxa_missing_lw <- complete_length %>%
#   dplyr::distinct(family, genus, species) %>%
#   dplyr::anti_join(filter(CheckEM::australia_life_history, !is.na(a)), by = c("family", "genus", "species"))
#
# #2. Fill length data with relevant a and b and if blank use family---
# length_species_ab <- CheckEM::australia_life_history %>% # done this way around to avoid duplicating Family coloum
#   dplyr::select(-family) %>%
#   dplyr::inner_join(length, ., by = c("genus", "species")) # only keeps row if has a and b
#
# # 3. Make family length.weight
# family_lw <- CheckEM::australia_life_history %>%
#   dplyr::group_by(family, length_max_type) %>%
#   dplyr::mutate(log.a = log10(a)) %>%
#   dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
#                    b = mean(b, na.rm = T),
#                    all = mean(all, na.rm = T),
#                    bll = mean(bll, na.rm = T)) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(!is.na(a)) %>%
#   dplyr::mutate(all = str_replace_all(all, "NaN", "0")) %>%
#   dplyr::mutate(bll = str_replace_all(bll, "NaN", "1")) %>%
#   dplyr::mutate(all = as.numeric(all)) %>%
#   dplyr::mutate(bll = as.numeric(bll)) %>%
#   dplyr::mutate(rank = ifelse(length_max_type == "FL", 1, ifelse(length_max_type == "TL", 2, 3))) %>%
#   dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE)) %>%
#   dplyr::filter(min.rank ==  0)
#
# length_family_ab <- length %>%
#   dplyr::anti_join(australia_life_history(), by = c("genus", "species")) %>%
#   dplyr::left_join(family_lw, by = "family")
#
# # 5. Fill length data with relevant a and b and if blank use family---
# complete.length.number.mass <- length_species_ab %>%
#   bind_rows(length_family_ab) %>%
#   dplyr::filter(!is.na(a)) %>% #this gets rid of species with no lw
#   dplyr::mutate(length.cm = length_mm/10) %>%
#   dplyr::mutate(all = ifelse(is.na(all) & length_max_type%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
#   dplyr::mutate(bll = ifelse(is.na(bll) & length_max_type%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
#   dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>%
#   dplyr::mutate(mass.g = (adjLength^b)*a*number) %>%
#   dplyr::filter(mass.g>0) %>%
#   dplyr::full_join(metadata.regions()) %>%
#   dplyr::select(c(campaignid, sample, family, genus, species, length_mm, number, mass.g, length.cm, code)) %>% # removed EM columns
#   tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
#   tidyr::replace_na(list(mass.g = 0)) %>%
#   dplyr::mutate(mass_kg = mass.g/1000)
```

## Save the checked data

``` r
saveRDS(complete_length,
          file = here::here(paste0("r-workflows/data/staging/",
                       name, "_complete-length.rds")))
```
