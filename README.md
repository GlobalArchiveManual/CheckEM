[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2FGlobalArchiveManual%2FCheckEM&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=views&edge_flat=false)](https://hits.seeyoufarm.com)

# Stereo-video workflows for fish and benthic ecologists
<div style="text-align: justify">Stereo imagery is widely used by research institutions and management bodies around the world as a cost-effective and non-destructive method to research and monitor fish and habitats (Whitmarsh, Fairweather and Huveneers, 2017). Stereo-video can provide accurate and precise size and range measurements and can be used to study spatial and temporal patterns in fish assemblages (McLean et al., 2016), habitat composition and complexity (Collins et al., 2017), behaviour (Goetze et al., 2017), responses to anthropogenic pressures (Bosch et al., 2022) and the recovery and growth of benthic fauna (Langlois et al. 2020). It is important that users of stereo-video collect, annotate, quality control and store their data in a consistent manner, to ensure data produced is of the highest quality possible and to enable large scale collaborations. Here we collate existing best practices and propose new tools to equip ecologists to ensure that all aspects of the stereo-video workflow are performed in a consistent way.

![](man/figures/checkem_workflow.png)

## Quick Links

<table><thead>
  <tr>
    <th>Field Manuals</th>
    <th>Video Annotation Guides</th>
    <th>CheckEM</th>
    <th>GlobalArchive</th>
  </tr></thead>
<tbody>
  <tr>
    <td>
    [stereo-BRUVs](https://benthic-bruvs-field-manual.github.io/)<br>
    [stereo-DOVs](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13189)<br>
    [stereo-BOSS](https://drop-camera-field-manual.github.io/)
    </td>
    <td>
    [EventMeasure](https://globalarchivemanual.github.io/CheckEM/articles/manuals/EventMeasure_annotation_guide.html)<br>
    [TransectMeasure](https://globalarchivemanual.github.io/CheckEM/articles/manuals/TransectMeasure_annotation_guide.html)
    </td>
    <td>
    [Launch app](https://marine-ecology.shinyapps.io/CheckEM/)<br>
    [User guide](https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html)<br>
    [How to video](https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_video.html)
    </td>
    <td>
    [Website](https://globalarchive.org/)<br>
    [User guide](https://globalarchivemanual.github.io/)
    </td>
  </tr>
</tbody>
</table>

## 1. Data Collection
Standardised protocols reduce variation in methodologies among researchers, and encourage the use of Findable, Accessible, Interoperable and Reusable (FAIR, Wilkinson et al., 2016) protocols, this increases collaborations and allows researchers to answer broadscale ecological questions. Two standard operating procedures for field data collection have been published:

* A globally endorsed best-practice field manual for baited remote underwater stereo-video (stereo-BRUVs) was published in 2020 (Langlois et al., 2020). The field manual is available online [here](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13470).

* A best-practice field manual for diver operated stereo-video (stereo-DOVs) was published in 2019. The field manual can be found online [here](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13189).

## 2. Video Annotation
The two field manuals mentioned above outline the standard protocols for video annotation. We have further developed these by creating step-by-step annotation guides for fish and habitat using two common software developed by [SeaGIS](https://www.seagis.com.au/), [EventMeasure](https://www.seagis.com.au/event.html) and [TransectMeasure](https://www.seagis.com.au/transect.html):

* The annotation guide for [EventMeasure](articles/manuals/EventMeasure_annotation_guide.html) outlines how to obtain count data and length measurements from stereo-BRUV imagery.

* The annotation guide for [TransectMeasure](articles/manuals/TransectMeasure_annotation_guide.html) outlines a rapid approach to characterise benthic composition and complexity from horizontally facing imagery (including stereo-BRUVs and panoramic drop cameras), adapting existing standardised schema for benthic composition (CATAMI classification scheme) and benthic complexity as per Wilson et al. (2006).

## 3. Quality Control
Robust and automated quality control procedures are essential to consistently identify errors in stereo-video data and provide immediate feedback before datasets are contributed for environmental reporting (Campbell et al., 2013). We have developed an interactive web-based application, called CheckEM, to allow users to compare annotations against life-history information, create interactive plots and tables in a graphical interface, and provide summarised data and error reports to download, and in addition the ability to compile multiple discrete annotation data sets into a single synthesis ready for databasing or analysis. A free online version of the app can be accessed via [this link](https://marine-ecology.shinyapps.io/CheckEM/) or you can run a local version of the CheckEM application (without the internet) using R (see [instructions below](#shiny-app)). A [user guide for the CheckEM shiny app is available here](articles/manuals/CheckEM_user_guide.html) 

We have also developed an R-package, containing functions and workflows, to allow R-savvy ecologists to perform quality control checks, format and model their data. Instructions for installing the R-package can be found [below](#to-install-in-r).

The R workflows are available here:

* [Check habitat data exported from TransectMeasure](articles/r-workflows/check-habitat.html)
* [Check fish data exported from EventMeasure](articles/r-workflows/check-fish.html)
* [Generate sptaila layers for modelling](articles/r-workflows/spatial-layers.html)
* [Format & visualise habitat data](articles/r-workflows/format-visualise-fish.html)
* [Format & visualise fish data](articles/r-workflows/format-visualise-fish.html)
* [Generate spatial predictions of habitat using FSSgam](articles/r-workflows/habitat-modelling.html)
* [Generate spatial predictions of fish using FSSgam](articles/r-workflows/fish-modelling.html)

## 4. Data Storage
[GlobalArchive](https://globalarchive.org/) is designed to be a centralised repository of ecological survey data and associated information. The overarching design principles for GlobalArchive have included ease of use, secure user access, flexible data import, and the collection of any sampling and image analysis information. GlobalArchive provides a secure archive of metadata and associated annotation or other data or files, with a focus on stereo techniques. Following the data format required by GlobalArchive ensures that researchers are formating theire data in a consistent way and allows multiple researchers to easily combine their data together. We encourage users of stereo-video to upload and store their stereo-video data in GlobalArchive to enable future colloborations to answer broad-scale ecological questions.

# Our aim
We have curated exisiting standard operating procedures and developed new tools to improve consistency in all aspects of the stereo-video workflow. We encourage the use of these tools and are open to [feedback](#feedback). We hope that our efforts to standardise the stereo-video workflow will streamline data collection, annotation and quality control and will result in data which meet the [FAIR principles](https://ardc.edu.au/resource/fair-data/) and increase colloboration between stereo-video users.

![](man/figures/stereo-video_workflow.png)

# CheckEM R Package
## To install in R
```
install.packages('devtools')
library('devtools')

devtools::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
```

## Shiny-app
CheckEM is available as a web based app hosted on shinyapps.io and can be accessed through [this link](https://marine-ecology.shinyapps.io/CheckEM/)
You can run a local version of CheckEM (without the internet) using R via:

```
CheckEM::runCheckEM()
```
# Feedback
Please submit and feedback on the R package, web based application, workflows or field manuals using the below form.

<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSeMIO3UIrkciATxmRA96xs36XejdO6GV-G6yHGXjxZOrzRBVA/viewform?embedded=true" width="100%" height="1000" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>

# Funding
The Australian Data Partnership: Fish and Shark Data project received investment (https://doi.org/10.47486/DP761) from the Australian Research Data Commons (ARDC) and The University of Western Australia. The ARDC is funded by the National Collaborative Research Infrastructure Strategy (NCRIS).

</div>
