# GlobalArchive 2.0 How to Guide

> Screenshots, diagrams, and table reference images are stored in the `images/` folder.

## Contents

- [About GlobalArchive](#about-globalarchive)
- [Overview - uploading Annotations](#overview---uploading-annotations)
- [Overview - creating a Synthesis](#overview---creating-a-synthesis)
- [Using this guide](#using-this-guide)
- [Create an account](#create-an-account)
- [Navigating the GlobalArchive landing page](#navigating-the-globalarchive-landing-page)
- [Uploading Annotations](#uploading-annotations)
- [●​ A pop-up will open to Create Campaign](#a-pop-up-will-open-to-create-campaign)
- [3.​Create a Campaign](#3create-a-campaign)
- [Campaign Method Metadata](#campaign-method-metadata)
- [●​ If the information for a method metadata field was not recorded or](#if-the-information-for-a-method-metadata-field-was-not-recorded-or)
- [Copying Method Metadata from existing Campaigns](#copying-method-metadata-from-existing-campaigns)
- [4.​Create Annotation Set](#4create-annotation-set)
- [NOTE](#note)
- [Importing Annotations](#importing-annotations)
- [●​ 1. Click the drop down arrow next to the imports to check the status](#1-click-the-drop-down-arrow-next-to-the-imports-to-check-the-status)
- [Check for Issues](#check-for-issues)
- [●​ Use the Data View section to view the rows in the uploaded data that are causing the](#use-the-data-view-section-to-view-the-rows-in-the-uploaded-data-that-are-causing-the)
- [EXAMPLE:](#example)
- [NOTE](#note)
- [Creating a Synthesis](#creating-a-synthesis)
- [●​ A pop-up will open to Create a Synthesis.](#a-pop-up-will-open-to-create-a-synthesis)
- [Importing data into a Synthesis](#importing-data-into-a-synthesis)
- [●​ Wait until tick icons appear in the Status column. The files are now ready for](#wait-until-tick-icons-appear-in-the-status-column-the-files-are-now-ready-for)
- [●​ 3. Confirm deletion.](#3-confirm-deletion)
- [Frequently Asked Questions and Tips](#frequently-asked-questions-and-tips)
- [Appendicies](#appendicies)
- [●​ For example levels of identification and software used to annotate.](#for-example-levels-of-identification-and-software-used-to-annotate)
- [Projects](#projects)
- [●​ 6. Click Process](#6-click-process)
- [Steps 1-8 will generate the following files.](#steps-1-8-will-generate-the-following-files)
- [Appendix 3 - R Workflows and GitHub template](#appendix-3---r-workflows-and-github-template)
- [Appendix 4 - Format required for Import](#appendix-4---format-required-for-import)
- [TZD = time zone designator (Z or](#tzd--time-zone-designator-z-or)
- [Sample Metadata for Synthesis](#sample-metadata-for-synthesis)
- [Count](#count)
- [Length](#length)
- [Habitat](#habitat)
- [Benthos Length File](#benthos-length-file)
- [Benthos Relief File](#benthos-relief-file)
- [Appendix 5 - Common errors](#appendix-5---common-errors)
- [●​ Click the](#click-the)
- [Appendix 6 - Data privacy and sharing agreements](#appendix-6---data-privacy-and-sharing-agreements)
- [Appendix 7 - Privacy Policy](#appendix-7---privacy-policy)

---


## About GlobalArchive

Mono and stereo-video imagery annotation provides useful information for the discovery, description and management of the marine environment (Harvey et al. 2021), not only for fish and shark assemblages, but also for characterising benthic biota (Langlois et al. 2021; Williams et al. 2020). Such imagery can be sourced from a variety of platforms (e.g.
stereo-BRUV, Baited Remote Underwater stereo-Video; stereo-DOV, Diver Operated stereo-Video). The standardisation, archiving and sharing of this annotation data through synthesis can contribute to understanding large spatial and temporal scale patterns in marine biodiversity to inform management.
GlobalArchive is a collaborative archive for stereo-video annotations of fish and benthic assemblages, designed to support data standardisation, discovery, sharing and synthesis of this data. The platform brings together sampling information and image annotation outputs (as Projects > Campaigns > Annotation sets) and summaries of count and length data for fish and benthic assemblages (as Syntheses) ready for reporting. It is designed around core principles of secure user access, standardised data import, and the capturing of both field sampling and image analysis information.
The platform is designed to ensure FAIR data principles are complied with, but to also enable granular data access so that users can choose the level of open-access applied to the sampling, annotation or Synthesis data. CARE data principles are achieved by enabling users to control the access to their data.
For stereo-video image annotation, data can be directly ingested from common software (e.g. SeaGIS EventMeasure) or imported in generic format, after Quality Control checks (see CheckEM). Schema controlled Annotation data is associated with Campaigns of samples that are organised within Projects.
Curated summaries of count and length data for fish and benthic assemblages made from annotation data on GlobalArchive or other platforms, can be published with a DOI and versioned to provide an unchangeable source for environmental reporting.
Data can be accessed via a secure API and R package, enabling efficient and structured querying of all sampling, annotation, within a Campaign, and Synthesis data.



---


## Overview - uploading Annotations

![Figure](images/page-05-image-01.png)


---


## Overview - creating a Synthesis

![Figure](images/page-06-image-01.png)


---


## Using this guide This manual details how to:

- ​ Create an account and navigate on GlobalArchive,
- ​ Upload an Annotation Set, within a Campaign, within a Project,
- ​ Create and upload a Synthesis.

The manual is best used in an electronic form - as it uses links to linked supporting Appendices and external resources.
Useful links
- ​ GlobalArchive.org 2.0
- ​ Example - BRUV Metadata
- ​ Example - BOSS Metadata
- ​ Example - Project metadata
- ​ Example - Synthesis metadata



---


## Create an account 1.​ Navigate to GlobalArchive.org 2.0 2.​ Click LOGIN

GlobalArchive uses ORCID IDs for user authentication.
- ​ If you already have an ORCID:
○​ Enter your account details. Click Sign in to ORCID.
- ​ If you don’t have an ORCID:
○​ Select Register now and create an ORCID.
- ​ If you have forgotten your ORCID ID or
password ○​ Click ‘Forgot your password or ORCID ID?’ and follow the instructions on the page.



![Figure](images/page-08-image-01.png)


![Figure](images/page-08-image-02.png)


---


## Navigating the GlobalArchive landing page

- ​ To view data
○​ Click BROWSE to view Syntheses ■​ Use the side menu to view Projects and Campaigns ○​ Click MAP to discover data spatially.
- ​ Click UPLOAD ANNOTATIONS to upload Annotations (within Campaigns within
Projects)
- ​ Click UPLOAD SYNTHESIS to upload Syntheses.



![Figure](images/page-09-image-01.png)


---


Uploading Annotations For stereo-video image annotation, data can be directly ingested from common software (e.g. SeaGIS EventMeasure) or imported in generic format after Quality Control checks (see CheckEM). Schema controlled Annotation data is associated with Campaigns that are organised within Projects.
1.​First, create a Project and Campaign to hold Annotations
- ​ Before uploading Annotations we must create a Campaign within a Project
○​ 1. From the landing page click UPLOAD ANNOTATIONS ○​ 2. Then ⊕ next to Select an Annotation Set.

- ​ A pop-up will open to create an Annotation Set
○​ 3. Click the ⊕



![Figure](images/page-10-image-01.png)


![Figure](images/page-10-image-02.png)


---


## - ​ A pop-up will open to Create Campaign

○​ 4. Click the ⊕ to Create Project.

2.​Create a Project
- ​ 1. Fill out all the information fields - see Definitions
○​ The Project name should indicate the location and/or objective of the data collection (e.g. Geographe Marine Park) ○​ Project names must be unique ○​ WARNING: The Project name cannot be changed after creation, so ensure it is spelt correctly. Other fields can be edited later.
- ​ 2. Click CREATE.



![Figure](images/page-11-image-01.png)


![Figure](images/page-11-image-02.png)


---


## 3.​Create a Campaign

- ​ 1. Fill out all the information fields - see Definitions
○​ If the Project was just created, the Project will automatically be selected ○​ The Campaign Name will form the middle of the generated CampaignID ○​ e.g. If the Campaign Name is “Abrolhos”, and the earliest stereo-BRUV sample was in May 2021, the CampaignID will be:
2021-05_Abrolhos_stereo-BRUVs
- ​ 2. Click CREATE.

NOTE
- ​ Multiple Campaigns within a Project can have the same Campaign Name, provided
they differ in method and/or the date of the earliest sample
- ​ For example, both of the following CampaignIDs can exist within a Project
○​ 2021-05_Abrolhos_stereo-BRUVs ○​ 2022-12_Abrolhos_stereo-BRUVs

WARNING
- ​ Once the Campaign has been created, the following fields cannot be edited
○​ Project ○​ Campaign name ○​ Method
- ​ Please take care when entering these in, and double check before clicking CREATE
- ​ If you do need to change the Project, Campaign Name or Method, you will need to
delete the Campaign and start again
- ​ All other fields can be edited after the Campaign is created



![Figure](images/page-12-image-01.png)


---


Campaign Method Metadata GlobalArchive collects additional metadata about the sampling method (e.g. type of bait used, duration of deployment, camera types). This information can be useful to standardise methods or as covariates for further analysis. Once a Method is selected the Method Metadata Fields and options will populate.

Below is an example of complete Method Metadata for a stereo-BRUVs Campaign.
- ​ A Campaign cannot be created if fields are left blank
- ​ The predefined fields and values for method metadata can be viewed here. If
you would like to add any further values, please contact the administrator.



![Figure](images/page-13-image-01.png)


---


## - ​ If the information for a method metadata field was not recorded or

unavailable, click the ‘x’ next to that field (see image below)



![Figure](images/page-14-image-01.png)


---


Copying Method Metadata from existing Campaigns 1.​ If you have the same Method Metadata across multiple Campaigns, GlobalArchive allows you to copy Method Metadata from a previous Campaign where you are the Custodian.
2.​ Select Campaign to copy from.
3.​ Click APPLY 4.​ Then CREATE.

NOTE: You can edit the Method Metadata fields later, which is useful when most but not all metadata is the same.

NOTE
- ​ Campaigns won’t be listed on the Campaign screen until annotation data has been
imported into the Annotation Set.
- ​ This means that if you need to delete a Campaign you will need to import data before
you can delete it.



![Figure](images/page-15-image-01.png)


---


## 4.​Create Annotation Set

- ​ Once the Campaign has been created upload an Annotation Set
○​ 1. If the Campaign has just been created the Campaign will be automatically selected
- ​ Annotation Set names must be unique within a Campaign and should be a
description on how you annotated the imagery. Example Annotation Set names could be ‘Langlois 2020’ if the methods were the same as that in the BRUV field manual, or ‘Shark and Rays’ if you only annotated sharks and rays.
- ​ 2. Fill out the fields.
- ​ 3. Click CREATE.

- ​ Annotation Metadata fields can be copied from existing Annotation Sets by
following the same steps as copying method metadata fields.
- ​ The predefined fields and values for Annotation Metadata fields can be
viewed here. If you would like to add any further values, please contact the administrator.



![Figure](images/page-16-image-01.png)


---


![Figure](images/page-17-image-01.png)


---


## NOTE

- ​ If you haven’t just created the Campaign
○​ 1. From the landing page click UPLOAD ANNOTATIONS ○​ 2. Click the ⊕ next to Select an Annotation Set ○​ 3. Use the drop down box or type the Campaign name in ○​ 4. Select the Campaign the Annotation Set will belong in



![Figure](images/page-18-image-01.png)


---


## Importing Annotations

- ​ 1. If the Annotation Set has just been created, it will automatically be selected and is
ready to start importing data
- ​ If the Annotation Set hasn’t just been created
○​ 1. On the landing page click UPLOAD ANNOTATION ○​ 2. In the ‘Select a Annotation Set’ box type in the Annotation set name or select it from the drop down menu To import Annotations ○​ 1. Click ‘Add files to Annotation Set’ ○​ 2. Select the metadata (see example metadata format) and all EMObs for the Annotation Set from your local computer ○​ Alternatively drag and drop the metadata and EMObs into the ‘Add files to Annotation Set’ section



![Figure](images/page-19-image-01.png)


---


## - ​ 1. Click the drop down arrow next to the imports to check the status

- ​ 2. If there is a tick in the Status column, the files are ready

- ​ Next
○​ 3 & 4. Select the taxonomic vocabulary used ○​ NOTE currently the only option is the Australian Aquatic Fauna (CAAB+WORMS+FishBase).
○​ This will add the vocabulary and refresh the screen, showing any errors with the uploads



![Figure](images/page-20-image-01.png)


![Figure](images/page-20-image-02.png)


---


## Check for Issues

- ​ Scroll to the Issues section.
- ​ The Issues section lists any problems detected in the uploaded data. Each row
shows:
○​ the type of issue ○​ a description of the issue ○​ the percentage of rows affected
- ​ The Type column indicates the severity of the issue:
○​ ℹ️ Info: General information about the data. These messages do not prevent the file from being imported but may highlight something useful to review.
○​  ⚠ Warning: A potential problem that should be checked before importing.
The file can usually still be imported, but some rows or values may need attention.
○​ ❗ Error: A problem that must be fixed before the file can be imported. Errors usually indicate missing required fields, invalid values, or formatting issues that prevent the import from continuing.
- ​ A detailed explanation of individual errors/warnings, common causes and trouble
shooting tips can be found in Table X. Coming soon…
- ​ Use the 👁  icon to filter the data view so that only the rows causing the selected
issue are displayed. This is useful when you want to inspect the affected records directly, check what needs to be corrected, or focus on one issue at a time without viewing all the problematic rows at once.
- ​ In the Cause column, hover your cursor over the ❓ symbol to view more details
about the issue, including the affected file, columns or rows, and the percentage of rows affected.



---


## - ​ Use the Data View section to view the rows in the uploaded data that are causing the

flagged issues.
- ​ The Data View section contains three tabs:
○​ Point Data, ○​ 3D Point Data, ○​ Length Data
- ​ Each tab displays the problematic rows from the uploaded files. If there are no
problematic rows for a particular data type, the table will be blank.
- ​ Cells containing an issue are highlighted in orange. Hover over a highlighted cell to
view a description of the issue.
- ​ At the bottom of the Data View table, you can change how many rows are displayed
per page.
- ​ Use the page arrows to move between pages of flagged rows.
- ​ Use the horizontal scroll bar to scroll across the table and view additional columns.
This is useful for reviewing more information about the errors, including details in the EMObs columns.
- ​ After reviewing the flagged rows, you will need to decide whether the issue
represents a genuine problem in the data. If the data needs to be corrected, return to the original annotation files in EventMeasure and fix the issue at the source. Once the source files have been corrected, re-upload the files and check the issues panel again.
- ​ Once you are happy with your uploads click ‘Import Data’.



![Figure](images/page-22-image-01.png)


---


## EXAMPLE:

- ​ The screengrab below shows the Issues Overview section filtered to one issue ‘‘The
3D Point Annotation data contain rows with blank or missing values in the ‘period’ column’.
- ​ The 3D Point Data tab lists the opcodes affected by the selected issue.
- ​ The cells containing the issue are highlighted in orange.
- ​ The affected cells are in the period column, there are no values in the period column
because the annotations are outside of a period definition.
- ​ The user will review all the 3D measurements that are outside of the period, and
check the values in the other columns. By looking at the comment column, we can see that a comment exists for each cell ‘sync point’.
- ​ 3D points without a period are commonly used to set a ‘sync point’ in EventMeasure,
as long as there are no values in Family, Genus, Species or Number.
- ​ Therefore, in this example the user will ignore this warning and continue with the
import after checking that the 3D measurements flagged are all sync points.
- ​ If there was a 3D point outside of the period with information in the Family, Genus
and Species columns.
- ​ The user would open the EMObs on EventMeasure and check it.
○​ If it needed to be changed, the user would fix it and save the EMObs, delete the existing EMObs file on GlobalArchive and upload the fixed one.



![Figure](images/page-23-image-01.png)


---


## NOTE

- ​ GlobalArchive provides a complete archive of all the information held with an
EventMeasure annotation file (.EMObs). However, GlobalArchive is NOT a video repository and therefore your local annotation files remain the “true” copy of the data and any corrections must be made in the annotation file and then re-imported to GlobalArchive.
- ​ Please look after your annotation files.
- ​ If you have used the EventMeasure software to annotate but have made
“corrections” on exported data (e.g. in Excel), this “corrected” data is now the “true” copy of the data and you should import your data as generic annotation files (e.g.
count and length data). However we strongly advise you to make these corrections to the EventMeasure annotation file (.EMObs).



![Figure](images/page-24-image-01.png)


---


## Creating a Synthesis What is a Synthesis?

Curated summaries of count and length data for fish and benthic assemblages, made from annotation data, from GlobalArchive or other platforms, and from multiple times and locations can be brought together in a Synthesis.
Syntheses can be versioned and published with a DOI to provide an unchangeable source for reporting.
Synthesis can be created via the CheckEM app or using example R code.

Creating a Synthesis on GlobalArchive
- ​ To create a Synthesis
○​ 1. Click Import .
○​ 2. Then Import Synthesis .
○​ 3. Then click the ⊕ next to Select a Synthesis ○​ Alternatively, from the landing page click UPLOAD SYNTHESIS and then click the ⊕ next to Select a Synthesis



![Figure](images/page-25-image-01.png)


---


## - ​ A pop-up will open to Create a Synthesis.

- ​ 1. Complete the relevant fields by typing into the text boxes and selecting options
from the drop-down menus.
○​ The Synthesis name should indicate the location and/or objective of the data collection (e.g. Geographe Marine Park).
○​ Synthesis names do not need to be unique. GlobalArchive will automatically assign a version number to a duplicate name.
○​ All information entered when creating a Synthesis can be edited after the Synthesis has been created.
- ​ 2. Click CREATE
○​ Examples of completed Synthesis information are shown below.



![Figure](images/page-26-image-01.png)


---


## Importing data into a Synthesis

- ​ On the Import Synthesis page, select the Synthesis by typing in the name or scrolling
in the dropdown ○​ Click the Synthesis you would like to import data into

- ​ 1. Click Add files to Synthesis
- ​ 2. A pop-up will open. Select the files for the Synthesis (see the Format required for
synthesis import) ○​ Alternatively drag and drop the files into the upload section



![Figure](images/page-27-image-01.png)


![Figure](images/page-27-image-02.png)


---


## - ​ Wait until tick icons appear in the Status column. The files are now ready for

importing.

- ​ GlobalArchive will search for the recommended naming patterns and automatically
assign the file type e.g. 2021-05_Abrolhos_stereo-BRUVs_benthos-count.csv will be assigned to Benthos Count File.
- ​ GlobalArchive accepts other naming conventions, if the files do not have the
recommended suffix the file type will need to be set manually. To do this:
○​ 1. Click the Set file type dropdown ○​ 2. Select the appropriate file type from the dropdown menu
- ​ The recommended naming conventions are listed in the format required for import
section

To delete multiple files
- ​ 1. Select the check boxes.
- ​ 2. At the bottom of the list of files press Delete selected files.



![Figure](images/page-28-image-01.png)


![Figure](images/page-28-image-02.png)


---


## - ​ 3. Confirm deletion.

Choosing a Vocabulary
- ​ Next select the taxonomic vocabulary used by using the drop down menu
- ​ NOTE at present, the only vocabularies are the Australian Aquatic Fauna
(CAAB+WORMS+FishBase) for fish and Australian Benthic Biota and Substrate (CATAMI+) for Benthic organisms and habitat classification.

Check for Issues
- ​ After adding files into a Synthesis, the Issues section will refresh with any errors
- ​ The Issues section lists any problems detected in the uploaded data. Each row
shows:
○​ the type of issue ○​ a description of the issue ○​ the percentage of rows affected
- ​ The Type column indicates the severity of the issue:
○​ 
