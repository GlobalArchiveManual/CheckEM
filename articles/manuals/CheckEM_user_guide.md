# CheckEM User Guide

[ Download PDF
Version](https://docs.google.com/document/d/15Z2rU9MZZj2J4tzwIQaUjgoOBNwsbaONkxJfQU98VVU/export?format=pdf)

[TABLE]

![](https://docs.google.com/docs-images-rt/APuouOdZ2-oUGkhvEgCpCH5AwyWfviESrJGrAXsdhf0oFg7uYojuydbmrmnvjfdPChYgrshkoqB1MqcQSHFis3fkPdYQlKjjW-W3NPYTyYm53kJFJVU4d_NOyYqZYSEZRPMqoQdbSSDq-msv1V8Arefw=s2048)

Table of Contents

[Definitions](#h.gx46nqjgpkix)

[General](#h.q3eohma06bmg)

[Introduction](#h.ifhfiy9cf98q)

[Using this manual](#h.3kxjgqqwdyh2)

[Version Information](#h.3lojiayg85pj)

[Format required for upload](#h.us75ugeoenuk)

[Sample Metadata](#h.1bahh8p4s4vr)

[Defining a sample](#h.wjfyd0jby6r9)

[Column requirements](#h.q7sp799f9dzn)

[Metadata tips](#h.dpsdv788jteq)

[Annotation data](#h.ufkr7l482piq)

[EventMeasure Database Output](#h.afb92yj94ww4)

[Generating database output from EventMeasure](#h.r6iofaokyh8s)

[“Generic” annotation data](#h.vgwl3o4khnjk)

[Uploading data](#h.83240lcrndnu)

[Privacy](#h.bqskt47fjtud)

[How to upload](#h.gnfey2s2rj2q)

[Setting the format of uploads](#h.ao5aur3s85f6)

[Setting the life history information and spatial zoning to
use](#h.w99s33wqk1l7)

[Reading the QC assessments](#h.f8cx6wcni5ji)

[Check Metadata & Periods](#h.na8b7seszym8)

[Sample Metadata Score](#h.o2g3jf8p4ukc)

[Create & Check MaxN (point methods only)](#h.x8i4bk2re2az)

[Check Length & 3D points](#h.lut3onuataog)

[Compare MaxN & length (point methods only)](#h.y448ywhra6kx)

[Downloading Data and QC score](#h.gga92w53uo8u)

[Downloading report of all potential ‘errors’](#h.syon0cvqjqe1)

[Downloading final data](#h.2j7vnmjvy5rc)

[Quality Control Score](#h.nc2uit3opxlc)

[Editing Maximum Lengths of Fish Species](#h.266a6x1vr134)

[Feedback](#h.b07w8ofeqevj)

------------------------------------------------------------------------

Table of Figures

[Figure 1. Example of errors in sample metadata.](#h.7duqbkupnwcg)

[Figure 2. Generating database output from
EventMeasure.](#h.3n86jfrz6zsk)

[Figure 3. Example settings to export database output from
EventMeasure.](#h.9tv7fgjdqsx4)

[Figure 4. Example summary whilst exporting database output from
EventMeasure.](#h.9tgyxjcnhdc0)

[Figure 5. Example database output from EventMeasure.](#h.en6444d0r1m)

[Figure 6. Setting the format of data to be uploaded.](#h.25xg9v1ym5u2)

[Figure 7. Setting the life history information to
use.](#h.gj3os4qt21le)

[Figure 8. Sample metadata preview in CheckEM.](#h.movg72nunsge)

[Figure 9. Example of more detail that is shown when clicking a QC
assessment box.](#h.pey92iuuxn2c)

[Figure 10. Screenshot from the Check Metadata & periods tab showing the
QC assessments.](#h.sa7a6buvjr72)

[Figure 11. Screenshot from the Check Metadata & periods tab of a
map.](#h.3eosffeaqihl)

[Figure 12. Screenshot from the Create & Check MaxN tab showing the QC
assessments.](#h.sgdyw9905wlc)

[Figure 13. Screenshot from the Create & Check MaxN tab showing the
spatial and status plot.](#h.xn4rmmtj7qnu)

[Figure 14. Screenshot from the Check Length & 3D points tab showing the
QC assessments.](#h.fm2gszakp7pk)

[Figure 15. Screenshot from the Check Length & 3D points tab showing an
example length histogram.](#h.kqidizgz9wue)

[Figure 16. Screenshot from the Compare MaxN and length tab showing the
QC assessments.](#h.smx6o8a5j31k)

[Figure 17. Screenshot from the Compare MaxN and length tab showing the
length measurements versus 3D points plot.](#h.qm0x0hh1elfl)

[Figure 18. Screenshot from the Compare MaxN and length tab showing the
length measurements versus 3D points as a proportion
plot.](#h.60uhk3e92apz)

[Figure 19. Adding a project name.](#h.vvgvj7apac6b)

[Figure 20. Setting the limits to download all potential
errors.](#h.4ml08tt41gyp)

[Figure 21. Setting the errors to remove from the final downloaded
data.](#h.a8essdunnb71)

[Figure 22. Example Quality Control Score plots.](#h.10r4cxe43pto)

[Figure 23. The form to edit species maximum lengths.](#h.v640ezri2eb0)

[Figure 24. The form to give feedback on CheckEM.](#h.450i5gf5cbf6)

Table of Tables

[Table 1. Column requirements for the \$\_Metadata.csv
file.](#h.fh6r703cg36v)

[Table 2. An example of the first five rows of a \$\_Metadata.csv
file.](#h.sw171m2ivdbp)

[Table 3. Column requirements for the \$\_Count.csv
file.](#h.1y72e095hl05)

[Table 4. Column requirements for the \$\_Length.csv
file.](#h.81oe516wq3tx)

[Table 5. First 5 rows of a  \$\_Count.csv file for an example
stereo-BRUVs campaign.](#h.yx0ul3lgihen)

[Table 6. First 5 rows of a  \$\_Length.csv file for an example
stereo-BRUVs campaign.](#h.97vezkwq4z5h)

[Table 7. QC assessments on the Metadata tab.](#h.7c7r7klszicl)

[Table 8. QC assessments on the MaxN tab.](#h.5g3g9b6y60uf)

[Table 9. QC assessments on the Length tab.](#h.jdmf1imk5sgs)

[Table 10. QC assessments on the Compare count and length
tab.](#h.y8ygzc6x24yd)

------------------------------------------------------------------------

# 

# Definitions

Campaign

- A discrete set (temporal and spatial) of samples.
- All samples within a campaign use the same sampling and image analysis
  methods.

CampaignID

- A CampaignID is a unique identifier for a Campaign made up of
  YYYY-MM_Campaign_Method. The date is the start date of the campaign
  (\$ is used to denote a CampaignID throughout this guide) e.g.
  2023-12_Rottnest_stereo-BRUVs.
- All files that are to be uploaded in CheckEM need to start with the
  CampaignID (YYYY-MM_Project.name-Method_Metadata.csv). The CampaignID
  is case sensitive and needs to match between files (for example the
  \$\_Count and \$\_Metadata files from the same Campaign need to have
  the exact same CampaignID).

Count data

- A data set containing the number of each species present in a sample.

EMObs file

- An EventMeasure measurement file. Has the file extension .EMObs.

EventMeasure

- A commonly used software for logging and reporting events occurring in
  digital underwater imagery. Available from
  [www.seagis.com.au](https://www.google.com/url?q=https://www.seagis.com.au/&sa=D&source=editors&ust=1784178453847089&usg=AOvVaw0BoA2wPHWoBE-7IvF4ct7a)

Floating point number

- A positive or negative whole number with a decimal point. For example,
  5.5, 0.25, and -103.342. A decimal point can "float" to any position
  necessary.

Generic annotation data

- Historical data that predates EventMeasure or data that has been
  changed or corrected outside of the annotation files.

GlobalArchive

- An online repository of ecological data and science communications. A
  platform for users to store and share ecological datasets and
  synthesis products. Accessed via
  [globalarchive.org](https://www.google.com/url?q=https://globalarchive.org/&sa=D&source=editors&ust=1784178453847933&usg=AOvVaw05iNOdYbZKmH1ReoGXsq16)

Integer

- A whole number. Not a fraction. For example 1, 16 or 72.

Length data

- A data set containing the body size information of each individual
  present in a sample.

MaxN

- The maximum number of a particular family, genus, and species measured
  in any single image.

OpCode

- Short for Operation Code. A code associated with an EventMeasure
  measurement file. The user enters the OpCode in the information fields
  for each EventMeasure measurement file.

Period

- A portion of time set by the user in an EventMeasure file. The user
  sets the period start time, end time and a name.

Sample

- A single observational unit (e.g. a single stereo-BRUV deployment or
  stereo-DOV transect).

Sampling method

- The technique or equipment used to collect the ecological data e.g.
  stereo-BRUVs (baited remote underwater stereo-video) and stereo-DOVs
  (diver operated stereo-video).

Sample metadata

- A spreadsheet of properties of all samples in the Campaign including
  where the sample is in space and time.

Stage

- Development stage. Either AD, F, M or J (Adult, Female, Male or
  Juvenile respectively).

------------------------------------------------------------------------

# 

# General

## Introduction

CheckEM is an open-source web based application which provides quality
control assessments on metadata and image annotations of fish
stereo-imagery. It is available at
[marine-ecology.shinyapps.io/CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178453850504&usg=AOvVaw0TQmmK1fDvrlEVtWJyXsgV).
The application can assess a range of sampling methods and annotation
data formats for common inaccuracies made whilst annotating stereo
imagery. CheckEM creates interactive plots and tables in a graphical
interface, and provides summarised data and a report of potential errors
to download.

CheckEM runs up to 30 quality control assessments on uploaded data, for
example it warns users if their metadata is missing information, flags
species that are outside of their documented distribution or are larger
than their maximum size listed on FishBase. It provides a list of all
the potential errors in the annotation files for analysts to easily
integrate into their quality control workflow.

CheckEM is not fully automated; it only flags suspicious annotations. It
is then up to the user to go back to the raw annotations and review the
imagery and annotations. This ensures that the link between the raw
annotations and the data exported is maintained.

A video summarising this user guide is available on
[vimeo](https://www.google.com/url?q=https://vimeo.com/873182345&sa=D&source=editors&ust=1784178453852091&usg=AOvVaw0OLghs-ehPatuImzaq3-ww) and
walks you through all steps of preparing data, uploading data and
downloading data.

## Using this manual

This manual details the data format requirements, how to set up an
upload, how to read the QC assessments and how to download data from
CheckEM. Because the manual uses links within the document and to
external websites it is best used in electronic form. This manual can be
accessed through the User guide tab on CheckEM or through the PDF
download or online on the R package documentation website
[here](https://www.google.com/url?q=http://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html&sa=D&source=editors&ust=1784178453852858&usg=AOvVaw1TKvf11mwZd7-YiWEUdENA).

## Version Information

See the Changelog tab for a summary of the recent changes between
versions of the application.

# Format required for upload

All files to upload need to be saved in one folder (can be on your
computer, harddrive or a network folder). CheckEM reads files based on
the suffix (e.g. \_Metadata.csv or \_Points.txt), therefore all files
uploaded to CheckEM need to be named consistently (CheckEM is case
sensitive).

CheckEM can check multiple campaigns at a time but can only check one
type of [annotation data format](#h.ufkr7l482piq).

## Sample Metadata

Both [annotation data types](#h.ufkr7l482piq) require a sample metadata
spreadsheet saved as a CSV file. The sample metadata is just as
important as the annotations. It is a record of where and when each
sample was collected, if the sample was successfully annotated for count
data and/or length data, who annotated the sample and if the sample was
collected in an area closed to fishing. The sample metadata is partially
collected in the field (e.g. date_time, latitude_dd and longitude_dd)
and partially filled out during annotating (e.g. observer_count and
successful_count). We suggest that the sample metadata collected in the
field is maintained in a shareable, online spreadsheet (e.g.
GoogleSheets or OneDrive) so multiple annotators can fill out the extra
columns whilst they are annotating, an example google sheet version of a
template sample metadata spreadsheet is available
[here](https://www.google.com/url?q=https://docs.google.com/spreadsheets/d/1Cu11abZmN5NgRA9HSOPZMLhzvaUHAuCnU8iVs6yU_HA/edit?usp%3Dsharing&sa=D&source=editors&ust=1784178453854751&usg=AOvVaw27Yg0fW0EnY9qaXUl_qHF1).

### Defining a sample

There should be one row in the sample metadata file for every sample
collected (e.g. one stereo-BRUV deployment or one stereo-DOV transect).
CheckEM will match the annotation data to the sample metadata using the
columns that are present in the sample metadata. There are three ways to
define a unique sample in the sample metadata:  

1.  opcode only (see example 1)
2.  period only (see example 2)
3.  opcode and period (see example 3)

Example 1. A campaign using stereo-BRUVs.

The annotator creates one EMObs file per stereo-BRUV deployment. They
have filled out the “OpCode” field in the information fields with a
unique identifier, therefore they need to include the opcode column in
their sample metadata. They have used periods to define the sampling
duration (60 minutes) but they have named this column with information
that is not needed to match the samples to the sample metadata (e.g.
“Time seabed” or “seabed”), they do not include the period column in
their sample metadata.

Example 2. A campaign using drop cameras.

The cameras in the drop camera were not turned off throughout a sampling
day. To save time, the annotator has created one EMObs file per field
day rather than creating one EMObs file for each deployment. They have
entered a value into the information field “OpCode” that is not used to
define the sample, therefore they do not need to include opcode in their
sample metadata. They have set a period start and end, and entered a
unique identifier into the period name for each deployment, therefore
they need to include a period column in their sample metadata.

Example 3. A campaign using stereo-DOVs.

The annotator has created one EMObs per site (six transects). They have
filled out the “OpCode” field in the information fields with the site
code. They have set a period start and end, and entered the transect
number in the period name (e.g. T1, T2, T3 …). As both opcode and period
name are needed to create a unique identifier for each sample, they need
to include both opcode and period column in their sample metadata.

It is very important that the opcode and period in the sample metadata
match EventMeasure exactly (case sensitive). Care should be taken when
entering the information fields and period names into EventMeasure, or
use the ‘skeleton’ EventMeasure measurement file generation feature in
EventMeasure (see the EventMeasure manual for instructions). Mismatches
will be flagged in CheckEM as missing sample metadata or missing
annotations.

### Column requirements

The required and optional columns to be included in the sample metadata
are listed in [Table 1.](#h.fh6r703cg36v) These are the same columns
required by
[GlobalArchive](https://www.google.com/url?q=https://globalarchive.org/&sa=D&source=editors&ust=1784178453858450&usg=AOvVaw08Et83dkiFXvFOZV_VeiyW).
If you are missing any required columns CheckEM will add the columns but
they will be blank and you will receive an error message (as shown in
[Figure 1](#h.7duqbkupnwcg)). Any errors in the metadata should be fixed
before proceeding. Additional columns with campaign specific information
can be added to the end of the sample metadata as required. An example
of a stereo-BRUV sample metadata file is given in [Table
2](#h.sw171m2ivdbp) and a google sheet version with examples for a
stereo-BRUV, stereo drop camera and stereo-DOV are available
[here](https://www.google.com/url?q=https://docs.google.com/spreadsheets/u/0/d/1BQbWKHUYnED91CrRS2f5wYNElTUBd4yl-4uWURZzW-A/edit&sa=D&source=editors&ust=1784178453859146&usg=AOvVaw0-ce7GOHrd4eaSixTeqlKd).  
![](https://docs.google.com/docs-images-rt/APuouOd7JqPTf305K-ZkQwZ5y8dlcNBH2vLGsE4WwtjltKPBC36_z_9oTRVLVHl4Gp4sLLXW2JF2N78roaK45IAVdoTj6fR2QePvw9WjBaX73j5z8junxrTBLCzvEXjCkif0YAdyQxMFj3VGIawGn-43Nks=s2048)  
![](https://docs.google.com/docs-images-rt/APuouOd6aZkY45etNny-BIg8tDQOHM3WN5gj4L0VB4h-jFdxJX0EjpGjhfQ2ZScYtuhNTTU0yZtePj4a7LnkYofJ7R_unmTplkZBLfU70qpj9wHVtTunmdV__x81aeim7AmIZuewH01VSjQsvXFyto6c=s2048)

###### Figure 1. Example of errors in sample metadata. 

These should be fixed before continuing.

### Metadata tips

- We recommended using the sample metadata template for new Campaigns to
  reduce data manipulation and re-formatting before uploading to CheckEM
  and
  [GlobalArchive](https://www.google.com/url?q=https://globalarchive.org/&sa=D&source=editors&ust=1784178453859725&usg=AOvVaw1srLA1_A7Pz6_rRFzhzgEC).

- If your longitude and latitude are not in the required format use a
  batch converter tool such as:
  [www.earthpoint.us](https://www.google.com/url?q=http://www.earthpoint.us&sa=D&source=editors&ust=1784178453860014&usg=AOvVaw3ez6KtjXoMpPHaaCitDDsR).
  A free account can be requested if it is for educational purposes.

&nbsp;

- Errors in latitude and longitude will prevent CheckEM plotting the
  sample metadata and annotations spatially or determining the marine
  region of the samples.

- Beware - Excel will parse any recognisable date and time data into the
  computer’s system default format - which may not match the required
  format for the \$\_Metadata.csv.

&nbsp;

- You may think you have the date format correct, but if you open the
  file in Excel it will change the format!

##### Table 1. Column requirements for the \$\_Metadata.csv file. 

Transposed (rows for columns) for formatting convenience.

[TABLE]

##### Table 2. An example of the first five rows of a \$\_Metadata.csv file.

This is an example for a stereo-BRUVs campaign with additional backwards
facing cameras for habitat annotation where the sample is defined using
the opcode column only.

|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| opcode | latitude_dd | longitude_dd | date_time | site | location | status | depth_m | successful_count | successful_length | observer_count | observer_length | successful_habitat_forward | successful_habitat_backward | observer_habitat_forward | observer_habitat_backward |
| 35 | -34.1315 | 114.9236 | 2023-03-15T07:36:19+08:00 | Site 1 | South-west Corner | No-take | 39.6 | Yes | Yes | Hannah Williams | Gidget Mirrabelle | Yes | Yes | Hannah Williams | Hannah Williams |
| 5 | -34.1295 | 114.9292 | 2023-03-15T07:49:41+08:00 | Site 1 | South-west Corner | No-take | 42.7 | Yes | Yes | Hannah Williams | Gidget Mirrabelle | Yes | Yes | Hannah Williams | Hannah Williams |
| 26 | -34.1272 | 114.9284 | 2023-03-15T07:54:35+08:00 | Site 1 | South-west Corner | No-take | 36 | Yes | Yes | Gidget Mirrabelle | Hannah Williams | Yes | Yes | Hannah Williams | Hannah Williams |
| 23 | -34.1283 | 114.9189 | 2023-03-15T08:01:12+08:00 | Site 2 | South-west Corner | Fished | 41 | Yes | Yes | Gidget Mirrabelle | Hannah Williams | Yes | Yes | Hannah Williams | Hannah Williams |
| 29 | -34.1229 | 114.9105 | 2023-03-15T08:07:51+08:00 | Site 2 | South-west Corner | Fished | 42.6 | Yes | Yes | Levi Peters | Gidget Mirrabelle | Yes | Yes | Hannah Williams | Hannah Williams |

## 

## Annotation data

There are two types of annotation formats that you can upload to
CheckEM: EventMeasure database outputs or “Generic” files. An
EventMeasure database upload requires the user to have access to
EventMeasure and the EMObs created during annotation. A “Generic” upload
is a much simpler format and allows users who haven’t used EventMeasure
to QC their annotation data.

We recommend using the EventMeasure upload if the EMObs files are
available and up to date. There are more assessments possible with an
EventMeasure upload than with a “Generic” upload. If you have used
EventMeasure software to annotate your samples but have made corrections
on the exported data (e.g. in Excel), this corrected data is now the
true copy of the data and you should import your data as “Generic”
annotation files.

The opcode and period names in the annotation data must exactly
match the names in the sample metadata if they are [required to define a
sample](#h.wjfyd0jby6r9).

CheckEM can assess multiple campaigns at a time but can only handle one
type of [annotation data format](#h.ufkr7l482piq).

### EventMeasure Database Output

Before exporting an EventMeasure database output you should make sure
that all your EMObs are in one folder, and you know where that folder is
saved on your computer/harddrive/network, note down how many EMObs files
are within this folder. We suggest that you export one database output
per Campaign (e.g. keep a separate folder of EMObs for each Campaign).

You should also create a new folder where you would like to save the
outputs to.

#### Generating database output from EventMeasure

- Open EventMeasure
- Go to Program
- Generate database output ([Figure 2](#h.3n86jfrz6zsk)).

![](https://docs.google.com/docs-images-rt/APuouOfo-h6mdBRbcargZM_3PQ-vsk7W6bZQXAWDf-mjbye6BwNxKq0BYWNRfshFr86mqFkdKUIaFv4n6gRjijjA0roHhC6Ii79IEQgLsfZdlfOZxbVDqnSq1q0Rkcb44XCcXMPyzf7N91sGVVGth_GJZ5QKuVc8f5VrrGQXb55sfQ=s2048)

###### Figure 2. Generating database output from EventMeasure. 

- Add the directory where your EMObs from one campaign are saved in the
  Input file directory ([Figure 3](#h.9tv7fgjdqsx4)).
- Add the directory where you would like to save the database outputs to
  in the Output file directory ([Figure 3](#h.9tv7fgjdqsx4)).
- Add the CampaignID as the Base name (ensure this is spelt exactly the
  same as the CampaignID prefix of the metadata file ([Figure
  3](#h.9tv7fgjdqsx4)).

![](https://docs.google.com/docs-images-rt/APuouOddo_TnuxK2CgXuhA1AdcRc2-1HtBlNZ4Ki7yjIYFuGklUpJ6YPbKD1RClkd4q2KTYzO9RMx8YEhq2AT5nfGLqM79IiIEI1O7HrZnyqlMnSlWrmkrl_Qokr5KwmdgbVez3pjHby6cCfBOkxBY8c40kK0zbX1Cz19SuCcf9Z9w=s2048)

###### Figure 3. Example settings to export database output from EventMeasure. 

- Click Process
- Check the summary table that the correct number of EMObs have been
  used (it should equal the number of EMObs in the input file directory)
  and that there are no errors shown. The summary table should look like
  the following screenshot ([Figure 4](#h.9tgyxjcnhdc0)).

![](https://docs.google.com/docs-images-rt/APuouOerwGydqKY2_q12-aei-MwM43drh5e8L25LsHNpc13Fw11lteyB4SUEbz5MHgDhm67vLQtTSA3Xo-jI3ESzzJQYLN19__WLieWp8KMOUPTug0bbTVv2yJKTs_XIkYcoNwWbOoxg-rb9Vo8xl1gzc_1SVZ7EknJjHZik8dJAdw=s2048)

###### Figure 4. Example summary whilst exporting database output from EventMeasure. 

- Navigate to the Output file directory in your file explorer. You will
  see the eight files exported from EventMeasure for the first campaign
  ([Figure 5](#h.en6444d0r1m)). Repeat the above process for each
  campaign you would like to assess in CheckEM.

![](https://docs.google.com/docs-images-rt/APuouOff8LeEr2uFWcKDRbYykrXd6R25EDHa_BzADPYpxyOoEV1-wJAdellwtFwZ7-Ybd4UqfDx5fUfyaPHWh2lTCg0Pb2sU8WIrMdbA9ZqQ9L9fnlAfZWw4aPoeaSu_5nzDVquePiz6ZaCsW4SQg5wgt50_ZtbnABxHxe_GOH0=s2048)

###### Figure 5. Example database output from EventMeasure. 

The files needed for an EventMeasure upload to CheckEM are:

- \$\_3DPoints.txt
- \$\_Lengths.txt
- \$\_Period.txt
- \$\_Points.txt

### “Generic” annotation data

If you do not use EventMeasure or have historical data that predates
EventMeasure or you have made changes to data or corrected errors
outside of the annotation files and this changed data is now the true
copy of the data (e.g. in excel) you will have to import “Generic”
files.

The files needed for a “Generic” upload are:

- \$\_Count.csv
- \$\_Length.csv

The column requirements for the \$\_Count.csv and the \$\_Length.csv
file are in [Tables 3](#h.1y72e095hl05) and
[4](#h.81oe516wq3tx) respectively. Examples of the \$\_Count.csv and the
\$\_Length.csv file are given in [Tables 5](#h.yx0ul3lgihen) and
[6](#h.97vezkwq4z5h) respectively.

##### Table 3. Column requirements for the \$\_Count.csv file.

Transposed (rows for columns) for formatting convenience.

[TABLE]

##### Table 4. Column requirements for the \$\_Length.csv file.

Transposed (rows for columns) for formatting convenience.

[TABLE]

##### Table 5. First 5 rows of a  \$\_Count.csv file for an example stereo-BRUVs campaign.

|        |              |            |             |       |       |          |
|--------|--------------|------------|-------------|-------|-------|----------|
| opcode | family       | genus      | species     | count | stage | code     |
| NCB606 | Nemipteridae | Pentapodus | porosus     | 2     | AD    | 37347007 |
| NCB606 | Labridae     | Coris      | caudimacula | 3     | AD    | 37384092 |
| NCB606 | Nemipteridae | Pentapodus | porosus     | 2     | AD    | 37347007 |
| NCB607 | Nemipteridae | Pentapodus | porosus     | 3     | AD    | 37347007 |
| NCB607 | Labridae     | Coris      | caudimacula | 10    | AD    | 37384092 |

##### Table 6. First 5 rows of a  \$\_Length.csv file for an example stereo-BRUVs campaign.

|        |              |            |             |       |           |       |          |
|--------|--------------|------------|-------------|-------|-----------|-------|----------|
| opcode | family       | genus      | species     | count | length_mm | stage | code     |
| NCB606 | Nemipteridae | Pentapodus | porosus     | 1     | 150       | AD    | 37347007 |
| NCB606 | Nemipteridae | Pentapodus | porosus     | 1     | 140       | AD    | 37347007 |
| NCB606 | Labridae     | Coris      | caudimacula | 1     | 190       | AD    | 37384092 |
| NCB606 | Labridae     | Coris      | caudimacula | 1     | 180       | AD    | 37384092 |
| NCB606 | Labridae     | Coris      | caudimacula | 1     | 165       | AD    | 37384092 |

# Uploading data

## Privacy

CheckEM does not save any data that is uploaded. Once the application is
closed or refreshed all uploaded data will be erased from the
applications memory. All uploaded data remains the property of the user.

## How to upload

- Navigate to
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178453911081&usg=AOvVaw19SlXpSzl1SgmVwSGFSJuk).
  We recommend using Google Chrome.

### Setting the format of uploads

- On the Upload data tab, fill out the Format of data box ([Figure
  6](#h.25xg9v1ym5u2)):

&nbsp;

- Type of upload: If you are uploading unedited data from EventMeasure
  choose EventMeasure, otherwise choose Generic (Generic is not
  available for transect campaigns at the time of writing). For more
  information on types of upload see [Annotation data](#h.ufkr7l482piq).
- Type of method: If you are uploading data from a stationary sampling
  platform choose a single point (this will calculate MaxNs) sampling
  method. If you are uploading data from a transect based sampling
  method (e.g. stereo-DOVs or ROVs) choose transect (will not calculate
  MaxNs).

&nbsp;

- If you choose a single point method, you will then be asked if you
  used MaxN by stage. Choose Yes or No.

&nbsp;

- Did you measure fish? If you are uploading length data choose Yes, if
  you did not measure fish or are not uploading length data choose No.
- How did you record the sample name: The options for this question will
  depend on the type of method chosen (single point: either opcode OR
  period, transect: either period OR opcode and period). Choose the
  column names used in EventMeasure or the “Generic” files to identify a
  sample. See [defining a sample](#h.wjfyd0jby6r9) for more information.
- Did you use periods? You will only be asked this question if you are
  uploading single point data that uses opcodes to define the sample. If
  you used periods to standardise the sampling duration (e.g. To ensure
  each stereo-BRUV deployment was 60 minutes long) then choose Yes
  otherwise choose No.

&nbsp;

- Once you have answered the above questions click on the ‘Select
  directory…’ box.
- Navigate to the folder/directory where you have saved your sample
  metadata and annotation files. CheckEM requires the sample metadata
  and chosen annotation files (EventMeasure or “Generic”) to be located
  in one folder.

![](https://docs.google.com/docs-images-rt/APuouOcHSYlauKQ3aQzyIut4P99o2nQ-wBrOOqzR_qwQlUnIfgFrL9ImjlMToT-msnUTMmEO_HicOA4Bhzub-hVdTKQ4P08kU00txTE-MDBK86Bn2GmA31AB5wtQupq0vtrzKi_dJKBwXlrWFzKaL9oD2bk=s2048)

###### Figure 6. Setting the format of data to be uploaded. 

### Setting the life history information and spatial zoning to use

- On the Upload data tab, fill out the Life history information… box
  ([Figure 7](#h.gj3os4qt21le)):

&nbsp;

- Keep or generate ‘status’ column: If you have filled out the status
  column correctly you can keep the status column, or you can choose to
  generate the column based on the shapefiles hosted in CheckEM
  (caution: there are some known issues in CAPAD and the WDPA, we
  suggest using the uploaded status column).
- Life history and regions: Choose the Australian life history sheet
  based on the Codes for Australian Aquatic Biota (CAAB) or the Global
  list based on fishbase and the World Register of Marine Species
  (WoRMS). The Australian list uses the CAAB distributions but at
  [Australia’s marine
  bioregion](https://www.google.com/url?q=https://www.imosoceanreport.org.au/australias-oceans/&sa=D&source=editors&ust=1784178453916446&usg=AOvVaw23cRRPeWBZPx6z-pGhJzFS) scale,
  the global list uses the fishbase distributions at the [Food and
  Agriculture Organisation of the United Nations Major Fishing
  Areas](https://www.google.com/url?q=https://www.fao.org/fishery/en/area/search&sa=D&source=editors&ust=1784178453916693&usg=AOvVaw1RdZwsAqzOwt6XBNbB0V4_) scale.
- Marine region for your data: Once you have chosen if you want to use
  the Australian or Global marine regions, you need to choose if you
  require one marine region per campaign or a marine region per sample.
  If your data crosses multiple marine regions (e.g. entire West Coast
  of Australia or Multiple Countries) you should choose a marine region
  per sample.

![](https://docs.google.com/docs-images-rt/APuouOc5pkAoWPn91PJ-odsCLe8Kem-SXi7pQkwEzY_q_DOyNgLijJMbESwNvbx72U7OYdami00_z2-rYYNkMnQiiIwg6s2rkEgwVfmVkAFAgKgsqeBsQpOE9_OpyFB_EeRSJVSZJtIcpLpzF6DDTxA65Rg=s2048)

###### Figure 7. Setting the life history information to use. 

- You can check that the upload and marine regions have worked by
  inspecting the preview of the sample metadata. A new
  marine_region column will be in your metadata. This column will be
  used to identify species that have not been observed in that marine
  region before ([Figure 8](#h.movg72nunsge)).

![](https://docs.google.com/docs-images-rt/APuouOc1J5EP_mxYWh8DdueLH3m9XAShuUrsbMIwCG0FmwV2c5orsw55R_CSVWZtmpDjFyVSBTBeQtziJ5FR-3vVvw3RpLXNKc3zPxf5iz7CCbTf60bncC4YIMIj62gTmq3EcfsFJgc7d5s_gve19fYWznyp6BeoAVjTU8u9JzKzQw=s2048)

###### Figure 8. Sample metadata preview in CheckEM. 

- If you are missing any required columns in the sample metadata CheckEM
  will add the columns but they will be blank and you will receive an
  error message (as shown in [Figure 1](#h.7duqbkupnwcg)). Any errors in
  the metadata should be fixed before proceeding.
- If you haven’t received any error messages you are now ready to start
  checking your data.

# Reading the QC assessments

Each QC assessment in CheckEM is displayed in a coloured box. CheckEM
uses a ‘traffic light’ system to colour the boxes which allows users to
easily identify issues that need their attention.

The colours correspond to:

- Blue: General information (e.g. the number of samples or fish
  observed).
- Green: Passed the QC assessment (e.g. zero samples are missing
  metadata).
- Orange: Should be reviewed, possibly not a problem (e.g. a sample in
  the sample metadata is missing fish).
- Red: Did not pass the assessment and needs to be reviewed (e.g. fish
  outside of the predefined range).

We suggest clicking through the tabs on the left hand side of the
application and checking each of the QC assessment boxes:

- Check Metadata & periods → 
- Create & check MaxN (only displayed if uploading [single
  point](#h.ao5aur3s85f6) data) → 
- Check length & 3D points → 
- Compare MaxN & length (only displayed if uploading [single
  point](#h.ao5aur3s85f6) data) → 
- Create & check mass

Most QC boxes can be clicked, which will display a box detailing more
information, see [Figure 9](#h.pey92iuuxn2c) for an example. The further
information can be downloaded by clicking the Download as csv button (or
[download all errors](#h.syon0cvqjqe1) at once on the Download data and
QC score tab).

![](https://docs.google.com/docs-images-rt/APuouOd6pSmpyrbQlrZdPUNvnB6-7RxKPp_04dJgiyDX2RAEibeg53QBJJB-i70jf5fahu5PhRFDQf7jHbrnyXkuCcoZ5l-Uq-HDwUyCMK_qZHZU_lNZAVDP5KNqmAA-OdKLlYBTqL1n9ZPdtAsUP_yc1ww82rvo5HcL-MFacWQ=s2048)

###### Figure 9. Example of more detail that is shown when clicking a QC assessment box.

In this example the “Species names updated” box was clicked.

In the following sections we describe each of the QC assessments on each
tab. Not every QC assessment is appropriate for each sampling method
(point or transect) or for each type of upload (EventMeasure or
“Generic), [Table 7](#h.7c7r7klszicl), [Table 8](#h.5g3g9b6y60uf),
[Table 9](#h.jdmf1imk5sgs) and [Table 10](#h.y8ygzc6x24yd) indicate if
the QC assessment is performed for each sampling method and type of
upload.

## Check Metadata & Periods

On the Check Metadata & periods tab up to 14 QC assessment boxes will be
displayed (the number is dependent on the selections made on the upload
tab, [Figure 10](#h.5615kn2qsxx)). Each assessment is described in
[Table 7](#h.7c7r7klszicl). A map of the sample metadata is displayed
([Figure 11](#h.j9mrkyjm7las)) to allow users to conduct a visual check
of the distribution of samples (make sure none are on land or in a
different hemisphere). Samples that have been identified as being on
land will be displayed in orange. Clicking on the marker icon will
display a pop-up of the metadata for that sample). For Australian
datasets the spatial zoning will also be displayed, clicking on the
polygons will display the type and IUCN category of the zone (e.g.
National Park (IUCN II), Multiple Use (IUCN VI) etc.).

![](https://docs.google.com/docs-images-rt/APuouOdZ2-oUGkhvEgCpCH5AwyWfviESrJGrAXsdhf0oFg7uYojuydbmrmnvjfdPChYgrshkoqB1MqcQSHFis3fkPdYQlKjjW-W3NPYTyYm53kJFJVU4d_NOyYqZYSEZRPMqoQdbSSDq-msv1V8Arefw=s2048)

###### Figure 10. Screenshot from the Check Metadata & periods tab showing the QC assessments.

In this example a stereo-BRUvs campaign that used periods to define the
sampling duration was uploaded using EventMeasure database outputs.

![](https://docs.google.com/docs-images-rt/APuouOcemJqk3gRG6CwYOE2A9qq5VI-C67l5_w_LCxCK25IFuc3LiF1g8UZzrIo8Hosy_I-vRjYBu2zucyNi93zMVYyRHQZH0HWrmqYtDC_BUqohO3n1XOlm56CLg9D8FQLDsOgtFHUMPfUNeIj8GcOixYXAaRStYUmYGx8-yb_mqA=s2048)

###### Figure 11. Screenshot from the Check Metadata & periods tab of a map.

### Sample Metadata Score

The Sample metadata score indicates if your sample metadata meets the
criteria outlined in the formatting requirements. For example if you
have a dataset with 50 samples, and you are missing the depth
information for 10 samples (the cells are blank) you will receive a
score of 80% (as only 80% of your sample metadata includes all the
necessary metadata).

![](https://docs.google.com/docs-images-rt/APuouOcNOxpGXmbEVbAUaa20VN2-tAKn42_nEmh_7NeazPZ5hdz4kUdaL9U6bQvFXZSM0hsWPFzwRGN99iCSvB7QBEb1XSkcEu5y9TXicaHZK0Z79E4kgeKznr2mePo3MDz2UgdH_k-nBmY47zDD9TOXgcgNGofSulOZfX7IKJlJig=s2048)

##### Table 7. QC assessments on the Metadata tab.

[TABLE]

## Create & Check MaxN (point methods only)

On the Create & Check MaxN tab up to five QC assessment boxes will be
displayed (the number is dependent on the selections made on the upload
tab, [Figure 12](#h.sgdyw9905wlc)). Each assessment is described in
[Table 8](#h.5g3g9b6y60uf).

A plot of the 15 most abundant species is created ([Figure
12](#h.sgdyw9905wlc)), the number can be changed using the Species to
plot number input. CheckEM also creates a spatial ‘bubble-plot’ for each
species, with a circle for each sample, the bigger the sample the higher
the abundance at that sample ([Figure 13](#h.xn4rmmtj7qnu)). Hovering
over the circle will display the MaxN for that sample. To update the
species plotted choose a new one in the dropdown (the dropdown is
ordered by total abundance with the most abundant species appearing
first in the dropdown) above the map.

The species dropdown will also update the four bar plots showing the
average abundance per sample for each status, location and site (taken
from the sample metadata) and zone (from the spatial zoning chosen on
the upload tab).

![](https://docs.google.com/docs-images-rt/APuouOdHz3Rbgvy5Ce7vUJUXfdGf6kaNEdMSLhSnPbVUq_IAo4UHgYJooPJFaZH0B6-Q28S4ap33rPEO1EyOS7_Xcwxp6h_Va1vi5dkw_GTBy6HedNF-9X2Htv8DhoBC6ZWRoy50IS6gY4pDcRu3Ym1ZIkc3hFhNQ7jqN4lhOWkqww=s2048)

###### Figure 12. Screenshot from the Create & Check MaxN tab showing the QC assessments.

In this example a stereo-BRUvs campaign that used periods to define the
sampling duration was uploaded using EventMeasure database outputs.

![](https://docs.google.com/docs-images-rt/APuouOelKF0xYxjXumvHoeFbRWCWxCwPncHH8HexkS-Fmh9Y7hECGSBD1DTsi27nOeKmcNKxSgcI3YuoVQhQW2yPF9sLirDSU0jy-PSj0KW2953sCmLDm2d0wzzF0YApuRC8HZdJh_VQUnPX65ii8kR5OoMoYFgLXWoL7GeXIiCOqA=s2048)

###### Figure 13. Screenshot from the Create & Check MaxN tab showing the spatial and status plot.

##### Table 8. QC assessments on the MaxN tab.

[TABLE]

## Check Length & 3D points

On the Check Length & 3D points tab up to 14 QC assessment boxes will be
displayed (the number is dependent on the selections made on the upload
tab, [Figure 13](#h.xn4rmmtj7qnu)). Each assessment is described in
[Table 9](#h.jdmf1imk5sgs).

CheckEM creates a length histogram for each species, with a vertical
line for the maximum length listed on FishBase (if available) and 15%
and 85% of this value for analysts to visualise the length distribution
of each species and identify any abnormalities in the length data
([Figure 15](#h.kqidizgz9wue)). To update the species plotted choose a
new one in the dropdown (the dropdown is ordered by total abundance,
with the most abundant species appearing first in the dropdown) above
the histogram. Another histogram faceted by status is created to see if
this pattern is consistent inside and outside of no-take areas.

The species dropdown will also update the two box plots showing the
length for each status (taken from the sample metadata) and zone (from
the spatial zoning chosen on the upload tab).

![](https://docs.google.com/docs-images-rt/APuouOc7GPDAQzYcK5hKqNaWCdngsGANQelbV_OekxDCQOfefdaaApf2XyrS1kNv_JxZ9S5aucOXJLDu79fBUo8S-sYe1b_hAYsAwDbjuPFV9MeSqnJ5BBe8-k7L_WbSCHdQFJseuJtaZ0abUjqjHizPeps=s2048)

###### Figure 14. Screenshot from the Check Length & 3D points tab showing the QC assessments.

In this example a stereo-BRUvs campaign was uploaded using EventMeasure
database outputs.

![](https://docs.google.com/docs-images-rt/APuouOfhs1XrxR-jkt6FqYtpAyQdNI3pewBOVf6edlSPQajHv_8er_Q1-Oh_3X2aIEn86Iiug9w2QripZD675nq3xw6Ekw2oia_eNOQKRoKsB9S2eMF8U14YOzhFgnn7pwXSIvUFQuglx8rrLVrkfSqPxnNGUhWYNi5lr-i080QzDQ=s2048)

###### Figure 15. Screenshot from the Check Length & 3D points tab showing an example length histogram.

##### Table 9. QC assessments on the Length tab.

[TABLE]

## Compare MaxN & length (point methods only)

We recommend the [length measurement procedures outlined in the field
and video annotation guide for
stereo-BRUVs](https://www.google.com/url?q=https://benthic-bruvs-field-manual.github.io/image-annotations%23body-size-measurements&sa=D&source=editors&ust=1784178453962431&usg=AOvVaw0ldq22oQjCY6oym15rCB1w) for
all stereo-video methods where MaxN is used. The guide outlines that if
fish cannot be measured, a 3D point measurement may be used for
annotation, which records the 3D location of the fish to ensure it is
within the sampling area. To create a relative abundance metric
standardised to a consistent sample area, abundance should be summed
from the lengths and 3D points at the MaxN for each species. Therefore
it is important to check if the MaxN for each species in each sample is
equal to the sum of the length measurements and the 3D points.

On the Compare MaxN & length tab up to three QC assessment boxes will be
displayed (the number is dependent on the selections made on the upload
tab, [Figure 16](#h.smx6o8a5j31k)). Each assessment is described in
[Table 10](#h.y8ygzc6x24yd).

The first plot compares the MaxN to the number of measurements (length
measurements + 3D point measurements) by plotting the MaxN of each
species from each sample against the number of length measurements
([Figure 16](#h.smx6o8a5j31k)). Each dot represents a species in a
sample, if the MaxN = the number of length measurements the dot will be
on the red line. If a dot is below the red line the MaxN is greater than
the number of length measurements and if the dot is above the line there
are more length measurements than the MaxN for that species in that
sample.

Arguably the most important data from stereo-video data is the length
measurements, it allows you to estimate age, biomass or fishing
pressure. The second plot visualises the ratio of length measurements to
3D point measurements for each sample ([Figure 17](#h.qm0x0hh1elfl)).
The green segment of the bar represents the number of length
measurements, the blue represents the number of 3D points, the pink
represents individuals in the MaxN that were not measured. A \* denotes
a sample where the sum of the length measurements and 3D points is
greater than the MaxN. If you are following the [length measurement
procedures outlined in the field and video annotation guide for
stereo-BRUVs](https://www.google.com/url?q=https://benthic-bruvs-field-manual.github.io/image-annotations%23body-size-measurements&sa=D&source=editors&ust=1784178453964817&usg=AOvVaw2JPxoEGpykDrlcNTudLEP9) these
any MaxNs that haven’t been measured should be, and any excess
measurements should be removed.  A similar plot displaying the same data
but as a proportion is displayed to easily compare if there are any
differences in the ratio of length measurements to 3D points across the
samples ([Figure 18](#h.60uhk3e92apz)). An option to facet the plots by
observer_length is available to investigate if the proportion of length
measurements to 3D points differs between length analysts.

The above three plots are available for each species in the campaign.
Use the dropdown to select a species.

![](https://docs.google.com/docs-images-rt/APuouOcMe3j_G3QmFiBlsKvfppW7b9_leL4ECUrRkJCsXG_tut4DGCZ9xV0p6J_A8_oqOwg-N3T7Xc2peup7JTe2rSDAx70ug_aE_QYDovZhlzRS6Nic0xuZshj1iD-koqm4Kb1_3tLfZ6y5r197WWs--CRDsBTfdYjlnwGDAkIOMg=s2048)

###### Figure 16. Screenshot from the Compare MaxN and length tab showing the QC assessments.

In this example a stereo-BRUvs campaign was uploaded using EventMeasure
database outputs.

![](https://docs.google.com/docs-images-rt/APuouOd63DgMYGcAF7DMMzRiNHd_9TAgCSkUNuF33NubFjiS8pMGF1FYZDN8x5yS4jYuizt7sv1obtndPSdlknzr4MoNE7YANZslAaxPq4TAP-HIeFXz-aKYDv-0bEhbIw0NZSyYKBjVw7wwcTdJuwsGGD-rgmqGpoIOuBNrQCo=s2048)

###### Figure 17. Screenshot from the Compare MaxN and length tab showing the length measurements versus 3D points plot.

In this example a stereo-BRUvs campaign was uploaded using EventMeasure
database outputs.

![](https://docs.google.com/docs-images-rt/APuouOdGT6cxkE-m22q0QI0DT_7-ts8whtSOBfeqYQMO3hDqd0Sb5-XDgxlIyiWKF2XtW4GYZ_10WimbH4eKDQw8-U7etju8yS1eIwm5_P7JixTqDC-Y3VNpTv4x1pwQbBvV81bBI9zvxy8WCcCp1SnXHFeEsMowUGVuIOLv5Q_kaQ=s2048)

###### Figure 18. Screenshot from the Compare MaxN and length tab showing the length measurements versus 3D points as a proportion plot.

##### Table 10. QC assessments on the Compare count and length tab.

[TABLE]

# Downloading Data and QC score

Once you are ready to download a report of all the ‘errors’ (or the
final ‘cleaned’ data), add a project name ([Figure
19](#h.vvgvj7apac6b)).

![](https://docs.google.com/docs-images-rt/APuouOcW-xPbfmGAV0aLVHLBBxw-ySU9NRSIFyR2qjpRzTGR8cBb-z_442edzfwV_-BOwcyYMylIODwA3SwlDZhQdhzawdwLfuKiaK5g0WYoEMyNHjqdCJkon_QKsDBHiVTZaQDNE_BTrItdVp1Y8846Phf8vTPclgEzu-fAoIY=s2048)

###### Figure 19. Adding a project name.

## Downloading report of all potential ‘errors’

A report of all potential errors is available to download. Before
downloading the ‘error’ report, you need to set some limits, the limits
will depend on what type of sampling method you have used and if you
have used periods to define the sampling duration ([Figure
20](#h.4ml08tt41gyp)). For example, for a single point campaign, using
EventMeasure exports, that has used periods to define the sampling
duration you will need to set the correct period time, the acceptable
range limit, the RMS limit and the precision:length ratio limit. Once
you have entered the limits, click the download all errors in the EMObs
button to download a CSV file of all the potential errors. The list is
ordered by opcode and period so analysts can fix the issues in one EMObs
at a time. Once you have edited the EMObs or generic data to fix up the
errors, we recommend running the data through CheckEM once more to
ensure you have removed all the errors.

![](https://docs.google.com/docs-images-rt/APuouOfUrvg3mFHT18ch9a5CvKXyWcy-y0TwXPlpg5C8arqU7A1TQJzb9y1jUGI60UH6sLq-2fQMn8v7K4rpggE0V4H28xuvugfZ7qZ84O_KfzGhW8xNqd4IRRnSz4K0-lvpTtY1LI9-2s5J_u2BDpv_Rd4FP0CiMVt9SSMczjo=s2048)

###### Figure 20. Setting the limits to download all potential errors.

## Downloading final data

Once you are happy that you have tidied the data and double checked all
the errors have been fixed as much as you can outside of CheckEM, you
can download the formatted data. Although we recommend fixing the data
outside of CheckEM, you can use CheckEM to remove some “errors”, for
example species that haven’t been observed in the area before or filter
out length measurements that are too small or too big (based on the
FishBase maximum sizes). Filtering out these errors is turned off by
default, to filter them out you need to check the checkbox next to the
error you would like to remove ([Figure 21](#h.a8essdunnb71)).

The default download in CheckEM adds zeros for every species for every
sample that it was not present in. If the default is selected, the files
downloaded will have the sample metadata included in them e.g. the
count.csv file downloaded will have the count columns as well as the
sample metadata columns. If adding the zeros is not selected, the sample
metadata file will be downloaded as a separate csv file.

Clicking the Download all files button will download a zip file
containing files for each campaign that was uploaded.

![](https://docs.google.com/docs-images-rt/APuouOc_mzHwC1qhwWO5CKXO3uuCl6FU7lyb1HAHF3asIPdSa_X6X-VTHZdvM3ZtsHTkTXTUCweSOg_VfSF2J8N4xZTRnXat-2JImLXB6d4ZkjNO1A9iwT8h-3AFzO8vZ5MPVnVJm6L9bzY9NImlA7ADsDG2Y5gP7Sipha3lw8QUmw=s2048)

###### Figure 21. Setting the errors to remove from the final downloaded data.

## Quality Control Score

The quality control score infographic is a quick and easy way to
visualise your data against five key QC assessments. This is only
available for single point uploads, with transect based uploads coming
soon. We have chosen the five assessments that we think are the most
important. The five scores are defined below:

- The Sample metadata score indicates if your sample metadata meets the
  criteria outlined in the formatting requirements. For example if you
  have a dataset with 50 samples, and you are missing the depth
  information for 10 samples (the cells are blank) you will receive a
  score of 80% (as only 80% of your sample metadata includes all the
  necessary metadata).

![](https://docs.google.com/docs-images-rt/APuouOdbYtDYhiQDkEICqihjBXC9crbodFucSjiHoQ0EBdQWJQimu__3ImC-gp0GpolPpAOUFpoZJrwgjD2LasnCXXGGCoyR1AKrCZQgWbWNmkSrfv3fOP0dTlKdf7T_qSRfvLtv56d-v5VHurkRl1t-pstsD0XHDlzfO9-x-FE=s2048)

- The Count score indicates if all the samples in your count data have a
  match in the sample metadata. For example if you have a count dataset
  with 10 samples, and three samples do not have a match in the metadata
  because (they are misspelt) you will receive a score of 70% (as only
  70% of your count data matches the sample metadata). Matching the
  sample metadata is important because you need to be able to match both
  datasets together to analyse patterns in depth, no-take status or
  locations.

![](https://docs.google.com/docs-images-rt/APuouOduni3OO2AkelTd2sqBKADBSj1b_eGUMFuuuDyHBn5hZj7ds2UFVXz_fda7nAkSOJ0B8grnLW3pSYTtn7gGm4Tfs0TjO0NOSdCNkVx8cmL-MJxxK3fKhkt3L6yXqasxBGJRsrP7zGNbIKdD2ZsCa-r0NF9aJIYKxVOJQusq5Q=s2048)

- The Count vs. length score indicates if the MaxNs are equal to the
  number of length measurements + number of 3D point measurements. A
  score of 100% indicates that every individual at the time of MaxN has
  been length measured or 3D pointed once. A score of 0% means that
  there are either too many or not enough length measurements compared
  to the MaxN.

![](https://docs.google.com/docs-images-rt/APuouOfofIHfvTC0oyE6Gn3U_K-QWG7LAIwR7FKapw10WgwjF3JYSgZDyviCLlUZZ66tCJAmfwGJ0mkBqyXA8VqvY9TpYGkj3Jueq_HjfK3SQGNJJ2N6IQOgbNNrQOoYnK66lseJVWOqW5G6wtqy7JdMZ7PbsWZBM7276q2zZ40apg=s2048)

- The % Length score indicates the proportion of measurements that have
  a value in length. If you have not used 3D points to ensure that every
  individual at the time of MaxN has been accounted for then ignore this
  check. If you have, we suggest aiming for a score above 70%, a score
  below this could indicate low visibility, habitat obstruction or other
  issues preventing length measurements and should be investigated. If
  you upload “generic” data you will not have this segment present in
  the QC plot.

![](https://docs.google.com/docs-images-rt/APuouOdT2WsmCc-YHDtGXacAtTFRX0DE0N_YGRWOB6YT5IWkmwn1YV3MfwnDc7iIhm6uS9MwSybnWm5VxcRFsPs2Kf7R7PIZP3hoBXPRmKv7gys7IrNMXZLCu_ywraXeIjpQ4W9rxQonEvsENTcDJDSgyOzactsF056wwD_UsdpVeg=s2048)

- The Length score if all the samples in your length data have a match
  in the sample metadata. For example if you have a length dataset with
  10 samples, and three samples do not have a match in the metadata
  because (they are misspelt) you will receive a score of 70% (as only
  70% of your length data matches the sample metadata). Matching the
  sample metadata is important because you need to be able to match both
  datasets together to analyse patterns in depth, no-take status or
  locations.

![](https://docs.google.com/docs-images-rt/APuouOdyI9QMdkZ6JgdtMDOFOgeaJzZbegEfu75P3Bq3P4rUcCIxRXzrK-W8NMbsc58c3_5y697Kg8Ygly608JXcTjOvEIJZE6OykYG4Tk3Beqa82VL0UzC8NkHqWAVwLnGjXFJ7bOGLhsAF6WN3CH6srbm3Zdl8GHr-z2_B1uNpfQ=s2048)

Three example quality control score plots are shown below ([Figure
22](#h.10r4cxe43pto)). In example A and C the user has uploaded sample
metadata where every row is formatted correctly and is not missing data.
In example B 96.7% of the samples in the sample metadata have all the
required information. The solid green line represents the score users
should aim for, in example A and C this goal is met, in example B the
goal is not met.

In example A and B, every sample in the count data has a match in the
sample metadata. In example B, only 99.14% of samples in the count data
have a match in the sample metadata. The solid yellow line represents
the score users should aim for.

In all examples, the user has uploaded data where a MaxN does not equal
the number of measurements, therefore they have not received a score of
100% for Count vs. Length and the segment is below the solid orange
line.

In example A, 71.53% of the measurements have length, this is above the
goal of 70%. In example B, only 59.57 of measurements had length, this
is below the target of 70%. The dark blue segment is not included in
example C because it is a “generic” upload (does not have 3D points).

In example A and C, every sample in the length data has a match in the
sample metadata (light blue segment). In example B, 99.66% of samples in
the length data have a match in the sample metadata. The solid light
blue line represents the score users should aim for.

![](https://docs.google.com/docs-images-rt/APuouOez6_047uRi9G-TPMAjEkicOoSXyK8XuKJIst23uXOV5C094PEqIv5O8VJEH93ymaIKymdqngd-OnTVXkPEffO7PuoYAIp28j-_P5hw4NdF3K9KtpH_zCO9ZnR8MB9EQtZEKTq24lemAdWQhNefd7k2IYVj6AKi2_yc2rwZNg=s2048)

###### Figure 22. Example Quality Control Score plots.

The five quality control assessments are shown as different segments of
the circle. The solid lines in each segment represent the target for
that quality control assessment.

# Editing Maximum Lengths of Fish Species

CheckEM uses maximum lengths for fish species from FishBase, sometimes
this length is smaller than that listed on [Fishes of
Australia](https://www.google.com/url?q=https://fishesofaustralia.net.au/&sa=D&source=editors&ust=1784178453980246&usg=AOvVaw1r8WvAcxDk_T9V5k0QNFIZ) or
other reputable sources. A google form is hosted in CheckEM to collect
information on maximum lengths that are greater than those on FishBase.
The form is available through the CheckEM tab Exit maximum lengths or
through [this
link](https://www.google.com/url?q=https://docs.google.com/forms/d/e/1FAIpQLSfDIxlLuxzdsXgWtdp6YI8s_LxpFLANnSHDnk9Io5USOyKGrQ/viewform&sa=D&source=editors&ust=1784178453980636&usg=AOvVaw1D0Eh1p1_Yg3Upt1XaMjOs).
To fill out the form you will need the Family, Genus and Species of the
fish species to update, the new maximum length (in centimetres), the
type of length measure (e.g. Total Length or Fork Length) and a link to
the source material ([Figure 23](#h.v640ezri2eb0)). Any maximum lengths
submitted will not be immediately reflected in CheckEM and will go
through a verification process.

###### Figure 23. The form to edit species maximum lengths.![](https://docs.google.com/docs-images-rt/APuouOfZF314CGb1Un1-Qfk0lNCw7udqA8uX3g4mhzv2vwmEmUzBuUuFObznioRJbO8HsZvTw_XwllZVSihYVQwcdedezxpW9wwOoFsK8Ss2qJEXI9MdnMzLN_bTo63SjrsUO5EBwkOKIMiOi2SA0XFdOwdqt_oXSo-Q5FrpxidQww=s2048)

# Feedback

Our development process has been heavily influenced by user feedback. We
strongly encourage and incorporate feedback on CheckEM, a feedback form
is available on the Feedback tab in CheckEM ([Figure
24](#h.450i5gf5cbf6)) or through this
[link](https://www.google.com/url?q=https://forms.gle/vMjymdYaEpMDvUYM8&sa=D&source=editors&ust=1784178453981583&usg=AOvVaw12DENR4-l9L2AgJv-RDYr7).

![](https://docs.google.com/docs-images-rt/APuouOfOu24QbP-OEWt2tPg8o3jFbJEJ168it6rMKH1GPpUuNUa1BLoy2q_mN1KD4y9hXuxojuD0D0W5BcQj79ONYDzMwBm8XbnmMG2WqnbXHrBsuapgdrTi_yNS-fFFObe2j8ZY0CSlbKzc_-PbLWO_-nbAGmLErJ-9Mz_moG9k1A=s2048)

###### Figure 24. The form to give feedback on CheckEM.
