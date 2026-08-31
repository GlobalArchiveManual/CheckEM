# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/ALKuztYDX3j3Ngev3CP6xo0Pvsyfa6hUoghyXodmMxpTo3PSRU4N2p7W03OKNg3_1WZfNJ-63nkiQzN2hNtR6-PXrVaesGBN3qsejI6WUEcxL1lwjUZyKPdzEaVefG3abenwSWwMKtiTFCTeEsPY0ectICoCsL_8lms34HaBJw0=s2048) 
     
![](https://docs.google.com/docs-images-rt/ALKuztaG2AfL97CEkOxX3EzgkEp_X7zUB8ir8BXx6oMx_b2un2qSnjMMwGmewYkORGUQ55itEpkwymDXBk1kAsEqFGIQg63E7CcSKv6aB7eZPklS6nI0S_nyENqPuW6PM2JB3q3C4HCgcodh1sWkvaklcWL8iKMNyAIdjSDjql0=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/ALKuztba5egat0MDMTxM3lCnvshMgFKxZRcseMEgQ_yZ_qRVsrj5Bpa3IE82fPcHaA6JLeDjlvE3SmIdiQSCEyDqjD64Ck34cpMHofjTQVR742AdrqctPyb6C7ETRhDowe1IDkdoQQOw2mHPaSOW928zWIStzOz47QI5Dl5f_xI=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/ALKuztawYlZGD2IYI1mxx60gCgtFRpC4q4v1SDCMD1X6u-ln5Fs7tOYha6X7xfQSJTfEoNRlGo4S_kbWxa9vBt5wfvz7YDrdUVpvOunyq-yNtR_Nku8wy34XjJKKQw8gL9zLNHOXGKvTipVttUF8ETfrUN1RRTyireMyNCwxMlg=s2048)

------------------------------------------------------------------------

Table of Contents

[Introduction](#h.f26m14x8fsxb)

[Brief explanation of the methodology](#h.7f6qcane86ek)

[Levels in annotation schema](#h.cf5m2n6emqar)

[TransectMeasure - Standard Operating Procedure](#h.y1ockp53guw1)

[1. Load images and attribute file](#h.qynxz9u1xouc)

[2. Setting and overlaying the annotation points and adding frame
information](#h.skp19kwegw3j)

[Single imagery e.g. BRUV](#h.wp91lalbvzu0)

[Composite imagery e.g. BOSS](#h.6lcxtks4h31n)

[3. Classifying the benthic composition in an image](#h.83oq21wyw0f)

[4. Classifying the relief of an image](#h.yqphnd44ciy)

[Single imagery e.g. BRUV](#h.9tl006grml9p)

[Composite imagery e.g. BOSS](#h.s192u55p4dy)

[To annotate relief for all methods](#h.mbcbyrnhudub)

[5. Exporting from TransectMeasure](#h.hu53atdsw2u9)

[Annotation summary and quality control](#h.c6e9xoqrv312)

[Examples of publications that have used this or earlier versions of
this SOP](#h.oxbhkpip7byt)

List of Figures

[Figure 1: TransectMeasure initial opening screen.](#h.3hytau3jewf2)

[Figure 2: Creating a new measurement file in
TransectMeasure.](#h.xxy7mnve9pur)

[Figure 3: Setting the picture directory and loading the image in
TransectMeasure.](#h.5braz3xbt2)

[Figure 4: Loading the attribute file in
TransectMeasure.](#h.q30uqg3kyd86)

[Figure x: Setting the information field names.](#h.8p2h1jz8ywc9)

[Figure x: Saving the measurement file.](#h.2xhkj75vez8w)

[Fig x: Information field value editor, with values for “campaignid” and
“relief_annotated” entered for a benthic annotation measurement
file.](#h.cb2j9s5vzctf)

[Figure 6: Setting the dot configuration in
TransectMeasure.](#h.os61v9twewhf)

[Figure XX. Total number of images/measurements in the TMobs
file.](#h.lnb6ithxlbql)

[Figure 7: Adding dots in TransectMeasure.](#h.qh787v4trzww)

[Figure 8: Example of a stereo-BRUV image with random points added to
the lower 50% of the image ready for annotation.](#h.eqtnf1e6bf4z)

[Figure 9: Example of adding frame information fields to a stereo-BRUV
image using the “ctrl + t” shortcut.](#h.rog6gjoaa2bm)

[Figure 10X: Adding sticky notes to your monitor to accurately define
the lower 50% of each of the panels.](#h.48qx7qqjkob2)

[Figure 9: Example of a panoramic drop camera image with random points
added to the lower 50% of each pane. The red box in the bottom right
image denotes the custom area of interest used to overlay the annotation
points in TransectMeasure.](#h.qnk4279hd9t3)

[Figure 10: The ‘attribute editor’ window within
TransectMeasure.](#h.bjtbnjqmd6a5)

[Fig xx. An example polygon selection made around multiple annotation
points. This selection allows multiple points to be assigned the same
attribute.](#h.q5ffqt5qasck)

[Fig xx. Editing a polygon selection to assign multiple annotation
points the same attribute selection from the schema.](#h.thzefyhjzih1)

[Figure 11: Setting the dot configuration in
TransectMeasure.](#h.nwm5h82z8bk5)

[Figure 12: Adding dots to an image in
TransectMeasure.](#h.llw1ov5j1i6f)

[Figure 14: Example of a stereo-BRUV image annotated for
relief.](#h.u2hkjbjmrr2x)

[Figure 15: Example of a panoramic drop camera composite image annotated
for relief.](#h.mwydz4z6tttw)

[Figure 17: Changing current settings to include frame information
fields in text reports.](#h.u3fzrl3a4d76)

[Figure 18: The ‘batch text file output’ option in
TransectMeasure.](#h.6kr7sdz2ueh)

[Figure 19: Input and output file directory options for batch text file
outputs in TransectMeasure.](#h.b3lvl7wjhven)

[Figure 19: Quality control visualisation of the benthic
annotations.](#h.ewakrht82sbm)

[Figure 20: Quality control visualisation of the relief
annotations.](#h.vv4fc69ndj4r)

[Figure 21: Quality control spatial visualisation of broad benthic
classes annotated.](#h.fiagxoa5czpr)

------------------------------------------------------------------------

## 

## Introduction

We have developed a simple approach to characterise benthic composition
and complexity from horizontally-facing imagery (including Baited Remote
Underwater stereo-Video and Benthic Observation Survey System), adapting
existing standardised schema for benthic composition ([CATAMI
classification
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1788152134832835&usg=AOvVaw2XtUS2PPJMdW4sb6cioXBX))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1788152134833831&usg=AOvVaw3lYThb1u3m3DHxpOL0A1Vp),
a flexible software designed for the analysis of marine imagery. Benthic
composition is classified using a random-point approach, wherein the
biota directly under twenty randomly assigned points per image are
analysed. To increase annotation efficiency, random points are only
allocated to the lower 50% of the image, to avoid numerous unscorable or
‘open water’ annotation points in the top half of horizontal-facing
benthic imagery. Benthic composition is classified using a hierarchical
scheme that allows for standardised annotation labels which are
consistent across different locations, over a wide range of depths, and
in variable water visibility ([CATAMI classification
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1788152134834796&usg=AOvVaw14Df8H2qDQySpB2eVAJ8ca)).
 

2.  Classifying the relief

Relief, or benthic complexity, is classified using a six point numeric
scale adapted from Polunin and Roberts (1993). This scale provides a
consistent method to score benthic complexity across a variety of
habitat types, locations and water depths. The scale scores complexity
from completely featureless and flat, up to highly complex benthos with
For the benthic relief annotation, a 5 x 4 grid composing 20 rectangles
across the entire image are assigned. During annotation, each grid-cell
is analysed individually, and scores for mean and standard deviation
across an image or deployment can be calculated.

### Levels in annotation schema

The annotation schema we provide, adapted from the [CATAMI
classification
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1788152134836576&usg=AOvVaw3xTQN2N7E5tu81jv1hr0JS),
uses a hierarchical system to allow for accurate and consistent
annotation labels. This schema uses ‘levels’ that increase in taxonomic
and morphological resolution, with low levels representing coarse
classifications (e.g. ‘macroalgae’, ‘sponges’) and high levels
representing fine classifications (e.g. ‘branching octocorals). We also
provide a scientific name level, where some easily identifiable and
ecologically meaningful species can be annotated to full taxonomic
resolution (e.g. Ecklonia radiata).

The hierarchy of this annotation schema means that the level of detail
can be decided by the custodians of the project, with annotators
choosing to leave annotation labels at a lower level where high
morphological or taxonomic resolution is not required. We recommend
interrogating the required outcomes of a project and choosing the lowest
annotation level required to meet the outcomes. This will increase
annotation efficiency and allow for rapid data analysis.

------------------------------------------------------------------------

## 

## TransectMeasure - Standard Operating Procedure

### 1. Load images and attribute file

- Open the program TransectMeasure and you will be welcomed with a blank
  screen ([Fig. 1](#h.3hytau3jewf2)).

![](https://docs.google.com/docs-images-rt/ALKuztY17oknEJr9TbrcQUbCwZi3LyeCPfH9BlOHkc871qGmJfAZ_LPCh5ZRwUac8khdKEp-tyu6EKDuQAFNvZLCbzQYG6NoZQdPMpv85cZYoQX2G84a6wh5wmkfYqZ20W_FYLpaX_eR2tebMGiW4pAqRvXLlsV8TYUmiUk=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/ALKuztb0Ydut730gF52kPTyqGV18fuLDZQaAPTUYrjq-NkQWvnOegAS9SJESLI964k4vYG47zJK7IYmNaFvUP6_c88PPuczvNgzl5XZxiBJg4MvRvUhjU4XUOwshjbGpPl6pC2ONrOnKP6WwagI5hbOJAO2OzkvDf8mb1wlynQ=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/ALKuztb3ZyJ-EPPSfmQl2xUk5JrLAUH5rYGtqRHTQGhV_VNoTxAvXo7U-9zT9sJE7GOvtIrYN3wqG65G5-JzSLvHKqbK5FTd8G0RatzK6f5rLmRzsqlUFQlVHMnODRUNBBHGtaK_wyDlNuH_8XcKaVzdDHSDvuKAr9D_aME=s2048 "image_tooltip")

##### Figure 3: Setting the picture directory and loading the image in TransectMeasure.

- Load the first image to be analysed: Go to “Picture” \> “Load picture
  ...” \> click on the first image in the folder (Fig. 3).

&nbsp;

- Note: It is important that the benthos is unobstructed by
  bubbles/fish/sediment plumes, retake any benthic images if they are
  unfocused/blurry or if visibility is low.
- For stereo-BRUV imagery, we recommend that left images are annotated,
  and for panoramic drop camera imagery, we recommend that top images
  are annotated.

&nbsp;

- Press the left arrow key to make sure that you are actually at the
  first image in the sequence (TransectMeasure often orders image
  filenames in a different manner to the native Windows filename
  ordering).
- To load the attribute file containing all of the CATAMI habitat
  classifications: Go to “Measurements” \> “Load attribute file ...” \>
  The attribute file is a text file containing the information necessary
  for populating the drop down tabs when classifying your image (Fig.
  4). This is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1788152134844319&usg=AOvVaw12FL8lf4hUdeVH_ObojRgJ) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/ALKuztbHdQDsnw8ui9_1akhcAAStyqtbHH8mD8oJ2VqTjJiUUkGSnBHN9nMFCnoKJryq0xjGJUswa27ALOvlt4GTEb_Cfdb8zXDeLhf30YXrG37oiWeCgP3dMiVLxR0mK-9EAHLy3qmsoJYfySuSwCSNnHhb8FY0gZJBidI=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/ALKuztYYG-Gu3JekMsLAWw-C9CdQao0qLdHofi8j4awsenJ4TDrJiiVnv-zM59W-iEkn2cofDlNcDI7arJTmjqJ4GccaD3RQGJmInojnWBtZbd_OmsMSdWiHqVZXUaQzuzWMYipIFuMVfoaVzCfk27_spfziVbtaYtWjtHD7AVs=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/ALKuztYSj08lEE3zgKa8GtVvr3acND_ltqIK5YaJnNrGxwklB_2m8KprGBBhLh2atTkWIpoHTvZ1c5eEpDQg7OEXqbpUgrXNmpteLoePVPTVqhkY5YtYLi-QFApBSZJXSYBO9TY-go01ae3j4oZnDeTwr97uyqeXD49l8Oth=s2048)Figure x: Saving the measurement file. 

### 2. Setting and overlaying the annotation points and adding frame information

This section is split into two sets of instructions for [single imagery
approaches](#h.wp91lalbvzu0), such as Baited Remote Underwater
stereo-Video (stereo-BRUVs), and [composite imagery
approaches](#h.6lcxtks4h31n), such as stereo Benthic Observation Survey
Systems (stereo-BOSS). Different procedures are required to overlay the
annotation points, due to the multi-pane imagery used for the composite
imagery.

#### Single imagery e.g. BRUV

- Enter the project information fields: “Measurements” \> “Information
  fields” \> “Edit field values” and enter the value for “campaignid”
  (e.g. 2024-05_Investigator_stereo-BRUVs) and “No” for
  “relief_annotated”.

![](https://docs.google.com/docs-images-rt/ALKuztYAFqtPAegZPAZmIKiHrHu0eve_BJN_xz78G5t6Af4RXqE_gV39V4I4z3rlHsMKW3881-SNOYdPg8R9COosqU4V2Ni67GE0iYAgD8DRW7z4NpEttFy_2AqoAXdPZfQDTchXfqTPDLzHe3gdouvZcaK4Hs2wXzhAKG-oHbw=s2048)

##### Fig x: Information field value editor, with values for “campaignid” and “relief_annotated” entered for a benthic annotation measurement file.

- To set up the area of interest:

&nbsp;

- Go to “Measurements” \> “Dot configuration” \> Select “Gridded Dots”
  and change the settings to: , ‘Dots across image’ = 1, ‘Dots down
  image’ = 2 and check the “Overlay rectangles” box) \> Right click on
  the image and select “Overlay dots”.
- Use the border of the rectangles as a guide to set the area of
  interest. Hold down the shift key and left click on the four corners
  of the rectangle that forms the lower 50% of the image \> then Right
  click \> Click “Add new area of interest” (Fig. 5).

![](https://docs.google.com/docs-images-rt/ALKuztbkmjEftH7_1J3V9b-wqhG-zTunSusSKKXO28isBWglPszZSAE13mc5N8TfGyCAEbonw5baQ0_V_YQJ6KCuUdjsZeTz6C6MrSbMoS_0ymQ1z1zDDCmya1v1RvuG9fTkKEgRt3JtV6u1SPAqdiqR-3bvTGb2skFjZ6lEcQ=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/ALKuztb05RGy70qvGUR4qgjZdoMnxESziZoCM8C-8pfZkegBeQQILh5uHQmM95AscwdUauV2UcEY_ja381nqbJsyWXR4lPTVx8N5tSrpJiyzMBHIg6iTMqzzju1HomCPBqJhe7c4P7bM_Y1HBRjoQ6YXTrES3UjgBBDltGLS9QQ=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/ALKuztakcCnwX01EJ_EkrhneSSiKrIqwBsDaigEG5JJQV66OiRaAzJnT_x_B6WRK2slAnUKaGi3ADgRHVMpGIvbsz0XozMRWYgQUYp1x9DGgSPC5ufOabJRJ5MHbKk1N5Gx_D8VlJvQuFHr9tt4HwXTFIxMwlnjAKWPmh02L0g=s2048)

##### Figure 6: Setting the dot configuration in TransectMeasure.

- To overlay the points: Right click on an image and select “Overlay
  dots” (Fig. 7). The name of the image will then appear in the table to
  the left of the image.
- Press the right arrow key to move to the next picture in the picture
  directory. Use “ctrl + i” to add the last used area of interest and
  overlay random dots (Fig. 8). Repeat this step until all pictures in
  the picture directory have the dots overlaid.

&nbsp;

- NOTE: Check to see if the number of images with dots overlaid matches
  the total number of successful images recorded in your metadata (Fig
  XX).

![](https://docs.google.com/docs-images-rt/ALKuztam6jCbIk8B-1fsMhlauinlW6P_1mfReiW1gAzyehj2VFFZOMQy4oCi7PIQk7qLflsBm5lrQoAzQQslY7_vCiTUk_MQiYwgH7M6Q4u2bawvGz7LZow9X5a37JWxajL4bh2vJo5rjwWHWkjyBYizusuA-hQOt9f8nXxBfy8=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/ALKuztYxKIB_DMfTRnIgT1X6P6wbgKGlTOQxj1mRbu9rOYAUBpyu9hHS0PsOZdt1zDsBlxyqRvyJ9uULU_upCBPkS-u1TmCcC2WB4JoeVDo_OsY8_yTUuwHzwBnYBbwYfrktcrew3HX_DYXivX962-Ljn_cmrAypzsoQATEEjw=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/ALKuztYciEYW463MXcdasNtu7HQtZCYQcweuTtzY7ipOe60hL5Nf_3M7OQ3v0QYyKXOIbLFmoQnppeNYOn2C4IXFMJoWPuCQ9C16FprPuEC1U1YSAn0H_sJNv_seIKpbWRgijaiSkKQcY-BFwNbLKvseYcAYoE7pOWn6Pstp5Qo=s2048)

##### Figure 8: Example of a stereo-BRUV image with random points added to the lower 50% of the image ready for annotation. 

- To set the frame information: After the image has had dots added, use
  “ctrl + t” to automatically fill information stored in the image
  filename into the frame information fields (when prompted review the
  autofill information and select “Yes”).

NOTE: The frame information fields are important to match the benthic
data to the sample metadata. The “ctrl + t” autofill information uses
the filename structure that is automatically generated when exporting
benthic images from EventMeasure (Right click \> “Save as  jpeg”. If you
wish to use this feature, you must either export your images from
EventMeasure, or name your images to match the following structure -
‘OpCode_Period_ImageName’. TransectMeasure will detect the first 2
underscores as separators and parse the information from these into the
frame information fields.

![](https://docs.google.com/docs-images-rt/ALKuztalTKfNwgKpoarMo5SlJ2r8x4WrEyMo3TyamBTR3KDHe6W0JQtREOw1szeyvUS2s_O7eSip38lF5hAlRbvgFwWju89RZtsrLCQyS12IpMfc5aEGhxoVEQzLbKViNybRiyWDRTuMvvy5pLgObbVfcw28z2YqLTNZ2jqD=s2048)

##### Figure 9: Example of adding frame information fields to a stereo-BRUV image using the “ctrl + t” shortcut. 

#### Composite imagery e.g. BOSS

- Enter the project information fields: “Measurements” \> “Information
  fields” \> “Edit field values” and enter the value for “campaignid”
  (e.g. 2024-05_Investigator_stereo-BOSS) and “No” for
  “relief_annotated”.

Figure

- Go to “Measurements” \> “Dot configuration” \> Select “Gridded Dots”
  and change the settings to: Overlay gridded dots with rectangles
  (“Measurements” \> “Dot configuration ...”. Set accordingly: Gridded
  dots, ‘Dots across image’ = 2, ‘Dots down image’ = 4 and check the
  “Overlay rectangles” box).

Figure

- Once you have added an area of interest you will not be able to
  overlay the grids again without overwriting any existing random point
  data. Our high-tech solution to this problem is to grab some sticky
  notes or another device to mark the corners of the rectangles on your
  screen (Fig. 10X). We have requested a change to TransectMeasure and
  will update this guide once the changes have been made.

![](https://docs.google.com/docs-images-rt/ALKuztaKMrszt61eBmcxev_m6PYCl8_KMRI8e7TOPM-W4jkOyL4lNyx7mS7G8QJqTw6hoL1gdHWViONgMx8BHERCQ8oXg4NUjqMw0SLYg3iW-BqMdxejr4hycAHDDYHl5sYMxTUe2EesaYHiEfwTkaHpnFpnL6dpM5FyFwdNNek=s2048)

##### Figure 10X: Adding sticky notes to your monitor to accurately define the lower 50% of each of the panels.

#####  

- To set up the area of interest for the first pane of the image: Hold
  shift and left click on the four corners of the rectangle that forms
  the lower 50% of one pane of the composite image \> Right click \>
  “Add new area of interest” (Fig. 9).

Figure

- To add the random points: Go to “Measurements” \> “Dot configuration
  ...”. Set accordingly: Random dots, ‘Number of dots’ = 20 (Fig. 6).
  This will allow you to classify the benthic composition according to
  20 randomly generated points over the lower 50% of the screen.

Figure

- To overlay the points: Right click on an image and select “Overlay
  dots” (Fig. 10). The name of the image will then appear in the table
  to the left of the image.

Figure

- Press the right arrow key to load the next picture in the picture
  directory. Use “ctrl + i” to add the last used area of interest and
  overlay random dots (Fig. 8). Repeat this process until all pictures
  have the first area of interest and random points.

Figure

- Move back to the first image in the sequence (this will appear at the
  top of the list in the left hand side of TransectMeasure). Use the
  sticky notes or markings as a guide to set the second area of
  interest.

Figure

- To add points to the second pane of the image: “ctrl + right click +
  Add dots” \> “Add to the existing points” (Fig. 9). For all subsequent
  images in the image directory, “ctrl + shift + i” will overlay the
  last area of interest and add additional random dots to the image.

Figure

- Repeat this process for the third and fourth panes.

![](https://docs.google.com/docs-images-rt/ALKuztYX2dAK3JZRLZ9YOhpzgnr5dxyVLSknkGlN26JCuWSel55ZAETWtUJ5d5mGAd5yDwIuAVnYcz5IMq7Li6QNwP0bDhvMPPZND8axsz8VJnS3JjE39ZZvsaY_Yzndua_r4M8C1OGyX9y4_NVIwqXZ8iqwbIzZX9L4uIZs=s2048)

##### Figure 9: Example of a panoramic drop camera image with random points added to the lower 50% of each pane. The red box in the bottom right image denotes the custom area of interest used to overlay the annotation points in TransectMeasure.

- To set the frame information: After the image has had dots added, use
  “ctrl + t” to automatically fill information stored in the image
  filename into the frame information fields (when prompted review the
  autofill information and select “Yes”).

Figure

### 3. Classifying the benthic composition in an image

- Left click on a random point to display the “Attribute editor” (Fig.
  10)

&nbsp;

- If the benthos is not obscured

&nbsp;

- Then select the biota that lies directly underneath the point in the
  “level_2” dropdown (Cnidaria, Sponges etc.).
- Continue to populate each level drop down (i.e. “level_3” \> “level_4”
  \> “level_5” \> “scientific” \> “qualifiers”) until you reach the
  highest level you have decided to annotate to (See [Levels in
  annotation schema](#h.cf5m2n6emqar)). Available selections will change
  depending on the value selected in the previous level.
- If you can not determine a level e.g. you know the level_2 category is
  Macroalgae but you are not sure of the level_3 category then leave the
  level you are unsure of blank.
- Select “Clear” to reset the dropdowns for all of the categories (Fig.
  10).

&nbsp;

- If the benthos is obscured (e.g. point lands on a bait arm, bait bag,
  part of a frame) or not visible (e.g. point lands on open water or the
  benthos is not distinguishable due to bad visibility), you should
  select “Unscorable” in the “level_2” dropdown (Fig. 8).

&nbsp;

- Repeat for all other points to annotate.

![](https://docs.google.com/docs-images-rt/ALKuztbREnxRa_epWjAd_p8tclwnWL3VTkuUgTdtF4foDMsgkn-M0zSF7NNI6AmuwXsDAql70V9yFRpw93S7DiK0zORda93RPxpN3UEQcuqxB10R9NhfeGp21ZOqZii4qYs5q6lqM5zPd4Uadx2Lyvnw02HkqaNgkffKInkMrHg=s2048)

##### Figure 10: The ‘attribute editor’ window within TransectMeasure.

Tips & annotation approaches:

- To zoom into an image, you can either hold “ctrl” and move the mouse
  towards the area you want to zoom to, or, you can use the scroll wheel
  on the mouse to zoom into an area of the image.
- In the attribute editor, points from the history can be quickly
  assigned to an annotation point using ‘hot keys’, which are called
  using the ‘function’ keys on the keyboard (e.g. f2, f3 etc…). The
  keyboard shortcuts for the history items are dynamic, and care must be
  taken when using them to ensure that incorrect annotations are not
  accidentally assigned.

Figure

- To assign the same label to multiple points, a polygon selection can
  be made: hold shift + left click around the desired points (Fig. x) \>
  right click and “Edit attributes (polygon selection)” (Fig. x).

![](https://docs.google.com/docs-images-rt/ALKuztY9ToKOH34Xds-kF6TS7NsqvI3z9sLOkDSeYLkiAFoT1UbokKmNnP3Ny75uyu29isvxVaJ0HYPBDlqnHNyLxD8fk2WDuB9Xnwv6vfkTkI1V-Rr2AO_sPj542jUl_PZ_b2arEzPI2FryJ4BzXenU9PvXPkWmal3ldEpeFnw=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/ALKuztYIAYhluONtxm5YJsx7hB8oL20pQHOqnuJi5ONSAfaiubayTkPRUniqk5IdR7wQtP4KFLo4HLpHdRJs-gknzt2hv_OeviU9PoLrzYwM1ABwl7Ao3VvaJRIIPdaTDmeOj_H2CRvb7cJ6zP1NcBiEt7RFGxrRQN91-ZmP7s4=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1788152134869254&usg=AOvVaw1djmluwq6KPUQ9T0Y89yon) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1788152134869507&usg=AOvVaw1mZcoMMy3DCo_G2yW0T3DW) are
some examples of relief annotations.

This section is split into a set of instructions for [single imagery
approaches](#h.wp91lalbvzu0), such as stereo-BRUVs, and a set of
instructions for [composite imagery approaches](#h.6lcxtks4h31n), such
as stereo-BOSS. Different procedures are required to overlay the
annotation points, due to the multi-pane imagery used for the composite
imagery.

#### Single imagery e.g. BRUV

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1788152134871175&usg=AOvVaw1yEbpzS94HivGsSBGJj_4j) -
  ‘Schema downloads - benthic-relief-annotation…’.

&nbsp;

- Enter the project information fields: “Measurements” \> “Information
  fields” \> “Edit field values” and enter the value for “campaignid”
  (e.g. 2024-05_Investigator_stereo-BRUVs) and “Yes” for
  “relief_annotated”.

Figure

- To set up the grid: “Measurements” \> “Dot configuration ...”. Set
  accordingly: Grided dots, ‘Dots across image’ = 5, ’Dots down image’ =
  4 and check the “Overlay rectangles” box (Fig. 11). This will allow
  you to classify the relief according to 20 gridded rectangles.

![](https://docs.google.com/docs-images-rt/ALKuztaQfQmCCLCrkD-bt6TWzDkE1bB_YKEnbewME6ia8-lqs2rodO_C0wnZCro2Zp1zPpr4dggAW67vhWGPWxkis54hOLiuci-uolo5y0jZH9rLVWrhW3hsHwNvM08B_3IudEC5LUbo_tci50MG3TFrbvQMWRY76s2EhntoiQ=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/ALKuztYxKIB_DMfTRnIgT1X6P6wbgKGlTOQxj1mRbu9rOYAUBpyu9hHS0PsOZdt1zDsBlxyqRvyJ9uULU_upCBPkS-u1TmCcC2WB4JoeVDo_OsY8_yTUuwHzwBnYBbwYfrktcrew3HX_DYXivX962-Ljn_cmrAypzsoQATEEjw=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1788152134874402&usg=AOvVaw0tcVO8Pcc-ldl5wmTd1rGa) -
  ‘Schema downloads - benthic-relief-annotation…’.

&nbsp;

- Enter the project information fields: “Measurements” \> “Information
  fields” \> “Edit field values” and enter the value for “campaignid”
  (e.g. 2024-05_Investigator_stereo-BOSS) and “Yes” for
  “relief_annotated”.

Figure

- To set up the grid: “Measurements” \> “Dot configuration ...”. Set
  accordingly: Grided dots, ‘Dots across image’ = 10, ’Dots down image’
  = 8 and check the “Overlay rectangles” box (Fig. 11). This will allow
  you to classify the relief according to 80 gridded points. You should
  only need to change these settings the first time you use the program
  on your computer.

Figure

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

Figure

#### To annotate relief for all methods

- Left click on a point to display the “Attribute editor” (Fig. 13)

Figure

- Select the relevant relief score for the whole rectangle from the
  “level_5” dropdown (Figs. 14 & 15). The remaining drop downs will
  populate after you select the appropriate value in “level_5”.

Figure

- The dropdown for “level_5” indicates the structural complexity of the
  substrate and associated benthos. This dropdown includes six distinct
  categories adapted from Polunin and Roberts (1993), which are;

&nbsp;

- 0. Flat substrate, sandy, rubble with few features. ~0 substrate slope
- 1. Some relief features amongst mostly flat substrate/sand/rubble.
  \<45 degree substrate slope
- 2. Mostly relief features amongst some flat substrate or rubble. ~45
  substrate slope
- 3. Good relief structure with some overhangs. \>45 substrate slope
- 4. High structural complexity, fissures and caves. Vertical wall. ~90
  substrate slope.
- 5. Exceptional structural complexity, numerous large holes and caves.
  Vertical wall. ~90 substrate slope.

NOTE: Any ‘rectangle’ that has more than 50% benthos/substrate visible
should be classified for Relief. Any rectangle with less than 50%
benthos/substrate visible (e.g. more than 50% open water) should not be
annotated. The relief score for rectangles that are entirely composed of
open water should also be left blank.

#### 

------------------------------------------------------------------------

#### 

#### 

![](https://docs.google.com/docs-images-rt/ALKuztYxbpepnAVu6xXUGy6h0XuBhKulY5cwanJ8B-tBANgLeXPQeCbpII7_hqQJXLXBr7MjuRxLToyTeMNYb0Bpn86V8GF5zCic3O5MGsrtT7LHzS6uhee8p9u_CPQ12-h4uJJWT-opOxAwnMzwVDg3ZPMnx3ghSRMBj4op4Z8=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/ALKuztaFUcCV6G7qqY3OsGuTHYaDj1_G-69C5x2nfD4zssYJx6jOWKYNjYoNbAHGGZsP1nvEBloNs4rS-LNnvyzoNIDhPS7LjWtzFjov_kmJEsL0oYieknAnxRidK4-SafhQGrQRSnPHPnDhKaGl484b4xtAjhnNO6arRljpSVE=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/ALKuztauo39ZrLe501xzz-d-E-emkmr_luqduhsWE0nNRKVC4LCJhiL4Ud6RLJXwPUwcSm7prngnlObSyXWHsbxZTUVWg6xS-iAxWPt6xT3bs2kDQ155Y1OqbsZhYJdi117Bz7weOObWELSF9vSeI1sTlY-tnUN1Q0kKUT2rXVQ=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/ALKuztaS5k9Y4x4MUuTPigUeK-1DVqKjFElwfqcpjw3LTgJz1GpQVDTRGozGyBeLhWT1ENxkzCTw1VWWVNH51VAh4vGeAOK4TnR-J7RLV1rijazmPe623DyQuqNJ947HK6-qbJR96rqhAwiScq8aaoWetnVmAZ1kpQq2AyDvgw=s2048 "image_tooltip")

##### Figure 18: The ‘batch text file output’ option in TransectMeasure.

- The following box should appear: Double click to the right of the ✓
  (under “Data”) in the “Input file directory” row, then locate the
  folder where your \*.TMObs files have been saved, do the same for the
  “output file directory” to specify the folder location for saving your
  text file (Fig. 19). Now select “Process”. This will generate an
  output text file that can be processed using the scripts included in
  the GitHub repository (see ‘[Annotation summary and quality
  control](#h.c6e9xoqrv312)’). 

#### 

#### 

![alt_text](https://docs.google.com/docs-images-rt/ALKuztYuITmQHdhCbtem0O5mhXmyGTbm-G4ZVVssaU6SAnSqBq7nidIvCuCG-zPFU2BklHFtBrbGv-Cr6DH67HsiIdARYmpMtC8Wi9KnMlo9EPKNxK1X1_yVG4eV8XNNnjg2Le2zUXNNc0GyiyPIVpD6H__Mm0Bd4cXShIc=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1788152134885947&usg=AOvVaw2DsU1ebKA3AYkdu538m82N)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/ALKuztbrZFRuoTXzHTRucJw2Tdm6ITH9odTd4XfaleJj0cUCnsHWLqcTKSg0XPpzw-fTCfuK9oPEdm6AnyDPrrYcfCZhEpTfU_uQWpcAVrCsPbj4R8Nhkfr5cdiWdZURnsgCHdZKOQdFq9Aq9StkxUygFvdH02nZQApMyiaBtLk=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/ALKuztY6gOsnib9aJysvD_mAWOArG6MIDYdigu8fPE5jWD554Y6mvLC3r-1NoL0MhaPgc8z7W5Y528PUdPpEbXX8I3aACE4tEhDb_wPLsWhF_McDw4iswRRttjWroBQT9UqfqkAXupezKiQZoi8YM2zb5UuSyCQVY3t_VLSYBJQ=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/ALKuztbuXlC57DhdhDu7SSANAk4MGVJwZ3HmCt4jIaMmVuYFDzDlBpQhmztI5taeOCwMZX4jFdngI_Lt0tk2hOw3zErsNs4jWZbr_WmLgUlRJQIymn_aZ3E3-hfiBW82p_jucb8TppWFHzPXqe5aXuz5xUJFkGz6rClplMCh4es=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134888688&usg=AOvVaw2NGz71l6TF1rWH7iffJ7jK)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134888846&usg=AOvVaw3i_nLdog5qir4__G75k_Hk)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134889239&usg=AOvVaw3HIT2TtxlK621v0zIEjZEZ)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134889403&usg=AOvVaw35QzrPoX2VI_cAtlYZ3S5M)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134889478&usg=AOvVaw1ilTdveR_ZuBSMol8LOIjR)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134889558&usg=AOvVaw2G19Vu6p8nW_LOzOE6fEAf)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1788152134889666&usg=AOvVaw1N9etzrN0ahedzcfY7Z-vx)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134889891&usg=AOvVaw0ZD-8UMi51lXoqnVzKJt5-)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890023&usg=AOvVaw1K5JuBC3_BaleRWexD6IQP)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890396&usg=AOvVaw3aQZqVKRl6psAWsryXmh6I)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890532&usg=AOvVaw0rvzIsKqka3Eu78liLsdb6)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890633&usg=AOvVaw3j3EYXY7i4AsqKZMEt2InN)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890731&usg=AOvVaw3VNYOHexwkWb5cd191Upsv)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1788152134890869&usg=AOvVaw2-MANQiLh51myDoOaoN7xk)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1788152134891630&usg=AOvVaw2eoAskJKLd3iMMDV1Ml37T)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1788152134891789&usg=AOvVaw3wSaTa5Jhb3Cx3zzjVojqM)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1788152134891883&usg=AOvVaw3XTLDLzELcgBssopGPNht3)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1788152134891974&usg=AOvVaw2fU1GDN_hrADgSc5Iaq3dP)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1788152134892111&usg=AOvVaw042C3nfKDyZ9-xHyr4O7Xu)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134892420&usg=AOvVaw3VR3l3Dxlamx0j5yjkUiCc)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134892557&usg=AOvVaw2b_M9ir8L_65b6UIc065qk)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134892946&usg=AOvVaw32rBT7yUyWGQUUHRB3s3aU)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134893112&usg=AOvVaw2Awv9NOsstb73V6KGNqN_n)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134893258&usg=AOvVaw2Dd4LjcmFDGPykY6trPfS-)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1788152134893457&usg=AOvVaw1V71FXn3nrAyPdAvMaAtHO)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1788152134893555&usg=AOvVaw2QcPQyf5PxuDCTlI9i9KvX)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134893805&usg=AOvVaw1F8nvrlhJLGnL0Svir-4oM)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134893908&usg=AOvVaw2PZIHeRh5XN88R_N4Zz4oH)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134894179&usg=AOvVaw1q7Ru901ivwQ-2XZQDyVb5)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134894291&usg=AOvVaw3x15JOKEj9zJDrEUzXQXvY)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134894387&usg=AOvVaw1ZxlpXGsrE6qQMGLs6H2UB)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134894509&usg=AOvVaw2-qB8o8lf4Uebv6cVY_2Hu)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1788152134894658&usg=AOvVaw3G6KdG-6qdVsIb2THV-DG0)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134894897&usg=AOvVaw27FbWtc6ZXENnHS5dGGiag)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895003&usg=AOvVaw2arMj29fGWVzK2i34A7LAH)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895272&usg=AOvVaw0D9cx5Lzxri2k7WNjN6UiD)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895370&usg=AOvVaw19XLuw_V3t_dd8Lj_SI3yB)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895438&usg=AOvVaw0qtQ_SvMg_ItxWA1wpqEVI)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895517&usg=AOvVaw1cyhGO532MtcBoIGmenfNe)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1788152134895629&usg=AOvVaw0ekdmNKmTKQsDuw3H2cAZS)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134895843&usg=AOvVaw2HdwB1mLKu8iTdpuRkj4gP)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134895944&usg=AOvVaw3UXVWFM8bNNKYqLl2zZHy0)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134896282&usg=AOvVaw0d4oRWsJ1efDmM3E9uhOzr)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134896379&usg=AOvVaw21EHEuvSi4N0gS6TgtivyH)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134896443&usg=AOvVaw0_mZTNb0U5_UpvA93BQAuy)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134896511&usg=AOvVaw1ljf4YlXtLreCEjIqmO9Iy)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1788152134896661&usg=AOvVaw1GtlE1QUocPKL3cXPxE8Rq)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134896867&usg=AOvVaw0-VlAsqafFjN-LkYaDZDzW)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134896941&usg=AOvVaw2mgo0bo_W2I5nT_CXhjnyu)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134897228&usg=AOvVaw1LbMQwzZoJT7sjcVCXkdgo)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134897324&usg=AOvVaw1XtQq005BqDjESJZuBpRRk)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134897395&usg=AOvVaw0QL3gGb5Sh0WRO5IJGSAsR)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134897468&usg=AOvVaw0mQCNy1iDd9VGHLUNFXXBx)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1788152134897572&usg=AOvVaw0qIJaFT_0FR_ZSvhGJN0WW)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1788152134898219&usg=AOvVaw10FJavpS9cgpWKfbHFI7W3)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1788152134898357&usg=AOvVaw15jPPvZ_nPcZOyGIAr8CBZ)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1788152134898468&usg=AOvVaw3Fsfs9qE0fYwZCSJ4Mm3xO)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1788152134898638&usg=AOvVaw0sD1tjTQKqB8RyWTLcevEW)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1788152134898763&usg=AOvVaw1ZtzN4q2GLeRWB8X3jfUCq)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1788152134899599&usg=AOvVaw0x0DSz3OYMmGTx5ZZmyWKs)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1788152134899740&usg=AOvVaw3GwhU7NAJTsbcuD1GhmKNY)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1788152134899818&usg=AOvVaw096aF5YR6bvdOD6r5ZGIfH)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1788152134899886&usg=AOvVaw1r2BmwXZEvenT9FM8PswFg)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1788152134899974&usg=AOvVaw0DAZjcxHnCKTMNzN7gY5KP)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1788152134900458&usg=AOvVaw36-IJPez6Y8EZCpSJsqU2d)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1788152134900598&usg=AOvVaw3Ut6CQxvBU2mZ3D2cGVRWv)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1788152134900729&usg=AOvVaw1k0DSVALcB8PrOTyKsjG8m)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1788152134900814&usg=AOvVaw0MlCgkSslbQEMbzTgg7pOT)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1788152134900920&usg=AOvVaw2iCW6EMRa96Jv-UCMN7ca0)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901109&usg=AOvVaw2zBdLd3KIAw7pBUNFHq4jE)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901201&usg=AOvVaw3qWtHWlb-tP2n1U3kFJ27c)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901397&usg=AOvVaw0XnT8JTWZosSFSytDu2Lu8)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901489&usg=AOvVaw0e27uo0d2hgaaNduWh-OoM)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901575&usg=AOvVaw3ApnvXNCtriW1aW9b6XGGv)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901667&usg=AOvVaw2sXAAvJFcllH_9wSFxYN5m)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1788152134901786&usg=AOvVaw1XeaHhWem3lGcr3Q4dP8qL)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902000&usg=AOvVaw0pEOhxaDKby6GmkRnNgQIH)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902094&usg=AOvVaw2t2O4YPCYiB_8hhKXfB9PF)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902318&usg=AOvVaw3vUuEXg6ga4n7LEW6UTH20)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902416&usg=AOvVaw3BOnoORyjQ4kRtrdoB2BON)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902488&usg=AOvVaw33dCoNGLBxFUfCzLbvrMNc)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902565&usg=AOvVaw0M3DhLwfcWNgWefZLYunC2)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1788152134902685&usg=AOvVaw2zOdkL_gl3Rc2UuqVXFKvw)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1788152134903238&usg=AOvVaw03_FgaJH9dFfMELcJtkVOt)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1788152134903384&usg=AOvVaw2MJp2en9XaiFOf9uLWLBqD)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1788152134903473&usg=AOvVaw1TYhzAVbNvTaMKbAtEUg8_)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1788152134903559&usg=AOvVaw0cNXXlAlPkZdFob5vLkYqg)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1788152134903673&usg=AOvVaw3sDTbhUmbxOnbAqc_fX9Bi)
