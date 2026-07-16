# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/APuouOe8H-IJLgrnBc769-Xhlsd8UaJhztyJUuj-KB2gT6zWtDoh4ugyoQj3Km6q-pcPR5ektT2Fvxt8kKeM17XxpYR7PJSe92tLXxyRD2l6wphIlUviOjfK4pzq0zX-MaRUtwZ20wytgRGjO9r1V-AQXWsR9b4nPnp2n9kBlRhkQw=s2048) 
     
![](https://docs.google.com/docs-images-rt/APuouOef4osEQZ4SfklpHmGDzCb4zi_OCrJ61WNYTnAw5qnYiNtDVVc54sQLlm9_stYT4TRuQUTPDUBmTvDAfPGgkl7Q6oSd-_lNp33qUhxUcnxQdRy4WtdueG2CxrT24uGaZqOkO1kZ3wo5JYEvGyFdtPA3-E6sb23Gpr6bu9oDoQ=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/APuouOePTHaG2JqdECTeyw4QRdDrhsQKwAcL2DcMYUdjPPhqAAAYfyssVbB7U0qvSy9m17bIam0zEfU6FK_1mic_n0Lb-4qj4jSqcygge7zb2vGTTHsSnyoLTkwh1W-1uxE7p6uY890ceqQ0cNp58ikmexdOonNqtz2b0g0f3C00Ag=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/APuouOdp10dA2L6jzfE_72Ds4qYStjqBWwXfFIKRVTs6AmuqGv1OgNYEpul7wSGJHCSrEQf828RYkHMLtHFsQs_ueiMk6uiJPqJEcqE-rynML4D6TK0pZRTTp_gjZIE99NSXj1FEfHiJUHl6uPMmBxVhYHSzYt3MSpV0dKQ1bdtW3A=s2048)

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185509430998&usg=AOvVaw2FCHf7v0AYhz4KeHQwLEUK))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1784185509431641&usg=AOvVaw1uMYA_M2yKjKKLjELM3hAW),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185509432200&usg=AOvVaw3sfPaT-oBt-_Wu2FQ4aQAf)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185509432951&usg=AOvVaw2pH7KOBgLG7_jrh5CCfOQZ),
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

![](https://docs.google.com/docs-images-rt/APuouOe952Q_aSehTbnvqtBL6zgwiRLil2mgSu2M0X3IiAUV64PELNVr1dVv-f8g3dv6Jpd0EFHcYJVESdO1svQMmvW2uu9BiBEPVZhhGMYNXUs8dAmXI83w5D4IpnjShY1RPcPng_MJwDLWXFamHUL6xbkoHVCJCnP55gU17A=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOcWmzXw2Cq3GMn7hsVoeJ7TNLHM3nIBdUS8BJT01q1UbAMwCRKAHd60nC7ltLITOJFgMDlijm2dPwxCWtgxGqQVc-VmjWYWDydJORk2_7S3ouDffmW1aIRpsw3hS34JD9Os1suDoPxUrDAdco2n2W5irZRg7NEnxi6DUEx7=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOfKbo0Fowm9JeHsQU-3E4a8sPmQo8mVpAzxSfyGFvKNXjOjvv_0YaXDsfE94PTH_9VNYzcpCmPfQtLRP9eD9m18Lp067FBFVqjBpy8lvbLwrSMDQ_jNu7oruLZkbnG9aYevYaZYiDrImRlRw07Y0_AVyy9eJEe7KZGgew=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185509435664&usg=AOvVaw0UgbDJrk14NVtScwSWD6YF) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/APuouOdC7JqXGJfB_TbfbD95KYPnO13I8r8ywfj8_gITixuOow-Oyy6JlgvqGIHxmle69pvUwIp27xQKW-WHXvQu-zFIsikCo_Jn7vKxLREpb-bKFpL5bmKE10m-wY09rIp_oSvvkwngaUHI0XGeytw3gkC8hST-20LJ-JdpOA=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/APuouOcaLickaWMp9r4cY3no5lZ_rtAZ7aaSwT5YvJk62M5YXpMyUidDinbSii8RDbXQJDlLF6MOt31XEVpyKxwoT2dJt-KeCNnYO_ZYdPtHZ4ZqUqOvAKA1PyCryI2DjCeZ1Cv1TFtuCt6MqROTLW8QrQvxEI9U0y9QHedhS8m9aA=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/APuouOcRdw9JcijfZFKYURoBGWGtcj6y2FQW1C5pjvo9BQBRlUP-fUj1P3TkWChjcqc4CMT6qguXHnSGyyLCaZspm4S9gRs_N2SwuvVey3WedskLepsBkwiXipFbTa5M3-uSimaH8uDQuF2_F2LP4Yl3tq-8UcX1e4suKszF2BI=s2048)Figure x: Saving the measurement file. 

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

![](https://docs.google.com/docs-images-rt/APuouOfMtb8JfGhUpB1iepN_cEile9V1zC0gIdMXcNoEQRbLWa-vrXiF4puuNgRjTyLrkFmBwFepxtYhd_5bSTPaHCVsF8c7_MLM06DHoP2WoOhYRzSavz6F19LKR7kZEFhAfswSUWInU-RYwTgXxd9yquHH-FxRFnI3ztzjRdtUJQ=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOfANkoZpaiL00pE3zG7fJk4FrDXGkRh4QHpLzVFi-3-CYctzj0mYAH_PtTQXNllueGPLKaTvXcx_IoX_0W8Wr9iqIddTfy7zBbEup2sis4Fnapo-fy0Pxls4EPdBr1EQQdZ-wYBj_bhKPzblhDiX_jbHq3Kmoh_kyQtVARx=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOc3Hc-mHWPJLSUp3ROcuyB2crXjLyEgSSfkVQFOlkbY-_cJkIxpj5L1NslGygzEJsOzNeioCpmKO14T-NBusVYndWjtsv4Zv5_ZgRtcBqWXCVF72GH3WODhjD4HHN3zR78y-xvvrRPO0WFRluNGEZl0jpT1Wxv4ad0AyvWaNg=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/APuouOePqaaiuDI-sU7qkozaM0UeepAs5RhEQBPn7bMCeS5ml7VXvsSTGG6w2GhyT7iPO4DCVwTyFLfXlhNqLryG4TskEE8BD1JR_QnoAgudEwrtFkz8_800icm3vbv0k-da0d6e28Tc8HoeX-izEH8WQmQIyhb7pFUrXtx9gNcJ=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOcEeJ1Qpl6UBImN5_ZjkcxJsIjbcoD4SGuKkFQueuJJCupzAmQczVkF5X54-gR4HikPBC6ZdV6t9SsTQeve9SPjMRUWkNI0YDaadj4RlPDFUNxUf4_2oQSBgFmvWepeWRwah-1FK5mcCSFScAMfee3Kbq0yl_gFrqWmT4Dzvg=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/APuouOchpKThUgiP9OcgIY-xr7vlvddUnC08G0_tClwQthl12xn66k6pZNdj1kVGl2cQizJr2tIPFWQ-SkJ-idSjKVswKJCDsKVJXYjUNwE1Aq24O_O1OTEH7RE5Zo_TyK0MxFADsB-Yi0zz6DEHFQ8d3YfH047FM81-ROVksPRn=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOcf-IIfrScBybXRyBHcnzGmgnvSXvw7iV1q_oeCq0MJ8kiZCOmV4urLFshzsv_MVflzLyjzYQmsSh5O7sSJj5TFsy4AsEybSaCZvKxDOxQyyOQYbUmVNGb0F8tUZFNylStwY_uSIwAOFP_mvTHB1eWIGeKoJEPOt8dGu6jwIA=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOeUGwMi2Qhn8quGVFDuoow_EfwNQusSEL13Iw1BtRXFtIgJUsDkQTmh4qS3hgr3htYbEPZdkbzssRwjCkLNoMXbLzOsV_pux78MNZo73TETL-wCfVmY_CaP0mJqG9QYx-zP8U5DlFjBtmtAag6TcOgg0lm1388PEVWKorI=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOf-6vCabQomZh55SvwQtT1MLTb9oyrfM60dB4uXvvv_iBp20AJ3QtFhRcBH3U_dDWQhY55WoFhzCziPLfu639_WBkYXDfM4wDe2ZmEUFhu0Aok1bcdBMGc9XpfQCfB_WB1dvgix1hKM85WPOYWidXsn4gV3hTe7L6Ql00gHpw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOcukeZ1ZTK1gBtDB3nJEoCl1Myc_lkxX32Wo5XOGVWvEysWwigG4wqO1aYapXBzHAIfe-AHYm1ueIAWakbAxFVQ8bbGHSayg8-geCp-pJqvwvNyY_dNe8itFBwe3D7VsxmQ4mm2FwlWKDlDXVs8FSqZn9ASePSwxyAonrg=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOf4HIoLFbHwrMRmMG4z_90MX1EA_srS7JKnIaBPM6cqOdzcxY7YlWgJSWrkcPr7sz51DIAPqpn-6g3QvajvSgnxyEe7zKW9cGA7ByqRgWe_D9AKvZZFopmm-yznsLGsD3q9vCenbGD2kMtGEkaSsmv0TdpnLPyUwGelkzjTMw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOfODb4xhJovqhsI64MDubr9o6ji_1dAvSOsgaGmtICq9hN30_IUyTqJalNeq3C26bin58jOqabeQbnl9Yen_2kEVXrcyEnI0CRl5PP5ThcK1h9SPA1K7pR3S60IebRiBdLFOQVUr84ZEUrAeN_67AR6Z7lfBylB2j4qIgfT3w=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/APuouOd3GJHkkoIOCrNCZAGTJ65knteMXydrg0vKXK6sOXnJfS-xACyuGmzIu4ZelKkyzj1EsKfE9-jvb3HJ-mw9T9gP8ht317SOLIdS72Ldh35W5IP48bvRLb76nQSAoX7uBP278NiOkexk8AZY5VuC0iK6yJNmFlXaacCxh5WXvw=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185509446528&usg=AOvVaw0fiy8ciKokLiMCHo94YGJ4) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1784185509446696&usg=AOvVaw2B4feJxtC7MKWFoIfwvH2l) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185509447487&usg=AOvVaw3ndC0TSupcIMSJo0MH0ANS) -
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

![](https://docs.google.com/docs-images-rt/APuouOeUVkRp6K5btuopoc8KtJWlLNSk6EpAiqy9uDslXDQBkMpWRIHBTzsdN0x1hVXxL4KH3IKykfUvo1WC6R3bmf7COhnnGltQ-iTYFvMiyNaPtJVPxi1EnoTmogj7N-yLfcztRCv3bGhq6AVgx7il_W_R01sRc9nqJVuvRgT-=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/APuouOchpKThUgiP9OcgIY-xr7vlvddUnC08G0_tClwQthl12xn66k6pZNdj1kVGl2cQizJr2tIPFWQ-SkJ-idSjKVswKJCDsKVJXYjUNwE1Aq24O_O1OTEH7RE5Zo_TyK0MxFADsB-Yi0zz6DEHFQ8d3YfH047FM81-ROVksPRn=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185509448851&usg=AOvVaw0q6y7tq03J97hejv_12kKV) -
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

![](https://docs.google.com/docs-images-rt/APuouOfEifydKg1rkRVFBIjCq9PQSzUtEiQgkJBN6H_iyw5GzEtPVH5It7KQEiMyg5vox088tv-ZH7kzPeS52GH_3Z8OtNYJIFsUFmwrXaDC027sgLo1p-cj2HibbYwAFddvj8ThlLR22fY-rNni8bbO5Kn9_qKRYXFvKT5KHXF4cw=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/APuouOflvQ-j8Oy3g7g2alpIJx1tDAIF34B8fyMDbjGZYWlxSMIgwJNaGSMu9lbyCbyIZpe1r80T6GBjVPkXsq2Gn4tng7x7yAf1fhbdybW8HMdoouTYHTWKzUqB7SV_DrXExcmq7dsCu-G6tj6fVRHnp7BKOp-Igy2Ug4Dvv5BnJw=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/APuouOcEhpl5umPWqaYc79OGnMjTYg6ayfpDtGwRQXqrHdL5GJtgqAfD8I1qI-BNC9apZi2_y9tEHXN075p_FXJzwUUfx2rJ2TrUX0cKcr9BmaEBylJLbsAfr9llkIM3dSwIu939j-oZsmesdABOLJjHsscGqwPiNDBSRRCM61q10w=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/APuouOcxn7I_MAZNCyoSrMgQ6N8D58auPIyLKCUIPQNfkJF4F39NED9frPDMFO6tdOztPMdwrJqIfqfJkjRNnxqtjxi1zbjlwUmqoJQ0ou6TJxNfIstJzhPEHYvoojoY0pD2O_IzxQ-fUpZuzw9pNm-IPXTtvzp-VOC8_jf_Lqn3=s2048 "image_tooltip")

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

![alt_text](https://docs.google.com/docs-images-rt/APuouOf5oM8b3bkH-b6Ng4HFAqBYYUFx5duHKxhJbwYPKeUu5wGOzvuSHCw7ev1KaPCU3Ldq0AnLzb3i5seAECQqHI0Z1t2AW_hCwkrCXwA53TfHdUjBIvQHjC69owOGUKby-sZxW9p0sz4fieXRIUfPZDrljR7SkJFG4IxiSA=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1784185509453612&usg=AOvVaw3aRTGIseICtbpO8X3Du7-v)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/APuouOe409NjX7Ui-cxKfQ9z5MoOqwOr4VwIcdsdaj8ip906RM8MTIMTybNjVvPHyblWV9Xpm94dJaKom44xYnHx73a6Q3udQfaT_9MbrWFQPjBfQpi1tcaZXB51f0sQ8ZycyIlBvNtk6cP_1wIOp6dYyK0A7vj4qZL7qpYR1obNLw=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/APuouOeEEMvA7WCKcCvLq5EbWiE52vhghf0k4pSCUKuaw9RCk6dlzK9qFqTs-jCPaenbkE2uBHadyaa5rX-_scl6cqkzemGuSb-O2Uu1gc0due2jR6qHiG28A66P1BhjNls0mjKctIr00ixLeG4SYRlUw-joJxlNfhI1DVmTHsIa7A=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/APuouOeWgIXJt2IQUeg2Qnxzk2MZ3yndfaRdlXrBkZUXmqeu_0gpeG8Bxi4VAxrWXFTr08f-EypPOgVUSqo9fUUYAPhG_vEF1lj-B6scAFJMjrHrRzq4J3aHVS2-yHukgOlOgQZ68CxxWlh5L5JWICTnZfJo4vLpbjOq6QqMDbxlzg=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454627&usg=AOvVaw3ODKymUZqsIT3ByOMsu7kK)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454681&usg=AOvVaw0N79G989AtYf0TwGlVNzOj)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454814&usg=AOvVaw2GqQvW9GtBIyrhgGFQdadV)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454888&usg=AOvVaw2hOJpeKc6UfKskOsLG0fj-)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454923&usg=AOvVaw0XViuUFi8j8lkWa-BsfMX5)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509454959&usg=AOvVaw1vafnD80qQr9luaUtu9sIp)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185509455004&usg=AOvVaw0RtbORgdO9cN0eHNgFQLJ3)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455085&usg=AOvVaw0nFOfznoWWqJOX5Whzzhm7)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455125&usg=AOvVaw2HP83VKtAcw8PYyylVKoHm)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455219&usg=AOvVaw3kpE-Jhs4rysWVNw7Wghui)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455266&usg=AOvVaw3uDzrHHErdCgwmNUtX5RWz)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455301&usg=AOvVaw373OJ5uQCcjk2jzLD-TfuQ)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455335&usg=AOvVaw0jOj2yva_PMgpQArmAogOt)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185509455382&usg=AOvVaw3NVwm71EYBz5yKSNrigRXw)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185509455581&usg=AOvVaw3I_A0-XIhDE4AvlglgHYWv)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185509455623&usg=AOvVaw1kHuahHjaV2yo5LZiuv8XH)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185509455658&usg=AOvVaw1jnjuYpGEguqdvlfe7ROic)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185509455695&usg=AOvVaw3th-zAjjttTFu-htcoEsVy)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185509455745&usg=AOvVaw3xnquipKN7n_EezlYXxfcp)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509455825&usg=AOvVaw25o1TGBdHgxfOw2Dmgod-j)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509455868&usg=AOvVaw2D62ceZCW5axxvJHpDQF-W)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509455985&usg=AOvVaw0ivAEvXOsuXfKp5GjskCyq)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509456034&usg=AOvVaw0BPYxUf0o8sCOBL_u6NWvD)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509456078&usg=AOvVaw0iJsO28mqAilQrtPIxRoC8)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1784185509456145&usg=AOvVaw2Iir2vah0kdpWIU8CGBKPb)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185509456183&usg=AOvVaw0XIP5v6BVs6cv-UtPquamg)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456259&usg=AOvVaw2cb-pK3pEvoFJVQwCOXHOz)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456297&usg=AOvVaw2zgr9TrNBVmskO2TZtdjVT)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456392&usg=AOvVaw3MAaNkaCxAJGTQuKvyQDiF)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456435&usg=AOvVaw0gGn3WpWbiolsO1Hx46Z60)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456469&usg=AOvVaw1FogM6qPfQah3tvp9qhEev)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456506&usg=AOvVaw2cMGXEgg9ti1My8fjCDYF9)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185509456554&usg=AOvVaw0_cUlVFNvdVknAkQgzdPuk)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456632&usg=AOvVaw0Pg7Vdx3jjCACs3fkTgU0W)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456673&usg=AOvVaw1Oju6VkvkC-bDIUlQ-n5k_)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456773&usg=AOvVaw0DFw_m4N2nIxIKIBp4afEm)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456814&usg=AOvVaw3wl0MdTHbsAvBPVx5Qvehi)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456854&usg=AOvVaw1_GycPKhocEz7I2NnuOj60)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456891&usg=AOvVaw0yyG7s-kfbsre0cOsuFPqk)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185509456934&usg=AOvVaw1BsQYJe1khkFktr17kp6QQ)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457018&usg=AOvVaw0YUGcaEDuvPzHoaaOhPyAa)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457057&usg=AOvVaw0QcOJW3FFLBBGXOg1D13pB)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457190&usg=AOvVaw22jWHAx81l_N6w8jqbIjbL)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457229&usg=AOvVaw3JAKlOE8SWSQ13mhfCeEPh)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457263&usg=AOvVaw1Te1946v7vy5mgo3knFwAa)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457300&usg=AOvVaw0IGaVJrynA1ZWT9xNapqPc)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185509457365&usg=AOvVaw3I9494GgpwJ7IFLRyjNK4G)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457447&usg=AOvVaw36CbXWyhUqlgrtW6VhX3yk)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457487&usg=AOvVaw3i76lkW-ShlXte4Yo0L7JS)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457607&usg=AOvVaw165eA7xQoCJ_7KvtdXWM3e)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457650&usg=AOvVaw2LG1hTaHqj4HfXpQ-2_vNr)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457687&usg=AOvVaw2YdRbvS4f3ydgemqOkgoyG)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457722&usg=AOvVaw3fl2-Ah8_khK5wSGpseygx)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185509457771&usg=AOvVaw0TDg2CpieRNGUvTaP8Cl02)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185509458034&usg=AOvVaw3Tqeh-miFZ_Y_SZGoE25TY)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185509458083&usg=AOvVaw2VTdsN0rEVbzxdJIc9OPXc)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185509458128&usg=AOvVaw0bXIwW3PsqSaFiOVJ3GxEN)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1784185509458205&usg=AOvVaw2rhGywVEM7yLYTTVnoJCQs)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185509458244&usg=AOvVaw2-FGN2TZ9Q-mAojU4L1IaT)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185509458503&usg=AOvVaw0HEdNMT1VRp41wec3gH1vy)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185509458562&usg=AOvVaw1XsJzW8X7q5zArp4xs6EUb)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185509458600&usg=AOvVaw1Q2cUte-7N5P8H61PrzVbp)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185509458636&usg=AOvVaw2Yn7k4WCa50xbgYJMKSP--)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185509458677&usg=AOvVaw0IapvkAvAym7-f39Iu-O7i)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185509458871&usg=AOvVaw3AYANGjQiIa4OWJkDkWS_N)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185509458926&usg=AOvVaw1cJmD_40_pNlqJpIPv2amb)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185509458962&usg=AOvVaw0sCF5WGOfzoJ8V3mL3Czcl)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185509458998&usg=AOvVaw1bS9MUCNNpI6xRIBjk4Ron)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185509459044&usg=AOvVaw2VppACxGjfBJpRRuYYWwVt)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459126&usg=AOvVaw2wF45Oo8SZQRl_MqAk0jfL)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459167&usg=AOvVaw38FjjZCQDZZSY53fvcbv6g)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459246&usg=AOvVaw3YxxeL59iICilV27MOEe-m)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459287&usg=AOvVaw0s1QrZ5q2_tfCXxtKt-dCt)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459322&usg=AOvVaw34W1lbqULjroL7N8sxyxKN)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459358&usg=AOvVaw0FJDdhKNWyLYaELyhbisyP)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185509459408&usg=AOvVaw0a5rYr-yEU_t-nIjrHBHXW)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459489&usg=AOvVaw0d71dNcU8ou4WQ06512WaJ)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459527&usg=AOvVaw3F1wRqqWYzNBwuavMdhgnH)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459612&usg=AOvVaw3IyjE7RF2FXZj8ZzOKGsTf)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459666&usg=AOvVaw2SlGUf6xdCnK0V7xbtiLpb)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459704&usg=AOvVaw26-OJhfmeciMO6xFa6uYhS)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459739&usg=AOvVaw3Pz0SmJ0UgJLFf1J6u4NgB)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185509459787&usg=AOvVaw2O4W0X_gcT5x6rm2v73C-U)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185509460003&usg=AOvVaw2ncJ8NexaClvCGRnLIC8TQ)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185509460055&usg=AOvVaw0cI8X0HKQ5MSv85mL9uNUG)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185509460090&usg=AOvVaw2CoXLmlUSTkA95TPrBmxrU)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185509460128&usg=AOvVaw2AjFiMIiKBaAevEdCbpSAI)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185509460179&usg=AOvVaw2ODXoBiIO1vBKsFPD3rkeS)
