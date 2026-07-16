# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/APuouOerFEdB1Ynh9Idrlg_Oa4KlaYOUsFYyi-sj7jESGHjzyshWgg1WcDSiDpdlmQvZlI_j8c7PUfZmF7qIAL3d791XIS6Bp4kaCIm9cZEFMzsgE6Jt9NZ8mT-C_QgQBX8nDvly7iVXwdiBCsECfxDx-7q_6bEDiyOINBRWjRHlIw=s2048) 
     
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185922240526&usg=AOvVaw2ODgS-W9JGwJzpJYDzZGGe))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1784185922241527&usg=AOvVaw2auOlwz41anjuN8nYmzdve),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185922242648&usg=AOvVaw3knPD1riJmVu10LwqtQ8aZ)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784185922244327&usg=AOvVaw2Xw9e6bcRQDTlFK8gO5P_D),
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

![alt_text](https://docs.google.com/docs-images-rt/APuouOeWaqErakPPBhW8iLaGJ3SaNufEKnQRkVMwdqBmArwVea1eCXCuDjsEz4mrz9cDTWvf0_qmCQUJUZ7Qq6hGwYQu9pYyDf6av9rp2SGQi43Abmvj2Akqm70Ovn6vbFHZb1wlOfluxBpBGMLSmCNOcwXHeP1rHRRIOhmfzwux=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOf8yG_mNrJazDnerUhzYx1gwr39DQcpDhZy5rGxi0_SziUh0RfaWvsyabp_R5oO0JwI4IRHKBYbrwTYc1Qft8qqrY5f45knUUt4IKu3VTZDOc61BKC6P7ww9SmrBhoDxuJE10IB_F9rhiw0gXunHk2e6jF1g8l0ZBf1dw=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185922250736&usg=AOvVaw1Nme_XwCGLt8URAHyIrnt9) -
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

![](https://docs.google.com/docs-images-rt/APuouOeYRvE9DTuywN7VG2tP2d05Rvsp728RRf3jPXUcv9DDYM4hsJMdZk1iyDwoUjiYGlMk-fqnok2S_WiGB4zEGott83ANJGqvDZrExF2H6Yu-X650w3JBlnPh3Oyvphv6_Rqh_xhhTXIAcpNeaW2kNEnGET6NKIcazK3ihnQ=s2048)

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
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185922271530&usg=AOvVaw0VckydGdlqnHIifXJcqQRj) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1784185922271917&usg=AOvVaw0H468uYRh4A1Q6m2zJM9Dz) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185922273018&usg=AOvVaw39_fn9Vwxvr73BPW5Ua2wj) -
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784185922274888&usg=AOvVaw2sKPH37yKqbJ2guIIIrRJA) -
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

![alt_text](https://docs.google.com/docs-images-rt/APuouOfVFNNLuEa265VqZB7Tmd-HDKWpvDjnFeziOJwPEH0F5-gE1s5csX6p1ZgMnIuv3R5OrDGJV_TMGV7qZ0QYifj51XD8f9cEpBshmTIRuvTqD10vPhH9SK_yRWnyzabF1FLKpMLxXI2Se2uE7N7key29JtJz3ekW7UDBX--0=s2048 "image_tooltip")

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
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1784185922282108&usg=AOvVaw1D-Sjm5_CCq59tAf-ddcaM)’).
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
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283462&usg=AOvVaw0d3pacE9E7ZMYQ5rq6Jefi)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283534&usg=AOvVaw2q7SX3K15vyxpF7F6mUvgp)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283717&usg=AOvVaw2UP5Yfsc079IIIVQHQf7Sb)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283806&usg=AOvVaw0elAmzMYcl74OAOBeKJVck)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283856&usg=AOvVaw1rYXJ5XHibtXgnnpaBbMsg)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283925&usg=AOvVaw3AR09AuFpjvlQV6UyyHCqD)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784185922283992&usg=AOvVaw3gwP0WUP1aZoH827JbR6ou)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284115&usg=AOvVaw1wCLEVhtly0OH8iTAusjHt)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284189&usg=AOvVaw31J8TPNI1_AIjhVr4VGnG9)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284319&usg=AOvVaw2eaPkXdUxDlOf4dsL6h6KE)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284386&usg=AOvVaw1fKFrlumi369rY9FZRcTRu)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284433&usg=AOvVaw3xAYTP8skDITRcIEydTncG)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284484&usg=AOvVaw2wYsCg1kWY_WzcwGZXz8tG)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784185922284548&usg=AOvVaw27G-P3o3ANWC8cjir0HlkG)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185922284820&usg=AOvVaw23GqSBkK3uGTt40BEacCrT)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185922284885&usg=AOvVaw0pPuPJjyXzcAOVATe_B-0W)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185922284937&usg=AOvVaw3O-H2aSxFhKyYROqY2946y)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185922284989&usg=AOvVaw3sjj4qbHSN-sfqbxyPMRx_)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784185922285058&usg=AOvVaw3Wt2zMmyHibE3EjMKEFjF6)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285169&usg=AOvVaw3o7VEbfrWS7nNg5WTF9pDW)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285225&usg=AOvVaw3ZaxRbXex_xVwf_bZNLnc5)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285383&usg=AOvVaw2haZNdFOhkdgxu9nnEZVb5)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285450&usg=AOvVaw03y30L7wrUJGlRSJc2_g0y)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285509&usg=AOvVaw0B5fsSoBpU6p-dcDwJWJeT)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1784185922285597&usg=AOvVaw2sqmOjvtZVsYy4r0K0_R3F)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784185922285650&usg=AOvVaw3WlzcrXqbbkOKSV-syENHG)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922285756&usg=AOvVaw1f9K_jORe2t1F5dPsRGexf)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922285811&usg=AOvVaw2VmcPzF3chbwTjr3-drie5)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922285952&usg=AOvVaw3WT5qPTmKj_gVQJgwbBBGN)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922286015&usg=AOvVaw3dfXTn6RLmcwybXIXt4HPX)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922286065&usg=AOvVaw1XC0nNBcwWv6cu8RCcGQ43)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922286133&usg=AOvVaw2H4JC9AUbZNR1dPFIjqhTK)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784185922286205&usg=AOvVaw3WFVLPkge1j9Y8LvPBix23)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286318&usg=AOvVaw0jrMf3u8yI0JMznBInisN9)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286374&usg=AOvVaw3Cm33DesuXqVCSzkFqFGH9)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286513&usg=AOvVaw0lWeo1LPzOXpKusUMxnmYn)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286570&usg=AOvVaw0GsiIy35_m13cLCGTvs35-)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286620&usg=AOvVaw07kJbqYzgNLcrN1X6UpSqg)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286668&usg=AOvVaw0FwbA1VIr0oWRSteZNBzkp)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784185922286729&usg=AOvVaw2csuY-myZyq8JfbSnq9Lqd)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922286839&usg=AOvVaw2vppmSjQpDPVeJyE_Lb4d3)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922286898&usg=AOvVaw3P2uviqaisSgxpOpc1oVnU)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922287077&usg=AOvVaw1oN6-Waz3F3sG_Lc-aby9I)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922287131&usg=AOvVaw17on5TKdA-XyF-lu9eM8Z3)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922287182&usg=AOvVaw3fKaLe3r002cHkoasEl8AA)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922287233&usg=AOvVaw1wSuzrKrcjPENMk0Xgt6JP)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784185922287301&usg=AOvVaw3hsPtTLilzNZ-frElj5iLt)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287416&usg=AOvVaw0j9X1WFKuMPPLs0cTPBRVT)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287471&usg=AOvVaw1CNLhcUlKt54nZqxz8cPxu)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287630&usg=AOvVaw0MNgTohlS9HVqq5EhP2XUb)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287685&usg=AOvVaw25gyrPCVxFRqm9u2Qu5PAj)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287733&usg=AOvVaw2hp_kd7s75HnxHsgQf4vp8)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287784&usg=AOvVaw1Wk7Ug1kdNqN1s7aMfFloZ)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784185922287868&usg=AOvVaw2yRBbOZ22kzXVDGvxgbf1d)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185922288520&usg=AOvVaw1M-grRRkL1gAV9t5jtU7rB)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185922288651&usg=AOvVaw3F8Fr59BFEZDLRCXkStr4n)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185922288757&usg=AOvVaw1n2_SuCdnMK09uaAHXWcWv)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1784185922288906&usg=AOvVaw0tXbac2WhTVDG_Kws5AhyQ)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784185922288994&usg=AOvVaw3QsNoRwW35xK0olve_w9bQ)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185922289594&usg=AOvVaw2YFQnplEkdJVHLbk_CFNuO)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185922289702&usg=AOvVaw3Ea3B0XBR8L_ubYvAA3qW8)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185922289787&usg=AOvVaw2Jpw1C28AFT4LZOXm6ajFE)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185922289877&usg=AOvVaw29mjJXPHEaz1S71ayIHZ8T)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784185922289973&usg=AOvVaw3L-6P6hMkP56B_pfYOTCas)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185922290421&usg=AOvVaw32zhpg5EbmpC-XJDSEPbJ_)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185922290553&usg=AOvVaw0-CdPr2X-yu-D7zVLmD26v)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185922290640&usg=AOvVaw2jhDW0Y6vOfUxkZiFUOr8v)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185922290728&usg=AOvVaw0cy-WL6UwUXkzA5m8UOJsT)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784185922290845&usg=AOvVaw1Nm7V2YcQdbf2E95O9wJix)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291067&usg=AOvVaw23DssOReY7-BmymiMatPLU)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291168&usg=AOvVaw0ef_-_1vCN6IkozG4emT98)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291360&usg=AOvVaw1a7H9W-zcSkYpOW8bmOYa-)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291457&usg=AOvVaw2UVOzNzCDM7yNIYIL2JCXu)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291588&usg=AOvVaw3-RIm2X3pM4xmIG9vb6ykB)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291686&usg=AOvVaw01EzcO8IheDlByOUWSW4I4)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784185922291806&usg=AOvVaw3bJtOD4v8c3HkbyAqfdO-9)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292021&usg=AOvVaw3LcAxr98hgGasw95DIXDT-)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292123&usg=AOvVaw3bq2rkXNa-IYR-Boy8eRwN)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292351&usg=AOvVaw3U4Nszng6idDe-Fm58mduP)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292456&usg=AOvVaw1A_cEFlQq8LPlwRKUO56Vl)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292546&usg=AOvVaw0arPCRJTgQA02XXA37miPF)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292638&usg=AOvVaw3sLsi9HpTI1_Z-iIAhHpZj)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784185922292756&usg=AOvVaw189IgAyweiBduB_PoFqYKu)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185922293294&usg=AOvVaw1vyYjCHBbdPdxRwTmzW6Kk)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185922293418&usg=AOvVaw04iCH3Ee0Jjhtq7WpF5jpt)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185922293507&usg=AOvVaw1_9WvWQYabC2Xjv3htLzjh)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185922293596&usg=AOvVaw2J0k0HUNxDETwq9qlBDM7z)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784185922293715&usg=AOvVaw1nRhjOzm8DvEP5j8QcSGXy)
