# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/AIpPl2D5BS24FODfYF7dVQX6x6NOTc73lqHDlCeVUrl2ZYN8DlyDkBHmHk0GtKJBrxKFtJH9cJv4uQ2gmV1mTJcKyfA7s_VTUxItQnc6COsVvFpxG8kxImhPn0NmppehNRMmb10lzuixR72sdsvWt14-YymH_woCYYmS718Esm8=s2048) 
     
![](https://docs.google.com/docs-images-rt/AIpPl2ADP3lksCDju05NfUqBs59CuUYU-INxaWj1kcINAMneX5zukqwAv6C7agBw79meNHObhC-mrQaVdkptcBaZY1JBT4z-dE0-fZxNnndncDN6XxwDPM0QemaP9fsa0m9KnyKz8DA1V85Ry9kVjfayeXcRiKbmgKvtwpSbbNc=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/AIpPl2D5pgaCj8kxHZe6O1ryda7xK4FbTU40d68jBpkqSWoU0XHYYAQdIOz4Qrbd-4RZzBBE5Qg8GGuw5bEhwx7PmH0LJB6a5p3ePjhz3cnYuiejvco6hXZglyVMb1JDXjFDf_jGiJHBEnx2f-Lh9pAjn1fzcNUhaHBNulr-z3s=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/AIpPl2DPz6YKKI7dCKKyLbll7982pzmdlrNO0nY3zJdMpo8yJ-lZLyKdpdObFwpUNsCfdhMUauL1Kg_8Vtfn40YwcECUAqGlkCYd3ffmG0KdsEkNxuhtnk_CA0kPk6aN0htlIF6GiOqXfRuWAJmDzlUGLsYMUZP0IViWdAx9Sb0=s2048)

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1795165526959514&usg=AOvVaw1TCO0yaQvz9m37BjSiRDQC))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1795165526959829&usg=AOvVaw2RlE8rc6q5vSrwnsAB2l6y),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1795165526959957&usg=AOvVaw1HM-2mD_TDJWkMP1KH3te7)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1795165526960226&usg=AOvVaw0Eu1K3NZTNdYaeC6przS-n),
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

![](https://docs.google.com/docs-images-rt/AIpPl2Cym9KG9MYqdpK7k2gNnlQyoOusZjAigThdvmv0zI-Eme7XEn1T1y7a-JRzmOhJkxoUoDv15sInLze-NG54S6y1OiEM_rdlODsupVojTC6GiUs-ScPSb4NO4A6zhLdbycmt4ljSYzkHSIKHy8CPoloIaeYqgu-I3QQ=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/AIpPl2DqIG2HJkHpQF78KwDp6C_tbaPJe4qJCcePPFbfmXuSH6YGoj80-qIKKwh-0CBOt7rEVsUK4VvDHx94r9_RJN_rZwEsY1NxVKjiUVqR5rF_US3gaMz9l25o7r8bQvnBigKWuBpzl9rM5HNHr3jOM7ot5s91bMKxy5ChlA=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/AIpPl2AsEQalZi2cC1pdUP8y9U_ikhxsJmEpyuR0fkp_Jm-Qm33EahULRxElXmwXTvTKXsvDw2nSzWRZGEAWF-IwHVKFLChk-lEje_MA9j9Q8U_3mBETGIC7UlJmkTKSX_DpcM660H9u47brunWqyrD6sN65BZi9BgCT09I=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1795165526961315&usg=AOvVaw1uaqFiMfu-4j6xHhJfCuyc) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/AIpPl2Bq1dr2uVqBOe8bPfiWknb0tgsnjbfNB1fIaw5QzXOx_2KFZ9A9doW38HZYf9pjuVXLdKy-qlCTowkOxpYBmYQ3Hf5a3lmEwrZ5hEkMYDzrJK1MJpUH8C9RADi0RR2D08vuGKJisSTIA5anhVK0Is3AFOnWqf84Cnw=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/AIpPl2DiHIWCN1XVnykvwsOnUOEZubvpG8eK79iiIQ1nx0x25C57u87hiPeS3oB9b1zKJn3vMGpR-T1q7tGxInUIMS9BxhXlo4X-7Fb2uH3UXsk5E-NhFYQlWF5bQaRsUsQtDoY2zcb-zpl5T8jcnB7LuWoG8mA94sq61tc9ILY=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/AIpPl2BunuM8B7dMvIrd89el2WnDg5Vaoy2GdSWq3IatppuN-WjRzpfBiNegKQmOXO5kW9SEI_Gvb_7ZPzMkZbSwqrekhI9W3xZ3OSx0bTq7kAcyDeh8zCWzq0j6Zmh6ZXXbRvXMN3OxNEFj82mfvHhemdxZfmMJaR_vXBwx=s2048)Figure x: Saving the measurement file. 

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

![](https://docs.google.com/docs-images-rt/AIpPl2CTG-P9fYhodcRRqEBzr5_q46IWX25h3ddmlZ1uUtWOzBK7DP8tiPHHTs3eIvhhtLCQhiWBHAzizlc3pD2bEWZpWrdGqDsovbeJ_lDpAVCJjj9TVioZWnVNNQlIvNwHQKTZvla7lRZhmMvhoF1jeOBWbXR_Zo0c9gAdi8g=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2DCi_DftTNaTcWKXcQplbtIW7WOrFf68ob-L7cm6prHDp5F6DzEK3Q8e7-IxQ_deD8knDx0p2pyuaI0yNsZ-PtVC9d4R_YtJmDo_3By3COMmeBbNuXK_ynGfQYlQSMDAP7jZVSpQVgOub1O_jwVd68sTPCR6EHnmQW8sw=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/AIpPl2AfdYUNJpUDGJOAH2q2e3HrzM_gJHcjQ01fWGb9pX4cBS8Ow1fMNCWUFuLIicKx2O96pG01NMo4rs4VOPI-UelvjRLv6uLnMmVN2U53ijCa3-CMdY9ySbHnQroj_ZB_s3Ngyt8CuRIJznuzwh57TAdXH8VdDDLhnJCVdQI=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/AIpPl2CBZcWm4qOR4Ve21eM7CI--Htx_Bv0qbNSJXU05CJZdkHzFIo5uxOH3Dh_3lR_bmax4Wn-yU_kJYofYtQyCW-dyokUQKNsgx5Gp4WEqRGSLHEhqC09ewLrca5_c58MSyjeIhoHij9y_FRNWY3aix76KeKW4Jx6D59t7Ng=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2CcZ6jrXNjmAXx1AMzdJ6sVBER3eaT4tn72GEDRlSPNjFamYNGHyXpXOKQHMd3bIDTHpp6kodsh6tBV0f_IcedLsYap_AmzNrz2aNxOr47YoQ4mMPaZB7lIymQkk83w2A44mdM3b9UpH19CP894sL11jx_U0CCCofa5cVM=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/AIpPl2CXFwQ0tB0w1-rwPX7kgK8AoQ74lQ4u9e4g5uehef9wM7AZ-0YBdcXx_e-sTjf3piGGIENjC4rJkBEI62PVQSNl4FOhGo28SZ0BX3DwA3lshU7mOqhj8p-4wsurE9nOqSfZg3Jk_m3Qlevg3y8UW7PdvKC5B9jxpmKzLg=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/AIpPl2BLRODjbsrFbtuWoAtm6Q5Pi6YonvzHhxhm_ZkqVlG84UWrA02GDoF6pN3UyY7j-car4VzKIqAsiWx9x9UYEWq0a-0b43K82Kxmm5OOuziynAKoWXQfELvpNpI-wnsZUqsYLYSkQ0aqv7wMHIQ1rzyP8JQ9F0gKbhGbDdY=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2ApvbguLMxpTN7TSmVRBydM8LUPgMxlcrhuVyvJtnbFfpr4tC1lZ6-gWfqqg1_knpJFhqn4CPGpdG-3mUG6RbKcjGg0h9DX_sg4B2m-NzKY5AaoUYiePpjGDYhZc_r8N8C8UCxOZk8iRTgAGV5v9CcdN1R5LztG2gcv=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2AJ20LYlDyHq2NIlhQbHJN5Kz9053mR_IAGGXNFw7lkBRLLwKdaAnAX3tdavH_9TmWdo6QOy8scVMGCeryoc_xwf7Sft-Drqv7_qSzW1kkJv-QuIdO4Qr_iBFShyUibDqqQujNvRTLHwXeTz1hMz4BQeg7A0lNFXC9Wu30=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2DV0Ry-MFe_2W9ulRIh83pvBKs7WH597NKybzsiyes4DdDfI498ApSznZf2s6_6_zqTTqYjNBPeQTb5UvEeOKC-34yiztoluLxQtd-igG3kyWVWdeRyKjqTkgJ8Vbmmxoo32FiEgxJaQS76288z0S4Sku6UGA18aerJ=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2Ac90P-Gw9mtda481S_AzFfJ0vfOqMfu_0BEk9QD1T_hBGJyrGWMSrXc7V2iFuPovS2ZmuI44QYo8OFmkoHhlEJ-rGPGvJ5R1J6SnxBOWmPGVE31pRtXpDJ-CKeNC4L6VzCADJlbFKN5lCf1Gc5LY3qU-c4Y3jrD8J6nGA=s2048)

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

![](https://docs.google.com/docs-images-rt/AIpPl2BXelZXsGzZ_yKvrAwTma4EFYr7tbOnhk9MKFa0A2ObMQdZGXAQTFxlm2FsBSm39ucMAYxM8Cm9wLj5wYzCsPPvFNMEKmmhNpN9HhZxoGphv63uoo1vPF2cI98i2fpL5E2hWn38WrmAoyF2AmHN0YRRrwsDQxziTtX2gz8=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/AIpPl2CgCmSg4nFWlSSZmGvv-U60MqZm_-W7L278Rk4Di_84P53Z_Si40zBTzeUZ-bJ5QdUy955sRsVEh7oF8wTs2FpNxj6ZMbsgJE4W_gWIf8LaLcJ76P303DCehNsyH44jsyKyO66qG6A2J1_4hfD8peDj1sfAY_DCReiRNdc=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1795165526965871&usg=AOvVaw1cudCTRgd-M1PgUKzBSFrt) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1795165526966014&usg=AOvVaw24JSiqoOXruBelLzraeU8p) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1795165526966353&usg=AOvVaw14xZcSjpudOja5BPfWOaY3) -
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

![](https://docs.google.com/docs-images-rt/AIpPl2D8ABZnIS7WDTWlQKMpvyTxG4YWksf0x1yEOXQfsPB_kXZsFkRJI5DoFITHGbuElZK8g9bVBUb4Uf6N2BAjI1ZPZboSjfoIqj1kW3jk0qSsg4Ay0hItFS0UyHTtAQL54J8EUyq37bzRNZ8P0pIHAJy3BO9GacsWMzGvnQ=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/AIpPl2CXFwQ0tB0w1-rwPX7kgK8AoQ74lQ4u9e4g5uehef9wM7AZ-0YBdcXx_e-sTjf3piGGIENjC4rJkBEI62PVQSNl4FOhGo28SZ0BX3DwA3lshU7mOqhj8p-4wsurE9nOqSfZg3Jk_m3Qlevg3y8UW7PdvKC5B9jxpmKzLg=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1795165526966943&usg=AOvVaw04xaEXU9hJU0jhm1PljBl2) -
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

![](https://docs.google.com/docs-images-rt/AIpPl2AOj5b-kJ_OjB9zETxefYexdh_gf1F9bqymRSkePgFLZIphTyw55xJrPVEDJ5KpbhUd2D97tgIy5602c1v1DEJ1zVbGUHHYuJG_MX3Em5CTYzBrv84pKtF2Nf6F6e1RUPoYmuWbhsiTfaO643-uGCOfO3RLmlbkrsBA314=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/AIpPl2CA7iYHObKXbpcyS24KMe7iPS3IIq5J_mPhzf0WsnVy-GGMJzPfEch7N1z1huD67jUfT6osGcUuvvZCONLnFyw9VcOQu2dIodG2et0YFcssaZLjoltLfkPMV-iwZDHFOHd8LqqLrp7AkwpOrKpwt5OvK5UcQPHowHu5Fzc=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/AIpPl2AGQirJ-uJPyplA0sWvG6kFKpmGxMjRiedKF6LPdWCAwHovF_guY0mTMAyzHLmyfhpluSLrisCzI7mVWsFcOvXlPkvMUsnItUETL885ua3_MDlcy8kJufheZI1vEO7Okt3ZTS1JMlCBypEjp0REneZq6oZ9_QXjO2MuPaA=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/AIpPl2Cu_rWJyD-YWZ3FkRhPhDUOyOQ0jYQFOXBRzsnSUzmzlmqqE2RBDgVmxI4usrzXBuISVTrJ-l2bVGDwx8juh8DYuR2DKxB1OvGDsmQ9_i9Yp5kA3kau9NYeaAzVFnwDatQvJRaNTOtWmjo6ZEtoTqQ7R0IAR2kHpvdqoA=s2048 "image_tooltip")

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

![alt_text](https://docs.google.com/docs-images-rt/AIpPl2CYf3vAiwhiRXKvMQwBuMov82o5j7GKJcAT3P62nElp86NJqtPzk324uvqjIrvrl1TpWHoM6S2yAHKV7Thi9xix2h06TR2wPN8MfXjkPBIZbX2UxbEDga-LL51bZ5xVeTjpO49T-TDmAyY5OZmnOUOlLCk9oxK9yEg=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1795165526969376&usg=AOvVaw1YNDuwm2DpgQ1x274-KdUt)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/AIpPl2A0Tqpa1r7cGMjlqnPkQpnTmPwHaGppEmNB0yP34TSHAzySE5mb-IMD7GshtEA8g1tnOGTAiDOBG6j-0jQbhAPPCl2YfMtwF_UnLFYLJ5ErL6945pN-m7LogDqRZqZXEC6PhB-OBinn8IA9llePo13vFBUOR136KXLOWa4=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/AIpPl2AehY0xa-zhKGFC5THjxxRzGLK-XCgWvcv0Rp_oPHReS5E0fT4k6g07klqVLW09BqPcNk46SuxmBacenKX3Y0h4Pjz7Z3-lOPwUuht4Z-UZwsrKVrAZNRrzJjaeI11f884rQdeyV1abiZ5XxFp_kCWwz4_SrU5-GxF15Lw=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/AIpPl2DWCmoGSxynaWwR68dgqhxS_14j8oMOuhNg6noBmFyxG9HSAjT9UoJcVRp9Wk93tZFvgiMddg1W1j_ZRZfiH-UpLXa6DLbE7oWpxoWYUvRLr2NHKk6qamkMbRy9-V7XF2eSOeiTDvCGNpJ_KikjtcqkblBku-s_aONq5ng=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526969973&usg=AOvVaw1aoji-K5Of9cCE_PwQOnJR)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970013&usg=AOvVaw3pJLtX3zmQw_CkoowUhTHI)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970054&usg=AOvVaw2lW_uIwOlcGQWQJ76XkN3H)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970087&usg=AOvVaw03I8dIeoMDVnKUy-Ww55Wa)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970121&usg=AOvVaw1dnKpwLKjMaLizCA-eW-tn)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970150&usg=AOvVaw3FxZqT3LuszjD32zMnv0Wy)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1795165526970181&usg=AOvVaw37GDP_1PtIfs8aEi1Sn42w)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970247&usg=AOvVaw1KYMaAXa_TJ39sVTsbGY40)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970277&usg=AOvVaw0f2bdFpiWK41l8LUKsLKzW)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970310&usg=AOvVaw25nQrgSyyqUAgHLi_dDhP_)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970340&usg=AOvVaw1m2DBCNyLcwbHhuh4QiLhw)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970368&usg=AOvVaw2aOkOhBGBX0mqxgEt0CL2h)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970425&usg=AOvVaw2-AJ_DzDw-AsUumZif0qXf)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1795165526970463&usg=AOvVaw0z7DfnvHQIhw-RcyPa1rsv)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1795165526970541&usg=AOvVaw28dRKP70YZJR1oEY2GobR9)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1795165526970573&usg=AOvVaw2O4TR0XO9WCE0srfPViypg)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1795165526970603&usg=AOvVaw1imZCAsUc0veQN4ymoXcv6)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1795165526970632&usg=AOvVaw273bVd0rFlCvqqkBHe5cvC)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1795165526970663&usg=AOvVaw2sc8JkW16k7tUYBShYW7ey)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970723&usg=AOvVaw3zegb_rzoFRISQCYSpE6lO)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970753&usg=AOvVaw2RZb7ysa_z0lX0NhcOCmKi)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970788&usg=AOvVaw0fZR7ZmAhzxz2gW9Dokt5d)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970819&usg=AOvVaw0WHQOhazOH5mqOLXBP8xtG)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970871&usg=AOvVaw20k7_dAoMqP8rnAUB194CW)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1795165526970911&usg=AOvVaw3xXSy9y9eOZLTWWzAqkiDV)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1795165526970943&usg=AOvVaw1iTLxziWbHMjbFhWa7c2BL)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971006&usg=AOvVaw0XNisMkCJmWNYEq83iiwoj)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971036&usg=AOvVaw0VKP4qSRQZL79fu7OjGv8a)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971070&usg=AOvVaw0Z31ofYwbnJrx_yZiEU5D-)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971105&usg=AOvVaw3NliGCiYJoU5FtrTuaGB8k)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971141&usg=AOvVaw3_DzOVy26nXWssy0V_Rvkd)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971172&usg=AOvVaw1DHr_IPIeNbPm1vWkHLPiZ)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1795165526971203&usg=AOvVaw1pPl5IX-Vl1_nlSf2uXROi)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971267&usg=AOvVaw0MEZsNkQ7EX8wjXEDl15db)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971296&usg=AOvVaw2n1-ga3VgXziyX34ifqEp2)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971331&usg=AOvVaw1M-jhGetYDiV3kauRpf2WN)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971361&usg=AOvVaw3T_vRs1_M1jNw3pn-yso7Q)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971389&usg=AOvVaw1vbC8lIcSy6bFkQ3SciMuR)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971418&usg=AOvVaw3ctC7gDKK-2wl5CAFNE7rm)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1795165526971448&usg=AOvVaw3AFPa4SkMkup5NL6iat1EV)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971514&usg=AOvVaw2WoxGFOgVvRmZMuGhupDGX)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971548&usg=AOvVaw1TkN90CASIEM754Rvu6uic)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971601&usg=AOvVaw0_VNC3YWGnZvDeYFHtfSFB)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971636&usg=AOvVaw2W2f6eCNfOP5SOTvOFt4Lk)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971667&usg=AOvVaw0TLj-6dDL5MPZhTyQOALDL)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971697&usg=AOvVaw3QxmIT62rLRBR4bCLqG-rK)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1795165526971731&usg=AOvVaw1qZ9CIl_LhSGyFR0UbAYnz)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971796&usg=AOvVaw1OJ2jau3c6A0hpG2yXB4wj)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971826&usg=AOvVaw2QNrYQZwfjyChlSNhDoac-)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971862&usg=AOvVaw15eJktCP3BNjBh8HCca_Co)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971891&usg=AOvVaw3BVNE0JlLipxKOanj1c2x2)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971952&usg=AOvVaw0xKg4d0mNATZVIoKcQMIEf)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526971981&usg=AOvVaw1-jTKoKmBfXBpnD8EwOPZ6)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1795165526972013&usg=AOvVaw3o8kTI_58SVZcbVAa61x-m)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1795165526972089&usg=AOvVaw2WUijxb3P4Z3wkR-LiPWXS)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1795165526972125&usg=AOvVaw2XpORe29Iff2JWwapfxa96)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1795165526972160&usg=AOvVaw1oHEHhksCAc1268it5axxn)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1795165526972195&usg=AOvVaw0ngdQyJ9F_2rTHlVSEFoGM)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1795165526972228&usg=AOvVaw2ISUA78pGmYB54pNcmiCOh)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1795165526972318&usg=AOvVaw3QnAzrW5Q2waNb0RgEiMEF)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1795165526972353&usg=AOvVaw2lZ2ME_-P58CfZp97G8svE)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1795165526972384&usg=AOvVaw1YBsVBL3rQa49n3pwWUfeE)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1795165526972416&usg=AOvVaw3w8XzZvI82lw9rHW62eF77)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1795165526972448&usg=AOvVaw1UiBWarnqY7K6A4ts5edp3)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1795165526972521&usg=AOvVaw2JMBJF3ZHdlKAiXBtc_SH4)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1795165526972552&usg=AOvVaw1OZyZN0H7yIQ0cscQ5c5HU)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1795165526972580&usg=AOvVaw1bU9EzBrjcV62SwpgTeb-U)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1795165526972609&usg=AOvVaw0Qim_a0muqfvYSx2pwOvWQ)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1795165526972639&usg=AOvVaw2bd0sQ2foWAmYjRDp5twA9)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972697&usg=AOvVaw3m-2BKjl6wCcDZ_CqlFea7)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972726&usg=AOvVaw0N1U2_vJlABDfInQVJbnW3)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972759&usg=AOvVaw1GB30BNge6fQBQZTZSikXY)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972787&usg=AOvVaw2aFXiQn2v5BMprTUtcc4Jy)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972816&usg=AOvVaw3bZD4V-bek16h9vEyn0gLT)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972844&usg=AOvVaw3pQW_-T_wy9nDxHgvXfVzT)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1795165526972874&usg=AOvVaw3ggJbEY_LnT_fC3Tb0XJpx)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526972955&usg=AOvVaw3waCX3xnr_sedxgeR0a0Fd)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526972985&usg=AOvVaw1HWMPWv-ftd3zNGzzd0bKy)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526973021&usg=AOvVaw0R1rBZlkjEENJKxjzw3ygg)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526973052&usg=AOvVaw20VQh5u84CfnSF4_FTHLJK)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526973082&usg=AOvVaw2OgVpbH7N-bjBZTgNHSDMj)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526973115&usg=AOvVaw0CQT5JJnRJVfe-Km0XTfdX)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1795165526973149&usg=AOvVaw0VZaRPaCId_sL04_HU0k8-)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1795165526973226&usg=AOvVaw0NUAu6mUISve5PBNqGzPkj)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1795165526973259&usg=AOvVaw2_oHIr9S3xaD-cp9UyWsoC)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1795165526973290&usg=AOvVaw3kkZlCyDs2DvwIdUFOB2qf)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1795165526973321&usg=AOvVaw0U96rioRVk3Ze3h5EyfWMf)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1795165526973352&usg=AOvVaw0FfMwx2h6TrUZDv6E1oPDp)
