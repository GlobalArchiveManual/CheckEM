# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/APuouOe2pBpj9R6vlA47mWIYmT5OkPoFKEsYIFS3HPSTEoI2Edwpt6yxHPBiaGVD8mE3SqXpyzBC106rSi_YaA3DPPE3hTbsNeviCEhf6-iBdobA_RqBlojabEsALJf0ZW4btqAujaolU16zHYWk6wGWgHUtAkqiNJD_OQMyjh_JRg=s2048) 
     
![](https://docs.google.com/docs-images-rt/APuouOfq_XebdkTfggATvgADZGkHzhbYAOIjejgnjS1Ayx8YF-90GH3IxqFrPJPk8TwVDU66nE69t5eMh9wkc6xfWELqn-FVPwZDneq5cHjCBQAOb8T9v81TxLzbobhwPXSPye4Ti7FXrI_hAbb_i9wA97FYoyVNtOzL5uuEkrnfFQ=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/APuouOd5iEmJdkrpzQBJA-lL9uL11t2ZDeGxX0-fzlHrG4k8OgS7YocEQiLqxACYff4zwU4-HyWW3_2f4YFeRI99-vRvZ_HwQLy8kVRJN5QzhnHbdyfzGugdMRfJ56yU9urp0Sjv9_XJAjOU2P49FpmXTk-LVdFRZBmS-lGzUc4-vg=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/APuouOcBMf__GyUvKT8clwsp7ixiO971wwpO_advtbvVUxyTnJjmql23tG9lKoe9lqSXNJVTufEbZQ71D8xHMRimua48qilKnzZS6T8-gMD3SHyDG-6PDvNm-mPMHGAA847xrWU85uYc8w7f3BwpsyYUUQuoFJEwSPvtnUCtvBM9iw=s2048)

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784100604527038&usg=AOvVaw0SZLv_L_aRcBZIzj2d_VyS))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1784100604527792&usg=AOvVaw2FCSSZMOQ7_c5HbztdoMWR),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784100604528362&usg=AOvVaw1SRqubdh9YYtw36h9MQ2I7)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784100604529133&usg=AOvVaw1Z5udyQWkv2-YrbrdyPuDF),
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

![](https://docs.google.com/docs-images-rt/APuouOc7X_y3jfDlI3tU4h6NjM11iK86OkXHddDbu5rRwWGurfCVtW9Do0magu7vdRoMXwQwAbiSu6OLkeGUAQTSAPg4TKczCoNogyWcI34Yf_DwlIQm9UFCpguY9yD8gHKwGDKBVhtFT5n1jRfVFl7LOZ_nz9vpg_0-mT0qTA=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOfYMR_bxnZKsEcE4B_9yYBzDPeqsU6QDrIb85oGufN71iONoGqQkgBhoWoQDvKF7X81lrPGAMcxO2ZQ1NYOv29_McmhCG-OCy-y1m4nJGNPvxgjH53R9JJI8FirZhItogCr8qoV4X2IFNJNiUv_R3PHTJlctt3mcK7K_Ccu=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOcrxH3dO6o1VU3MZETqc5lpuSRT-1cgoEohSRTDe0tQqyMfETFqloRFvrINj-DHY18CTCIUBVnCO_gbTt3-RhCFQ7UrjoyahA7OFR-8IoH0Vevg3Q40TuVLnXquJub4ht303xwGCsSylVaiiL2lVvGZ-PJOTvGtg6M7XQ=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784100604532083&usg=AOvVaw1B6SG9eDU75R4FK9QsICdN) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/APuouOcY5LPaZoUcVnOC7lT1XXeS4S7f3Ji0-LAbVrxNk5j8Gd7SWvEy1JZu7Jx-JUI_am0pIXqEUWQWpzsH6bqdZS9jXvsjePG4yiL1YHOqYL3bsQed8kLMdpG0QsEwJAfEPkG5Cmigufx_gtmbzNn7q_s4C6fU48lCSfv3Qg=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/APuouOdx9PoT_0xvr12JZ2KYBaAP9EXL-Tm-IQZ10ggwLINynj1M9s9ZZrUD94k63PORDxU8SiX2dDAfPVuhUshxwUHcvsHS1KE5vhkOpZeeHlSrTARCJrOdGX5XHsrXinkpX5y8ej0Y4DqhoPHrjghQZ1tRhCb8AaGCsDqjuD1oCw=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/APuouOfkmZX4IPJ1gWP-rkvxsLk_BpxESUCdd_h4RW5JHUcKm9R4dE9IWWUAXjo_RO8MOSeZXLud7epgY5XSFnd9DSXAYVm6RMxTbpdH9PFEt-XQBjgbFIAktKlw_q-tgmJ5t5s25pPbglbphlgcDW6bCGWWGlpd0VVL_eh2LyY=s2048)Figure x: Saving the measurement file. 

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

![](https://docs.google.com/docs-images-rt/APuouOfbSXpp8yl5MJxMuoD2s_9Y208PLX8TglQou7hg_cDtXVG8L6oHY1DB5xIjX4fBmHujIXfVAc4lCqdliZ9N226DB9p2UGw-2Caq8nu0NeLSSrrC9dz6hyT2go0hQRZrKHRFWINM7Oujnb0QW-Y3xF7pW45htN7v-BIzKPE4xQ=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOegT1SUiNej3KY7-opZtKfXEotw_tDHAz4lMnoruYDhbOBbFsRix-DRVNVVG0Z9EBxm0R1j6AvkgB8hrTXU8yyjHsc6QJHphdtDATA0v8y7tcOgVHjajwzjwTJ_bdezgAsHoii2GT47szdqLGDWrFrfGk8-pIapRfock4j2=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOfXmy8uBNO1zb5g6KDi70Rc_S4QF8MVwzyMvodGIgnKimdyisjpWzBnFfJ1-KbEhgWce42aJ7HL_AzlYZL2EK4Ta61uPET1YXLMpeD5shji80Dusqk6D6XWOC5smJn9rgem9kddeoj95vMD7Rov4UsNi_gT2AYSA_Xd5EAXcw=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/APuouOekZoDieJflNQtZ0p8DisVtg0BcVIjtxTeJhEZy4OWaVQI4m0CIKTTG95ZkmnzJAsOFok7sO0GUXhnOB61gp5KDpwNhFgDEmmLzV3dNrDCdNZSVXil6nd9NBQlrtRKGDaQTwAbjJbYivdjVI-96BgCsp0dDYyZl1BUJY2zK=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdRPrD2H4ANDPGCj-IfepRoP0PcPvJ0QVWgrdtKLwOxyW4AjkKI9XRz4lnwJRhwtPjQggJZ3qp25HXWAGZ1s4SNb45MHWgbgbJRjtwqXM0IhxEJdIcj_AE5vGVLLeuJjqg8pwz0YmtDaNokypTyYMEXoN_OZLt3K5uwHCnXDA=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/APuouOduH6gUfCCC5Qc-tt0I0sh_3TgmRnSZ5IntnStz95t5N2POVq6kHB7WtW4YOvNPoaDu0WYj14f5j95CddOYK_qmQe9dPLcqHCCCvWmITy-7tGVbXYDBwOEOFTfalEGMdGbAT_M1PzJ5K4fQ_5KLK1iNDGcmvxDt9Cj685s3=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOcr1FTsM-NFxVs_1ccRS8r_mFNMk-c_cC2FvAMsrAYbWwGEJU55-twVAyD4rXxBHJCF85taGrJmtl5NbLaju3v8--sqIIaF295O3wcCu4ZS5QFoIvwAymAUgs9lC0NFmtBq79I_Ni8GRXm9Aukde7kMyaPjd4oTQ6G4jrqpmw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdkV-2hWXC1idjbfiJwH5h0ibeNK8salbxUHZNChXXk57hTD0PxP78u1-mmYtb1gzKCR6tOW9TpXsbX6G8ASnv_yF4SZjTH4_HJAJ3M1ksGJaj9oInYmHUt2YpGchTFmzI2Ae0LEJYR1ptGPA_ukgFdFDCS6hsP7Spwdys=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOfnHT_8psdvzK7-2499wVQH7_DV_SgjxcHCIc425Ez-LjqK-XDcx5yVAPSInbxWXrXFQKpBi9oK64lCmIPxAmMufVck19bV6Bmsu_h_GtkN-boeyRV65i_jPRB4Ba1RxlCn5Ryk3WunxtIkWpEehB9FXymz8WVUb15nWqFPlw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdkp8YsyUdU24H_Kt0FtgjJUun9vWwJ_AkyGY2FNVH34h336DwRcOha3Wo8V_ML1btLjyUvgvDirt9K8AkH9qBb2yXeUETDQBeQGq43Ukh9logAdhcDIJFwbeyNy7PMwfft8bWhEvKA0GHgxPMlXTx9shPaDsulGprnoDw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdGSSEIpaE2TPXL5yZLkx5LKf3KR-Q5OaMLZCCjqvPR6lMZXKQ0S2iBld_1GZ68r8p8CKFLainFGu3a1ShpshBqI4T_lqPvaNrTPnUzvMoThQlA68iQhmKMlaETbvkQjsqjv23difD0troi_DrsIJgCPlqle1e08SNgB8Kfkw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdtOqqCLqU7mbqEPTPWP49PcADsWRK5-K3HyhcFgKAQf8YcVGo_ZcogtRlb6gQ_HrWxUQSbjjw-HHSHQVUL1uWznkbgJoxyElUxgzL8ON4FhYPJRzfzHFzPOR4caY8bVJqi1TE6in_MM1HxP0F8PgjS27K4-gOkW7H5F05y1A=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/APuouOe6f5VOYSoRYL-t1qW1LwmdNYIFy68mLGeQ9hZknqtX_pIGK3NrZmCvqoqd-CAyeuf4W7te6TkJdtpre030D51uplyGOxnEsU2XHkq5OkkjN3UC7XZ8Bx3OCyelC20goYWguHhNnwcK5i1PfQuGmPRDZ-lrHPcPVzYgQt7Wfg=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784100604543426&usg=AOvVaw0w_DLvLXwiBGW7wP_PP750) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1784100604543604&usg=AOvVaw39Fo9OsCI9jpB2uddDqalk) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784100604544441&usg=AOvVaw1eQYbWHpMWosMQS1PC3hZT) -
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

![](https://docs.google.com/docs-images-rt/APuouOdZE3lUUQ3Y7P2JLyPIXpaz7V4Ki6lKR9Qe-yIfFWVmPTkhoSa3RjM88krNqpX6SjSJgbzEbh69u5X1fEt5S9JDY-tV6tBnHsBHyLR2_nS4bYsJJcPFBvLgt6Y2NpxLVcCTAMija94YsJFIqnfPYBdcl1xBYF-Hhg2JTL1r=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/APuouOduH6gUfCCC5Qc-tt0I0sh_3TgmRnSZ5IntnStz95t5N2POVq6kHB7WtW4YOvNPoaDu0WYj14f5j95CddOYK_qmQe9dPLcqHCCCvWmITy-7tGVbXYDBwOEOFTfalEGMdGbAT_M1PzJ5K4fQ_5KLK1iNDGcmvxDt9Cj685s3=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784100604545867&usg=AOvVaw07OQNqU59vl4eksV6E1mDh) -
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

![](https://docs.google.com/docs-images-rt/APuouOep8lp62Nb7cSACtt8XtQVcaSqU53vaeuLy153UJuvSfHtyfp4z-56hxop8Ik0l7KO7rh8gCBklHRhXJBvTbYDKVkIPkWKqd9Kl8MiQtSjoQ6Qpq_IE73EfAY2zTn43SgKGSAyqXtPlgCC0MiQfXDhL2GQw57pHJgGd_17-nw=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/APuouOcSXtQJVOJgVdviISjz0ZZEmzS-5Far7-PBidOTFOwe5jFdXM05Eck1h-IHZcvw9DxCoxxrIQWHTS2OlZY1R1QKp_SDHqgjKoIsIVk4lE3k465nbrvPuT0zgi73yNIy36SGs17orOUCXqs7PsFfqCvSOLCFZPwHN91oEyGrMQ=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/APuouOcZ8f3OhLsZd7uRIBNIpBen67NVV0Hy-35gsy6Taiw_x_kVyI5fpY7k00AfsRpixHZlUzesq99cmi4i87mpz3zrRZSqms4U_QUdXylUaclenHOVu3bNHYR2dGXJKikAa0r2MLf1YFxD-KhMwmlqaJTOej9Atwloxa8vzHEPQA=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/APuouOeXaLlDUMlDHdWzUCeL8gp-2ABM1QGKYTqnGn4PvpPDoHkR9t3RgT8KpNOjAwSe1gu71qyHaDbha9qs3hQhq4-o8S4bjKSveGFdKU55avKn6OVCh0OWNbqPtMFJeS32JFzA-yab06C9dLPFoObIxJ9Krs2E54qlrYGk9E_X=s2048 "image_tooltip")

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

![alt_text](https://docs.google.com/docs-images-rt/APuouOc96PFouLnjDnG7h9dHkhd1LNHCKaTcnJfXQ94td_y2IxKVku5V0ErB7tQg7f5RRvi0ej2MaWJbEIhzuB3mKElQXCrMLHgL0UnNBObfe3oIgQF62P_3RFM8-nhbSwnsL_T51U3Ogq5F3t0vNlpX4Gy8skkehQiY38t46g=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1784100604550813&usg=AOvVaw0EF-xxA1HTLHB5wSxFWGS3)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/APuouOcli7_1Dvy5RJBLK2ExQNlRLibaK46YqMBLpNwNMJ4yXRTLr3tEXXISvBqi-PbD8Ac-9urKgu34wTctZedmUNOPUxfcP-3MqgA_8IANoiKWZA1e7m5tqBebP-Y-AUUC7gbSGcyaoF3LZ1f4MB8mCn7a83XyJzXsTYMfECcFVw=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/APuouOddgWLqQ9JWQCbv907g5I8Wcx3SAq-iZjUG9Z2E_bB4CqfazbbpMqvf7FZ-_mhfj4SxcP9gM67GgIk9x9qiwyUrmlTgrBKlelATG-UN3Ac2WvuKk2Dxk_pnJCL1a1B_z3b6E1pQ-GJ9S4bd6Z2jSc74JVoInzhKHovADxaGoA=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/APuouOd_uSm8gG-5hgHb8_qVEuiymwkSWxbU4Hj8rpUMzKPnBvwkrI8sIeDAth9iM2SjowPdk74AtL2PEo01B_LANUBBSOtOSi5VeoLaCn5ZyRYTfQWJruOo5beId_lNODLQ9-p4dAkDHq5_W31CdcNEK9Wpr5ofZx70RtQVkzZ3gg=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604551864&usg=AOvVaw1zwxlAb4LZkRi2Vy3vbPOs)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604551918&usg=AOvVaw0eZ3zxBEtymB5wlAa-6XRG)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604552060&usg=AOvVaw0ozugsLfVLa4DBMxzW0Wvd)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604552127&usg=AOvVaw3_LhHJsUBxsfwkya91DeTr)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604552167&usg=AOvVaw25zNiNTd1GDdbDUx8LvDS8)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604552208&usg=AOvVaw0CCZuIKkYKcW6TCcRyi4e-)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784100604552258&usg=AOvVaw0Ml6kyPSLUiSCgoXasEXnf)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552349&usg=AOvVaw0QNhlM1LTR8uBRrcNp5cs6)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552392&usg=AOvVaw3biuCOwLXb4J-GXAOmTxS7)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552492&usg=AOvVaw1kbVTHAvG4vad6-vxfm0kz)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552542&usg=AOvVaw2t3mzEdxdCiDgzWWbOHRxY)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552580&usg=AOvVaw3H30jynEve1M014AW9LM8h)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552644&usg=AOvVaw0rpi431ojlR-PbYm23N8ED)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784100604552698&usg=AOvVaw1QwxhdxKnx-K_r_phJd6p6)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784100604552908&usg=AOvVaw0RUlFDXNTzFhfYxLz8jzLQ)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784100604552954&usg=AOvVaw1WprXhHNPo12XyBScsTTgV)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784100604552992&usg=AOvVaw2JzVF7sPG8SfFa8aBpM2ei)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784100604553030&usg=AOvVaw0sZvM2VrDRw0Obn9EFISni)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784100604553083&usg=AOvVaw3aWiuJEpn47enyzyLHt2Qa)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553166&usg=AOvVaw0qD947QWaoAaeZMVZGVBLs)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553210&usg=AOvVaw3uBy7ejjlUn4odulQ373ux)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553346&usg=AOvVaw2xZAjjFSyONJ9MHkvcngdj)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553401&usg=AOvVaw1fpb_JaER-2orTqiBp0mzW)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553449&usg=AOvVaw02FMvRqVhAuni1Vt97JsP9)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1784100604553529&usg=AOvVaw0pbvx7Ii_zrBdl5cK0nw3j)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784100604553570&usg=AOvVaw2yim4BoBupM4X0aSvEPkcQ)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553652&usg=AOvVaw0Q4FmU3rC5Jr9tgPU-obRs)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553694&usg=AOvVaw3X0da1jkqL4_enkiJq75W8)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553793&usg=AOvVaw33evxuQF4bWuowx9m11H18)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553840&usg=AOvVaw2aw1LaxYmqMNhD35N7mUbc)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553880&usg=AOvVaw13ZLaz0c41tV2o_ITxx_Fz)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553921&usg=AOvVaw2gU63vq3vI_dIF4xmrfur0)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784100604553972&usg=AOvVaw1VvrD0X00lArehhJglUplM)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554057&usg=AOvVaw03qZhDwpNtfRdjG8KznPj4)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554100&usg=AOvVaw3J4hxlo1YL8Vj9gD8yv4N3)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554205&usg=AOvVaw1BCUXAwENKXqPDVJ0LMa43)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554250&usg=AOvVaw29ERE5grMj_J1inAKbT-RE)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554288&usg=AOvVaw1fJV42HCyKJiugSGV5kbo2)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554327&usg=AOvVaw379fGJgAKw8_25BLxKbnn2)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784100604554379&usg=AOvVaw2mv1_vyQ_y3ViloDMRLbUF)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554466&usg=AOvVaw2j04HBAKTmnGYnQXq9faaF)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554510&usg=AOvVaw0ypB4KDrlEvcwSFhm4V1Xn)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554649&usg=AOvVaw0XyEe3Qy2ikMtJIDWmZQZ2)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554692&usg=AOvVaw2JQnOwB9QVQZ0CHlO9AUs-)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554731&usg=AOvVaw0lEba8ggfCJJNI281Y0Cb7)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554773&usg=AOvVaw20bOkgDHaYXiDL5mBj6vQA)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784100604554827&usg=AOvVaw1GjsazbNbRq_5j3uUtpwm0)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604554916&usg=AOvVaw38SrWndYEa6jvL5fFwLJgp)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604554958&usg=AOvVaw3q0q0cXncs4Vcye5yMZtan)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604555080&usg=AOvVaw2kcDmPxStoVTLJ75cOJvPd)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604555124&usg=AOvVaw0BNdi53zSU5DxSKgDEfMuR)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604555162&usg=AOvVaw2eSQsvZS-uAFcvO8iyOYT1)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604555200&usg=AOvVaw1c4AN3_rsEMfmzKolkWT7i)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784100604555252&usg=AOvVaw1HuusjSIitJa4b91fg3XqM)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784100604555508&usg=AOvVaw2CzTsp__jUD4dZdUv-qRYx)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784100604555558&usg=AOvVaw2zDgKo8Esm2MmACD_PCPAb)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784100604555606&usg=AOvVaw0nwGsXGdsk7Odfg__moiFJ)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1784100604555669&usg=AOvVaw1lyZOozS_6VgfrOb8leFlQ)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784100604555709&usg=AOvVaw12f9YYrhv3v37GKpSEWGZ0)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784100604555941&usg=AOvVaw1ukeGhFuXoIRBxQdvgSTRt)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784100604555986&usg=AOvVaw2o2UnOYLDB69MFBHLP5-Cy)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784100604556025&usg=AOvVaw1MFJ8A4gt5mM7Kadraf8oj)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784100604556065&usg=AOvVaw3g4z_hiKZtUl536sV4XD6n)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784100604556111&usg=AOvVaw2APn3xh6YQclbeEmbfsez6)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784100604556309&usg=AOvVaw3XcifdjlF7W5IcfO7zSr5_)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784100604556370&usg=AOvVaw0eKQppNIn_O-JQud7SetF7)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784100604556409&usg=AOvVaw3GvR6QiZM0EmAlp7g5dLtO)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784100604556449&usg=AOvVaw2pCbel5EsSO5Tt_CsDiCuI)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784100604556499&usg=AOvVaw3s2Dq01_HVnXRmMup1cMp-)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556585&usg=AOvVaw1oJd1PZ1jHRJZ2Ehek2dI0)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556629&usg=AOvVaw0bz4vJUv2slEwBDDbiqU25)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556714&usg=AOvVaw1ACUnfw9tjdUyVnx6m3SAS)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556757&usg=AOvVaw2_86Aty57M6up4K9fuHMx6)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556795&usg=AOvVaw370EWGo0oSuo1PVq0q7ry6)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556834&usg=AOvVaw3I4_iqcMTG3fy8nZFGDpWR)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784100604556887&usg=AOvVaw1tdMWnrgFveDG390XZlAyp)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604556972&usg=AOvVaw0Itx9NSm5LWvkX49DoH7Qn)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557014&usg=AOvVaw2lPEcrbEVzZsqHSDv8x54I)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557104&usg=AOvVaw1tUmv3-BQdI4PWNPfhM4gY)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557147&usg=AOvVaw3jsKRsLHkxVM-ql6uoYHGA)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557186&usg=AOvVaw1vh_pAoQORBbow-Wbs1fJQ)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557224&usg=AOvVaw04HVGdmrdTcvIfhc0lg0HM)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784100604557282&usg=AOvVaw2XOWwkTm-tOBYN0y4-DAgi)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784100604557502&usg=AOvVaw3qcbpBYSTejCLgjKuZ1pNz)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784100604557590&usg=AOvVaw0XvYyKVO__4Qp8DnbrV8H1)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784100604557631&usg=AOvVaw0PAkAKt7WL2a36wz7v5mLK)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784100604557672&usg=AOvVaw1sfX0J4fWGBecRaDF-cfH-)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784100604557726&usg=AOvVaw1qjNG-_XWrEdDm3OEI9kqJ)
