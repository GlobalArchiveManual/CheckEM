# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/APuouOePiMiA5o5Qs3vTNNlt4xjpNrD9xCnhVYTMKwe69tEihmAevooJKzHjgzLGrW5DfFuqdXgebP2xhVSI7QW2K9S89eMjd2e_EjH1Letr0xGpJ_o-6LQ_-g-Zpj5C8vDbfoKoDerWwhzdFvXAvVl5Wk7ONdk3PHVKvmymvbgqRA=s2048) 
     
![](https://docs.google.com/docs-images-rt/APuouOcESeByxP9mNtQNJdOd3xMK4oWnu6_IAUlqTUg-ITeT9lSoSUFSeleQq5YO34QMjAez2pugksCmTHfcnlTWI6bFXbGmtGOPKbXqq9FWZt-uuaFKdh9z1rqeB4ZB3TyuQ0poL4FRni9Zm43B6Kg59csyhUSUDr7wS5ifA6oDow=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/APuouOd6glZ4dL_PgAkm20Gv_X-LwbweeJKbI_SnGFF_e1H5qvH-V0WbyA3diOl3T3MD5ll9d_k0mFhZlJXwkH2THXdeQUCr-S9NBVtZczKDdx3pBf8GZNWAHRbUEOajBL8P7xpQRgl1gEKfr0kcwuYjCTj7Fcz-pGVwhBwlcUljNA=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/APuouOfn2PCBHPPvS32_ww_QCr9YadBAH-i_gYjLJfJg57qWIOKMvThlm7Z8_kMjg47vr2DLdjnxsAqI8WFNT0W55F2JGBdISprtiUAvf7tjeqGyuKhDDJ5NHRMxs3vn0vWepobK48RFj5iWfQ-oAVGw2kRage76ETvAjvvK1buAOA=s2048)

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784178062089198&usg=AOvVaw0YilE6OPwFlQiKGbMnvN0J))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1784178062090552&usg=AOvVaw3EdvxGOBcjObbZISBwcnfb),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784178062091652&usg=AOvVaw04ozlH1lE0q6uJHcFXEoop)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1784178062093150&usg=AOvVaw22RrjxyCV467wE3p6govyS),
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

![](https://docs.google.com/docs-images-rt/APuouOeDXUj1BMrNC6HN6ruf2tqfyYBygYkzHR3y2iJ4eg9nrXQW5nqj6kob6-a12-rPRcS4R7UT9TSgm2L7txLlA6xIMkYkJxW6krHQRbH2zh68SwazTWCvCt1j2W6CV7V95zIE2yMqQiF6xvYfRpy4tTUpxUxAjFCmot5sDA=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOf9Wjn8KnT3FvtURDkUevizcixYNVthdMuNJOmbP2NMd2MPThR_wc2Q4c8VQpO8QkCsiRWvu_YmL8RIy5mefH2swrgzhWLkp-qWj8VjubO-9TLxJNW-vLQ0iZgsHG0wQNleUkE9BHAoymSbS_5gK8dnSUDk00x5MSJUe4nW=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOfyRMsRolC5yvUnxdcecoDNycW9rT5do9crJMpJKrX-ITBw7lEzYkuQIspMmQNo5vsZH9BR025QgiFtExZa6ssUWVJXnBzMUfensmClx9osd5uSAigex28qFQqr7Y-a_ge-u5ale_zbOeQuKdnCFvAL32-g9t29DX-r0g=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178062098658&usg=AOvVaw0Q-lAplaSzwwA00OL451HY) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/APuouOdNsqcauYQM62CbVjpYrKl0SBFmr5RO30_609IIcavNbI7CN26p8iqRgW_ESygDwSS4IVgLBF7ib2EfDbRgeteQadgiyrteStGiEPiw-Rl56s3TRD1TOBiP1DzU6oUN9aUx6S35qv1h0hVRRsLVJmgtjfld6ykguLbcTg=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/APuouOd_i0P-1M1-R5Mv4E4meJsvQVuJVSti7e2vjvjoepy96y97Dy-mDnMeJ7p5OfeSD-f89a9Yug-tywVvX-iZOv6MHK3EpYmjyK9xHbDhnf1z-y7SfHJCq7FJMtjdXETIBRCp9_NF1PJblgWbPvcrK1vgfJciKt6UDuK26LsUTw=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/APuouOewUMd4FJMLAzgXvGbsJuUBJQi-QNVBsgObZzPcf6dO186-4Mii31UW8LYvJLJKVgx_MBhDvk-47zWSIS9k0IqUcjJIobJbZgOnIumYBht4lb7DhCfqUNoT9o1xjW_9PmC-o1C2YJGtqkDLp9fddgeLIS0ODBExPta3u1g=s2048)Figure x: Saving the measurement file. 

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

![](https://docs.google.com/docs-images-rt/APuouOcpTiiQkQ95fEOxbLwBx2U6eotRiJGB90RUFEXnGgQNUVqwygfxDgf7zleBsQqQBhYLkuteOVmHZ00IATpoQkZhdBN0_KVIG89BUkeK3mgask5_wj9sZNm8qrBGZmS3THGeeVjTwXIf79gybMomcr6NNR0iwWJezu9GSejyIA=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOfBjHi7IWnLdmPRLsGdjMWEyneBk2xr8t_PQx4mT5znX683PbNbnIzOI46znfJpkdJobSWkGzCtUc_3qiZoVENERtmEltmdnSDB7cBeEe26xXSGmNDN8Cpd4GWMotKjUJ0seDMYISGGOvffkCHSQAwznsXb3i7zu1OKUebL=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOeAcTo2u5q5FeOlB5XTkq8hRVAxpfHyOq9yBpXjj2mnX9kP5KTDmRwJzrRY9iqGMwq3rLOt_9ur91bHZboIh3_RsIy9D0H6CDqcsP8Cy0OlgnvXaPyr3T0aE-hAr0K_ld9Hj6BY1HVMMM76GktbrKVjJRm9zzEBXXBmvdvbeQ=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/APuouOeGRedHzCFsFW5eZsxzReMPSK7CDIgS5gsZxVBXCiyX5juXPvYkhrCAXIU3_fruI0zEFncNHvobHVcd_NV4TM54vJXKovimKZusZEdfcdU_jxebgdRw8Rhmhgaz6S98YHpAeP7Bg2gG0-UIbcY6nPTKfW3DhW9Ph30X9y2j=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdbdui32p3M6SpKcAWnffTQsFzz3t48lnVFpO6iSSyFesErzzNqCUhFWjxJdUQPu3InayYbxHq2oxbBYn4UDg9tMkL83scbE_x5SRpw8p_vSIgaRUW94JcY1BtPymfriEY_HQYVxmDBKTV4y5R5H_CrjTpT96WJSpeCTmcoLw=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/APuouOeplqSUNg-khLmECveUr_c3nIK9yg3P0ht3P8wi1EuFu9Qt28VPQ6Y7DixByinUcwUG0VrUTPxBonVPqDQiG6zdRRdnaie4iW6b4s8gq2Qo7aGQef-LfhZVl_n_Rx0KmrrJgc6c5zhm-55oCPlCK9CTYZ1dDk2g7sIM8NYO=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOfe6dDorG6D5pAk79yhwdma0cTjD95muIHJjQeJ7uWQTfayQcAtRz7ePy4HSp7Er-EaIBIfO_GR-chD0nCcW5FN1ZZ_U2mZVLGjahNzjtQenDwpuaITaASjv-ydLAILHkMCtAc0Cxf0kfsJUrpRAV8rOmBYMdaAKXpkfIoDvw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOcFlTSXoWCUJlxFNsJ3dX9-AoYlgywPY8a9SBvLGcLgWi9rEa0LVcjp5uMhWT0E2YMaX_RECY4_-jNxLDtiCt6wGZtfuglmYQPTR33drvfxwrLt6UKgl0_ocJ9a5tfo-dPOyAGMJAfLE-69fjiaK45k2W-4m0V337L-MC0=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOcCBe-Z2GboQI6fTjlQfM-lS_n4z19IANKC2kfGi2ZK2Lusz0y8vDZz8hreigt-1cnHvKihd97vO4AyBqTT5YDeTDYm9dBqEgA_6VKPTgbqZ-ye6Kl8qE0ZNVso1hYrvDJ0jfdWvmk-zA6nv2IhMahAvoF1nwdrK3yB1e8QYg=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdpaTCtJhDfDQkeqfMl8ldEatUcaxXn886I815SxypIcJom1ULbfM67gYsvk3_USlqSgcAfhP9iyZzT1AGDTw92SKNc0w3zuPcu50aZ216gr4-xYndABuBr1ETxcob8vriT4Uo2cMVSFbr13eC6COuC6N9AgBlnIDIEwdM=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdL866fKKqBBrcI4dezExUr3gzGwJKLDCUXNDjdyaV_xZPAusGVy4_UH6X3IYzW26f0-PQ9sie_611Zo7GPwBVk0e7C6t5-fkguwW19Y7lxFHQUgSnSzn5qTZQmXYOJBWJQeKzVO7UVDsh6UHAePtgk2AxnUBJ6nb-zNvPa7Q=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdi3PXN2Np_Z5KmxTgG15Ynz20TlIO3s9QmbzGDPnxaau6rLANjTCWzDKKV457deC8cKqGA7vBUJ901OS4KlmTq-1E8LHzBnsXTrKQr476w5J1goPiOYVI_t5-NM-1J-kqOWIhe658dFgz50WwvzUN0axYMdVKaPMKXIxlb4g=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/APuouOd3radkNUSMsPuHPaSUUhsXrwvKfA3RpGAwNBEdb4YnzW8wPCkYPclckanfUkZFYZUxXfArdjKeyYqU607xWrZFide-3Sab_v0_am8HdZuip4Cu3DTI5cGA1qRnyeCrKA7RgPLrWE6vlKRSbo6z4xaX2lBtPlp9MYRj1130Rw=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178062126746&usg=AOvVaw1nS0kLvx46rCeYLdIes-9j) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1784178062127275&usg=AOvVaw1ihcx4VAw_im8vtiRuzEeb) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178062130076&usg=AOvVaw3IGF-ATmA--hUPzAYVVkMK) -
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

![](https://docs.google.com/docs-images-rt/APuouOfPtGumpoVQDUvqElS5YM2uRXjzjb_ywAY_JDW5VO2hiDvbLRrFhsCVYEJxYjb1_vk11pKNYoGx5MUVwFx3LeREYqo7LMFbwIXbt96tFVEqYg85yGA7uZURnraDUL4viaoUiA8Dl8OHPxd1fDgV_m2uUImj5qhge-Dx9lCh=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/APuouOeplqSUNg-khLmECveUr_c3nIK9yg3P0ht3P8wi1EuFu9Qt28VPQ6Y7DixByinUcwUG0VrUTPxBonVPqDQiG6zdRRdnaie4iW6b4s8gq2Qo7aGQef-LfhZVl_n_Rx0KmrrJgc6c5zhm-55oCPlCK9CTYZ1dDk2g7sIM8NYO=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1784178062134613&usg=AOvVaw2oAt84rm4ISNFDcvW3VmUv) -
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

![](https://docs.google.com/docs-images-rt/APuouOd2PCxpBivxYZmTuTqPQ-ASVVf1wIkuq5Shui8aCxb-SE66th52fMDYy3yteZhTMUo4IlcEkkRTu6bb9DjMGnaZ7sXrwri1ldlsE8MTa4eH4x36pjwXOA_CfuEMTzTL5LU360PpNU3bpaKHLOIfLwCBPPPFbWyKYXAw3qRQvg=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/APuouOcPutw6zSdVJffQ6vwHeDo1upIYmkpgrh5lO3PXe0CvD8Sk-Lts1WQQ26f42-z9JZHCS-dxs8ZjpcE_X3j5Y4PJNUc8gv1la_vTIytzQc44j9mf1VHnfJ7qN9-6LC9jtKSY_Nt41_qLWEyyqPoZiiYMexy7XiYFxS3eY9B8Yg=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/APuouOdBlXWwcGFfgI9MPvg0iAPOl2Mg32hGVkX8WZblYvd_REmSj0t7-VLV0dZF3zELg57iNbGN_IpK4iihCKd42Ziqd_A9PFDXhLHso90fOvkUcWxEdGX3IiXQmchHJGWh8QSmhTSLh3MgBqi6HvZPmd0-xm4dKekTuYBk49JNrQ=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/APuouOf3EpG3HLcxX7XQmDkfJZtUI3m_hDkcU3BGqm3e1IM2SI-gZ4zRhHwDC4VxtZluU5M9BtW7-6yjI3Hx6PpoUD8dN9ghbgCgtiqPoIKfwjrFLjJ8M5nZHyZBH0nqD0A2DFqec2ThiYvqDb8E0NmoytSkOjCcc3-vJBurPtiX=s2048 "image_tooltip")

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

![alt_text](https://docs.google.com/docs-images-rt/APuouOcXZ1p9sjAxaoo2XDBhA25kgZVwZxqWprFslwEPT14tRC9bLSSPu1fTnbbjQWogBB8DymPDJM8VgpHDIVBoQe-7rpo-fpp6ctoHJKYDAtdacM85T7y8xToHY12fl86Fj0_qfBIO3rHupRoHKTjKnrSktjUjInqQQzv90w=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1784178062149289&usg=AOvVaw0nT6tazno4246YH2VOfL5u)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/APuouOd5p5iGBQSBWrdTBuYeRGcTwgREgxnOVRsRSKiZAR5XS-ICF0DWv1bmtVT9Dwdz1yhIJLN6yqwdRMaaCqysoZM-IJcm9FSAP9nHd-XFo-LqU7r-AmSY-PsBEzStfz3qzz8I-XkTE1EuVKXVLmlm_llgmcvAWkECVychA2UhtA=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/APuouOddXMmlBaQ0LRCjmKEvqs2CghV36yEASmuR_Wz25hzdcMMlIPDBBmQdHn3cEK6AE6ky7KBTkNa1XoBtqpEVbVuez-Npxbk1loliv_yEHccYB7krHbLJ0-uACElYy9HR1pKz06I3Kr0c_VCa8AxdcRnqeA9IQSezvIU3UYNePg=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/APuouOeRTh4CKm30J7sMt2LcQUysHZ9XnKSqAZ_tvos5J-n7Bfc3y1ImnJVmoXon5d-sp-ce_Qr7RBjXTbWgzGfhgXptF9jexw1wKeaoQjj2flInC0X0DQXWMgU9XcK5_crqWzjdIuyGAmZdhvkaN4nXmMVfEuLiJJi7GQvhoDFRMg=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062151604&usg=AOvVaw3uQQRf9n_PDoGpOiWe2AMf)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062151705&usg=AOvVaw1nLvxJiCinbQZ8oivC-FyK)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062151959&usg=AOvVaw0Qjl7yApMbtFj1bXEWjMpZ)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062152073&usg=AOvVaw3qDddVuSW_lgP82_xWTzUz)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062152135&usg=AOvVaw3yxdv6Zmk_w_Y7uwhDa3PX)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062152203&usg=AOvVaw0vbhUV-5NGtswS5yK60OPc)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1784178062152285&usg=AOvVaw0Ef7OkLLmMgs5oL65tPq98)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152443&usg=AOvVaw2x1F4aH_B0aSdk5NR3BxDM)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152514&usg=AOvVaw1GTwuzvTUCLcgUkqHalRxV)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152693&usg=AOvVaw1gIpXDEFTSyjPWbqqvRBVi)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152790&usg=AOvVaw20LYUswKekdtQZW4HBeR5N)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152851&usg=AOvVaw3vZysapGV2d3D11CxTz8U4)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152914&usg=AOvVaw1Pie3ScxuNzTs7qribDjk8)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1784178062152995&usg=AOvVaw0jCcTh6fcxNOnoILPij8_K)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784178062153395&usg=AOvVaw26aRmb3u52FPj39AYfGEJ8)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784178062153467&usg=AOvVaw2TDNaE8Uo1EHduzWgkVVoO)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784178062153526&usg=AOvVaw1e0MxVGIBKfNt35pr8bv6n)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784178062153588&usg=AOvVaw16VGZ6xCN33D2YppLpdAGl)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1784178062153671&usg=AOvVaw2gR5vE1kOiSrI2XIK4OLxn)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062153822&usg=AOvVaw19mMUFGJojjPP_0kpMJZ6c)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062153892&usg=AOvVaw0-yB4p1y_OYCfKOda1hQfN)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062154116&usg=AOvVaw3R7YeUxnIpUDvj7NKk4aev)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062154200&usg=AOvVaw27PtQ8zKgMYzNlysn1OeSy)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062154275&usg=AOvVaw02HMdYYU8lCYwKD56SgKqi)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1784178062154399&usg=AOvVaw2_trglx8nihM0QZNr_yfoR)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1784178062154486&usg=AOvVaw2SenitfjY2x99j-80TM-9U)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062154627&usg=AOvVaw23xGpd-g0PMdWXB56ifVEE)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062154713&usg=AOvVaw1Bw93JjB1HRZItHKZFo5Kj)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062154893&usg=AOvVaw3uO4BBnM4nk9pqUgMKZaPm)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062154967&usg=AOvVaw0rs91oNlzxo1vI42zfqMUq)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062155026&usg=AOvVaw06K_P5UXTGncaX3cBKYybK)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062155093&usg=AOvVaw3Jk9G4nrpSdVWjUIZXgWgm)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1784178062155175&usg=AOvVaw2xax_S0fkf1t2J6eyi1P2C)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155313&usg=AOvVaw1QxIfBAU-7HVizQabxt7zG)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155381&usg=AOvVaw3SvlmiQEAoOM4q76spkmxJ)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155569&usg=AOvVaw0OasoRecLfvJIMrYPat0f3)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155640&usg=AOvVaw0um--fbWAw8PvBU6VdyJ7q)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155705&usg=AOvVaw34ompPfmPLJ-N7KSGpW_r9)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155766&usg=AOvVaw2lx71TcW53tpMbOR2UA1l1)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1784178062155845&usg=AOvVaw3d7gKh9qDvVa62T9he83hw)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156033&usg=AOvVaw0fPPc2Y295nVRJ9GkR_Qrk)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156108&usg=AOvVaw0BIVAf2qq5ld9V2f_h3LER)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156402&usg=AOvVaw3JXnHBEXzgbQJBYbgJcmBq)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156477&usg=AOvVaw0-3em1IaetrgIYOdmgCN7n)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156538&usg=AOvVaw1OTgJJRAv3jbcUm14bB-Zm)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156601&usg=AOvVaw1ezWQ7hLGmj_Yg4HwKTKoK)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1784178062156728&usg=AOvVaw28GBqVEc5PMkII16o_LfLZ)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062156909&usg=AOvVaw2injQan6doBvqqBVJkYn6f)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157001&usg=AOvVaw2flmGF-3o1uxw-iSd_mphH)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157254&usg=AOvVaw3fVrkUQRp0Cm6SaA0zb2u7)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157353&usg=AOvVaw0Opx03ffx-_LDAJ9stfkDi)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157439&usg=AOvVaw28yCoDecqyKSvHwOcDxUA3)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157523&usg=AOvVaw2KtCg9HaTnU3S8eKsaKi6B)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1784178062157623&usg=AOvVaw0a2Rkbnbhxk9tKQOtCzEwB)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784178062158178&usg=AOvVaw2V-o7bDwA9UZHMfg4fX3gI)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784178062158268&usg=AOvVaw2N21nxxTG_BvoT12VFjCjv)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784178062158349&usg=AOvVaw0E3_ao6SxYOgPwGSOJ1Ado)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1784178062158462&usg=AOvVaw34_LjxiEiMtAKl9RUt1XHn)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1784178062158531&usg=AOvVaw2wuaABKHU2_oIEBbe4MLSA)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784178062159057&usg=AOvVaw0yqobXiVxJKGhPRYwPIfQS)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784178062159148&usg=AOvVaw3Xdu2dVIJ3g7LJUXfKbSS4)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784178062159214&usg=AOvVaw0HT4MYUVhXfH3HGbFCYYZV)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784178062159289&usg=AOvVaw3fiq1oA4v5PAFjvqpQujFs)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1784178062159399&usg=AOvVaw3wdJakimyDAwxC9DhEGmBL)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784178062160051&usg=AOvVaw0EKOpD4l2FBWAVifNsZPfA)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784178062160218&usg=AOvVaw3fmMWvAcl0TYsWgtkXY6uz)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784178062160313&usg=AOvVaw2XNtkLprp43kftUViClC40)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784178062160415&usg=AOvVaw39UHTmX-YFrvtno2XpVcp0)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1784178062160540&usg=AOvVaw1ZDmrkCVV2UL3nFRH70u5h)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062160785&usg=AOvVaw1pqqFCi51-CF_l4SrIJXWj)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062160901&usg=AOvVaw2AzxyefmOU8HBM-l1EhqqD)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062161138&usg=AOvVaw0xhQW817PC88wbNgZY_9XR)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062161250&usg=AOvVaw2gmj1y2JWl9WWiHg1nkpDM)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062161355&usg=AOvVaw1e1TOaL0-kn-HpBcyeyXm4)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062161428&usg=AOvVaw0MFfX_qADc0JzDPQBW0vf4)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1784178062161524&usg=AOvVaw09UZtuFEpKjxO3ux-alPgu)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062161714&usg=AOvVaw2-DBojIt6nXQejXFJr80AL)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062161818&usg=AOvVaw1eB2zRqk96GotBTcigkxu6)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062162045&usg=AOvVaw12vJsANoRf8HneeqSFJMLf)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062162152&usg=AOvVaw0T29rgkFgYkqb6B4GFin4D)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062162247&usg=AOvVaw2Cq7TuPMaN_52C472H8AYk)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062162325&usg=AOvVaw1wX9_EsgrVDs7XHVJy6eo9)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1784178062162438&usg=AOvVaw1ytnjU09OC2Dz88pIJB-0w)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784178062163031&usg=AOvVaw1MNokDohAtH7MUaQfGua3b)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784178062163158&usg=AOvVaw2a-_ldKPvARSCiBMHOsnyO)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784178062163251&usg=AOvVaw0zwfLWXaP-gw7bBAQ2mrRZ)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784178062163337&usg=AOvVaw0AUY_tkcH0jfzRfCRdACym)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1784178062163462&usg=AOvVaw1-3ifQoQlLx0Hnb1FuwgeB)
