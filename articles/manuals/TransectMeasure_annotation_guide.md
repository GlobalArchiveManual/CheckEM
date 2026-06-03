# TransectMeasure Annotation Guide

[ Download PDF
Version](https://docs.google.com/document/d/1c0lo-wQbJqHk0GPm824wP3o2zVROBeT_G25M46eV6js/export?format=pdf)

![](https://docs.google.com/docs-images-rt/APuouOe5FuSTN79oVmEcDUtbjPf4cs0PD3GbwsYAfy86xLCeR6p1hjCqZjKvGSggo90sdy5Et6inpTQ0G8Vgwg1NnoxxAjyeV9nd3to-l4rr-7WxV5F_hhi-bl7Qth5cG9JG_j0RKgDo_YChU5tbvIpizz4uEB9iZl65tor92g=s2048) 
     
![](https://docs.google.com/docs-images-rt/APuouOdfHViuena6PraMAgPDWKgSKcTSy2nCZpedvvbmVJKkCHnWB3wlnNL-tqinOvuu1Q80OnaoISvLzdSaiXA1lHi6dL-biIu72d4N6o0c11OffCB9q3K1Id9BFNUBhG2cj363Rdao7Ol8tdt2H59pHnFP7AfihU_z12Fhqg=s2048) 
 
 ![](https://docs.google.com/docs-images-rt/APuouOeYtkEh4MBrdeNJfL1VjaxZVmSR1lPAEhKUPjjN-3VeY0bcPPUrULGM4PdaiajEaFLTmnC7BNqLt_PirbdF4hVdQsJGh4jnMfcvgacCmUSIS3RUY-r9qJvYSYC3RxiS65NtAWukoQL-rjoex6SKDcYM7mTdhQ27KVA9wA=s2048)

Annotation guide: benthic composition and relief for horizontally facing
imagery

Updated: July 2024

![](https://docs.google.com/docs-images-rt/APuouOdWMYybFUMnzDWOxzn6edg5EwFLn3gfvqLpCyKq2StgCqexdem5Ph0FflH-bAz9X5DJ7pKAhzL1ZBVCp06iZ32UbXplOupurR06f3w3yjL3MU8-PA5fc8QZlLx1wWGVCyMzSIK0hKOqqE_-m9_8T-J1QbASLOkk_CExJQ=s2048)

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1780455910319267&usg=AOvVaw1_Iibm46R0olL2IDggXY6v))
and benthic complexity (Polunin and Roberts 1993).

The annotation approach is rapid and produces point annotation-level
benthic composition and mean and standard deviation estimates of
complexity, which enable flexible modelling of benthic class occurrence
and fish-habitat relationships. A set of scripts to ensure quality
assurance and quality control are also provided.

### Brief explanation of the methodology

1.  Classifying the benthos composition

Our recommended approach to characterise benthic composition utilises
[TransectMeasure](https://www.google.com/url?q=https://www.seagis.com.au/transect.html&sa=D&source=editors&ust=1780455910319970&usg=AOvVaw1U8OqCGIjXPh-LzXlYV5Ze),
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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1780455910320586&usg=AOvVaw2N4cL0KcM3Svm-9u9aoHa4)).
 

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
scheme](https://www.google.com/url?q=https://github.com/catami/catami.github.com/blob/master/catami-docs/CATAMI%2520class_PDFGuide_V4_20141218.pdf&sa=D&source=editors&ust=1780455910321384&usg=AOvVaw1_PveLGe04-oqTXv3YUhuO),
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

![](https://docs.google.com/docs-images-rt/APuouOcjquvjFOvS0tZucbU62GkWcfH5IAAE5Xyr4GcPRYSYJlfezIg7_S5-ULEGLG-ob7xfvTbbh8H_vyZfRaVP_u9tvmaN7G9AJle5UdJjVnSYP_pJZUxMZbW_c57Rn7xzGdJZjr80Veo4HT8EZ8V-LoiQcg1tI1V4OA=s2048)

##### Figure 1: TransectMeasure initial opening screen. 

- To start an analysis for a new set of images: Go to “Measurement” \>
  “New measurement file” ([Fig. 2](#h.xxy7mnve9pur)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOd1FdnUlmOn8B5sVsv9M9A-t9lFznDmqSNno9mDYiWZ3LTr16Kvj0c6X4zfBQSr_I0LqBaIkzX_ToRaa00SQNpOmXuK3mQVw-fbgS7IQoDd1octR4hblxIe0v1sE2L2NQ1F85faGgvks6fC0IA0xbxDebNUxXyygao3=s2048 "image_tooltip")

##### Figure 2: Creating a new measurement file in TransectMeasure.

- Locate the folder where your images have been stored: Go to “Picture”
  \> “Set picture directory ...” ([Fig. 3](#h.5braz3xbt2)).

![alt_text](https://docs.google.com/docs-images-rt/APuouOdojNf1GpDCFwc5gQqh6Jv47spo5yVRiwQyqmE3lEr7aAX5YSd7OdKA9zN-z-OPRTgCSRB-vYmBA2ao9P36O2-8iRr4mzr7HV4FavI8VmVZqZvHSETlESwEMlvYGSiMA3wbxbWoZV8Op06cZjCs_fOAZ2FD2LlJRg=s2048 "image_tooltip")

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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1780455910324235&usg=AOvVaw3rotfgbxsSgIPeJwQtJGPI) -
  ‘Schema downloads - benthic-habitat-annotation…’. See [Levels in
  annotation schema](#h.cf5m2n6emqar) for more information about
  choosing a schema.

![](https://docs.google.com/docs-images-rt/APuouOdt1ZQ-JADWEWpPB0MSiTvsOmFIoWHdO4fga94453HdoLXbH_gTz4qm8BT7vqmT6pSH-H4LRtRbBKSRTPFj_6_IwPJa4xuuWKZll98R8mQDWlVz8tgC835JksALm52jyzo-f-1HUxC3npXOFi_NI2ZEArv6mOgvBA=s2048)

##### Figure 4: Loading the attribute file in TransectMeasure.

- Set the information field names: “Measurements” \> “Information
  fields” \> “Edit field names” (This only needs to be done once per
  user per computer).

&nbsp;

- Field heading 11 = “campaignid”
- Field heading 12 = “relief_annotated”

![](https://docs.google.com/docs-images-rt/APuouOcRteu2v66KwNtpI7n5CumtJsFvXnnP37vJ9J5FnND2hlE4sJebPB1B47WePE3s6suM37pkQYWb7gyOcrNTAx2brBieaYhlQJBGHbcDEweHlNLp1dAeRrYFVdzQuZCfxUo10Wf_ZnBJmnDpAPW8oMLBW6FgwK93I0hGIw=s2048)

##### Figure x: Setting the information field names.

- Save the observation file: “Measurements” \> “Write to file” (Fig x).
  Our recommended file-naming convention is:
  ‘campaignid_forwards/backwards’ for BRUV imagery, and campaignid for
  BOSS imagery. We recommend saving your measurement files in a
  dedicated ‘TransectMeasure’ folder for each campaign, and periodically
  saving your work throughout the annotation process. 

##### ![](https://docs.google.com/docs-images-rt/APuouOe8bXEOSXAH7E8LS-hGzKqMOuK3kT9u1O6XIsKCQcuoAVwHXszXPzZufy0K846P96ezP-IjAs27tWwHtjYeGYLuRc2Ip3k_z0fd7PFfoSW8oSuiMkqNGqo5hoN-jUQkePYS8GgGasjY3t57Y6KJGoeecbCQIKioL6c=s2048)Figure x: Saving the measurement file. 

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

![](https://docs.google.com/docs-images-rt/APuouOefI1M7tqQ1HVegHHt1E3rySMAGSxgRoOXaCON6YIt2A5qieKkZ4HHa3tjNAEr1aQBi2XRryFwzBqQp2VaNGDsuKZJc57lhmvWi9VyrIvS06Tsid_43dJq9raQkd9M8hGirvrpbKTLX8qwrZt6TcgWWTpaXavcz-4BEFQ=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOebQdRSDV2AxKFuslUba4tMPNUfYM_-Dzt_sGVwz-FOIZZdXvRkgSBnOxGUkmtqwAdsyCgrT-Jw0c7DmeXxpQeU5DLDvRRM7MgRVrgBu7ilWwHHUEA2PKNSrNSyuRwmF78Uo-2PDY3iMkGKMzkUO8FCp8Qigbtk_H-k=s2048)

Figure 5: Adding a new area of interest in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOeMRPqJAUGH6jZRxC3fUvhen2lOi_pdUm56iJHRPU5Hayy9STnUZyjKj2p7LuSJsoOP_u4NDcnDfAqqwuigjp8xDWItK1tkh5MgPP6JxknkWPFThH_QuS0kJdEkklE4W9DjljbONMHwlShvqp5EhKeCLGED8hZTswo_3A=s2048)

- To set up the random points:

&nbsp;

- Go to “Measurements” \> “Dot configuration ...”. And change the
  settings to: Random dots, ‘Number of dots’ = 20 (Fig. 6). This will
  allow you to classify the benthic composition according to 20 randomly
  generated points over the lower 50% of the screen.

![](https://docs.google.com/docs-images-rt/APuouOdNGSRL-60VLYq9vEIYfZP5xsdgALPaIykY4A9Ip-4uqyuLTQBbvLAw30LvYBDhGEZlgowFnKgQ7Q4_AOcBVjU4zoY2HLtJXdR3yGytSHoYOSB2_SUwqDV_zcye5VDaX5z_BweQDG-KE_lN1yFrvrcHrArtKAx9Tj52=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOeSCBoHszfpJzvHgsKmmf5OYf2HqoIgKcYqa6Svf9RR55zXlhpJiRIgjTGmQ6m8400cWoJPGz1Bs4wn1H06MehIsoEk5k3JS0bXzrxS8X7cG3u29eoqEZZJmoIThRKC1cX5__PQ44aBCPAgnoigaU02FhnpbzCNQ5kkhg=s2048)

##### Figure XX. Total number of images/measurements in the TMobs file.

![](https://docs.google.com/docs-images-rt/APuouOdzUUxsj_PhHwcYLwYHMjde3HLaNOrMkD1_rEl3Lytl5JmO5qoIYBi9VCnv7TTSZIGsIWhIpQwP4pf9OH_6yBv7O7P6_sZfnLvKEau4Ctq3TlRuml6PXWhzStsyfMNKdyWJRunAOpuuE4vnF__MLVQNpWbuLUblN-pY=s2048)

##### Figure 7: Adding dots in TransectMeasure.

![](https://docs.google.com/docs-images-rt/APuouOdBYk3lVx_BHDRIQctL-eCTktSm_Zok3s7aIFTQUS2mCGq7DN8jWO4zuX0OacTud2mRehOJmC4cI1v6EI6uhfBvJE0aXAoOQYqhrhJrEFHi7eq2kkULe8g9kFWl2ZcFSEJtCUVQ7IuXlxVVt5ri9u_fkRr455LMqA29XA=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdfpOaT8zyVE90Gynp2GRzcOflygDUhbRhYmL0gzJS8Xj7ZeMTuoh2iA5V1nYIN5R6R4gztraf0DZ9qDoor64jBcWiSCN_n3JaxL67RtfJHpchJMXZxuSP_sy_p6nqsJJG7JxdGaZ3b-xxGKIY4Qn2TzKRrziBpJII=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOf1I-3nOUzWSEFtzGVhDy5CY58wSgTiNiWivjIMhMfoWD7xARrDzz30-GD0dKgOmcYxKbA7rHt5MFZhUdEOzNdHi5lrd-BeauUS8PEYlk99gmnWVJeg2_aRhRI1PLJS2TfBQ0sPUzDHveZjcieg__-fLePANIEHrMNPZg=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOdtB-y6MIaGPhF7KzSnC_GI55vPjoWbgIJdc9RUybDFdk2Xmt0wyIA-7JRVN56e92tX8Ea8kUUOq_RuORHYg0Ish5BRHqLIP_Zx3NjsPxejCQ5rUzGm9-sKCL2-Zkudko5Zfv7h-sBRGaWlC4bQYxEsu8AVOUwzy3A=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOcf9UuwwVYpNJsjKABv8Y2tEpB7-h9Tx7V9ziZCf0JEycMOicwfb8aKNnudnLl2WbvHqYXBAOBleILMo7Y7ErTu2ArUys5EB6LotsrC20niI0zkhd_TWgURrqEUhcXgJCYTghP4NpFakmk3foGgxtpeaDXnbQLrzhmhnw=s2048)

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

![](https://docs.google.com/docs-images-rt/APuouOegaGaum7dl33MQYl_kYOgVitCuLhOCMDogjYTSdVBRSzxup2dCUm75uWusni9G7US37PtE8tHgKhP_X2GrgXqk0SF6SUQ3qln6BzYSnWgNItVoVEsHQ6zrHj2Nk424Rwh-XH3hBbY3688TQuqPiVjQe0HfTMHsDtDTvA=s2048)

##### Fig xx. An example polygon selection made around multiple annotation points. This selection allows multiple points to be assigned the same attribute.

![](https://docs.google.com/docs-images-rt/APuouOei3m5f_n0LyjsLJkNYNsM72LQx73Jcmt8lOF-vN4n4pnu--xn_lNaAqNRFD9xinB_XFOD-aoxxSXqQwdd2bOnTjASrQEH5lWR5xtlaSQ7K1FHQf3vv8TwE1P8V_lElgfypEu2xMk5j3Kwacvnl7Oj4pawmPibnpOZxiw=s2048)

##### Fig xx. Editing a polygon selection to assign multiple annotation points the same attribute selection from the schema.

- How to change level of schema

##### 

### 4. Classifying the relief of an image

A separate schema for relief is available for download via
[CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1780455910335529&usg=AOvVaw3yvobjldqrR9mZPq7gcObv) -
‘Schema downloads’. 

[Here](https://www.google.com/url?q=https://docs.google.com/presentation/d/17UJJwhTpNy2hRC4jBb_Hz24ppvlwIHj1dMp-URR7bc8/edit?usp%3Dsharing&sa=D&source=editors&ust=1780455910335708&usg=AOvVaw0O3DIMe9wnrfk7YL-eDKjZ) are
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
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1780455910336500&usg=AOvVaw0XakbKb0gll77NfSy9zK5D) -
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

![](https://docs.google.com/docs-images-rt/APuouOfiNwm720NKAsvX2Sua5Gw5WQamOMTr9bRL08P0f5ZnJE2PMJzJS8T-_W2omJt6U4ySytSbQD352jgaPcHFllg3XlQHVaeoTg2_I6o4iaugQG7RLfaZWzVvI-EV8dBs2cHnZC3akNt2WWDtjXKYwCimAOZ6fN32TBgl=s2048)

##### Figure 11: Setting the dot configuration in TransectMeasure.

- To overlay the grid: Right click on an image and select “Overlay dots”
  (Fig. 12). The name of the image will then appear in the table to the
  left of the image.

![](https://docs.google.com/docs-images-rt/APuouOdzUUxsj_PhHwcYLwYHMjde3HLaNOrMkD1_rEl3Lytl5JmO5qoIYBi9VCnv7TTSZIGsIWhIpQwP4pf9OH_6yBv7O7P6_sZfnLvKEau4Ctq3TlRuml6PXWhzStsyfMNKdyWJRunAOpuuE4vnF__MLVQNpWbuLUblN-pY=s2048)

##### Figure 12: Adding dots to an image in TransectMeasure.

#### Composite imagery e.g. BOSS

- Annotation of relief will need to be annotated in a separate
  TransectMeasure file (.TMObs), which can be set up by following the
  same steps defined in ‘[1. Load images and attribute
  file](#h.qynxz9u1xouc)’.

&nbsp;

- A separate attribute file is required for annotation of benthic
  relief, which is available for download via
  [CheckEM](https://www.google.com/url?q=https://marine-ecology.shinyapps.io/CheckEM/&sa=D&source=editors&ust=1780455910337878&usg=AOvVaw304DU8q_NR6gTIs3slWmIM) -
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

![](https://docs.google.com/docs-images-rt/APuouOd66RdjbfaMn7UKZQ3YEIYq3dPZ7Y37NmUhbgX8AWyeB1EvoYMlmfBF0D8uVlqeU7iiZzgttZQXb2n2Vj1sRtqVSbqcAPi5iaNz4cGMjYNZNf91zpIicEaXk2M5dIZl_k0SPHUwzOJB1nMFhOHRp5ySYJ69F9CNtFEgEA=s2048)

Figure 14: Example of a stereo-BRUV image annotated for relief.

------------------------------------------------------------------------

#### 

##### ![](https://docs.google.com/docs-images-rt/APuouOfLl1Rl_atUUzPPYLGRwnlhSN5ivMpI5YHB2xJy70CrPdOJ8D7xAgJMkZy1Y-Ggpwh7n4oTXCjsyM7hGXZXDIMD2QFTxRqZM41PkbhwfsZ_mdwqtnyHko_BB97KGeasTVT4Brfitl5i_s5v6swEmY5oZYKdAOlsXd-RQQ=s2048)Figure 15: Example of a panoramic drop camera composite image annotated for relief.

### 5. Exporting from TransectMeasure

##### 

- To include frame information fields in the data output: “Program” \>
  “Current settings” \> “Include frame information fields in text
  reports” = “True” . You will only need to do this once per computer.

![](https://docs.google.com/docs-images-rt/APuouOcWnueFcYWC1S7UThtQu4NBFtxc4zu6oO6cF-03JbX_uOAEPwbRVQjtwWUFm43ymIEtJCK3Oea5NaIc3NOY1KKdPjKA3GaDf9i9Oi3NmKGqara17sV76nRpO0Wf4Le0Z8o0g1jPKN3DYp5hWi2d2maAwSXmrewLWgV43g=s2048)

##### Figure 17: Changing current settings to include frame information fields in text reports.

- To export text file summaries of annotation data: “Program” \> “Batch
  text file output ...” (Fig. 18)

![alt_text](https://docs.google.com/docs-images-rt/APuouOdquof-cArQXWxP1Zb1nOCsqEndtdK485IiSt5zJG3WA8MnA6VbaAVHqKNLyttjBHYKnbMVuJGzQOE7Vju8We3rNN-ogqBmGOa459KNDrz8DBkmdO69juL2Cb5Awae8I0rRiUUu46n_dd2kjwroU2m2iFLipTpYQnVB=s2048 "image_tooltip")

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

![alt_text](https://docs.google.com/docs-images-rt/APuouOdgduh8Ba_ECqpWnMjds9lHJhXL87f0gBw14g_RYuF85GGn4Qlcm0PbRn6bcW0ebrSCTHbs4IQhZBNYoenEdc_gyd2SRqvAjQJdyzhTNh1EiawUCEZEEC9UtYTJSGw7a-IzdPM1UcOPUZpckGKq6C4xGBXgefVApw=s2048 "image_tooltip")

##### Figure 19: Input and output file directory options for batch text file outputs in TransectMeasure.

## Annotation summary and quality control

All corrections should be made within the original annotation files to
ensure data consistency over time. We recommend the following approaches
to ensure quality control:

- Check that all annotation points have been assigned values (see R
  scripts included in the GitHub repository -
  ‘[forward-facing-benthic-composition-annotation](https://www.google.com/url?q=https://github.com/UWA-Marine-Ecology-Group-projects/forward-facing-benthic-composition-annotation&sa=D&source=editors&ust=1780455910342967&usg=AOvVaw1YeeQL3AHpg06vbLaaRlDT)’).
- Check that the image names match the metadata sample names.
- Check all successful deployments have benthic composition data.
- Visually assess data for trends, outliers and potential errors (Figs
  19-20).
- Spatially visualise the data for patterns in the distribution of key
  benthic classes (Fig. 21).

![](https://docs.google.com/docs-images-rt/APuouOewfYahja9SeFUZLgM9e_OeE7dr5zbCgB-H6DHFSEBaHlI88l86k_Utu66tGkcR-sUGKjNiigxT1qpHcH160HQD7_jeSedhtcis2eZFT_1TR_856bqtTkWSexMICTPPxKAM1qeqe59VFVIBbN9VNNyBJPz7jFqcCrTpRQ=s2048)

##### Figure 19: Quality control visualisation of the benthic annotations. 

![](https://docs.google.com/docs-images-rt/APuouOfjciyhpjtEUh0HhfaDEuK0VI39huDSSgdwbhbdJTgoa-r90NaDolfjLAOuGGiCi5EmM9EwhnvBOGnjBEeHRwnFrL94J8GMABqOhffmKnxtDW0ONqfB5lEn5XzpeTuMZ2bS7_ngTcwP0ErmA6o1TXO0RTr_tbNd5UNESw=s2048)

##### Figure 20: Quality control visualisation of the relief annotations. 

![](https://docs.google.com/docs-images-rt/APuouOfK1WOi_irpjI-Qtpz0Ubu9zpSaDlb1cPQp4f_b71ak8MGH-RRZX4L1TimluG_7PZFTzT-ewQ7-Rav_y1MAM2wDsQcE2z7vRDT5mqtUWMEol0NUZcLZ6o39fP3H4NMuF4b7QvxTmpbTrHZMUT6fT592xuUhkJMHlI3dSw=s2048)

##### Figure 21: Quality control spatial visualisation of broad benthic classes annotated. 

## Examples of publications that have used this or earlier versions of this SOP

1.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344030&usg=AOvVaw35AZJFqND_6ZsKoo46kcZp)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344087&usg=AOvVaw24CGxIjTk4MsSHRXgP0AYD)[ Distribution,
abundance, diversity and habitat associations of fishes across a
bioregion experiencing rapid coastal
development.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344232&usg=AOvVaw2TBasovvR1DfKK-lrJqc8G)
[Estuarine, Coastal and Shelf
Science](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344297&usg=AOvVaw1GXh4EaxzaFbEjPWmkofpc)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344331&usg=AOvVaw2jAm3IiwSiY2KT8P0NCLy0)[178](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344367&usg=AOvVaw32zeGM1AfwI8H8THohEuP3)[,
36–47
(2016).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/4g9p&sa=D&source=editors&ust=1780455910344417&usg=AOvVaw05jNXcLsHfpP_bSF9UizOr)

2.        [McLean, D.
L.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344501&usg=AOvVaw0wju0BlFpiKBPFoAM9WrTI)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344541&usg=AOvVaw0hQrJ1VjKAQy96vM99sJtt)[ Using
industry ROV videos to assess fish associations with subsea
pipelines.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344638&usg=AOvVaw2fD_tp9dEcwutlMgooixdC)
[Cont. Shelf
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344687&usg=AOvVaw0ybooPPYJXx-SDI9GlvF_z)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344721&usg=AOvVaw2FpLUcLd3Ue5jcUhirc_HK)[141](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344756&usg=AOvVaw0Yj1GX0qRriSCspIgZX9jM)[,
76–97
(2017).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/UzhQ&sa=D&source=editors&ust=1780455910344812&usg=AOvVaw2J5-itDtjymIyHikm4HfRJ)

3.        [Bond, T., Partridge, J. C., Taylor, M. D., Cooper, T. F. &
McLean, D. L. The influence of depth and a subsea pipeline on fish
assemblages and commercially fished
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1780455910345019&usg=AOvVaw1aye2PA1kUnsfPSXtUrlec)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1780455910345063&usg=AOvVaw13-NMtHgr5OWSU5l0cQzFH)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1780455910345137&usg=AOvVaw0RCVY6HtHk5aJ_0Bb6f1Hf)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1780455910345173&usg=AOvVaw3VyIj4XQ7_QHigVT_Jrza7)[,
e0207703
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/whHg&sa=D&source=editors&ust=1780455910345221&usg=AOvVaw3c8xkMc32TU_uZkJkaJ9Mj)

4.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345297&usg=AOvVaw2sBSE9lMNgzH2BITBw_M8U)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345336&usg=AOvVaw0DwmZsKWgGwQXTMPgfQe-c)[ Fish
associated with a subsea pipeline and adjacent seafloor of the North
West Shelf of Western
Australia.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345461&usg=AOvVaw3C1LyM-AZ5lKeZWzSQUcEY)
[Mar. Environ.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345510&usg=AOvVaw2CRf8-C8-Nu5VMW2CDUJO6)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345554&usg=AOvVaw0LrFRpX9ojpAslHJHQ0dSf)[10.1016/j.marenvres.2018.08.003](https://www.google.com/url?q=http://dx.doi.org/10.1016/j.marenvres.2018.08.003&sa=D&source=editors&ust=1780455910345620&usg=AOvVaw1xgoeqUvNTkniJ0cWJukuc)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/moP5&sa=D&source=editors&ust=1780455910345657&usg=AOvVaw3ZhMWczmhmmWQWcDZTt7t9)

5.        [Bond,
T.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910345751&usg=AOvVaw16Dbm6DXzloJIldhY984Fv)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910345790&usg=AOvVaw2Vwz3u8TXOxFFy-3SsZmz5)[ Diel
shifts and habitat associations of fish assemblages on a subsea
pipeline.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910345892&usg=AOvVaw38GmfH240ZW7HGHVFiAjer)
[Fish.
Res.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910345937&usg=AOvVaw130-TA6X3lFVx9pl5kXR37)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910345974&usg=AOvVaw0fc4dALqFMcpVhJiEOLqxt)[206](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910346023&usg=AOvVaw3nHErGS_dKPG5iN9W7Hc7h)[,
220–234
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/bRFC&sa=D&source=editors&ust=1780455910346073&usg=AOvVaw3GfJDLoHgI3MUwWgCnlD85)

6.        [Lester,
E.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346154&usg=AOvVaw1UYdNj8DYRvovNyVNE3Q36)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346195&usg=AOvVaw3cDv3qYe61bnqXjUvQ6cLi)[ Drivers
of variation in occurrence, abundance, and behaviour of sharks on coral
reefs.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346301&usg=AOvVaw3KrFrsv-99fi83oEfptDdC)
[Sci.
Rep.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346343&usg=AOvVaw0Dl8cvIB4K7bCn5m5--jGw)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346376&usg=AOvVaw3Mebo6Ik3VImKNjcuMeXMw)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346413&usg=AOvVaw0CcBQJQsgLw06eZEILjM5h)[,
728
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/70U7&sa=D&source=editors&ust=1780455910346458&usg=AOvVaw1Ho8n2C008w3khqrz4kPjg)

7.        [Lester, E.
K.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346540&usg=AOvVaw0j2J9zsAH5NK6WCdeV1dDY)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346579&usg=AOvVaw0ZjPknKMq1BTW-bC5nfT4Q)[ Relative
influence of predators, competitors and seascape heterogeneity on
behaviour and abundance of coral reef
mesopredators.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346717&usg=AOvVaw2lFxLmcUsWxi9urJHGWjHB)
[Oikos](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346755&usg=AOvVaw2RSa8QfO_Rh-uv7UIqVDJw)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346790&usg=AOvVaw1-_Cafo7YjzG9vAesL89gz)[130](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346826&usg=AOvVaw3iOLZLu1kSQyjLHefzqvNq)[,
2239–2249
(2021).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/ZVgq&sa=D&source=editors&ust=1780455910346878&usg=AOvVaw0Ni2GPW-ZpoZLuEMy7s9d2)

8.        [Rolim, F.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910346959&usg=AOvVaw29b5k673JK0iSWiUZJcw2K)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347000&usg=AOvVaw2wvex20BL7Umr3T_r_0ohs)[ Network
of small no-take marine reserves reveals greater abundance and body size
of fisheries target
species.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347126&usg=AOvVaw3qx4iRVA_a3Y6mLrXfj8w1)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347169&usg=AOvVaw1FteueOW-FxVDDovV_W1xM)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347218&usg=AOvVaw3FpIQPFSMNBY5zzjKr_Mrv)[14](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347254&usg=AOvVaw2ip0519iL-qrEDh2I5D8nw)[,
e0204970
(2019).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/1H8t&sa=D&source=editors&ust=1780455910347336&usg=AOvVaw3roiG3m9-anO7d08TJSDt2)

9.        [Rolim, F. A., Rodrigues, P. F. C. & Gadig, O. B. F. Baited
videos to assess semi-aquatic mammals: occurrence of the neotropical
otter Lontra longicaudis (Carnivora: Mustelidae) in a marine coastal
island in São Paulo, Southeast
Brazil.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1780455910347619&usg=AOvVaw2EFOZtCsNiXk1N3A98TEZ9)
[Mar.
Biodivers.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1780455910347671&usg=AOvVaw1gRFmvi11QCMTygP0YnFXd)[ (2018)
doi:](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1780455910347717&usg=AOvVaw0Y3jnGWu49Zdw6ZMjSPv_M)[10.1007/s12526-018-0868-7](https://www.google.com/url?q=http://dx.doi.org/10.1007/s12526-018-0868-7&sa=D&source=editors&ust=1780455910347777&usg=AOvVaw2EHqC0BiTA0xwVqfq6z3UN)[.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/pPIR&sa=D&source=editors&ust=1780455910347832&usg=AOvVaw2sVZlZkixdrZhykKgs6Nw8)

10.        [Haberstroh, A. J., McLean, D., Holmes, T. H. & Langlois, T.
Baited video, but not diver video, detects a greater contrast in the
abundance of two legal-size target species between no-take and fished
zones.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1780455910348078&usg=AOvVaw1x5skkYkmRGg0hGigssF1E)
[Mar.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1780455910348125&usg=AOvVaw2U6TZPReW6MMPN680-aTa_)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1780455910348162&usg=AOvVaw3BBR6O96w3oaN5Boc9Re1_)[169](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1780455910348198&usg=AOvVaw2uzCFMbBZjDJEYE8NpLRCw)[,
(2022).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/cHif&sa=D&source=editors&ust=1780455910348243&usg=AOvVaw1CorNSP9p94B5F8vY2y3JZ)

11.        [Piggott, C. V. H., Depczynski, M., Gagliano, M. & Langlois,
T. J. Remote video methods for studying juvenile fish populations in
challenging
environments.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1780455910348457&usg=AOvVaw0k0tC7lLxMJ5OI-2f3DleQ)
[J. Exp. Mar. Bio.
Ecol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1780455910348515&usg=AOvVaw38Y_G7KKC1-vnazELE_Zmm)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1780455910348553&usg=AOvVaw1dyV4vaeHfjOneGWOrlkiK)[532](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1780455910348590&usg=AOvVaw3ifFjiVRJBbAiG-yNKCGwA)[,
151454
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/LrAF&sa=D&source=editors&ust=1780455910348636&usg=AOvVaw0V_LjZMMr-6jU8jzwN9uEb)

12.        [MacNeil, M.
A.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348723&usg=AOvVaw1Tirxj_-NS04CszEQJAlD2)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348763&usg=AOvVaw11nbtAIKsKZYXSq4joJHm7)[ Global
status and conservation potential of reef
sharks.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348850&usg=AOvVaw3dzkgy77tYQAkwn6zmKu2N)
[Nature](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348891&usg=AOvVaw1fBBk8GEuHpqF9NUv7pxNI)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348926&usg=AOvVaw3jFtfF_auJuDry-EH2rMVa)[583](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910348964&usg=AOvVaw2j1Otnlw-W9pXY-Sz9XBDt)[,
801–806
(2020).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/uWWg&sa=D&source=editors&ust=1780455910349016&usg=AOvVaw37n9apIxrRgUh_EqpvIhmL)

13.        [Goetze, J.
S.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349099&usg=AOvVaw31U-3PWuqPgeuJF2FphqkQ)
[et
al.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349137&usg=AOvVaw2j0o_U3Cz4A2eNBE6obK8J)[ Drivers
of reef shark abundance and biomass in the Solomon
Islands.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349227&usg=AOvVaw3V6XGASsxGdTAghvgKoBNm)
[PLoS
One](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349275&usg=AOvVaw1aNM-5FGFGzs1GkTlWrbHH)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349311&usg=AOvVaw2fXUNUFxkEdvNEfT00lKvF)[13](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349345&usg=AOvVaw3IhGy7rfSqMFLf0vhonu6C)[,
e0200960
(2018).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/wTym&sa=D&source=editors&ust=1780455910349393&usg=AOvVaw2lnPEgEsMVGzVpmEI8zEO4)

14.        [Wilson, S. K., Graham, N. A. J., Pratchett, M. S., Jones, G.
P. & Polunin, N. V. C. Multiple disturbances and the global degradation
of coral reefs: are reef fishes at risk or
resilient?](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1780455910349666&usg=AOvVaw2-o0v85sRa5m65kNAwzaxa)
[Glob. Chang.
Biol.](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1780455910349717&usg=AOvVaw28Gisthr_NH4lOaio5TRGj)[ ](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1780455910349752&usg=AOvVaw1JngQvtNt4syCsla4CqAlZ)[12](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1780455910349787&usg=AOvVaw0Yd9rsz4Za3FZ6rqIxLtZW)[,
2220–2234
(2006).](https://www.google.com/url?q=http://paperpile.com/b/0wnItn/M1vE&sa=D&source=editors&ust=1780455910349837&usg=AOvVaw3Zpj4muDw2Zl0COXcKTVbp)
