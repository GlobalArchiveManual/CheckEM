# About GlobalArchive

## ***GlobalArchive 2.0 How to Guide***

#### Table of Contents

| **About GlobalArchive………………………………………………………………………………………………. 4** |
|----|
| Overview - uploading Annotations…………………………………………………………………………..5 |
| Overview - creating a Synthesis……………………………………………………………………………..6 |
| Using this guide…………………………………………………………………………………………………………7 |
| **Useful links…………………………………………………………………………………………………………….. 7** |
| Create an account……………………………………………………………………………………………………..8 |
| Navigating the GlobalArchive landing page…………………………………………………………….. 9 |
| Uploading Annotations…………………………………………………………………………………………….. 10 |
| 1\. First, create a Project and Campaign to hold Annotations…………………………………….10 |
| 2\. Create a Project………………………………………………………………………………………………11 |
| 3\. Create a Campaign………………………………………………………………………………………… 12 |
| Campaign Method Metadata………………………………………………………………………….. 13 |
| Copying Method Metadata from existing Campaigns………………………………………….15 |
| 4\. Create Annotation Set……………………………………………………………………………………..16 |
| Importing Annotations………………………………………………………………………………………… 19 |
| Check for Issues……………………………………………………………………………………………21 |
| Creating a Synthesis……………………………………………………………………………………………….. 25 |
| What is a Synthesis?…………………………………………………………………………………………..25 |
| Creating a Synthesis on GlobalArchive………………………………………………………………….25 |
| Importing data into a Synthesis…………………………………………………………………………….27 |
| Choosing a Vocabulary…………………………………………………………………………………. 29 |
| Check for Issues……………………………………………………………………………………………29 |
| Frequently Asked Questions and Tips……………………………………………………………………….. 31 |
| Frequently asked questions………………………………………………………………………………… 31 |
| Tips…………………………………………………………………………………………………………………..31 |
| **Appendicies…………………………………………………………………………………………………………..32** |
| **Appendix 1 - Definitions…………………………………………………………………………………… 32** |
| Project and Campaign terms…………………………………………………………………………..32 |
| Project…………………………………………………………………………………………………… 32 |

| Scale………………………………………………………………………………………………………32 |
|----|
| Type……………………………………………………………………………………………………….32 |
| Campaign………………………………………………………………………………………………. 32 |
| CampaignID…………………………………………………………………………………………….32 |
| Annotation Set…………………………………………………………………………………………32 |
| Annotation Metadata……………………………………………………………………………….. 32 |
| Method……………………………………………………………………………………………………33 |
| Sample………………………………………………………………………………………………….. 33 |
| Vocabularies……………………………………………………………………………………………33 |
| Synthesis terms…………………………………………………………………………………………….33 |
| Synthesis/Syntheses……………………………………………………………………………….. 33 |
| Scale………………………………………………………………………………………………………33 |
| Type……………………………………………………………………………………………………….33 |
| User Access and Sharing terms………………………………………………………………………33 |
| Custodian………………………………………………………………………………………………. 33 |
| Projects…………………………………………………………………………………………………..34 |
| Synthesis………………………………………………………………………………………………..34 |
| User……………………………………………………………………………………………………….34 |
| Appendix 2 - How to export from EventMeasure……………………………………………………..34 |
| Generating datatables from EventMeasure……………………………………………………….36 |
| Common mistakes and troubleshooting tips…………………………………………………37 |
| Check the information fields for duplicates and rename the duplicated ones……………… 37 |
| Appendix 3 - R Workflows and GitHub template……………………………………………………..38 |
| Appendix 4 - Format required for Import……………………………………………………………….. 39 |
| Sample Metadata…………………………………………………………………………………………. 39 |
| Metadata Examples………………………………………………………………………………….41 |
| Sample Metadata for Synthesis……………………………………………………………………… 42 |
| Count…………………………………………………………………………………………………………..43 |
| Length………………………………………………………………………………………………………… 44 |
| Habitat………………………………………………………………………………………………………… 45 |
| Benthos Count File………………………………………………………………………………………..45 |
| Benthos Length File……………………………………………………………………………………… 46 |
| Benthos Relief File……………………………………………………………………………………….. 47 |
| Appendix 5 - Common errors………………………………………………………………………………. 48 |
| Examples of Errors………………………………………………………………………………………..48 |
| **Appendix 6 - Data privacy and sharing agreements……………………………………………50** |
| Data Privacy………………………………………………………………………………………………… 50 |
| Data sharing agreements………………………………………………………………………………. 50 |
| Appendix 7 - Privacy Policy………………………………………………………………………………….51 |
| Personal identification information………………………………………………………………….. 51 |
| Google and cookies……………………………………………………………………………………….51 |
| How we use collected information……………………………………………………………………51 |
| How we protect your information……………………………………………………………………..52 |
| Sharing your personal information………………………………………………………………….. 52 |

| Third party websites……………………………………………………………………………………… 52     |
|--------------------------------------------------------------|
| Changes to this privacy policy…………………………………………………………………………52 |
| Your acceptance of these terms………………………………………………………………………52  |
| Contacting us………………………………………………………………………………………………. 52        |

Mono and stereo-video imagery annotation provides useful information for
the discovery, description and management of the marine environment
(Harvey et al. 2021), not only for fish and shark assemblages, but also
for characterising benthic biota (Langlois et al. 2021; Williams et
al. 2020). Such imagery can be sourced from a variety of platforms
(e.g. stereo-BRUV, Baited Remote Underwater stereo-Video; stereo-DOV,
Diver Operated stereo-Video). The standardisation, archiving and sharing
of this annotation data through *synthesis* can contribute to
understanding large spatial and temporal scale patterns in marine
biodiversity to inform management.

GlobalArchive is a collaborative archive for stereo-video annotations of
fish and benthic assemblages, designed to support data standardisation,
discovery, sharing and synthesis of this data. The platform brings
together sampling information and image annotation outputs (as *Projects
\> Campaigns \> Annotation sets) and summaries of count and length data
for* fish and benthic assemblages (as *Syntheses) ready for reporting.
It is designed around core* principles of secure user access,
standardised data import, and the capturing of both field sampling and
image analysis information.

The platform is designed to ensure *FAIR data principles* are complied
with, but to also enable granular data access so that users can choose
the level of open-access applied to the sampling, annotation or
Synthesis data. CARE data principles are achieved by enabling users to
control the access to their data.

For stereo-video image annotation, data can be directly ingested from
common software (e.g. SeaGIS EventMeasure) or imported in generic
format, after Quality Control checks (see *CheckEM). Schema controlled
Annotation data is associated with Campaigns of samples* that are
organised within Projects.

Curated summaries of count and length data for fish and benthic
assemblages made from annotation data on GlobalArchive or other
platforms, can be published with a DOI and versioned to provide an
unchangeable source for environmental reporting.

Data can be accessed via a secure API and R package, enabling efficient
and structured querying of all sampling, annotation, within a Campaign,
and Synthesis data.

PROJECT CT  
TheA project highest can level.contain one © e.g.PROJEGeographe Marine
Park  
or more campaigns.  
CAMPAIGN  
ceeie) Aprojectspecific campaign andsurvey represents belongseffort.
toEach a a @) CAMPAIGN 1 @) CAMPAIGN 2  
campaign has one method 2024-04_Geographe_stereo-BRUVs
2024-04_Geographe_stereo-BOSS  
(e.g. stereo-BRUVs).  
Method Metadata is METHOD: stereo-BRUVs\|\| :== Method Metadata v
METHOD: stereo-BOSS \|\| != Method Metadata v  
entered by users at the  
time of campaign creation.  
ANNOTATION SET  
DS Within each campaign ANNOTATION SET @B)) { ANNOTATION SET @
ANNOTATION SET @  
wa there canbe multiple  
annotation sets. Each Following MaxN and Length MaxN and Length
Following MaxN and Length  
annotation set represents a standards in Langlois et al. Measurements
Sharks and standards in Langlois et al.  
discrete upload or version. 2020 Rays only 2020  
Annotation Metadata is  
annotation data is  
uploaded.  
\[) CONTAINS  
containsEach annotation two key set \[) Metadatasample EM EMObs i
MetadataSali)ets EM EMObs \[) MetadataSan\] EM EMObs  
components:  
¢ Sample Metadata  
¢ EMObs  

SYNTHESIS oP oP oP  
A synthesis combines  
obs CAMPAIGN 1 GR CAMPAIGN 2 VR CAMPAIGN 3 NO  
abundance and body size e.g. 2020-06_south-west_stereo-BRUVs
e.g. 2020-10_south-west_stereo-BRUVs
e.g. 2025-04_south-west_stereo-BRUVs  
information from multiple  
campaignsven to uncover D \| \>s\>\> \|\| we‘ \|) DB \| oem\> \|) \|\|)
Dem\> \|) ;  
Sample Fish Benthic Sample Fish Benthic Sample Fish Benthic  
Metadata Annotations Annotations Metadata Annotations Annotations
Metadata Annotations Annotations  
ONE METHOD  
wis Amore synthesis containscampaigns that oneuse or  
the same method, e.g.  
stereo-BRUVs.  
Annotation data are curated and  
summarised using CheckEM, R  
workflows or other tools to generate  
datasets for analysis and reporting.  
Syntheses are created in  
CheckEM or via other tools  
to ensure custodians have  
oversight of data. - a  
(ah Syntheses can be Metadata data data count data relief data  
versioned and published  
with a DOI to provide an  
unchangeable source for  
\< SHAREABLEporns +3) SYNTHESISe.g. South-west Corner Marine Park  
Syntheses are shareable  
with other users and can METHOD: stereo-BRUVs VERSION: 1 DOI:
Available  
be accessed via API.  

# Using this guide

This manual details how to:

- Create an account and navigate on GlobalArchive,

- Upload an *Annotation Set, within a Campaign, within a Project,*

- Create and upload a Synthesis.

The manual is best used in an electronic form - as it uses links to
linked supporting Appendices and external resources.

# Useful links

- *GlobalArchive.org 2.0*

- *Example - BRUV Metadata*

- *Example - BOSS Metadata*

- *Example - Project metadata*

- *Example - Synthesis metadata*

GlobalArchiveP 2:0 mc®=~Seca ak fy ~ “% 7 ie2 BS4 \> ~ Ds  
eTrE, APUNUINNIVAWANSWSSN,“Shsaw +SOnr ss \>  
ae PRR AF) AN AW SANS y oy ty —~ ee =  
= : 16g 4 . ath  a. . 7 NS ) ” e \> a \_ =”  
; fe / \\  vy j . ‘— \> \>  
Sere thioib bY K; IDX XN SyNG yy  »\> AS Sa \> = =~ \>~ wy\>  
Welcome to GlobalArchive! Browse Archive  

Click LOGIN

Sign in to ORCID  
Don’t have your ORCID iD yet?  
an account yet  
Email or ORCIDiD  
Email or 16-digit ORCID iD  
have an account  
Password  
Your ORCID password  
Forgot. your password; OF ORCID 1D? ¢q———————————n\| Clickhave
forgottenhere if you  
or ORCIDID  
& in through your institution  

GlobalArchive 2082 ~ VQ z Le 4 = —. To switch from  
URIS SCaia ao oe SS  
2 os 4 LN et ¥% SANS s ees a NOW ——  
z 2 ae  NN \\ AY » va es \> . ~—  
eoRAN+a ea ne 4),  NNWWRAY. WieoSaN\_ LS (WSSSeesINN RS. SS“\> Se  
Soy bet AL SY AA AWAI RIN ee ee \| 10 upload data  
Welcome to GlobalArchive! Browse Archive  
logout atistics  
Ean ne sco susuiisatnin nemo tungsten oe View GA  
\_ \_ \_ — statistics  

GlobalArchivesoar as » Ys2. Onis—S SNE6 wsA Ze°\*- eS23\> —\_  
Se SE NIWA SSS Sas ae Se  
Et od ALN Yin Baal Sale ¥

ee RANA Se  
® TAINS AN WTS SYA Rae  
ow \\ 85 . \> \>  
Welcome to GlobalArchive! Browse Archive  
GlobalArchive.org P  
Main menu = \_\_sImport Annotation Set  
39 Projects  
Create or select a Annotation Set  
\> Campaigns  
A Syntheses —~  
o Map  /  
2) Import A  
r Import Annotation …  
A Import Synthesis  

{…} Create Annotation Set  
Annotation Set Name  
Annotation Metadata Fields  

% Create Campaign  
Project\> a \> (+)» 4  
Method -  
Description  

9\$2 Create Project  
AbrolhosProject Name Marine Park ~ Parks Australia - @) ¢ 1. Fillwithin.
cut information.this box .  
Local .  
ManagementType \*  
Data Sharing  
Fully public  
Shared with  
Brooke Gibbons Tim= AnmikaLewnig © Abbey Gibbons = Henry Jacques Fyans
.  
Description  
stereo-BRUV in the Abrolhos Marine Park  
Funding Sources  
Yamatji Southern Regional Corporation  
Ackneniedgemencs  
Thank you to Chris Beissel and Tom from Kamikaze, and Tom MacTavish from
the New Zealand  
Department of Conservation for their assistance in the field. \|  
fete 2. Glick CREATE  

> Create Campaign  
> Project as,  
> Abrothos Marine Park-Parks Australia - ( + }  
> Campaign Name  
> Abrolhos ~ Qe  
> Methacl  
> stereo-BRUVs \*  
> Description  
> stereo-GRUVS in the Abrolhos Marine Park for the Parks Australia  
> Survey Design Description  
> Spatially Balanced  
> Samoling Design Source Code Link  
> <https://github.com/UWA-Marine-Ecology-Group-projects/abralhos/tree/main/R/sampling-design>  
> 1. Fill out information  
> within this box  

3% Create Campaign  
\|  
Pian dapioymeant dination \| mia  
a0 Ue ibe fe  
41. Fill out information  
Aah Type within this box  
Pilchards *  
1000 oa ie \[ae  
Bad deployment  
Plastic costed wire mesh bag*  
Mushed *  
400Minimum separalion dalance of saenples (rm ,* te *  
GoPral2*  
amen POW sering  
mediuinn *  
‘i x ~ a  
300 a)“  
Baur1.20ann length (mi) .xo oa  
Armificial Gigiring  
White*  
2. Click CREATE  

oe Create Campaign  
Ball type  
Pilchards .  
Haat quantity (9) in  
1000 x Press his crass  
\> Create Campaign  
Bait Type  
Pilchards .  
It will then show up  
like this  

re 1. Glick on drop down or  
start typing  
Externally Allocated DOI  
2. Select campaign ta  
Method Metadata Fields copy from  
Copy metadata from existing Campaign F  
a 3. Click APPLY  
2021-05_Abrolhos_stereo-BRUVs  
Project: Abrolhos Marine Park-Parks Australia, Campaignic:
2021-05_Abrolhos_stereo-BRUVs  
2023-02_Yamatji-Shallow-Bank_stereo-BRUVs \|  
Project: Abrolhos Marine Park-Parks Australia, Campaignid:
2025-02_Yamatji-Shallow-Bank_stereo-BRUVs  
2025-06_Clio-Bank_stereo-BRUVs  
Project: Abrolhos Marine Park-Parks Australia, Campaignic:
2025-406_Clio-Bank_stereo BRUVS  
2022-11_Abrolhos?\_stereo-BRUVs  
Project: Abrolhos Marine Park-Parks Australia 2, Campaignid:
2022-11_Abrolhos?\_stereo-BRUVs  
2022-11_Salisbury_stereo-BRUVs .  
Project: Eastern Recherche Marine Park, Campaignid:
2022-11_Salisbury_stereo-BRUVs  
?025-06_Wudjari_stereo-BRUVs  
Project: Eastern Recherche Marine Park, Campaignid:
2025-06_Wudjari_stereo-BRUVS  
Pane ™  
Bait consistency ¥  
Minimum separation distance of samples (1m) x “  
REATE 4. CREATE  

{…} Create Annotation Set  
2021-05_Abrolhos_stereo-BRUVSs . f ; 1. Select campaign  
aT align A tation Set Name  
UWA Standard Example .  
UWA standard  
Annotation. Metadata Fields. 2. Fillwithinout thisinformationbox  
\>opy metadata from existing Campaign Annotation Set i & APPLY  
Yes .  
Mobile fauna annotation software (e.g. for fish  
EventMeasure-stereo .  
Benth labitat and relief a tatior ftware  
TransectMeasure .  

{…} Create Annotation Set  
Poa score of benthas i  
ee This is an example,  
Point score of relief . all campaigns won’t  
be identical  
Ves *  
No .  
All individuals ID’d to species bevel where possible .  
All individuals ID’d to epeces evel where possible*  
Not commatently recorded *  
Not consistently recorded . 2. Fill out information  
within this box  
Not recorded*  
Not recorded .  
All individuals ID’d to familly level where possible “  
All species where possible .  
Fork length .  
All species where possiibbe”  
Fork length *  
Not measured*  
Not measured \*  
3. Create  

GlobalArchivei 2:0…RS 23S es i aeeB aT:  
aI ee ee  
oy ETI WS SS SSS SE  
eS ay ANe WAS Jy) StS ae —=  
a. 46.4 Wt NA en  my KAI Sa Re \* \> —\_—  
¥ arr Po Y S S Wry RY  \> .-\* \>  
OOF a1 77 JN SAS ON Yi § ONS \>  
ane  NY Wit UNAS \>  
py Logi SAL SENN WtVO OUT SN eo a SS  
Welcome to GlobalArchive! Browse Archive  
This will open  
this  
Cre§te or select a Annotation Set  
{..} Create Annotation Set  
2021-052022-11_Abrolhos_Lobster-pots2025-02_YamatjAbrothos_stereo-BRUVsShutlon
bore nen : \|1 campaign4. Select  
2025-06 Clio oor wereo BRUVs - .  
2022-11_Salisbury_stereo-BRUVs  
2025-06_Wudjari_stereo-BRUVs  

Selected Annotation Set: -  
Files in Annotation Set: “  
same ‘Size Owe Samus Fie Type Actors,  
1. Click here to  
rm select files to  
import or drag files  
into this section  
Vocabulary Selected: -  
Issues  
Overview This will open up  
Type Descripece Afectea tows Cause your documents  
3D POINT DATA LENGTH DATA  
)s  
-  
t This PC SCI-MEGFISH-001 (\drive.irds.uwa.edu.au) (Z: Project Folders »
2021-05_Abrolhos Stereo \_BRUVs BOSS » Working Video Analysis \>»
BRUVs  
a ’ .  
Quick access  
WW Desktop CAAB~ Codes F 4  
- Calibration 2022 2 \| File folde  
\$ Downloads Cant 0 , ; \<br\>Documents Cool Clips ‘ File folde  
& Pictures EM Export f 1  
Video Analysis EM_Output_Jess 2 ‘ F j 2. Select the files  
EventMeasure Event Measure 4 File folde to import  
Coolo - Clips Event Measure - Jessi f 4  
Event Measure MaxN by size class File fold  
EventMeasure FieldFishnClipzrecording- sheet scans 2021 9:22 4 FileF
folde:{  
GA upload example Habitat images F j  
to upload Habitat Schemas f {  
@ OneDrive- UWA QAQC testing 2021 9:22 F fe  
Representative clips A F {  
Attachments TM Export F {  
\[BB Desktop Transect Measure ’ .  
Documents g® 2021-05_Abrothos_stereo-BRUVs_metadata 2026 2 \| Mocr  
Meetings cit) Project Specific Video Analysis Rules 4 A I  
Microsoft Copilot Chat Files  

oO Group Manne Sine Date Status File Type Actions  
1. Press the Ch Sample Metadata Fite (1)  
dropdown  
. 2021-05_Abrolhos_sterec- 16.19 11/06/2026, e al  
O BRUVs_metadata,cav. KB 14:02:06 ra 5. \| in.\|  
oO vy EMObs File (49)  
O np26.10_EMObs.. KB16.38 11/06/2026,13:53:11 ¥a E.. ¥ ia  
\_  
= 14.06 11/06/2026, ot  
L np26.13.EMOba“ . KB 12:53:11 , “ E. aa  
2. Check the  
status  

Selected Annotation Set: oa  
Files in Annotation Set: a  
i Group Name Size Date ‘Status File Type 4 Actions  
(\> Sample MetadataFile (1) ¥v  
CO \> EMObs Fite (60) Y  
r Add files to Annotation Set e  
Select Vocabulary Used a  
Australian aAquatic Fauna (CAAB+WORMS+FishBase) = 3. Click drop down  
Australian Aquatic Fauna (CAAB+WORMS+FishBase)  
Issues 4. Select the  
oh vocabulary used  
Type Description Affected Rows Cause  
Loading import preview… This might take @ lite while - please be
patient  

##### Check for Issues

- Scroll to the *Issues* section.

- The *Issues* section lists any problems detected in the uploaded data.
  Each row shows:

  - the type of issue

  - a description of the issue

  - the percentage of rows affected

- The *Type* column indicates the severity of the issue:

  - ℹ Info: General information about the data. These messages do not
    prevent the file from being imported but may highlight something
    useful to review.

  - ⚠ Warning: A potential problem that should be checked before
    importing.

  - The file can usually still be imported, but some rows or values may
    need attention.

  - ❗ Error: A problem that must be fixed before the file can be
    imported. Errors usually indicate missing required fields, invalid
    values, or formatting issues that prevent the import from
    continuing.

- A detailed explanation of individual errors/warnings, common causes
  and trouble shooting tips can be found in Table X. Coming soon…

- Use the 👁 icon to filter the data view so that only the rows causing
  the selected issue are displayed. This is useful when you want to
  inspect the affected records directly, check what needs to be
  corrected, or focus on one issue at a time without viewing all the
  problematic rows at once.

- In the *Cause* column, hover your cursor over the ❓ symbol to view
  more details about the issue, including the affected file, columns or
  rows, and the percentage of rows affected.

Description of the  
Issue  
Issues  
Overview  
— Affected  
. Type Description Rows Cause  
ri\] Dropping unsupported columns from Sample Metadata file. @  
- Thecolumn3D Point Annotation data contain rows with blank or missing
values in the period e\_  
- Column ‘number in 3D Point Annotation data is expected to be integer
valued, but contains =  
values which cannot be converted to integers. These rows will be filled
with 0. oY  
The Length Annotation data contain values in the period column. Since
this column was not  
= used in the given Sample Metadata, these values will be ignored when
matching to ey  
0 Samples. If you intended to use this column to identify Samples,
please ensure that the Y  
conmesponding column in the Sample Metadata contains values.  
The Point Annotation data contain values in the period column. Since
this column was not  
used in the given Sample Metadata, these values will be ignored when
matching to \_  
: 0 Samples. If you intended to use this column to identify Samples,
please ensure that the oe  
corresponding column inthe Sample Metadata contains values.  
The 3D Point Annotation data contain values in the period column, Since
this column was  
: not used in the given Sample Metadata, these values will be ignored
when matching to =  
0 Samples. If you intended to use this column to identify Samples,
please ensure that the ©  
a comesponding column in the Sample Metadata contains values.  
AN  
Use this to focus on Type of issue To see what  
one issue at a time in columns/rows are  
the data view frame affected  

Data View  
3D POINT DATA LENGTH DATA  
Sepage EEE—EE  
File od Row \# filename frame time_mins opcode period period_time_mins
imx imy rectx  
‘SB-8V-002.EMObs 1 62_GX010040. MP4 ans 2.622063888888889 ‘SB-8V-002
0.2713822222222224 404_70588235294116 445.8823529411/646 1  
SB BV.002.EMObs 2 L62_GX010040.MP4 6408 3.5635600000000003 SB BV 002
1.2128783333333337 917.6470588235294 670.5882352941177 1  
S0-BV-002.EMObs 3 L62.6X010040.MP4 8303 4.617390555555556 SO-BV-002
2.266708888880889 —64.70508235294117 \_661.1764705882352. 1  
SB-BV-002.EMObs 4 L67_Gxo10040.MP4 8684 4.8292688888888895 —*SB-BV-002
2.478587222227273 «292.9411 7647088823 931.7647058873529 -1  
SB BV.002.EMObs 5 L62_GX010040.MP4 12535 6.970852777777778 SB BV 002
4.6201719191119115 580 622.3529411764706 1  
SB-BV-002.EMObs 6 L62.6x010040.MP4 13120 7296\77777777777-—=«SB-BV-002
A945A96111111111* -1196.4705882952041 522.9529411764706 1  
s8-8V-002.EMObs 7 \62.Gxo10040.MP4 13120 1L206\7TTTTITTTT §~—S8-8V-002
4945496111111 976.47058823570414 —514.1176470588735 1  
SB BV.002.EMObs 8 L62.Gx010040.MP4 1376 7.660986666666667 SB BV.002
5.3103050000000005 1624.7088823529412 750.5882352941177 1  
S0-BV-002.EMObs 9 L62.GX010040.MP4 1376 7.660986666666667 \_SD-BV-002
5.3109050000000005 1201.1764705882354 656.4705882352941 1  
SB-8V-002.EMObs 10 162.Gx010040.MP4 13776 7.660986666666667 SB-8V-002
5.3103050000000005 791.7647058823529 498.8295204117647 1  
SB BV.002.EMObs ” L62.GX010040.MP4 15663 8.710368333333333 \_SB.BV.002
6.359686666656667 -1768.235204117647 483.52941176470586 1  
SB-BV-002.EMobs 12 L62.GX010040.MP4 15663 8.710968333333333 \_SB-BV-002
6.359686666666667 952.9411764705883 544,7058823520412. 1  
‘SB-8V-002.EMObs 3 162_GX010040.MP4 15663 8.710368333333333 ‘SB-BV-002
6.359686666666667 725.8823529411765 467.05882357941177 1  
SB BV.002.EMObs 14 L62_GX010040.MP4 15663 8.710368333333333 \_SB.BV.002
6.359686666666667 240 628.2382041176471 1  
S8-8V-002,EMObs 15 162.6x010040.MP4 18790 VOAA9S27777777777 — SB-BV-002
8.09864611111111 105.88295794117646 877.6470588735794 1  
ee ,  
To view more/less To navigate To import  
on each page through pages P  

Issues  
Overview  
Type mcripion AMT ecteed couse  
o Croppingunaupporeed columns fram Ramepie Kirtanfile a  
viewed  
.?  
o@  
oo  
o TY\]  
Data View  
Poin cata Leworn para  
Fite ad fom ee Slename lett filename right faree_leh ‘Treen ye_ringtet
Tite mine eapeenctie Pere i Meat irea_tett  
rev? HuaMoes 1 Loma (17d eo 1} aan sms OIOMOETTTTTTTTS peu. / 4 Io
Taaranaare\|  
rere EMObs 1 LO3S (02). MP4 ADDS (924s wa 12486 DOATZ BS) GGG? npr. 9 /~
4 1627. 93000 7900 508  
roe. a LOR? (OoLMMa = NER (ONL uae ta08 DONO OMARNARLAT — ng.9 re 1 13E
mUOTEONIIETT  
nerd LEMObS 1 LOS! (02). ADS (Oz) Mae ian a TEOMOMIEIF =— np. 12 /~
1384. 57IGOBESERD  
nord 1REMDOR 1 LORE (0T7).MeA ANA (TZ)MAPA = TINS ano OSESDONMAEAEIE mgd
re 1 170048361 8802318  
nga? MCs 1 Loan (Deja RCD oe) na 2230 20rd pe er td é~ 4 TOOL 1 eeyzE
vena\]  
ner.) MOGs: 1 LO? (07). MPa Aa) (02). MPS 2900 anar 30.8380 17227227003
npr. 1 /~ 4 VSES. 755258126199  
Opcodes with ; The issue is there is  
the issue no period data  

GlobalArchive.orgMain menu \_— ImportSynthesis  
32 _(—s«) Projects  
=e Campaigns Create or select a synthesis  
A, Syntheses —  
 + \| Select a synthesis  
1 oO Map Nt ,  
5) Import “A  
~ Import Campaign …  
2 a Import Synthesis  

7  
Synthesis Mame  
Abrolhos Marine Park-YSRC 2 = e  
Method  
stereo-BRUVs .  
Local .  
Management .  
<https://github.com/UWA-Marine-Ecology-Group-syntheses/>
Abrolhos-Marine-Park-v2_git <https://gith>  
\* 1. Fill out  
Analysis Source Code Link information  
within this box  
Externally Allocated DO)  
Data Sharing  
Fully public  
Shared with  
Brooke Gibbons Tim Langloss \*  
Description  
stereco-BRUYV in the Abrolhos Marine Park-YSRC funded by Yamatji
Southern Regional Corporation  
¥  
2. Create  

GlobalArchive.orgMain menu = \_\_Import Synthesis, ©  3 R \>Pe we\| .-
~  
22 Projects  
\>= Campaigns Create or select a Synthesis  
A sSyntheses -  
4 Select a synthesis Search for  
oO Map Synthesis  
2) Import A Abrolhos Marine Park-YSRC  
Version 1, Method: stereo-BRUVs, Created: 2026-05-26, Latest sample:
2025-06-27 Select  
\* Import Annotation … Abrolhos Marine Park-YSRC Synthesis  
A Import Synthesis. Version 1, Method: stereo-BOSS, Created: 2026-07-06
atest sample: 2021-05-14  
Eastern Recherche Marine; Park  
Version 1,Method: stereo-BRUVs, Created: 2026-05-26, Latest sample:
2025-07-02  
Eastern Recherche Marine Park  
Geographe Marine Park  
Version2,Method: stereo-BOSS, Created: 2026-07-07, Latest sample:
2024-04-18  
Geographe Marine Park  
Version 3, Created: 2026-06-02, Latest sample: 2024-04-17  
Selected Synthesis: ~  
Files in Synthesis: . 1. Click here to  
select files to  
O nem Sze Date ‘Saatus File Type 4 ‘Actoms import or drag files  
No fen attedt0 in Rymneain yor into this section  
A e This will open up  
your documents  
€ T Abbeys Github (E:) \>» Abrothos-Marine-Park-v2 data \> uploads  
=\| Documents a Jame Date modified Type ae  
=) Pictures p2/ 2021-05_Abrothos\_ stereo-BRUVs_bentho.. 10:50 AM
ficrosoft Excel f  
Video Analysis ig) 2021-05_Abrolhos_stereo-BRUYs_bentho.. 6/2026 10:57
AM Microsoft Excel B  
EventMeasure ip 2025-02_Yamatji-Shallow-Bank stereo-B . . 8/06/2026
11:16 AM Microsoft Excel C 19 KB  
Cool Clips ig! 2025-02_Yamatji-Shallow-Bank_stereo-B… 16 AM ficrosoft
Excel f 2 Select the files  
2022-12_Daw_stereo-BOSSEventMeasurea g#/jg)
2025-06_Clio-Bank_stereo-BRUVs_bentho..2025-06_Clio-Bank_stereo-BRUVs_bentho…
8/06/20266/2026 11:14 1 :14 AMAM MicrosoftMicrosoft ExcelExcel C 7KBf to
import  
g4! Abrolhos-Marine-Park-v2_count PM ticrosoft Excel i  
GA upload’upload exempreI \*/ Abrolhos-Manine- Park-v2_length 5/2026
2:53 PM Microsoft Excel 66 KE  
to uploed 2) Abrolhos-Marine-Park-v2_metadata 27/05/2026 2:53 PM
Microsoft Excel C 12 KB  

Selected Synthesis:  
Files in Synthesis:  
OO mame Size Date Status File Type Actions  
o Abrolhos-Marine-Park v2_mevadata.cav 41.29 KB 2705/2026, 14:54:00 wf
Sample Metadata File \* \| ns \|  
QO Abrothos-Marine-Park-v2_count.cey 197.12 KB Z7N5/2026, 14:54:01 wf
Count File . in  
Oo Abrolhog-Marine-Park-v2_lengtn.cav 1.04 MB 2705/2006, 14:54:01 we
Length File . ia  
San file Type  
( -2021-05_Abrotnos_stereo-BRUVs_bemhos-courtcsv 26.19 KB 08/06/2026,
11:01:27 a Benthos Count File ~ ia  
set fle Tyne  
(Os 2025-02_vamalj-Shallow-Bank_stereoBRUVs_benthas-counLcey 40.45 KB
27/05/2026, 0-48 a Benthos Count Fille ” ia  
set fhe tyne  
oO 025-+06_Clioank_sterecHALiVs_benihos-counLosv 71.69 KB PFS 2076,
00-2017 ae Benthos Count File . i a  
1, Check the  
status  

O tex“snes v moyen 11/06/2026,14:09:54  
1. Click in this box  
Sample Metadata File  
Count File This will open this  
drop down menu  
Length File  
Benthos Count File  
2. Select the  
Benthos Length F File appropriate. file\| type  
Benthos Relief File  
Benthos Count File .  

gy REL Nemep atow tank ren One bentoe asia 77/8/2028 v Benthos Relief
File . ia  
1. Select the  
check boxes  
2025-06. Cho Bank stereoBAUVE_benthos-elief csv 16.70KB a on v Benthos
Relief File , ia  
2. Click delete  
DELETE SELECTED FILES + selected files  
id t e  

Select Vocabularies Used a  
sc Fh cab sc ens Vecsey 1. Click the drop  
Australian Aquatic Fauna (CAAB+WORMS+FishBase) Australian Benthic Biota
and Substrate (CATAMI+) \* down  
Australian Benthic Biota and Substrate (CATAMI+)  
2. Select the  
vocabulary used  

= \_\_s Import Synthesis Logged \| « 9} Oct ars Daz fe)  
O —\_2025-06_Clio-Bank_stereo-BRUVs_benthos-countcsv 21.69 KB
30/06/2026, 22:10:41 v Benthos Count File . \| \|  
a) 2021-05_Abrothos_stereo-BRUVS_benthos-relief.csv 9.74 KB 30/06/2026,
22:10:41 ¥ Benthos Relief File . it a  
oO 2025-02_Yamaitji-Shallow-Bank_stereo-BRUVs_benthos-telief.csv 34.31
KB 30/06/2026, 22:10:41 vv Benthos Relief File ~ it a  
0 = 2025-06_Clio-Bank_stereo-BRUVs_benthos-telief.csv 16.79 KB
30/06/2026, 22:10:41 ¥ Benthos Relief File . 5 a  
fy Add files to Synthesis e  
Vocabularies Selected: v  
Issues  
Type Description Affected Rows Cause  
Perfect! No errors or warnings raised for this import.  
1. Check for 2. Import  
Issues Data  

# Frequently Asked Questions and Tips

### Frequently asked questions

Can *Projects* include multiple methods?

- Yes, *Projects* can include multiple methods.

Can *Syntheses* include multiple methods?

- No, *Synthesis* must only contain one method. Separate *Syntheses*
  must be made for different methods.

- If I am uploading for someone else what information do I need to get
  from the custodian? - *Here* is a spreadsheet to get the custodian to
  fill out

### Tips

- Light and Dark Mode: In the top right corner of the landing page you
  can click the ☼ to switch between light and dark mode

# Appendicies

### Appendix 1 - Definitions

##### Project and Campaign terms

###### Project

- Contains one to multiple Campaigns with a shared purpose/objective
  (e.g. monitoring of a Marine Park, a bioregional study).

- *Project is a unique identifier and the name should be carefully
  chosen (e.g.* “MarineParkMonitoring” is not a good Project name but
  “Houtman Abrolhos Reef Observation Areas long-term monitoring” is a
  great Project name)

- Sampling and image analysis methods are generally standardised within
  a *Project* but sampling method for each Campaign can be different
  (e.g. one *Campaign could* use DOV whereas another Campaign could use
  BRUV or UVC).

###### Scale

- The geographic extent at which the dataset, Project, or *sample* is
  relevant or intended to be applied (e.g. International, National,
  Regional, or Local).

###### Type

- The category that best describes the primary context or intended use
  of the dataset or project.

- Cultural, Academic, Industry or Management

###### Campaign

- A discrete set (temporal and spatial) of *samples.*

- All *samples within a Campaign* use the same sampling and image
  analysis methods.

###### CampaignID

- A unique identifier for a *Campaign made up of*
  YYYY-MM_Campaign_Method .

- The date is the start date of the *Campaign (e.g.*
  2023-12_Rottnest-Island-Marine-Parks_stereo-BRUVs ).

###### Annotation Set

- A collection of annotations from a defined set of samples within a
  Campaign , grouped together as a single dataset.

###### Annotation Metadata

- The descriptive information associated with an Annotation Set that
  provides context for the annotations and information on how the
  samples were annotated.

- For example levels of identification and software used to annotate.

###### Method

- Any sampling *method defined by a user (e.g. stereo-BRUVs,
  stereo-DOVs, UVC).* Defined in the “Methods” tab.

###### Sample

- Single observational unit (e.g. a BRUV deployment or DOV transect).
  Must match exactly across sample metadata and any annotation/EMObs
  files. Defined by opcode only, period only, or opcode + period.

- 

###### Vocabularies

- The approved controlled vocabularies used in EMObs to standardise
  annotation of fish, benthic biota, and substrates.

- These ensure annotations are applied consistently and can be compared
  across *projects and datasets.*

##### Synthesis terms

###### Synthesis/Syntheses

- Combines information from multiple Campaigns with the same *method
  from multiple* locations and/or multiple times.

###### Scale

- The geographic extent at which the *Synthesis* is relevant or intended
  to be applied

- International, National, Regional or Local

###### Type

- The category that best describes the primary context or intended use
  of the *Synthesis.*

- Cultural, Academic, Industry or Management

##### User Access and Sharing terms

###### Custodian

- A *user* who creates a *Project, and the Campaigns* within it, is the
  Custodian.

- Only the Custodian can upload *Campaigns* to that Project and share
  the *Campaigns* or the entire Project with other Users.

- The Custodian should be the person within the institution with a long
  term interest in the data and the person who should be contacted if
  another user wants to use the data (e.g. will more likely be a
  Principal Investigator than a research assistant).

\[ + }-—FrrgramEa) EventMeasure:Picture TR-BV-CP8.EMObsMeasurement
Stereo: 20260505_BRUVAbout 3_L19\_¢  
Current settings … ” \[  
View reference images …  
File concatenation utility … 3 mins)  
Batch text file output …  
Batch binary file concatenation …  
\[2 +s Generate speciesdatabaseoutput file … …  
Text to/from EMObs \>  
Batch generate skeleton EMObs …  
Batch change picture directories …  
Close  

3.  Click & select  
    where EMObs  
    are saved  
    Database table generation  
    File  
    Marne Data Extra into  
    Input file directory = WS
    Folders\*2021-05_Abnolhos_Stereo_BRUYs_BOSS‘vorkingBAL svent Measune
    Directoy: Selection Dinecton! where EM Obs files are located  
    Qutput fle drecton v2
    Folder’2021-05_Abnolhos_Stereo_BRUWs_BOSS‘WorkingAnakeisExport
    Directoy Selection Dinecton! where database files are generated  
    Base name wf 2021-06_Abrothos_steeo-BR Us  
    4. Click & select  
    where files will be  
    saved  
    5. Enter  
    Campaign name  
    L Cancel \| Process:  
    6. Click Process  

###### Database generation summary

Clear Save text Using: npz6.BOSS41.EMObs A Using: npz6.C01.EMObs Using:
npz6.C02.EMObs Using: np29.1.EMObs Using: npz3.10.EMObs Using:
npz9.11.EMObs Using: np2z9.12.EMObs Using: npz9.13.EMObs Using:
npz9.14.EMObs Using: np29.16.EMObs Using: np29.17.EMObs Using:
np29.18.EMObs Using: npz9.15.—EMObs Using: np29.2.EMObs Using:
np29.21.EMObs Using: np2z9.22 EMObs Using: npz9.24.EMObs Using:
npz9.25.EMObs Using: np29.26.EMObs Using: npz9.27.EMObs Using:
np29.28.EMObs Using: npz9.29.EMObs Using: np29.3.EMObs Using:
np29.30.EMObs Using: np2z9.4.EMObs Using: npz9.5.EMObs Using:
np29.7.EMObs Using: np29.8.EMObs Using: npz9.5.EMObs Files inspected: 49
Files used: 49 ¥

rx \| 2021-05_Abrolhos_stereo-BRUVs_3DPoints 29/06/2026 9:29 4M TXT File
49 KB rx \| 2021-05_Abrolhos_stereo-BRUVs_ImagePtPair 9 29/06/2026 9:29
AM TXT File 115 KB rx \| 2021-05_Abrolhos_stereo-BRUVsInfo 29/06/2026
9:29 AM TXT File 2 KB rx \| 2021-05_Abrolhos_stereo-BRUVs_Lengths
29/06/2026 9:29 AM TXT File 207 KB rx \| 2021-05_Abrolhos_stereo-BRUVs
MovieSeq 29/06/2026 9:29 AM TXT File 39 KB a
2021-05_Abrolhos_stereo-BRUVs_Period 29/06/2026 9:29 AM TXT File 4KB rx
\| 2021-05_Abrolhos_stereo-BRUVs_Points 29/06/2026 9:29 AM TXT File
1,267 KB a 2021-05 *Abrolhos* stereo-BRUVs Source 29/06/2026 9:29 4M TXT
File 4KB

The component is an option and you will need a specific licence to
access it. To check if you have this, on the EventMeasure window go to
the menu About then Version. Under the Installed components, Database
output should be listed. Otherwise contact James Seager.

The datatable export function (Program\>Generate database output) will
generate 9 data tables from a discrete set of .EMObs files that make up
a*C ampaign.*

Common mistakes and troubleshooting tips

- Duplicated OpCodes in the information fields across .EMObs files will
  cause an error, and will only export data from the first EMOBs file
  with that opcode.

Check the information fields for duplicates and rename the duplicated
ones.

pe! Abrolhos-Marine-Park-v2_count 2! Abrolhos-Marine-Park-v2_length fp!
Abrolhos-Marine-Park-v2_metadata

27/05/2026 2:53 PM 27/05/2026 2:53 PM 27/05/2026 2:53 PM

Microsoft Excel C.., 198 KB Microsoft Excel C.., 1,066 KB Microsoft
Excel C.., 42 KB

### Appendix 4 - Format required for Import

##### Sample Metadata

- A sample metadata file is required for both Annotation and Synthesis
  imports.

- Sample metadata must be provided as a comma-separated values (.csv)
  file.

- We recommend naming the file using the format

  - YYYY-MM_Project-name_Method_metadata.csv (e.g. 

  - 2026-06_Abrolhos_stereo-BRUVs_metadata.csv ) to ensure compatibility
    with CheckEM, although any file names are accepted.

- GlobalArchive requires specific columns (listed in Table 1), including
  defined formats and validation rules applied during import (see
  example in Table 1).

- Any unsupported columns will be ignored and removed during ingestion.

**Table 1.** Column and formatting requirements of the sample metadata
file, note the tables described below are transposed (rows for columns)
for formatting convenience.

[TABLE]

[TABLE]

**Table 2.** First three lines of an example Annotation sample metadata
file from a stereo-BRUVs *Campaign that used additional backwards
facing* cameras for habitat annotation. The opcode column is used to
define the sample.

[TABLE]

###### Metadata Examples

- We have provided example templates for both *BRUV* and *BOSS*
  metadata.

- In both templates:

  - The coloured column headings are the headings that GlobalArchive
    requires. Uncoloured columns are optional.

  - Row 2 provides descriptions of the expected data for each column.

  - Row 3 & 4 contain example entries.

  - These templates can be copied for your own metadata, but rows 2–4
    must be removed before entering new data.

##### Sample Metadata for Synthesis

The sample metadata requirements for Annotations and Syntheses are very
similar. The primary difference is that synthesis metadata must include
a *campaignid column (Table 3). This is required because a synthesis may
combine data from multiple Campaigns, and the campaignid* identifies the
Campaign associated with each *sample.*

● Recommended naming suffix: \_metadata.csv

**Table 3** . First three lines of an example *Synthesis sample metadata
file from a stereo-BRUVs Campaign that used additional backwards facing*
cameras for habitat annotation. The opcode column is used to define the
sample.

[TABLE]

##### Count

- A Count file is required for *Synthesis* imports.

- Count file must be provided as a comma-separated values (.csv) file.

- Recommended naming suffix: \_count.csv

**Table 4** . First four lines of an example Count file from two
stereo-BRUVs *Campaigns* ready to be imported into a Synthesis. The
opcode column is used to define the sample.

| **campaignid** | **opcode** | **family** | **genus** | **species** | **count** | **stage** | **caab_code** |
|----|----|----|----|----|----|----|----|
| 2021-05_Abrolhos_stereo-BRUVs | npz6.10 | Carangidae | Seriola | dumerili | 5 | AD | 37337025 |
| 2021-05_Abrolhos_stereo-BRUVs | npz6.10 | Monacanthidae | Nelusetta | ayraud | 1 | AD | 37465006 |
| 2025-02_Yamatji-Shallow-Bank_stereo-BRUVs | SB-BV-007 | Sparidae | Chrysophrys | auratus | 38 | AD | 37353001 |
| 2025-02_Yamatji-Shallow-Bank_stereo-BRUVs | SB-BV-008 | Carangidae | Pseudocaranx | spp | 1 | AD | 37337924 |

##### Length

- A Length file is required for *Synthesis imports.*

- Length file must be provided as a comma-separated values (.csv) file.

- Recommended naming suffix: \_length.csv

**Table 5** .First four lines of an example Length file from two
stereo-BRUVs Campaigns ready to be imported into a *Synthesis. The
opcode* column is used to define the sample.

| **campaignid** | **opcode** | **caab_code** | **count** | **family** | **genus** | **species** | **length_mm** | **precision_mm** | **range_mm** | **rms_mm** | **stage** |
|----|----|----|----|----|----|----|----|----|----|----|----|
| 2021-05_Abrolhos_stereo-BRUVs | npz6.21 | 37372907 | 1 | Pomacentridae | Chromis | spp | NA | NA | 1623.842 | 7.40469 | AD |
| 2021-05_Abrolhos_stereo-BRUVs | npz6.22 | 37355015 | 1 | Mullidae | Parupeneus | spilurus | NA | NA | 2735.874 | 25.38756 | AD |
| 2025-02_Yamatji-Shallow-Bank_stereo-BRUVs | SB-BV-051 | 37384923 | 1 | Labridae | Iniistius | spp | 131.1065 | 4.0386 | 2899.085 | 0.69228 | AD |
| 2025-02_Yamatji-Shallow-Bank_stereo-BRUVs | SB-BV-022 | 37351009 | 1 | Lethrinidae | Lethrinus | miniatus | 335.4285 | 3.09009 | 1632.069 | 1.07326 | AD |

##### Habitat

The strongly recommended benthos annotation method is to use a
random-point approach. Where 20 points are randomly selected in the
bottom 50% of the image. Using the bottom 50% of the image reduces the
amount of open water annotations.

The strongly recommended relief annotation method is to use a 5 x 4 grid
composing of 20 rectangles, we then annotate the relief of each
grid-cell by scaling them from completely featureless and flat, up to
highly complex.

##### Benthos Count File

- A Benthos Count file is required for Synthesis imports.

- Benthos Count file must be provided as a comma-separated values (.csv)
  file.

- Recommended naming suffix: \_benthos-count.csv

**Table 6** . First four lines of an example Benthos Count file from two
stereo-BRUVs Campaigns ready to be imported into a *Synthesis. The*
opcode column is used to define the *sample.*

[TABLE]

##### Benthos Length File

- A Benthos Length file is required for Synthesis imports.

- Benthos Length file must be provided as a comma-separated values
  (.csv) file.

- Recommended naming suffix: \_benthos-length.csv

**Table 7** . First four lines of an example Benthos Length file from
two stereo-BRUVs *Campaigns* ready to be imported into a *Synthesis.
The* opcode column is used to define the *sample.*

[TABLE]

##### Benthos Relief File

- A Benthos Relief file is required for *Synthesis* imports.

- Benthos Relief file must be provided as a comma-separated values
  (.csv) file.

- Recommended naming suffix: \_benthos-relief.csv

**Table 8** . First four lines of an example Benthos Relief file from
two stereo-BRUVs Campaigns ready to be imported into a Synthesis. The
opcode column is used to define the *sample.*

[TABLE]

The Length Annotation data contain values in the period column. Since
this  
column was not used in the given Sample Metadata, these values will be
ignored  
& oe when matching to Samples. If you intended to use this column to
identify 7)  
Samples, please ensure that the corresponding column in the Sample
Metadata  
contains values.  
The Point Annotation data contain values in the period column. Since
this column  
was not used in the given Sample Metadata, these values will be ignored
when  
® Ci\] Matching to Samples. If you intended to use this column to
identify Samples, 2  
please ensure that the corresponding column in the Sample Metadata
contains  
values.  
The 3D Point Annotation data contain values in the period column. Since
this  
column was not used in the given Sample Metadata, these values will be
ignored  
® \[i\] when matching to Samples. If you intended to use this column to
identify @  
Samples, please ensure that the corresponding column in the Sample
Metadata  
contains values.  

The Benthos Count data contain rows which reference Sample Metadata
which do not occur in the provided Sample Metadata files, or were
dropped by preceding checks. These a rows will be dropped, but this
means there is a serious problem with the input data. \_ e Perhaps some
Sample Metadata files were not submitted?

Some Samples have the exact same coordinates as other Samples within the
same campaign. Unless coordinates are recorded extremely imprecisely,
this is a sign that se locations may have been accidentally repeated.
Please review the data to ensure they are — correct.

Py  

### Appendix 6 - Data privacy and sharing agreements

##### Data Privacy

All data uploaded to GlobalArchive is held securely within NeCTAR Cloud
infrastructure within a secure industry standard database.

High-level metadata for all uploads (either in *Projects* or Syntheses)
is publicly available as outlined in the Privacy Policy. Data*Custodians
can choose whether Sample Metadata (e.g. )* and annotation, count or
length data are open-access.

Any associated data (e.g. species, count and length) can be “Shared”
with another User by the data Custodian.

The Administrator of GlobalArchive, can only delete a User’s data – not
access it.

##### Data sharing agreements

Formal data sharing agreements can be implemented as part of
GlobalArchive “Syntheses”, more information on this coming soon.

### Appendix 7 - Privacy Policy

We take the privacy of our users very seriously at GlobalArchive. Most
importantly, we do not and never will sell anyone’s information to a
third party.

This Privacy Policy gives a few more details about how we collect, use,
maintain and disclose information collected from you when you use our
site.

##### Personal identification information

We will collect personal identification information from you in a
variety of ways, including when you register on the site and share*C
ampaign* *s, Projects* or Syntheses with other users. When registering
users are asked for their full name, username, email address,
affiliation(s), short biography and to provide external links to
personal, institutional or other sites (e.g. Google Scholar,
ResearchGate).

We will collect personal identification information from you only if you
voluntarily submit such information to us by registering as a user. You
can always refuse to supply the information.

##### Google and cookies

This website uses*Google Analytics, a web analytics service provided by
Google, Inc. (‘Google’).* Google Analytics uses ‘cookies’, which are
text files placed on your computer, to help the website analyse how
users use the site. The information generated by the cookie about your
use of the website (including your IP address) will be transmitted to
and stored by Google on servers. Google will use this information for
the purpose of evaluating your use of the website, compiling reports on
website activity for website operators and providing other services
relating to website activity and internet usage.

Google may also transfer this information to third parties where
required to do so by law, or where such third parties process the
information on Google’s behalf. Google will not associate your IP
address with any other data held by Google. You may refuse the use of
cookies by selecting the appropriate settings on your browser, however,
please note that if you do this you may not be able to use the full
functionality of this website. By using this website, you consent to the
processing of data about you by Google in the manner and for the
purposes set out above.

##### How we use collected information

We will use your personal information for the following purposes:

###### **Publically available**

If you are the custodian of a Project, your username/email will be
publicly available on the site, via the Explore page.

###### **For logged in users**

Additionally, if a GlobalArchive user is logged in, they can also see
the email address, affiliation(s), short biography and external links
provided by each user. In addition, logged in users will be able to see
who is the custodian (or creator) of each Project or*Synthesis. For
Projects or Synthesis* that

were created by a user or shared with a user, that user will also be
able to see the usernames of each user that the Project or*Synthesis is
shared with.*

###### **To send periodic emails**

We may use the email address to send you information and updates about
the site. It may also be used to respond to your inquiries, questions,
and/or other requests. To be completely clear about this — you’ll only
get these from us, we’ll never pass them on to “partners” or anything
like that.

##### How we protect your information

We adopt appropriate data collection, storage and processing practices
and security measures to protect against unauthorized access,
alteration, disclosure or destruction of your personal information,
username, password, transaction information and data stored on our site.

All communication between the site and you happens over an SSL secured
channel and is encrypted and protected with digital signatures.

##### Sharing your personal information

We do not sell, trade, or rent your personal identification information
to others. We may share generic aggregated demographic information not
linked to any personal identification information regarding visitors and
users in academic publications or conference presentations.

##### Third party websites

Any other sites that we link to will have their own privacy policies,
which might differ from ours.

##### Changes to this privacy policy

We have the right to update this privacy policy at any time. If we do,
we will revise the updated date at the bottom of this page and email all
users.

##### Your acceptance of these terms

By using this site, you signify your acceptance of this policy. If you
do not agree to this policy, please do not use our site. Your continued
use of the site following the posting of changes to this policy will be
deemed your acceptance of those changes.

##### Contacting us

If you have any questions about this Privacy Policy, the practices of
this site, or your dealings with this site, please contact
<tim.langlois@uwa.edu.au>
