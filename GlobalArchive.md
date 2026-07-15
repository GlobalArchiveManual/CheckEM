## **<u>GlobalArchive 2.0 How to Guide</u>** 



#### Table of Contents 

|**About GlobalArchive............................................................................................................. 4**|
|---|
|Overview - uploading Annotations......................................................................................5|
|Overview - creating a Synthesis.........................................................................................6|
|Using this guide........................................................................................................................7|
|**Useful links............................................................................................................................. 7**|
|Create an account....................................................................................................................8|
|Navigating the GlobalArchive landing page....................................................................... 9|
|Uploading Annotations........................................................................................................... 10|
|1. First, create a Project and Campaign to hold Annotations...........................................10|
|2. Create a Project............................................................................................................11|
|3. Create a Campaign...................................................................................................... 12|
|Campaign Method Metadata...................................................................................... 13|
|Copying Method Metadata from existing Campaigns.................................................15|
|4. Create Annotation Set..................................................................................................16|
|Importing Annotations...................................................................................................... 19|
|Check for Issues.........................................................................................................21|
|Creating a Synthesis.............................................................................................................. 25|
|What is a Synthesis?........................................................................................................25|
|Creating a Synthesis on GlobalArchive............................................................................25|
|Importing data into a Synthesis........................................................................................27|
|Choosing a Vocabulary.............................................................................................. 29|
|Check for Issues.........................................................................................................29|
|Frequently Asked Questions and Tips................................................................................... 31|
|Frequently asked questions............................................................................................. 31|
|Tips...................................................................................................................................31|
|**Appendicies..........................................................................................................................32**|
|**Appendix 1 - Definitions................................................................................................ 32**|
|Project and Campaign terms......................................................................................32|
|Project.................................................................................................................. 32|



|Scale.....................................................................................................................32|
|---|
|Type......................................................................................................................32|
|Campaign............................................................................................................. 32|
|CampaignID..........................................................................................................32|
|Annotation Set......................................................................................................32|
|Annotation Metadata............................................................................................ 32|
|Method..................................................................................................................33|
|Sample................................................................................................................. 33|
|Vocabularies.........................................................................................................33|
|Synthesis terms..........................................................................................................33|
|Synthesis/Syntheses............................................................................................ 33|
|Scale.....................................................................................................................33|
|Type......................................................................................................................33|
|User Access and Sharing terms.................................................................................33|
|Custodian............................................................................................................. 33|
|Projects.................................................................................................................34|
|Synthesis..............................................................................................................34|
|User......................................................................................................................34|
|Appendix 2 - How to export from EventMeasure..............................................................34|
|Generating datatables from EventMeasure................................................................36|
|Common mistakes and troubleshooting tips.........................................................37|
|Check the information fields for duplicates and rename the duplicated ones.................. 37|
|Appendix 3 - R Workflows and GitHub template..............................................................38|
|Appendix 4 - Format required for Import.......................................................................... 39|
|Sample Metadata....................................................................................................... 39|
|Metadata Examples..............................................................................................41|
|Sample Metadata for Synthesis................................................................................. 42|
|Count..........................................................................................................................43|
|Length........................................................................................................................ 44|
|Habitat........................................................................................................................ 45|
|Benthos Count File.....................................................................................................45|
|Benthos Length File................................................................................................... 46|
|Benthos Relief File..................................................................................................... 47|
|Appendix 5 - Common errors........................................................................................... 48|
|Examples of Errors.....................................................................................................48|
|**Appendix 6 - Data privacy and sharing agreements...................................................50**|
|Data Privacy............................................................................................................... 50|
|Data sharing agreements........................................................................................... 50|
|Appendix 7 - Privacy Policy..............................................................................................51|
|Personal identification information............................................................................. 51|
|Google and cookies....................................................................................................51|
|How we use collected information..............................................................................51|
|How we protect your information................................................................................52|
|Sharing your personal information............................................................................. 52|



|Third party websites................................................................................................... 52|
|---|
|Changes to this privacy policy....................................................................................52|
|Your acceptance of these terms.................................................................................52|
|Contacting us............................................................................................................. 52|



# About GlobalArchive 

Mono and stereo-video imagery annotation provides useful information for the discovery, description and management of the marine environment (Harvey et al. 2021), not only for fish and shark assemblages, but also for characterising benthic biota (Langlois et al. 2021; Williams et al. 2020). Such imagery can be sourced from a variety of platforms (e.g. stereo-BRUV, Baited Remote Underwater stereo-Video; stereo-DOV, Diver Operated stereo-Video). The standardisation, archiving and sharing of this annotation data through <u>synthesis</u> can contribute to understanding large spatial and temporal scale patterns in marine biodiversity to inform management. 

GlobalArchive is a collaborative archive for stereo-video annotations of fish and benthic assemblages, designed to support data standardisation, discovery, sharing and synthesis of this data. The platform brings together sampling information and image annotation outputs (as <u>Projects > Campaigns > Annotation sets) and summaries of count and length data for</u> fish and benthic assemblages (as <u>Syntheses) ready for reporting. It is designed around core</u> principles of secure user access, standardised data import, and the capturing of both field sampling and image analysis information. 

The platform is designed to ensure <u>FAIR data principles</u> are complied with, but to also enable granular data access so that users can choose the level of open-access applied to the sampling, annotation or Synthesis data. CARE data principles are achieved by enabling users to control the access to their data. 

For stereo-video image annotation, data can be directly ingested from common software (e.g. SeaGIS EventMeasure) or imported in generic format, after Quality Control checks (see <u>CheckEM). Schema controlled Annotation data is associated with Campaigns of samples</u> that are organised within Projects. 

Curated summaries of count and length data for fish and benthic assemblages made from annotation data on GlobalArchive or other platforms, can be published with a DOI and versioned to provide an unchangeable source for environmental reporting. 

Data can be accessed via a secure API and R package, enabling efficient and structured querying of all sampling, annotation, within a Campaign, and Synthesis data. 



<!-- Start of picture text -->
| PROJECT CT<br>TheA project highest can level.contain one © e.g.PROJEGeographe Marine Park<br>or more campaigns.<br>CAMPAIGN<br>ceeie) Aprojectspecific campaign andsurvey represents belongseffort. toEach a a @) CAMPAIGN 1 @) CAMPAIGN 2<br>campaign has one method 2024-04_Geographe_stereo-BRUVs 2024-04_Geographe_stereo-BOSS<br>(e.g. stereo-BRUVs).<br>Method Metadata is METHOD: stereo-BRUVs|| :== Method Metadata v METHOD: stereo-BOSS || != Method Metadata v<br>entered by users at the<br>time of campaign creation.<br>ANNOTATION SET<br>DS Within each campaign ANNOTATION SET @B)) { ANNOTATION SET @ ANNOTATION SET @<br>wa there canbe multiple<br>annotation sets. Each Following MaxN and Length MaxN and Length Following MaxN and Length<br>annotation set represents a standards in Langlois et al. Measurements Sharks and standards in Langlois et al.<br>discrete upload or version. 2020 Rays only 2020<br>Annotation Metadata is<br>annotation data is<br>uploaded.<br>[) CONTAINS<br>containsEach annotation two key set [) Metadatasample EM EMObs i MetadataSali)ets EM EMObs [) MetadataSan] EM EMObs<br>components:<br>¢ Sample Metadata<br>¢ EMObs<br><!-- End of picture text -->



<!-- Start of picture text -->
SYNTHESIS oP oP oP<br>A synthesis combines<br>obs CAMPAIGN 1 GR CAMPAIGN 2 VR CAMPAIGN 3 NO<br>abundance and body size e.g. 2020-06_south-west_stereo-BRUVs e.g. 2020-10_south-west_stereo-BRUVs e.g. 2025-04_south-west_stereo-BRUVs<br>information from multiple<br>campaignsven to uncover D | >s>> || we‘ |) DB | oem>  |) ||) Dem> |) ;<br>Sample Fish Benthic Sample Fish Benthic Sample Fish Benthic<br>Metadata Annotations Annotations Metadata Annotations Annotations Metadata Annotations Annotations<br>ONE METHOD<br>wis Amore synthesis containscampaigns that oneuse or<br>the same method, e.g.<br>stereo-BRUVs.<br>Annotation data are curated and<br>summarised using CheckEM, R<br>workflows or other tools to generate<br>datasets for analysis and reporting.<br>Syntheses are created in<br>CheckEM or via other tools<br>to ensure custodians have<br>oversight of data. - a<br>(ah Syntheses can be Metadata data data count data relief data<br>versioned and published<br>with a DOI to provide an<br>unchangeable source for<br>< SHAREABLEporns +3) SYNTHESISe.g. South-west Corner Marine Park<br>Syntheses are shareable<br>with other users and can METHOD: stereo-BRUVs VERSION: 1 DOI: Available<br>be accessed via API.<br><!-- End of picture text -->

# Using this guide 

This manual details how to: 

- Create an account and navigate on GlobalArchive, 

- Upload an <u>Annotation Set, within a Campaign, within a Project,</u> 

- Create and upload a Synthesis. 

The manual is best used in an electronic form - as it uses links to linked supporting Appendices and external resources. 

# Useful links 

- <u>GlobalArchive.org 2.0</u> 

- <u>Example - BRUV Metadata</u> 

- <u>Example - BOSS Metadata</u> 

- <u>Example - Project metadata</u> 

- <u>Example - Synthesis metadata</u> 



<!-- Start of picture text -->
GlobalArchiveP 2:0 mc®=~Seca ak fy ~ “% 7 ie2 BS4 > ~ Ds<br>eTrE, APUNUINNIVAWANSWSSN,“Shsaw +SOnr ss ><br>ae PRR AF) AN AW SANS y oy ty —~ ee =<br>= : 16g 4 \.\ ath \ a. . 7 NS ) ~~” e > a _ =”<br>; fe / \\ \ vy j . ~~ ‘— > ><br>Sere thioib bY K; IDX XN\ SyNG yy \ \»>  AS Sa > = =~ >~ wy><br>Welcome to GlobalArchive! Browse Archive<br><!-- End of picture text -->

Click LOGIN 



<!-- Start of picture text -->
Sign in to ORCID<br>Don't have your ORCID iD yet?<br>an account yet<br>Email or ORCIDiD<br>Email or 16-digit ORCID iD<br>have an account<br>Password<br>Your ORCID password<br>Forgot. your password; OF ORCID 1D? ¢q———————————n| Clickhave forgottenhere if you<br>or  ORCIDID<br>& in through your institution<br><!-- End of picture text -->



<!-- Start of picture text -->
GlobalArchive 2082 ~ VQ z Le 4 = —. To switch from<br>URIS SCaia ao oe SS<br>2 os 4 LN et ¥% SANS s\ ees a NOW ——<br>z 2 ae \ NN \\ \r NAY »\ va es > . ~—<br>eoRAN+a ea ne 4),\  NNWWRAY.  WieoSaN_ LS (WSSSeesINN RS. SS“> Se<br>Soy bet AL SY AA AWAI RIN ee ee | 10 upload data<br>Welcome to GlobalArchive! Browse Archive<br>logout atistics<br>Ean ne sco susuiisatnin nemo tungsten oe View GA<br>_ _ _ — statistics<br><!-- End of picture text -->





<!-- Start of picture text -->
GlobalArchivesoar as » Ys2. Onis—S SNE6 wsA Ze°*- eS23> —_<br>Se SE NIWA SSS Sas ae Se<br>Et od ALN Yin Baal Sale ¥<p S=<br>ee RANA Se<br>® TAINS AN WTS SYA Rae<br>ow \\ 85 . > ><br>Welcome to GlobalArchive! Browse Archive<br>GlobalArchive.org P<br>Main menu = __sImport Annotation Set<br>39 Projects<br>Create or select a Annotation Set<br>> Campaigns<br>A Syntheses —~<br>o Map \ /<br>2) Import A<br>r Import Annotation ...<br>A Import Synthesis<br><!-- End of picture text -->



<!-- Start of picture text -->
{...} Create Annotation Set<br>Annotation Set Name<br>Annotation Metadata Fields<br><!-- End of picture text -->



<!-- Start of picture text -->
% Create Campaign<br>Project> a > (+)» 4<br>Method -<br>Description<br><!-- End of picture text -->



<!-- Start of picture text -->
9$2 Create Project<br>AbrolhosProject Name Marine Park ~ Parks Australia - @) ¢ 1. Fillwithin.  cut information.this box .<br>Local .<br>ManagementType *<br>Data Sharing<br>Fully public<br>Shared with<br>Brooke Gibbons  Tim\Langings = AnmikaLewnig © Abbey Gibbons = Henry Jacques Fyans .<br>Description<br>stereo-BRUV in the Abrolhos Marine Park<br>Funding Sources<br>Yamatji Southern Regional Corporation<br>Ackneniedgemencs<br>Thank you to Chris Beissel and Tom from Kamikaze, and Tom MacTavish from the New Zealand<br>Department of Conservation for their assistance in the field. |<br>fete 2. Glick CREATE<br><!-- End of picture text -->



<!-- Start of picture text -->
> Create Campaign<br>Project as,<br>Abrothos Marine Park-Parks Australia - ( + }<br>Campaign Name<br>Abrolhos ~ Qe<br>Methacl<br>stereo-BRUVs *<br>Description<br>stereo-GRUVS in the Abrolhos Marine Park for the Parks Australia<br>Survey Design Description<br>Spatially Balanced<br>Samoling Design Source Code Link<br>https://github.com/UWA-Marine-Ecology-Group-projects/abralhos/tree/main/R/sampling-design<br>1. Fill out information<br>within this box<br><!-- End of picture text -->



<!-- Start of picture text -->
3% Create Campaign<br>|<br>Pian dapioymeant dination | mia<br>a0 Ue ibe fe<br>41. Fill out information<br>Aah Type within this box<br>Pilchards *<br>1000 oa ie [ae<br>Bad deployment<br>Plastic costed wire mesh bag *<br>Mushed *<br>400Minimum separalion dalance of saenples (rm ,* te *<br>GoPral2 *<br>amen POW sering<br>mediuinn *<br>‘i x ~ a<br>300 a)"<br>Baur1.20ann length (mi) .xo oa<br>Armificial Gigiring<br>White *<br>2. Click CREATE<br><!-- End of picture text -->



<!-- Start of picture text -->
oe Create Campaign<br>Ball type<br>Pilchards .<br>Haat quantity (9) in<br>1000 x Press his crass<br>> Create Campaign<br>Bait Type<br>Pilchards .<br>It will then show up<br>like this<br><!-- End of picture text -->



<!-- Start of picture text -->
re 1. Glick on drop down or<br>start typing<br>Externally Allocated DOI<br>2. Select campaign ta<br>Method Metadata Fields copy from<br>Copy metadata from existing Campaign F<br>a 3. Click APPLY<br>2021-05_Abrolhos_stereo-BRUVs<br>Project: Abrolhos Marine Park-Parks Australia, Campaignic: 2021-05_Abrolhos_stereo-BRUVs<br>2023-02_Yamatji-Shallow-Bank_stereo-BRUVs |<br>Project: Abrolhos Marine Park-Parks Australia, Campaignid: 2025-02_Yamatji-Shallow-Bank_stereo-BRUVs<br>2025-06_Clio-Bank_stereo-BRUVs<br>Project: Abrolhos Marine Park-Parks Australia, Campaignic: 2025-406_Clio-Bank_stereo BRUVS<br>2022-11_Abrolhos?_stereo-BRUVs<br>Project: Abrolhos Marine Park-Parks Australia 2, Campaignid: 2022-11_Abrolhos?_stereo-BRUVs<br>2022-11_Salisbury_stereo-BRUVs .<br>Project: Eastern Recherche Marine Park, Campaignid: 2022-11_Salisbury_stereo-BRUVs<br>?025-06_Wudjari_stereo-BRUVs<br>Project: Eastern Recherche Marine Park, Campaignid: 2025-06_Wudjari_stereo-BRUVS<br>Pane ™<br>Bait consistency ¥<br>Minimum separation distance of samples (1m) x “<br>REATE 4. CREATE<br><!-- End of picture text -->



<!-- Start of picture text -->
{...} Create Annotation Set<br>2021-05_Abrolhos_stereo-BRUVSs . f ; 1. Select campaign<br>aT align A tation Set Name<br>UWA Standard Example .<br>UWA standard<br>Annotation. Metadata Fields. 2. Fillwithinout thisinformationbox<br>>opy metadata from existing Campaign Annotation Set i & APPLY<br>Yes .<br>Mobile fauna annotation software (e.g. for fish<br>EventMeasure-stereo .<br>Benth labitat and relief a tatior ftware<br>TransectMeasure .<br><!-- End of picture text -->



<!-- Start of picture text -->
{...} Create Annotation Set<br>Poa score of benthas i<br>ee This is an example,<br>Point score of relief . all campaigns won't<br>be identical<br>Ves *<br>No .<br>All individuals ID'd to species bevel where possible .<br>All individuals ID'd to epeces evel where possible *<br>Not commatently recorded *<br>Not consistently recorded . 2. Fill out information<br>within this box<br>Not recorded *<br>Not recorded .<br>All individuals ID'd to familly level where possible "<br>All species where possible .<br>Fork length .<br>All species where possiibbe "<br>Fork length *<br>Not measured *<br>Not measured *<br>3. Create<br><!-- End of picture text -->



<!-- Start of picture text -->
GlobalArchivei 2:0...RS 23S es i aeeB aT:<br>aI ee ee<br>oy ETI WS SS SSS SE<br>eS ay ANe WAS Jy) StS ae —=<br>a. 46.4 Wt NA en \ my KAI Sa Re * > —_—<br>¥ arr Po Y S S Wry RY \ > .-* ><br>OOF a1 77 JN SAS ON Yi § ONS ~~ ><br>ane \W \ NY Wit UNAS ><br>py Logi SAL SENN WtVO OUT SN eo a SS<br>Welcome to GlobalArchive! Browse Archive<br>This will open<br>this<br>Cre§te or select a Annotation Set<br>{..} Create Annotation Set<br>2021-052022-11_Abrolhos_Lobster-pots2025-02_YamatjAbrothos_stereo-BRUVsShutlon bore nen : |1 campaign4. Select<br>2025-06 Clio oor wereo BRUVs - .<br>2022-11_Salisbury_stereo-BRUVs<br>2025-06_Wudjari_stereo-BRUVs<br><!-- End of picture text -->



<!-- Start of picture text -->
Selected Annotation Set: -<br>Files in Annotation Set: “<br>same ‘Size Owe Samus Fie Type Actors,<br>1. Click here to<br>rm select files to<br>import or drag files<br>into this section<br>Vocabulary Selected: -<br>Issues<br>Overview This will open up<br>Type Descripece Afectea tows Cause your documents<br>3D POINT DATA LENGTH DATA<br>)s<br>-<br>t This PC SCI-MEGFISH-001 (\\drive.irds.uwa.edu.au) (Z: Project Folders » 2021-05_Abrolhos Stereo _BRUVs BOSS » Working Video Analysis >» BRUVs<br>a ' .<br>Quick access<br>WW Desktop CAAB~ Codes F 4<br>- Calibration 2022 2 | File folde<br>$ Downloads Cant 0 , ; \<br>Documents Cool Clips ‘ File folde<br>& Pictures EM Export f 1<br>Video Analysis EM_Output_Jess 2 ‘ F j 2. Select the files<br>EventMeasure Event Measure 4 File folde to import<br>Coolo - Clips Event Measure - Jessi f 4<br>Event Measure MaxN by size class File fold<br>EventMeasure FieldFishnClipzrecording- sheet scans 2021 9:22 4 FileF folde:{<br>GA upload example Habitat images F j<br>to upload Habitat Schemas f {<br>@ OneDrive- UWA QAQC testing 2021 9:22 F fe<br>Representative clips A F {<br>Attachments TM Export F {<br>[BB Desktop Transect Measure ' .<br>Documents g® 2021-05_Abrothos_stereo-BRUVs_metadata 2026 2 | Mocr<br>Meetings cit) Project Specific Video Analysis Rules 4 A I<br>Microsoft Copilot Chat Files<br><!-- End of picture text -->



<!-- Start of picture text -->
oO Group Manne Sine Date Status File Type Actions<br>1. Press the Ch Sample Metadata Fite (1)<br>dropdown<br>. 2021-05_Abrolhos_sterec- 16.19 11/06/2026, e al<br>O BRUVs_metadata,cav. KB 14:02:06 ra 5. | in.|<br>oO vy  EMObs File (49)<br>O np26.10_EMObs.. KB16.38 11/06/2026,13:53:11 ¥a E.. ¥ ia<br>_<br>= 14.06 11/06/2026, ot<br>L np26.13.EMOba“ . KB 12:53:11 , “ E. aa<br>2. Check the<br>status<br><!-- End of picture text -->



<!-- Start of picture text -->
Selected Annotation Set: oa<br>Files in Annotation Set: a<br>i Group Name Size Date ‘Status File Type 4 Actions<br>(> Sample MetadataFile (1) ¥v<br>CO > EMObs Fite (60) Y<br>r Add files to Annotation Set e<br>Select Vocabulary Used a<br>Australian aAquatic Fauna (CAAB+WORMS+FishBase) = 3. Click drop down<br>Australian Aquatic Fauna (CAAB+WORMS+FishBase)<br>Issues 4. Select the<br>oh vocabulary used<br>Type Description Affected Rows Cause<br>Loading import preview... This might take @ lite while - please be patient<br><!-- End of picture text -->

##### Check for Issues 

- Scroll to the _Issues_ section. 

- The _Issues_ section lists any problems detected in the uploaded data. Each row shows: 

   - the type of issue 

   - a description of the issue 

   - the percentage of rows affected 

- The _Type_ column indicates the severity of the issue: 

   - ℹ Info: General information about the data. These messages do not prevent the file from being imported but may highlight something useful to review. 

   - ⚠ Warning: A potential problem that should be checked before importing. 

   - The file can usually still be imported, but some rows or values may need attention. 

   - ❗ Error: A problem that must be fixed before the file can be imported. Errors usually indicate missing required fields, invalid values, or formatting issues that prevent the import from continuing. 

- A detailed explanation of individual errors/warnings, common causes and trouble shooting tips can be found in Table X. Coming soon… 

- Use the <mark>👁</mark> icon to filter the data view so that only the rows causing the selected issue are displayed. This is useful when you want to inspect the affected records directly, check what needs to be corrected, or focus on one issue at a time without viewing all the problematic rows at once. 

- In the _Cause_ column, hover your cursor over the ❓ symbol to view more details about the issue, including the affected file, columns or rows, and the percentage of rows affected. 



<!-- Start of picture text -->
Description of the<br>Issue<br>Issues<br>Overview<br>— Affected<br>. Type Description Rows Cause<br>ri] Dropping unsupported columns from Sample Metadata file. @<br>- Thecolumn3D Point Annotation data contain rows with blank or missing values in the period e_<br>- Column ‘number in 3D Point Annotation data is expected to be integer valued, but contains =<br>values which cannot be converted to integers. These rows will be filled with 0. oY<br>The Length Annotation data contain values in the period column. Since this column was not<br>= used in the given Sample Metadata, these values will be ignored when matching to ey<br>0 Samples. If you intended to use this column to identify Samples, please ensure that the Y<br>conmesponding column in the Sample Metadata contains values.<br>The Point Annotation data contain values in the period column. Since this column was not<br>used in the given Sample Metadata, these values will be ignored when matching to _<br>: 0 Samples. If you intended to use this column to identify Samples, please ensure that the oe<br>corresponding column inthe Sample Metadata contains values.<br>The 3D Point Annotation data contain values in the period column, Since this column was<br>: not used in the given Sample Metadata, these values will be ignored when matching to =<br>0 Samples. If you intended to use this column to identify Samples, please ensure that the ©<br>a comesponding column in the Sample Metadata contains values.<br>AN<br>Use this to focus on Type of issue To see what<br>one issue at a time in columns/rows are<br>the data view frame affected<br><!-- End of picture text -->



<!-- Start of picture text -->
Data View<br>3D POINT DATA LENGTH DATA<br>Sepage EEE—EE<br>File od Row # filename frame time_mins opcode period period_time_mins imx imy rectx<br>‘SB-8V-002.EMObs 1 62_GX010040. MP4 ans 2.622063888888889 ‘SB-8V-002 0.2713822222222224 404_70588235294116 445.8823529411/646 1<br>SB BV.002.EMObs 2 L62_GX010040.MP4 6408 3.5635600000000003 SB BV 002 1.2128783333333337 917.6470588235294 670.5882352941177 1<br>S0-BV-002.EMObs 3 L62.6X010040.MP4 8303 4.617390555555556  SO-BV-002 2.266708888880889 —64.70508235294117 _661.1764705882352. 1<br>SB-BV-002.EMObs 4 L67_Gxo10040.MP4 8684 4.8292688888888895 —_SB-BV-002 2.478587222227273 «292.9411 7647088823 931.7647058873529 -1<br>SB BV.002.EMObs 5 L62_GX010040.MP4 12535 6.970852777777778 SB BV 002 4.6201719191119115 580 622.3529411764706 1<br>SB-BV-002.EMObs 6 L62.6x010040.MP4 13120 7296\77777777777-—=«SB-BV-002 A945A96111111111 _-1196.4705882952041 522.9529411764706 1<br>s8-8V-002.EMObs 7 \62.Gxo10040.MP4 13120 1L206\7TTTTITTTT §~—S8-8V-002 4945496111111 976.47058823570414 —514.1176470588735 1<br>SB BV.002.EMObs 8 L62.Gx010040.MP4 1376 7.660986666666667 SB BV.002 5.3103050000000005  1624.7088823529412 750.5882352941177 1<br>S0-BV-002.EMObs 9 L62.GX010040.MP4 1376 7.660986666666667 _SD-BV-002 5.3109050000000005 1201.1764705882354 656.4705882352941 1<br>SB-8V-002.EMObs 10 162.Gx010040.MP4 13776 7.660986666666667 SB-8V-002 5.3103050000000005 791.7647058823529 498.8295204117647 1<br>SB BV.002.EMObs " L62.GX010040.MP4 15663 8.710368333333333 _SB.BV.002 6.359686666656667 -1768.235204117647 483.52941176470586 1<br>SB-BV-002.EMobs 12 L62.GX010040.MP4 15663 8.710968333333333 _SB-BV-002 6.359686666666667 952.9411764705883 544,7058823520412. 1<br>‘SB-8V-002.EMObs 3 162_GX010040.MP4 15663 8.710368333333333 ‘SB-BV-002 6.359686666666667 725.8823529411765 467.05882357941177 1<br>SB BV.002.EMObs 14 L62_GX010040.MP4 15663 8.710368333333333 _SB.BV.002 6.359686666666667 240 628.2382041176471 1<br>S8-8V-002,EMObs 15 162.6x010040.MP4 18790 VOAA9S27777777777 — SB-BV-002 8.09864611111111 105.88295794117646 877.6470588735794 1<br>ee ,<br>To view more/less To navigate To import<br>on each page through pages P<br><!-- End of picture text -->



<!-- Start of picture text -->
Issues<br>Overview<br>Type mcripion AMT ecteed couse<br>o Croppingunaupporeed columns fram Ramepie Kirtanfile a<br>viewed<br>.?<br>o@<br>oo<br>o TY]<br>Data View<br>Poin cata Leworn para<br>Fite ad fom ee Slename lett filename right faree_leh ‘Treen ye_ringtet Tite mine eapeenctie Pere i Meat irea_tett<br>rev? HuaMoes 1 Loma (17d eo 1} aan sms OIOMOETTTTTTTTS peu. / 4 Io Taaranaare|<br>rere EMObs 1 LO3S (02). MP4 ADDS (924s wa 12486 DOATZ BS) GGG? npr. 9 /~ 4 1627. 93000 7900 508<br>roe. a LOR? (OoLMMa = NER (ONL uae ta08 DONO OMARNARLAT — ng.9 re 1 13E mUOTEONIIETT<br>nerd LEMObS 1 LOS! (02). ADS (Oz) Mae ian a TEOMOMIEIF =— np. 12 /~ 1384. 57IGOBESERD<br>nord 1REMDOR 1 LORE (0T7).MeA ANA (TZ)MAPA = TINS ano OSESDONMAEAEIE mgd re 1 170048361 8802318<br>nga? MCs 1 Loan (Deja RCD oe) na 2230 20rd pe er td é~ 4 TOOL 1 eeyzE vena]<br>ner.) MOGs: 1 LO? (07). MPa Aa) (02). MPS 2900 anar 30.8380 17227227003 npr. 1 /~ 4 VSES. 755258126199<br>Opcodes with ; The issue is there is<br>the issue no period data<br><!-- End of picture text -->



<!-- Start of picture text -->
GlobalArchive.orgMain menu _— ImportSynthesis<br>32 ~—s«~ Projects<br>=e Campaigns Create or select a synthesis<br>A, Syntheses —<br>\ + | Select a synthesis<br>1 oO Map Nt ,<br>5) Import “A<br>~ Import Campaign ...<br>2 a Import Synthesis<br><!-- End of picture text -->



<!-- Start of picture text -->
7<br>Synthesis Mame<br>Abrolhos Marine Park-YSRC 2 = e<br>Method<br>stereo-BRUVs .<br>Local .<br>Management .<br>https://github.com/UWA-Marine-Ecology-Group-syntheses/ Abrolhos-Marine-Park-v2_git https://gith<br>* 1. Fill out<br>Analysis Source Code Link information<br>within this box<br>Externally Allocated DO)<br>Data Sharing<br>Fully public<br>Shared with<br>Brooke Gibbons Tim Langloss *<br>Description<br>stereco-BRUYV in the Abrolhos Marine Park-YSRC funded by Yamatji Southern Regional Corporation<br>¥<br>2. Create<br><!-- End of picture text -->



<!-- Start of picture text -->
GlobalArchive.orgMain menu = __Import Synthesis, © ~~ \ 3 R >Pe we| .- ~<br>22 Projects<br>>= Campaigns Create or select a Synthesis<br>A sSyntheses -<br>4 Select a synthesis Search for<br>oO Map Synthesis<br>2) Import A Abrolhos Marine Park-YSRC<br>Version 1, Method: stereo-BRUVs, Created: 2026-05-26, Latest sample: 2025-06-27 Select<br>* Import Annotation ... Abrolhos Marine Park-YSRC Synthesis<br>A Import Synthesis. Version 1, Method: stereo-BOSS, Created: 2026-07-06 atest sample: 2021-05-14<br>Eastern Recherche Marine; Park<br>Version 1,Method: stereo-BRUVs, Created: 2026-05-26, Latest sample: 2025-07-02<br>Eastern Recherche Marine Park<br>Geographe Marine Park<br>Version2,Method: stereo-BOSS, Created: 2026-07-07, Latest sample: 2024-04-18<br>Geographe Marine Park<br>Version 3, Created: 2026-06-02, Latest sample: 2024-04-17<br>Selected Synthesis: ~<br>Files in Synthesis: . 1. Click here to<br>select files to<br>O nem Sze Date ‘Saatus File Type 4 ‘Actoms import or drag files<br>No fen attedt0 in Rymneain yor into this section<br>A e This will open up<br>your documents<br>€ T Abbeys Github (E:) >» Abrothos-Marine-Park-v2 data > uploads<br>=| Documents a Jame Date modified Type ae<br>=) Pictures p2/ 2021-05_Abrothos_ stereo-BRUVs_bentho.. 10:50 AM ficrosoft Excel f<br>Video Analysis ig) 2021-05_Abrolhos_stereo-BRUYs_bentho.. 6/2026 10:57 AM Microsoft Excel B<br>EventMeasure ip 2025-02_Yamatji-Shallow-Bank stereo-B . . 8/06/2026 11:16 AM Microsoft Excel C 19 KB<br>Cool Clips ig! 2025-02_Yamatji-Shallow-Bank_stereo-B... 16 AM ficrosoft Excel f 2 Select the files<br>2022-12_Daw_stereo-BOSSEventMeasurea g#/jg) 2025-06_Clio-Bank_stereo-BRUVs_bentho..2025-06_Clio-Bank_stereo-BRUVs_bentho... 8/06/20266/2026 11:14 1 :14 AMAM MicrosoftMicrosoft ExcelExcel C 7KBf to import<br>g4! Abrolhos-Marine-Park-v2_count PM ticrosoft Excel i<br>GA upload’upload exempreI */ Abrolhos-Manine- Park-v2_length 5/2026 2:53 PM Microsoft Excel 66 KE<br>to uploed 2) Abrolhos-Marine-Park-v2_metadata 27/05/2026 2:53 PM Microsoft Excel C 12 KB<br><!-- End of picture text -->



<!-- Start of picture text -->
Selected Synthesis:<br>Files in Synthesis:<br>OO mame Size Date Status File Type Actions<br>o Abrolhos-Marine-Park v2_mevadata.cav 41.29 KB 2705/2026, 14:54:00 wf Sample Metadata File * | ns |<br>QO Abrothos-Marine-Park-v2_count.cey 197.12 KB Z7N5/2026, 14:54:01 wf Count File . in<br>Oo Abrolhog-Marine-Park-v2_lengtn.cav 1.04 MB 2705/2006, 14:54:01 we Length File . ia<br>San file Type<br>( -2021-05_Abrotnos_stereo-BRUVs_bemhos-courtcsv 26.19 KB 08/06/2026, 11:01:27 a Benthos Count File ~ ia<br>set fle Tyne<br>(Os 2025-02_vamalj-Shallow-Bank_stereoBRUVs_benthas-counLcey 40.45 KB 27/05/2026, 0-48 a Benthos Count Fille " ia<br>set fhe tyne<br>oO 025-+06_Clioank_sterecHALiVs_benihos-counLosv 71.69 KB PFS 2076, 00-2017 ae Benthos Count File . i a<br>1, Check the<br>status<br><!-- End of picture text -->



<!-- Start of picture text -->
O tex“snes v moyen 11/06/2026,14:09:54<br>1. Click in this box<br>Sample Metadata File<br>Count File This will open this<br>drop down menu<br>Length File<br>Benthos Count File<br>2. Select the<br>Benthos Length F File appropriate. file| type<br>Benthos Relief File<br>Benthos Count File .<br><!-- End of picture text -->



<!-- Start of picture text -->
gy REL Nemep atow tank ren One bentoe asia 77/8/2028 v Benthos Relief File . ia<br>1. Select the<br>check boxes<br>2025-06. Cho Bank stereoBAUVE_benthos-elief csv 16.70KB a on v Benthos Relief File , ia<br>2. Click delete<br>DELETE SELECTED FILES + selected files<br>id t e<br><!-- End of picture text -->



<!-- Start of picture text -->
Select Vocabularies Used a<br>sc Fh cab sc ens Vecsey 1. Click the drop<br>Australian Aquatic Fauna (CAAB+WORMS+FishBase) Australian Benthic Biota and Substrate (CATAMI+) * down<br>Australian Benthic Biota and Substrate (CATAMI+)<br>2. Select the<br>vocabulary used<br><!-- End of picture text -->





<!-- Start of picture text -->
= __s Import Synthesis Logged | « 9} Oct ars Daz fe)<br>O —_2025-06_Clio-Bank_stereo-BRUVs_benthos-countcsv 21.69 KB 30/06/2026, 22:10:41 v Benthos Count File . | |<br>a) 2021-05_Abrothos_stereo-BRUVS_benthos-relief.csv 9.74 KB 30/06/2026, 22:10:41 ¥ Benthos Relief File . it a<br>oO 2025-02_Yamaitji-Shallow-Bank_stereo-BRUVs_benthos-telief.csv 34.31 KB 30/06/2026, 22:10:41 vv Benthos Relief File ~ it a<br>0 = 2025-06_Clio-Bank_stereo-BRUVs_benthos-telief.csv 16.79 KB 30/06/2026, 22:10:41 ¥ Benthos Relief File . 5 a<br>fy Add files to Synthesis e<br>Vocabularies Selected: v<br>Issues<br>Type Description Affected Rows Cause<br>Perfect! No errors or warnings raised for this import.<br>1. Check for 2. Import<br>Issues Data<br><!-- End of picture text -->

# Frequently Asked Questions and Tips 

### Frequently asked questions 

Can <u>Projects</u> include multiple methods? 

- Yes, <u>Projects</u> can include multiple methods. 

Can <u>Syntheses</u> include multiple methods? 

   - No, <u>Synthesis</u> must only contain one method. Separate <u>Syntheses</u> must be made for different methods. 

- If I am uploading for someone else what information do I need to get from the custodian? - <u>Here</u> is a spreadsheet to get the custodian to fill out 

### Tips 

- Light and Dark Mode: In the top right corner of the landing page you can click the ☼ to switch between light and dark mode 

# Appendicies 

### Appendix 1 - Definitions 

##### Project and Campaign terms 

###### Project 

- Contains one to multiple Campaigns with a shared purpose/objective (e.g. monitoring of a Marine Park, a bioregional study). 

- <u>Project is a unique identifier and the name should be carefully chosen (e.g.</u> “MarineParkMonitoring” is not a good Project name but “Houtman Abrolhos Reef Observation Areas long-term monitoring” is a great Project name) 

- Sampling and image analysis methods are generally standardised within a <u>Project</u> but sampling method for each Campaign can be different (e.g. one <u>Campaign could</u> use DOV whereas another Campaign could use BRUV or UVC). 

###### Scale 

- The geographic extent at which the dataset, Project, or <u>sample</u> is relevant or intended to be applied (e.g. International, National, Regional, or Local). 

###### Type 

- The category that best describes the primary context or intended use of the dataset or project. 

- Cultural, Academic, Industry or Management 

###### Campaign 

- A discrete set (temporal and spatial) of <u>samples.</u> 

- All <u>samples within a Campaign</u> use the same sampling and image analysis methods. 

###### CampaignID 

- A unique identifier for a <u>Campaign made up of</u> YYYY-MM_Campaign_Method . 

- The date is the start date of the <u>Campaign (e.g.</u> 2023-12_Rottnest-Island-Marine-Parks_stereo-BRUVs ). 

###### Annotation Set 

- A collection of annotations from a defined set of samples within a Campaign , grouped together as a single dataset. 

###### Annotation Metadata 

- The descriptive information associated with an Annotation Set that provides context for the annotations and information on how the samples were annotated. 

- For example levels of identification and software used to annotate. 

###### Method 

- Any sampling <u>method defined by a user (e.g. stereo-BRUVs, stereo-DOVs, UVC).</u> Defined in the “Methods” tab. 

###### Sample 

- Single observational unit (e.g. a BRUV deployment or DOV transect). Must match exactly across sample metadata and any annotation/EMObs files. Defined by opcode only, period only, or opcode + period. 

- 

###### Vocabularies 

- The approved controlled vocabularies used in EMObs to standardise annotation of fish, benthic biota, and substrates. 

- These ensure annotations are applied consistently and can be compared across <u>projects and datasets.</u> 

##### Synthesis terms 

###### Synthesis/Syntheses 

- Combines information from multiple Campaigns with the same <u>method from multiple</u> locations and/or multiple times. 

###### Scale 

- The geographic extent at which the <u>Synthesis</u> is relevant or intended to be applied 

- International, National, Regional or Local 

###### Type 

- The category that best describes the primary context or intended use of the <u>Synthesis.</u> 

- Cultural, Academic, Industry or Management 

##### User Access and Sharing terms 

###### Custodian 

- A <u>user</u> who creates a <u>Project, and the Campaigns</u> within it, is the Custodian. 

- Only the Custodian can upload <u>Campaigns</u> to that Project and share the <u>Campaigns</u> or the entire Project with other Users. 

- The Custodian should be the person within the institution with a long term interest in the data and the person who should be contacted if another user wants to use the data (e.g. will more likely be a Principal Investigator than a research assistant). 



<!-- Start of picture text -->
[ + }-—FrrgramEa) EventMeasure:Picture TR-BV-CP8.EMObsMeasurement Stereo: 20260505_BRUVAbout 3_L19_¢<br>Current settings ... " [<br>View reference images ...<br>File concatenation utility ... 3 mins)<br>Batch text file output ...<br>Batch binary file concatenation ...<br>[2 +s Generate  speciesdatabaseoutput file ...  ...<br>Text to/from EMObs ><br>Batch generate skeleton EMObs ...<br>Batch change picture directories ...<br>Close<br><!-- End of picture text -->



<!-- Start of picture text -->
3. Click & select<br>where EMObs<br>are saved<br>Database table generation<br>File<br>Marne Data Extra into<br>Input file directory = WS \Project Folders*2021-05_Abnolhos_Stereo_BRUYs_BOSS‘vorking\VideoAnais BAL s\E vent Measune Directoy: Selection Dinecton! where EM Obs files are located<br>Qutput fle drecton v2 \Project Folder'2021-05_Abnolhos_Stereo_BRUWs_BOSS‘Working\Video Anakeis\BRUYssEM Export Directoy Selection Dinecton! where database files are generated<br>Base name wf 2021-06_Abrothos_steeo-BR Us<br>4. Click & select<br>where files will be<br>saved<br>5. Enter<br>Campaign name<br>L Cancel | Process:<br>6. Click Process<br><!-- End of picture text -->

###### Database generation summary 

Clear Save text Using: npz6.BOSS41.EMObs A Using: npz6.C01.EMObs Using: npz6.C02.EMObs Using: np29.1.EMObs Using: npz3.10.EMObs Using: npz9.11.EMObs Using: np2z9.12.EMObs Using: npz9.13.EMObs Using: npz9.14.EMObs Using: np29.16.EMObs Using: np29.17.EMObs Using: np29.18.EMObs Using: npz9.15.—EMObs Using: np29.2.EMObs Using: np29.21.EMObs Using: np2z9.22 EMObs Using: npz9.24.EMObs Using: npz9.25.EMObs Using: np29.26.EMObs Using: npz9.27.EMObs Using: np29.28.EMObs Using: npz9.29.EMObs Using: np29.3.EMObs Using: np29.30.EMObs Using: np2z9.4.EMObs Using: npz9.5.EMObs Using: np29.7.EMObs Using: np29.8.EMObs Using: npz9.5.EMObs Files inspected: 49 Files used: 49 ¥ 

rx | 2021-05_Abrolhos_stereo-BRUVs_3DPoints 29/06/2026 9:29 4M TXT File 49 KB rx | 2021-05_Abrolhos_stereo-BRUVs_ImagePtPair 9 29/06/2026 9:29 AM TXT File 115 KB rx | 2021-05_Abrolhos_stereo-BRUVsInfo 29/06/2026 9:29 AM TXT File 2 KB rx | 2021-05_Abrolhos_stereo-BRUVs_Lengths 29/06/2026 9:29 AM TXT File 207 KB rx | 2021-05_Abrolhos_stereo-BRUVs MovieSeq 29/06/2026 9:29 AM TXT File 39 KB a 2021-05_Abrolhos_stereo-BRUVs_Period 29/06/2026 9:29 AM TXT File 4KB rx | 2021-05_Abrolhos_stereo-BRUVs_Points 29/06/2026 9:29 AM TXT File 1,267 KB a 2021-05 _Abrolhos_ stereo-BRUVs Source 29/06/2026 9:29 4M TXT File 4KB 



<mark>The component is an option and you will need a specific licence to access it. To check if you have this, on the EventMeasure window go to the menu About then Version. Under the Installed components, Database output should be listed. Otherwise contact James Seager.</mark> 

<mark>The datatable export function (Program>Generate database output) will generate 9 data tables from a discrete set of .EMObs files that make up a</mark> <u><mark>C</mark> ampaign.</u> 

Common mistakes and troubleshooting tips 

- Duplicated OpCodes in the information fields across .EMObs files will cause an error, and will only export data from the first EMOBs file with that opcode. 

Check the information fields for duplicates and rename the duplicated ones. 

pe! Abrolhos-Marine-Park-v2_count 2! Abrolhos-Marine-Park-v2_length fp! Abrolhos-Marine-Park-v2_metadata 

27/05/2026 2:53 PM 27/05/2026 2:53 PM 27/05/2026 2:53 PM 

Microsoft Excel C.., 198 KB Microsoft Excel C.., 1,066 KB Microsoft Excel C.., 42 KB 

### Appendix 4 - Format required for Import 

##### Sample Metadata 

- A sample metadata file is required for both Annotation and Synthesis imports. 

- Sample metadata must be provided as a comma-separated values (.csv) file. 

- We recommend naming the file using the format 

   - YYYY-MM_Project-name_Method_metadata.csv (e.g. 

   - 2026-06_Abrolhos_stereo-BRUVs_metadata.csv ) to ensure compatibility with CheckEM, although any file names are accepted. 

- GlobalArchive requires specific columns (listed in Table 1), including defined formats and validation rules applied during import (see example in Table 1). 

- Any unsupported columns will be ignored and removed during ingestion. 

**Table 1.** Column and formatting requirements of the sample metadata file, note the tables described below are transposed (rows for columns) for formatting convenience. 

|Name|Format|Cannot be blank|
|---|---|---|
|opcode|String|✔<br>_if opcodes were used to define a_<br>_sample._<br>_DON’T include this column if it is not_<br>_required to define a sample._|
|period|String|if periods were used to define a<br>sample.<br>_DON’T include this column if it is not_<br>_required to define a sample._|
|latitude_dd|Decimal degrees. Must be between<br>-90 to 90.|✔|
|longitude_dd|Decimal degrees. Must be between<br>-180 to 180.|✔|
|date_time|String in<br>YYYY-MM-DDThh:mm:ssTZD<br>YYYY = four-digit year<br>MM = two-digit month (01=January,<br>etc.)<br>DD = two-digit day of month (01<br>through 31)<br>T being a required literal character.<br>hh = two digits of hour (00 through<br>23)<br>mm = two digits of minute (00<br>through 59)<br>ss = two digits of second (00<br>through 59)|✔|



||TZD = time zone designator (Z or<br>+hh:mm or -hh:mm)||
|---|---|---|
|site|String. The scale of sites are up to<br>the user to define.|✖|
|location|String. The scale of locations are up<br>to the user to define.|✖|
|status|MPA status (must be Fished,<br>No-take, I, II, III, IV, V, VI)|✔|
|depth_m|Floating point number (meters)|✔|
|successful_count|String (must be Yes or No)<br>Was the sample annotated for count<br>and will that data be included in any<br>analysis?|✔|
|successful_length|String (must be Yes or No)<br>Was the sample annotated for<br>length and will that data be included<br>in any analysis?|✔|
|observer_count|String (full name), cannot be NA|Only required if successful_count =<br>“Yes”|
|observer_length|String (full name), cannot be NA|Only required if successful_length =<br>“Yes”|
|successful_habitat_forward|String (must be Yes or No)|✖|
|successful_habitat_backward|String (must be Yes or No)|✖|
|observer_habitat_forward|String (full name), cannot be NA|✖|
|observer_habitat_backward|String (full name), cannot be NA|✖|



**Table 2.** First three lines of an example Annotation sample metadata file from a stereo-BRUVs <u>Campaign that used additional backwards facing</u> cameras for habitat annotation. The opcode column is used to define the sample. 

|**opcode**|**latitude_dd**|**longitude_dd**|**date_time**|**site**|**location**|**status**|**depth_m**|**successful**<br>**_count**|**successful**<br>**_length**|**observer**<br>**_count**|**observer_l**<br>**ength**|**successfu**<br>**l_habitat_f**<br>**orward**|**successfu**<br>**l_habitat_**<br>**backward**|**observer_**<br>**habitat_fo**<br>**rward**|**observer_**<br>**habitat_b**<br>**ackward**|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|TR-BV-67|-28.439983|113.742067|2026-06-25<br>T08:36:45+<br>08:00|NA|Abrolhos|Fished|67.4|Yes|Yes|Abbey<br>Gibbons|Annika<br>Leunig|Yes|Yes|Abbey<br>Gibbons|Abbey<br>Gibbons|
|TR-BV-16|-28.439673|113.742028|2026-06-25<br>T08:45:03+<br>08:00|NA|Abrolhos|No-take|36.7|Yes|Yes|Abbey<br>Gibbons|Annika<br>Leunig|Yes|Yes|Abbey<br>Gibbons|Abbey<br>Gibbons|
|TR-BV-28|-28.436767|113.672028|2026-06-25<br>T08:52:49+<br>08:00|NA|Abrolhos|Fished|42.9|Yes|Yes|Abbey<br>Gibbons|UWA<br>MEG lab|Yes|Yes|Abbey<br>Gibbons|Abbey<br>Gibbons|



###### Metadata Examples 

- We have provided example templates for both <u>BRUV</u> and <u>BOSS</u> metadata. 

- In both templates: 

   - The coloured column headings are the headings that GlobalArchive requires. Uncoloured columns are optional. 

   - Row 2 provides descriptions of the expected data for each column. 

   - Row 3 & 4 contain example entries. 

   - These templates can be copied for your own metadata, but rows 2–4 must be removed before entering new data. 

##### Sample Metadata for Synthesis 

The sample metadata requirements for Annotations and Syntheses are very similar. The primary difference is that synthesis metadata must include a <u>campaignid column (Table 3). This is required because a synthesis may combine data from multiple Campaigns, and the campaignid</u> identifies the Campaign associated with each <u>sample.</u> 

● Recommended naming suffix: _metadata.csv 

**Table 3** . First three lines of an example <u>Synthesis sample metadata file from a stereo-BRUVs Campaign that used additional backwards facing</u> cameras for habitat annotation. The opcode column is used to define the sample. 

|**campaign**<br>**id**|**opcode**|**status**|**latitude_dd**|**longitude_dd**|**date_time**|**site**<br>**location**|**depth_**<br>**m**|**successful**<br>**_count**|**successful**<br>**_length**|**observer**<br>**_count**|**observer**<br>**_length**|**successfu**<br>**l_habitat_**<br>**forward**|**successfu**<br>**l_habitat_**<br>**backward**|**observer**<br>**_habitat_**<br>**forward**|**observer_**<br>**habitat_b**<br>**ackward**|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|2021-05_<br>Abrolhos<br>_stereo-<br>BRUVs|TR-BV-67|Fished|-28.439983|113.742067|2026-06-25<br>T08:36:45+<br>08:00|NA<br>Abrolhos|67.4|Yes|Yes|Abbey<br>Gibbons|Annika<br>Leunig|Yes|Yes|Abbey<br>Gibbons|UWA<br>MEG|
|2021-05_<br>Abrolhos<br>_stereo-<br>BRUVs|TR-BV-16|No-take|-28.439673|113.742028|2026-06-25<br>T08:45:03+<br>08:00|NA<br>Abrolhos|36.7|Yes|Yes|Abbey<br>Gibbons|Annika<br>Leunig|Yes|Yes|Abbey<br>Gibbons|UWA<br>MEG|
|2021-05_<br>Abrolhos<br>_stereo-<br>BRUVs|TR-BV-28|Fished|-28.436767|113.672028|2026-06-25<br>T08:52:49+<br>08:00|NA<br>Abrolhos|42.9|Yes|Yes|Abbey<br>Gibbons|Annika<br>Leunig|Yes|Yes|Abbey<br>Gibbons|UWA<br>MEG|



##### Count 

- A Count file is required for <u>Synthesis</u> imports. 

- Count file must be provided as a comma-separated values (.csv) file. 

- Recommended naming suffix: _count.csv 

**Table 4** . First four lines of an example Count file from two stereo-BRUVs <u>Campaigns</u> ready to be imported into a Synthesis. The opcode column is used to define the sample. 

|**campaignid**|**opcode**|**family**|**genus**|**species**|**count**|**stage**|**caab_code**|
|---|---|---|---|---|---|---|---|
|2021-05_Abrolhos_stereo-BRUVs|npz6.10|Carangidae|Seriola|dumerili|5|AD|37337025|
|2021-05_Abrolhos_stereo-BRUVs|npz6.10|Monacanthidae|Nelusetta|ayraud|1|AD|37465006|
|2025-02_Yamatji-Shallow-Bank_stereo-BRUVs|SB-BV-007|Sparidae|Chrysophrys|auratus|38|AD|37353001|
|2025-02_Yamatji-Shallow-Bank_stereo-BRUVs|SB-BV-008|Carangidae|Pseudocaranx|spp|1|AD|37337924|



##### Length 

- A Length file is required for <u>Synthesis imports.</u> 

- Length file must be provided as a comma-separated values (.csv) file. 

- Recommended naming suffix: _length.csv 

**Table 5** .First four lines of an example Length file from two stereo-BRUVs Campaigns ready to be imported into a <u>Synthesis. The opcode</u> column is used to define the sample. 

|**campaignid**|**opcode**|**caab_code**|**count**|**family**|**genus**|**species**|**length_mm**|**precision_mm**|**range_mm**|**rms_mm**|**stage**|
|---|---|---|---|---|---|---|---|---|---|---|---|
|2021-05_Abrolhos_stereo-BRUVs|npz6.21|37372907|1|Pomacentridae|Chromis|spp|NA|NA|1623.842|7.40469|AD|
|2021-05_Abrolhos_stereo-BRUVs|npz6.22|37355015|1|Mullidae|Parupeneus|spilurus|NA|NA|2735.874|25.38756|AD|
|2025-02_Yamatji-Shallow-Bank_stereo-BRUVs|SB-BV-051|37384923|1|Labridae|Iniistius|spp|131.1065|4.0386|2899.085|0.69228|AD|
|2025-02_Yamatji-Shallow-Bank_stereo-BRUVs|SB-BV-022|37351009|1|Lethrinidae|Lethrinus|miniatus|335.4285|3.09009|1632.069|1.07326|AD|



##### Habitat 

The strongly recommended benthos annotation method is to use a random-point approach. Where 20 points are randomly selected in the bottom 50% of the image. Using the bottom 50% of the image reduces the amount of open water annotations. 

The strongly recommended relief annotation method is to use a 5 x 4 grid composing of 20 rectangles, we then annotate the relief of each grid-cell by scaling them from completely featureless and flat, up to highly complex. 

##### Benthos Count File 

- A Benthos Count file is required for Synthesis imports. 

- Benthos Count file must be provided as a comma-separated values (.csv) file. 

- Recommended naming suffix: _benthos-count.csv 

**Table 6** . First four lines of an example Benthos Count file from two stereo-BRUVs Campaigns ready to be imported into a <u>Synthesis. The</u> opcode column is used to define the <u>sample.</u> 

|**campaignid**<br>**opcode**|**level_1**|**level_2**|**level_3**|**level_4**|**level_5**|**level_6**|**level_7**|**level_8**|**family**|**genus**|**species**|**caab_code**|**count**|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|2021-05_Abrolhos_stereo-BRUVs<br>npz6.13|Biota|Bryozoa|NA|NA|NA|NA|NA|NA|NA|NA|NA|20000000|1|
|2021-05_Abrolhos_stereo-BRUVs<br>npz6.13|Biota|Cnidaria|Corals|Black &<br>Octocorals|Branching<br>(3D)|NA|NA|NA|NA|NA|NA|11168902|1|
|2021-05_Abrolhos_stereo-BRUVs<br>npz6.28|Physical|Substrate|Unconsolidated<br>(soft)|Sand / mud<br>(<2mm)|Fine sand<br>(no shell<br>fragments)|NA|NA|NA|NA|NA|NA|82001015|27|
|2021-05_Abrolhos_stereo-BRUVs<br>npz6.36|Biota|Ascidians|NA|NA|NA|NA|NA|NA|NA|NA|NA|35000000|2|



##### Benthos Length File 

- A Benthos Length file is required for Synthesis imports. 

- Benthos Length file must be provided as a comma-separated values (.csv) file. 

- Recommended naming suffix: _benthos-length.csv 

**Table 7** . First four lines of an example Benthos Length file from two stereo-BRUVs <u>Campaigns</u> ready to be imported into a <u>Synthesis. The</u> opcode column is used to define the <u>sample.</u> 

|**campaignid**|**opcode**|**caab_code**|**count**<br>**level_1**|**level_2**|**level_3**|**level_4**|**level_5**|**level_6**|**level**<br>**_7**|**level_8**|**family**|**genus**|**species**|**length**<br>**_mm**|**precision**<br>**_mm**|**range_m**<br>**m**|**rms_mm**|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|2021-05_Abrolhos<br>_stereo-BRUVs|npz6.21|20000000|1<br>Biota|Bryozoa|NA|NA|NA|NA|NA|NA|NA|NA|NA|NA|NA|1623.84<br>2|7.40469|
|2021-05_Abrolhos<br>_stereo-BRUVs|npz6.22|11168902|1<br>Biota|Cnidaria|Corals|Black &<br>Octocorals|Branching<br>(3D)|NA|NA|NA|NA|NA|NA|NA|NA|2735.87<br>4|25.3675<br>6|
|2025-02_Yamatji-<br>Shallow-Bank_ster<br>eo-BRUVs|SB-BV-051|82001015|1<br>Physical|Substrate|Unconsolidated<br>(soft)|Sand /<br>mud<br>(<2mm)|Fine sand<br>(no shell<br>fragments)|NA|NA|NA|NA|NA|NA|131.10<br>67|4.0386|2899.08<br>5|0.69228|
|2025-02_Yamatji-<br>Shallow-Bank_ster<br>eo-BRUVs|SB-BV-022|35000000|1<br>Biota|Ascidians|NA|NA|NA|NA|NA|NA|NA|NA|NA|135.42<br>85|3.09009|1672.06<br>9|1.07326|



##### Benthos Relief File 

- A Benthos Relief file is required for <u>Synthesis</u> imports. 

- Benthos Relief file must be provided as a comma-separated values (.csv) file. 

- Recommended naming suffix: _benthos-relief.csv 

**Table 8** . First four lines of an example Benthos Relief file from two stereo-BRUVs Campaigns ready to be imported into a Synthesis. The opcode column is used to define the <u>sample.</u> 

|**campaignid**|**opcode**|**level_1**|**level_5**|**level_2**|**level_3**|**level_4**|**level_6**|**level_7**|**level_8**|**family**|**genus**|**species**|**caab_code**|**count**|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|2021-05_Abrolhos_stereo-BRUVs|npz6.10|Physical|0|Relief|Flat|NA|NA|NA|NA|NA|NA|NA|82003001|20|
|2021-05_Abrolhos_stereo-BRUVs|npz6.14|Physical|2|Relief|Low /<br>moderate|Moderate<br>(1-3m)|NA|NA|NA|NA|NA|NA|82003004|4|
|2021-05_Abrolhos_stereo-BRUVs|npz6.14|Physical|3|Relief|High|High<br>(>3m)|NA|NA|NA|NA|NA|NA|82003006|22|
|2021-05_Abrolhos_stereo-BRUVs|npz9.5|Physical|4|Relief|High|Wall|NA|NA|NA|NA|NA|NA|82003007|3|





<!-- Start of picture text -->
The Length Annotation data contain values in the period column. Since this<br>column was not used in the given Sample Metadata, these values will be ignored<br>& oe when matching to Samples. If you intended to use this column to identify 7)<br>Samples, please ensure that the corresponding column in the Sample Metadata<br>contains values.<br>The Point Annotation data contain values in the period column. Since this column<br>was not used in the given Sample Metadata, these values will be ignored when<br>® Ci] Matching to Samples. If you intended to use this column to identify Samples, 2<br>please ensure that the corresponding column in the Sample Metadata contains<br>values.<br>The 3D Point Annotation data contain values in the period column. Since this<br>column was not used in the given Sample Metadata, these values will be ignored<br>® [i] when matching to Samples. If you intended to use this column to identify @<br>Samples, please ensure that the corresponding column in the Sample Metadata<br>contains values.<br><!-- End of picture text -->



The Benthos Count data contain rows which reference Sample Metadata which do not occur in the provided Sample Metadata files, or were dropped by preceding checks. These a rows will be dropped, but this means there is a serious problem with the input data. _ e Perhaps some Sample Metadata files were not submitted? 



Some Samples have the exact same coordinates as other Samples within the same campaign. Unless coordinates are recorded extremely imprecisely, this is a sign that se locations may have been accidentally repeated. Please review the data to ensure they are — correct. 



<!-- Start of picture text -->
Py<br><!-- End of picture text -->

### Appendix 6 - Data privacy and sharing agreements 

##### Data Privacy 

All data uploaded to GlobalArchive is held securely within NeCTAR Cloud infrastructure within a secure industry standard database. 

High-level metadata for all uploads (either in <u>Projects</u> or Syntheses) is publicly available as <mark>outlined in the Privacy Policy. Data</mark> <u><mark>Custodians can choose whether Sample Metadata (e.g. )</mark></u> <mark>and annotation, count or length data are open-access.</mark> 

<mark>Any associated data (e.g. species, count and length) can be “Shared” with another User by the data Custodian.</mark> 

<mark>The Administrator of GlobalArchive, can only delete a User’s data – not access it.</mark> 

##### Data sharing agreements 

<mark>Formal data sharing agreements can be implemented as part of GlobalArchive “Syntheses”, more information on this coming soon.</mark> 

### Appendix 7 - Privacy Policy 

<mark>We take the privacy of our users very seriously at GlobalArchive. Most importantly, we do not and never will sell anyone's information to a third party.</mark> 

<mark>This Privacy Policy gives a few more details about how we collect, use, maintain and disclose information collected from you when you use our site.</mark> 

##### Personal identification information 

<mark>We will collect personal identification information from you in a variety of ways, including when you register on the site and share</mark> <u><mark>C</mark> ampaign</u> <u><mark>s, Projects</mark></u> <mark>or Syntheses with other users. When registering users are asked for their full name, username, email address, affiliation(s), short biography and to provide external links to personal, institutional or other sites (e.g. Google Scholar, ResearchGate).</mark> 

<mark>We will collect personal identification information from you only if you voluntarily submit such information to us by registering as a user. You can always refuse to supply the information.</mark> 

##### Google and cookies 

<mark>This website uses</mark> <u><mark>Google Analytics, a web analytics service provided by Google, Inc. ('Google').</mark></u> <mark>Google Analytics uses 'cookies', which are text files placed on your computer, to help the website analyse how users use the site. The information generated by the cookie about your use of the website (including your IP address) will be transmitted to and stored by Google on servers. Google will use this information for the purpose of evaluating your use of the website, compiling reports on website activity for website operators and providing other services relating to website activity and internet usage.</mark> 

<mark>Google may also transfer this information to third parties where required to do so by law, or where such third parties process the information on Google's behalf. Google will not associate your IP address with any other data held by Google. You may refuse the use of cookies by selecting the appropriate settings on your browser, however, please note that if you do this you may not be able to use the full functionality of this website. By using this website, you consent to the processing of data about you by Google in the manner and for the purposes set out above.</mark> 

##### How we use collected information 

<mark>We will use your personal information for the following purposes:</mark> 

###### **<mark>Publically available</mark>** 

<mark>If you are the custodian of a Project, your username/email will be publicly available on the site, via the Explore page.</mark> 

###### **<mark>For logged in users</mark>** 

<mark>Additionally, if a GlobalArchive user is logged in, they can also see the email address, affiliation(s), short biography and external links provided by each user. In addition, logged in users will be able to see who is the custodian (or creator) of each Project or</mark> <u><mark>Synthesis. For Projects or Synthesis</mark></u> <mark>that</mark> 

<mark>were created by a user or shared with a user, that user will also be able to see the usernames of each user that the Project or</mark> <u><mark>Synthesis is shared with.</mark></u> 

###### **<mark>To send periodic emails</mark>** 

<mark>We may use the email address to send you information and updates about the site. It may also be used to respond to your inquiries, questions, and/or other requests. To be completely clear about this — you'll only get these from us, we'll never pass them on to "partners" or anything like that.</mark> 

##### How we protect your information 

<mark>We adopt appropriate data collection, storage and processing practices and security measures to protect against unauthorized access, alteration, disclosure or destruction of your personal information, username, password, transaction information and data stored on our site.</mark> 

<mark>All communication between the site and you happens over an SSL secured channel and is encrypted and protected with digital signatures.</mark> 

##### Sharing your personal information 

<mark>We do not sell, trade, or rent your personal identification information to others. We may share generic aggregated demographic information not linked to any personal identification information regarding visitors and users in academic publications or conference presentations.</mark> 

##### Third party websites 

<mark>Any other sites that we link to will have their own privacy policies, which might differ from ours.</mark> 

##### Changes to this privacy policy 

<mark>We have the right to update this privacy policy at any time. If we do, we will revise the updated date at the bottom of this page and email all users.</mark> 

##### Your acceptance of these terms 

<mark>By using this site, you signify your acceptance of this policy. If you do not agree to this policy, please do not use our site. Your continued use of the site following the posting of changes to this policy will be deemed your acceptance of those changes.</mark> 

##### Contacting us 

<mark>If you have any questions about this Privacy Policy, the practices of this site, or your dealings with this site, please contact tim.langlois@uwa.edu.au</mark> 

