# This script uses the CAAB codes and scientific names from script 01 to scrape from the CSIRO CAAB website
# This is to get information on recent taxonomic name changes

# Load libraries required
library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(CheckEM)
library(stringr)

# Read in the caab code list created in the first script
caab <- readRDS("annotation-schema/data/staging/caab-with-regions.RDS") %>%
  dplyr::mutate(scientific.name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(!species == "spp")

## Scrape information from the website by looping through all CAAB codes (this takes roughly an hour with ~5100 codes)
## Remove the hashes in this section to run again
# temp.info <- data.frame()
# 
# for (caab.code in unique(caab$caab_code)) {
# 
#   url <- paste0("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", caab.code)
# 
#   # Read the webpage content as lines
#   webpage <- readLines(url)
# 
#   # Send a GET request to the webpage and parse the HTML content
#   page <- read_html(url)
# 
#   # Find the second table on the webpage
#   table <- page %>% html_nodes("table")
# 
#   # Convert the table to a data frame
#   table_df <- html_table(table, fill = TRUE)
# 
#   # # Print the table
#   # print(table_df)
# 
#   caab.info <- table_df[[1]] %>%
#     pivot_wider(names_from = X1, values_from = X2) %>%
#     ga.clean.names()
# 
#   names(caab.info) <- (sub("[.]$", "", names(caab.info))) # Remove full stops from end of names
#   names(caab.info)
# 
#   species.info <- table_df[[2]] %>%
#     dplyr::select(X1, X2) %>%
#     dplyr::filter(X1 %in% c("Family:",
#                             "Standard Name (AS5300):",
#                             "Standard Name:",
#                             "Synonyms:",
#                             "WoRMS")) %>%
#     pivot_wider(names_from = X1, values_from = X2) %>%
#     ga.clean.names()
# 
#   names(species.info) <- (sub("[.]$", "", names(species.info))) # Remove full stops from end of names
#   names(species.info)
# 
#   info <- bind_cols(caab.info, species.info)
# 
#   temp.info <- bind_rows(temp.info, info)
# 
#   message(paste("up to ", nrow(temp.info), "of ", nrow(caab)))
# 
# }
# 
# saveRDS(temp.info, "annotation-schema/data/staging/scraped-caab.RDS")

info <- readRDS("annotation-schema/data/staging/scraped-caab.RDS")

# Remove spaces from CAAB
info$caab.code <- gsub(" \nshow as JSON", "", info$caab.code)
info$caab.code <- gsub("[^[:alnum:]]", "", info$caab.code)

# Need to split name into scientific and authority
clean.info <- info %>%
  tidyr::separate(scientific.nameand.authority, into = c("scientific.name"), sep = " \n", remove = FALSE) %>%
  dplyr::mutate(scientific.name = trimws(scientific.name)) %>%
  dplyr::mutate(scientific.name = str_replace_all(.$scientific.name, c("^(\\S+\\s+\\S+)\\s+" = "\\1"))) %>%
  tidyr::separate(scientific.name, into = c("genus", "species"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(family = str_replace_all(.$family, c('[[:digit:]]+' = "", "  " = "", "Rock Whitings" = "Labridae", "\\(|\\)" = ""))) %>%
  tidyr::separate(family, into = c("family"), sep = " ") %>%
  dplyr::filter(is.na(attention.non.current.taxon.this.record.represents.an.old.code.which.has.been.superseded.by.a.new.active.caab.code.see.taxon.notes.for.more.details.if.available)) %>%
  dplyr::filter(is.na(attention.non.current.taxon.this.record.represents.an.obsolete.code.that.currently.has.no.corresponding.active.code.in.caab.see.taxon.notes.for.more.details.if.available)) %>%
  dplyr::mutate(common.name = if_else(is.na(standard.name.as5300), standard.name, standard.name.as5300)) %>%
  dplyr::mutate(worms = str_replace_all(.$worms, c("urn:lsid:marinespecies.org:taxname:" = ""))) %>%
  tidyr::separate(worms, into = c("worms.id", "worms.valid"), sep = "\n") %>%
  dplyr::select(caab.code, family, genus, species, scientific.name, common.name, worms.id, worms.valid, synonyms) %>%
  glimpse()
  
unique(clean.info$scientific.name)
unique(clean.info$synonyms) # a mess - will need to do separately
unique(clean.info$family)# looks mostly ok
unique(clean.info$genus)# looks mostly ok
unique(clean.info$species)# looks mostly ok
unique(clean.info$worms.id)# looks mostly ok
unique(clean.info$worms.valid)

# The synonyms are very messy, this is a very manual way of removing inconsistencies
synonyms <- clean.info %>%
  dplyr::select(caab.code, family, genus, species, scientific.name, synonyms) %>%
  # dplyr::filter(!is.na(synonyms)) %>%
  dplyr::mutate(synonyms = str_replace_all(.$synonyms, c("\\[misidentification\\]" = "misidentified, ",
                                                         "\\(misidentification\\)" = "misidentified, ",
                                                         " \\[Russell et al 1983: 20; Checklist of Fishes GBR Capricornia section - misidentification\\]" = "misidentified",
                                                         " \\(Steindachner 1892 \\[Thomson 1997:515 - misidentification\\] " = "misidentified, ",
                                                         " \\(Gilchrist & Thompson, 1911\\) \\[Thomson 1954:106 - misidentification\\]" =  "misidentified",
                                                         " Sih et al 2017 \\(misidentification of BRUV image - no specimen collected\\)" = " misidentified",
                                                         " \\[Whitley, 1951, Australian Faunal Directory, CAAB - misidentification\\]" = " misidentified",
                                                         " \\[in CAAB; misidentification, in part\\]" = " misidentified",
                                                         " Richardson, 1845 \\[Randall et al 1990:477 - Fishes of the Great Barrier Reef and Coral Sea. - misidentification, C. papua was considered a synonym of C. solandri at the time\\] \\(Australian Faunal Directory, accessed 30.6.2020\\)." = " misidentified",
                                                         " \\(Paxton et al 1989 - misidentification due to exclusion of subspecies at that time\\)" = " misidentified",
                                                         " \\[misidentification, see Bath \\(2004\\)\\]" =  " misidentified",
                                                         " \\[misidentifications - Waite, 1921: 198; Waite, 1923: 230-231\\]" =  " misidentified",
                                                         "\\[Hoese & Larson 2006 \\(ZooCat\\), misidentification\\]" =  " misidentified, ",
                                                         "\\[misidentification in Australia\\]" = "misidentified",
                                                         "\\[voucher specimen originally misidentified\\]" = "misidentified, ",
                                                         
                                                         " \\s*\\([^\\)]+\\)" = "",
                                                         "\\[.*?\\]" = "",
                                                         "  \n" = ", ",
                                                         "\n\n" = ", ",
                                                         "\n" = ", ",
                                                         "  " = ", ",
                                                         
                                                         "sp. A " = "sp. A, ",
                                                         "sp. B " = "sp. B, ",
                                                         "sp. B Ventrifossa" = "sp. B, Ventrifossa",
                                                         "sp. A Cephaloscyllium" = "sp. A, Cephaloscyllium",
                                                         "sp. F Coelorinchus" = "sp. F, Coelorinchus",
                                                         "sp. 5 ,Arius" = "sp. 5, Arius",
                                                         " Note that " = "",
                                                         
                                                         "sp. C " = "sp. C, ",
                                                         "sp. D " = "sp. D ",
                                                         "sp. E " = "sp. E, ",
                                                         "sp. J " = "sp. J, ",
                                                         "sp. F " = "sp. F, ",
                                                         
                                                         "sp. 1 " = "sp. 1, ",
                                                         "sp. 2 " = "sp. 2, ",
                                                         "sp. 3 " = "sp. 3, ",
                                                         "sp. 4 " = "sp. 4, ",
                                                         "sp. 5 " = "sp. 5 ,",
                                                         "sp. 9 " = "sp. 9, ",
                                                         
                                                         "sp. Br" = "sp., Br",
                                                         
                                                         "elongatus " = "elongatus, ",
                                                         "lacepede " = "lacepede, ",
                                                         "paplebratum " = "paplebratum, ",
                                                         "longispinis " = "longispinis, ",
                                                         " Richardson, 1842" = "",
                                                         ",," = ",",
                                                         " , " = ", ",
                                                         
                                                         "russelli " = "russelli, ",
                                                         " Smith & Radcliffe, 1912" = "",
                                                         "longicaudatus " = "longicaudatus, ",
                                                         "longicaudus " = "longicaudus, ",
                                                         "tumifrons " = "tumifrons, ",
                                                         " De Buen, 1960" = "",
                                                         " Latham, 1794 ; " = ", ",
                                                         "timorensis " = "timorensis, ",
                                                         " Alleyne & Macleay, 1877" = "",
                                                         "nebulosus " = "nebulosus, ",
                                                         "m Macleay, 1883 D" = "m, D",
                                                         "Mustelus sp.Mustelus sp. A" = "Mustelus sp., Mustelus sp. A",
                                                         " Whitley, 1950" = "",
                                                         " Macleay, 1883" = "",
                                                         "praepositum " = "praepositum, ",
                                                         "Corythoichthys sp. 2 . Note that Corythoichthys sp. 2 in Kuiter 2009 is a different species closely related to C. amplexus." = "Corythoichthys sp. 2",
                                                         "s Whitley 1954D" = "s, D",
                                                         "Plectranthias pallidus Randall & Hoese, 1995, Plectranthias kelloggi Sih et al 2017" = "Plectranthias pallidus, Plectranthias kelloggi",
                                                         "Cheilinus sp.Oxycheilinus sp." = "Cheilinus sp., Oxycheilinus sp.",
                                                         " Whitley, 1940" = "",
                                                         "Neoplatycephalus sp. 1Neoplatycephalus aurimaculatus" = "Neoplatycephalus sp. 1, Neoplatycephalus aurimaculatus",
                                                         " Jewett & Lachner 1983" = "",
                                                         " McCoy, 1874" = "",
                                                         "in most references prior to 2014" = "",
                                                         "\\[|\\]" = "",
                                                         " Garrick, 1957" = "",
                                                         "politus " = "politus, ",
                                                         " Chan 1966" = "",
                                                         " Anderson 2005" = "",
                                                         "serrulata " = "serrulata, ",
                                                         " Garrick, 1957" = "",
                                                         "politus " = "politus, ",
                                                         " Chan 1966" = ", Chan 1966",
                                                         "guileri " = "guileri, ",
                                                         " Lachner & McKinney, 1978" = " Lachner & McKinney, 1978",
                                                         " Whitley 1950" = "",
                                                         "godfreyi " = "godfreyi, ",
                                                         " in most Australian references prior to 2006, and some after this date" = "",
                                                         "i Whitley, 1936M" = "i, M",
                                                         "a Whitley, 1936M" = "a, M",
                                                         "sp. Hy" = "sp., Hy",
                                                         " Günther, 1880" = "",
                                                         "georgianus " = "georgianus, ",
                                                         "nobilis " = "nobilis, ",
                                                         " Zugmayer, 1911" = "",
                                                         "verrucosus " = "verrucosus, ",
                                                         "e Whitley 1959 S" = "e, S",
                                                         " Ogilby, 1898:291" = "",
                                                         " Smith & Böhlke, 1983" = "",
                                                         "sp. Jo" = "sp., Jo",
                                                         " Lachner & McKinney, 1978" = "",
                                                         ": Taylor, 1964; Gloerfelt-Tarp & Kailola, 1984; Sainsbury et al 1985; Hutchins & Swainston, 1986; Allen & Swainston, 1988; Paxton et al 1989; Allen, 1997; Knapp, 1999, Hoese et al 2006." = "",
                                                         "s sp. At" = "s sp., At",
                                                         "Ateleopus, cf japonicus" = "Ateleopus cf japonicus",
                                                         "aureus Ap" = "aureus, Ap",
                                                         " Kuiter, 1998" = "",
                                                         " Richardson, 1845 ." = "",
                                                         "japonicus Ura" = "japonicus, Ura",
                                                         " Jordan & Richardson, 1909" = "",
                                                         " Valenciennes, 1836" = "",
                                                         "olivacea St" = "olivacea, St",
                                                         " Randall et al 1997:96" = "",
                                                         " Chen 1981" = "",
                                                         "Last & Stevens, 1994" = "",
                                                         " Regan 1906" = "",
                                                         " Rüppell, 1837" = ", ",
                                                         " Whitley, 1934" = "",
                                                         "fasciatum Stegostoma" = "fasciatum, Stegostoma",
                                                         ", Whitley, 1939" = "",
                                                         " Klausewitz, 1960" = "",
                                                         "nus sp Het" = "nus sp, Het",
                                                         " Larson 1976" = "",
                                                         " Sakai & Nakabo, 2004" = "",
                                                         "barbatusMalakichthys" = "barbatus, Malakichthys",
                                                         "moretoniensis P" = "moretoniensis, P",
                                                         "praeteritus Arnoglossus" = "praeteritus, Arnoglossus",
                                                         " Whitley 1940" = "",
                                                         " Lönnberg 1907" = "",
                                                         " Davies 1950" = "",
                                                         "in most references prior to 2022" = "",
                                                         "coeruleopinnatus Carangoides" = "coeruleopinnatus, Carangoides",
                                                         " White, Ebert & Compagno 2008" = "",
                                                         "nigripinnisApogon" = "nigripinnis, Apogon",
                                                         "brevicaudatus Apogon" = "brevicaudatus, Apogon",
                                                         "; Hoese et al. 2006: 1680; Larson 2001: 203 " = ", ",
                                                         "poicilosomus Pseudogobius" = "poicilosomus, Pseudogobius",
                                                         "—Davis 1988: 164; Blaber et al. 1992: 9 " = ", ",
                                                         "Larson 1992: 9; Larson & Williams 1997: 370; Larson 1997: 41; Larson 1998: 36; Larson 1999b: 44 " = "",
                                                         ", ‘plain’—Hammer et al. 2021: 4." = "",
                                                         " Compagno & Stevens 1993" = "",
                                                         "annella Bryaninops" = "annella, Bryaninops",
                                                         " Regan 1909" = ", ",
                                                         "parvifasciatus C" = "parvifasciatus, C",
                                                         " in many Australian references prior to 2019" = "",
                                                         " Amaoka 1969" = ", ",
                                                         " or " = "",
                                                         "sp.Paraulopus" = "sp., Paraulopus",
                                                         ", Randall 1981" = "",
                                                         " Whitley, 1939" = "",
                                                         "diversidens Ratabulus" = "diversidens, Ratabulus",
                                                         " Whitley 1964: 56" = "",
                                                         "sp. LDipturus" = "sp. L, Dipturus",
                                                         "cirrhosa Scorpaenopsis" = "cirrhosa, Scorpaenopsis",
                                                         " Goode & Bean, 1896" = "",
                                                         ", Last & Compagno 1999" = "",
                                                         "sp. 1Doryrhamphus" = "",
                                                         " Johnson 1864" = "",
                                                         " Valenciennes, 1835" = "",
                                                         " Scott, 1935" = " Scott, 1935",
                                                         "kuhlii Neotrygon" = "kuhlii, Neotrygon",
                                                         "cavitiensisApogon" = "cavitiensis, Apogon",
                                                         "niger Malacosteus" = "niger, Malacosteus",
                                                         "chrysopomus Apogon" = "chrysopomus, Apogon",
                                                         "seali Apogon" = "seali, Apogon",
                                                         "spChlorophthalmus" = "sp, Chlorophthalmus",
                                                         "sp.Chlorophthalmus" = "sp., Chlorophthalmus",
                                                         " Bleeker, 1857 " = ", ",
                                                         "Hymenocephalus, sp." = "Hymenocephalus sp.",
                                                         "leuciscus Photoplagios" = "leuciscus, Photoplagios",
                                                         ", Smith, 1960" = "",
                                                         "ceramensis Apogon" = "ceramensis, Apogon",
                                                         ", Whitley, 1931" = "",
                                                         " Hoese et al 2006" = "",
                                                         ", Macleay, 1884" = "",
                                                         " Bleeker, 1846" = ", ",
                                                         " Ogilby, 1908" = ", ",
                                                         " Ogilby, 1910" = "",
                                                         "hexophtalma Parapercis" = "hexophtalma, Parapercis",
                                                         # "Tubbia sp, Tubbia sp.",
                                                         "ovatus Xystaema" = "ovatus, Xystaema",
                                                         "ovatum Xystaema" = "ovatum, Xystaema",
                                                         "sp. Philypnodon" = "sp., Philypnodon",
                                                         "annotata Amphotistius" = "annotata, Amphotistius",
                                                         " Borodin 1928" = "",
                                                         " De Vis, 1884" = "",
                                                         "favagineus Gymnothorax" = "favagineus, Gymnothorax",
                                                         " Bonaparte, 1840" = "",
                                                         " in CAAB, 2015-2017" = "",
                                                         "sephen Pastinachus" = "sephen, Pastinachus",
                                                         " Ogilby, 1899" = "",
                                                         "undulata Himantura" = "undulata, Himantura",
                                                         " Ogilby, 1910: 19" = "",
                                                         "Allen 1985: 2508; Paxton et al. 1989: 507; Hutchins 2001: 30; Allen et al. 2006: 991" = "",
                                                         " Horne, 2001" = "",
                                                         ", which is possibly restricted to the Red Sea" = "",
                                                         "moultoni Parascorpaena" = "moultoni, Parascorpaena",
                                                         " Larson & Takita, 2004" = ", ",
                                                         " Whitley 1953" = "",
                                                         "artocephalus Amblyotrypauchen" = "artocephalus, Amblyotrypauchen",
                                                         "rubrilineatus Brachyamblyopus" = "rubrilineatus, Brachyamblyopus",
                                                         " Whitley, 1931" = "",
                                                         " Kamohara, 1960" = "",
                                                         "sp. 1: Ziebell's morph Brachionichthys sp. 2: Loney's morph" = "sp. 1, Brachionichthys sp. 2",
                                                         "\\(|\\)" = "",
                                                         ",," = "",
                                                         "Cephaloscyllium sp. A Cephaloscyllium nascione" = "Cephaloscyllium sp. A, Cephaloscyllium nascione",
                                                         "Ventrifossa sp. B Ventrifossa sp." = "Ventrifossa sp. B, Ventrifossa sp.",
                                                         "Caelorinchus sp. F Coelorinchus sp. F" = "Caelorinchus sp. F, Coelorinchus sp. F",
                                                         "sp. 5 ,Arius" = "sp. 5, Arius",
                                                         "pectinifer Sphenanthias" = "pectinifer, Sphenanthias",
                                                         " mis-identification" = "",
                                                         "\\{|\\}" = "",
                                                         "notatusApogon" = "notatus, Apogon",
                                                         " cf " = ", ",
                                                         "Parequula, melbournensis" = "Parequula melbournensis",
                                                         "Coelorinchus, argus, Caelorinchus, argus" = "Coelorinchus argus, Caelorinchus argus",
                                                         "japonicum,Acropoma" = "japonicum, Acropoma",
                                                         " Scott" = "",
                                                         "\\: 19" = "",
                                                         ", 1935" = "",
                                                         "Lepidotrigla, riggsi" = "Lepidotrigla riggsi",
                                                         "Ocosia, zaspilota" = "Ocosia zaspilota",
                                                         "elongatus, australiensis" = "elongatus australiensis",
                                                         "Pseudanthias, rubrizonatus" = "Pseudanthias rubrizonatus",
                                                         "Ateleopus, japonicus" = "Ateleopus japonicus",
                                                         "Apogon, aureus" = "Apogon aureus",
                                                         "Uranoscopus, japonicus" = "Uranoscopus japonicus",
                                                         "Solocisquama, stellulata" = "Solocisquama stellulata",
                                                         "Malakichthys, barbatus" = "Malakichthys barbatus",
                                                         "Neosebastes, entaxis" = "Neosebastes entaxis",
                                                         "Pterygotrigla, multipunctata" = "Pterygotrigla multipunctata",
                                                         "Caelorinchus, maurofasciatus" = "Caelorinchus maurofasciatus",
                                                         "Ratabulus, diversidens" = "Ratabulus diversidens",
                                                         "Neosebastes, entaxis" = "Neosebastes entaxis",
                                                         "Trachyscorpia, cristulata" = "Trachyscorpia cristulata",
                                                         "  " = " ",
                                                         " Randall & Hoese, 1995" = "",
                                                         "Taenioides limicola, misidentified " = "Taenioides limicola misidentified, ",
                                                         "Lethrinus nebulosus, misidentified" = "Lethrinus nebulosus misidentified",
                                                         "Mola mola, misidentified" = "Mola mola misidentified",
                                                         "Gymnothorax melanospilusmisidentified" = "Gymnothorax melanospilus misidentified",
                                                         "Liza diademamisidentified" = "Liza diadema misidentified"))) %>% 
  dplyr::mutate(synonyms = trimws(synonyms)) %>%
  # dplyr::mutate(tidy.synonym = if_else(str_count(synonyms, " ") == 1, synonyms, NA)) %>%
  # dplyr::mutate(synonyms = if_else(!is.na(tidy.synonym), NA, synonyms)) %>% # temp to find which ones still need work
  # dplyr::mutate(test = if_else(str_count(synonyms, " ") == 3, synonyms, NA)) 
  dplyr::mutate(synonyms = strsplit(as.character(synonyms), split = ", "))%>%
  unnest(synonyms) %>%
  dplyr::mutate(synonyms = trimws(synonyms)) %>%
  dplyr::filter(!str_detect(synonyms, "sp.|sp |Trimma DFH 8|sp$|misidentified")) %>%
  
  dplyr::mutate(synonyms = str_replace_all(.$synonyms, c("[.]$" = "",
                                                         "[,]$" = ""))) %>%
  
  dplyr::mutate(synonyms = trimws(synonyms)) %>%
  dplyr::mutate(family = str_replace_all(.$family, c("[^[:alnum:]]" = "", "[[:punct:]]" = "", "[:]$" = ""))) %>%
  dplyr::mutate(synonym.family = family) %>%
  tidyr::separate(synonyms, into = c("synonym.genus", "synonym.species"), sep = " ") %>%
  dplyr::filter(!scientific.name == paste(synonym.genus, synonym.species))

unique(synonyms$family) %>% sort()

# Test for actual species that are "synoynms"
test.dat <- synonyms %>% dplyr::mutate(scientific.name = paste(synonym.genus, synonym.species)) %>% distinct(scientific.name)

synonym.is.actual.species <- semi_join(test.dat, clean.info) %>%
  tidyr::separate(scientific.name, into = c("synonym.genus", "synonym.species")) # 74 species where the synonym is also a species, need to remove these from final data

final.synonyms <- anti_join(synonyms, synonym.is.actual.species) # remove any synonyms that are also a valid species name

test.multiple.matches <- final.synonyms %>%
  dplyr::group_by(synonym.family, synonym.genus, synonym.species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

final.synonyms <- anti_join(final.synonyms, test.multiple.matches) 

final.caab.codes <- clean.info %>%
  dplyr::select(-c(synonyms))

saveRDS(final.synonyms, "annotation-schema/data/staging/australia_fish_caab-synonyms.RDS")
saveRDS(final.caab.codes, "annotation-schema/data/staging/australia_fish_caab-codes_common-names.RDS")
