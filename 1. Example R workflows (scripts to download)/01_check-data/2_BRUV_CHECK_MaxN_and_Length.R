# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets4)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study <- "Parks-Ningaloo-synthesis" 

## Set your working directory ----
working.dir <- getwd()

## Save these directory names to use later----
staging.dir <- paste(working.dir,"data/staging",sep="/") 
download.dir <- paste(working.dir,"data/raw/EM Export",sep="/")
tidy.dir <- paste(working.dir,"data/tidy",sep="/")
plots.dir <- paste(working.dir,"figures/format",sep="/")
error.dir <- paste(working.dir,"data/errors to check",sep="/")

# Import unchecked data from staging folder----
setwd(staging.dir)

# Import metadata ---
metadata <- read_csv(paste(study,"metadata.csv",sep="_"), col_types = cols(sample = col_character()))

# Import MaxN file---
maxn <- read_csv(paste(study,"maxn.csv",sep="_"), col_types = cols(sample = col_character()), na = c("", " ", NA)) %>%
  mutate(maxn=as.numeric(maxn)) %>%
  mutate(species=tolower(species)) %>%
  dplyr::select(campaignid,sample,family,genus,species,maxn) %>%
  replace_na(list(family = "Unknown", genus = "Unknown", species = "spp")) %>% # remove any NAs in taxa name
  glimpse()

unique(maxn$sample)

# Import length/3d file----
length <- read_csv(file = paste(study,"length3dpoints.csv",sep = "_"),
                   col_types = cols(sample = col_character()), na = c("", " ", NA)) %>% #, 
  mutate(number = as.numeric(number)) %>%
  mutate(range = as.numeric(range)) %>%
  mutate(length = as.numeric(length)) %>%
  dplyr::select(campaignid,sample,family,genus,species,length,number,range) %>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  replace_na(list(family="Unknown",genus="Unknown",species="spp")) %>% # remove any NAs in taxa name
  mutate(species = tolower(species)) %>%
  # mutate(genus = str_replace_all(.$genus,c("NA"="Unknown"))) %>%
  glimpse()

unique(length$sample)

# BASIC checks----
# Check if we have 3d points (Number) in addition to length----
three.d.points <- length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? 

# Check if we have more than one fish associated with single length measurement----
schools <- length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? 

# Plot to visualise length data ----
# Add justification and units to x
setwd(plots.dir)

theme_ga <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Length vs. Density ----
# Use this plot to visualise the length distribution in your data
ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Length (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.length.png",sep = "_"))

# Range vs. Density ---
# Use this plot to visulaise the range distribution in your data
ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.range.png",sep = "_"))

# Plot to visualise Range vs Length data ---
# note that only 3Dpoints typically occur past 10m---
ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Length (mm)")+theme_ga

ggsave(file=paste(study,"check.range.vs.length.png",sep = "_"))

# Standardise for Range ----
# To standardise for Range we can remove any length observations outside Range rules
# i.e. the length data, and any abundance calculated from it, will be restricted by this Range

summary(length$range) # shows min, mean and max range

out.of.range<-filter(length,range>8000)%>% # 10 m = 10000 mm
  glimpse() # Shows fish more than 10 m away

# SERIOUS data checks using the life.history googlesheet ----
# Checks on fish length vs their max.length in the life.history sheet will be done below
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"

master<-googlesheets4::read_sheet(url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('NW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  distinct()%>%
  glimpse()

synonymsurl <- "https://docs.google.com/spreadsheets/d/1R0uU9Q0VkUDQFgGTK3VnIGxmc101jxhlny926ztWoiQ/edit?ts=5e6f37a2#gid=567803926"

synonyms<- googlesheets4::read_sheet(synonymsurl)%>%
  distinct()%>%
  ga.clean.names()%>%
  dplyr::select(-comment)

# Update by synonyms ----
# This function will change the names of species that have been reclassified (i.e. Pagrus auratus to Chrysophrys auratus). This function also fixes some common spelling mistakes (i.e. Chyrosophyrs	auratus to Chrysophrys auratus)

# Use return.changes=T to view the taxa.names.updated
# Use save.report to save .csv file in your error directory

maxn <- ga.change.synonyms(maxn,return.changes=T,save.report = T)
length <- ga.change.synonyms(length,return.changes=T,save.report = T)

# Check MaxN for species that have not previously been observed in your region ----
maxn.species.not.previously.observed<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>% 
  distinct(campaignid,sample,family,genus,species)%>% # use this line to show specific drops OR
  # distinct(family,genus,species)%>% # use this line to keep only fam, gen, spe
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(maxn.species.not.previously.observed,file=paste(study,"maxn.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length for species that have not previously been observed in your region ----
#maxn.species.not.previously.observed
length.species.not.previously.observed<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(campaignid,sample,family,genus,species)%>%
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(length.species.not.previously.observed,file=paste(study,"length.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length measurements vs. maximum length in life.history----
# 1. Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---

family.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max))%>%
  ungroup()

genus.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(genus)%>%
  dplyr::summarise(genuslength_max=mean(fb.length_max))%>%
  ungroup()

# 2. Create a new master list with family and genus average maximum length where missing species max.length ----
# In this example fish bigger than 85% of the maximum length recorded on FishBase will show up as an error ("too big").
# We think this is a better way to check lengths, because extremelly big individuals should be re-checked. For example on Fishbase the maximim length for Tiger Sharks (Galeocerdo cuvier) is 7.5 m, however we think we should really be checking any sharks bigger than 6 m rather than just those over 7.5 m.
# In this example the minimum length is 15% of the maximum on fishbase, fish that are smaller will show up as an error ("too small").
# you can change these values to be stricter (e.g. increase the minimum value and decrease the maximum) or more tolerant (decrease min and increase max) below.

master.min.max<-left_join(master,family.max.length,by=c("family"))%>% # add in family values
  left_join(.,genus.max.length)%>% # add in genus values
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),genuslength_max,fb.length_max))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max,genuslength_max))%>%
  mutate(min.length=0.15*fb.length_max)%>% # change values here
  mutate(max.length=0.85*fb.length_max)%>% # change values here
  glimpse()

# 3. Create list of wrong length, ordered by %error, for checking in .EMObs files or removing from data----
wrong.length.taxa<-left_join(length,master.min.max,by=c("family","genus","species"))%>%
  dplyr::filter(length<min.length|length>max.length)%>%
  mutate(reason=ifelse(length<min.length,"too small","too big"))%>%
  dplyr::select(campaignid,sample,family,genus,species,length,min.length,max.length,fb.length_max,reason)%>%
  mutate(difference=ifelse(reason%in%c("too small"),(min.length-length),(length-max.length)))%>%
  dplyr::mutate(percent.of.fb.max=(length/fb.length_max*100))%>%
  glimpse()

setwd(error.dir)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)

# You should view this .csv outside of R and revisit the .EMObs to check the lengths. Some of the length measurements could be correct.
# We suggest sorting OpCode and then checking and/or fixing each .EMObs file one at a time.

# Check MaxN per species/family vs. StereoMaxN, e.g. how many lengths are missing from the MaxN ----
# In this part we are checking the maxn (for each species) against the number measured (length and 3D point)
# this is important if you use the 3D measurements (3D points and length) for abundance 
# E.g. number above legal size

length.sample <- length %>% distinct(campaignid,sample) # only examine samples where lengths were possible

# summarise length and then compare to maxn
taxa.maxn.vs.stereo.summary <- length %>%
  group_by(campaignid,sample,family,genus,species)%>%
  dplyr::summarise(stereo.maxn=sum(number))%>%
  full_join(maxn)%>%
  replace_na(list(maxn=0))%>%
  dplyr::filter(!stereo.maxn==maxn)%>%
  mutate(percent.difference = (maxn-stereo.maxn)/maxn*100)%>%
  semi_join(length.sample)%>% # only keep ones where length was possible
  replace_na(list(percent.difference=1))%>%
  filter(!percent.difference%in%c(0))%>% #only for those that have missing lengths
  mutate(difference=(maxn-stereo.maxn))%>%
  mutate(difference=abs(difference))%>%
  mutate(percent.difference=abs(percent.difference))%>%
  dplyr::select(campaignid,sample,family,genus,species,maxn,stereo.maxn,difference,percent.difference)%>%
  arrange(-difference)%>%
  glimpse()

setwd(error.dir)
write.csv(taxa.maxn.vs.stereo.summary,file=paste(study,"taxa.maxn.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

# Plot of maxn vs stereo maxn (length and 3D point)
ggplot(taxa.maxn.vs.stereo.summary,aes(x=maxn,y=stereo.maxn,label = paste(genus,species,sep=" ")))+
  geom_abline(colour="red",alpha=0.5)+
  geom_point()+
  geom_text(alpha=0.2)+theme_ga
setwd(plots.dir)
ggsave(file=paste(study,"check.stereo.vs.maxn.png",sep = "_"))


# We strongly encourage you to fix these errors at the source (i.e. EMObs), however, there may be observations that you want to keep in the raw data but not upload to Global Archive (i.e. seasnakes), that you can drop using the code below.
# NOW check through the files in your "Errors to check" folder and make corrections to .EMObs / generic files and then re-run this script.
# IF you are happy to proceed by removing the species, length and range errors here you can run the lines below and write the checked data 

# Drop errors from data ----

# WRITE FINAL checked data----
setwd(tidy.dir)
dir()

write.csv(metadata, file=paste(study,"checked.metadata.csv",sep = "."), row.names=FALSE)
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.csv",sep = "."), row.names=FALSE)

setwd(working.dir)
# Go to FORMAT script (3) 
