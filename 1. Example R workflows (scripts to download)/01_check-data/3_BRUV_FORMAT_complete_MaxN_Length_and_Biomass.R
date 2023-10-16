# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets4)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(fst)

# Study name---
study<-"Parks-Ningaloo-synthesis" 

## Set your working directory ----
working.dir<-getwd()

## Save these directory names to use later---- 
tidy.dir<-paste(working.dir,"data/tidy",sep="/")
plots.dir=paste(working.dir,"figures/format",sep="/")
error.dir=paste(working.dir,"data/errors to check",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Read in metadata----
metadata<-read_csv(file=paste(study,"checked.metadata.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  glimpse()

# Make complete.maxn: fill in 0s and join in factors----
dat<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  full_join(metadata)%>%
  dplyr::select(c(id,campaignid,sample,family,genus,species,maxn))%>%
  tidyr::complete(nesting(id,campaignid,sample),nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0))%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>% #always a good idea to ungroup() after you have finished using the group_by()!
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::select(sample,scientific,maxn)%>%
  spread(scientific,maxn, fill = 0)%>% #why do we need this?
  glimpse()

# Make family, genus and species names to merge back in after data is complete ---
maxn.families<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  filter(!(family=="Unknown"))%>%
  dplyr::select(c(family,genus,species,scientific))%>%
  distinct()%>% #to join back in after complete
  glimpse()

# Make complete data and join with metadata
complete.maxn<-dat%>%
  gather(key=scientific, value = maxn,-sample)%>%
  inner_join(maxn.families,by=c("scientific"))%>%
  full_join(metadata)%>% # Joining metadata will use a lot of memory - # out if you need too
  glimpse()

unique(dat$sample)
unique(complete.maxn$sample)

# Make complete.length.number.mass: fill in 0s and join in factors----
length.families<-read_csv(file=paste(study,"checked.length.csv",sep = "."),na = c("", " "))%>%
  filter(!(family=="Unknown"))%>%
  dplyr::select(family,genus,species)%>%
  distinct()%>% #to join back in after complete
  glimpse()

complete.length.number<-read_csv(file=paste(study,"checked.length.csv",sep = "."))%>% #na = c("", " "))
  filter(!family=="Unknown")%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%                         # Using IDs for when we have multiple campaigns
  dplyr::right_join(metadata ,by = c("id","campaignid", "sample")) %>% # add in all samples
  dplyr::filter(!successful.length %in% "No") %>%
  dplyr::select(id,campaignid,sample,family,genus,species,length,number,range) %>%
  tidyr::complete(nesting(id,campaignid,sample),nesting(family,genus,species)) %>%
  replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup()%>%
  dplyr::filter(!is.na(number))%>% # this should not do anything
  mutate(length=as.numeric(length))%>%
  left_join(.,metadata)%>%
  glimpse()

length(unique(metadata$id)) # 196
length(unique(complete.length.number$id)) # 168

# Make the expanded length data----
# For use in length analyses - i.e KDE or histograms
expanded.length<-complete.length.number%>%
  filter(!is.na(length))%>%
  uncount(number)%>%
  glimpse()

setwd(plots.dir)
ggplot(data=expanded.length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
ggsave(file=paste(study,"check.length.png",sep = "_"))

ggplot(data=expanded.length, aes(y=as.numeric(length))) +
  geom_boxplot(col="red",
                 fill="blue",
                 alpha = .2)

# Make mass data from complete.length.number----
# There are 6 steps
# 1. use life.history---
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  #filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>% 
  distinct()%>%
  glimpse()

# 2. Check for missing length weight relationship ---
taxa.missing.lw <- complete.length.number%>%
  distinct(family,genus,species)%>%
  anti_join(filter(master,!is.na(a)), by=c("family","genus","species"))%>%
  glimpse()

# Missing Genus length weight ---
genus.missing.lw <- complete.length.number%>%
  distinct(genus)%>%
  anti_join(filter(master,!is.na(a)), by="genus")%>%
  glimpse()

# Missing Family length weight ----
family.missing.lw <- complete.length.number%>%
  distinct(family)%>%
  anti_join(filter(master,!is.na(a)), by="family")%>%
  glimpse()

#3. Fill length data with relevant a and b and if blank use family---
length.species.ab<-master%>% #done this way around to avoid duplicating Family coloum
  dplyr::select(-family)%>%
  inner_join(complete.length.number,., by=c("genus","species")) # only keeps row if has a and b

# 4. Make family length.weigth
family.lw <- master%>%
  dplyr::group_by(family,length.measure)%>%
  dplyr::mutate(log.a = log10(a))%>%     
  dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                   b = mean(b, na.rm = T),
                   all = mean(all, na.rm = T),
                   bll = mean(bll, na.rm = T))%>%
  dplyr::filter(!is.na(a))%>%
  dplyr::mutate(all=str_replace_all(all,"NaN","0"))%>%
  dplyr::mutate(bll=str_replace_all(bll,"NaN","1"))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(rank = ifelse(length.measure=="FL",1,ifelse(length.measure=="TL", 2, 3)))%>%
  dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE))%>%
  dplyr::filter(min.rank == 0)%>%
  glimpse()

length.family.ab<-complete.length.number%>%
  anti_join(master, by=c("genus","species"))%>%
  left_join(family.lw, by="family")

# 5. Fill length data with relevant a and b and if blank use family---
complete.length.number.mass<-length.species.ab%>%
  bind_rows(length.family.ab)%>%
  dplyr::filter(!is.na(a))%>% #this gets rid of species with no lw
  mutate(length.cm = length/10)%>%
  mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL","SL"), 0, all))%>% # Temporary fix, remove later
  mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL","SL"), 1, bll))%>% # Temporary fix, remove later
  mutate(adjLength = ((length.cm*bll)+all)) %>% 
  mutate(mass.g = (adjLength^b)*a*number)%>%
  dplyr::select(c(sample,family,genus,species,length,range,number,mass.g,length.cm))%>%
  inner_join(metadata)%>%
  glimpse()

# 6. Checks of distribution and species estimates---
# Check - distribution of mass.g
setwd(plots.dir)
ggplot(data=complete.length.number.mass, aes(as.numeric(mass.g))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
ggsave(file=paste(study,"check.mass.g.png",sep = "."))

# Check - length.cm vs. mass.g
ggplot(data=complete.length.number.mass, aes(length.cm,mass.g)) +
  geom_point(alpha=0.25)+
  geom_smooth(alpha=0.05, se=FALSE)
ggsave(file=paste(study,"check.length.cm.vs.mass.g.png",sep = "."))


# Check the mass estimates across species - in kg's----
check.mass<- complete.length.number.mass %>%
  dplyr::group_by(family,genus,species)%>%
  filter(mass.g>0)%>%
  dplyr::mutate(mass.kg.individual = (mass.g/number)/1000)%>% # Work out the mass per individual fish
  dplyr::mutate(length=length/10)%>%
  mutate(length=round(length,digits=2))%>%
  dplyr::summarise(mean.kg = mean(mass.kg.individual, na.rm = TRUE),max.kg = max(mass.kg.individual, na.rm = TRUE),min.kg = min(mass.kg.individual, na.rm = TRUE),min.length = min(length, na.rm = TRUE),mean.length = mean(length, na.rm = TRUE),max.length = max(length, na.rm = TRUE))%>%
  arrange(-mean.kg)%>%
  glimpse()%>%
  mutate(mean.kg=round(mean.kg,digits=3))%>%
  mutate(max.kg=round(max.kg,digits=3))%>%
  mutate(min.kg=round(min.kg,digits=3))%>%
  mutate(mean.length=round(mean.length,digits=2))%>%
  arrange(-mean.kg)%>%
  glimpse()

setwd(error.dir)
write.csv(check.mass,file=paste(study,"check.mass.csv",sep = "_"), row.names=FALSE)

# CHECK these mass estimates before using them!!!
# WRITE FINAL complete and expanded data----
setwd(tidy.dir)
dir()

write.csv(complete.maxn, file=paste(study,"complete.maxn.csv",sep = "."), row.names=FALSE)

write.csv(complete.length.number, file=paste(study,"complete.length.csv",sep = "."), row.names=FALSE)

write.csv(expanded.length, file=paste(study,"expanded.length.csv",sep = "."), row.names=FALSE)

write.csv(complete.length.number.mass, file=paste(study,"complete.mass.csv",sep = "."), row.names=FALSE)

setwd(working.dir)
