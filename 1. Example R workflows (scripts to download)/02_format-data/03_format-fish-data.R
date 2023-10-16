###
# Project: Parks OMP Ningaloo
# Data:    BRUV and BOSS fish MaxN and length data
# Task:    Format data for use in FSSgam model selection
# Author:  Claude Spencer & Brooke Gibbons from beckyfisher - FSSgam
# Date:    October 2022
##

rm(list=ls())

# libraries----
library(tidyverse)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(purrr)
library(readr)
library(corrr)
library(terra)
library(sf)


campaignid <- "Parks-Ningaloo-synthesis"                                        # CampaignID name for the synthesis
name       <- "Parks-Ningaloo-synthesis"                                        # Why both??

# Load and join datasets
# MaxN
maxn <- list.files(path = "data/tidy/",
                           pattern = "*.complete.maxn.csv",
                           full.names = T) %>%
  lapply(., function(x){read.csv(x)}) %>%
  bind_rows() %>%
  mutate(method = if_else(str_detect(campaignid, "BOSS"), "BOSS", "BRUV"),
         sample = as.character(sample),
         site = as.character(site)) %>%
  glimpse()

# Length
length <- list.files(path = "data/tidy/",
                   pattern = "*.complete.length.csv",
                   full.names = T) %>%
  lapply(., function(x){read.csv(x)}) %>%
  bind_rows() %>%
  mutate(method = if_else(str_detect(campaignid, "BOSS"), "BOSS", "BRUV"),
         scientific = paste(family, genus, species, sep = " "),
         sample = as.character(sample),
         site = as.character(site)) %>%
  glimpse()

# Habitat
allhab <- readRDS(paste(paste0('data/tidy/', name), 
                        'nesp-habitat-bathy-derivatives.rds', sep = "_")) %>%   # Only  few missing from here
  ga.clean.names() %>%
  transform(sand = sand / broad.total.points.annotated,
            inverts = inverts / broad.total.points.annotated) %>%
  dplyr::mutate(habitat.class = ifelse(inverts > 0.2, "inverts","sand")) %>%
  dplyr::select(-c(x, y)) %>%
  glimpse()

metadata <- maxn %>%
  distinct(campaignid, sample, method,latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  top_n(10)%>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid, scientific, sample, method) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 4:(ncol(.))], na.rm = TRUE )) %>% # Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 4:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(campaignid, sample, method, total.abundance, species.richness) %>%
  tidyr::gather(., "scientific", "maxn", 4:5) %>%
  # dplyr::mutate(sample = as.character(sample)) %>%
  dplyr::glimpse()

dat.maxn <- bind_rows(ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::mutate(depth = ifelse(depth %in% "?", z, depth)) %>%                   # Samples didn't have in situ depth recorded
  dplyr::mutate(depth = as.numeric(depth)) %>%                                  # To numeric
  dplyr::mutate(depth = ifelse(depth > 0, depth * -1, depth)) %>%               # All to the same direction (negative)
  dplyr::filter(!is.na(z)) %>%
  glimpse()
  
summary(dat.maxn$depth)
unique(dat.maxn$scientific)
189*2 # Matches - sick

# Set predictor variables---
names(dat.maxn)

pred.vars = c("depth", # In situ depth plus missing ones from bathy
              "z", # Bathy depth 
              "mean.relief",
              "tpi",
              "roughness",
              "detrended",
              "slope") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
correlate(dat.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8)    

# Sand and inverts 100% correlated
# Sand and mean relief
# Slope and roughness
# Cut slope, sand

ggplot() + geom_point(data = dat.maxn, aes(x = depth, y = z))                   # Few random depth outliers

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x <- dat.maxn[ , i]
#   x = as.numeric(unlist(x)) 
#   hist((x)) #Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# } # Not running?

# Write data to load in to next script
saveRDS(dat.maxn, paste(paste0('data/tidy/', name), 
                        'gam-abundance.rds', sep = "_"))

#lengths
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

species <- as.data.frame(unique(length$scientific))

# Filter by fished species, manually adding in 'spp' fished species and removing species not counted as targeted
fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp",
                                                        "Lethrinidae Gymnocranius spp","Lethrinidae Lethrinus spp",
                                                        "Lethrinidae Unknown spp","Platycephalidae Platycephalus spp", 
                                                        "Lutjanidae Pristipomoides spp", "Lutjanidae Pristipomoides sp1",
                                                        "Lethrinidae Gymnocranius sp1")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius sp1"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae", 
                             "Carcharhinidae", "Sphyrnidae", "Pomacanthidae"))%>%    # Remove non-targeted families   
  dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
  glimpse()

fished.spp <- as.data.frame(unique(fished.species$scientific))

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) 

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

combined.length <- bind_rows(legal, sublegal) 

unique(combined.length$scientific)

dat.length <- combined.length %>%
  ungroup() %>%
  dplyr::right_join(metadata, by = c("campaignid","sample")) %>% # add in all samples
  dplyr::select(campaignid, sample,scientific,number, method) %>%
  tidyr::complete(nesting(campaignid, sample, method), scientific) %>%
  replace_na(list(number = 0)) %>% # we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y", "Yes", "yes")) %>%
  dplyr::mutate(scientific = as.character(scientific)) %>%
  dplyr::mutate(depth = ifelse(depth %in% "?", z, depth)) %>%                   # Samples didn't have in situ depth recorded
  dplyr::mutate(depth = as.numeric(depth)) %>%                                  # To numeric
  dplyr::mutate(depth = ifelse(depth > 0, depth * -1, depth)) %>%               # All to the same direction (negative)
  dplyr::filter(!is.na(z)) %>%
  dplyr::glimpse()

#write data to load in to next script
saveRDS(dat.length, paste(paste0('data/tidy/', name), 
                        'gam-length.rds', sep = "_"))
