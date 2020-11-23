metadata<-read.csv("data/ningaloo_metadata.csv")%>%
  mutate(sample=as.character(sample))%>%
  ga.clean.names()

maxn <- read.delim("data/example_Points.txt")%>%
  ga.clean.names()%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::group_by(sample,filename,period,periodtime,frame,family,genus,species,comment)%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::inner_join(metadata) %>%
  dplyr::filter(successful.count == "Yes") %>%
  dplyr::mutate(species = tolower(species)) %>%
  dplyr::mutate(genus = ga.capitalise(genus)) %>%
  dplyr::mutate(family = ga.capitalise(family))%>%
  glimpse()

dat<-maxn%>%
  dplyr::select(c(sample,family,genus,species,maxn))%>%
  tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0))%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()

### Read shapefiles###
marine.regions <- readOGR(dsn="data/spatial/marine_regions.shp")
marine.regions$REGION<-as.character(marine.regions$REGION)
commonwealth.marineparks <- readOGR(dsn="data/spatial/AustraliaNetworkMarineParks.shp")
wa.marineparks <- readOGR(dsn="data/spatial/WA_MPA_2018.shp")
proj4string(marine.regions)<-CRS(wgs.84)
proj4string(commonwealth.marineparks)<-CRS(wgs.84)
proj4string(wa.marineparks)<-CRS(wgs.84)

metadata.coords<-metadata
coordinates(metadata.coords) <- ~ longitude + latitude
proj4string(metadata.coords)<-CRS(wgs.84)

metadata.commonwealth.marineparks <- over(metadata.coords, commonwealth.marineparks)%>%
  dplyr::select(ZoneName)
metadata.wa.marineparks <- over(metadata.coords, wa.marineparks)%>%
  dplyr::select(ZONE_TYPE)

test<-bind_cols(metadata,metadata.commonwealth.marineparks)%>%
  dplyr::select(-c(status))%>%
  dplyr::rename(status=ZoneName)%>%
  bind_cols(.,metadata.wa.marineparks)%>%
  dplyr::mutate(status=ifelse(status%in%c(NA),as.character(ZONE_TYPE),as.character(status)))%>%
  dplyr::select(-c(ZONE_TYPE))%>%
  dplyr::mutate(status=str_replace_all(.$status,c(" Zone"="","Zone "="","(IUCN II)"="","(IUCN IV)"="","(IUCN IA)"="","[^[:alnum:] ]"=""," Benthic Protection"="","Use "="Use","y "="y")))%>%
  dplyr::select(sample,latitude,longitude,status)%>%
  dplyr::mutate(status=as.factor(status))%>%
  dplyr::mutate(status.simple=str_replace_all(.$status,c("General Use"="Fished",
                                                         "Recreational Use"="Fished",
                                                         "Multiple Use"="Fished",
                                                         "National Park"="No-take",
                                                         "Sanctuary"="No-take")))%>%
  glimpse()

unique(test$status)

test2$status <- fct_collapse(test$status,
                      fished=c("General Use","Recreational Use","Multiple Use"),
                      notake=c("National Park","Sanctuary"))

fct_count(test$status)
fct_count(test2)
## Set up container for results
n <- length(metadata.coords)
nearest.region <- character(n)
marine.regions$REGION<-as.character(marine.regions$REGION)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearest.region)) {
  nearest.region[i] <- marine.regions$REGION[which.min(gDistance(metadata.coords[i,], marine.regions, byid=TRUE))]
}

nearest.region

unique(marine.regions$REGION)

