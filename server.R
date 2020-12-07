function(input, output) {
  
  options(shiny.maxRequestSize=50*1024^2)
  
# Dropdown function -----
  create_dropdown <- function(input_name, choices, label) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]]
    } else {
      selected <- choices[1]
    }
    
    selectInput(
      inputId = input_name,
      label = label,
      choices = choices,
      selected = selected
    )
  }
# Read in metadata ----
metadata <- reactive({
    if(is.null(input$upload.metadata)){
      metadata<- read.csv("data/example_metadata.csv")%>%
        ga.clean.names()%>%
        dplyr::select(sample,latitude,longitude,date,time,site,location,status,depth,successful.count,successful.length)%>%
        as.data.frame()}
    else{
      metadata <-  read.csv(input$upload.metadata$datapath)%>%
        ga.clean.names()%>%
        dplyr::select(sample,latitude,longitude,date,time,site,location,status,depth,successful.count,successful.length)%>%
        as.data.frame()}
    
      metadata<-metadata
  })  

metadata.regions <- reactive({
  metadata<-metadata()
  
  coordinates(metadata) <- c('longitude', 'latitude')
  proj4string(metadata)<-CRS(wgs.84)

  n <- nrow(metadata())

  nearest.region <- character(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
  for (i in seq_along(nearest.region)) {
  nearest.region[i] <- marine.regions$REGION[which.min(gDistance(metadata[i,], marine.regions, byid=TRUE))]}

## Check that it worked
metadata.2<-as.data.frame(nearest.region)%>%
  bind_cols(metadata())%>%
  dplyr::rename(marine.region=nearest.region)%>%
  dplyr::mutate(sample=as.character(sample))

# add in commonwealth reserves
metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks)

metadata.3<-metadata.2%>%
  #dplyr::select(-c(status))%>%
  bind_cols(metadata.commonwealth.marineparks)%>%
  dplyr::rename(zone=ZoneName)

# add in state reserves
metadata.wa.marineparks <- over(metadata, wa.marineparks)%>%
  dplyr::select(ZONE_TYPE)%>%
  mutate(ZONE_TYPE=as.character(ZONE_TYPE))

metadata.regions<-metadata.3%>%
  bind_cols(metadata.wa.marineparks)%>%
  dplyr::mutate(zone=ifelse(zone%in%c(NA),as.character(ZONE_TYPE),as.character(zone)))%>%
  dplyr::select(-c(ZONE_TYPE))%>%
  dplyr::mutate(zone=str_replace_all(.$zone,c(" Zone"="","Zone "="","(IUCN II)"="","(IUCN IV)"="","(IUCN IA)"="","[^[:alnum:] ]"=""," Benthic Protection"="","Use "="Use","y "="y")))%>%
  dplyr::mutate(status=stringr::str_replace_all(.$zone,c("General Use"="Fished",
                                                         "Recreational Use"="Fished",
                                                         "Multiple Use"="Fished",
                                                         "National Park"="No-take",
                                                         "Sanctuary"="No-take",
                                                         "Special Purpose "="Fished"
                                                )))%>%
  dplyr::mutate(status = fct_recode(status, "No-take" = "No-take", 
                                            "No-take" = "NoTake", 
                                            "No-take" = "No Take", 
                                            "Fished" = "FISHED",
                                            "Fished" = "Outside",
                                            "Fished" = "Fishes", 
                                            "Fished" = "FALSE",
                                            "No-take" = "Reserve",
                                            "No-take" = "No take", 
                                            "No-take" = "Not Fished", 
                                            "No-take" = "No-Take",
                                            "Fished" = "Special Purpose"))
})

# Preview metadata ----
  output$table.metadata <- renderTable({
    # req(input$upload.metadata)
    metadata.regions()
  })

  
# Read in points data ----
points <- reactive({
  if(is.null(input$upload.points)){
    points<- read.delim("data/example_Points.txt", na.strings = "")%>%
      ga.clean.names()%>%
      dplyr::rename(sample=opcode)%>%
      mutate(sample=as.factor(sample))%>%
      mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
      as.data.frame()
    }
  else{
    points <- read.delim(input$upload.points$datapath, na.strings = "")%>%
      ga.clean.names()%>%
      dplyr::rename(sample=opcode)%>%
      mutate(sample=as.factor(sample))%>%
      mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
      as.data.frame()}
  
  points<-points
      # req(input$upload.points)
      # points <- read.delim(input$upload.points$datapath)%>%
      #   ga.clean.names()%>%
      #   dplyr::rename(sample=opcode)%>%
      #   mutate(sample=as.factor(sample))%>%
      #   as.data.frame()
    })
  
# Preview points ----
  output$table.points <- renderTable({
    # req(input$upload.points)
    points()
  })  
  
# Read in length data ----
# length <- reactive({
#     req(input$upload.length)
#     length <- read.delim(input$upload.length$datapath, na.strings = "")%>%
#       as.data.frame()%>%
#       ga.clean.names()%>%
#       mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
#       dplyr::rename(sample=opcode)%>%
#       dplyr::filter(number>0)%>%
#       dplyr::filter(!is.na(number))
#   })

length <- reactive({
  if(is.null(input$upload.length)){
    length<- read.delim("data/example_Lengths.txt", na.strings = "")%>%
      as.data.frame()%>%
      ga.clean.names()%>%
      mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
      dplyr::rename(sample=opcode)%>%
      dplyr::filter(number>0)%>%
      dplyr::filter(!is.na(number))%>%
      dplyr::filter(!is.na(family))
  }
  else{
    length <- read.delim(input$upload.length$datapath, na.strings = "")%>%
            as.data.frame()%>%
            ga.clean.names()%>%
            mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
            dplyr::rename(sample=opcode)%>%
            dplyr::filter(number>0)%>%
            dplyr::filter(!is.na(number))}
  
  length <- length
  
})

# Preview length ----
output$table.length <- renderTable({
  # req(input$upload.length)
  length()
})  
  
# Read in 3D points data ----
# threedpoints <- reactive({
#   req(input$upload.3dpoints)
#   threedpoints <- read.delim(input$upload.3dpoints$datapath, na.strings = "")%>%
#     as.data.frame()%>%
#     ga.clean.names()%>%
#     dplyr::mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
#     dplyr::filter(!comment%in%c("sync"))%>%
#     dplyr::rename(sample=opcode)%>%
#     dplyr::mutate(number=as.numeric(number))%>%
#     dplyr::filter(number>0)%>%
#     dplyr::filter(!is.na(number))
# })

threedpoints <- reactive({
  if(is.null(input$upload.3dpoints)){
    threedpoints<- read.delim("data/example_3DPoints.txt", na.strings = "")%>%
          as.data.frame()%>%
          ga.clean.names()%>%
          dplyr::mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
          dplyr::filter(!comment%in%c("sync"))%>%
          dplyr::rename(sample=opcode)%>%
          dplyr::mutate(number=as.numeric(number))%>%
          dplyr::filter(number>0)%>%
          dplyr::filter(!is.na(number))%>%
      dplyr::filter(!is.na(family))
  }
  else{
    threedpoints <- read.delim(input$upload.3dpoints$datapath, na.strings = "")%>%
      as.data.frame()%>%
      ga.clean.names()%>%
      dplyr::mutate(genus=ifelse(genus%in%c("NA","NANA",NA,"unknown"),"Unknown",as.character(genus)))%>%
      dplyr::filter(!comment%in%c("sync"))%>%
      dplyr::rename(sample=opcode)%>%
      dplyr::mutate(number=as.numeric(number))%>%
      dplyr::filter(number>0)%>%
      dplyr::filter(!is.na(number))}
  
  threedpoints<-threedpoints
})




  
# Preview 3D points ----
output$table.3dpoints <- renderTable({
  # req(input$upload.3dpoints)
  threedpoints()
})  

# METADATA - Number of samples - Valuebox ----
output$metadata.no.samples <- renderValueBox({
  
  metadata.samples<-metadata()%>%
    mutate(campaignid="metadata")%>%
    dplyr::group_by(campaignid)%>%
    dplyr::summarise(no.of.samples=n())%>%
    ungroup()%>%
    dplyr::select(no.of.samples)
  
    valueBox(width=3,metadata.samples$no.of.samples, "Metadata samples",
             icon = icon("list"), color = "green"
    )
  })

# METADATA - Empty point samples - Valuebox ----
output$metadata.samples.without.fish <- renderValueBox({
  
  metadata.samples<-metadata()%>%
    distinct(sample)%>%
    mutate(sample=as.factor(sample))
  
  points.samples<-points()%>%
    distinct(sample)
  
  missing.fish <- anti_join(metadata.samples,points.samples)%>%
    dplyr::mutate(test="missing fish")%>%
    distinct(test,sample)%>%
    dplyr::group_by(test)%>%
    dplyr::summarise(no.of.samples=n())
  
  if (dim(missing.fish)[1] > 0) {
    total<-missing.fish$no.of.samples
  }
  else{
    total=0
  }
  
  valueBox(width=3,
    total,
    "Sample(s) without points",
    icon = icon("question"), color = "yellow"
  )
})

# METADATA - Empty point samples - Dataframe ----
metadata.samples.without.fish <- reactive({
  # req(input$upload.metadata, input$upload.points)
  
  metadata.samples<-metadata()%>%
    distinct(sample)%>%
    mutate(sample=as.factor(sample))
  
  points.samples<-points()%>%
    distinct(sample)
  
  missing.fish <- anti_join(metadata.samples,points.samples)
})

# METADATA - Empty point samples - on click
onclick('click.metadata.samples.without.fish', showModal(modalDialog(
  title = "Samples without fish in points text file",
  renderDataTable(metadata.samples.without.fish(),rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))
))

# METADATA - Samples without metadata - Valuebox ----
output$points.samples.without.metadata <- renderValueBox({
  
  metadata.samples<-metadata()%>%
    distinct(sample)%>%
    mutate(sample=as.factor(sample))
  
  points.samples<-points()%>%
    distinct(sample)
  
  missing.metadata <- anti_join(points.samples,metadata.samples)%>%
    dplyr::mutate(test="missing metadata")%>%
    distinct(test,sample)%>%
    dplyr::group_by(test)%>%
    dplyr::summarise(no.of.samples=n())
  
  if (dim(missing.metadata)[1] > 0) {
    total<-missing.metadata$no.of.samples
  }
  else{
    total=0
  }
  
  valueBox(width=2,
    total,
    "Sample(s) in points file missing metadata",
    icon = icon("exclamation-circle"), color = "red"
  )
})

# METADATA - Samples without metadata - Dataframe ----
points.samples.without.metadata <- reactive({
  # req(input$upload.metadata, input$upload.points)
  metadata.samples<-metadata()%>%
    distinct(sample)%>%
    mutate(sample=as.factor(sample))
  
  points.samples<-points()%>%
    distinct(sample)
  
  missing.metadata <- anti_join(points.samples,metadata.samples)
})

# METADATA - Samples without metadata - onclick ----
onclick('click.points.samples.without.metadata', showModal(modalDialog(
  title = "Samples in points without metadata",
  renderDataTable(points.samples.without.metadata(),rownames=FALSE, options = list(paging = FALSE, searching = TRUE)))
))

# METADATA - Leaflet map ----
output$map.metadata <- renderLeaflet({
  
  metadata<-metadata.regions()%>%
    mutate(content=paste(sep = " ",
                     "<b>Sample:",sample,"</b>","<br/>",
                     "<b>Status:</b>",status,"<br/>",
                     "<b>Depth:</b>",depth,"m","<br/>",
                     "<b>Site:</b>",site,"<br/>",
                     "<b>Location:</b>",location,"<br/>",
                     "<b>Date:</b>",date,"<br/>",
                     "<b>Time:</b>",time,"<br/>"
    ))
  
  leaflet(data = metadata) %>%
    addTiles() %>%
    addMarkers(lng = ~longitude, lat = ~latitude, label = ~as.character(sample),popup = ~content)
})


# MAXN - Create maxn (Raw) -----
maxn.raw <- reactive({
maxn <- points() %>%
  dplyr::mutate(number=as.numeric(number)) %>%
  tidyr::replace_na(list(species="spp",genus="Unknown"))%>%
  dplyr::group_by(sample,filename,period,periodtime,frame,family,genus,species,comment) %>%
  dplyr::summarise(maxn=sum(number)) %>%
  dplyr::group_by(sample,family,genus,species) %>%
  dplyr::slice(which.max(maxn)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(maxn)) %>%
  dplyr::select(-frame) %>%
  tidyr::replace_na(list(maxn=0)) %>%
  dplyr::mutate(maxn=as.numeric(maxn)) %>%
  dplyr::filter(maxn>0) %>%
  dplyr::inner_join(metadata.regions()) %>%
  dplyr::filter(successful.count%in%c("Yes","Y","y","yes")) %>%
  dplyr::mutate(species = tolower(species)) %>%
  dplyr::mutate(genus = ga.capitalise(genus)) %>%
  dplyr::mutate(family = ga.capitalise(family))
    
})

# MAXN - Create maxn (Clean) -----
maxn.clean <- reactive({
maxn.clean <- dplyr::left_join(maxn.raw(),synonyms,by=c("family","genus","species"))%>%
    dplyr::mutate(genus=ifelse(!genus_correct%in%c(NA),genus_correct,genus))%>%
    dplyr::mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
    dplyr::mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
    dplyr::select(-c(family_correct,genus_correct,species_correct))%>%
    dplyr::group_by(sample,family,genus,species)%>%
    dplyr::slice(which.max(maxn))%>%
    dplyr::ungroup()
})

# MAXN - Create maxn (Complete) -----
maxn.complete <- reactive({

maxn.complete <- maxn.clean()%>%
  dplyr::select(c(sample,family,genus,species,maxn))%>%
  tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0))%>%
  dplyr::group_by(sample,family,genus,species)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()
})

# MAXN - create filtered maxn download -----
maxn.complete.download <- reactive({
  
  maxn <- maxn.raw()# can't use clean as have already changed synonyms

  
  if (input$error.synonyms==TRUE) {
    maxn.complete <- dplyr::left_join(maxn, synonyms,by=c("family","genus","species")) %>%
      dplyr::mutate(genus=ifelse(!genus_correct%in%c(NA),genus_correct,genus)) %>%
      dplyr::mutate(species=ifelse(!is.na(species_correct),species_correct,species)) %>%
      dplyr::mutate(family=ifelse(!is.na(family_correct),family_correct,family)) %>%
      dplyr::select(-c(family_correct,genus_correct,species_correct)) %>%
      dplyr::group_by(sample,family,genus,species) %>%
      dplyr::slice(which.max(maxn)) %>%
      dplyr::ungroup()%>%
      dplyr::select(c(sample,family,genus,species,maxn))%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(maxn = 0))%>%
      dplyr::group_by(sample,family,genus,species)%>%
      dplyr::summarise(maxn=sum(maxn))%>%
      ungroup()%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::mutate(campaignid = input$campaign.name) %>%
      dplyr::mutate(project = input$project.name)%>%
      dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
      dplyr::mutate(scientific = paste(genus,species,sep=" "))
  } 
  else{ 
    maxn.complete <- maxn%>%
      dplyr::select(c(sample,family,genus,species,maxn))%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(maxn = 0))%>%
      dplyr::group_by(sample,family,genus,species)%>%
      dplyr::summarise(maxn=sum(maxn))%>%
      ungroup()%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::mutate(campaignid = input$campaign.name) %>%
      dplyr::mutate(project = input$project.name)%>%
      dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
      dplyr::mutate(scientific = paste(genus,species,sep=" "))
  }
  
  maxn.complete <- maxn.complete
  
  species.out.of.area <- master.expanded%>%
    anti_join(maxn.clean(),.,by=c("family","genus","species","marine.region"))%>%
    distinct(family,genus,species,marine.region)%>%
    filter(!species%in%c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10","spp"))
  
  
  if (input$error.area==FALSE) {
    maxn.area <- maxn.complete
  } 
  else{ 
    maxn.area <- anti_join(maxn.complete,species.out.of.area)
  }
  
  maxn.area<-maxn.area%>%
    dplyr::mutate(campaignid = input$campaign.name) %>%
    dplyr::mutate(project = input$project.name) 
    
})

# MAXN - species dropdown ----
output$maxn.species.dropdown <- renderUI({
    df<-maxn.complete()
    
    options <- df %>%
      dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown","NANA"),as.character(family),as.character(genus)))%>%
      dplyr::group_by(family,genus,species)%>%
      dplyr::summarise(n=sum(maxn))%>%
      arrange(-n)%>%
      # left_join(master)%>%
      # dplyr::mutate(scientific=paste(genus," ",species," (",australian.common.name,")",sep=""))%>%
      dplyr::mutate(scientific=paste(genus," ",species,sep=""))%>%
      distinct(scientific) %>%
      pull("scientific")
    
    create_dropdown("maxn.species.dropdown", options, NULL)
})

# MAXN - taxa replaced by synonym - dataframe -----
maxn.synonym <- reactive({
  maxn<-maxn.raw()
  
  maxn.synonym<-dplyr::left_join(maxn,synonyms,by=c("family","genus","species"))%>% 
    dplyr::filter(!is.na(genus_correct))%>%
    dplyr::mutate('old name'=paste(family,genus,species,sep=" "))%>%
    dplyr::mutate('new name'=paste(family_correct,genus_correct,species_correct,sep=" "))%>%
    #dplyr::select(sample,family,genus,species,family_correct,genus_correct,species_correct)%>%
    dplyr::select('old name','new name')%>%
    dplyr::distinct()
})

# MAXN - taxa replaced by synonym - Valuebox ----
output$maxn.synonym <- renderValueBox({
  
  maxn.synonym<-maxn.synonym()%>%
    dplyr::rename(old.name='old name')%>%
    dplyr::mutate(count=1)
  
  if (dim(maxn.synonym)[1] > 0) {
    total<-sum(maxn.synonym$count)
  }
  else{
    total=0
  }
  
  valueBox(width=2,
           total,
           "Species names updated",
           icon = icon("exclamation-circle"), color = "yellow"
  )
})

# MAXN - taxa replaced by synonym - download ----
output$download.maxn.synonyms <- downloadHandler(
  filename = function() {
    paste("maxn.synonyms_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(maxn.synonym(), file, row.names = FALSE)
  }
)

# MAXN - taxa replaced by synonym - onclick ----
onclick('click.maxn.synonym', showModal(modalDialog(
  title = "Samples with species name updates", size="l",easyClose = TRUE,
  downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(maxn.synonym(), rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))


# MAXN - total abundance - dataframe ----
maxn.total.abundances <- reactive({
  maxn<-maxn.clean()%>%
    dplyr::group_by(sample)%>%
    dplyr::summarise(total.abundance=sum(maxn))
})

# MAXN - total abundance - value box ----
output$maxn.total.number <- renderValueBox({
  total<-sum(maxn.total.abundances()$total.abundance)
  valueBox(width=2,
           total,
           "Fish observed",
           icon = icon("fish"), color = "green"
  )
})

# MAXN - total abundance - onclick ----
onclick('click.maxn.total.number', showModal(modalDialog(
  title = "Total abundance per sample", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(maxn.total.abundances(),  
                  options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))

# MAXN - species not observed - dataframe ----
maxn.species.not.observed <- reactive({
  maxn<-master.expanded%>%
    anti_join(maxn.clean(),.,by=c("family","genus","species","marine.region"))%>%
    distinct(family,genus,species,marine.region)%>% # use this line to show specific drops OR
    dplyr::rename('marine region not observed in'=marine.region)%>%
    filter(!species%in%c("spp"))#%>% # Ignore spp in the report 
  
})

output$download.maxn.species.not.observed <- downloadHandler(
  filename = function() {
    paste("maxn.species.not.observed_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(maxn.species.not.observed(), file, row.names = FALSE)
  }
)
# MAXN - species not observed - onclick ----
onclick('click.maxn.species.not.observed', showModal(modalDialog(
  title = "Species not previously observed in the marine region", size="l",easyClose = TRUE,
  downloadButton("download.maxn.species.not.observed","Download as csv"),
  checkboxInput("maxn.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE),
  renderDataTable(
    if(input$maxn.filter.spp==TRUE)
      maxn.species.not.observed() %>% filter(!species%in%c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10"))
    else
      maxn.species.not.observed(),  rownames=FALSE,
                  options = list(paging = FALSE, row.names = FALSE, searching = TRUE)))))

# MAXN - species not observed - valuebox ----
output$maxn.species.not.observed <- renderValueBox({
  maxn.species.not.observed<-maxn.species.not.observed()%>%
    mutate(scientific=paste(family,genus,species,sep=" "))
  
  if (dim(maxn.species.not.observed)[1] > 0) {
     total<-base::length(unique(maxn.species.not.observed$scientific))
    }
  else{
    total=0
    }
    
  valueBox(width=2,
           total,
           "Species not observed in area before",
           icon = icon("map-marked"), color = "red"
  )
})

# MAXN - Spatial plot ----
output$maxn.spatial.plot <- renderLeaflet({
  
  req(input$maxn.species.dropdown)
  
  maxn<-maxn.complete()%>%
    left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific==input$maxn.species.dropdown)
  
  map <- leaflet(maxn) %>%
    addTiles()%>%
    fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  
  overzero <- filter(maxn, maxn > 0)
  equalzero <- filter(maxn, maxn == 0)
  
  if (nrow(overzero)) {
    map <- map %>%
      addCircleMarkers(
        data = overzero, lat = ~ latitude, lng = ~ longitude,
        radius = ~((maxn/max(maxn))*15), fillOpacity = 0.5, stroke = FALSE,
        label = ~as.character(maxn)
      )
  }
  if (nrow(equalzero)) {
    map <- map %>%
      addCircleMarkers(
        data = equalzero, lat = ~ latitude, lng = ~ longitude,
        radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
        label = ~as.character(maxn)
      )
  }
  map
})

# MAXN - Status plot ----
output$maxn.status.plot <- renderPlot({
  
  maxn.per.sample<-maxn.complete()%>%
    left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    dplyr::filter(scientific%in%c(input$maxn.species.dropdown))%>%
    group_by(sample,status)%>%
    summarise(maxn=sum(maxn))
  
  scientific.name<-input$maxn.species.dropdown
  
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(maxn.per.sample, aes(x = status,y=maxn, fill = status)) + 
    stat_summary(fun.y=mean, geom="bar",colour="black") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("Status")+
    ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#3c8dbc"))+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    annotation_custom(grob.sci)+ 
    Theme1
})

# MAXN - Zone plot ----
output$maxn.zone.simple <- renderPlot({
  maxn.per.sample.simple<-maxn.complete()%>%
    dplyr::left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    dplyr::filter(scientific%in%c(input$maxn.species.dropdown))%>%
    group_by(sample,zone)%>%
    summarise(maxn=sum(maxn))%>%
    ungroup()
  
  scientific.name<-input$maxn.species.dropdown
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(maxn.per.sample.simple, aes(x = zone,y=maxn, fill = zone)) + 
    stat_summary(fun.y=mean, geom="bar",colour="black") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("Status")+
    ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#3c8dbc"))+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    annotation_custom(grob.sci)+ 
    Theme1#+
  #ggtitle("Plot of abundance by Status")
})

# MAXN - Location plot ----
output$maxn.location.plot <- renderPlot({
  
  maxn.per.sample<-maxn.complete()%>%
    left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    dplyr::filter(scientific%in%c(input$maxn.species.dropdown))%>%
    group_by(sample,location)%>%
    summarise(maxn=sum(maxn))
  
  scientific.name<-input$maxn.species.dropdown
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(maxn.per.sample, aes(x = location,y=maxn)) + 
    stat_summary(fun.y=mean, geom="bar",colour="black",fill="white") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("Location")+
    ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    annotation_custom(grob.sci)+ 
    Theme1#+
    #ggtitle("Plot of abundance by Location")
})

# MAXN - Site plot ----
output$maxn.site.plot <- renderPlot({
  
  maxn.per.sample<-maxn.complete()%>%
    left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    dplyr::filter(scientific%in%c(input$maxn.species.dropdown))%>%
    group_by(sample,site)%>%
    summarise(maxn=sum(maxn))
  
  
  scientific.name<-input$maxn.species.dropdown
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(maxn.per.sample, aes(x = site,y=maxn)) + 
    stat_summary(fun.y=mean, geom="bar",colour="black",fill="white") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("Site")+
    ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    annotation_custom(grob.sci)+ 
    Theme1#+
    #ggtitle("Plot of abundance by Location")
})

# MAXN top species ----
output$maxn.top.species <- renderPlot({
maxn.sum<-maxn.complete()%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(input$species.limit)

## Total frequency of occurance
ggplot(maxn.sum, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  Theme1+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# MAXN - download -----
# output$download.maxn.complete <- downloadHandler(
#   filename = function() {
#     paste("maxn.complete_", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(maxn.complete(), file, row.names = FALSE)
#   }
# )
# 
# output$download.maxn.clean <- downloadHandler(
#   filename = function() {
#     paste("maxn.clean_", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(maxn.clean(), file, row.names = FALSE)
#   }
# )

# LENGTH - combine lengths and 3d points ----
length3dpoints <- reactive({
  length3dpoints<-length()%>%
    plyr::rbind.fill(threedpoints())%>%
    dplyr::mutate(length=as.numeric(length))%>%
    dplyr::mutate(number=as.numeric(number))%>%
    dplyr::filter(!is.na(number))%>%
    tidyr::replace_na(list(species="spp"))%>%
    dplyr::select(-c(time))%>%
    dplyr::mutate(sample=as.character(sample))%>%
    dplyr::left_join(metadata.regions())
})

# LENGTH - create filtered length download -----
length.complete.download <- reactive({
  
  length <- length3dpoints() # can't use clean as have already changed synonyms
  
  if (input$error.synonyms==TRUE) {
    length.complete <- dplyr::left_join(length3dpoints(),synonyms,by=c("family","genus","species"))%>%
      dplyr::mutate(genus=ifelse(!genus_correct%in%c(NA),genus_correct,genus))%>%
      dplyr::mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
      dplyr::mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
      dplyr::select(-c(family_correct,genus_correct,species_correct))%>%
      dplyr::right_join(metadata.regions())%>% # add in all samples
      dplyr::select(sample,family,genus,species,length,number,range,frameleft,frameright)%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup()%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::mutate(marine.region=as.character(marine.region))%>%
      dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))
  } 
  else{ 
    length.complete <- dplyr::left_join(length3dpoints(),synonyms,by=c("family","genus","species"))%>%
      dplyr::right_join(metadata.regions())%>% # add in all samples
      dplyr::select(sample,family,genus,species,length,number,range,frameleft,frameright)%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup()%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))%>%
      dplyr::mutate(marine.region=as.character(marine.region))
  }
  
  length.complete <- length.complete%>%
    dplyr::mutate(campaignid = input$campaign.name) %>%
    dplyr::mutate(project = input$project.name)%>%
    dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
    dplyr::mutate(scientific = paste(genus,species,sep=" "))
  
  species.out.of.area <- master.expanded%>%
    dplyr::mutate(marine.region=as.character(marine.region))%>%
    anti_join(length.complete,.,by=c("family","genus","species","marine.region"))%>%
    distinct(family,genus,species,marine.region)%>%
    filter(!species%in%c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10","spp"))
  
  
  if (input$error.area==FALSE) {
    length.area <- length.complete
  } 
  else{ 
    length.area <- anti_join(length.complete, species.out.of.area)
  }
  
  length.area<-length.area%>%
    dplyr::filter(range<(input$error.range.limit*1000))
  
  length.wrong<-left_join(length.area,master.min.max,by=c("family","genus","species"))%>%
    dplyr::filter(length<min.length|length>fb.length_max)%>%
    mutate(reason=ifelse(length<min.length,"too small","too big"))

  length.too.small<-length.wrong%>%
    dplyr::filter(reason%in%c("too small"))

  length.too.big<-length.wrong%>%
    dplyr::filter(reason%in%c("too big"))

  if (input$error.length.small==TRUE) {
    length.small <- anti_join(length.area,length.too.small)
  }
  else{
    length.small <- length.area
  }

  length.small<-length.small

  if (input$error.length.big==TRUE) {
    length.big <- anti_join(length.small,length.too.big)
  }
  else{
    length.big <- length.small
  }
  length.big<-length.big%>%
    dplyr::right_join(metadata.regions())%>% # add in all samples
    dplyr::select(sample,family,genus,species,length,number,range,frameleft,frameright)%>%
    tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
    replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
    dplyr::mutate(length=as.numeric(length))%>%
    dplyr::left_join(metadata.regions())%>%
    filter(!is.na(family))%>%
    dplyr::mutate(campaignid = input$campaign.name) %>%
    dplyr::mutate(project = input$project.name)%>%
    dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
    dplyr::mutate(scientific = paste(genus,species,sep=" "))
  
})











length3dpoints.clean <- reactive({
  length3dpoints.clean<- dplyr::left_join(length3dpoints(),synonyms,by=c("family","genus","species"))%>%
  dplyr::mutate(genus=ifelse(!genus_correct%in%c(NA),genus_correct,genus))%>%
  dplyr::mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  dplyr::mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  dplyr::select(-c(family_correct,genus_correct,species_correct))%>%
  dplyr::filter(range<(input$range.limit*1000))%>%
  dplyr::right_join(metadata.regions())%>% # add in all samples
  dplyr::select(sample,family,genus,species,length,number,range,frameleft,frameright)%>%
  tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
  replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::left_join(metadata.regions())%>%
  dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))
  
})

# LENGTH - species dropdown ----
output$length.species.dropdown <- renderUI({
  df<-length3dpoints.clean()
  
  options <- df %>%
    dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown"),as.character(family),as.character(genus)))%>%
    dplyr::group_by(family,genus,species)%>%
    dplyr::summarise(n=sum(number))%>%
    dplyr::arrange(-n)%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    distinct(scientific) %>%
    pull("scientific")
  
  create_dropdown("length.species.dropdown", options, NULL)
})

# LENGTH - number of lengths - dataframe ----
length.abundance <- reactive({
  length<-length3dpoints.clean()%>%
    dplyr::filter(!length%in%c(NA))%>%
    dplyr::group_by(sample)%>%
    dplyr::summarise(total.abundance=sum(number))
})

# LENGTH - number of lengths - value box ----
output$length.abundance <- renderValueBox({
  total<-sum(length.abundance()$total.abundance)
  valueBox(width=3,
           total,
           "Length measurements",
           icon = icon("ruler"), color = "green"
  )
})

# LENGTH - number of lengths - onclick ----
onclick('click.length.abundance', showModal(modalDialog(
  title = "Number of fish measured per sample", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(length.abundance(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# 3D points - number of 3d points - dataframe ----
threedpoints.abundance <- reactive({
  threedpoints.abundance<-length3dpoints.clean()%>%
    dplyr::filter(length%in%c(NA))%>%
    dplyr::group_by(sample)%>%
    dplyr::summarise(total.abundance=sum(number))%>%
    dplyr::ungroup()%>%
    tidyr::replace_na(list(total.abundance=0))
})

# 3D points - number of 3d points - value box ----
output$threedpoints.abundance <- renderValueBox({
  threedpoints.abundance<-threedpoints.abundance()
  
  if (dim(threedpoints.abundance)[1] > 0) {
    total<-sum(threedpoints.abundance$total.abundance)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "3D points",
           icon = icon("dot-circle"), color = "green"
  )
})

# 3D points - number of lengths - onclick ----
onclick('click.threedpoints.abundance', showModal(modalDialog(
  title = "Number of fish with 3D points per sample", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(threedpoints.abundance(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - taxa replaced by synonym - dataframe -----
length.synonym <- reactive({
  length<-length3dpoints()
  
  length.synonym<-dplyr::left_join(length,synonyms,by=c("family","genus","species"))%>% 
    dplyr::filter(!is.na(genus_correct))%>%
    dplyr::mutate('old name'=paste(family,genus,species,sep=" "))%>%
    dplyr::mutate('new name'=paste(family_correct,genus_correct,species_correct,sep=" "))%>%
    #dplyr::select(sample,family,genus,species,family_correct,genus_correct,species_correct)%>%
    dplyr::select('old name','new name')%>%
    dplyr::distinct()
})

# LENGTH - taxa replaced by synonym - Valuebox ----
output$length.synonym <- renderValueBox({
  
  length.synonym<-length.synonym()%>%
    dplyr::rename(old.name='old name')%>%
    dplyr::mutate(count=1)
  
  if (dim(length.synonym)[1] > 0) {
    total<-sum(length.synonym$count)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Species names updated",
           icon = icon("exclamation-circle"), color = "yellow"
  )
})

# LENGTH - taxa replaced by synonym - download ----
output$download.length.synonyms <- downloadHandler(
  filename = function() {
    paste("length.synonyms", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(length.synonym(), file, row.names = FALSE)
  }
)

# LENGTH - taxa replaced by synonym - onclick ----
onclick('click.length.synonym', showModal(modalDialog(
  title = "Samples with species name updates", size="l",easyClose = TRUE,
  downloadButton("download.length.synonyms","Download as csv"),
  renderDataTable(length.synonym(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - species not observed - dataframe ----
length.species.not.observed <- reactive({
  length<-master.expanded%>%
    anti_join(length3dpoints.clean(),.,by=c("family","genus","species","marine.region"))%>%
    distinct(family,genus,species,marine.region)%>% # use this line to show specific drops OR
    dplyr::rename('marine region not observed in'=marine.region)%>%
    filter(!species%in%c("spp"))#%>% # Ignore spp in the report 
  
})

# LENGTH - species not observed - onclick ----
onclick('click.length.species.not.observed', showModal(modalDialog(
  title = "Species not previously observed in the marine region", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  checkboxInput("length.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE),
  renderDataTable(
    if(input$length.filter.spp==TRUE)
    length.species.not.observed() %>% filter(!species%in%c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10"))
    else
      length.species.not.observed(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - species not observed - valuebox ----
output$length.species.not.observed <- renderValueBox({
  length.species.not.observed<-length.species.not.observed()%>%
    mutate(scientific=paste(family,genus,species,sep=" "))
  
  if (dim(length.species.not.observed)[1] > 0) {
    total<-base::length(unique(length.species.not.observed$scientific))
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Species not observed in area before",
           icon = icon("map-marked"), color = "red"
  )
})

# LENGTH - species wrong length - dataframe ----
length.wrong <- reactive({
  length.wrong<-left_join(length3dpoints.clean(),master.min.max,by=c("family","genus","species"))%>%
    dplyr::filter(length<min.length|length>max.length)%>%
    mutate(reason=ifelse(length<min.length,"too small","too big"))%>%
    dplyr::select(sample,family,genus,species,length,min.length,max.length,fb.length_max,reason)%>%
    mutate(difference=ifelse(reason%in%c("too small"),(min.length-length),(length-max.length)))%>%
    dplyr::mutate(percent.of.fb.max=(length/fb.length_max*100))
  
})

# LENGTH - species wrong length small - valuebox ----
output$length.wrong.small <- renderValueBox({
  length.wrong.small<-length.wrong()%>%
    dplyr::filter(reason%in%c("too small"))%>%
    dplyr::mutate(count=1)
  
  if (dim(length.wrong.small)[1] > 0) {
    total<-sum(length.wrong.small$count)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Lengths smaller than 15% of max",
           icon = icon("less-than"), color = "red"
  )
})

# LENGTH - species wrong length small - download ----
output$download.length.wrong.small <- downloadHandler(
  filename = function() {
    paste("length.15.percent.of.max", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dplyr::filter(length.wrong(), reason%in%c("too small")), file, row.names = FALSE)
  }
)

# LENGTH - species wrong length small - onclick ----
onclick('click.length.wrong.small', showModal(modalDialog(
  title = "Length measurement smaller than 15% of the fishbase maximum", size="l",easyClose = TRUE,
  downloadButton("download.length.wrong.small","Download as csv"),
  renderDataTable(filter(length.wrong(),reason=="too small"), rownames=FALSE, 
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - species wrong length big - valuebox ----
output$length.wrong.big <- renderValueBox({
  length.wrong.big<-length.wrong()%>%
    dplyr::filter(reason%in%c("too big"))%>%
    dplyr::mutate(count=1)
  
  if (dim(length.wrong.big)[1] > 0) {
    total<-sum(length.wrong.big$count)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Lengths bigger than 85% of max",
           icon = icon("greater-than"), color = "red"
  )
})

# LENGTH - species wrong length big - download ----
output$download.length.wrong.big <- downloadHandler(
  filename = function() {
    paste("length.85.percent.of.max", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dplyr::filter(length.wrong(), reason%in%c("too big")), file, row.names = FALSE)
  }
)

# LENGTH - species wrong length big - onclick ----
onclick('click.length.wrong.big', showModal(modalDialog(
  title = "Length measurement bigger than 85% of the fishbase maximum", size="l",easyClose = TRUE,
  downloadButton("download.length.wrong.big","Download as csv"),
  renderDataTable(filter(length.wrong(),reason=="too big"), rownames=FALSE, 
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - out of range - dataframe ----
length.out.of.range <- reactive({
  req(input$range.limit)
  
  range.limit<-(input$range.limit*1000)
  
  length.out.of.range<-length3dpoints()%>%
    dplyr::filter(range>range.limit)%>%
    dplyr::select(sample,family,genus,species,range,frameleft,frameright)
})

output$length.out.of.range <- renderValueBox({
  length.out.of.range<-length.out.of.range()%>%
    dplyr::mutate(count=1)
  
  if (dim(length.out.of.range)[1] > 0) {
    total<-sum(length.out.of.range$count)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Out of range",
           icon = icon("greater-than"), color = "red"
  )
})

onclick('click.length.out.of.range', showModal(modalDialog(
  title = "Length measurement out of range", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(length.out.of.range(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH - histogram ----
output$length.histogram <- renderPlot({
  req(input$length.species.dropdown)
  
  length3dpoints<-length3dpoints.clean()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))%>%
    replace_na(list(status="Fished"))
  
  sizes<-master.min.max%>%
    mutate(scientific=paste(genus,species,sep =" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))%>%
    distinct(scientific,fb.length_max,min.length,max.length)
  
  fishbase.max<-sum(sizes$fb.length_max)
  min.15<-sum(sizes$min.length)
  max.85<-sum(sizes$max.length)
  
  scientific.name<-input$length.species.dropdown
  #common.name<-unique(maxn_species_data()$australian.common.name)
  
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(length3dpoints,aes(x = length), col = "black",alpha=0.5)+
    geom_histogram(alpha=0.5, position="identity",binwidth=input$length.binwidth,col="black")+
    xlab("Length (mm)") + ylab("Count") +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_vline(xintercept=fishbase.max, colour="red",size=1) +
    geom_vline(xintercept=min.15, colour="grey",size=1) +
    geom_vline(xintercept=max.85, colour="grey",size=1) +
    geom_text(aes(x=fishbase.max, label="\n  Fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    geom_text(aes(x=min.15, label="\n  15% of fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    geom_text(aes(x=max.85, label="\n  85% of fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#3c8dbc"))+
    annotation_custom(grob.sci)+ 
    Theme1
})

# LENGTH - histogram status ----
output$length.histogram.status <- renderPlot({
  req(input$length.species.dropdown)
  
  length3dpoints<-length3dpoints.clean()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))%>%
    replace_na(list(status="Fished"))
  
  sizes<-master.min.max%>%
    mutate(scientific=paste(genus,species,sep =" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))%>%
    distinct(scientific,fb.length_max,min.length,max.length)
  
  fishbase.max<-sum(sizes$fb.length_max)
  min.15<-sum(sizes$min.length)
  max.85<-sum(sizes$max.length)
  
  scientific.name<-input$length.species.dropdown
  #common.name<-unique(maxn_species_data()$australian.common.name)
  
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(length3dpoints,aes(x = length), col = "black",alpha=0.5)+
    geom_histogram(alpha=0.5, position="identity",binwidth=input$length.binwidth,col="black")+
    xlab("Length (mm)") + ylab("Count") +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_vline(xintercept=fishbase.max, colour="red",size=1) +
    geom_vline(xintercept=min.15, colour="grey",size=1) +
    geom_vline(xintercept=max.85, colour="grey",size=1) +
    geom_text(aes(x=fishbase.max, label="\n  Fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    geom_text(aes(x=min.15, label="\n  15% of fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    geom_text(aes(x=max.85, label="\n  85% of fishbase maximum", y=0), colour="black", angle=90, text=element_text(size=14),hjust = 0,vjust=0)+
    annotation_custom(grob.sci)+ 
    Theme1+ 
    facet_wrap(vars(status), ncol = 1)
    
})

# LENGTH - species plot - zone ----
output$length.status.plot <- renderPlot({
  req(input$length.species.dropdown)
  length3dpoints<-length3dpoints.clean()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))
  
  scientific.name<-input$length.species.dropdown
  #common.name<-unique(maxn_species_data()$australian.common.name)
  
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))

ggplot(length3dpoints,aes(x = factor(zone), y = length,  fill = zone, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
  stat_boxplot(geom='errorbar')+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
  xlab("Zone") + ylab("Length (mm)") +
  annotation_custom(grob.sci)+ 
  Theme1
})

# LENGTH - species plot - status ----
output$length.zone.plot <- renderPlot({
  req(input$length.species.dropdown)
  length3dpoints<-length3dpoints.clean()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific%in%c(input$length.species.dropdown))%>%
    replace_na(list(status="Fished"))
  
  scientific.name<-input$length.species.dropdown
  
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(length3dpoints,aes(x = factor(status), y = length,  fill = status, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
    xlab("Status") + ylab("Length (mm)") +
    annotation_custom(grob.sci)+ 
    Theme1
})

# MASS - create mass dataframe ----
mass <- reactive({
  # 1. Check for missing length weight relationship
taxa.missing.lw <- length3dpoints.clean()%>%
  dplyr::distinct(family,genus,species)%>%
  dplyr::anti_join(filter(master,!is.na(a)), by=c("family","genus","species"))

#2. Fill length data with relevant a and b and if blank use family---
length.species.ab<-master%>% # done this way around to avoid duplicating Family coloum
  dplyr::select(-family)%>%
  dplyr::inner_join(length3dpoints.clean(),., by=c("genus","species")) # only keeps row if has a and b

# 3. Make family length.weight
family.lw <- master%>%
  dplyr::group_by(family,length.measure)%>%
  dplyr::mutate(log.a = log10(a))%>%     
  dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                   b = mean(b, na.rm = T),
                   all = mean(all, na.rm = T),
                   bll = mean(bll, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(a))%>%
  dplyr::mutate(all=str_replace_all(all,"NaN","0"))%>%
  dplyr::mutate(bll=str_replace_all(bll,"NaN","1"))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(rank = ifelse(length.measure=="FL",1,ifelse(length.measure=="TL", 2, 3)))%>%
  dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE))%>%
  dplyr::filter(min.rank == 0)

length.family.ab<-length3dpoints.clean()%>%
  dplyr::anti_join(master, by=c("genus","species"))%>%
  dplyr::left_join(family.lw, by="family")

# 5. Fill length data with relevant a and b and if blank use family---
complete.length.number.mass<-length.species.ab%>%
  bind_rows(length.family.ab)%>%
  dplyr::filter(!is.na(a))%>% #this gets rid of species with no lw
  dplyr::mutate(length.cm = length/10)%>%
  dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL","SL"), 0, all))%>% # Temporary fix, remove later
  dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL","SL"), 1, bll))%>% # Temporary fix, remove later
  dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>% 
  dplyr::mutate(mass.g = (adjLength^b)*a*number)%>%
  dplyr::filter(mass.g>0)%>%
  dplyr::full_join(metadata.regions())%>%
  dplyr::select(c(sample,family,genus,species,length,range,number,mass.g,length.cm))%>%
  tidyr::complete(sample,nesting(family,genus,species)) %>%
  tidyr::replace_na(list(mass.g=0))%>%
  dplyr::mutate(mass.kg=mass.g/1000)%>%
  dplyr::left_join(metadata.regions())
})


# MASS - create filtered MASS download -----
mass.complete.download <- reactive({
  
  length3dpoints<- dplyr::left_join(length3dpoints(),synonyms,by=c("family","genus","species"))%>%
    dplyr::select(-c(family_correct,genus_correct,species_correct))%>%
    dplyr::right_join(metadata.regions())%>% # add in all samples
    dplyr::select(sample,family,genus,species,length,number,range)%>%
    tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
    replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
    dplyr::ungroup()%>%
    dplyr::mutate(length=as.numeric(length))%>%
    dplyr::left_join(metadata.regions())%>%
    dplyr::mutate(marine.region=as.character(marine.region))%>%
    dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))
  
  # 1. Check for missing length weight relationship
  taxa.missing.lw <- length3dpoints%>%
    dplyr::distinct(family,genus,species)%>%
    dplyr::anti_join(filter(master,!is.na(a)), by=c("family","genus","species"))
  
  #2. Fill length data with relevant a and b and if blank use family---
  length.species.ab<-master%>% # done this way around to avoid duplicating Family coloum
    dplyr::select(-family)%>%
    dplyr::inner_join(length3dpoints,., by=c("genus","species")) # only keeps row if has a and b
  
  # 3. Make family length.weight
  family.lw <- master%>%
    dplyr::group_by(family,length.measure)%>%
    dplyr::mutate(log.a = log10(a))%>%     
    dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                     b = mean(b, na.rm = T),
                     all = mean(all, na.rm = T),
                     bll = mean(bll, na.rm = T))%>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(a))%>%
    dplyr::mutate(all=str_replace_all(all,"NaN","0"))%>%
    dplyr::mutate(bll=str_replace_all(bll,"NaN","1"))%>%
    dplyr::mutate(all=as.numeric(all))%>%
    dplyr::mutate(bll=as.numeric(bll))%>%
    dplyr::mutate(rank = ifelse(length.measure=="FL",1,ifelse(length.measure=="TL", 2, 3)))%>%
    dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE))%>%
    dplyr::filter(min.rank == 0)
  
  length.family.ab<-length3dpoints%>%
    dplyr::anti_join(master, by=c("genus","species"))%>%
    dplyr::left_join(family.lw, by="family")
  
  # 5. Fill length data with relevant a and b and if blank use family---
  complete.length.number.mass<-length.species.ab%>%
    bind_rows(length.family.ab)%>%
    dplyr::filter(!is.na(a))%>% #this gets rid of species with no lw
    dplyr::mutate(length.cm = length/10)%>%
    dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL","SL"), 0, all))%>% # Temporary fix, remove later
    dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL","SL"), 1, bll))%>% # Temporary fix, remove later
    dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>% 
    dplyr::mutate(mass.g = (adjLength^b)*a*number)%>%
    dplyr::filter(mass.g>0)%>%
    dplyr::full_join(metadata.regions())%>%
    dplyr::select(c(sample,family,genus,species,length,range,number,mass.g,length.cm))%>%
    tidyr::complete(sample,nesting(family,genus,species)) %>%
    tidyr::replace_na(list(mass.g=0))%>%
    dplyr::mutate(mass.kg=mass.g/1000)
  
  if (input$error.synonyms==TRUE) {
    complete.length.number.mass <- dplyr::left_join(complete.length.number.mass,synonyms,by=c("family","genus","species"))%>%
      dplyr::mutate(genus=ifelse(!genus_correct%in%c(NA),genus_correct,genus))%>%
      dplyr::mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
      dplyr::mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
      dplyr::select(-c(family_correct,genus_correct,species_correct))%>%
      dplyr::right_join(metadata.regions())%>% # add in all samples
      dplyr::select(sample,family,genus,species,length,number,range,mass.kg)%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup()%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::mutate(marine.region=as.character(marine.region))%>%
      dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))
  } 
  else{ 
    complete.length.number.mass <- dplyr::left_join(complete.length.number.mass,synonyms,by=c("family","genus","species"))%>%
      dplyr::right_join(metadata.regions())%>% # add in all samples
      dplyr::select(sample,family,genus,species,length,number,range,mass.kg)%>%
      tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
      replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup()%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::left_join(metadata.regions())%>%
      dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))%>%
      dplyr::mutate(marine.region=as.character(marine.region))%>%
      dplyr::mutate(campaignid = input$campaign.name) %>%
      dplyr::mutate(project = input$project.name)%>%
      dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
      dplyr::mutate(scientific = paste(genus,species,sep=" "))
  }
  
  complete.length.number.mass <- complete.length.number.mass
  
  species.out.of.area <- master.expanded%>%
    dplyr::mutate(marine.region=as.character(marine.region))%>%
    anti_join(complete.length.number.mass,.,by=c("family","genus","species","marine.region"))%>%
    distinct(family,genus,species,marine.region)%>%
    filter(!species%in%c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10","spp"))
  
  
  if (input$error.area==FALSE) {
    mass.area <- complete.length.number.mass
  } 
  else{ 
    mass.area <- anti_join(complete.length.number.mass, species.out.of.area)
  }
  
  mass.area<-mass.area%>%
    dplyr::filter(range<(input$error.range.limit*1000))
  
  length.wrong<-left_join(mass.area,master.min.max,by=c("family","genus","species"))%>%
    dplyr::filter(length<min.length|length>fb.length_max)%>%
    mutate(reason=ifelse(length<min.length,"too small","too big"))
  
  length.too.small<-length.wrong%>%
    dplyr::filter(reason%in%c("too small"))
  
  length.too.big<-length.wrong%>%
    dplyr::filter(reason%in%c("too big"))
  
  if (input$error.length.small==TRUE) {
    mass.small <- anti_join(mass.area,length.too.small)
  }
  else{
    mass.small <- mass.area
  }
  
  mass.small<-mass.small
  
  if (input$error.length.big==TRUE) {
    mass.big <- anti_join(mass.small,length.too.big)
  }
  else{
    mass.big <- mass.small
  }
  mass.big<-mass.big%>%
    dplyr::right_join(metadata.regions())%>% # add in all samples
    dplyr::select(sample,family,genus,species,length,number,range,mass.kg)%>%
    tidyr::complete(nesting(sample),nesting(family,genus,species)) %>%
    replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
    dplyr::left_join(metadata.regions())%>%
    filter(!is.na(family))%>%
    dplyr::mutate(campaignid = input$campaign.name) %>%
    dplyr::mutate(project = input$project.name)%>%
    dplyr::mutate(id=paste(project,campaignid,sep="."))%>%
    dplyr::mutate(scientific = paste(genus,species,sep=" "))
  
})










# MASS - species dropdown ----
output$mass.species.dropdown <- renderUI({
  df<-mass()
  
  options <- df %>%
    dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown"),as.character(family),as.character(genus)))%>%
    dplyr::group_by(family,genus,species)%>%
    dplyr::summarise(sum.mass=sum(mass.g))%>%
    dplyr::arrange(-sum.mass)%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    distinct(scientific) %>%
    pull("scientific")
  
  create_dropdown("mass.species.dropdown", options, NULL)
})



# MASS - top species ----
output$mass.top.species <- renderPlot({
  mass.sum<-mass()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    left_join(classes)%>%
    dplyr::group_by(class,scientific)%>%
    dplyr::summarise(sum.mass.g=sum(mass.g))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(sum.mass.kg=sum.mass.g/1000)
  
  if (input$mass.include.sharks=="yes"){
    mass.sum <- mass.sum%>%
      top_n(input$mass.species.limit)
  } else {
    mass.sum <- mass.sum%>%
      dplyr::filter(!class%in%c("Elasmobranchii"))%>%
      top_n(input$mass.species.limit)
  }

  ## Total frequency of occurance
  ggplot(mass.sum, aes(x=reorder(scientific,sum.mass.kg), y=sum.mass.kg)) +   
    geom_bar(stat="identity",position=position_dodge())+
    coord_flip()+
    xlab("Species")+
    ylab(expression(Total~mass~(Sigma~Kg)))+
    Theme1+
    theme(axis.text.y = element_text(face="italic"))+
    theme_collapse+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
})

# MASS - species plot - zone ----
output$mass.status.plot <- renderPlot({
  req(input$mass.species.dropdown)
  mass<-mass()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    dplyr::filter(scientific%in%c(input$mass.species.dropdown))
  
  scientific.name<-input$mass.species.dropdown
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(mass,aes(x = factor(status), y = mass.g,  fill = status, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
    xlab("Status") + ylab("Mass (g)") +
    annotation_custom(grob.sci)+ 
    Theme1
})

# MASS - species plot - Status ----
output$mass.zone.plot <- renderPlot({
  req(input$mass.species.dropdown)
  mass<-mass()%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific%in%c(input$mass.species.dropdown))
  
  scientific.name<-input$mass.species.dropdown
  grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                gp=gpar(col="black", fontsize=13, fontface="italic")))
  
  ggplot(mass,aes(x = factor(zone), y = mass.g,  fill = zone, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
    xlab("Zone") + ylab("Mass (g)") +
    annotation_custom(grob.sci)+ 
    Theme1
})

# MASS - Spatial plot ----
output$mass.spatial.plot <- renderLeaflet({
  
  req(input$mass.species.dropdown)
  
  mass<-mass()%>%
    left_join(metadata.regions())%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific==input$mass.species.dropdown)
  
  map <- leaflet(mass) %>%
    addTiles()%>%
    fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  
  overzero <- filter(mass, mass.g > 0)
  equalzero <- filter(mass, mass.g == 0)
  
  if (nrow(overzero)) {
    map <- map %>%
      addCircleMarkers(
        data = overzero, lat = ~ latitude, lng = ~ longitude,
        radius = ~((mass.g/max(mass.g))*15), fillOpacity = 0.5, stroke = FALSE,
        label = ~as.character(mass.g)
      )
  }
  if (nrow(equalzero)) {
    map <- map %>%
      addCircleMarkers(
        data = equalzero, lat = ~ latitude, lng = ~ longitude,
        radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
        label = ~as.character(mass.g)
      )
  }
  map
})
# Length vs maxn use 2.2 as check ----
length.vs.maxn <- reactive({
length.sample <- metadata.regions()%>%
  dplyr::filter(successful.length%in%c("Yes","Y","y","yes"))%>%
  distinct(sample)

length<-length3dpoints.clean()
maxn<-maxn.complete()

# summarise length and then compare to maxn
length.vs.maxn<-length3dpoints.clean()%>%
  dplyr::group_by(sample,family,genus,species)%>%
  dplyr::summarise(length.maxn=sum(number))%>%
  dplyr::ungroup()%>%
  dplyr::full_join(maxn.complete())%>%
  replace_na(list(maxn=0))%>%
  dplyr::filter(!length.maxn==maxn)%>%
  dplyr::mutate(percent.difference = (maxn-length.maxn)/maxn*100)%>%
  dplyr::semi_join(length.sample)%>% # only keep ones where length was possible
  replace_na(list(percent.difference=1))%>%
  dplyr::filter(!percent.difference%in%c(0))%>% #only for those that have missing lengths
  dplyr::mutate(difference=(maxn-length.maxn))%>%
  dplyr::mutate(difference=abs(difference))%>%
  dplyr::mutate(percent.difference=abs(percent.difference))%>%
  dplyr::select(sample,family,genus,species,maxn,length.maxn,difference,percent.difference)%>%
  arrange(-difference)
})

# LENGTH VS MAXN - valuebox ----
output$length.vs.maxn <- renderValueBox({
  length.vs.maxn<-length.vs.maxn()%>%
    dplyr::mutate(count=1)
  
  if (dim(length.vs.maxn)[1] > 0) {
    total<-sum(length.vs.maxn$count)
  }
  else{
    total=0
  }
  
  valueBox(width=3,
           total,
           "Number of lengths/3D points does not match MaxN",
           icon = icon("not-equal"), color = "red"
  )
})

# LENGTH VS MAXN - onclick ----
onclick('click.length.vs.maxn', showModal(modalDialog(
  title = "Number of lengths/3D points does not match MaxN", size="l",easyClose = TRUE,
  #downloadButton("download.maxn.synonyms","Download as csv"),
  renderDataTable(length.vs.maxn(),  rownames=FALSE,
                  options = list(paging = FALSE, searching = TRUE)))))

# LENGTH VS MAXN - plot ----
output$length.vs.maxn.plot <- renderPlot({
ggplot(length.vs.maxn(),aes(x=maxn,y=length.maxn,label = paste(genus,species,sep=" ")))+
  geom_abline(colour="red",alpha=0.5)+
  geom_point()+
  geom_text(alpha=0.2)+ 
  Theme1
})

# LENGTH VS MAXN - dropdown -----
output$length.vs.maxn.species.dropdown <- renderUI({
  df<-length.vs.maxn()
  
  options <- df %>%
    dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown"),as.character(family),as.character(genus)))%>%
    dplyr::group_by(family,genus,species)%>%
    dplyr::summarise(n=n())%>%
    dplyr::arrange(-n)%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    distinct(scientific) %>%
    pull("scientific")
  
  create_dropdown("length.vs.maxn.species.dropdown", options, NULL)
})

# LENGTH VS MAXN - species plot ----
output$length.vs.maxn.species.plot <- renderPlot({
  req(input$length.vs.maxn.species.dropdown)
  
  length.vs.maxn<-length.vs.maxn()%>%
    dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown"),as.character(family),as.character(genus)))%>%
    dplyr::mutate(scientific=paste(genus,species,sep=" "))%>%
    filter(scientific==input$length.vs.maxn.species.dropdown)
  
  ggplot(length.vs.maxn,aes(x=maxn,y=length.maxn,label = paste(sample)))+
    geom_abline(colour="red",alpha=0.5)+
    geom_point()+
    geom_text(alpha=0.2)+ 
    scale_y_continuous(expand = expand_scale(mult = c(-0.5, .5)))+
    scale_x_continuous(expand = expand_scale(mult = c(-0.5, .5)))+
    Theme1
})



# Download MAXN ----
output$info.download.maxn <- renderInfoBox({
  infoBox(
    "Download", HTML(paste("MaxN",br(),"")), icon = icon("download"),
    color = "green"
  )
})

output$download.maxn <- downloadHandler(
  filename = function() {
    req(input$campaign.name)
    paste(input$campaign.name,"_maxn.summary_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(maxn.complete.download(), file, row.names = FALSE)
  }
)

output$download.maxn.fst <- downloadHandler(
  filename = function() {
    req(input$campaign.name)
    paste(input$campaign.name,"_maxn.summary_", Sys.Date(), ".fst", sep="")
  },
  content = function(file) {
    write.fst(maxn.complete.download(), file)
  }
)

onclick('click.download.maxn',showModal(modalDialog(
  title = "Summarised maxn: adds zeros where a species is not present", size="l",easyClose = TRUE,
  downloadButton("download.maxn","Download as csv"),
  downloadButton("download.maxn.fst","Download as FST"))))

# output$download.length.wrong.big <- downloadHandler(
#   filename = function() {
#     paste("length.85.percent.of.max", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(dplyr::filter(length.wrong(), reason%in%c("too big")), file, row.names = FALSE)
#   }
# )

# Download maxn complete no errors ----
# output$info.download.maxn <- renderInfoBox({
#   infoBox(
#     "Download", HTML(paste("MaxN",br(),"")), icon = icon("download"),
#     color = "red",width=6
#   )
# })
# 
# onclick('click.info.download.maxn.complete.no.errors',showModal(modalDialog(
#   title = "Complete maxn: removes species identified as being in the wrong region and adds zeros where a species is not present",
#   downloadHandler(
#     filename = function() {
#       paste("maxn.complete.remove.errors-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(maxn.complete.download(), file)
#     }
#   ))
# ))

# Download length complete ----
output$info.download.length <- renderInfoBox({
  infoBox(
    "Download", HTML(paste("Length",br(),"")), icon = icon("download"),
    color = "green"
  )
})

output$download.length <- downloadHandler(
  filename = function() {
    req(input$campaign.name)
    paste(input$campaign.name,"_length.summary_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(length.complete.download(), file, row.names = FALSE)
  }
)

output$download.length.fst <- downloadHandler(
  filename = function() {
    paste(input$campaign.name,"_length.summary_", Sys.Date(), ".fst", sep="")
  },
  content = function(file) {
    write.fst(length.complete.download(), file)
  }
)

onclick('click.download.length',showModal(modalDialog(
  title = "Summarised length: adds zeros where a species is not present", size="l",easyClose = TRUE,
  downloadButton("download.length","Download as csv"),
  downloadButton("download.length.fst","Download as FST"))))

# Download mass complete ----
output$info.download.mass <- renderInfoBox({
  infoBox(
    "Download", HTML(paste("Mass",br(),"")), icon = icon("download"),
    color = "green"
  )
})

output$download.mass <- downloadHandler(
  filename = function() {
    paste(input$campaign.name,"_mass.summary_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(mass.complete.download(), file, row.names = FALSE)
  }
)

output$download.mass.fst <- downloadHandler(
  filename = function() {
    paste(input$campaign.name,"_mass.summary_", Sys.Date(), ".fst", sep="")
  },
  content = function(file) {
    write.fst(mass.complete.download(), file)
  }
)

onclick('click.download.mass',showModal(modalDialog(
  title = "Summarised mass: adds zeros where a species is not present", size="l",easyClose = TRUE,
  downloadButton("download.mass","Download as csv"),
  downloadButton("download.mass.fst","Download as FST"))))

}


