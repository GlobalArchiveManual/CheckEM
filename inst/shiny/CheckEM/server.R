function(input, output, session) {
  
  # TODO change the example dataset to 2022-05 Point cloates BRUVs
  
  # Increase size of files that can be uploaded
  options(shiny.maxRequestSize = 50*1024^2)
  
  observeEvent(input$new_user, {
    req(input$new_user)
    showModal(modalDialog(
      # title = "CheckEM has changed", 
      includeMarkdown("markdown/new.content.md"),
      easyClose = TRUE,
      footer = NULL,
      div(
        style = "display:inline-block;width:100%;text-align: center;",
        actionBttn(
          inputId = "okay",
          label = "Ok",
          style = "unite",
          size = "lg",
          color = "primary"
        )
      ))
    )
  })
  
  observeEvent(input$okay, {
    removeModal()
  })
  
  # FUNCTIONS -----
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
  
  ## _______________________________________________________ ----
  ##                        CREATE LIFE HISTORY              ----
  ## _______________________________________________________ ----
  
  life.history <- reactive({
    
    if(input$lifehistory %in% "aus"){
      lh <- all_data$lh.aus
    } else {
      lh <- all_data$lh.glo
    }
    
    lh
    
  })
  
  life.history.expanded <- reactive({
    
    if(input$lifehistory %in% "aus"){
      lh <- all_data$lh.aus.expanded
    } else {
      lh <- all_data$lh.glo.expanded
    }
    
    lh
    
  })
  
  life.history.min.max <- reactive({
    
    if(input$lifehistory %in% "aus"){
      lh <- all_data$lh.aus.min.max
    } else {
      lh <- all_data$lh.glo.min.max
    }
    
    lh
    
  })
  
  ## _______________________________________________________ ----
  ##                     CREATE SYNONYMS                     ----
  ## _______________________________________________________ ----
  synonyms <- reactive({
    
    if(input$lifehistory %in% "aus"){
      lh <- all_data$lh.aus.synonyms %>% dplyr::distinct()
    } else {
      lh <- all_data$lh.glo.synonyms %>% dplyr::distinct()
    }
    
    lh
    
  })
  
  ## _______________________________________________________ ----
  ##                     CREATE SCORE PLOT                     ----
  ## _______________________________________________________ ----
  
  output$score.plot <- renderPlot({
    
    length.vs.maxn <- length.vs.maxn() %>%
      dplyr::filter(!(maxn %in% 0 & length_maxn %in% 0))%>%
      dplyr::mutate(difference = abs(difference)) %>%
      dplyr::mutate(error = if_else(difference > 0, 1, 0)) #%>% glimpse()
    
    total.rows <- nrow(length.vs.maxn) #%>% glimpse()
    total.errors <- sum(length.vs.maxn$error) #%>% glimpse()
    
    length.maxn.score <- round(((total.rows - total.errors)/ total.rows) * 100, 2)
    
    metadata.score <- round(metadata.score(), 2)
    
    # TODO only include this if it is not a transect campaign
    count.score <- round(count.score(), 2)
    length.score <- round(length.score(), 2)
    
    length.v.3d <- length.v.3d()
    
    total.lengths <- sum(length.v.3d$number_of_length_measurements)
    total <- sum(length.v.3d$total_measurements)
    
    length.percent.score <- round((total.lengths/total) * 100, 2)
    
    if(input$upload %in% "EM"){
    
    dat <- data.frame(score = c("Sample Metadata", "% Length", "Count vs. Length", "Count", "Length"),
                      value = c(metadata.score, length.percent.score, length.maxn.score, count.score, length.score))
    
    dat$score <- fct_relevel(dat$score, "Count", "Count vs. Length", "% Length", "Length",  "Sample Metadata")
    
    cols <- c("Sample Metadata" = "#99D199", 
              "% Length" = "#99C3D1", 
              "Count vs. Length" = "#F8BEB3",
              "Count" = "#ffe69c",
              "Length" = "#bad0e8")
        
    p <- ggplot(dat, aes(score, value, fill = score)) +
      scale_fill_manual(values = cols) +
      xlab("") + ylab("") +
      theme_classic() +
      theme(legend.position = "none", 
            axis.line = element_blank(), 
            axis.text.y = element_blank() ,
            axis.text.x = element_blank() ,
            axis.ticks = element_blank()) +
      
      scale_y_continuous(limits = c(-50, 150)) + 
      
      geom_rect(xmin = 4.5, xmax = 5.5,
                ymin = 0,
                ymax = metadata.score, fill = "#99D199", color = "white", size = 2) +
      
      draw_line(x = c(4.51, 5.49), y = c(100, 100), color = "#008B00", size = 2) +
      
      geom_rect(xmin = 0.5, xmax = 1.5,
                ymin = 0,
                ymax = count.score, fill = "#ffe69c", color = "white", size = 2) +
      
      draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
      
      geom_rect(xmin = 1.5, xmax = 2.5,
                ymin = 0,
                ymax = length.maxn.score, fill = "#F8BEB3", color = "white", size = 2) +
      
      draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#EE5C42", size = 2) +
      
      geom_rect(xmin = 2.5, xmax = 3.5,
                ymin = 0,
                ymax = length.percent.score, fill = "#99C3D1", color = "white", size = 2) +
      
      draw_line(x = c(2.51, 3.49), y = c(70, 70), color = "#00688B", size = 2) +
      
      geom_rect(xmin = 3.5, xmax = 4.5,
                ymin = 0,
                ymax = length.score, fill = "#bad0e8", color = "white", size = 2) +
      
      draw_line(x = c(3.51, 4.49), y = c(100, 100), color = "#92bcea", size = 2) +
      
      
      geom_rect(xmin = 4.5, xmax = 5.5,
                ymin = 110,
                ymax = 150, fill = "#008B00", color = "white", size = 2) +
      geom_rect(xmin = 0.5, xmax = 1.5,
                ymin = 110,
                ymax = 150, fill = "#FFD966", color = "white", size = 2) +
      geom_rect(xmin = 1.5, xmax = 2.5,
                ymin = 110,
                ymax = 150, fill = "#EE5C42", color = "white", size = 2) +
      geom_rect(xmin = 2.5, xmax = 3.5,
                ymin = 110,
                ymax = 150, fill = "#00688B", color = "white", size = 2) +
      geom_rect(xmin = 3.5, xmax = 4.5,
                ymin = 110,
                ymax = 150, fill = "#92bcea", color = "white", size = 2) +
      
      geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 6.5, fontface = "bold") + 

      geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
      coord_polar() #start = 1.05
    
    } else {
      
      dat <- data.frame(score = c("Sample Metadata", "Count vs. Length", "Count", "Length"),
                        value = c(metadata.score, length.maxn.score, count.score, length.score))
      
      dat$score <- fct_relevel(dat$score, "Count", "Count vs. Length", "Length",  "Sample Metadata")
      
      cols <- c("Sample Metadata" = "#99D199", 
                # "% Length" = "#99C3D1", 
                "Count vs. Length" = "#F8BEB3",
                "Count" = "#ffe69c",
                "Length" = "#bad0e8")
      
      p <- ggplot(dat, aes(score, value, fill = score)) +
        scale_fill_manual(values = cols) +
        xlab("") + ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
              axis.line = element_blank(), 
              axis.text.y = element_blank() ,
              axis.text.x = element_blank() ,
              axis.ticks = element_blank()) +
        
        scale_y_continuous(limits = c(-50, 150)) + 
        
        geom_rect(xmin = 3.5, xmax = 4.5,
                  ymin = 0,
                  ymax = metadata.score, fill = "#99D199", color = "white", size = 2) +
        
        draw_line(x = c(3.51, 4.49), y = c(100, 100), color = "#008B00", size = 2) +
        
        geom_rect(xmin = 0.5, xmax = 1.5,
                  ymin = 0,
                  ymax = count.score, fill = "#ffe69c", color = "white", size = 2) +
        
        draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
        
        geom_rect(xmin = 1.5, xmax = 2.5,
                  ymin = 0,
                  ymax = length.maxn.score, fill = "#F8BEB3", color = "white", size = 2) +
        
        draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#EE5C42", size = 2) +
        geom_rect(xmin = 2.5, xmax = 3.5,
                  ymin = 0,
                  ymax = length.score, fill = "#bad0e8", color = "white", size = 2) +
        
        draw_line(x = c(2.51, 3.49), y = c(100, 100), color = "#92bcea", size = 2) +
        
        
        geom_rect(xmin = 3.5, xmax = 4.5,
                  ymin = 110,
                  ymax = 150, fill = "#008B00", color = "white", size = 2) +
        geom_rect(xmin = 0.5, xmax = 1.5,
                  ymin = 110,
                  ymax = 150, fill = "#FFD966", color = "white", size = 2) +
        geom_rect(xmin = 1.5, xmax = 2.5,
                  ymin = 110,
                  ymax = 150, fill = "#EE5C42", color = "white", size = 2) +
        geom_rect(xmin = 2.5, xmax = 3.5,
                  ymin = 110,
                  ymax = 150, fill = "#92bcea", color = "white", size = 2) +
        
        geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 6.5, fontface = "bold") + 
        
        geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
        coord_polar() #start = 1.05
      
    }
      
    
    plot <- ggdraw()  + draw_plot(p) + draw_image("https://globalarchivemanual.github.io/images/earth-globe.png", scale = 0.15, hjust = -0.0165, vjust = -0.020) # -0.025
    
    plot + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
  })
  
  ## _______________________________________________________ ----
  ##                     CREATE REGIONS                     ----
  ## _______________________________________________________ ----
  marine.regions <- reactive({
    
    if(input$lifehistory %in% "aus"){
      marine.regions <- all_data$aus.regions
    } else {
      marine.regions <- all_data$world.regions
    }
    
    marine.regions
    
  })
  
  ## _______________________________________________________ ----
  ##                        METADATA                         ----
  ## _______________________________________________________ ----
  
  ### ► Read in metadata ----
  metadata <- reactive({
    # When metadata uploaded ---
    if(!is.null(input$folderdir)) {
      
      # TODO change this to include a gif
      shinyalert("Uploading data", type = "info")
      
      # Get all _Metadata files in the folder
      files <- input$folderdir%>%
        dplyr::filter(grepl("_Metadata|_metadata", name))
      
      #print("metadata files")
      #glimpse(files)
      
      metadata <- data.frame() 
      
      if (is.null(files))
        
        return(NULL)
      
      for (i in seq_along(files$datapath)) {
        tmp <- read_csv(files$datapath[i], col_types = cols(.default = "c")) 
        
        if("campaignid" %in% colnames(metadata))
        {
          metadata <- metadata %>%
            dplyr::select(-c(campaignid))
        }
        
        
        tmp <- tmp %>%
          dplyr::mutate(campaignid = files$name[i])
        
        metadata <- bind_rows(metadata, tmp)
        
        if("CampaignID" %in% colnames(metadata))
        {
          metadata <- metadata %>%
            dplyr::select(-c(CampaignID))
        }
        

        
      }
      
      # print("previewed semi tidied names")
      
      metadata <- metadata %>%
        clean_names() #%>%
        #glimpse()
      
      # Rename any old names
      lookup <- c(depth_m = "depth",
                  visibility_m = "visibility",
                  latitude_dd = "latitude",
                  longitude_dd = "longitude")

      metadata <- metadata %>%
        dplyr::rename(dplyr::any_of(lookup))
      
      #message("uploaded metadata")
      
      metadata <- metadata %>%
        dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Metadata.csv" = "", "_metadata.csv" = ""))) %>%
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) #%>%
        #glimpse()
      
      message("metadata names")
      print(names(metadata))
      
      original_metadata_names <- c(names(metadata))
      
      if(!"sample" %in% original_metadata_names){
        
        message("metadata does not have sample as a column")
        
        # If point method and samples are opcodes
        if(input$method == "point" & input$sample == "opcode") {
          
          metadata <- metadata %>%
            dplyr::mutate(sample = opcode)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          metadata <- metadata %>%
            dplyr::mutate(sample = period)
        }
        
        # If transect method and sample = "opcode" + "period"
        if(input$method == "transect" & input$sample.t == "opcodeperiod") {
          
          metadata <- metadata %>%
            dplyr::mutate(sample = paste(opcode, period, sep = "_"))
          
        }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          lookup <- c(sample = "period") # If people have used period or sample then this will work
          
          metadata <- metadata %>%
            dplyr::mutate(sample = period)
        }
      } else {
        message("metadata DOES have sample as a column")
        
        if(input$method == "point" & input$sample == "opcode") {
          
          metadata <- metadata %>%
            dplyr::mutate(opcode = sample)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          metadata <- metadata %>%
            dplyr::mutate(period = sample)
        }
        
        # TODO figure this out 
        # # If transect method and sample = "opcode" + "period"
        # if(input$method == "transect" & input$sample.t == "opcodeperiod") {
        #   
        #   metadata <- metadata %>%
        #     dplyr::mutate(sample = paste(opcode, period, sep = "_"))
        #   
        # }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          lookup <- c(sample = "period") # If people have used period or sample then this will work
          
          metadata <- metadata %>%
            dplyr::mutate(period = sample)
        }
        
      }
      
    }
    
    # if no metadata file uploaded and method = single point. dataset = Ningloo BRUVs
    if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
      
      metadata <-  read.csv("data/examples/2022-05_PtCloates_stereo-BRUVS_Metadata.csv") %>%
        clean_names() %>%
        dplyr::rename(latitude_dd = latitude,
                      longitude_dd = longitude,
                      depth_m = depth) %>%
        dplyr::mutate(sample = opcode) %>%
        dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
        dplyr::select(campaignid, opcode, sample, latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, observer_count, observer_length) %>% 
        as.data.frame()
      
      original_metadata_names <- c("opcode")
      
      # TODO add an example dataset for DOVs
      #   # If no metadata file and method = transect. dataset = Ningaloo DOVs
      # } else if(is.null(input$upload.metadata) & input$method == "transect") {
      #
      #   ## ONLY ONE EXAMPLE METADATA FOR DOVs BG 13/07/2022
      #
      #   metadata <-  read.csv("data/2014-08_small subset_stereoDOVs_Metadata.csv") %>%
      #     clean_names() %>%
      #     dplyr::mutate(campaignid = "2014-08_small subset_stereoDOVs") %>%
      #     dplyr::mutate(sample = paste(opcode, period, sep = "_")) %>%
      #     dplyr::select(campaignid, sample, latitude_dd, longitude_dd, date, time, site, location, status, depth_m, successful_count, successful_length)
      
      # IF metadata file uploaded AND method = single point
    } 
    
    # If old date time format then:
    # - Turn into new format YYYY-MM-DDThh:mm:ss
    # - get timezone from lat/lon column
    
    # latitude_dd outside of -90 - 90
    if(nrow(metadata %>% dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
            dplyr::filter(latitude_dd < -90 | latitude_dd > 90)) > 0){
      shinyalert("Error in Metadata", "The latitude_dd column has values that are not between -90 - 90 decimal degrees. Please fix the error before continuing", type = "error")
    } 
    
    # longitude_dd outside of -180 - 180
    if(nrow(metadata %>% dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
            dplyr::filter(longitude_dd < -180 | longitude_dd > 180)) > 0){
      shinyalert("Error in Metadata", "The longitude_dd column has values that are not between -180 - 180 decimal degrees. Please fix the error before continuing", type = "error")
    } 
    
    met.names <- c(names(metadata))
    # print(met.names)
    
    if('observer' %in% met.names) {
      
      metadata <- metadata %>%
        dplyr::rename(observer_count = observer)
    } 
    
    if('observer' %in% met.names & 'date' %in% met.names) {
      shinyalert("Columns updated", "<b>'Observer'</b> column renamed to <b>'observer_count'</b><br><b>'Date'</b> and <b>'Time'</b> columns converted to <b>'date_time'", type = "warning", html = TRUE)
    } 
    
    if(!'observer' %in% met.names & 'date' %in% met.names) {
      shinyalert("Column updated", "<b>'Date'</b> and <b>'Time'</b> columns converted to <b>'date_time'", type = "warning", html = TRUE)
    } 
    
    if('observer' %in% met.names & !'date' %in% met.names) {
      shinyalert("Column updated", "<b>'Observer'</b> column renamed to <b>'observer_count'</b>", type = "warning", html = TRUE)
    } 
    
    if('date' %in% met.names) {
      # print("changing metadata date time info")

      metadata <- metadata %>%
        dplyr::mutate(date_time = paste0(str_sub(date, 1, 4), # Year
                                         "-",
                                         str_sub(date, 5, 6), # Month
                                         "-",
                                         str_sub(date, 7, 8), # Day
                                         "T",
                                         time
        )) %>%
        dplyr::mutate(tz_name = lutz::tz_lookup_coords(latitude_dd, longitude_dd)) #%>% glimpse()
      
      test <- metadata %>%
        dplyr::filter(is.na(tz_name))# %>%
        #glimpse()
      
      #message("timezones")
      timezones <- metadata %>%
        distinct(tz_name) %>%
        #glimpse() %>%
        dplyr::filter(!is.na(tz_name))
      
      dat <- data.frame()
      
      for(tz in unique(timezones$tz_name)){
        
        #TODO change this to be the actual date of the metadata (will mess up if there was daylight savings)
        temp.dat <- lutz::tz_offset("2023-07-31", tz = tz) %>%
          dplyr::select(tz_name, utc_offset_h)
        
        #print("looping")
        dat <- bind_rows(dat, temp.dat)
      }
      
      #print("dat")
      dat <- dat %>% distinct() #%>% glimpse()
      
      # Some timezones are not whole numbers e.g. Darwin is +09:30
      
      #print("tidied metadata")
      metadata <- left_join(metadata, dat) %>%
        dplyr::mutate(tz.sign = if_else(utc_offset_h > 0, "+", "-")) %>%
        dplyr::mutate(tz.floor = floor(utc_offset_h)) %>%
        dplyr::mutate(tz.extra = 60 * (utc_offset_h-tz.floor)) %>%
        dplyr::mutate(date_time = paste0(date_time, tz.sign, str_pad(abs(tz.floor), width = 2, pad = "0", side = "left"), ":", str_pad(tz.extra, width = 2, pad = "0", side = "left"))) #%>%
        #glimpse() 
      
      #print(unique(metadata$tz.sign))
      
    }
    
    if(!"sample" %in% original_metadata_names){
      # If point method and samples are opcodes
      if(input$method == "point" & input$sample == "opcode") {
        sample.cols <- c(opcode = NA_real_)
      }
      
      # If point method and samples are periods
      if(input$method == "point" & input$sample == "period") {
        sample.cols <- c(period = NA_real_)
      }
      
      # If transect method and sample = "opcode" + "period"
      if(input$method == "transect" & input$sample.t == "opcodeperiod") {
        sample.cols <- c(opcode = NA_real_,
                         period = NA_real_)
      }
      
      # If transect method and sample = "period"
      if(input$method == "transect" & input$sample.t == "period") {
        sample.cols <- c(period = NA_real_)
      }
    } else {
      sample.cols <- c(sample = NA_real_)
    }
    
    metadata.cols <- c(campaignid = NA_real_, 
                       latitude_dd = NA_real_,  
                       longitude_dd = NA_real_,  
                       date_time = NA_real_,  
                       site = NA_real_, 
                       location = NA_real_, 
                       status = NA_real_, 
                       depth_m = NA_real_, 
                       successful_count = NA_real_, 
                       successful_length = NA_real_, 
                       observer_count = NA_real_, 
                       observer_length = NA_real_,
                       inclusion_probability = NA_real_,
                       visibility_m = NA_real_, 
                       sample.cols)
    
    Sys.sleep(2)
    shinyjs::runjs("swal.close();")
    
    message("viewing metadata")
    metadata <- metadata %>%
      tibble::add_column(!!!metadata.cols[!names(metadata.cols) %in% names(.)]) %>%
      dplyr::filter(successful_count %in% c("Yes", "Y", "y", "yes")) %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::mutate(date_time = as.character(date_time)) %>% 
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, observer_count, observer_length, inclusion_probability, visibility_m, dplyr::any_of(c("successful_habitat_forward", "successful_habitat_backward")))  %>%
      dplyr::distinct() %>%
      glimpse()
  })  
  
  ## ► Find nearest marine regions add commonwealth and state zoning ----
  metadata.regions <- reactive({
    metadata <- metadata()
    
    if(input$region %in% "sample"){
      coordinates(metadata) <- c('longitude_dd', 'latitude_dd')
      proj4string(metadata) <- CRS(all_data$wgs.84)
      n <- nrow(metadata)
      nearest.region <- character(n)
      
      ## For each point, find name of nearest polygon
      for (i in seq_along(nearest.region)) {
        nearest.region[i] <- marine.regions()$REGION[which.min(gDistance(metadata[i, ], marine.regions(), byid = TRUE))]}
      
      ## Check that it worked
      #message("checking each sample nearest region")
      metadata.2 <- as.data.frame(nearest.region) %>%
        bind_cols(metadata()) %>%
        dplyr::rename(marine_region = nearest.region) %>%
        dplyr::mutate(sample = as.character(sample)) #%>%
        #dplyr::select(!status)
        #glimpse()
      
    } else {
      
      metadata.summed <- metadata %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(latitude_dd = mean(latitude_dd), longitude_dd = mean(longitude_dd))
      
      coordinates(metadata.summed) <- c('longitude_dd', 'latitude_dd')
      proj4string(metadata.summed) <- CRS(all_data$wgs.84)
      
      n <- nrow(metadata.summed)
      nearest.region <- character(n)
      
      ## For each point, find name of nearest polygon
      for (i in seq_along(nearest.region)) {
        nearest.region[i] <- marine.regions()$REGION[which.min(gDistance(metadata.summed[i, ], marine.regions(), byid = TRUE))]}
      
      #print("new metadata")
      
      ## Check that it worked
      metadata.2 <- metadata %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(latitude_dd = mean(latitude_dd), longitude_dd = mean(longitude_dd)) %>%
        dplyr::ungroup() %>%
        bind_cols(as.data.frame(nearest.region)) %>%
        dplyr::rename(marine_region = nearest.region) %>%
        dplyr::select(-c(latitude_dd, longitude_dd)) %>%
        dplyr::full_join(metadata, .) 
      
      # Changed here
      coordinates(metadata) <- c('longitude_dd', 'latitude_dd')
      proj4string(metadata) <- CRS(all_data$wgs.84)
    }
    
    # add in marine parks

    
    if(input$lifehistory %in% "aus"){
      #print("view metadata.marineparks for Australia")
      
      metadata.marineparks <- over(metadata, all_data$marineparks)  %>%
        dplyr::rename(zone = ZONE_TYPE) %>%
        tidyr::replace_na(list(status = "Fished")) %>%
        dplyr::mutate(status = fct_recode(status, "No-take" = "No-take", "Fished" = "Fished")) #%>% glimpse()
      
    } else {
      
      #print("view metadata.marineparks for Global")
      sf_use_s2(FALSE)
      
      metadata <- metadata %>% st_as_sf() %>% glimpse()
      
      metadata.marineparks <- st_intersection(metadata %>% dplyr::select(-c(status)), all_data$world_marineparks) %>%
        st_set_geometry(NULL) #%>% glimpse()
      
      metadata.marineparks <- full_join(metadata %>% dplyr::select(-c(status)), metadata.marineparks) %>%
        dplyr::select(zone, status)
      
    }
    
    # If user wants to keep status column only join zone
    if(input$status %in% "uploaded"){
      metadata.marineparks <- metadata.marineparks %>%
        dplyr::select(zone)
      
    } else {
      
      metadata.2 <- metadata.2 %>%
        dplyr::select(!status)
      
    }

    # Only bind in new columns if there is data
    if(nrow(metadata.marineparks) > 0) {
    
    # print("view metadata.regions")
    metadata.regions <- metadata.2 %>%
      bind_cols(metadata.marineparks) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, zone, marine_region, observer_count, observer_length, inclusion_probability, visibility_m) %>% 
      as.data.frame() #%>% glimpse()
    
    } else {
      
      if(input$status %in% "uploaded"){
      metadata.regions <- metadata.2 %>%
        dplyr::mutate(zone = "Not Reported") %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, zone, marine_region, observer_count, observer_length, inclusion_probability, visibility_m) %>% 
        as.data.frame() 
      
      } else {
        
        metadata.regions <- metadata.2 %>%
          dplyr::mutate(status = "", zone = "Not Reported") %>%
          dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, zone, marine_region, observer_count, observer_length, inclusion_probability, visibility_m) %>% 
          as.data.frame() 
      }
      
      
    }
    message("final metadata")
    glimpse(metadata.regions)
    
    
  })
  
  ## ► Preview metadata in dashboard ----
  output$table.metadata <- renderDataTable({
    
    # Checks on metadata
    #print("checking metadata")
    #glimpse(metadata.regions())
    
    errors <- ""
    
    # TODO change this to opcode and period
    # NA in sample
    # if(nrow(metadata.regions() %>% dplyr::filter(is.na(sample))) > 0){
    #   
    #   errors <- paste0(errors, "<li>The <b>Sample</b> column is missing values</li>", "<br>")
    #   # shinyalert("Missing Metadata", "The Sample column is missing values", type = "error")
    # }
    
    # NA in longitude_dd
    if(nrow(metadata.regions() %>% dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
            dplyr::filter(is.na(longitude_dd))) > 0){
      
      errors <- paste0(errors, "<li>The <b>longitude_dd</b> column is missing values or is non-numeric</li>", "<br>")
      # shinyalert("Missing Metadata", "The longitude_dd column is missing values or is non-numeric", type = "error")
    } 
    
    # NA in latitude_dd
    if(nrow(metadata.regions() %>% dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
            dplyr::filter(is.na(latitude_dd))) > 0){
      errors <- paste0(errors, "<li>The <b>latitude_dd</b> column is missing values or is non-numeric</li>", "<br>")
      # shinyalert("Missing Metadata", "The latitude_dd column is missing values or is non-numeric", type = "error")
    } 
    
    # NA in date_time
    if(nrow(metadata.regions() %>% dplyr::filter(is.na(date_time))) > 0){
      errors <- paste0(errors, "<li>The <b>date_time</b> column is missing values</li>", "<br>")
      # shinyalert("Missing Metadata", "The date_time column is missing values", type = "error")
    } 
    
    # NA in depth_m
    if(nrow(metadata.regions() %>% dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
            dplyr::filter(is.na(depth_m))) > 0){
      errors <- paste0(errors, "<li>The <b>depth_m</b> column is missing values or is non-numeric</li>", "<br>")
      # shinyalert("Missing Metadata", "The depth_m column is missing values or is non-numeric", type = "error")
    } 
    
    # NA in observer_count
    if(nrow(metadata.regions() %>% dplyr::filter(successful_count %in% c("Yes")) %>% dplyr::filter(is.na(observer_count))) > 0){
      errors <- paste0(errors, "<li>The <b>observer_count</b> column is missing values when successful_count = Yes</li>", "<br>")
      # shinyalert("Missing Metadata", "The observer_count column is missing values", type = "warning")
    } 
    
    # NA in observer_length
    if(nrow(metadata.regions() %>% dplyr::filter(successful_length %in% c("Yes")) %>% dplyr::filter(is.na(observer_length))) > 0){
      errors <- paste0(errors, "<li>The <b>observer_length</b> column is missing values when successful_length = Yes</li>", "<br>")
      # shinyalert("Missing Metadata", "The observer_length column is missing values", type = "warning")
    }
    
    # NA in successful_count
    if(nrow(metadata.regions() %>% dplyr::filter(is.na(successful_count))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_count column is missing values</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_count column is missing values", type = "error")
    }
    
    # NA in successful_length
    if(nrow(metadata.regions() %>% dplyr::filter(is.na(successful_length))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_length</b> column is missing values</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_length column is missing values", type = "error")
    }
    
    # Format of successful_count
    if(nrow(metadata.regions() %>% dplyr::filter(!successful_count %in% c("Yes", "No"))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_count</b> column has values that are not 'Yes' or 'No'</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_count column has values that are not 'Yes' or 'No'", type = "error")
    }
    
    # Format of successful_length
    if(nrow(metadata.regions() %>% dplyr::filter(!successful_length %in% c("Yes", "No"))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_length</b> column has values that are not 'Yes' or 'No'</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_length column has values that are not 'Yes' or 'No'", type = "error")
    }
    
    if(!errors == ""){
      beepr::beep("coin")
      
      shinyalert("Issues with Metadata", text = HTML(paste0("Please fix these before continuing<br><br>", errors)), type = "error", html = TRUE)
    }
    
      # Show table
      metadata.regions() %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
        dplyr::select(!sample)#%>% glimpse()
  })
  
  ## ►  Metadata score - dataframe ----
  metadata.score <- reactive({

    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {

      metadata.failed <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd) | is.na(date_time) | is.na(depth_m) | 
                        !successful_count %in% c("Yes", "No") | !successful_length %in% c("Yes", "No") | 
                        successful_count %in% c("Yes") & is.na(observer_count) |
                        is.na(opcode)) 
      
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {

      metadata.failed <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd) | is.na(date_time) | is.na(depth_m) | 
                        !successful_count %in% c("Yes", "No") | !successful_length %in% c("Yes", "No") | 
                        successful_count %in% c("Yes") & is.na(observer_count) |
                        is.na(period)) 
      
    }
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {

      metadata.failed <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd) | is.na(date_time) | is.na(depth_m) | 
                        !successful_count %in% c("Yes", "No") | !successful_length %in% c("Yes", "No") | 
                        successful_count %in% c("Yes") & is.na(observer_count) |
                        is.na(opcode) | is.na(period)) 
      
    }
    
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {

      metadata.failed <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd) | is.na(date_time) | is.na(depth_m) | 
                        !successful_count %in% c("Yes", "No") | !successful_length %in% c("Yes", "No") | 
                        successful_count %in% c("Yes") & is.na(observer_count) |
                        is.na(period)) 
      
    }
    
    score <- ((nrow(metadata.regions()) - nrow(metadata.failed)) / nrow(metadata.regions())) * 100
    
    # score <- 0
    # 
    # # TODO change this to opcode and period
    # # NA in sample
    # if(!nrow(metadata.regions() %>% dplyr::filter(is.na(sample))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in sample")
    #   #print(score)
    # }
    # 
    # # NA in longitude_dd
    # if(!nrow(metadata.regions() %>% dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
    #         dplyr::filter(is.na(longitude_dd))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in longitude_dd")
    #   #print(score)
    # }
    # 
    # # NA in latitude_dd
    # if(!nrow(metadata.regions() %>% dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
    #         dplyr::filter(is.na(latitude_dd))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in latitude_dd")
    #   #print(score)
    # } 
    # 
    # # NA in date_time
    # if(!nrow(metadata.regions() %>% dplyr::filter(is.na(date_time))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in date_time")
    #   #print(score)
    # } 
    # 
    # # NA in depth_m
    # if(!nrow(metadata.regions() %>% dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
    #         dplyr::filter(is.na(depth_m))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in depth_m")
    #   #print(score)
    # } 
    # 
    # # NA in observer_count
    # if(!nrow(metadata.regions() %>% dplyr::filter(successful_count %in% c("Yes")) %>% dplyr::filter(is.na(observer_count))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in observer_count")
    #   #print(score)
    # } 
    # 
    # # # NA in observer_length
    # # if(!nrow(metadata.regions() %>% dplyr::filter(is.na(observer_length))) > 0){
    # #   score <- score + 6.25
    # #   #print("No NA in observer_length")
    # #   #print(score)
    # # }
    # 
    # # Format of successful_count
    # if(!nrow(metadata.regions() %>% dplyr::filter(!successful_count %in% c("Yes", "No"))) > 0){
    #   score <- score + 6.25
    #   #print("No NA in successful_count")
    #   #print(score)
    # }
    # 
    # # Format of successful_length
    # if(!nrow(metadata.regions() %>% dplyr::filter(!successful_length %in% c("Yes", "No"))) > 0){
    # score <- score + 6.25
    # #   print("No NA in successful_length")
    # #   print(score)
    # }
    # 
    # # If any samples are in points or count that don't have metadata
    # if (dim(points.samples.without.metadata())[1] > 0) {
    #   score <- score + 0
    #   
    # } else {
    #   score <- score + 50
    # }
    
    # score <- score
    
    })
  
  ## ► Metadata score - valueBox ----
  output$metadata.score <- renderValueBox({
    
    valueBox(width = 3, round(metadata.score(), 2), "Sample metadata score", 
             icon = icon("percent"), color = "blue"
    )
  })
  
  ## ► Metadata score transect - valueBox ----
  output$metadata.score.t <- renderValueBox({
    
    valueBox(width = 3, round(metadata.score(), 2), "Sample metadata score", 
             icon = icon("percent"), color = "blue"
    )
  })
  
  
  
  ## _______________________________________________________ ----
  ##        Metadata checking for single point campaigns     ----
  ## _______________________________________________________ ----
  
  ## ► Total number of samples - valueBox ----
  output$metadata.no.samples <- renderValueBox({
    
    metadata.samples <- metadata() %>%
      dplyr::distinct(campaignid, sample)
    
    valueBox(width = 3, nrow(metadata.samples), "Samples in the Sample Metadata", 
             icon = icon("list"), color = "blue"
    )
  })
  
  ## ► Samples without points - dataframe ----
  metadata.samples.without.fish <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      samples <- points() %>%
        distinct(campaignid, sample)
    } else {
      samples <- count() %>%
        distinct(campaignid, sample)
    }
    missing.fish <- anti_join(metadata.samples, samples) %>%
      dplyr::select(-sample)
    
  })
  
  ## ► Samples without points - valueBox ----
  output$metadata.samples.without.fish <- renderValueBox({
    
    if(input$upload %in% "EM"){
      text <- "Sample(s) without points data"
    } else {
      text <- "Sample(s) without count data"
    }
    
    if (dim(metadata.samples.without.fish())[1] > 0) {
      total <- nrow(metadata.samples.without.fish())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 3, 
             total, 
             text, 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without points - onclick----
  onclick('click.metadata.samples.without.fish', 
          showModal(modalDialog(
            title = "Samples without fish in the count csv or points text file", 
            easyClose = TRUE,
            h4("This is a list of samples (from the metadata) that do not have any count data or points. Please check if no fish were observed or the 'successful_count' column needs to be updated"),
            
            renderDataTable(metadata.samples.without.fish(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Samples without metadata - dataframe ----
  points.samples.without.metadata <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      samples <- points() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    } else {
      samples <- count() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    }
    missing.metadata <- anti_join(samples, metadata.samples) %>%
      dplyr::select(-sample)
  })
  
  
  count.score <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      
      dat <- points()
      
      samples <- points() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
      
    } else {
      
      dat <- count()
      
      samples <- count() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
    }
    
    rows.missing.metadata <- anti_join(samples, metadata.samples) %>%
      dplyr::select(-sample)
    
    score <- ((nrow(dat) - nrow(rows.missing.metadata))/nrow(dat))*100
    
  })
  
  ## ► Samples without metadata - valueBox ----
  output$points.samples.without.metadata <- renderValueBox({
    
    if(input$upload %in% "EM"){
      text <- "Sample(s) in points file missing metadata"
    } else {
      text <- "Sample(s) in count file missing metadata"
    }
    
    if (dim(points.samples.without.metadata())[1] > 0) {
      total <- nrow(points.samples.without.metadata())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2, 
             total, 
             text, 
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Samples without metadata - onclick ----
  onclick('click.points.samples.without.metadata', 
          showModal(modalDialog(
            title = "Sample(s) in count csv or points text file without metadata", 
            easyClose = TRUE,
            renderDataTable(points.samples.without.metadata(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  
  ## ► Samples without lengths - dataframe ----
  metadata.samples.without.length <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      distinct() %>%
      mutate(sample = as.factor(sample)) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y","YES"))
    
    if(input$upload %in% "EM"){
      samples <- length() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    } else {
      samples <- gen.length() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    }
    missing.fish <- anti_join(metadata.samples, samples) %>%
      dplyr::select(-sample)
    
  })
  
  ## ► Samples without lengths - valueBox ----
  output$metadata.samples.without.length <- renderValueBox({
    
    if (dim(metadata.samples.without.length())[1] > 0) {
      total <- nrow(metadata.samples.without.length())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Sample(s) without lengths", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without lengths - onclick----
  onclick('click.metadata.samples.without.length', 
          showModal(modalDialog(
            title = "Samples without length in the _lengths or _length files", 
            easyClose = TRUE,
            h4("This is a list of samples (from the metadata) that do not have any lengths Please check if no fish were observed or the 'successful_length' column needs to be updated"),
            renderDataTable(metadata.samples.without.length(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Length Samples without metadata - dataframe ----
  length.samples.without.metadata <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      samples <- bind_rows(length(), threedpoints()) %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    } else {
      samples <- gen.length() %>%
        distinct()
    }
    
    missing.metadata <- anti_join(samples, metadata.samples) %>%
      dplyr::select(!sample)
  })
  
  ## ► Length Samples without metadata - dataframe ----
  length.score <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      dat <- bind_rows(length(), threedpoints())
      
      samples <- bind_rows(length(), threedpoints()) %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
      
    } else {
      dat <- gen.length()
      
      samples <- gen.length()
    }
    
    rows.missing.metadata <- anti_join(samples, metadata.samples)
    
    score <- ((nrow(dat) - nrow(rows.missing.metadata))/nrow(dat))*100
    
  })
  
  ## ► Length Samples without metadata - valueBox ----
  output$length.samples.without.metadata <- renderValueBox({
    
    if(input$upload %in% "EM"){
      text <- "Sample(s) in lengths or 3D points file missing metadata"
    } else {
      text <- "Sample(s) in length file missing metadata"
    }
    
    if (dim(length.samples.without.metadata())[1] > 0) {
      total <- nrow(length.samples.without.metadata())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2, 
             total, 
             text, 
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Length Samples without metadata - onclick ----
  onclick('click.length.samples.without.metadata', 
          showModal(modalDialog(
            title = "Sample(s) in length or lengths files without metadata", 
            easyClose = TRUE,
            renderDataTable(length.samples.without.metadata(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Leaflet map ----
  
  # show shiny alert once user clicks on metadata tab
  
  observeEvent(input$tabs, {
   if(input$tabs == "checkmetadata"){
     if(input$lifehistory %in% "aus"){
       shinyalert("Loading Australia's marine spatial zoning", "The map may take a minute to load", type = "info")
     } else {
       shinyalert("Warning", "The World Database on Protected Areas is NOT displayed on the map", type = "info")
     }
   }
  })
  
  output$map.metadata <- renderLeaflet({
    
    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {
      metadata <- metadata.regions() %>%
        mutate(content = paste(sep = " ", 
                               "<b>opcode:", opcode, "</b>", "<br/>", 
                               "<b>status:</b>", status, "<br/>", 
                               "<b>depth_m:</b>", depth_m, "m", "<br/>", 
                               "<b>site:</b>", site, "<br/>", 
                               "<b>location:</b>", location, "<br/>", 
                               "<b>date_time:</b>", date_time, "<br/>"
        ))
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {
      metadata <- metadata.regions() %>%
        mutate(content = paste(sep = " ", 
                               "<b>period:", period, "</b>", "<br/>", 
                               "<b>status:</b>", status, "<br/>", 
                               "<b>depth_m:</b>", depth_m, "m", "<br/>", 
                               "<b>site:</b>", site, "<br/>", 
                               "<b>location:</b>", location, "<br/>", 
                               "<b>date_time:</b>", date_time, "<br/>"
        ))
    }
    
    map <- leaflet_basemap(data = metadata) %>%
      
      addAwesomeMarkers(icon = ~iconSet[status], label = ~as.character(sample), popup = ~content, ~longitude_dd, ~latitude_dd) %>%
      
      fitBounds(lng1 = min(metadata$longitude_dd), 
                lat1 = min(metadata$latitude_dd), 
                lng2 = max(metadata$longitude_dd), 
                lat2 = max(metadata$latitude_dd))
    
    if(input$lifehistory %in% "aus"){
    map <- map %>%
      addGlPolygons(data =  all_data$marineparks.single, # Changed from clipped
                    fillColor = ~ all_data$comm.pal(zone),
                    # color = ~ all_data$comm.pal(ZoneName),
                    popup =  "ZONE_TYPE",
                    group = "Marine parks",
                    opacity = 0.9) %>%
      addLayersControl(
        overlayGroups = c("Marine parks"),
        options = layersControlOptions(collapsed = FALSE))
      # hideGroup("Marine parks")
    } else {
      
      map <- map #%>%
        # addGlPolygons(data =  all_data$world_marineparks_single, # Changed from clipped
        #               fillColor = ~ all_data$iucn.pal(zone),
        #               # color = ~ all_data$comm.pal(ZoneName),
        #               popup =  "NAME",
        #               group = "Marine parks",
        #               opacity = 0.9) %>%
        # addLayersControl(
        #   overlayGroups = c("Marine parks"),
        #   options = layersControlOptions(collapsed = FALSE))
      
    }
    
    map
    
    
    
  })
  
  ## _______________________________________________________ ----
  ##           Metadata checking for transect campaigns      ----
  ## _______________________________________________________ ----
  
  ## ► Total number of samples - valueBox ----
  output$metadata.no.samples.t <- renderValueBox({
    
    metadata.samples <- metadata() %>%
      dplyr::distinct(campaignid, sample)
    
    valueBox(width = 3, nrow(metadata.samples), "Samples in the Sample Metadata", 
             icon = icon("list"), color = "blue"
    )
  })
  
  ## ► Samples without lengths - dataframe----
  metadata.samples.without.fish.t <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y","YES"))
    
    length.samples <- length() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      dplyr::distinct() 
    
    missing.fish <- anti_join(metadata.samples, length.samples) %>%
      dplyr::select(!sample)
  })
  
  ## ► Samples without lengths - valueBox ----
  output$metadata.samples.without.fish.t <- renderValueBox({
    
    if (dim(metadata.samples.without.fish.t())[1] > 0) {
      total <- nrow(metadata.samples.without.fish.t())
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Sample(s) without lengths", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without lengths - onclick----
  onclick('click.metadata.samples.without.fish.t', 
          showModal(modalDialog(
            title = "Sample(s) without fish in the length or lengths files", 
            easyClose = TRUE,
            h4("This is a list of samples (from the metadata) that do not have any length measurements. Please check if no fish were observed or the 'successful_length' column needs to be updated"),
            
            renderDataTable(metadata.samples.without.fish.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  
  ## ► Samples without 3D points - dataframe----
  metadata.samples.without.3dpoints.t <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample))
    
    threedpoints.samples <- threedpoints() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      dplyr::distinct() 
    
    missing.fish <- anti_join(metadata.samples, threedpoints.samples) %>%
      dplyr::select(!sample)
  })
  
  ## ► Samples without 3D points - valueBox ----
  output$metadata.samples.without.3dpoints.t <- renderValueBox({
    
    if (dim(metadata.samples.without.3dpoints.t())[1] > 0) {
      total <- nrow(metadata.samples.without.3dpoints.t())
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Sample(s) without 3D points", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without lengths - onclick----
  onclick('click.metadata.samples.without.3dpoints.t', 
          showModal(modalDialog(
            title = "Sample(s) without fish in the 3D points text file", 
            easyClose = TRUE,
            h4("This is a list of samples (from the metadata) that do not have any 3D points. Please check if no fish were observed or the 'successful_length' column needs to be updated"),
            
            renderDataTable(metadata.samples.without.3dpoints.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Samples without metadata - dataframe ----
  length.samples.without.metadata.t <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample))
    
    threedpoints.samples <- threedpoints() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
    
    length.samples <- length() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
    
    samples <- bind_rows(threedpoints.samples, length.samples) %>%
      dplyr::distinct()
    
    missing.metadata <- anti_join(samples, metadata.samples) %>%
      dplyr::select(!sample)
  })
  
  ## ► Samples without metadata - valueBox ----
  output$length.samples.without.metadata.t <- renderValueBox({
    
    if (dim(length.samples.without.metadata.t())[1] > 0) {
      total <- nrow(length.samples.without.metadata.t())
      col = "red"
    } else {
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Sample(s) in length or 3D points file missing metadata", 
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Samples without metadata - onclick ----
  onclick('click.length.samples.without.metadata.t', 
          showModal(modalDialog(
            title = "Sample(s) in length or 3D points without metadata", 
            easyClose = TRUE,
            renderDataTable(length.samples.without.metadata.t(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Leaflet map ----
  
  # show shiny alert once user clicks on metadata tab
  
  observeEvent(input$tabs, {
    if(input$tabs == "checkmetadatat"){
      if(input$lifehistory %in% "aus"){
        shinyalert("Loading Australia's marine spatial zoning", "The map may take a minute to load", type = "info")
      } else {
        shinyalert("Warning", "The World Database on Protected Areas is NOT displayed on the map", type = "info")
      }
    }
  })
  
  output$map.metadata.t <- renderLeaflet({
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {
      metadata <- metadata.regions() %>%
        mutate(content = paste(sep = " ", 
                               "<b>opcode:", opcode, "</b>", "<br/>", 
                               "<b>period:", period, "</b>", "<br/>", 
                               "<b>status:</b>", status, "<br/>", 
                               "<b>depth_m:</b>", depth_m, "m", "<br/>", 
                               "<b>site:</b>", site, "<br/>", 
                               "<b>location:</b>", location, "<br/>", 
                               "<b>date_time:</b>", date_time, "<br/>"
        ))
    }
    
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {
      metadata <- metadata.regions() %>%
        mutate(content = paste(sep = " ", 
                               "<b>period:", period, "</b>", "<br/>", 
                               "<b>status:</b>", status, "<br/>", 
                               "<b>depth_m:</b>", depth_m, "m", "<br/>", 
                               "<b>site:</b>", site, "<br/>", 
                               "<b>location:</b>", location, "<br/>", 
                               "<b>date_time:</b>", date_time, "<br/>"
        ))
    }
    
    map <- leaflet_basemap(data = metadata) %>%
      
      addAwesomeMarkers(icon = ~iconSet[status], label = ~as.character(sample), popup = ~content, ~longitude_dd, ~latitude_dd) %>%
      
      fitBounds(lng1 = min(metadata$longitude_dd), 
                lat1 = min(metadata$latitude_dd), 
                lng2 = max(metadata$longitude_dd), 
                lat2 = max(metadata$latitude_dd))
    
    if(input$lifehistory %in% "aus"){
      map <- map %>%
        addGlPolygons(data =  all_data$marineparks.single, # Changed from clipped
                      fillColor = ~ all_data$comm.pal(zone),
                      # color = ~ all_data$comm.pal(ZoneName),
                      popup =  "ZONE_TYPE",
                      group = "Marine parks",
                      opacity = 0.9) %>%
        addLayersControl(
          overlayGroups = c("Marine parks"),
          options = layersControlOptions(collapsed = FALSE))
      # hideGroup("Marine parks")
    } else {
      
      map <- map #%>%
        # addGlPolygons(data =  all_data$world_marineparks_single, # Changed from clipped
        #               fillColor = ~ all_data$iucn.pal(zone),
        #               # color = ~ all_data$comm.pal(ZoneName),
        #               popup =  "NAME",
        #               group = "Marine parks",
        #               opacity = 0.9) %>%
        # addLayersControl(
        #   overlayGroups = c("Marine parks"),
        #   options = layersControlOptions(collapsed = FALSE))
      
    }
    
    map
  })
  
  ## _______________________________________________________ ----
  ##                          PERIODS                        ----
  ## _______________________________________________________ ----
  ## ► Read in periods ----
  periods <- reactive({
    # When folder chosen ---
    if(!is.null(input$folderdir)) {
      
      # Get all _Period files in the folder
      files <- input$folderdir%>%
        dplyr::filter(grepl("_Period.txt", name)) #%>% glimpse() 
      
      periods <- data.frame() 
      
      if (is.null(files)) return(NULL)
      
      for (i in seq_along(files$datapath)) {
        tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"))  %>%
          dplyr::mutate(campaignid = files$name[i])
        
        periods <- bind_rows(periods, tmp) #%>% glimpse()
      }
      
      periods <- periods %>%
        clean_names() %>%
        dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Period.txt" = ""))) #%>% glimpse()
      
      # If point method and samples are opcodes
      if(input$method == "point" & input$sample == "opcode") {
        
        periods <- periods %>%
          dplyr::mutate(sample = opcode)
      }
      
      # If point method and samples are periods
      if(input$method == "point" & input$sample == "period") {
        
        periods <- periods %>%
          dplyr::mutate(sample = period)
      }
      
      # If transect method and sample = "opcode" + "period"
      if(input$method == "transect" & input$sample.t == "opcodeperiod") {
        
        periods <- periods %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "_"))
        
      }
      # If transect method and sample = "period"
      if(input$method == "transect" & input$sample.t == "period") {
        
        periods <- periods %>%
          dplyr::mutate(sample = period)
      }
    }
      
    #   # If point method and opcode = sample e.g. BRUVs
    #   if(input$method == "point" & input$sample == "opcode") {
    #     
    #     periods <- periods %>%
    #       dplyr::rename(sample = opcode)
    #   }
    #   
    #   # If point method and opcode = period e.g. BOSS
    #   if(input$method == "point" & input$sample == "period") {
    #     
    #     periods <- periods %>%
    #       dplyr::mutate(sample = period)
    #   }
    #   
    #   # If transect method and sample = "opcode" + "period"
    #   if(input$method == "transect" & input$sample.t == "opcodeperiod") {
    #     
    #     periods <- periods %>%
    #       dplyr::mutate(sample = paste(opcode, period, sep = "_"))
    #     
    #   }
    #   # If transect method and sample = "period"
    #   if(input$method == "transect" & input$sample.t == "period") {
    #     
    #     lookup <- c(sample = "period") # If people have used period or sample then this will work
    #     
    #     periods <- periods %>%
    #       dplyr::rename(dplyr::any_of(lookup)) 
    #   }
    # }
    
    # if no folder chosen and method = single point. dataset = Ningloo BRUVs
    if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
      
      periods <-  read.delim("data/examples/2022-05_PtCloates_stereo-BRUVS_Period.txt", na.strings = "") %>%
        clean_names() %>%
        dplyr::mutate(sample = as.factor(opcode)) %>%
        dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
        as.data.frame()
      
      # TODO add an example dataset for DOVs
    } 
    
    periods <- periods %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::semi_join(metadata()) %>%
      dplyr::rename(time_start = timestart,
                    time_end = timeend,
                    has_end = hasend) %>%
      dplyr::mutate(time_start	= as.numeric(time_start)) %>%
      dplyr::mutate(time_end	= as.numeric(time_end))
    
  })
  
  # ► Preview periods ----
  output$table.periods <- renderDataTable({
    
    periods()
    
  })  
  
  ## _______________________________________________________ ----
  ##        Period checking for single point campaigns       ----
  ## _______________________________________________________ ----
  ## ► Periods without end - dataframe ----
  periods.no.end <- reactive({
    if(input$upload %in% "EM"){
      periods.no.end <- periods() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), time_start, time_end, has_end) %>%
        dplyr::distinct() %>%
        dplyr::filter(has_end == 0) %>%
        dplyr::select(!sample)
    }
  })
  
  ## ► Periods without end - valueBox ----
  output$periods.no.end <- renderValueBox({
    if(input$upload %in% "EM"){
      if (dim(periods.no.end())[1] > 0) {
        total <- nrow(periods.no.end())
        col <- "red"
      }
      else{
        total = 0
        col <- "green"
      }
      
      valueBox(width = 4, 
               total, 
               "Period(s) without an end", 
               icon = icon("question"), color = col
      )
    }
  })
  
  ## ► Periods without end - onclick----
  onclick('click.periods.no.end', 
          showModal(modalDialog(
            title = "Period(s) without an end", 
            easyClose = TRUE,
            renderDataTable(periods.no.end(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Samples without periods - dataframe ----
  samples.without.periods <- reactive({
    if(input$upload %in% "EM"){
      metadata.samples <- metadata() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
        dplyr::distinct() %>%
        dplyr::mutate(sample = as.factor(sample))
      
      periods.samples <- periods() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
      
      missing.periods <- anti_join(metadata.samples, periods.samples) %>%
        dplyr::select(!sample)
    }
  })
  
  ## ► Samples without periods - valueBox ----
  output$samples.without.periods <- renderValueBox({
    if(input$upload %in% "EM"){
      if (dim(samples.without.periods())[1] > 0) {
        total <- nrow(samples.without.periods())
        col <- "yellow"
      }
      else{
        total = 0
        col <- "green"
      }
      
      # If point method and samples are opcodes
      if(input$method == "point" & input$sample == "opcode") {
        text <- "Sample(s) without a period"
      }
      
      # If point method and samples are periods
      if(input$method == "point" & input$sample == "period") {
        text <- "Sample(s) missing in EMObs"
      }
    
      valueBox(width = 4, 
               total, 
               text, 
               icon = icon("question"), color = col
      )
    }
  })
  
  ## ► Samples without periods - onclick----
  onclick('click.samples.without.periods', 
          showModal(modalDialog(
            title = "Sample(s) without periods", 
            easyClose = TRUE,
            renderDataTable(samples.without.periods(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Periods wrong length - dataframe ----
  periods.wrong <- reactive({
    if(input$upload %in% "EM"){
      
      print("periods wrong time")
      
      periods.wrong <- periods() %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), time_start, time_end, has_end) %>%
        dplyr::distinct() %>%
        dplyr::mutate(period_time = round(time_end - time_start)) %>%
        dplyr::filter(!period_time %in% c(input$period.limit)) #%>%
        #glimpse()
    }
  })
  
  ## ► Periods wrong length - valueBox ----
  output$periods.wrong <- renderValueBox({
    if(input$upload %in% "EM"){
      if (dim(periods.wrong())[1] > 0) {
        total <- nrow(periods.wrong())
        col <- "yellow"
      }
      else{
        total = 0
        col <- "green"
      }
      
      valueBox(width = 4, 
               total, 
               paste("Periods not", input$period.limit, "mins long", sep = " "), 
               icon = icon("question"), color = col
      )
    }
  })
  
  ## ► Periods wrong length - onclick----
  onclick('click.periods.wrong', 
          showModal(modalDialog(
            title = "Sample(s) without periods", 
            easyClose = TRUE,
            renderDataTable(periods.wrong(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Points without periods - dataframe ----
  points.outside.periods <- reactive({
    if(input$upload %in% "EM"){
      points <- points() %>%
        dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, frame, em_comment, code)
    }
  })
  
  ## ► Points without periods - valueBox ----
  output$points.outside.periods <- renderValueBox({
    if(input$upload %in% "EM"){
      if (dim(points.outside.periods())[1] > 0) {
        total <- nrow(points.outside.periods())
        col <- "yellow"
      }
      else{
        total = 0
        col <- "green"
      }
      
      valueBox(width = 4, 
               total, 
               "Point(s) outside periods", 
               icon = icon("question"), color = col
      )
    }
  })
  
  ## ► Points without periods - onclick----
  onclick('click.points.outside.periods', 
          showModal(modalDialog(
            title = "Point(s) without periods", 
            easyClose = TRUE,
            renderDataTable(points.outside.periods(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Lengths without periods - dataframe ----
  lengths.outside.periods <- reactive({
    if(input$upload %in% "EM"){
      lengths <- length3dpoints() %>%
        dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, length_mm, frame_left, em_comment)
    }
  })
  
  ## ► Lengths without periods - valueBox ----
  output$lengths.outside.periods <- renderValueBox({
    if(input$upload %in% "EM"){
      if (dim(lengths.outside.periods())[1] > 0) {
        total <- nrow(lengths.outside.periods())
        col <- "yellow"
      }
      else{
        total = 0
        col <- "green"
      }
      
      valueBox(width = 4, 
               total, 
               "Length(s) or 3D point(s) outside periods", 
               icon = icon("question"), color = col
      )
    }
  })
  
  ## ► Lengths without periods - onclick----
  onclick('click.lengths.outside.periods', 
          showModal(modalDialog(
            title = "Length(s) or 3D point(s) outside periods", 
            easyClose = TRUE,
            renderDataTable(lengths.outside.periods(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## _______________________________________________________ ----
  ##      Period checking for transect based campaigns       ----
  ## _______________________________________________________ ----
  ## ► Periods without end - dataframe ----
  periods.no.end.t <- reactive({
    
    periods.no.end <- periods() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), time_start, time_end, has_end) %>%
      dplyr::distinct() %>%
      dplyr::filter(has_end == 0)
  })
  
  ## ► Periods without end - valueBox ----
  output$periods.no.end.t <- renderValueBox({
    
    if (dim(periods.no.end.t())[1] > 0) {
      total <- nrow(periods.no.end.t())
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4, 
             total, 
             "Period(s) without an end", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Periods without end - onclick----
  onclick('click.periods.no.end.t', 
          showModal(modalDialog(
            title = "Period(s) without an end", 
            easyClose = TRUE,
            renderDataTable(periods.no.end.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  # ## ► Periods wrong length - dataframe ----
  # periods.wrong.t <- reactive({
  #   
  #   periods.wrong <- periods() %>%
  #     distinct(campaignid, sample, period, time_start, time_end, has_end) %>%
  #     mutate(period_time = round(time_end - time_start)) %>%
  #     filter(!period_time %in% c(input$period.limit))
  # })
  # 
  # ## ► Periods wrong length - valueBox ----
  # output$periods.wrong.t <- renderValueBox({
  #   
  #   if (dim(periods.wrong.t())[1] > 0) {
  #     total <- nrow(periods.wrong())
  #     col <- "yellow"
  #   }
  #   else{
  #     total = 0
  #     col <- "green"
  #   }
  #   
  #   valueBox(width = 4, 
  #            total, 
  #            paste("Periods not", input$period.limit, "mins long", sep = " "), 
  #            icon = icon("question"), color = col
  #   )
  # })
  # 
  # ## ► Periods wrong length - onclick----
  # onclick('click.periods.wrong', 
  #         showModal(modalDialog(
  #           title = "Samples without periods", 
  #           easyClose = TRUE,
  #           renderDataTable(periods.wrong(), rownames = FALSE, 
  #                           options = list(paging = FALSE, searching = TRUE)))
  #         ))
  
  ## ► Points without periods - dataframe ----
  points.outside.periods.t <- reactive({
    
    points <- points() %>%
      dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, frame, em_comment)
  })
  
  ## ► Points without periods - valueBox ----
  output$points.outside.periods.t <- renderValueBox({
    
    if (dim(points.outside.periods.t())[1] > 0) {
      total <- nrow(points.outside.periods.t())
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4, 
             total, 
             "Point(s) outside periods", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Points without periods - onclick----
  onclick('click.points.outside.periods.t', 
          showModal(modalDialog(
            title = "Point(s) without periods", 
            easyClose = TRUE,
            renderDataTable(points.outside.periods.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Lengths without periods - dataframe ----
  lengths.outside.periods.t <- reactive({
    
    lengths <- length3dpoints.t() %>%
      dplyr::filter(period %in% c("NA", NA, NULL, "")) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, length_mm, frame_left, em_comment)
  })
  
  ## ► Lengths without periods - valueBox ----
  output$lengths.outside.periods.t <- renderValueBox({
    
    if (dim(lengths.outside.periods.t())[1] > 0) {
      total <- nrow(lengths.outside.periods.t())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4, 
             total, 
             "Length(s) or 3D point(s) outside periods", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Lengths without periods - onclick----
  onclick('click.lengths.outside.periods.t', 
          showModal(modalDialog(
            title = "Length(s) or 3D point(s) outside periods", 
            easyClose = TRUE,
            renderDataTable(lengths.outside.periods.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Average period time - dataframe ----
  periods.avg.t <- reactive({
    
    periods <- periods() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), time_start, time_end, has_end) %>%
      distinct() %>%
      mutate(period_time = round(time_end - time_start, digits = 2)) %>%
      replace_na(list(period_time = 0)) 
  })
  
  ## ► Average period time - valueBox ----
  output$periods.avg.t <- renderValueBox({
    
    average <- mean(periods.avg.t()$period_time)
    
    valueBox(width = 4, 
             round(average, digits = 2), 
             "Average period time (mins)", 
             icon = icon("question"), color = "blue"
    )
  })
  
  ## ► Average period time - onclick----
  onclick('click.periods.avg.t', 
          showModal(modalDialog(
            title = "Period times", 
            easyClose = TRUE,
            renderDataTable(periods.avg.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Samples without periods - dataframe ----
  samples.without.periods.t <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample))
    
    periods.samples <- periods() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct()
    
    missing.periods <- anti_join(metadata.samples, periods.samples) %>%
      dplyr::select(!sample)
  })
  
  ## ► Samples without periods - valueBox ----
  output$samples.without.periods.t <- renderValueBox({
    
    if (dim(samples.without.periods.t())[1] > 0) {
      total <- nrow(samples.without.periods.t())
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4, 
             total, 
             "Sample(s) missing in EMObs", 
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without periods - onclick----
  onclick('click.samples.without.periods.t', 
          showModal(modalDialog(
            title = "Sample(s) without periods", 
            easyClose = TRUE,
            renderDataTable(samples.without.periods.t(), rownames = FALSE, 
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## _______________________________________________________ ----
  ##                          MAXN                           ----
  ## _______________________________________________________ ----
  ## ► Read in points data ----
  points <- reactive({
    if(input$upload %in% "EM"){
      # When folder chosen ---
      if(!is.null(input$folderdir)) {
        
        # Get all _Period files in the folder
        files <- input$folderdir%>%
          dplyr::filter(grepl("_Points.txt", name)) #%>% glimpse() 
        
        points <- data.frame() 
        
        if (is.null(files)) return(NULL)
        
        for (i in seq_along(files$datapath)) {
          tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"))  %>%
            dplyr::mutate(campaignid = files$name[i])
          
          points <- bind_rows(points, tmp) # %>%glimpse()
        }
        
        points <- points %>%
          clean_names() %>%
          dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Points.txt" = ""))) #%>% glimpse()
        
        # If point method and samples are opcodes
        if(input$method == "point" & input$sample == "opcode") {
          
          points <- points %>%
            dplyr::mutate(sample = opcode)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          points <- points %>%
            dplyr::mutate(sample = period)
        }
        
        # If transect method and sample = "opcode" + "period"
        if(input$method == "transect" & input$sample.t == "opcodeperiod") {
          
          points <- points %>%
            dplyr::mutate(sample = paste(opcode, period, sep = "_"))
          
        }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          points <- points %>%
            dplyr::mutate(sample = period)
        }
      }
      
      # if no folder chosen and method = single point. dataset = Ningloo BRUVs
      if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
        
        points <-  read.delim("data/examples/2022-05_PtCloates_stereo-BRUVS_Points.txt", na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      } 
      
      # print("checking points 1")
      # print(unique(points$family))
      
      points <- points %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::mutate(species = as.character(tolower(species))) %>%
        dplyr::mutate(genus = as.character(ga.capitalise(genus))) %>%
        dplyr::mutate(family = as.character(ga.capitalise(family))) %>%
        dplyr::rename(em_comment = comment, period_time = periodtime)
    }
  })
  
  # ► Preview points ----
  #TODO change this to be points/count
  output$table.points <- renderDataTable({
    points()
  })  
  
  ## ► Create MaxN (Raw) ----
  maxn.raw <- reactive({
    
    # print("checking points")
    # print(unique(points()$family))
    
    maxn <- points() %>%
      dplyr::mutate(number = as.numeric(number)) %>%
      replace_na(list(family = "Unknown", genus = "Unknown", species = "spp")) %>% # remove any NAs in taxa name
      dplyr::group_by(campaignid, sample, filename, period_time, frame, family, genus, species) %>% # removed comment 21/10/21 removed code 02/08/23
      dplyr::summarise(maxn = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code 02/08/23
      dplyr::slice(which.max(maxn)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(maxn)) %>%
      # dplyr::select(-frame) %>%
      tidyr::replace_na(list(maxn = 0)) %>%
      dplyr::mutate(maxn = as.numeric(maxn)) %>%
      dplyr::filter(maxn > 0) %>%
      dplyr::inner_join(metadata.regions()) %>%
      dplyr::mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      dplyr::mutate(genus = ifelse(genus%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      dplyr::mutate(species = ifelse(species%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
      dplyr::filter(successful_count %in% c("Yes", "Y", "y", "yes")) %>%
      dplyr::mutate(species = as.character(species)) %>%
      dplyr::mutate(genus = as.character(genus)) %>%
      dplyr::mutate(family = as.character(family)) %>%
      filter(!family %in% c("Unknown")) #%>% glimpse() # Added 2023-08-01
    
    
    
  })
  
  maxn.clean <- reactive({
    
    # print("unique genus")
    
    # print(unique(maxn.raw()$genus)) %>% sort()
    
    maxn.clean <- dplyr::full_join(maxn.raw(), metadata.regions()) %>%
      dplyr::left_join(., synonyms()) %>% #by = c("family", "genus", "species"), 
      dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
      dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
      dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
      dplyr::slice(which.max(maxn)) %>%
      dplyr::ungroup() %>%
      as_tibble() 
  })
  
  ## ►  Create MaxN (Complete) -----
  maxn.complete <- reactive({
    if(input$upload %in% "EM"){
      maxn.complete <- maxn.clean() %>%
        dplyr::full_join(metadata.regions()) %>% 
        dplyr::select(c(campaignid, sample, family, genus, species, maxn)) %>% # removed code
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>% # removed code
        replace_na(list(maxn = 0)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
        dplyr::summarise(maxn = sum(maxn)) %>%
        dplyr::ungroup()
    }
  })
  
  ## ► Create filtered MaxN download -----
  maxn.complete.download <- reactive({
    if(input$upload %in% "EM"){
      maxn <- full_join(maxn.raw(), metadata.regions()) # can't use clean as have already changed synonyms
      
      if (input$error.synonyms == TRUE) {
        
        #print("1")
        maxn.complete <- dplyr::left_join(maxn, synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
          dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
          dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
          dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
          dplyr::slice(which.max(maxn)) %>%
          dplyr::ungroup() %>%
          dplyr::full_join(metadata.regions()) %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn)) %>% # removed code
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>% # removed code
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
          dplyr::summarise(maxn = sum(maxn)) %>%
          ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
          #glimpse()
      } 
      else{ 
       #print("2")
        maxn.complete <- maxn %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn)) %>% # removed code
          dplyr::full_join(metadata.regions()) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>% # removed code
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
          dplyr::summarise(maxn = sum(maxn)) %>%
          ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
          #glimpse()
      }
      
      maxn.complete <- maxn.complete
      
      species.out.of.area <- life.history.expanded() %>%
        anti_join(maxn.clean(), ., by = c("family", "genus", "species", "marine_region")) %>%
        distinct(family, genus, species, marine_region) %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))
      
      # If "Remove species not observed in the area before" = FALSE, keep species, TRUE = remove
      if (input$error.area == FALSE) {
        maxn.area <- maxn.complete
      } else { 
        maxn.area <- anti_join(maxn.complete, species.out.of.area)}
      
      # If "Remove extra columns" = TRUE
      if (input$error.extra.col == TRUE) {
        maxn.area <- maxn.area %>%
          dplyr::select(-c(zone, marine_region, scientific))}
      
      if (input$error.zeros == TRUE) {
        maxn.area <- maxn.area %>%
          dplyr::select(!sample)%>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything())
        
      } else { 
        
        maxn.area <- maxn.area %>%
          dplyr::filter(!maxn %in% 0) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, maxn)
        } # remove metadata columns # removed code
      
      print("final")
      
      maxn.area <- maxn.area %>%
        dplyr::filter(!family %in% c("", NA, NULL)) #%>% glimpse()
    }
  })
  
  ## ►  Species dropdown ----
  output$maxn.species.dropdown <- renderUI({
    
    if(input$upload %in% "EM"){
      df <- maxn.complete()
      
    } else {
      df <- count.complete()
    }
    
    options <- df %>%
      dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(n = sum(maxn)) %>%
      arrange(-n) %>%
      dplyr::mutate(scientific = paste(genus, " ", species, sep = "")) %>%
      distinct(scientific) %>%
      pull("scientific")
    
    create_dropdown("maxn.species.dropdown", options, NULL)
  })
  
  ## ►  Taxa replaced by synonym - dataframe -----
  maxn.synonym <- reactive({
    
    if(input$upload %in% "EM"){
      maxn <- full_join(maxn.raw(), metadata.regions())
    } else {
      maxn <- full_join(count.raw(), metadata.regions())
    }
    
    # print("synonym check")
    
    maxn.synonym <- dplyr::left_join(maxn, synonyms()) %>%
      dplyr::filter(!is.na(genus_correct)) %>%
      dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
      dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
      dplyr::select('old name', 'new name') %>%
      dplyr::distinct()
  })
  
  ## ► Taxa replaced by synonym - Valuebox ----
  output$maxn.synonym <- renderValueBox({
    
    maxn.synonym <- maxn.synonym() %>%
      dplyr::mutate(count = 1)
    
    if (dim(maxn.synonym)[1] > 0) {
      total <- sum(maxn.synonym$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Species names updated", 
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ►  Taxa replaced by synonym - download ----
  output$download.maxn.synonyms <- downloadHandler(
    filename = function() {
      paste("maxn.synonyms_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(maxn.synonym(), file, row.names = FALSE)
    }
  )
  
  ## ►  Taxa replaced by synonym - onclick ----
  onclick('click.maxn.synonym', showModal(modalDialog(
    title = "Species names that have been updated", size = "l", easyClose = TRUE, 
    downloadButton("download.maxn.synonyms", "Download as csv"), 
    renderDataTable(maxn.synonym(), rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ►  Total abundance - dataframe ----
  maxn.total.abundances <- reactive({
    
    if(input$upload %in% "EM"){
      
      maxn <- maxn.clean() %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(maxn)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), total_abundance)
      
    } else {
      
      maxn <- count.clean() %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(maxn)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), total_abundance)
      
    }
    
  })
  
  ## ►  Total abundance EM - value box ----
  output$maxn.total.number.em <- renderValueBox({
    total <- sum(maxn.total.abundances()$total_abundance)
    valueBox(total, 
             "Individuals observed", 
             icon = icon("fish"), color = "blue"
    )
  })
  
  ## ►  Total abundance EM - onclick ----
  onclick('click.maxn.total.number.em', 
          showModal(modalDialog(
            title = "Total abundance per sample", 
            size = "l", 
            easyClose = TRUE, 
            renderDataTable(maxn.total.abundances(),  
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))
  
  
  ## ►  Total abundance GEN - value box ----
  output$maxn.total.number.gen <- renderValueBox({
    # if(!input$upload %in% "EM"){
    total <- sum(maxn.total.abundances()$total_abundance)
    valueBox(total, 
             "Individuals observed", 
             icon = icon("fish"), color = "blue"
    )
    # }
  })
  
  ## ►  Total abundance - onclick ----
  onclick('click.maxn.total.number.gen', 
          showModal(modalDialog(
            title = "Total abundance per sample", 
            size = "l", 
            easyClose = TRUE, 
            renderDataTable(maxn.total.abundances(),  
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))
  
  ## ►  Points without a number - dataframe ----
  points.no.number <- reactive({
    points.no.number <- points() %>%
      filter(number %in% c("NA", NA, 0, NULL, "", " ")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, period_time, frame, em_comment)
  })
  
  ## ►  Points without a number - value box ----
  output$points.no.number <- renderValueBox({
    
    points.no.number <- points.no.number() %>%
      dplyr::mutate(count = 1)
    
    if (dim(points.no.number)[1] > 0) {
      total <- sum(points.no.number$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Point(s) without a number", 
             icon = icon("fish"), color = col
    )
  })
  
  ## ►  Points without a number - onclick ----
  onclick('click.points.no.number', 
          showModal(modalDialog(
            title = "Point(s) without a number", 
            size = "l", 
            easyClose = TRUE, 
            renderDataTable(points.no.number(),  
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))
  
  
  
  
  ## ► Species not observed in region- dataframe ----
  maxn.species.not.observed <- reactive({
    
    if(input$upload %in% "EM"){
      
      maxn <- dplyr::anti_join(maxn.clean(), life.history.expanded(), by = c("family", "genus", "species", "marine_region")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region) %>%
        dplyr::distinct() %>%
        dplyr::rename('marine region not observed in' = marine_region) %>%
        dplyr::semi_join(., life.history.expanded(), by = c("family", "genus", "species"))
      
    } else {
      
      maxn <- life.history.expanded() %>%
        dplyr::anti_join(count.clean(), ., by = c("family", "genus", "species", "marine_region")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region) %>%
        dplyr::distinct() %>%
        dplyr::rename('marine region not observed in' = marine_region) %>%
        dplyr::semi_join(., life.history.expanded(), by = c("family", "genus", "species"))
      
    }
    
  })
  
  ## ►  Species not observed in region - download ----
  output$download.maxn.species.not.observed <- downloadHandler(
    filename = function() {
      paste("maxn.species.not.observed_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(maxn.species.not.observed(), file, row.names = FALSE)
    }
  )
  
  ## ►  Species not observed in region - onclick ----
  onclick('click.maxn.species.not.observed', showModal(modalDialog(
    title = "Species not previously observed in the marine region", size = "l", easyClose = TRUE, 
    downloadButton("download.maxn.species.not.observed", "Download as csv"), 
    checkboxInput("maxn.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE), 
    checkboxInput("maxn.observed.distinct", label = "Show unique species per campaign", value = TRUE), 
    renderDataTable(
      if(input$maxn.filter.spp == TRUE & input$maxn.observed.distinct == TRUE)
        maxn.species.not.observed() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>% distinct(campaignid, family, genus, species)
      else if (input$maxn.filter.spp == TRUE & input$maxn.observed.distinct == FALSE)
        maxn.species.not.observed() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) 
      else if (input$maxn.filter.spp == FALSE & input$maxn.observed.distinct == TRUE)
        maxn.species.not.observed() %>% distinct(campaignid, family, genus, species)
      else
        maxn.species.not.observed(),  rownames = FALSE, 
      options = list(paging = FALSE, row.names = FALSE, searching = TRUE)))))
  
  ## ►  Species not observed in region - valuebox ----
  output$maxn.species.not.observed <- renderValueBox({
    maxn.species.not.observed <- maxn.species.not.observed() %>%
      distinct(family, genus, species) %>%
      mutate(scientific = paste(family, genus, species, sep = " "))
    
    if (dim(maxn.species.not.observed)[1] > 0) {
      total <- base::length(unique(maxn.species.not.observed$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Species not observed in area before", 
             icon = icon("map-marked"), color = col
    )
  })
  
  ## ► Species not observed in life history list - dataframe ----
  maxn.species.not.observed.lh <- reactive({
    
    if(input$upload %in% "EM"){
      
      # print("no match")
      
      # print(unique(maxn.clean()$family))
      
      maxn <- dplyr::anti_join(maxn.clean(), life.history.expanded(), by = c("family", "genus", "species")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species) %>%
        dplyr::distinct()
      
    } else {
      
      maxn <- dplyr::anti_join(count.clean(), life.history.expanded(), by = c("family", "genus", "species")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species) %>%
        dplyr::distinct()
      
    }
    
  })
  
  ## ►  Species not observed in life history list - download ----
  output$download.maxn.species.not.observed.lh <- downloadHandler(
    filename = function() {
      paste("maxn.species.not.observed.in.life-history_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(maxn.species.not.observed.lh(), file, row.names = FALSE)
    }
  )
  
  ## ►  Species not observed in life history list - onclick ----
  onclick('click.maxn.species.not.observed.lh', showModal(modalDialog(
    
    title = "Species not in the life history list", size = "l", easyClose = TRUE, 
    downloadButton("download.maxn.species.not.observed.lh", "Download as csv"), 
    checkboxInput("maxn.filter.spp.lh", label = "Filter out sp1, sp2, spp etc.", value = FALSE), 
    checkboxInput("maxn.observed.distinct.lh", label = "Show unique species per campaign", value = TRUE), 
    renderDataTable(
      if(input$maxn.filter.spp.lh == TRUE & input$maxn.observed.distinct.lh == TRUE)
        maxn.species.not.observed.lh() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>% distinct(campaignid, family, genus, species)
      else if (input$maxn.filter.spp.lh == TRUE & input$maxn.observed.distinct.lh == FALSE)
        maxn.species.not.observed.lh() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) 
      else if (input$maxn.filter.spp.lh == FALSE & input$maxn.observed.distinct.lh == TRUE)
        maxn.species.not.observed.lh() %>% distinct(campaignid, family, genus, species)
      else
        maxn.species.not.observed.lh(),  rownames = FALSE, 
      options = list(paging = FALSE, row.names = FALSE, searching = TRUE)))))
  
  ## ►  Species not observed in life history list - valuebox ----
  output$maxn.species.not.observed.lh <- renderValueBox({
    maxn.species.not.observed.lh <- maxn.species.not.observed.lh() %>%
      distinct(family, genus, species) %>%
      mutate(scientific = paste(family, genus, species, sep = " "))
    
    if (dim(maxn.species.not.observed.lh)[1] > 0) {
      total <- base::length(unique(maxn.species.not.observed.lh$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Species not in the life history list", 
             icon = icon("list"), color = col
    )
  })
  
  
  ## ► Spatial plot ----
  output$maxn.spatial.plot <- renderLeaflet({
    
    req(input$maxn.species.dropdown)
    
    if(input$upload %in% "EM"){
      
      maxn <- maxn.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific == input$maxn.species.dropdown) 
      
    } else {
      
      maxn <- count.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific == input$maxn.species.dropdown) 
      
    }
    
    map <- leaflet(maxn) %>%
      addTiles() %>%
      fitBounds(~min(longitude_dd), ~min(latitude_dd), ~max(longitude_dd), ~max(latitude_dd))
    
    overzero <- filter(maxn, maxn > 0)
    equalzero <- filter(maxn, maxn ==  0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd, 
          radius = ~((maxn/max(maxn))*15), fillOpacity = 0.5, stroke = FALSE, 
          label = ~as.character(maxn)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd, 
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE, 
          label = ~as.character(maxn)
        )
    }
    map
  })
  
  ## ►  Status plot ----
  output$maxn.status.plot <- renderPlot({
    
    if(input$upload %in% "EM"){
      
      maxn.per.sample <- maxn.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, status) %>%
        dplyr::summarise(maxn = sum(maxn))
      
    } else {
      
      maxn.per.sample <- count.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, status) %>%
        dplyr::summarise(maxn = sum(maxn))
      
    }
    
    scientific.name <- input$maxn.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(maxn.per.sample, aes(x = status, y = maxn, fill = status)) + 
      stat_summary(fun.y = mean, geom = "bar", colour = "black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept = 0))+
      xlab("Status")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## ►  Zone plot ----
  output$maxn.zone.simple <- renderPlot({
    
    if(input$upload %in% "EM"){
      
      maxn.per.sample.simple <- maxn.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, zone) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        ungroup()
      
    } else {
      
      maxn.per.sample.simple <- count.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, zone) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        ungroup()
      
    }
    
    scientific.name <- input$maxn.species.dropdown
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(maxn.per.sample.simple, aes(x = zone, y = maxn, fill = zone)) + 
      stat_summary(fun.y = mean, geom = "bar", colour = "black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept = 0))+
      xlab("Zone")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## ►  Location plot ----
  output$maxn.location.plot <- renderPlot({
    
    if(input$upload %in% "EM"){
      
      maxn.per.sample <- maxn.complete() %>%
        left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, location) %>%
        dplyr::summarise(maxn = sum(maxn))
      
    } else {
      
      maxn.per.sample <- count.complete() %>%
        left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, location) %>%
        dplyr::summarise(maxn = sum(maxn))
    }
    
    scientific.name <- input$maxn.species.dropdown
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(maxn.per.sample, aes(x = location, y = maxn)) + 
      stat_summary(fun.y = mean, geom = "bar", colour = "black", fill = "white") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept = 0))+
      xlab("Location")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      annotation_custom(grob.sci)+ 
      Theme1#+
    #ggtitle("Plot of abundance by Location")
  })
  
  ## ►  Site plot ----
  output$maxn.site.plot <- renderPlot({
    
    if(input$upload %in% "EM"){
      
      maxn.per.sample <- maxn.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, site) %>%
        dplyr::summarise(maxn = sum(maxn))
      
    } else {
      
      maxn.per.sample <- count.complete() %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
        dplyr::group_by(campaignid, sample, site) %>%
        dplyr::summarise(maxn = sum(maxn))
      
    }
    
    
    scientific.name <- input$maxn.species.dropdown
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(maxn.per.sample, aes(x = site, y = maxn)) + 
      stat_summary(fun.y = mean, geom = "bar", colour = "black", fill = "white") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept = 0))+
      xlab("Site")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## ►  top species ----
  output$maxn.top.species <- renderPlot({
    
    if(input$upload %in% "EM"){
      
      maxn.sum <- maxn.complete() %>%
        mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::group_by(scientific) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        dplyr::ungroup() %>%
        top_n(input$species.limit)
      
    } else {
      
      maxn.sum <- count.complete() %>%
        mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::group_by(scientific) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        dplyr::ungroup() %>%
        top_n(input$species.limit)
      
    }
    
    ## ►  Total frequency of occurrence ----
    ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      xlab("Species") +
      ylab(expression(Overall~abundance~(Sigma~MaxN))) +
      Theme1 +
      theme(axis.text.y = element_text(face = "italic")) +
      theme_collapse +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  })
  
  ## _______________________________________________________ ----
  ##                     LENGTH + 3D POINTS                  ----
  ## _______________________________________________________ ----
  
  ## ► Read in length data ----
  length <- reactive({
    if(input$upload %in% "EM"){
      # When folder chosen ---
      if(!is.null(input$folderdir)) {
        
        # Get all _Period files in the folder
        files <- input$folderdir%>%
          dplyr::filter(grepl("_Lengths.txt", name)) #%>% glimpse() 
        
        length <- data.frame() 
        
        if (is.null(files)) return(NULL)
        
        for (i in seq_along(files$datapath)) {
          tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"))  %>%
            dplyr::mutate(campaignid = files$name[i])
          
          length <- bind_rows(length, tmp) #%>% glimpse()
        }
        
        length <- length %>%
          clean_names() %>%
          dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Lengths.txt" = ""))) #%>% glimpse()
        
        # If point method and samples are opcodes
        if(input$method == "point" & input$sample == "opcode") {
          
          length <- length %>%
            dplyr::mutate(sample = opcode)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          length <- length %>%
            dplyr::mutate(sample = period)
        }
        
        # If transect method and sample = "opcode" + "period"
        if(input$method == "transect" & input$sample.t == "opcodeperiod") {
          
          length <- length %>%
            dplyr::mutate(sample = paste(opcode, period, sep = "_"))
          
        }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          length <- length %>%
            dplyr::mutate(sample = period)
        }
      }
      
      # if no folder chosen and method = single point. dataset = Ningloo BRUVs
      if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
        
        length <-  read.delim("data/examples/2022-05_PtCloates_stereo-BRUVS_Lengths.txt", na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      } 
      
      length <- length %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::mutate(species = as.character(tolower(species))) %>%
        dplyr::mutate(genus = as.character(ga.capitalise(genus))) %>%
        dplyr::mutate(family = as.character(ga.capitalise(family))) %>%
        dplyr::mutate(length_mm = as.numeric(length)) %>%
        dplyr::select(!length) %>%
        dplyr::mutate(rms = as.numeric(rms)) %>%
        dplyr::mutate(precision = as.numeric(precision)) %>%
        dplyr::mutate(range = as.numeric(range)) %>%
        dplyr::mutate(number = as.numeric(number)) %>%
        dplyr::rename(em_comment = comment,
                      frame_left = frameleft,
                      frame_right = frameright, 
                      period_time = periodtime)#%>% glimpse()
    }
  })
  
  # ► Preview length ----
  output$table.length <- renderDataTable({
    if(input$upload %in% "EM"){
    length() %>% dplyr::select(!sample)
    } else {
      gen.length()
    }
  })  
  
  ## ► Read in 3D points data ----
  threedpoints <- reactive({
    if(input$upload %in% "EM"){
      # When folder chosen ---
      if(!is.null(input$folderdir)) {
        
        # Get all _Period files in the folder
        files <- input$folderdir%>%
          dplyr::filter(grepl("_3DPoints.txt", name)) #%>% glimpse()
        
        threedpoints <- data.frame() 
        
        if (is.null(files)) return(NULL)
        
        for (i in seq_along(files$datapath)) {
          tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"))  %>%
            dplyr::mutate(campaignid = files$name[i])
          
          threedpoints <- bind_rows(threedpoints, tmp) #%>% glimpse()
        }
        
        threedpoints <- threedpoints %>%
          clean_names() %>%
          dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_3DPoints.txt" = ""))) #%>% glimpse()
        
        # If point method and samples are opcodes
        if(input$method == "point" & input$sample == "opcode") {
          
          threedpoints <- threedpoints %>%
            dplyr::mutate(sample = opcode)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          threedpoints <- threedpoints %>%
            dplyr::mutate(sample = period)
        }
        
        # If transect method and sample = "opcode" + "period"
        if(input$method == "transect" & input$sample.t == "opcodeperiod") {
          
          threedpoints <- threedpoints %>%
            dplyr::mutate(sample = paste(opcode, period, sep = "_"))
          
        }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          threedpoints <- threedpoints %>%
            dplyr::mutate(sample = period)
        }
      }
      
      # if no folder chosen and method = single point. dataset = Ningloo BRUVs
      if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
        
        threedpoints <-  read.delim("data/examples/2022-05_PtCloates_stereo-BRUVS_3DPoints.txt", na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      } 
      
      threedpoints <- threedpoints %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::mutate(species = as.character(tolower(species))) %>%
        dplyr::mutate(genus = as.character(ga.capitalise(genus))) %>%
        dplyr::mutate(family = as.character(ga.capitalise(family))) %>%
        dplyr::mutate(rms = as.numeric(rms)) %>%
        dplyr::mutate(range = as.numeric(range)) %>%
        dplyr::mutate(number = as.numeric(number)) %>%
        dplyr::rename(em_comment = comment,
                      frame_left = frameleft,
                      frame_right = frameright,
                      period_time = periodtime)
    }
  })
  
  # ► Preview 3D points ----
  output$table.3dpoints <- renderDataTable({
    threedpoints() %>% dplyr::select(!sample)
  }) 
  
  ## _______________________________________________________ ----
  ##              Lengths for single point campaigns         ----
  ## _______________________________________________________ ----
  
  # ► Combine lengths and 3d points ----
  length3dpoints <- reactive({
    
    #print("length og")
    
    length3dpoints <- length() %>%
      dplyr::bind_rows(threedpoints()) %>% # changed to bind_rows
      mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
      dplyr::select(-c(time)) %>%
      dplyr::mutate(sample = as.character(sample)) %>%
      dplyr::left_join(metadata.regions()) %>%
      filter(!family %in% c("Unknown")) #%>% glimpse()
  })
  
  length3dpoints.clean <- reactive({
    
    #print("view lengths")
    #glimpse(length3dpoints())
    
    #print("view synonyms")
    #glimpse(synonyms())
    
    length3dpoints.clean <-  dplyr::left_join(length3dpoints() %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), synonyms()) %>% #, by = c("family", "genus", "species")
      dplyr::mutate(genus = ifelse(!genus_correct %in% c(NA), genus_correct, genus)) %>%
      dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
      dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      # dplyr::filter(range < (input$range.limit * 1000)) %>%
      dplyr::full_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, rms, precision, code) %>%
      # dplyr::mutate(precision_percent = (precision/length) *100) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% # we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes"))
    
  })
  
  ## ► Create filtered length download -----
  length.complete.download <- reactive({
    
    if(input$upload %in% "EM"){
      #TODO add generic downloading
      # print("preview length data for downloading")
      length <- length3dpoints() # can't use clean as have already changed synonyms # glimpse()
      
      if (input$error.synonyms == TRUE) {
        length.complete <- dplyr::left_join(length3dpoints() %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
          dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
          dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
          dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
          dplyr::full_join(metadata.regions()) %>% # add in all samples
          dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, rms, precision, code) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
          dplyr::ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::filter(!is.na(family)) %>%
          dplyr::mutate(marine_region = as.character(marine_region)) %>%
          dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))
      } 
      else{ 
        length.complete <- dplyr::left_join(length3dpoints() %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::full_join(metadata.regions()) %>% # add in all samples
          dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, rms, precision, code) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
          dplyr::ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) %>%
          dplyr::filter(!is.na(family)) %>%
          dplyr::mutate(marine_region = as.character(marine_region))
      }
      
      length.complete <- length.complete %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " "))
    
      species.out.of.area <- life.history.expanded() %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        anti_join(length.complete, ., by = c("family", "genus", "species", "marine_region")) %>%
        distinct(family, genus, species, marine_region) %>%
        filter(!species %in% c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))
      
      if (input$error.area == FALSE) {
        length.area <- length.complete
      } 
      else{ 
        length.area <- anti_join(length.complete, species.out.of.area)
      }
      
      # Need to split these into 3D points and lengths to apply precision rule
      precision.limit <- input$error.precision.limit
      
      points <- length.area %>%
        dplyr::filter(is.na(length_mm)) %>%
        dplyr::filter(range < (input$error.range.limit*1000)) %>%
        dplyr::filter(rms < input$error.rms.limit)
      
      length.area <- length.area %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::filter(range < (input$error.range.limit*1000)) %>%
        dplyr::filter(rms < input$error.rms.limit) %>%
        dplyr::mutate(precision_percent = (precision/length_mm)*100) %>% # I THINK BECAUSE OF THIS LINE
        dplyr::filter(precision_percent < precision.limit) %>%
        dplyr::select(-c(precision_percent)) %>%
        bind_rows(points)
      
      # print("test 2 (area) for 3D points") # they are gone here
      # test <- length.area %>% dplyr::filter(is.na(length)) %>% filter(number > 0) %>% glimpse()
      
      length.wrong <- left_join(length.area %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), life.history.min.max(), by = c("family", "genus", "species")) %>%
        dplyr::filter(length_mm<min_length|length_mm>fb_length_max) %>%
        mutate(reason = ifelse(length_mm<min_length, "too small", "too big"))
      
      length.too.small <- length.wrong %>%
        dplyr::filter(reason%in%c("too small"))
      
      length.too.big <- length.wrong %>%
        dplyr::filter(reason%in%c("too big"))
      
      if (input$error.length.small == TRUE) {
        length.small <- anti_join(length.area, length.too.small)
      }
      else{
        length.small <- length.area
      }
      
      length.small <- length.small
      
      if (input$error.length.big == TRUE) {
        length.big <- anti_join(length.small, length.too.big)
      }
      else{
        length.big <- length.small
      }
      
      length.big <- length.big %>%
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, rms, precision, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>% 
        dplyr::left_join(metadata.regions()) %>%
        filter(!is.na(family)) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " "))
      
      # If "Remove extra columns" = TRUE
      if (input$error.extra.col == TRUE) {
        length.big <- length.big %>%
          dplyr::select(-c(zone, em_comment, marine_region, scientific, frame_left, frame_right))#%>% glimpse()
      } 
      
      if (input$error.zeros == TRUE) {
        length.big <- length.big %>%
          dplyr::select(!sample) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything())
        
      } else { 
        length.big <- length.big %>%
          dplyr::filter(!number %in% 0)%>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, number, range, rms, precision, code) # remove metadata columns
      }
      length.big <- length.big 
    }
  })
  
  ## ► Species dropdown ----
  output$length.species.dropdown <- renderUI({
    
    if(input$upload %in% "EM"){
      df <- length3dpoints.clean()
    } else {
      df <- gen.length.clean()
    }
    
    options <- df %>%
      dplyr::mutate(genus = ifelse(genus%in%c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(n = sum(number)) %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      distinct(scientific) %>%
      pull("scientific")
    
    create_dropdown("length.species.dropdown", options, NULL)
  })
  
  ## ► Number of lengths - dataframe ----
  length.abundance <- reactive({
    
    if(input$upload %in% "EM"){
      
      length <- length3dpoints.clean() %>%
        dplyr::filter(!length_mm %in% c(NA)) %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), total_abundance)
      
    } else {
      
      length <- gen.length.clean() %>%
        dplyr::filter(!length_mm %in% c(NA)) %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), total_abundance)
      
    }
  })
  
  ## ► Number of lengths EM - value box ----
  output$length.abundance.em <- renderValueBox({
    lengths <- sum(length.abundance()$total_abundance)
    
    
    if(input$upload %in% "EM"){
      threedpoints <- sum(threedpoints.abundance()$total_abundance)
      total <- lengths + threedpoints
      
      text <- paste0(lengths, " (", round((lengths/total)*100), "%)")
      wide <- 3
   
      valueBox(width = wide, 
               text, 
               "Length measurements", 
               icon = icon("ruler"), color = "blue")
      
    }
  })
  
  ## ► Number of lengths GEN - value box ----
  output$length.abundance.gen <- renderValueBox({
    lengths <- sum(length.abundance()$total_abundance)
    
    
    if(!input$upload %in% "EM"){
      text <- lengths
      wide <- 12
      
      valueBox(width = wide, 
               text, 
               "Length measurements", 
               icon = icon("ruler"), color = "blue")
    }
  })
  
  ## ► Number of lengths - onclick ----
  onclick('click.length.abundance.em', showModal(modalDialog(
    title = "Number of fish measured per sample", size = "l", easyClose = TRUE, 
    renderDataTable(length.abundance() %>% dplyr::select(!sample),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  onclick('click.length.abundance.gen', showModal(modalDialog(
    title = "Number of fish measured per sample", size = "l", easyClose = TRUE, 
    renderDataTable(length.abundance() %>% dplyr::select(!sample),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Number of 3d points - dataframe ----
  threedpoints.abundance <- reactive({
    threedpoints.abundance <- length3dpoints.clean() %>%
      dplyr::filter(length_mm %in% c(NA)) %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(number)) %>%
      dplyr::ungroup() %>%
      tidyr::replace_na(list(total_abundance = 0)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), total_abundance)
  })
  
  ## ► Number of 3d points - value box ----
  output$threedpoints.abundance <- renderValueBox({
    threedpoints.abundance <- threedpoints.abundance()
    lengths <- sum(length.abundance()$total_abundance)
    threedpoints <- sum(threedpoints.abundance()$total_abundance)
    total <- lengths + threedpoints
    
    text <- paste0(threedpoints, " (", round((threedpoints/total)*100), "%)")
    
    if (dim(threedpoints.abundance)[1] > 0) {
      total <- sum(threedpoints.abundance$total_abundance)
    }
    else{
      total = 0
    }
    
    valueBox(width = 3, 
             text, 
             "3D points", 
             icon = icon("dot-circle"), color = "blue"
    )
  })
  
  ## ► Number of 3d points - onclick ----
  onclick('click.threedpoints.abundance', showModal(modalDialog(
    title = "Number of 3D points per sample", size = "l", easyClose = TRUE, 
    renderDataTable(threedpoints.abundance() %>% dplyr::select(!sample),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ► Number of lengths vs. 3d points - dataframe ----
  length.v.3d <- reactive({
    if(input$upload %in% "EM"){
      #print("view 3D points")
      threedpoints <- threedpoints.abundance() %>%
        dplyr::mutate(type = "number_of_3D_points") #%>% glimpse()
      
      #print("XX Lengths")
      lengths <- length.abundance() %>%
        dplyr::mutate(type = "number_of_length_measurements") #%>% glimpse()
      
      total <- bind_rows(threedpoints, lengths) %>%
        tidyr::pivot_wider(names_from = type, values_from = total_abundance) %>%
        tidyr::replace_na(list(total_measurements = 0, number_of_3D_points = 0, number_of_length_measurements = 0)) %>%
        dplyr::mutate(total_measurements = number_of_3D_points + number_of_length_measurements) %>%
        dplyr::mutate(percent_length = round((number_of_length_measurements/total_measurements) * 100, 2)) %>%
        dplyr::filter(!(number_of_3D_points %in% 0 & number_of_length_measurements %in% 0))
        
    }
  })
  
  ## ► Number of lengths vs. 3d points - value box ----
  output$prop.lengths <- renderValueBox({
    length.v.3d <- length.v.3d()
    
    total.lengths <- sum(length.v.3d$number_of_length_measurements)
    total <- sum(length.v.3d$total_measurements)
    
    valueBox(width = 3, 
             round((total.lengths/total) * 100, 2), 
             "Percent of measurements that have length", 
             icon = icon("percent"), color = "blue"
    )
  })
  
  ## ► Number of lengths vs. 3d points - onclick ----
  onclick('click.prop.lengths', showModal(modalDialog(
    title = "Length Measurements VS. 3D points", size = "l", easyClose = TRUE, 
    renderDataTable(length.v.3d(),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  
  ## ► Number of lengths vs. 3d points ACTUAL NUMBERS - Plot ----
  output$length.vs.3d.plot <- renderPlot({

    if(input$upload %in% "EM"){
    
      # print("MAXN")
    maxn <- maxn.complete() %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(maxn)) %>%
      dplyr::ungroup() #%>%
      #dplyr::glimpse()
    
    # looks fine
    
    # print("MAXN missing")
    maxn.missing <- length.v.3d() %>%
      #glimpse() %>%
      full_join(maxn) %>% 
      dplyr::mutate(type = "MaxN not measured") %>%
      dplyr::mutate(total_abundance = total_abundance - total_measurements) #%>%
      #dplyr::glimpse()
    
    # print("too many measurements")
    too.many.measurements <- maxn.missing %>%
      dplyr::filter(total_abundance < 0) %>%
      dplyr::mutate(extra = TRUE) %>%
      dplyr::select(campaignid, sample, extra) #%>%
      #dplyr::glimpse()
    
    maxn.missing <- maxn.missing %>%
      dplyr::filter(total_abundance > 0) %>%
      full_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))  #%>%
      #dplyr::glimpse()
      
    threedpoints <- threedpoints.abundance() %>%
      dplyr::mutate(type = "3D points")
    
    lengths <- length.abundance() %>%
      dplyr::mutate(type = "Length Measurements")
    
    dat <- bind_rows(threedpoints, lengths, maxn.missing) %>%
      dplyr::select(campaignid, sample, total_abundance, type) %>%
      full_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
    
    totals <- dat %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(total_abundance)) %>%
      dplyr::ungroup() %>%
      left_join(too.many.measurements) %>%
      dplyr::filter(extra == TRUE) %>%
      dplyr::mutate(type = "")
    
    # print("TESTING ++++++++++++++++++++++++++")
    # glimpse(dat)
    
    dat$type <- fct_relevel(dat$type, 
                            "MaxN not measured",
                            "3D points",
                            "Length Measurements")
  
    plot <- ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
      geom_bar(position = "stack", stat = "identity") +
      geom_text(data = totals, aes(x = sample, y = total_abundance + 1, label = "*"), size = 12) +
      xlab("Sample") + ylab("Number of measurements") +
      Theme1 +
      scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                 "Length Measurements"= '#7CAE00', 
                                 "3D points" = '#619CFF')) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_y_continuous(expand = c(0, 0))
    
    } else {
      
      maxn <- count.complete() %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(maxn)) %>%
        dplyr::ungroup()
      
      maxn.missing <- length.abundance() %>%
        dplyr::rename(total_measurements = total_abundance) %>%
        full_join(maxn) %>% 
        dplyr::mutate(total_abundance = total_abundance - total_measurements)
      
      too.many.measurements <- maxn.missing %>%
        dplyr::filter(total_abundance < 0) %>%
        dplyr::mutate(extra = TRUE) %>%
        dplyr::select(campaignid, sample, extra)
      
      maxn.missing <- maxn.missing %>%
        dplyr::filter(total_abundance > 0) %>%
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))  %>%
        dplyr::mutate(type = "MaxN not measured")
      
      lengths <- length.abundance() %>%
        dplyr::mutate(type = "Length Measurements")
      
      dat <- bind_rows(lengths, maxn.missing) %>%
        dplyr::select(campaignid, sample, total_abundance, type) %>%
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
      
      totals <- dat %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(total_abundance)) %>%
        dplyr::ungroup() %>%
        left_join(too.many.measurements) %>%
        dplyr::filter(extra == TRUE) %>%
        dplyr::mutate(type = "")
      
      dat$type <- fct_relevel(dat$type, 
                              #"3D points",
                              "MaxN not measured",
                              "Length Measurements")
      
      plot <- ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
        geom_bar(position = "stack", stat = "identity") +
        geom_text(data = totals, aes(x = sample, y = total_abundance + 1, label = "*"), size = 12) +
        xlab("Sample") + ylab("Number of measurements") +
        Theme1 +
        scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                   "Length Measurements"= '#7CAE00')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_y_continuous(expand = c(0, 0))
      
    }
    
    if(input$length.vs.3d.plot.facet == TRUE){
      
      plot <- plot +
        facet_wrap(vars(observer_length), ncol = 1)
      
    }

    plot
    
  })
  
  ## ► Number of lengths vs. 3d points PROPORTION - Plot ----
  output$length.vs.3d.plot.prop <- renderPlot({
    
    if(input$upload %in% "EM"){
    maxn <- maxn.complete() %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(maxn)) %>%
      dplyr::ungroup()
    
    maxn.missing <- length.v.3d() %>%
      full_join(maxn) %>% 
      dplyr::mutate(type = "MaxN not measured") %>%
      dplyr::mutate(total_abundance = total_abundance - total_measurements) %>%
      full_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
    
    too.many.measurements <- maxn.missing %>%
      dplyr::filter(total_abundance < 0) %>%
      dplyr::mutate(extra = TRUE) %>%
      dplyr::select(campaignid, sample, extra)
    
    maxn.missing <- maxn.missing %>%
      dplyr::filter(total_abundance > 0)
    
    threedpoints <- threedpoints.abundance() %>%
      dplyr::mutate(type = "3D points")
    
    lengths <- length.abundance() %>%
      dplyr::mutate(type = "Length Measurements")
    
    dat <- bind_rows(threedpoints, lengths, maxn.missing) %>%
      dplyr::select(campaignid, sample, total_abundance, type) %>%
      full_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
    
    totals <- dat %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(total_abundance)) %>%
      dplyr::ungroup() %>%
      left_join(too.many.measurements) %>%
      dplyr::filter(extra == TRUE) %>%
      dplyr::mutate(type = "")
    
    dat$type <- fct_relevel(dat$type, 
                            "MaxN not measured",
                            "3D points",
                            "Length Measurements")
    
    plot <- ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
      geom_bar(position = "fill", stat = "identity") +
      geom_text(data = totals, aes(x = sample, y = 1, label = "*"), size = 12) +
      xlab("Sample") + ylab("Number of measurements") +
      Theme1 +
      scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                 "Length Measurements"= '#7CAE00', 
                                 "3D points" = '#619CFF')) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_y_continuous(expand = c(0, 0))
    } else {
      
      maxn <- count.complete() %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(maxn)) %>%
        dplyr::ungroup()
      
      maxn.missing <- length.abundance() %>%
        dplyr::rename(total_measurements = total_abundance) %>%
        full_join(maxn) %>% 
        dplyr::mutate(total_abundance = total_abundance - total_measurements)%>%
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
      
      too.many.measurements <- maxn.missing %>%
        dplyr::filter(total_abundance < 0) %>%
        dplyr::mutate(extra = TRUE) %>%
        dplyr::select(campaignid, sample, extra)
      
      maxn.missing <- maxn.missing %>%
        dplyr::filter(total_abundance > 0) %>%
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))  %>%
        dplyr::mutate(type = "MaxN not measured")
      
      lengths <- length.abundance() %>%
        dplyr::mutate(type = "Length Measurements")
      
      dat <- bind_rows(lengths, maxn.missing) %>%
        dplyr::select(campaignid, sample, total_abundance, type) %>%
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) 
      
      totals <- dat %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(total_abundance)) %>%
        dplyr::ungroup() %>%
        left_join(too.many.measurements) %>%
        dplyr::filter(extra == TRUE) %>%
        dplyr::mutate(type = "")
      
      dat$type <- fct_relevel(dat$type, 
                              "MaxN not measured",
                              "Length Measurements")
      
      plot <- ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
        geom_bar(position = "fill", stat = "identity") +
        geom_text(data = totals, aes(x = sample, y = 1, label = "*"), size = 12) +
        xlab("Sample") + ylab("Number of measurements") +
        Theme1 +
        scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                   "Length Measurements"= '#7CAE00')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(expand = c(0, 0))
      
    }
    
    if(input$length.vs.3d.plot.prop.facet == TRUE){
      
      plot <- plot +
        facet_wrap(vars(observer_length), ncol = 1)
      
    }
    
    plot
    
  })
  
  ## ► Number of lengths vs. 3d points Species Numbers - Plot ----
  output$length.vs.3d.species.plot.stack <- renderPlot({

    if(input$upload %in% "EM"){
    
      maxn <- maxn.complete() %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::mutate(total_abundance = maxn) %>%
        dplyr::select(campaignid, sample, total_abundance) %>%
        dplyr::mutate(sample = as.character(sample)) #%>% glimpse()
      
      lengths <- length3dpoints.clean() %>%
        dplyr::filter(!length_mm %in% c(NA)) %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::ungroup() %>%
        full_join(metadata.regions()) %>%
        replace_na(list(total_abundance = 0)) %>%
        dplyr::mutate(type = "Length Measurements") %>%
        dplyr::mutate(calc = "length.measurements") %>% 
        dplyr::select(campaignid, sample, total_abundance, calc, type) # %>% glimpse()
      
      threedpoints <- length3dpoints.clean() %>%
        dplyr::filter(length_mm %in% c(NA)) %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::ungroup() %>%
        full_join(metadata.regions()) %>%
        replace_na(list(total_abundance = 0)) %>%
        dplyr::mutate(type = "3D points") %>%
        dplyr::mutate(calc = "points") %>%
        dplyr::select(campaignid, sample, total_abundance, calc, type)  #%>% glimpse()
      
      # test <- threedpoints %>% filter(sample %in% "3.02") #%>% glimpse()
      
      maxn.missing <- bind_rows(threedpoints, lengths) %>%
        dplyr::select(-c(type)) %>%
        tidyr::pivot_wider(names_from = calc, values_from = total_abundance) %>%
        dplyr::mutate(measurements = points + length.measurements) %>%
        dplyr::select(campaignid, sample, measurements) %>%
        full_join(maxn) %>%
        replace_na(list(total_abundance = 0, measurements = 0)) %>%
        dplyr::mutate(type = "MaxN not measured") %>%
        dplyr::mutate(total_abundance = total_abundance - measurements) %>%
        distinct() %>% 
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) #%>% glimpse()
        
      too.many.measurements <- maxn.missing %>%
        dplyr::filter(total_abundance < 0) %>%
        dplyr::mutate(extra = TRUE) %>%
        dplyr::select(campaignid, sample, extra) #%>% glimpse()
      
      maxn.missing <- maxn.missing %>%
        dplyr::filter(total_abundance > 0) #%>% glimpse()
      
      dat <- bind_rows(threedpoints, lengths, maxn.missing) %>% distinct() 
      
      totals <- dat %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(total_abundance)) %>%
        dplyr::ungroup() %>%
        left_join(too.many.measurements) %>%
        dplyr::filter(extra == TRUE) %>%
        dplyr::mutate(type = "") %>% distinct() 
      
      dat$type <- fct_relevel(dat$type, 
                              "MaxN not measured",
                              "3D points",
                              "Length Measurements")
      
      ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
        geom_bar(position = "stack", stat = "identity") +
        geom_text(data = totals, aes(x = sample, y = total_abundance + 1, label = "*"), size = 12) +
        xlab("Sample") + ylab("Number of measurements") +
        Theme1 +
        scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                   "Length Measurements"= '#7CAE00', 
                                   "3D points" = '#619CFF')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(expand = c(0, 0))
      
    } else {
      
      maxn <- count.complete() %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::mutate(total_abundance = maxn) %>%
        dplyr::select(campaignid, sample, total_abundance) %>%
        dplyr::mutate(sample = as.character(sample)) #%>% glimpse()
      
      lengths <- gen.length.clean() %>%
        dplyr::filter(!length_mm %in% c(NA)) %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::ungroup() %>%
        full_join(metadata.regions()) %>%
        replace_na(list(total_abundance = 0)) %>%
        dplyr::mutate(type = "Length Measurements") %>%
        dplyr::mutate(calc = "length.measurements") %>% 
        dplyr::select(campaignid, sample, total_abundance, calc, type) # %>% glimpse()
      
      maxn.missing <- lengths %>%
        dplyr::select(-c(type)) %>%
        tidyr::pivot_wider(names_from = calc, values_from = total_abundance) %>%
        dplyr::rename(measurements = length.measurements) %>%
        dplyr::select(campaignid, sample, measurements) %>%
        full_join(maxn) %>%
        replace_na(list(total_abundance = 0, measurements = 0)) %>%
        dplyr::mutate(type = "MaxN not measured") %>%
        dplyr::mutate(total_abundance = total_abundance - measurements) %>%
        distinct() %>% 
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) #%>% glimpse()
      
      too.many.measurements <- maxn.missing %>%
        dplyr::filter(total_abundance < 0) %>%
        dplyr::mutate(extra = TRUE) %>%
        dplyr::select(campaignid, sample, extra) #%>% glimpse()
      
      maxn.missing <- maxn.missing %>%
        dplyr::filter(total_abundance > 0) #%>% glimpse()
      
      dat <- bind_rows(lengths, maxn.missing) %>% distinct() 
      
      totals <- dat %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(total_abundance)) %>%
        dplyr::ungroup() %>%
        left_join(too.many.measurements) %>%
        dplyr::filter(extra == TRUE) %>%
        dplyr::mutate(type = "") %>% distinct() 
      
      dat$type <- fct_relevel(dat$type, 
                              "MaxN not measured",
                              # "3D points",
                              "Length Measurements")
      
      ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
        geom_bar(position = "stack", stat = "identity") +
        geom_text(data = totals, aes(x = sample, y = total_abundance + 1, label = "*"), size = 12) +
        xlab("Sample") + ylab("Number of measurements") +
        Theme1 +
        scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                   "Length Measurements"= '#7CAE00')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(expand = c(0, 0))
      
    }
    
  })
  
  ## ► Number of lengths vs. 3d points PROPORTION - Plot ----
  output$length.vs.3d.species.plot.prop <- renderPlot({

    if(input$upload %in% "EM"){
    
    maxn <- maxn.complete() %>%
      dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
      dplyr::mutate(total_abundance = maxn) %>%
      dplyr::select(campaignid, sample, total_abundance) %>%
      dplyr::mutate(sample = as.character(sample)) #%>% glimpse()
    
    lengths <- length3dpoints.clean() %>%
      dplyr::filter(!length_mm %in% c(NA)) %>%
      dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>%
      dplyr::summarise(total_abundance = sum(number)) %>%
      dplyr::ungroup() %>%
      full_join(metadata.regions()) %>%
      replace_na(list(total_abundance = 0)) %>%
      dplyr::mutate(type = "Length Measurements") %>%
      dplyr::mutate(calc = "length.measurements") %>% 
      dplyr::select(campaignid, sample, total_abundance, calc, type) # %>% glimpse()
    
    threedpoints <- length3dpoints.clean() %>%
      dplyr::filter(length_mm %in% c(NA)) %>%
      dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>%
      dplyr::summarise(total_abundance = sum(number)) %>%
      dplyr::ungroup() %>%
      full_join(metadata.regions()) %>%
      replace_na(list(total_abundance = 0)) %>%
      dplyr::mutate(type = "3D points") %>%
      dplyr::mutate(calc = "points") %>%
      dplyr::select(campaignid, sample, total_abundance, calc, type)  #%>% glimpse()
    
    maxn.missing <- bind_rows(threedpoints, lengths) %>%
      dplyr::select(-c(type)) %>%
      tidyr::pivot_wider(names_from = calc, values_from = total_abundance) %>%
      dplyr::mutate(measurements = points + length.measurements) %>%
      dplyr::select(campaignid, sample, measurements) %>%
      full_join(maxn) %>%
      replace_na(list(total_abundance = 0, measurements = 0)) %>%
      dplyr::mutate(type = "MaxN not measured") %>%
      dplyr::mutate(total_abundance = total_abundance - measurements) %>%
      distinct() %>% 
      full_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) #%>% glimpse()
    
    too.many.measurements <- maxn.missing %>%
      dplyr::filter(total_abundance < 0) %>%
      dplyr::mutate(extra = TRUE) %>%
      dplyr::select(campaignid, sample, extra) #%>% glimpse()
    
    maxn.missing <- maxn.missing %>%
      dplyr::filter(total_abundance > 0) #%>% glimpse()
    
    dat <- bind_rows(threedpoints, lengths, maxn.missing) %>% distinct() 
    
    totals <- dat %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(total_abundance)) %>%
      dplyr::ungroup() %>%
      left_join(too.many.measurements) %>%
      dplyr::filter(extra == TRUE) %>%
      dplyr::mutate(type = "") %>% distinct() 
    
    dat$type <- fct_relevel(dat$type, 
                            "MaxN not measured",
                            "3D points",
                            "Length Measurements")
    
    ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
      geom_bar(position = "fill", stat = "identity") +
      geom_text(data = totals, aes(x = sample, y = 1, label = "*"), size = 12) +
      xlab("Sample") + ylab("Number of measurements") +
      Theme1 +
      scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                 "Length Measurements"= '#7CAE00', 
                                 "3D points" = '#619CFF')) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
      scale_y_continuous(expand = c(0, 0))
    
    } else {
      
      maxn <- count.complete() %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::mutate(total_abundance = maxn) %>%
        dplyr::select(campaignid, sample, total_abundance) %>%
        dplyr::mutate(sample = as.character(sample)) #%>% glimpse()
      
      lengths <- gen.length.clean() %>%
        dplyr::filter(!length_mm %in% c(NA)) %>%
        dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(total_abundance = sum(number)) %>%
        dplyr::ungroup() %>%
        full_join(metadata.regions()) %>%
        replace_na(list(total_abundance = 0)) %>%
        dplyr::mutate(type = "Length Measurements") %>%
        dplyr::mutate(calc = "length.measurements") %>% 
        dplyr::select(campaignid, sample, total_abundance, calc, type) # %>% glimpse()
      
      maxn.missing <- lengths %>%
        dplyr::select(-c(type)) %>%
        tidyr::pivot_wider(names_from = calc, values_from = total_abundance) %>%
        dplyr::mutate(measurements = length.measurements) %>%
        dplyr::select(campaignid, sample, measurements) %>%
        full_join(maxn) %>%
        replace_na(list(total_abundance = 0, measurements = 0)) %>%
        dplyr::mutate(type = "MaxN not measured") %>%
        dplyr::mutate(total_abundance = total_abundance - measurements) %>%
        distinct() %>% 
        full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) #%>% glimpse()
      
      too.many.measurements <- maxn.missing %>%
        dplyr::filter(total_abundance < 0) %>%
        dplyr::mutate(extra = TRUE) %>%
        dplyr::select(campaignid, sample, extra) #%>% glimpse()
      
      maxn.missing <- maxn.missing %>%
        dplyr::filter(total_abundance > 0) #%>% glimpse()
      
      dat <- bind_rows(lengths, maxn.missing) %>% distinct() 
      
      totals <- dat %>%
        dplyr::group_by(campaignid, sample) %>%
        dplyr::summarise(total_abundance = sum(total_abundance)) %>%
        dplyr::ungroup() %>%
        left_join(too.many.measurements) %>%
        dplyr::filter(extra == TRUE) %>%
        dplyr::mutate(type = "") %>% distinct() 
      
      dat$type <- fct_relevel(dat$type, 
                              "MaxN not measured",
                              # "3D points",
                              "Length Measurements")
      
      ggplot(dat, aes(fill = type, y = total_abundance, x = sample)) + 
        geom_bar(position = "fill", stat = "identity") +
        geom_text(data = totals, aes(x = sample, y = 1, label = "*"), size = 12) +
        xlab("Sample") + ylab("Number of measurements") +
        Theme1 +
        scale_fill_manual(values=c("MaxN not measured" = '#F8766D', 
                                   "Length Measurements"= '#7CAE00')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
        scale_y_continuous(expand = c(0, 0))
      
      
    }
    
  })
  
  ## ►  Lengths without a number - dataframe ----
  lengths.no.number <- reactive({
    lengths.no.number <- length() %>%
      dplyr::filter(number %in% c("NA", NA, 0, NULL, "", " ")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, length_mm, period_time,  frame_left, em_comment, rms, precision, code)
  })
  
  ## ►  Lengths without a number - value box ----
  output$lengths.no.number <- renderValueBox({
    
    lengths.no.number <- lengths.no.number() %>%
      dplyr::mutate(count = 1)
    
    if (dim(lengths.no.number)[1] > 0) {
      total <- sum(lengths.no.number$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "Length(s) without a number", 
             icon = icon("fish"), color = col
    )
  })
  
  ## ►  Lengths without a number - onclick ----
  onclick('click.lengths.no.number', 
          showModal(modalDialog(
            title = "Length(s) without a number", 
            size = "l", 
            easyClose = TRUE, 
            renderDataTable(lengths.no.number(),  
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))
  
  ## ►  3d points without a number - dataframe ----
  threedpoints.no.number <- reactive({
    threedpoints.no.number <- threedpoints() %>%
      dplyr::filter(number %in% c("NA", NA, 0, NULL, "", " ")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, period_time, frame_left, em_comment)
  })
  
  ## ►  3d points without a number - value box ----
  output$threedpoints.no.number <- renderValueBox({
    
    threedpoints.no.number <- threedpoints.no.number() %>%
      dplyr::mutate(count = 1)
    
    if (dim(threedpoints.no.number)[1] > 0) {
      total <- sum(threedpoints.no.number$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 2, 
             total, 
             "3D point(s) without a number", 
             icon = icon("fish"), color = col
    )
  })
  
  ## ►  3d points without a number - onclick ----
  onclick('click.threedpoints.no.number', 
          showModal(modalDialog(
            title = "3D points(s) without a number", 
            size = "l", 
            easyClose = TRUE, 
            renderDataTable(threedpoints.no.number(),  
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))
  
  
  ## ► Taxa replaced by synonym - dataframe -----
  length.synonym <- reactive({
    
    if(input$upload %in% "EM"){
      length <- length3dpoints()
    } else {
      length <- gen.length()
    }
    
    length.synonym <- dplyr::left_join(length %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), synonyms()) %>% #, by = c("family", "genus", "species")
      dplyr::filter(!is.na(genus_correct)) %>%
      dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
      dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
      dplyr::select('old name', 'new name') %>%
      dplyr::distinct()
  })
  
  ## ► Taxa replaced by synonym - Valuebox ----
  output$length.synonym <- renderValueBox({
    
    length.synonym <- length.synonym() %>%
      dplyr::rename(old.name = 'old name') %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.synonym)[1] > 0) {
      total <- sum(length.synonym$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Species names updated", 
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Taxa replaced by synonym - download ----
  output$download.length.synonyms <- downloadHandler(
    filename = function() {
      paste("length.synonyms", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(length.synonym(), file, row.names = FALSE)
    }
  )
  
  ## ► Taxa replaced by synonym - onclick ----
  onclick('click.length.synonym', showModal(modalDialog(
    title = "Species names that have been updated", size = "l", easyClose = TRUE, 
    downloadButton("download.length.synonyms", "Download as csv"), 
    renderDataTable(length.synonym(),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species not observed in the region - dataframe ----
  length.species.not.observed <- reactive({
    
    if(input$upload %in% "EM"){
      
      length <- life.history.expanded() %>%
        anti_join(length3dpoints.clean() %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), ., by = c("family", "genus", "species", "marine_region")) %>%
        filter(number > 0) %>%
        distinct(campaignid, sample, family, genus, species, marine_region) %>% # use this line to show specific drops OR
        dplyr::rename('marine region not observed in' = marine_region) %>%
        # filter(!species%in%c("spp"))%>% # %>% # Ignore spp in the report 
        mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        filter(!family %in% c("Unknown")) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region) %>%
        dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)) %>%
        dplyr::semi_join(., life.history.expanded(), by = c("family", "genus", "species"))
      
    } else {
      
      length <- life.history.expanded() %>%
        anti_join(gen.length.clean() %>% dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)), ., by = c("family", "genus", "species", "marine_region")) %>%
        filter(number > 0) %>%
        distinct(campaignid, sample, family, genus, species, marine_region) %>% # use this line to show specific drops OR
        dplyr::rename('marine region not observed in' = marine_region) %>%
        # filter(!species%in%c("spp"))%>% # %>% # Ignore spp in the report 
        mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        filter(!family %in% c("Unknown")) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region)%>%
        dplyr::mutate(family = as.character(family), genus = as.character(genus), species = as.character(species)) %>%
        dplyr::semi_join(. , life.history.expanded(), by = c("family", "genus", "species"))
      
    }
  })
  
  ## ► Species not observed  in the region- onclick ----
  onclick('click.length.species.not.observed', showModal(modalDialog(
    title = "Species not previously observed in the marine region", size = "l", easyClose = TRUE, 
    checkboxInput("length.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE), 
    checkboxInput("length.observed.distinct", label = "Show unique species per campaign", value = TRUE), 
    renderDataTable(
      if(input$length.filter.spp == TRUE & input$length.observed.distinct == TRUE)
        length.species.not.observed() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>% distinct(campaignid, family, genus, species)
      else if (input$length.filter.spp == TRUE & input$length.observed.distinct == FALSE)
        length.species.not.observed() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) 
      else if (input$length.filter.spp == FALSE & input$length.observed.distinct == TRUE)
        length.species.not.observed() %>% distinct(campaignid, family, genus, species)
      else
        length.species.not.observed(),  rownames = FALSE, 
      options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species not observed  in the region- valuebox ----
  output$length.species.not.observed <- renderValueBox({
    length.species.not.observed <- length.species.not.observed() %>%
      mutate(scientific = paste(family, genus, species, sep = " ")) %>%
      distinct(family, genus, species, scientific)
    
    if (dim(length.species.not.observed)[1] > 0) {
      total <- base::length(unique(length.species.not.observed$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Species not observed in area before", 
             icon = icon("map-marked"), color = col
    )
  })
  
  ## ► Species not observed in the life history - dataframe ----
  length.species.not.observed.lh <- reactive({
    
    if(input$upload %in% "EM"){
      
      length <- life.history.expanded() %>%
        anti_join(length3dpoints.clean(), ., by = c("family", "genus", "species")) %>%
        filter(number > 0) %>%
        distinct(campaignid, sample, family, genus, species) %>% # use this line to show specific drops OR
        mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        filter(!family %in% c("Unknown")) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species)
      
    } else {
      
      length <- life.history.expanded() %>%
        anti_join(gen.length.clean(), ., by = c("family", "genus", "species")) %>%
        filter(number > 0) %>%
        distinct(campaignid, sample, family, genus, species) %>% # use this line to show specific drops OR
        mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        filter(!family %in% c("Unknown")) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species)
      
    }
  })
  
  ## ► Species not observed in the life history - onclick ----
  onclick('click.length.species.not.observed.lh', showModal(modalDialog(
    title = "Species not in the life history list", size = "l", easyClose = TRUE, 
    checkboxInput("length.filter.spp.lh", label = "Filter out sp1, sp2, spp etc.", value = FALSE), 
    checkboxInput("length.observed.distinct.lh", label = "Show unique species per campaign", value = TRUE), 
    renderDataTable(
      if(input$length.filter.spp.lh == TRUE & input$length.observed.distinct.lh == TRUE)
        length.species.not.observed.lh() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>% distinct(campaignid, family, genus, species)
      else if (input$length.filter.spp.lh == TRUE & input$length.observed.distinct.lh == FALSE)
        length.species.not.observed.lh() %>% filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) 
      else if (input$length.filter.spp.lh == FALSE & input$length.observed.distinct.lh == TRUE)
        length.species.not.observed.lh() %>% distinct(campaignid, family, genus, species)
      else
        length.species.not.observed.lh(),  rownames = FALSE, 
      options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species not observed in the life history - valuebox ----
  output$length.species.not.observed.lh <- renderValueBox({
    length.species.not.observed.lh <- length.species.not.observed.lh() %>%
      mutate(scientific = paste(family, genus, species, sep = " ")) %>%
      distinct(family, genus, species, scientific)
    
    if (dim(length.species.not.observed.lh)[1] > 0) {
      total <- base::length(unique(length.species.not.observed.lh$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Species not in the life history list", 
             icon = icon("list"), color = col
    )
  })
  
  ## ► Species wrong length - dataframe ----
  length.wrong <- reactive({
    
    if(input$upload %in% "EM"){
      
      length.wrong <- left_join(length3dpoints.clean(), life.history.min.max(), by = c("family", "genus", "species")) %>%
        dplyr::filter(length_mm<min_length|length_mm>max_length) %>%
        mutate(reason = ifelse(length_mm<min_length, "too small", "too big")) %>%
        dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, fb_length_max, reason, em_comment, frame_left) %>%
        mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
        dplyr::mutate(percent.of.fb.max = (length_mm/fb_length_max*100)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, fb_length_max, reason, em_comment, frame_left)
      
    } else {
      
      length.wrong <- left_join(gen.length.clean(), life.history.min.max(), by = c("family", "genus", "species")) %>%
        dplyr::filter(length_mm<min_length|length_mm>max_length) %>%
        mutate(reason = ifelse(length_mm<min_length, "too small", "too big")) %>%
        dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, fb_length_max, reason) %>%
        mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
        dplyr::mutate(percent.of.fb.max = (length_mm/fb_length_max*100)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, fb_length_max, reason)
        #glimpse()
      
    }
    
  })
  
  ## ► Species wrong length small - valuebox ----
  output$length.wrong.small <- renderValueBox({
    length.wrong.small <- length.wrong() %>%
      dplyr::filter(reason%in%c("too small")) %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.wrong.small)[1] > 0) {
      total <- sum(length.wrong.small$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Lengths smaller than 15% of max", 
             icon = icon("less-than"), color = col
    )
  })
  
  ## ► Species wrong length small - download ----
  output$download.length.wrong.small <- downloadHandler(
    filename = function() {
      paste("length.15.percent.of.max", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(dplyr::filter(length.wrong(), reason%in%c("too small")), file, row.names = FALSE)
    }
  )
  
  ## ► Species wrong length small - onclick ----
  onclick('click.length.wrong.small', showModal(modalDialog(
    title = "Length measurements smaller than 15% of the fishbase maximum", size = "l", easyClose = TRUE, 
    downloadButton("download.length.wrong.small", "Download as csv"), 
    renderDataTable(filter(length.wrong()%>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                           '85%_fb_maximum_length' = max_length,
                                                           'fb_maximum_length' = fb_length_max), reason == "too small"), rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species wrong length big - valuebox ----
  output$length.wrong.big <- renderValueBox({
    length.wrong.big <- length.wrong() %>%
      dplyr::filter(reason%in%c("too big")) %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.wrong.big)[1] > 0) {
      total <- sum(length.wrong.big$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Lengths bigger than 85% of max", 
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Species wrong length big - download ----
  output$download.length.wrong.big <- downloadHandler(
    filename = function() {
      paste("length.85.percent.of.max", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(dplyr::filter(length.wrong(), reason%in%c("too big")), file, row.names = FALSE)
    }
  )
  
  ## ► Species wrong length big - onclick ----
  onclick('click.length.wrong.big', showModal(modalDialog(
    title = "Length measurements bigger than 85% of the fishbase maximum", size = "l", easyClose = TRUE, 
    downloadButton("download.length.wrong.big", "Download as csv"), 
    renderDataTable(filter(length.wrong()%>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                           '85%_fb_maximum_length' = max_length,
                                                           'fb_maximum_length' = fb_length_max), reason == "too big"), rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species wrong length bigger than 100% - valuebox ----
  output$length.wrong.big.100 <- renderValueBox({
    length.wrong.big <- length.wrong() %>%
      dplyr::filter(reason %in% c("too big")) %>%
      dplyr::filter(fb_length_max < length_mm) %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.wrong.big)[1] > 0) {
      total <- sum(length.wrong.big$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Lengths bigger than 100% of max", 
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Species wrong length bigger than 100% - download ----
  output$download.length.wrong.big.100 <- downloadHandler(
    filename = function() {
      paste("length.100.percent.of.max", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(dplyr::filter(length.wrong(), fb_length_max < length_mm), file, row.names = FALSE)
    }
  )
  
  ## ► Species wrong length bigger than 100% - onclick ----
  onclick('click.length.wrong.big.100', showModal(modalDialog(
    title = "Length measurements bigger than 100% of the fishbase maximum", size = "l", easyClose = TRUE, 
    downloadButton("download.length.wrong.big.100", "Download as csv"), 
    renderDataTable(filter(length.wrong()%>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                           '85%_fb_maximum_length' = max_length,
                                                           'fb_maximum_length' = fb_length_max), fb_maximum_length < length_mm), rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ► Out of range - dataframe ----
  length.out.of.range <- reactive({
    req(input$range.limit)
    
    range.limit <- (input$range.limit*1000)
    
    length.out.of.range <- length3dpoints() %>%
      dplyr::filter(range > range.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, frame_left, frame_right, em_comment, rms, precision, code)
  })
  
  ## ► Out of range - valuebox ----
  output$length.out.of.range <- renderValueBox({
    length.out.of.range <- length.out.of.range() %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.out.of.range)[1] > 0) {
      total <- sum(length.out.of.range$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Out of range", 
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Out of range - onclick ----
  onclick('click.length.out.of.range', showModal(modalDialog(
    title = "Length measurement(s) out of range", size = "l", easyClose = TRUE, 
    renderDataTable(length.out.of.range(),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Over RMS - dataframe ----
  length.wrong.rms <- reactive({
    rms.limit <- (input$rms.limit)
    
    length.wrong.rms <- length3dpoints() %>%
      dplyr::filter(rms > rms.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)
  })
  
  ## ► Over RMS - valuebox ----
  output$length.wrong.rms <- renderValueBox({
    length.wrong.rms <- length.wrong.rms() %>%
      dplyr::mutate(count = 1)
    
    lengths <- length.wrong.rms %>%
      dplyr::filter(length_mm > 0)
    
    if (dim(length.wrong.rms)[1] > 0) {
      total <- sum(length.wrong.rms$count)
      
      # If any RMS errors are lengths then make the box red otherwise make the box orange (only 3D points)
      if(sum(lengths$count) > 0){
        col = "red"
      } else {
        col = "yellow"
      }
      

    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Measurements over RMS limit", 
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over RMS - onclick ----
  onclick('click.length.wrong.rms', showModal(modalDialog(
    title = "Length measurement(s) over RMS limit", size = "l", easyClose = TRUE, 
    renderDataTable(length.wrong.rms(),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Over precision - dataframe ----
  length.wrong.precision <- reactive({
    precision.limit <- (input$precision.limit)
    
    length.wrong.precision <- length3dpoints() %>%
      dplyr::mutate(precision_percent = (precision/length_mm)*100) %>%
      dplyr::filter(precision_percent > precision.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, precision_percent, code)
  })
  
  ## ► Over precision - valuebox ----
  output$length.wrong.precision <- renderValueBox({
    length.wrong.precision <- length.wrong.precision() %>%
      dplyr::mutate(count = 1)
    
    if (dim(length.wrong.precision)[1] > 0) {
      total <- sum(length.wrong.precision$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3, 
             total, 
             "Measurements over Precision limit", 
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over precision - onclick ----
  onclick('click.length.wrong.precision', showModal(modalDialog(
    title = "Length measurement(s) over precision limit", size = "l", easyClose = TRUE, 
    renderDataTable(length.wrong.precision(),  rownames = FALSE, 
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Histogram ----
  output$length.histogram <- renderPlot({
    req(input$length.species.dropdown)
    
    if(input$upload %in% "EM"){
      
      length3dpoints <- length3dpoints.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    } else {
      
      length3dpoints <- gen.length.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        dplyr::filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    }
    
    sizes <- life.history.min.max() %>%
      dplyr::mutate(scientific = paste(genus, species, sep  = " ")) %>%
      dplyr::filter(scientific %in% c(input$length.species.dropdown)) %>%
      dplyr::distinct(scientific, fb_length_max, min_length, max_length)
    
    fishbase.max <- sum(sizes$fb_length_max)
    min.15 <- sum(sizes$min_length)
    max.85 <- sum(sizes$max_length)
    
    scientific.name <- input$length.species.dropdown
    #common.name <- unique(maxn_species_data()$australian.common.name)
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(length3dpoints, aes(x = length_mm), col = "black", alpha = 0.5)+
      geom_histogram(alpha = 0.5, position = "identity", binwidth = input$length.binwidth, col = "black")+
      xlab("Length (mm)") + ylab("Count") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      geom_vline(xintercept = fishbase.max, colour = "red", size = 1) +
      geom_vline(xintercept = min.15, colour = "grey", size = 1) +
      geom_vline(xintercept = max.85, colour = "grey", size = 1) +
      geom_text(aes(x = fishbase.max, label = "\n  Fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      geom_text(aes(x = min.15, label = "\n  15% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      geom_text(aes(x = max.85, label = "\n  85% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#3c8dbc"))+
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## ► Histogram status ----
  output$length.histogram.status <- renderPlot({
    req(input$length.species.dropdown)
    
    if(input$upload %in% "EM"){
      
      length3dpoints <- length3dpoints.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    } else {
      
      length3dpoints <- gen.length.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    }
    
    sizes <- life.history.min.max() %>%
      mutate(scientific = paste(genus, species, sep  = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown)) %>%
      distinct(scientific, fb_length_max, min_length, max_length)
    
    fishbase.max <- sum(sizes$fb_length_max)
    min.15 <- sum(sizes$min_length)
    max.85 <- sum(sizes$max_length)
    
    scientific.name <- input$length.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(length3dpoints, aes(x = length_mm), col = "black", alpha = 0.5)+
      geom_histogram(alpha = 0.5, position = "identity", binwidth = input$length.binwidth, col = "black")+
      xlab("Length (mm)") + ylab("Count") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      geom_vline(xintercept = fishbase.max, colour = "red", size = 1) +
      geom_vline(xintercept = min.15, colour = "grey", size = 1) +
      geom_vline(xintercept = max.85, colour = "grey", size = 1) +
      geom_text(aes(x = fishbase.max, label = "\n  Fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      geom_text(aes(x = min.15, label = "\n  15% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      geom_text(aes(x = max.85, label = "\n  85% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 15), hjust = 0, vjust = 0)+
      annotation_custom(grob.sci)+ 
      Theme1+ 
      facet_wrap(vars(status), ncol = 1)
    
  })
  
  ## ► Species plot - zone ----
  output$length.status.plot <- renderPlot({
    req(input$length.species.dropdown)
    
    if(input$upload %in% "EM"){
      
      length3dpoints <- length3dpoints.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown))
      
    } else {
      
      length3dpoints <- gen.length.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown))
      
    }
    
    scientific.name <- input$length.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(length3dpoints, aes(x = factor(zone), y = length_mm,  fill = zone, notch = FALSE, outlier.shape = NA), alpha = 0.5) + 
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      xlab("Zone") + ylab("Length (mm)") +
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## ► Species plot - status ----
  output$length.zone.plot <- renderPlot({
    req(input$length.species.dropdown)
    if(input$upload %in% "EM"){
      
      length3dpoints <- length3dpoints.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    } else {
      
      length3dpoints <- gen.length.clean() %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        filter(scientific %in% c(input$length.species.dropdown)) %>%
        replace_na(list(status = "Fished"))
      
    }
    
    scientific.name <- input$length.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
    
    ggplot(length3dpoints, aes(x = factor(status), y = length_mm,  fill = status, notch = FALSE, outlier.shape = NA), alpha = 0.5) + 
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
      xlab("Status") + ylab("Length (mm)") +
      annotation_custom(grob.sci)+ 
      Theme1
  })
  
  ## _______________________________________________________ ----
  ##                Lengths for transect campaigns           ----
  ## _______________________________________________________ ----

  threedpoints.t <- reactive({
    threedpoints <- threedpoints()
  })

  # ► Combine lengths and 3d points ----
  length3dpoints.t <- reactive({
    length3dpoints <- length() %>%
      plyr::rbind.fill(threedpoints.t()) %>%
      mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      mutate(genus = ifelse(genus%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      mutate(species = ifelse(species%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
      dplyr::filter(!is.na(number)) %>%
      tidyr::replace_na(list(species = "spp")) %>%
      dplyr::select(-c(time)) %>%
      dplyr::mutate(sample = as.character(sample)) %>%
      dplyr::left_join(metadata.regions()) %>%
      filter(!family %in% c("Unknown"))
  })

  length3dpoints.clean.t <- reactive({

    length3dpoints.clean <-  dplyr::left_join(length3dpoints.t(), synonyms()) %>% #, synonyms(), by = c("family", "genus", "species")
      dplyr::mutate(genus = ifelse(!genus_correct %in% c(NA), genus_correct, genus)) %>%
      dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
      dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::filter(range < (input$range.limit.t * 1000)) %>%
      dplyr::full_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, rms, x, y, z, midx, midy, midz, em_comment, rms, precision, code) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))

  })

  ## ► Create filtered length download -----
  length.complete.download.t <- reactive({

    length <- length3dpoints.t() #%>% glimpse() # can't use clean as have already changed synonyms

    if (input$error.synonyms.t == TRUE) {
      length.complete <- dplyr::left_join(length3dpoints.t(), synonyms()) %>% #, by = c("family", "genus", "species")
        dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
        dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
        dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
        dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
        dplyr::full_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, midx, midy, x, y, rms, precision, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))

    } else {

      length.complete <- dplyr::left_join(length3dpoints.t(), synonyms()) %>% # , by = c("family", "genus", "species")
        dplyr::full_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, midx, midy, x, y, rms, precision, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(family)) %>%
        # dplyr::mutate(length = as.numeric(length)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes")) %>%
        dplyr::mutate(marine_region = as.character(marine_region))
    }

    print("length complete")

    length.complete <- length.complete %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) 

    species.out.of.area <- life.history.expanded() %>%
      dplyr::mutate(marine_region = as.character(marine_region)) %>%
      anti_join(length.complete, ., by = c("family", "genus", "species", "marine_region")) %>%
      distinct(family, genus, species, marine_region) %>%
      filter(!species %in% c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))

    if (input$error.area.t == FALSE) {
      length.area <- length.complete
    } else {
      length.area <- anti_join(length.complete, species.out.of.area)
    }

    length.area <- length.area %>%
      dplyr::mutate(x = as.numeric(x),
                    y = as.numeric(y),
                    midx = as.numeric(midx),
                    midy = as.numeric(midy)
    )

    transect.limit <- (input$error.transect.limit.t*1000)/2

    out.of.transect <- length.area %>%
      tidyr::replace_na(list(x = 0, y = 0, midx = 0, midy = 0)) %>%
      dplyr::filter(c(midx > transect.limit | midx < -transect.limit | midy > transect.limit | midy < -transect.limit | x > transect.limit | x < -transect.limit | y > transect.limit | y < -transect.limit))

    length.area <- length.area %>%
      dplyr::filter(range < (input$error.range.limit.t * 1000)) %>%
      anti_join(., out.of.transect) %>%
      dplyr::filter(rms < input$error.rms.limit.t) %>%
      dplyr::mutate(precision_percent = (precision/length_mm)*100) %>%
      dplyr::filter(precision_percent < input$error.precision.limit.t) %>%
      dplyr::select(-c(precision_percent))

    print("length wrong")

    length.wrong <- left_join(length.area, life.history.min.max(), by = c("family", "genus", "species")) %>%
      dplyr::filter(length_mm < min_length | length_mm > fb_length_max) %>%
      mutate(reason = ifelse(length_mm < min_length, "too small", "too big"))

    length.too.small <- length.wrong %>%
      dplyr::filter(reason %in% c("too small"))

    length.too.big <- length.wrong %>%
      dplyr::filter(reason %in% c("too big"))

    if (input$error.length.small.t == TRUE) {

      length.small <- anti_join(length.area, length.too.small)

    } else {

      length.small <- length.area

    }

    length.small <- length.small

    if (input$error.length.big.t == TRUE) {

      length.big <- anti_join(length.small, length.too.big)

    } else {
      length.big <- length.small
    }

    length.big <- length.big %>%
      dplyr::right_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, frame_left, frame_right, em_comment, rms, precision, code) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>%
      dplyr::left_join(metadata.regions()) %>%
      filter(!is.na(family)) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " "))

    # If "Remove extra columns" = TRUE
    if (input$error.extra.col.t == TRUE) {
      length.big <- length.big %>%
        dplyr::select(-c(zone, em_comment, marine_region, scientific, frame_left, frame_right))
    }

    if (input$error.zeros.t == TRUE) {

      length.big <- length.big %>%
        dplyr::select(!sample) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything())

    } else {
      length.big <- length.big %>%
        filter(!number %in% 0) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, number, range, rms, precision, code)
      }

    length.big <- length.big


  })


  ## ► Species dropdown ----
  output$length.species.dropdown.t <- renderUI({
    df <- length3dpoints.clean.t()

    options <- df %>%
      dplyr::mutate(genus = ifelse(genus%in%c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(n = sum(number)) %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      distinct(scientific) %>%
      pull("scientific")

    create_dropdown("length.species.dropdown.t", options, NULL)
  })

  ## ► Number of lengths EM - dataframe ----
  length.abundance.t <- reactive({
    length <- length3dpoints.clean.t()%>%
      dplyr::filter(!length_mm%in%c(NA)) %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(number)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), total_abundance)
  })

  ## ► Number of lengths EM - value box ----
  output$length.abundance.t.em <- renderValueBox({
    lengths <- sum(length.abundance.t()$total_abundance)
    threedpoints <- sum(threedpoints.abundance.t()$total_abundance)

    total <- lengths + threedpoints

    text <- paste0(lengths, " (", round((lengths/total)*100), "%)")

    # total <- sum(length.abundance.t()$total_abundance)
    valueBox(width = 3,
             text,
             "Length measurements",
             icon = icon("ruler"), color = "blue"
    )
  })

  ## ► Number of lengths - onclick ----
  onclick('click.length.abundance.t.em', showModal(modalDialog(
    title = "Number of fish measured per sample", size = "l", easyClose = TRUE,
    renderDataTable(length.abundance.t(),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))

  #TODO if adding generic need to add this for Generic

  ## ► Number of 3d points - dataframe ----
  threedpoints.abundance.t <- reactive({
    threedpoints.abundance <- length3dpoints.clean.t() %>%
      dplyr::filter(length_mm %in% c(NA)) %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(total_abundance = sum(number)) %>%
      dplyr::ungroup() %>%
      tidyr::replace_na(list(total_abundance = 0)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), total_abundance)
  })

  ## ► Number of 3d points - value box ----
  output$threedpoints.abundance.t <- renderValueBox({
    threedpoints.abundance <- threedpoints.abundance.t()

    lengths <- sum(length.abundance.t()$total_abundance)
    threedpoints <- sum(threedpoints.abundance.t()$total_abundance)

    total <- lengths + threedpoints

    if (dim(threedpoints.abundance)[1] > 0) {
      text <- paste0(threedpoints, " (", round((threedpoints/total)*100), "%)")
    }
    else{
      text <- 0
    }

    valueBox(width = 3,
             text,
             "3D points",
             icon = icon("dot-circle"), color = "blue"
    )
  })

  ## ► Number of 3d points - onclick ----
  onclick('click.threedpoints.abundance.t', showModal(modalDialog(
    title = "Number of 3D points per sample", size = "l", easyClose = TRUE,
    renderDataTable(threedpoints.abundance.t(),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))


  ## ►  Lengths without a number - dataframe ----
  lengths.no.number.t <- reactive({
    lengths.no.number.t <- length() %>%
      filter(number %in% c("NA", NA, 0, NULL, "", " ")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, length_mm, period_time,  frame_left, em_comment, rms, precision, code)
  })

  ## ►  Lengths without a number - value box ----
  output$lengths.no.number.t <- renderValueBox({

    lengths.no.number.t <- lengths.no.number.t() %>%
      dplyr::mutate(count = 1)

    if (dim(lengths.no.number.t)[1] > 0) {
      total <- sum(lengths.no.number.t$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 2,
             total,
             "Length(s) without a number",
             icon = icon("fish"), color = col
    )
  })

  ## ►  Lengths without a number - onclick ----
  onclick('click.lengths.no.number.t',
          showModal(modalDialog(
            title = "Length(s) without a number",
            size = "l",
            easyClose = TRUE,
            renderDataTable(lengths.no.number.t(),
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))

  ## ►  3d points without a number - dataframe ----
  threedpoints.no.number.t <- reactive({
    threedpoints.no.number.t <- threedpoints() %>%
      filter(number %in% c("NA", NA, 0, NULL, "", " ")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, period_time, frame_left, em_comment)
  })

  ## ►  3d points without a number - value box ----
  output$threedpoints.no.number.t <- renderValueBox({

    threedpoints.no.number.t <- threedpoints.no.number.t() %>%
      dplyr::mutate(count = 1)

    if (dim(threedpoints.no.number.t)[1] > 0) {
      total <- sum(threedpoints.no.number.t$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 2,
             total,
             "3D point(s) without a number",
             icon = icon("fish"), color = col
    )
  })

  ## ►  3d points without a number - onclick ----
  onclick('click.threedpoints.no.number.t',
          showModal(modalDialog(
            title = "3D points(s) without a number",
            size = "l",
            easyClose = TRUE,
            renderDataTable(threedpoints.no.number.t(),
                            options = list(paging = FALSE, row.names = FALSE, searching = FALSE)))))

  ## ► Taxa replaced by synonym - dataframe -----
  length.synonym.t <- reactive({
    length <- length3dpoints.t()

    length.synonym <- dplyr::left_join(length, synonyms()) %>%  #, by = c("family", "genus", "species")
      dplyr::filter(!is.na(genus_correct)) %>%
      dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
      dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
      dplyr::select('old name', 'new name') %>%
      dplyr::distinct()
  })

  ## ► Taxa replaced by synonym - Valuebox ----
  output$length.synonym.t <- renderValueBox({

    length.synonym <- length.synonym.t() %>%
      dplyr::rename(old.name = 'old name') %>%
      dplyr::mutate(count = 1)

    if (dim(length.synonym)[1] > 0) {
      total <- sum(length.synonym$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Species names updated",
             icon = icon("exclamation-circle"), color = col
    )
  })

  ## ► Taxa replaced by synonym - download ----
  output$download.length.synonyms.t <- downloadHandler(
    filename = function() {
      paste("length.synonyms", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(length.synonym(), file, row.names = FALSE)
    }
  )

  ## ► Taxa replaced by synonym - onclick ----
  onclick('click.length.synonym.t', showModal(modalDialog(
    title = "Species names that have been updated", size = "l", easyClose = TRUE,
    downloadButton("download.length.synonyms", "Download as csv"),
    renderDataTable(length.synonym.t(),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))

  ## ► Species not observed in region - dataframe ----
  length.species.not.observed.t <- reactive({
    length <- life.history.expanded() %>%
      anti_join(length3dpoints.clean.t(), ., by = c("family", "genus", "species", "marine_region")) %>%
      filter(number > 0) %>%
      distinct(campaignid, sample, family, genus, species, marine_region) %>% # use this line to show specific drops OR
      dplyr::rename('marine region not observed in' = marine_region) %>%
      filter(!species%in%c("spp")) %>% # %>% # Ignore spp in the report
      mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      filter(!family %in% c("Unknown")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, marine_region)

  })



  ## ► Species not observed in region - onclick ----
  onclick('click.length.species.not.observed.t', showModal(modalDialog(
    title = "Species not previously observed in the marine region", size = "l", easyClose = TRUE,
    checkboxInput("length.filter.spp.t", label = "Filter out sp1, sp2, spp etc.", value = FALSE),
    checkboxInput("length.observed.distinct.t", label = "Show unique species per campaign", value = TRUE),
    renderDataTable(
      if(input$length.filter.spp.t == TRUE & input$length.observed.distinct.t == TRUE)
        length.species.not.observed.t() %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp")) %>%
        distinct(campaignid, family, genus, species)

      else if (input$length.filter.spp.t == TRUE & input$length.observed.distinct.t == FALSE)

        length.species.not.observed.t() %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp"))

      else if (input$length.filter.spp.t == FALSE & input$length.observed.distinct.t == TRUE)
        length.species.not.observed.t() %>%
        # filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp")) %>%
        distinct(campaignid, family, genus, species)

      else length.species.not.observed.t(),

      rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))))

  ## ► Species not observed in region  - valuebox ----
  output$length.species.not.observed.t <- renderValueBox({
    length.species.not.observed <- length.species.not.observed.t() %>%
      mutate(scientific = paste(family, genus, species, sep = " ")) %>%
      distinct(family, genus, species, scientific)

    if (dim(length.species.not.observed)[1] > 0) {
      total <- base::length(unique(length.species.not.observed$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Species not observed in area before",
             icon = icon("map-marked"), color = col
    )
  })

  ## ► Species not observed in life history - dataframe ----
  length.species.not.observed.t.lh <- reactive({
    length <- life.history.expanded() %>%
      anti_join(length3dpoints.clean.t(), ., by = c("family", "genus", "species")) %>%
      filter(number > 0) %>%
      distinct(campaignid, sample, family, genus, species) %>% # use this line to show specific drops OR
      mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      filter(!family %in% c("Unknown")) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species)

  })

  ## ► Species not observed in life history - onclick ----
  onclick('click.length.species.not.observed.t.lh', showModal(modalDialog(
    title = "Species not in the life history sheet", size = "l", easyClose = TRUE,
    checkboxInput("length.filter.spp.t.lh", label = "Filter out sp1, sp2, spp etc.", value = FALSE),
    checkboxInput("length.observed.distinct.t.lh", label = "Show unique species per campaign", value = TRUE),
    renderDataTable(
      if(input$length.filter.spp.t.lh == TRUE & input$length.observed.distinct.t.lh == TRUE)
        length.species.not.observed.t.lh() %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp")) %>%
        distinct(campaignid, family, genus, species)

      else if (input$length.filter.spp.t.lh == TRUE & input$length.observed.distinct.t.lh == FALSE)

        length.species.not.observed.t.lh() %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp"))

      else if (input$length.filter.spp.t.lh == FALSE & input$length.observed.distinct.t.lh == TRUE)
        length.species.not.observed.t.lh() %>%
        # filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "sp", "spp")) %>%
        distinct(campaignid, family, genus, species)

      else length.species.not.observed.t.lh(),

      rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))))

  ## ► Species not observed in life history  - valuebox ----
  output$length.species.not.observed.t.lh <- renderValueBox({
    length.species.not.observed.lh <- length.species.not.observed.t.lh() %>%
      mutate(scientific = paste(family, genus, species, sep = " ")) %>%
      distinct(family, genus, species, scientific)

    if (dim(length.species.not.observed.lh)[1] > 0) {
      total <- base::length(unique(length.species.not.observed.lh$scientific))
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Species not in life history sheet",
             icon = icon("list"), color = col
    )
  })

  ## ► Species wrong length - dataframe ----
  length.wrong.t <- reactive({
    length.wrong <- left_join(length3dpoints.clean.t(), life.history.min.max(), by = c("family", "genus", "species")) %>%
      dplyr::filter(length_mm < min_length | length_mm > max_length) %>%
      mutate(reason = ifelse(length_mm < min_length, "too small", "too big")) %>%
      dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, fb_length_max, reason, frame_left, rms, precision) %>% # , code
      mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
      dplyr::mutate(percent.of.fb.max = (length_mm/fb_length_max*100)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, fb_length_max, reason, frame_left, rms, precision)

  })

  ## ► Species wrong length small - valuebox ----
  output$length.wrong.small.t <- renderValueBox({
    length.wrong.small <- length.wrong.t() %>%
      dplyr::filter(reason%in%c("too small")) %>%
      dplyr::mutate(count = 1)

    if (dim(length.wrong.small)[1] > 0) {
      total <- sum(length.wrong.small$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Lengths smaller than 15% of max",
             icon = icon("less-than"), color = col
    )
  })

  ## ► Species wrong length small - download ----
  output$download.length.wrong.small.t <- downloadHandler(
    filename = function() {
      paste("length.15.percent.of.max", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dplyr::filter(length.wrong.t(), reason%in%c("too small")), file, row.names = FALSE)
    }
  )

  ## ► Species wrong length small - onclick ----
  onclick('click.length.wrong.small.t', showModal(modalDialog(
    title = "Length measurements smaller than 15% of the fishbase maximum", size = "l", easyClose = TRUE,
    downloadButton("download.length.wrong.small.t", "Download as csv"),
    renderDataTable(filter(length.wrong.t() %>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                              '85%_fb_maximum_length' = max_length,
                                                              'fb_maximum_length' = fb_length_max), 
                           reason == "too small"), rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))

  ## ► Species wrong length big - valuebox ----
  output$length.wrong.big.t <- renderValueBox({
    length.wrong.big <- length.wrong.t() %>%
      dplyr::filter(reason%in%c("too big")) %>%
      dplyr::mutate(count = 1)

    if (dim(length.wrong.big)[1] > 0) {
      total <- sum(length.wrong.big$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Lengths bigger than 85% of max",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Species wrong length big - download ----
  output$download.length.wrong.big.t <- downloadHandler(
    filename = function() {
      paste("length.85.percent.of.max", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dplyr::filter(length.wrong.t(), reason %in% c("too big")), file, row.names = FALSE)
    }
  )

  ## ► Species wrong length big - onclick ----
  onclick('click.length.wrong.big.t',
          showModal(modalDialog(
            title = "Length measurements bigger than 85% of the fishbase maximum", size = "l",
            easyClose = TRUE,
            downloadButton("download.length.wrong.big.t", "Download as csv"),
            renderDataTable(filter(length.wrong.t()%>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                                     '85%_fb_maximum_length' = max_length,
                                                                     'fb_maximum_length' = fb_length_max), reason == "too big"), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))))

  ## ► Species wrong length 100% too big - valuebox ----
  output$length.wrong.big.100.t <- renderValueBox({
    length.wrong.big <- length.wrong.t() %>%
      dplyr::filter(reason %in% c("too big")) %>%
      dplyr::filter(fb_length_max < length_mm) %>%
      dplyr::mutate(count = 1)

    if (dim(length.wrong.big)[1] > 0) {
      total <- sum(length.wrong.big$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Lengths bigger than 100% of max",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Species wrong length 100% too big - download ----
  output$download.length.wrong.big.100.t <- downloadHandler(
    filename = function() {
      paste("length.100.percent.of.max", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dplyr::filter(length.wrong.t(), fb_length_max < length_mm), file, row.names = FALSE)
    }
  )

  ## ► Species wrong length 100% too big - onclick ----
  onclick('click.length.wrong.big.100.t',
          showModal(modalDialog(
            title = "Length measurements bigger than 100% of the fishbase maximum", size = "l",
            easyClose = TRUE,
            downloadButton("download.length.wrong.big.100.t", "Download as csv"),
            renderDataTable(filter(length.wrong.t()%>% dplyr::rename('15%_fb_maximum_length' = min_length,
                                                                     '85%_fb_maximum_length' = max_length,
                                                                     'fb_maximum_length' = fb_length_max), fb_maximum_length < length_mm), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))))

  ## ► Out of range - dataframe ----
  length.out.of.range.t <- reactive({
    req(input$range.limit.t)

    range.limit <- (input$range.limit.t*1000)

    length.out.of.range <- length3dpoints.t() %>%
      dplyr::filter(range > range.limit)  %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, length_mm,  frame_left, frame_right, em_comment, rms, precision, code)
  })

  ## ► Out of range - valuebox ----
  output$length.out.of.range.t <- renderValueBox({
    length.out.of.range <- length.out.of.range.t() %>%
      dplyr::mutate(count = 1)

    if (dim(length.out.of.range)[1] > 0) {
      total <- sum(length.out.of.range$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Out of range",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Out of range - onclick ----
  onclick('click.length.out.of.range.t',
          showModal(modalDialog(
            title = "Length measurement(s) and 3D point(s) out of range", size = "l", easyClose = TRUE,
            renderDataTable(length.out.of.range.t(),  rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))))

  ## ► Out of transect - dataframe ----
  length.out.of.transect.t <- reactive({
    req(input$transect.limit.t)

    transect.limit <- (input$transect.limit.t*1000)/2

    length.out.of.transect <- length3dpoints.t() %>%
      dplyr::mutate(x = as.numeric(x),
                    y = as.numeric(y),
                    z = as.numeric(z),
                    midx = as.numeric(midx),
                    midy = as.numeric(midy),
                    midz = as.numeric(midz)
                    ) %>%
      tidyr::replace_na(list(x = 0, y = 0, midx = 0, midy = 0)) %>%
      dplyr::filter(c(midx > transect.limit | midx < -transect.limit | midy > transect.limit | midy < -transect.limit | x > transect.limit | x < -transect.limit | y > transect.limit | y < -transect.limit)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, length_mm, frame_left, frame_right, midx, midy, x, y, em_comment, rms, precision, code)
  })

  ## ► Out of transect - valuebox ----
  output$length.out.of.transect.t <- renderValueBox({
    length.out.of.transect <- length.out.of.transect.t() %>%
      dplyr::mutate(count = 1)

    if (dim(length.out.of.transect)[1] > 0) {
      total <- sum(length.out.of.transect$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Out of transect",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Out of transect - onclick ----
  onclick('click.length.out.of.transect.t',
          showModal(modalDialog(
            title = "Length measurement(s) and 3D point(s) out of transect", size = "l", easyClose = TRUE,
            renderDataTable(length.out.of.transect.t(),  rownames = FALSE,
                            options = list(paging = TRUE, searching = TRUE)))))

  ## ► Over RMS - dataframe ----
  length.wrong.rms.t <- reactive({
    rms.limit <- (input$rms.limit.t)

    length.wrong.rms.t <- length3dpoints.t() %>%
      dplyr::filter(rms > rms.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)
  })

  ## ► Over RMS - valuebox ----
  output$length.wrong.rms.t <- renderValueBox({
    length.wrong.rms.t <- length.wrong.rms.t() %>%
      dplyr::mutate(count = 1)

    lengths <- length.wrong.rms.t %>%
      dplyr::filter(length_mm > 0)

    if (dim(length.wrong.rms.t)[1] > 0) {
      total <- sum(length.wrong.rms.t$count)
      # If any RMS errors are lengths then make the box red otherwise make the box orange (only 3D points)
      if(sum(lengths$count) > 0){
        col = "red"
      } else {
        col = "yellow"
      }
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width =3,
             total,
             "Measurements over RMS limit",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Over RMS - onclick ----
  onclick('click.length.wrong.rms.t', showModal(modalDialog(
    title = "Length measurement(s) over RMS limit", size = "l", easyClose = TRUE,
    renderDataTable(length.wrong.rms.t(),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))

  ## ► Over precision - dataframe ----
  length.wrong.precision.t <- reactive({
    precision.limit <- (input$precision.limit.t)

    length.wrong.precision.t <- length3dpoints() %>%
      dplyr::mutate(precision_percent = (precision/length_mm)*100) %>%
      dplyr::filter(precision_percent > precision.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, precision_percent, code)
  })

  ## ► Over precision - valuebox ----
  output$length.wrong.precision.t <- renderValueBox({
    length.wrong.precision.t <- length.wrong.precision.t() %>%
      dplyr::mutate(count = 1)

    if (dim(length.wrong.precision.t)[1] > 0) {
      total <- sum(length.wrong.precision.t$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }

    valueBox(width = 3,
             total,
             "Measurements over Precision limit",
             icon = icon("greater-than"), color = col
    )
  })

  ## ► Over precision - onclick ----
  onclick('click.length.wrong.precision.t', showModal(modalDialog(
    title = "Length measurement(s) over precision limit", size = "l", easyClose = TRUE,
    renderDataTable(length.wrong.precision.t(),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))


  ## ► Histogram ----
  output$length.histogram.t <- renderPlot({
    req(input$length.species.dropdown.t)

    length3dpoints <- length3dpoints.clean.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t)) %>%
      replace_na(list(status = "Fished"))

    sizes <- life.history.min.max() %>%
      mutate(scientific = paste(genus, species, sep  = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t)) %>%
      distinct(scientific, fb_length_max, min_length, max_length)

    fishbase.max <- sum(sizes$fb_length_max)
    min.15 <- sum(sizes$min_length)
    max.85 <- sum(sizes$max_length)

    scientific.name <- input$length.species.dropdown.t

    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(length3dpoints, aes(x = length_mm), col = "black", alpha = 0.5)+
      geom_histogram(alpha = 0.5, position = "identity", binwidth = input$length.binwidth, col = "black")+
      xlab("Length (mm)") + ylab("Count") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      geom_vline(xintercept = fishbase.max, colour = "red", size = 1) +
      geom_vline(xintercept = min.15, colour = "grey", size = 1) +
      geom_vline(xintercept = max.85, colour = "grey", size = 1) +
      geom_text(aes(x = fishbase.max, label = "\n  Fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      geom_text(aes(x = min.15, label = "\n  15% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      geom_text(aes(x = max.85, label = "\n  85% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#3c8dbc"))+
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Histogram status ----
  output$length.histogram.status.t <- renderPlot({
    req(input$length.species.dropdown.t)

    length3dpoints <- length3dpoints.clean.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t)) %>%
      replace_na(list(status = "Fished"))

    sizes <- life.history.min.max() %>%
      mutate(scientific = paste(genus, species, sep  = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t)) %>%
      distinct(scientific, fb_length_max, min_length, max_length)

    fishbase.max <- sum(sizes$fb_length_max)
    min.15 <- sum(sizes$min_length)
    max.85 <- sum(sizes$max_length)

    scientific.name <- input$length.species.dropdown.t

    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(length3dpoints, aes(x = length_mm), col = "black", alpha = 0.5)+
      geom_histogram(alpha = 0.5, position = "identity", binwidth = input$length.binwidth, col = "black")+
      xlab("Length (mm)") + ylab("Count") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      geom_vline(xintercept = fishbase.max, colour = "red", size = 1) +
      geom_vline(xintercept = min.15, colour = "grey", size = 1) +
      geom_vline(xintercept = max.85, colour = "grey", size = 1) +
      geom_text(aes(x = fishbase.max, label = "\n  Fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      geom_text(aes(x = min.15, label = "\n  15% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      geom_text(aes(x = max.85, label = "\n  85% of fishbase maximum", y = 0), colour = "black", angle = 90, text = element_text(size = 14), hjust = 0, vjust = 0)+
      annotation_custom(grob.sci)+
      Theme1+
      facet_wrap(vars(status), ncol = 1)

  })

  ## ► Species plot - zone ----
  output$length.status.plot.t <- renderPlot({
    req(input$length.species.dropdown.t)
    length3dpoints <- length3dpoints.clean.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t))

    scientific.name <- input$length.species.dropdown.t

    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(length3dpoints, aes(x = factor(zone), y = length_mm,  fill = zone, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      xlab("Zone") + ylab("Length (mm)") +
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Species plot - status ----
  output$length.zone.plot.t <- renderPlot({
    req(input$length.species.dropdown.t)

    length3dpoints <- length3dpoints.clean.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific %in% c(input$length.species.dropdown.t)) %>%
      replace_na(list(status = "Fished"))

    scientific.name <- input$length.species.dropdown.t

    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(length3dpoints, aes(x = factor(status), y = length_mm,  fill = status, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      xlab("Status") + ylab("Length (mm)") +
      annotation_custom(grob.sci)+
      Theme1
  })


  ## _______________________________________________________ ----
  ##                            MASS                         ----
  ## _______________________________________________________ ----

  ## _______________________________________________________ ----
  ##                Mass for single point campaigns           ----
  ## _______________________________________________________ ----


  ## ► Create mass dataframe ----
  mass <- reactive({

    if(input$upload %in% "EM"){
      length <- length3dpoints.clean()
    } else {
      length <- gen.length.clean()
    }

    # 1. Check for missing length weight relationship
    taxa.missing.lw <- length %>%
      dplyr::distinct(family, genus, species) %>%
      dplyr::anti_join(filter(life.history(), !is.na(a)), by = c("family", "genus", "species"))

    #2. Fill length data with relevant a and b and if blank use family---
    length.species.ab <- life.history() %>% # done this way around to avoid duplicating Family coloum
      dplyr::select(-family) %>%
      dplyr::inner_join(length, ., by = c("genus", "species")) # only keeps row if has a and b

    # 3. Make family length.weight
    family.lw <- life.history() %>%
      dplyr::group_by(family, length.measure) %>%
      dplyr::mutate(log.a = log10(a)) %>%
      dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                       b = mean(b, na.rm = T),
                       all = mean(all, na.rm = T),
                       bll = mean(bll, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(a)) %>%
      dplyr::mutate(all = str_replace_all(all, "NaN", "0")) %>%
      dplyr::mutate(bll = str_replace_all(bll, "NaN", "1")) %>%
      dplyr::mutate(all = as.numeric(all)) %>%
      dplyr::mutate(bll = as.numeric(bll)) %>%
      dplyr::mutate(rank = ifelse(length.measure == "FL", 1, ifelse(length.measure == "TL", 2, 3))) %>%
      dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE)) %>%
      dplyr::filter(min.rank ==  0)

    length.family.ab <- length %>%
      dplyr::anti_join(life.history(), by = c("genus", "species")) %>%
      dplyr::left_join(family.lw, by = "family")

    # 5. Fill length data with relevant a and b and if blank use family---
    complete.length.number.mass <- length.species.ab %>%
      bind_rows(length.family.ab) %>%
      dplyr::filter(!is.na(a)) %>% #this gets rid of species with no lw
      dplyr::mutate(length.cm = length_mm/10) %>%
      dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
      dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>%
      dplyr::mutate(mass.g = (adjLength^b)*a*number) %>%
      dplyr::filter(mass.g>0) %>%
      dplyr::full_join(metadata.regions()) %>%
      dplyr::select(c(campaignid, sample, family, genus, species, length_mm, number, mass.g, length.cm, code)) %>% # removed EM columns
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      tidyr::replace_na(list(mass.g = 0)) %>%
      dplyr::mutate(mass_kg = mass.g/1000) %>%
      dplyr::left_join(metadata.regions())
  })


  ## ► Create filtered MASS download -----
  mass.complete.download <- reactive({
    
    if(input$upload %in% "EM"){
      length <- length3dpoints.clean()
    } else {
      length <- gen.length.clean()
    }

    length3dpoints <-  dplyr::left_join(length, synonyms()) %>% #, by = c("family", "genus", "species")
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::right_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, dplyr::any_of(c("range", "em_comment", "rms", "precision", "code"))) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup() %>%
      # dplyr::mutate(length = as.numeric(length)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::mutate(marine_region = as.character(marine_region)) %>%
      dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes"))

    # 1. Check for missing length weight relationship
    taxa.missing.lw <- length3dpoints %>%
      dplyr::distinct(family, genus, species) %>%
      dplyr::anti_join(filter(life.history(), !is.na(a)), by = c("family", "genus", "species"))

    #2. Fill length data with relevant a and b and if blank use family---
    length.species.ab <- life.history() %>% # done this way around to avoid duplicating Family coloum
      dplyr::select(-family) %>%
      dplyr::inner_join(length3dpoints, ., by = c("genus", "species")) # only keeps row if has a and b

    # 3. Make family length.weight
    family.lw <- life.history() %>%
      dplyr::group_by(family, length.measure) %>%
      dplyr::mutate(log.a = log10(a)) %>%
      dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                       b = mean(b, na.rm = T),
                       all = mean(all, na.rm = T),
                       bll = mean(bll, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(a)) %>%
      dplyr::mutate(all = str_replace_all(all, "NaN", "0")) %>%
      dplyr::mutate(bll = str_replace_all(bll, "NaN", "1")) %>%
      dplyr::mutate(all = as.numeric(all)) %>%
      dplyr::mutate(bll = as.numeric(bll)) %>%
      dplyr::mutate(rank = ifelse(length.measure == "FL", 1, ifelse(length.measure == "TL", 2, 3))) %>%
      dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE)) %>%
      dplyr::filter(min.rank ==  0)

    length.family.ab <- length3dpoints %>%
      dplyr::anti_join(life.history(), by = c("genus", "species")) %>%
      dplyr::left_join(family.lw, by = "family")

    # 5. Fill length data with relevant a and b and if blank use family---
    complete.length.number.mass <- length.species.ab %>%
      bind_rows(length.family.ab) %>%
      dplyr::filter(!is.na(a)) %>% #this gets rid of species with no lw
      dplyr::mutate(length.cm = length_mm/10) %>%
      dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
      dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>%
      dplyr::mutate(mass.g = (adjLength^b)*a*number) %>%
      dplyr::filter(mass.g>0) %>%
      dplyr::full_join(metadata.regions()) %>%
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, mass.g, length.cm, dplyr::any_of(c("range", "em_comment", "rms", "precision", "code"))) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      tidyr::replace_na(list(mass.g = 0)) %>%
      dplyr::mutate(mass_kg = mass.g/1000)

    if (input$error.synonyms == TRUE) {
      complete.length.number.mass <- dplyr::left_join(complete.length.number.mass, synonyms()) %>% #, by = c("family", "genus", "species")
        dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
        dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
        dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
        dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, mass_kg, dplyr::any_of(c("range", "em_comment", "rms", "precision", "code"))) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
        dplyr::ungroup() %>%
        # dplyr::mutate(length = as.numeric(length)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))
    }
    else{
      complete.length.number.mass <- dplyr::left_join(complete.length.number.mass, synonyms()) %>% #, by = c("family", "genus", "species")
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, mass_kg, dplyr::any_of(c("range", "em_comment", "rms", "precision", "code"))) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
        dplyr::ungroup() %>%
        # dplyr::mutate(length = as.numeric(length)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes")) %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        # dplyr::mutate(project = input$project.name) %>%
        # dplyr::mutate(id = paste(project, campaignid, sep = ".")) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " "))
    }

    complete.length.number.mass <- complete.length.number.mass

    species.out.of.area <- life.history.expanded() %>%
      dplyr::mutate(marine_region = as.character(marine_region)) %>%
      anti_join(complete.length.number.mass, ., by = c("family", "genus", "species", "marine_region")) %>%
      distinct(family, genus, species, marine_region) %>%
      filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))


    if (input$error.area == FALSE) {
      mass.area <- complete.length.number.mass
    }
    else{
      mass.area <- anti_join(complete.length.number.mass, species.out.of.area)
    }

    if(input$upload %in% "EM"){
    mass.area <- mass.area %>%
      dplyr::filter(range<(input$error.range.limit*1000))
    }

    length.wrong <- left_join(mass.area, life.history.min.max(), by = c("family", "genus", "species")) %>%
      dplyr::filter(length_mm<min_length|length_mm>fb_length_max) %>%
      mutate(reason = ifelse(length_mm<min_length, "too small", "too big"))

    length.too.small <- length.wrong %>%
      dplyr::filter(reason%in%c("too small"))

    length.too.big <- length.wrong %>%
      dplyr::filter(reason%in%c("too big"))

    if (input$error.length.small == TRUE) {
      mass.small <- anti_join(mass.area, length.too.small)
    }
    else{
      mass.small <- mass.area
    }

    mass.small <- mass.small

    if (input$error.length.big == TRUE) {
      mass.big <- anti_join(mass.small, length.too.big)
    }
    else{
      mass.big <- mass.small
    }
    mass.big <- mass.big %>%
      dplyr::right_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, mass_kg, dplyr::any_of(c("range", "em_comment", "code"))) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::left_join(metadata.regions()) %>%
      filter(!is.na(family)) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " "))

    # If "Remove extra columns" = TRUE
    if (input$error.extra.col == TRUE) {
      mass.big <- mass.big %>%
        dplyr::select(-c(zone, marine_region, scientific, dplyr::any_of(c("em_comment"))))
    }

    if (input$error.zeros == TRUE) {
      mass.big <- mass.big %>%
        dplyr::select(!sample) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything())

    } else{
      mass.big <- mass.big %>%
        dplyr::filter(!number %in% 0) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, number, mass_kg, dplyr::any_of(c("range", "code"))) 
      }

  })

  ## ► Species dropdown ----
  output$mass.species.dropdown <- renderUI({
    df <- mass()

    options <- df %>%
      dplyr::mutate(genus = ifelse(genus%in%c(NA, "NA", "Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(sum.mass = sum(mass.g)) %>%
      dplyr::arrange(-sum.mass) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      distinct(scientific) %>%
      pull("scientific")

    create_dropdown("mass.species.dropdown", options, NULL)
  })

  ## ► Top species ----
  output$mass.top.species <- renderPlot({
    mass.sum <- mass() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      left_join(all_data$classes) %>%
      dplyr::group_by(class, scientific) %>%
      dplyr::summarise(sum.mass.g = sum(mass.g)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sum.mass.kg = sum.mass.g/1000)

    if (input$mass.include.sharks == "yes"){
      mass.sum <- mass.sum %>%
        top_n(input$mass.species.limit)
    } else {
      mass.sum <- mass.sum %>%
        dplyr::filter(!class%in%c("Elasmobranchii")) %>%
        top_n(input$mass.species.limit)
    }

    ## Total frequency of occurance
    ggplot(mass.sum, aes(x = reorder(scientific, sum.mass.kg), y = sum.mass.kg)) +
      geom_bar(stat = "identity", position = position_dodge())+
      coord_flip()+
      xlab("Species")+
      ylab(expression(Total~mass~(Sigma~Kg)))+
      Theme1+
      theme(axis.text.y = element_text(face = "italic"))+
      theme_collapse+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  })

  ## ► Species plot - status ----
  output$mass.status.plot <- renderPlot({
    req(input$mass.species.dropdown)
    mass <- mass() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific%in%c(input$mass.species.dropdown)) %>%
      dplyr::group_by(campaignid, sample, scientific, status) %>%
      dplyr::summarise(mass.g = sum(mass.g))

    scientific.name <- input$mass.species.dropdown
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(mass, aes(x = factor(status), y = mass.g,  fill = status, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
      xlab("Status") + ylab("Mass (g)") +
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Species plot - zone ----
  output$mass.zone.plot <- renderPlot({
    req(input$mass.species.dropdown)
    mass <- mass() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific%in%c(input$mass.species.dropdown)) %>%
      dplyr::group_by(campaignid, sample, scientific, zone) %>%
      dplyr::summarise(mass.g = sum(mass.g))

    scientific.name <- input$mass.species.dropdown
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(mass, aes(x = factor(zone), y = mass.g,  fill = zone, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
      xlab("Zone") + ylab("Mass (g)") +
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Spatial plot ----
  output$mass.spatial.plot <- renderLeaflet({

    req(input$mass.species.dropdown)

    mass <- mass() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific == input$mass.species.dropdown) %>%
      dplyr::group_by(campaignid, sample, scientific) %>%
      dplyr::summarise(mass.g = sum(mass.g)) %>%
      left_join(metadata.regions())

    map <- leaflet(mass) %>%
      addTiles() %>%
      fitBounds(~min(longitude_dd), ~min(latitude_dd), ~max(longitude_dd), ~max(latitude_dd))

    overzero <- filter(mass, mass.g > 0)
    equalzero <- filter(mass, mass.g ==  0)

    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
          radius = ~((mass.g/max(mass.g))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(mass.g)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~as.character(mass.g)
        )
    }
    map
  })


  ## _______________________________________________________ ----
  ##             Mass for transect based campaigns           ----
  ## _______________________________________________________ ----


  ## ► Create mass dataframe ----
  mass.t <- reactive({
    # 1. Check for missing length weight relationship
    taxa.missing.lw <- length3dpoints.clean.t() %>%
      dplyr::distinct(family, genus, species) %>%
      dplyr::anti_join(filter(life.history(), !is.na(a)), by = c("family", "genus", "species"))

    #2. Fill length data with relevant a and b and if blank use family---
    length.species.ab <- life.history() %>% # done this way around to avoid duplicating Family coloum
      dplyr::select(-family) %>%
      dplyr::inner_join(length3dpoints.clean.t(), ., by = c("genus", "species")) # only keeps row if has a and b

    # 3. Make family length.weight
    family.lw <- life.history() %>%
      dplyr::group_by(family, length.measure) %>%
      dplyr::mutate(log.a = log10(a)) %>%
      dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                       b = mean(b, na.rm = T),
                       all = mean(all, na.rm = T),
                       bll = mean(bll, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(a)) %>%
      dplyr::mutate(all = str_replace_all(all, "NaN", "0")) %>%
      dplyr::mutate(bll = str_replace_all(bll, "NaN", "1")) %>%
      dplyr::mutate(all = as.numeric(all)) %>%
      dplyr::mutate(bll = as.numeric(bll)) %>%
      dplyr::mutate(rank = ifelse(length.measure == "FL", 1, ifelse(length.measure == "TL", 2, 3))) %>%
      dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE)) %>%
      dplyr::filter(min.rank ==  0)

    length.family.ab <- length3dpoints.clean.t() %>%
      dplyr::anti_join(life.history(), by = c("genus", "species")) %>%
      dplyr::left_join(family.lw, by = "family")

    # 5. Fill length data with relevant a and b and if blank use family---
    complete.length.number.mass <- length.species.ab %>%
      bind_rows(length.family.ab) %>%
      dplyr::filter(!is.na(a)) %>% #this gets rid of species with no lw
      dplyr::mutate(length.cm = length_mm/10) %>%
      dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
      dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>%
      dplyr::mutate(mass.g = (adjLength^b)*a*number) %>%
      dplyr::filter(mass.g>0) %>%
      dplyr::full_join(metadata.regions()) %>%
      dplyr::select(c(campaignid, sample, family, genus, species, length_mm, range, number, mass.g, length.cm, code)) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      tidyr::replace_na(list(mass.g = 0)) %>%
      dplyr::mutate(mass_kg = mass.g/1000) %>%
      dplyr::left_join(metadata.regions())
  })


  ## ► Create filtered MASS download -----
  mass.complete.download.t <- reactive({

    length3dpoints <-  dplyr::left_join(length3dpoints.t(), synonyms()) %>% #, by = c("family", "genus", "species")
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::right_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, code) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup() %>%
      # dplyr::mutate(length = as.numeric(length)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::mutate(marine_region = as.character(marine_region)) %>%
      dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes"))

    # 1. Check for missing length weight relationship
    taxa.missing.lw <- length3dpoints %>%
      dplyr::distinct(family, genus, species) %>%
      dplyr::anti_join(filter(life.history(), !is.na(a)), by = c("family", "genus", "species"))

    #2. Fill length data with relevant a and b and if blank use family---
    length.species.ab <- life.history() %>% # done this way around to avoid duplicating Family coloum
      dplyr::select(-family) %>%
      dplyr::inner_join(length3dpoints, ., by = c("genus", "species")) # only keeps row if has a and b

    # 3. Make family length.weight
    family.lw <- life.history() %>%
      dplyr::group_by(family, length.measure) %>%
      dplyr::mutate(log.a = log10(a)) %>%
      dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                       b = mean(b, na.rm = T),
                       all = mean(all, na.rm = T),
                       bll = mean(bll, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(a)) %>%
      dplyr::mutate(all = str_replace_all(all, "NaN", "0")) %>%
      dplyr::mutate(bll = str_replace_all(bll, "NaN", "1")) %>%
      dplyr::mutate(all = as.numeric(all)) %>%
      dplyr::mutate(bll = as.numeric(bll)) %>%
      dplyr::mutate(rank = ifelse(length.measure == "FL", 1, ifelse(length.measure == "TL", 2, 3))) %>%
      dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE)) %>%
      dplyr::filter(min.rank ==  0)

    length.family.ab <- length3dpoints %>%
      dplyr::anti_join(life.history(), by = c("genus", "species")) %>%
      dplyr::left_join(family.lw, by = "family")

    # 5. Fill length data with relevant a and b and if blank use family---
    complete.length.number.mass <- length.species.ab %>%
      bind_rows(length.family.ab) %>%
      dplyr::filter(!is.na(a)) %>% #this gets rid of species with no lw
      dplyr::mutate(length.cm = length_mm/10) %>%
      dplyr::mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
      dplyr::mutate(adjLength = ((length.cm*bll)+all)) %>%
      dplyr::mutate(mass.g = (adjLength^b)*a*number) %>%
      dplyr::filter(mass.g>0) %>%
      dplyr::full_join(metadata.regions()) %>%
      dplyr::select(c(campaignid, sample, family, genus, species, length_mm, range, number, mass.g, length.cm, code)) %>%
      tidyr::complete(campaignid, sample, nesting(family, genus, species, code)) %>%
      tidyr::replace_na(list(mass.g = 0)) %>%
      dplyr::mutate(mass_kg = mass.g/1000)

    if (input$error.synonyms == TRUE) {
      complete.length.number.mass <- dplyr::left_join(complete.length.number.mass, synonyms()) %>% #, by = c("family", "genus", "species")
        dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
        dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
        dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
        dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, mass_kg, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
        dplyr::ungroup() %>%
        # dplyr::mutate(length = as.numeric(length)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))
    }
    else{
      complete.length.number.mass <- dplyr::left_join(complete.length.number.mass, synonyms()) %>% #, by = c("family", "genus", "species")
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, mass_kg, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
        dplyr::ungroup() %>%
        # dplyr::mutate(length = as.numeric(length)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_length%in%c("Yes", "Y", "y", "yes")) %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        # dplyr::mutate(project = input$project.name) %>%
        # dplyr::mutate(id = paste(project, campaignid, sep = ".")) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " "))
    }

    complete.length.number.mass <- complete.length.number.mass

    species.out.of.area <- life.history.expanded() %>%
      dplyr::mutate(marine_region = as.character(marine_region)) %>%
      anti_join(complete.length.number.mass, ., by = c("family", "genus", "species", "marine_region")) %>%
      distinct(family, genus, species, marine_region) %>%
      filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))


    if (input$error.area.t == FALSE) {
      mass.area <- complete.length.number.mass
    }
    else{
      mass.area <- anti_join(complete.length.number.mass, species.out.of.area)
    }

    mass.area <- mass.area %>%
      dplyr::filter(range < (input$error.range.limit.t*1000))

    length.wrong <- left_join(mass.area, life.history.min.max(), by = c("family", "genus", "species")) %>%
      dplyr::filter(length_mm<min_length|length_mm>fb_length_max) %>%
      mutate(reason = ifelse(length_mm<min_length, "too small", "too big"))

    length.too.small <- length.wrong %>%
      dplyr::filter(reason%in%c("too small"))

    length.too.big <- length.wrong %>%
      dplyr::filter(reason%in%c("too big"))

    if (input$error.length.small.t == TRUE) {
      mass.small <- anti_join(mass.area, length.too.small)
    }
    else{
      mass.small <- mass.area
    }

    mass.small <- mass.small

    if (input$error.length.big.t == TRUE) {
      mass.big <- anti_join(mass.small, length.too.big)
    }
    else{
      mass.big <- mass.small
    }
    mass.big <- mass.big %>%
      dplyr::right_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, range, mass_kg, code) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::left_join(metadata.regions()) %>%
      filter(!is.na(family)) %>%
      # dplyr::mutate(project = input$project.name) %>%
      # dplyr::mutate(id = paste(project, campaignid, sep = ".")) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " "))

    # If "Remove extra columns" = TRUE
    if (input$error.extra.col.t == TRUE) {
      mass.big <- mass.big %>%
        dplyr::select(-c(zone, marine_region, scientific))
    }

    if (input$error.zeros.t == TRUE) {

      mass.big <- mass.big %>%
        dplyr::select(!sample) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything())

    } else {
      mass.big <- mass.big %>%
        filter(!number %in% 0) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, mass_kg, number, range, code)
    }

    mass.big <- mass.big

  })

  ## ► Species dropdown ----
  output$mass.species.dropdown.t <- renderUI({
    df <- mass.t()

    options <- df %>%
      dplyr::mutate(genus = ifelse(genus%in%c(NA, "NA", "Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(sum.mass = sum(mass.g)) %>%
      dplyr::arrange(-sum.mass) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      distinct(scientific) %>%
      pull("scientific")

    create_dropdown("mass.species.dropdown.t", options, NULL)
  })

  ## ► Top species ----
  output$mass.top.species.t <- renderPlot({
    mass.sum <- mass.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      left_join(all_data$classes) %>%
      dplyr::group_by(class, scientific) %>%
      dplyr::summarise(sum.mass.g = sum(mass.g)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sum.mass.kg = sum.mass.g/1000)

    if (input$mass.include.sharks.t == "yes"){
      mass.sum <- mass.sum %>%
        top_n(input$mass.species.limit.t)
    } else {
      mass.sum <- mass.sum %>%
        dplyr::filter(!class%in%c("Elasmobranchii")) %>%
        top_n(input$mass.species.limit.t)
    }

    ## Total frequency of occurance
    ggplot(mass.sum, aes(x = reorder(scientific, sum.mass.kg), y = sum.mass.kg)) +
      geom_bar(stat = "identity", position = position_dodge())+
      coord_flip()+
      xlab("Species")+
      ylab(expression(Total~mass~(Sigma~Kg)))+
      Theme1+
      theme(axis.text.y = element_text(face = "italic"))+
      theme_collapse+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  })

  ## ► Species plot - status ----
  output$mass.status.plot.t <- renderPlot({
    req(input$mass.species.dropdown.t)
    mass <- mass.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific %in% c(input$mass.species.dropdown.t)) %>%
      dplyr::group_by(campaignid, sample, scientific, status) %>%
      dplyr::summarise(mass.g = sum(mass.g))

    scientific.name <- input$mass.species.dropdown.t
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(mass, aes(x = factor(status), y = mass.g,  fill = status, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
      xlab("Status") + ylab("Mass (g)") +
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Species plot - zone ----
  output$mass.zone.plot.t <- renderPlot({
    req(input$mass.species.dropdown.t)
    mass <- mass.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific%in%c(input$mass.species.dropdown.t)) %>%
      dplyr::group_by(campaignid, sample, scientific, zone) %>%
      dplyr::summarise(mass.g = sum(mass.g))

    scientific.name <- input$mass.species.dropdown.t
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0,
                                  gp = gpar(col = "black", fontsize = 13, fontface = "italic")))

    ggplot(mass, aes(x = factor(zone), y = mass.g,  fill = zone, notch = FALSE, outlier.shape = NA), alpha = 0.5) +
      stat_boxplot(geom = 'errorbar')+
      geom_boxplot(outlier.color = NA, notch = FALSE)+
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
      xlab("Zone") + ylab("Mass (g)") +
      annotation_custom(grob.sci)+
      Theme1
  })

  ## ► Spatial plot ----
  output$mass.spatial.plot.t <- renderLeaflet({

    req(input$mass.species.dropdown.t)

    mass <- mass.t() %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      filter(scientific == input$mass.species.dropdown.t) %>%
      dplyr::group_by(campaignid, sample, scientific) %>%
      dplyr::summarise(mass.g = sum(mass.g)) %>%
      left_join(metadata.regions())

    map <- leaflet(mass) %>%
      addTiles() %>%
      fitBounds(~min(longitude_dd), ~min(latitude_dd), ~max(longitude_dd), ~max(latitude_dd))

    overzero <- filter(mass, mass.g > 0)
    equalzero <- filter(mass, mass.g ==  0)

    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
          radius = ~((mass.g/max(mass.g))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(mass.g)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~as.character(mass.g)
        )
    }
    map
  })

  ## _______________________________________________________ ----
  ##                    LENGTH Vs. MAXN                     ----
  ## _______________________________________________________ ----


  ## ► Length vs MaxN  ----
  length.vs.maxn <- reactive({
    length.sample <- metadata.regions() %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) %>%
      distinct(sample)

    if(input$upload %in% "EM"){

      length <- length3dpoints.clean()
      maxn <- maxn.clean()

    } else {

      length <- gen.length.clean()
      maxn <- count.clean()

    }

    print("number of rows in length")
    print(nrow(length))

    print(nrow(maxn))

    # summarise length and then compare to maxn
    length.vs.maxn <- length %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>%
      dplyr::summarise(length_maxn = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(maxn) %>% # Changed to full join 22/08/2023
      replace_na(list(maxn = 0, length_maxn = 0)) %>%
      # dplyr::filter(!length_maxn == maxn) %>%
      dplyr::mutate(percent_difference = (maxn-length_maxn)/maxn*100) %>%
      dplyr::semi_join(length.sample) %>% # only keep ones where length was possible
      replace_na(list(percent_difference = 0)) %>%
      dplyr::mutate(difference = (maxn - length_maxn)) %>%
      dplyr::mutate(difference = abs(difference)) %>%
      dplyr::mutate(percent_difference = abs(percent_difference)) %>%
      arrange(-difference) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), family, genus, species, maxn, length_maxn, difference, percent_difference)
  })



  ## ► Valuebox ----
  output$length.vs.maxn <- renderValueBox({
    length.vs.maxn <- length.vs.maxn() %>%
      dplyr::mutate(count = 1) %>%
      dplyr::filter(!length_maxn == maxn) %>%
      dplyr::filter(!percent_difference%in%c(0)) #only for those that have missing lengths

    if (dim(length.vs.maxn)[1] > 0) {
      total <- sum(length.vs.maxn$count)
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }

    valueBox(width = 3,
             total,
             "Number of lengths/3D points does not match MaxN",
             icon = icon("not-equal"), color = col
    )
  })

  ## ► Onclick ----
  onclick('click.length.vs.maxn', showModal(modalDialog(
    title = "Number of rows where the total lengths + 3D points does not match MaxN", size = "l", easyClose = TRUE,
    renderDataTable(length.vs.maxn() %>% filter(!length_maxn == maxn) %>% dplyr::select(!sample)
                    ,  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))

  ## ► Plot ----
  output$length.vs.maxn.plot <- renderPlot({

    ggplot(length.vs.maxn(), aes(x = maxn, y = length_maxn, label = paste(genus, species, sep = " ")))+
      geom_abline(colour = "red", alpha = 0.5)+
      geom_point()+
      geom_text(alpha = 0.2)+
      Theme1
  })

  ## ► Valuebox - Length Vs. MaxN score ----
  output$length.vs.maxn.score <- renderValueBox({

    print("new length error score")

    length.vs.maxn <- length.vs.maxn() %>%
      dplyr::filter(!(maxn %in% 0 & length_maxn %in% 0))%>%
      dplyr::mutate(difference = abs(difference)) %>%
      dplyr::mutate(error = if_else(difference > 0, 1, 0)) #%>% glimpse()

    total.rows <- nrow(length.vs.maxn)# %>% glimpse()
    total.errors <- sum(length.vs.maxn$error) #%>% glimpse()

    percentage <- ((total.rows - total.errors)/ total.rows) * 100

    valueBox(width = 3,
             round(percentage, 2),
             "Number in count VS Number in length (+3D Point if EM) score",
             icon = icon("percent"), color = "blue"
    )
  })

  ## ► Plot - Particular species----
  output$length.vs.maxn.plot.species <- renderPlot({

    dat <- length.vs.maxn() %>%
      dplyr::mutate(genus = ifelse(genus %in% c("Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      dplyr::filter(scientific %in% input$length.vs.maxn.species.dropdown)

    ggplot(dat, aes(x = maxn, y = length_maxn, label = sample))+
      geom_abline(colour = "red", alpha = 0.5)+
      geom_point()+
      geom_text(alpha = 0.2)+
      Theme1
  })

  ## ► Dropdown -----
  output$length.vs.maxn.species.dropdown <- renderUI({

    if(input$upload %in% "EM"){
      df <- maxn.clean()
    } else {
      df <- count.clean()
    }

    options <- df %>%
      dplyr::mutate(genus = ifelse(genus %in% c(NA, "NA", "Unknown"), as.character(family), as.character(genus))) %>%
      dplyr::group_by(family, genus, species) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
      distinct(scientific) %>%
      pull("scientific")

    create_dropdown("length.vs.maxn.species.dropdown", options, NULL)
  })

  # ## ► Species plot ----
  # output$length.vs.maxn.species.plot <- renderPlot({
  #   req(input$length.vs.maxn.species.dropdown)
  #
  #   length.vs.maxn <- length.vs.maxn() %>%
  #     dplyr::mutate(genus = ifelse(genus%in%c(NA, "NA", "Unknown"), as.character(family), as.character(genus))) %>%
  #     dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  #     filter(scientific == input$length.vs.maxn.species.dropdown)
  #
  #   ggplot(length.vs.maxn, aes(x = maxn, y = length_maxn, label = paste(sample)))+
  #     geom_abline(colour = "red", alpha = 0.5)+
  #     geom_point()+
  #     geom_text(alpha = 0.2)+
  #     scale_y_continuous(expand = expand_scale(mult = c(-0.5, .5)))+
  #     scale_x_continuous(expand = expand_scale(mult = c(-0.5, .5)))+
  #     Theme1
  # })

  ## _______________________________________________________ ----
  ##                        HABITAT                          ----
  ## _______________________________________________________ ----

  hab.points <- reactive({

    if(input$hab == "Yes"){

      # When folder chosen ----
      if(!is.null(input$folderdir)) {

        # Get all _Dot Point Measurements files in the folder
        message("habitat files detected")
        
        files <- input$folderdir %>%
          dplyr::filter(grepl("_Dot Point Measurements.txt", name)) #%>% glimpse()

        points <- data.frame()

        if (is.null(files)) return(NULL)

        for (i in seq_along(files$datapath)) {
          tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"), skip = 3,
                          na = "NA")
          points <- bind_rows(points, tmp) #%>% glimpse()
        }

        message("habitat points")

        points <- points %>%
          clean_names() %>%
          glimpse()
        
        # If point method and samples are opcodes
        if(input$method == "point" & input$sample == "opcode") {
          
          points <- points %>%
            dplyr::mutate(sample = opcode)
        }
        
        # If point method and samples are periods
        if(input$method == "point" & input$sample == "period") {
          
          points <- points %>%
            dplyr::mutate(sample = period)
        }
        
        # If transect method and sample = "opcode" + "period"
        if(input$method == "transect" & input$sample.t == "opcodeperiod") {
          
          points <- points %>%
            dplyr::mutate(sample = paste(opcode, period, sep = "_"))
          
        }
        # If transect method and sample = "period"
        if(input$method == "transect" & input$sample.t == "period") {
          
          lookup <- c(sample = "period") # If people have used period or sample then this will work
          
          points <- points %>%
            dplyr::mutate(sample = period)
        }
        
        points[points == ''] <- NA # Make all empty cells NA
        
      }

      # TODO change this to hab and add example data
      # if no folder chosen and method = single point. dataset = Ningloo BRUVs
      # if(is.null(input$folderdir) & input$method == "point" & input$sample == "opcode") {
      #
      #   periods <-  read.delim("data/example_Period.txt", na.strings = "") %>%
      #     clean_names() %>%
      #     dplyr::rename(sample = opcode) %>%
      #     dplyr::mutate(sample = as.factor(sample)) %>%
      #     dplyr::mutate(campaignid = "2022-01_example-campaign_stereo-BRUVs") %>%
      #     as.data.frame()
      # }

      # TODO add an example dataset for DOVs
      
      points <- points %>%
        dplyr::select(campaignid, sample, opcode, period, image_row, image_col, 
                      level_2, level_3, level_4, level_5, scientific, qualifiers, caab_code,
                      relief_annotated) %>%
        dplyr::mutate(id = 1:nrow(.)) %>%
        dplyr::glimpse()
    }
  })


  ## ► Preview habitat in dashboard ----
  output$table.habitat <- renderDataTable({
    hab.points() %>% dplyr::select(-c(id, sample))
  })
  
  hab.annotations <- reactive({
    hab.points() %>%
      dplyr::filter(relief_annotated %in% "no") %>%
      dplyr::select(campaignid, sample, id, starts_with("level"), scientific, caab_code)
  })
  
  relief.annotations <- reactive({
    hab.points() %>%
      dplyr::filter(relief_annotated %in% "yes") %>%
      dplyr::select(campaignid, sample, id, starts_with("level"), scientific, caab_code)
  })
  
  ## ► Samples without habitat - dataframe ----
  metadata.samples.without.hab <- reactive({

    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), 
                    successful_count, successful_length, 
                    successful_habitat_forward, successful_habitat_backward) %>%
      dplyr::distinct()

    points.samples <- hab.annotations() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), sample) %>%
      dplyr::distinct()

    missing.hab <- anti_join(metadata.samples, points.samples)
  })
  
  ## ► Samples without relief - dataframe ----
  metadata.samples.without.relief <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), 
                    successful_count, successful_length, 
                    successful_habitat_forward, successful_habitat_backward) %>%
      dplyr::distinct()
    
    points.samples <- relief.annotations() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), sample) %>%
      dplyr::distinct()
    
    missing.hab <- anti_join(metadata.samples, points.samples)
  })

  ## ► Samples without habitat - valueBox ----
  output$metadata.samples.without.hab <- renderValueBox({

    if (dim(metadata.samples.without.hab())[1] > 0) {
      total <- nrow(metadata.samples.without.hab())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }

    valueBox(width = 2,
             total,
             "Sample(s) in metadata without biota annotations",
             icon = icon("question"), color = col
    )
  })
  
  ## ► Samples without relief - valueBox ----
  output$metadata.samples.without.relief <- renderValueBox({
    
    if (dim(metadata.samples.without.relief())[1] > 0) {
      total <- nrow(metadata.samples.without.relief())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Sample(s) in metadata without relief annotations",
             icon = icon("question"), color = col
    )
  })

  ## ► Samples without habitat - onclick----
  onclick('click.metadata.samples.without.hab',
          showModal(modalDialog(
            title = "Sample(s) without habitat",
            easyClose = TRUE,
            renderDataTable(metadata.samples.without.hab(), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Samples without relief - onclick----
  onclick('click.metadata.samples.without.relief',
          showModal(modalDialog(
            title = "Sample(s) without relief",
            easyClose = TRUE,
            renderDataTable(metadata.samples.without.relief(), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))
          ))

  ## ► Habitat samples without metadata - dataframe ----
  habitat.samples.without.metadata <- reactive({
    metadata.samples <- metadata() %>%
      distinct(campaignid, sample) %>%
      mutate(sample = as.factor(sample)) %>%
      ungroup()

    points.samples <- hab.annotations() %>%
      distinct(campaignid, sample) %>%
      ungroup()

    missing.metadata <- anti_join(points.samples, metadata.samples)
  })

  ## ► Habitat samples without metadata - valueBox ----
  output$habitat.samples.without.metadata <- renderValueBox({

    if (dim(habitat.samples.without.metadata())[1] > 0) {
      total <- nrow(habitat.samples.without.metadata())
      col <- "red"

    } else {
      total = 0
      col <- "green"
    }

    valueBox(width = 2,
             total,
             "Sample(s) in biota annotation file(s) missing metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })

  ## ► Samples without metadata - onclick ----
  onclick('click.habitat.samples.without.metadata',
          showModal(modalDialog(
            title = "Sample(s) in habitat without metadata",
            easyClose = TRUE,
            renderDataTable(habitat.samples.without.metadata(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  
  ## ► Relief samples without metadata - dataframe ----
  relief.samples.without.metadata <- reactive({
    metadata.samples <- metadata() %>%
      distinct(campaignid, sample) %>%
      mutate(sample = as.factor(sample)) %>%
      ungroup()
    
    points.samples <- relief.annotations() %>%
      distinct(campaignid, sample) %>%
      ungroup()
    
    missing.metadata <- anti_join(points.samples, metadata.samples)
  })
  
  ## ► Relief samples without metadata - valueBox ----
  output$relief.samples.without.metadata <- renderValueBox({
    
    if (dim(relief.samples.without.metadata())[1] > 0) {
      total <- nrow(relief.samples.without.metadata())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Sample(s) in relief annotation file(s) missing metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Samples without metadata - onclick ----
  onclick('click.relief.samples.without.metadata',
          showModal(modalDialog(
            title = "Sample(s) in relief annotation file(s) missing metadata",
            easyClose = TRUE,
            renderDataTable(relief.samples.without.metadata(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))

  ## ► Habitat number of annotations - dataframe ----
  habitat.annotations.per.sample <- reactive({

    points.samples <- hab.annotations() %>%
      ungroup() %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(number.of.annotations = n())

  })

  ## ► Habitat wrong number of annotations - dataframe ----
  habitat.wrong.annotations <- reactive({

    wrong <- habitat.annotations.per.sample() %>%
      dplyr::distinct(campaignid, sample, number.of.annotations) %>%
      dplyr::left_join(metadata()) %>%
      dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 2,
                                         successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ input$number.of.annotations * 1,
                                         successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 1,
                                         successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ 0)) %>%
      dplyr::filter(!number.of.annotations == expected) %>%
      dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
  })

  ## ► Habitat wrong number of annotations - valueBox ----
  output$habitat.wrong.annotations <- renderValueBox({

    if (dim(habitat.wrong.annotations())[1] > 0) {
      total <- nrow(habitat.wrong.annotations())
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }

    valueBox(width = 4,
             total,
             paste("Samples with incorrect number of biota annotations", sep = " "),
             icon = icon("question"), color = col
    )
  })

  ## ► Habitat wrong number of annotations - onclick----
  onclick('click.habitat.wrong.annotations',
          showModal(modalDialog(
            title = "Number of annotations per sample",
            easyClose = TRUE,
            renderDataTable(habitat.wrong.annotations(), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  
  ## ► Relief number of annotations - dataframe ----
  relief.annotations.per.sample <- reactive({
    
    points.samples <- relief.annotations() %>%
      ungroup() %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(number.of.annotations = n())
    
  })
  
  ## ► Habitat wrong number of annotations - dataframe ----
  relief.wrong.annotations <- reactive({
    
    wrong <- relief.annotations.per.sample() %>%
      dplyr::distinct(campaignid, sample, number.of.annotations) %>%
      dplyr::left_join(metadata()) %>%
      dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 2,
                                         successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ input$number.of.annotations * 1,
                                         successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 1,
                                         successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ 0)) %>%
      dplyr::filter(!number.of.annotations == expected) %>%
      dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
  })
  
  ## ► relief wrong number of annotations - valueBox ----
  output$relief.wrong.annotations <- renderValueBox({
    
    if (dim(relief.wrong.annotations())[1] > 0) {
      total <- nrow(relief.wrong.annotations())
      col <- "red"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4,
             total,
             paste("Samples with incorrect number of relief annotations", sep = " "),
             icon = icon("question"), color = col
    )
  })
  
  ## ► Habitat wrong number of annotations - onclick----
  onclick('click.relief.wrong.annotations',
          showModal(modalDialog(
            title = "Number of relief annotations per sample",
            easyClose = TRUE,
            renderDataTable(relief.wrong.annotations(), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))
          ))


  ## ► Relief - dataframe ----
  tidy.relief <- reactive({
    
    message("view tidy relief")
    relief.annotations() %>%
      # dplyr::left_join(schema) %>%
      dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>% # Remove Open water and Unknown entries from broad
      dplyr::mutate(number = 1) %>%                   
      dplyr::group_by(campaignid, sample, across(starts_with("level")), caab_code) %>%
      dplyr::tally(number, name = "number") %>%                                                    
      dplyr::ungroup()  %>%
      dplyr::select(campaignid, sample, everything()) %>% #level_1, 
      dplyr::left_join(metadata.regions())
    
  })

  ## ► Habitat broad points - dataframe ----
  tidy.habitat <- reactive({
    
    message("view tidy habitat")
    tidy.habitat <- hab.annotations() %>%
      dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
      # dplyr::left_join(schema) %>%
      dplyr::select(campaignid, sample, number, starts_with("level"), caab_code) %>% # family, genus, species, 
      dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%  
      dplyr::group_by(campaignid, sample, across(starts_with("level")), caab_code) %>% #family, genus, species, 
      dplyr::tally(number, name = "number") %>%
      dplyr::ungroup() %>%
      dplyr::select(campaignid, sample, everything()) %>% #level_1, 
      dplyr::left_join(metadata.regions())

  })
  
  ## ► Habitat - does not match schema - dataframe ----
  hab.not.in.schema <- reactive({
    
    dat <- bind_rows(tidy.relief(), tidy.habitat()) %>%
      dplyr::select(starts_with("level"), caab_code) %>%
      dplyr::distinct()
    
    missing.in.schema <- anti_join(dat, schema)

  })
  
  ## ► Habitat wrong number of annotations - valueBox ----
  output$hab.not.in.schema <- renderValueBox({
    
    if (dim(hab.not.in.schema())[1] > 0) {
      total <- nrow(hab.not.in.schema())
      col <- "yellow"
    }
    else{
      total = 0
      col <- "green"
    }
    
    valueBox(width = 4,
             total,
             paste("Habitat annotation not in CATAMI schema", sep = " "),
             icon = icon("question"), color = col
    )
  })
  
  ## ► Habitat wrong number of annotations - onclick----
  onclick('click.hab.not.in.schema',
          showModal(modalDialog(
            title = "Habitat annotation not in CATAMI schema",
            easyClose = TRUE,
            renderDataTable(hab.not.in.schema(), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Habitat level dropdowns ----
  output$habitat.levels <- renderUI({
    
    options <- tidy.habitat() %>%
      dplyr::select(starts_with("level_")) 
    
    create_dropdown("habitat.levels", names(options), NULL)
  })
  
  # Habitat data for plot ----
  habitat.plot.data <- reactive({
    
    req(input$habitat.levels)
    
    message("habitat level selected")
    print(input$habitat.levels)
    
    dat <- tidy.habitat() %>%
      dplyr::mutate(level_5 = paste(level_2, level_3, level_4, level_5, sep = ": ")) %>%
      dplyr::mutate(level_4 = paste(level_2, level_3, level_4, sep = ": ")) %>%
      dplyr::mutate(level_3 = paste(level_2, level_3, sep = ": ")) %>%
      dplyr::select(campaignid, sample, starts_with(input$habitat.levels), number) %>%
      dplyr::group_by(campaignid, sample, across(starts_with(input$habitat.levels))) %>%
      dplyr::tally(number, name = "number") %>%
      glimpse()
    
    names(dat)[3] = "levels"
    
    dat
    
  })
  
  number.of.levels <- reactive({

    base::length(base::unique(habitat.plot.data()$levels))
    
  })
  
  # ► habitat plot - choose levels ----
  observe({
    req(input$hab == "Yes" & !is.null(input$folderdir))

    output$habitat.broad.plot <- renderPlot(
      
      # message("number of levels")
      # print(number.of.levels())

      ggplot(habitat.plot.data()) +
        geom_quasirandom(data = habitat.plot.data(),
                         aes(x = number, y = levels), groupOnX = F, method = "quasirandom",
                         alpha = 0.25, size = 1.8, width = 0.2) +
        labs(x = "Number of points", y = "") +
        theme_classic()+
        Theme1
    , height = 25 * number.of.levels() + 10
    )
    
    output$broad.box <- renderUI({
      box(width = 12, title = "Broad habitat", status = "primary",
          height = 25 * number.of.levels() + 70,
          plotOutput("habitat.broad.plot"))
    })
    
  })
  

  ## ► habitat plot - relief  ----
  output$habitat.relief.plot <- renderPlot({
    
    dat <- tidy.relief() %>%
      dplyr::group_by(campaignid, sample, level_5) %>%
      dplyr::tally(number, name = "number") %>% 
      glimpse()
    
    ggplot(dat) +
      geom_quasirandom(data = dat,
                       aes(x = number, y = level_5), groupOnX = F, method = "quasirandom",
                       alpha = 0.25, size = 1.8, width = 0.2) +
      labs(x = "Number of points", y = "Relief (0-5)") +
      theme_classic()+ 
      Theme1
  })
  
  ## ► habitat plot - depth  ----
  output$habitat.depth.plot <- renderPlot({
    
    dat <- tidy.habitat() %>%
      dplyr::group_by(campaignid, sample, level_2) %>%
      dplyr::tally(number, name = "number") %>% 
      dplyr::left_join(metadata()) %>%
      tidyr::uncount(number) %>%
      dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
      dplyr::filter(!is.na(depth_m)) %>%
      glimpse() 
    
    ggplot(dat, aes(x = depth_m)) +
      geom_histogram(color = "black", fill = "#3C8DBC", binwidth = input$depth.bins)+
      facet_grid(level_2 ~ ., scales = "free_y") +
      labs(x = "Depth (m)", y = "Count") +
      theme_classic()+ 
      Theme1
  })


  # # ► Leaflet pies ----
  
  ## FIXME work out why pie charts are breaking the list
  # output$hab.pies <- renderLeaflet({
  # 
  #   dat <- tidy.habitat() %>%
  #     dplyr::group_by(campaignid, sample, level_2) %>%
  #     dplyr::tally(number, name = "number") %>%
  #     dplyr::ungroup()
  # 
  #   level_2s <- dat %>%
  #     dplyr::mutate(level_2 = tolower(str_replace_all(level_2, " ", "_")))
  # 
  #   number <- base::length(unique(level_2s$level_2))
  # 
  #   message("number of hab colours")
  #   print(number)
  # 
  #   colours <- brewer.pal(n = number, "Set1")
  # 
  #   level_2s <- unique(level_2s$level_2)
  # 
  #   hab <- dat %>%
  #     tidyr::pivot_wider(names_from = level_2, values_from = number) %>%
  #     dplyr::left_join(metadata()) %>%
  #     clean_names() %>%
  #     glimpse()
  # 
  #   # Create the plot
  #   leaflet() %>%
  #     addTiles() %>%
  #     addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  #     addLayersControl(baseGroups = c("Open Street Map", "World Imagery"),
  #                      options = layersControlOptions(collapsed = FALSE)) %>%
  #     leaflet.minicharts::addMinicharts(hab$longitude_dd, hab$latitude_dd,
  #                   type = "pie",
  #                   colorPalette = colours,
  #                   chartdata = hab[level_2s],
  #                   width = 20,
  #                   legend = TRUE,
  #                   legendPosition = "topright")
  # })

  ## ► Species dropdown ----
  output$hab.dropdown <- renderUI({

    options <- tidy.habitat() %>%
      dplyr::arrange(level_2) %>%
      distinct(level_2) %>%
      pull("level_2")

    create_dropdown("hab.dropdown", options, NULL)
  })

  ## ► Leaflet bubble ----
  output$hab.bubble <- renderLeaflet({

    hab <- tidy.habitat() %>%
      dplyr::group_by(campaignid, sample, level_2) %>%
      dplyr::tally(number, name = "number") %>% 
      dplyr::left_join(metadata())

    # Filter the data for plotting
    overzero <-  hab %>% # Any sample with a value greater than zero
      filter(level_2 %in% input$hab.dropdown & number > 0)

    equalzero <- hab %>% # Any sample with a value equal to zero
      filter(level_2  %in% input$hab.dropdown & number == 0)

    bubble.plot <- leaflet(data = hab) %>%
      addTiles() %>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addLayersControl(baseGroups = c("Open Street Map", "World Imagery"),
                       options = layersControlOptions(collapsed = FALSE))

    if (nrow(overzero)) {
      bubble.plot <- bubble.plot %>%
        addCircleMarkers(data = overzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
                         radius = ~(number/4) + 3,
                         fillOpacity = 0.5, stroke = FALSE, label = ~as.character(sample))
    }
    if (nrow(equalzero)) {
      bubble.plot <- bubble.plot %>%
        addCircleMarkers(data = equalzero, lat = ~ latitude_dd, lng = ~ longitude_dd,
                         radius = 2,
                         fillOpacity = 0.5, color = "white",stroke = FALSE, label = ~as.character(sample))
    }

    bubble.plot
  })

  ## _______________________________________________________ ----
  ##                         DOWNLOADS                       ----
  ## _______________________________________________________ ----

  ## _______________________________________________________ ----
  ##                  Single point campaigns                 ----
  ## _______________________________________________________ ----

  ## ► Download all files ----
  observeEvent(input$project.name, {
    if (input$project.name %in% c(NA, NULL, "")){
      shinyjs::disable("download.maxn")
    } else {
      shinyjs::enable("download.maxn")
      # showElement("element")
    }
  })


  output$download.maxn <- downloadHandler(

    filename = function() {
      #
      paste0(input$project.name, "_all-files_", Sys.Date(),'.zip')
    }, content = function(file) {

      on.exit(removeModal())

      # TODO add some css to make modals pretty

      if (TRUE){
        showModal(
          modalDialog(
            title = 'Downloading data...',
            includeMarkdown("markdown/downloading.md"),
            easyClose = FALSE,
            footer = NULL
          )
        )
        
        print("glimpse downloading data")
        # glimpse(maxn.complete.download())

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)

        # EVENTMEASURE ----
        if(input$upload %in% "EM"){
          for(i in unique(maxn.complete.download()$campaignid)){

            print(i)

            dat <- maxn.complete.download() %>%
              dplyr::rename(count = maxn) %>%
              dplyr::filter(campaignid == i) %>%
              dplyr::glimpse()

            fileName <- paste(i, "_count.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }

          for(i in unique(length.complete.download()$campaignid)){

            print("campaignid length")
            print(i)

            dat <- length.complete.download() %>%
              dplyr::filter(campaignid == i)# %>% glimpse()

            fileName <- paste(i, "_length.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }

          for(i in unique(mass.complete.download()$campaignid)){

            # print("campaignid mass")
            # print(i)

            dat <- mass.complete.download() %>%
              dplyr::filter(campaignid == i) #%>% glimpse()

            fileName <- paste(i, "_mass.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }

          if (input$error.zeros == FALSE) {

            if (input$error.extra.col == TRUE) {

              metadata <- metadata.regions() %>%
                dplyr::select(-c(zone, marine_region))

            } else {
              metadata <- metadata.regions()
            }

            metadata <- metadata

            for(i in unique(metadata$campaignid)){

              print("campaignid metadata")
              print(i)

              dat <- metadata %>%
                filter(campaignid == i) #%>% glimpse()

              fileName <- paste(i, "_metadata.csv", sep = "")

              write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
            }
          }

        } else {
          # GENERIC -----
          for(i in unique(count.complete.download()$campaignid)){
            
            print(i)
            
            dat <- count.complete.download() %>%
              dplyr::rename(count = maxn) %>%
              dplyr::filter(campaignid == i) %>%
              dplyr::glimpse()
            
            fileName <- paste(i, "_count.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          for(i in unique(gen.length.complete.download()$campaignid)){
            
            print("campaignid length")
            print(i)
            
            dat <- gen.length.complete.download() %>%
              dplyr::filter(campaignid == i)# %>% glimpse()
            
            fileName <- paste(i, "_length.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          for(i in unique(mass.complete.download()$campaignid)){

            # print("campaignid mass")
            # print(i)

            dat <- mass.complete.download() %>%
              dplyr::filter(campaignid == i) #%>% glimpse()

            fileName <- paste(i, "_mass.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          if (input$error.zeros == FALSE) {
            
            if (input$error.extra.col == TRUE) {
              
              metadata <- metadata.regions() %>%
                dplyr::select(-c(zone, marine_region))
              
            } else {
              metadata <- metadata.regions()
            }
            
            metadata <- metadata
            
            for(i in unique(metadata$campaignid)){
              
              print("campaignid metadata")
              print(i)
              
              dat <- metadata %>%
                dplyr::filter(campaignid == i) %>%
                dplyr::select(!sample)
              
              fileName <- paste(i, "_metadata.csv", sep = "")
              
              write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
            }
          }
          
        }


        #create the zip file
        zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)

      }}, contentType = "application/zip"
  )

  ## ► Habitat ----
  # TODO add habitat to all files

  # output$download.broad.habitat <- downloadHandler(
  #   filename = function() {
  #     paste(input$project.name, "_broad.habitat_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(habitat.broad.points(), file, row.names = FALSE)
  #   }
  # )

  ## ► All errors ----
  all.errors <- reactive({
    
    sample.cols <- c(opcode = NA_real_,
                     period = NA_real_)

    if(input$upload %in% "EM"){
      print("samples.without.points ")
      samples.without.points <- metadata.samples.without.fish() %>%
        mutate(error = "sample.without.points") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("samples.without.length")
      samples.without.length <- metadata.samples.without.length() %>%
        mutate(error = "sample.without.length") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("points.samples.without.metadata")
      points.samples.without.metadata <- points.samples.without.metadata() %>%
        mutate(error = "sample.in.points.without.metadata") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("length.samples.without.metadata")
      length.samples.without.metadata <- length.samples.without.metadata() %>%
        mutate(error = "sample.in.lengths.without.metadata") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("samples.without.periods")
      samples.without.periods <- samples.without.periods()%>%
        mutate(error = "sample.without.period") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("periods.no.end")
      periods.no.end <- periods.no.end() %>%
        mutate(error = "period.with.no.end") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("periods.wrong")
      periods.wrong <- periods() %>%
        distinct(campaignid, sample, period, time_start, time_end, has_end) %>%
        mutate(period_time = round(time_end - time_start)) %>%
        filter(!period_time %in% c(input$error.period.length)) %>%
        mutate(error = "period.wrong.length") %>%
        mutate(across(everything(), as.character))# %>% glimpse()

      print("points.outside.periods")
      points.outside.periods <- points.outside.periods() %>%
        mutate(error = "point.outside.period") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("lengths.outside.periods")
      lengths.outside.periods <- lengths.outside.periods() %>%
        mutate(error = "length.or.3D.point.outside.period") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("points.no.number")
      points.no.number <- points.no.number() %>%
        mutate(error = "point.without.a.number") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("lengths.no.number")
      lengths.no.number <- lengths.no.number() %>%
        mutate(error = "length.without.a.number") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("3d.no.number")
      threedpoints.no.number <- threedpoints.no.number() %>%
        mutate(error = "3D.point.without.a.number") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("maxn.species.not.observed")
      maxn.species.not.observed <- maxn.species.not.observed() %>%
        mutate(error = "species.not.observed.in.region.before") %>%
        mutate(across(everything(), as.character))  #%>% glimpse()

      print("maxn.species.not.in.list")
      maxn.species.not.in.lh <- maxn.species.not.observed.lh() %>%
        mutate(error = "species.not.in.life.history.sheet") %>%
        mutate(across(everything(), as.character))  #%>% glimpse()

      print("length.species.not.observed")
      length.species.not.observed <- length.species.not.observed() %>%
        mutate(error = "species.not.observed.in.region.before") %>%
        mutate(across(everything(), as.character))  #%>% glimpse()

      print("length.species.not.in.list")
      length.species.not.in.lh <- length.species.not.observed.lh() %>%
        mutate(error = "species.not.in.life.history.sheet") %>%
        mutate(across(everything(), as.character))  #%>% glimpse()

      range.limit <- (input$error.report.range*1000)

      print("length.out.of.range")
      length.out.of.range <- length3dpoints() %>%
        dplyr::filter(range>range.limit) %>%
        dplyr::select(campaignid, sample, family, genus, species, range, frame_left, frame_right, em_comment) %>%
        mutate(error = "out.of.range") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("length.wrong.small")
      length.wrong.small <- length.wrong() %>%
        dplyr::filter(reason%in%c("too small")) %>%
        dplyr::mutate(error = reason) %>%
        mutate(across(everything(), as.character))

      print("length.wrong.big")
      length.wrong.big <- length.wrong() %>%
        dplyr::filter(reason%in%c("too big")) %>%
        dplyr::mutate(error = reason)  %>%
        mutate(across(everything(), as.character))

      rms.limit <- (input$error.report.rms)

      print("length.wrong.rms")
      length.wrong.rms <- length3dpoints() %>%
        dplyr::filter(rms > rms.limit) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)%>%
        mutate(error = "over.rms")  %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      precision.limit <- (input$error.report.precision)

      print("length.wrong.precision")
      length.wrong.precision <- length3dpoints() %>%
        dplyr::mutate(precision_percent = (precision/length_mm)*100) %>%
        dplyr::filter(precision_percent > precision.limit) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, precision_percent, code)%>%
        mutate(error = "over.precision")  %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      print("stereo.maxn.does.not.equal.maxn")
      stereo.maxn.does.not.equal.maxn <- length.vs.maxn() %>%
        dplyr::mutate(count = 1) %>%
        dplyr::filter(!length_maxn == maxn) %>%
        dplyr::filter(!percent_difference%in%c(0)) %>%
        mutate(error = "stereo.maxn.does.not.equal.maxn") %>%
        mutate(across(everything(), as.character)) #%>% glimpse()

      # all errors
      print("all errors")


      if(input$periods %in% "yes") {
        
        all.errors <- bind_rows(samples.without.points,
                                samples.without.length,
                                points.samples.without.metadata,
                                length.samples.without.metadata,
                                
                                samples.without.periods,
                                periods.no.end,
                                periods.wrong,
                                points.outside.periods,
                                lengths.outside.periods,
                                
                                points.no.number,
                                lengths.no.number,
                                threedpoints.no.number,
                                
                                maxn.species.not.observed,
                                maxn.species.not.in.lh,
                                
                                length.species.not.observed,
                                length.species.not.in.lh,
                                
                                length.out.of.range,
                                length.wrong.rms,
                                length.wrong.precision,
                                
                                length.wrong.small,
                                length.wrong.big,
                                
                                stereo.maxn.does.not.equal.maxn) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), error, family, genus, species, number, length_mm, frame, frame_left, range, min_length, max_length, fb_length_max, em_comment, rms, precision, code) %>%
          distinct() %>%
          tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
          arrange(campaignid, opcode, period) %>%
          dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA
        
      } else {
      
      all.errors <- bind_rows(samples.without.points,
                              samples.without.length,
                              points.samples.without.metadata,
                              length.samples.without.metadata,

                              points.no.number,
                              lengths.no.number,
                              threedpoints.no.number,

                              maxn.species.not.observed,
                              maxn.species.not.in.lh,

                              length.species.not.observed,
                              length.species.not.in.lh,

                              length.out.of.range,
                              length.wrong.rms,
                              length.wrong.precision,

                              length.wrong.small,
                              length.wrong.big,

                              stereo.maxn.does.not.equal.maxn) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), error, family, genus, species, number, length_mm, frame, frame_left, range, min_length, max_length, fb_length_max, em_comment, rms, precision, code) %>%
        distinct() %>%
        tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
        arrange(campaignid, opcode, period) %>%
        dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA
      }

      
      
      

    } else {
      # ERRORS FOR GENERIC ----

      print("samples.without.points ")
      samples.without.points <- metadata.samples.without.fish() %>%
        mutate(error = "sample.without.points") %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("samples.without.length")
      samples.without.length <- metadata.samples.without.length() %>%
        mutate(error = "sample.without.length") %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("count.samples.without.metadata")
      points.samples.without.metadata <- points.samples.without.metadata() %>%
        mutate(error = "sample.in.count.without.metadata") %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("length.samples.without.metadata")
      length.samples.without.metadata <- length.samples.without.metadata() %>%
        mutate(error = "sample.in.lengths.without.metadata") %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("count.species.not.observed")
      maxn.species.not.observed <- maxn.species.not.observed() %>%
        mutate(error = "species.not.observed.in.region.before")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("maxn.species.not.in.list")
      maxn.species.not.in.lh <- maxn.species.not.observed.lh() %>%
        mutate(error = "species.not.in.life.history.sheet")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("length.species.not.observed")
      length.species.not.observed <- length.species.not.observed() %>%
        mutate(error = "species.not.observed.in.region.before")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("length.species.not.in.list")
      length.species.not.in.lh <- length.species.not.observed.lh() %>%
        mutate(error = "species.not.in.life.history.sheet")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      print("length.wrong.small")
      length.wrong.small <- length.wrong() %>%
        dplyr::filter(reason%in%c("too small")) %>%
        mutate(error = reason) %>%
        dplyr::mutate(error = as.character(error))%>%
        mutate(across(everything(), as.character))

      print("length.wrong.big")
      length.wrong.big <- length.wrong() %>%
        dplyr::filter(reason%in%c("too big")) %>%
        mutate(error = reason)%>%
        dplyr::mutate(error = as.character(error))%>%
        mutate(across(everything(), as.character))

      print("stereo.maxn.does.not.equal.maxn")
      stereo.maxn.does.not.equal.maxn <- length.vs.maxn() %>%
        dplyr::mutate(count = 1) %>%
        dplyr::filter(!length_maxn == maxn) %>%
        dplyr::filter(!percent_difference%in%c(0)) %>%
        mutate(error = "stereo.maxn.does.not.equal.maxn") %>%
        mutate(across(everything(), as.character))#%>% glimpse()

      # all errors
      print("all errors")
      all.errors <- bind_rows(samples.without.points,
                              samples.without.length,
                              points.samples.without.metadata,
                              length.samples.without.metadata,

                              maxn.species.not.observed,
                              maxn.species.not.in.lh,

                              length.species.not.observed,
                              length.species.not.in.lh,

                              length.wrong.small,
                              length.wrong.big,

                              stereo.maxn.does.not.equal.maxn) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), error, family, genus, species, length_mm, min_length, max_length, fb_length_max) %>%
        distinct() %>%
        tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
        arrange(campaignid, opcode, period) %>%
        dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA


  }
    # Remember to make this distinct!
    # TODO For transect version need to include those outside of transect
  })

  output$download.all.errors <- downloadHandler(
    filename = function() {
      paste(input$project.name, "_all.errors_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(all.errors(), file, row.names = FALSE, na = "")
    }
  )

  ## _______________________________________________________ ----
  ##                  Transect based campaigns                 ----
  ## _______________________________________________________ ----
  ## ► Download all files ----
  observeEvent(input$project.name.t, {
    if (input$project.name.t %in% c(NA, NULL, "")){
      shinyjs::disable("download.transect")
    } else {
      shinyjs::enable("download.transect")
      # showElement("element")
    }
  })

  output$download.transect <- downloadHandler(

    filename = function() {
      #
      paste0(input$project.name.t, "_all-files_", Sys.Date(),'.zip')
    }, content = function(file) {

      on.exit(removeModal())

      # TODO add some css to make modals pretty

      message("downloading data")
      # print(unique(length.complete.download.t()$campaignid))

      if (TRUE){
        showModal(
          modalDialog(
            title = 'Downloading data...',
            includeMarkdown("markdown/downloading.md"),
            easyClose = FALSE,
            footer = NULL
          )
        )

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)

        if(input$upload %in% "EM"){
          for(i in unique(length.complete.download.t()$campaignid)){

            print(i)

            dat <- length.complete.download.t() %>%
              dplyr::filter(campaignid == i) #%>% glimpse()

            fileName <- paste(i, "_length.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }

          for(i in unique(mass.complete.download.t()$campaignid)){

            dat <- mass.complete.download.t() %>%
              dplyr::filter(campaignid == i)

            fileName <- paste(i, "_mass.csv", sep = "")

            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }

          if (input$error.zeros.t == FALSE) {

            if (input$error.extra.col.t == TRUE) {

              metadata <- metadata.regions() %>%
                dplyr::select(-c(zone, marine_region))

            } else {
              metadata <- metadata.regions()
            }

            metadata <- metadata

            for(i in unique(metadata$campaignid)){

              print("campaignid metadata")
              print(i)

              dat <- metadata %>%
                filter(campaignid == i) #%>% glimpse()

              fileName <- paste(i, "_metadata.csv", sep = "")

              write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
            }
          }

        }

        #create the zip file
        zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)

      }}, contentType = "application/zip"
  )

  ## ► Habitat ----
  # TODO add habitat to all files download
  output$download.broad.habitat.t <- downloadHandler(
    filename = function() {
      paste(input$project.name.t, "_broad.habitat_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(habitat.broad.points(), file, row.names = FALSE)
    }
  )

  ## ► All errors ----
  all.errors.t <- reactive({
    
    sample.cols <- c(opcode = NA_real_,
                     period = NA_real_)
    
    
    metadata.samples.without.lengths.t <- metadata.samples.without.fish.t() %>%
      mutate(error = "samples.without.lengths") %>%
      mutate(across(everything(), as.character))

    metadata.samples.without.3dpoints.t <- metadata.samples.without.3dpoints.t() %>%
      mutate(error = "samples.without.3D.points") %>%
      mutate(across(everything(), as.character))

    length.samples.without.metadata.t <- length.samples.without.metadata.t() %>%
      mutate(error = "samples.without.metadata") %>%
      mutate(across(everything(), as.character))

    samples.without.periods.t <- samples.without.periods.t()%>%
      mutate(error = "sample.without.period") %>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    periods.no.end.t <- periods.no.end.t() %>%
      mutate(error = "period.with.no.end")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    points.outside.periods.t <- points.outside.periods.t() %>%
      mutate(error = "point.outside.period")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    lengths.outside.periods.t <- lengths.outside.periods.t() %>%
      mutate(error = "length.or.3D.point.outside.period")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    lengths.no.number.t <- lengths.no.number.t() %>%
      mutate(error = "length.without.a.number")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    threedpoints.no.number.t <- threedpoints.no.number.t() %>%
      mutate(error = "3D.point.without.a.number")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    length.species.not.observed.t <- length.species.not.observed.t() %>%
      mutate(error = "species.not.observed.in.region.before")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    length.species.not.observed.t.lh <- length.species.not.observed.t.lh() %>%
      mutate(error = "species.not.in.life.history.sheet")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    range.limit <- (input$error.report.range.t*1000)

    length.out.of.range.t <- length3dpoints.t() %>%
      dplyr::filter(range>range.limit) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, frame_left, frame_right, em_comment) %>%
      mutate(error = "out.of.range")%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    length.wrong.small.t <- length.wrong.t() %>%
      dplyr::filter(reason %in% c("too small")) %>%
      mutate(error = reason)%>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    length.wrong.big.t <- length.wrong.t() %>%
      dplyr::filter(reason %in% c("too big")) %>%
      mutate(error = reason) %>%
      # glimpse()%>%
      mutate(across(everything(), as.character))

    transect.limit <- (input$error.report.transect.t*1000)/2

    length.out.of.transect.t <- length3dpoints.t() %>%
      dplyr::filter(c(midx > transect.limit | midx < transect.limit | midy > transect.limit | midy < -transect.limit | x > transect.limit | x < -transect.limit | y > transect.limit | y < -transect.limit)) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, length_mm, frame_left, frame_right, midx, midy, x, y, em_comment) %>%
      dplyr::mutate(error = "out.of.transect")%>%
      mutate(across(everything(), as.character))

    rms.limit <- (input$error.report.rms.t)

    length.wrong.rms <- length3dpoints.t() %>%
      dplyr::filter(rms > rms.limit) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)%>%
      mutate(error = "over.RMS")%>%
      #glimpse()%>%
      mutate(across(everything(), as.character))

    precision.limit <- (input$error.report.precision.t)

    length.wrong.precision <- length3dpoints.t() %>%
      dplyr::mutate(precision_percent = (precision/length_mm)*100) %>%
      dplyr::filter(precision_percent > precision.limit) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, precision_percent, code)%>%
      mutate(error = "over.precision")%>%
      #glimpse()%>%
      mutate(across(everything(), as.character))

    # points.samples.without.metadata.t,
    all.errors <- bind_rows(metadata.samples.without.lengths.t,
                            metadata.samples.without.3dpoints.t,
                            length.samples.without.metadata.t,

                            samples.without.periods.t,
                            periods.no.end.t,
                            points.outside.periods.t,
                            lengths.outside.periods.t,

                            lengths.no.number.t,
                            threedpoints.no.number.t,

                            length.species.not.observed.t,
                            length.species.not.observed.t.lh,

                            length.out.of.range.t,
                            length.out.of.transect.t,
                            length.wrong.small.t,
                            length.wrong.big.t,
                            length.wrong.rms,
                            length.wrong.precision) %>%
      
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), error, family, genus, species, number, length_mm, frame, frame_left, range, min_length, max_length, fb_length_max, em_comment, rms, precision) %>%
      distinct() %>%
      tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
      arrange(campaignid, opcode, period) %>%
      dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA

    # Remember to make this distinct!
    # For transect version need to include those outside of transect
  })

  output$download.all.errors.t <- downloadHandler(
    filename = function() {
      paste(input$project.name.t, "_all.errors_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(all.errors.t(), file, row.names = FALSE, na = "")
    }
  )

  # Download schemas
  output$schema.fish <- downloadHandler(
    filename = function() {
      paste("fish.life.history_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(all_data$schema.fish, file, row.names = FALSE, na = "", sep = "\t", quote = FALSE)
    }
  )

  output$schema.relief <- downloadHandler(
    filename = function() {
      paste("benthic.relief_annotation.schema.forward.facing_", Sys.Date(),".txt", sep = "")
    },
    content = function(file) {
      write.table(all_data$schema.relief, file, row.names = FALSE, na = "", sep = "\t", quote = FALSE)
    }
  )

  output$schema.habitat <- downloadHandler(
    filename = function() {
      paste("benthic.habitat.annotation.schema.forward.facing_", Sys.Date(),".txt", sep = "")
    },
    content = function(file) {
      write.table(all_data$schema.habitat, file, row.names = FALSE, na = "", sep = "\t", quote = FALSE)
    }
  )

  ## _______________________________________________________ ----
  ##                    MAPPING FOR GUIDE                    ----
  ## _______________________________________________________ ----
  ## ► Leaflet map - World regions ----
  output$regions.leaflet <- renderLeaflet({

    map <- leaflet() %>%
      addTiles(group = "Open Street Map") %>%
      addPolygons(data = world.regions.display,
                  popup = world.regions.display$NAME_EN,
                  fillColor = "white",
                  color = "black",
                  fillOpacity = 0.9,
                  group = "FAO major fishing areas") %>%

      # addGlPolylines(data = world.regions.display,
      #               weight = 1,
      #               label = world.regions.display$NAME_EN,
      #               color = "black",
      #               group = "FAO major fishing areas") %>%

      addPolygons(data = all_data$marine.regions,
                  weight = 1,
                  label = all_data$marine.regions@data$REGION,
                  fillColor = "green",
                  color = "black",
                  fillOpacity = 0.9,
                  group = "Australian Marine Regions") %>%

      hideGroup("FAO major fishing areas") %>%

      addLayersControl(
        overlayGroups = c("Australian Marine Regions",
                          "FAO major fishing areas"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%

      fitBounds(-180, -90, 180, 90)

    return(map)

  })

  # Link between inputs ----
  observeEvent(input$period.limit, {
    updateNumericInput(session, "error.period.length", value = input$period.limit)
  })

  observeEvent(input$rms.limit, {
    updateNumericInput(session, "error.report.rms", value = input$rms.limit)
  })

  observeEvent(input$rms.limit, {
    updateNumericInput(session, "error.rms.limit", value = input$rms.limit)
  })

  observeEvent(input$precision.limit, {
    updateNumericInput(session, "error.report.precision", value = input$precision.limit)
  })

  observeEvent(input$precision.limit, {
    updateNumericInput(session, "error.precision.limit", value = input$precision.limit)
  })

  observeEvent(input$range.limit, {
    updateNumericInput(session, "error.report.range", value = input$range.limit)
  })

  observeEvent(input$range.limit, {
    updateNumericInput(session, "error.range.limit", value = input$range.limit)
  })


  ## Transect based
  observeEvent(input$rms.limit.t, {
    updateNumericInput(session, "error.report.rms.t", value = input$rms.limit.t)
  })

  observeEvent(input$rms.limit.t, {
    updateNumericInput(session, "error.rms.limit.t", value = input$rms.limit.t)
  })

  observeEvent(input$precision.limit.t, {
    updateNumericInput(session, "error.report.precision.t", value = input$precision.limit.t)
  })

  observeEvent(input$precision.limit.t, {
    updateNumericInput(session, "error.precision.limit.t", value = input$precision.limit.t)
  })

  observeEvent(input$range.limit.t, {
    updateNumericInput(session, "error.report.range.t", value = input$range.limit.t)
  })

  observeEvent(input$range.limit.t, {
    updateNumericInput(session, "error.range.limit.t", value = input$range.limit.t)
  })

  observeEvent(input$transect.limit.t, {
    updateNumericInput(session, "error.report.transect.t", value = input$transect.limit.t)
  })

  observeEvent(input$transect.limit.t, {
    updateNumericInput(session, "error.transect.limit.t", value = input$transect.limit.t)
  })

  ## _______________________________________________________ ----
  ##                    GENERIC COUNT                        ----
  ## _______________________________________________________ ----
  ## ► Read in count data ----
  count <- reactive({
    # When folder chosen ----
    if(!is.null(input$folderdir)) {

      # Get all _Count.csv files in the folder
      files <- input$folderdir%>%
        dplyr::filter(grepl("_Count.csv", name))

      count <- data.frame()

      if (is.null(files)) return(NULL)

      for (i in seq_along(files$datapath)) {
        tmp <- read_csv(files$datapath[i], col_types = cols(.default = "c"))  %>%
          dplyr::mutate(campaignid = files$name[i])

        count <- bind_rows(count, tmp)

        if("CampaignID" %in% colnames(count))
        {
          count <- count %>%
            dplyr::select(-c(CampaignID))
        }
      }

      count <- count %>%
        clean_names() %>%
        dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Count.csv" = "")))
    }
    
    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {
      
      count <- count %>%
        dplyr::mutate(sample = opcode)
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {
      
      count <- count %>%
        dplyr::mutate(sample = period)
    }
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {
      
      count <- count %>%
        dplyr::mutate(sample = paste(opcode, period, sep = "_"))
      
    }
    
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {
      
      count <- count %>%
        dplyr::mutate(sample = period)
    }

    count <- count %>%
      mutate(sample = as.factor(sample)) %>%
      mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "spp."), "spp", as.character(species))) %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::mutate(species = as.character(tolower(species))) %>%
      dplyr::mutate(genus = as.character(ga.capitalise(genus))) %>%
      dplyr::mutate(family = as.character(ga.capitalise(family))) %>%
      dplyr::left_join(all_data$lh.aus)

  })

  ## ► Create Count (Raw) ----
  count.raw <- reactive({
    #TODO add code column with lifehistory sheet
    maxn <- count() %>%
      dplyr::mutate(count = as.numeric(count)) %>%
      replace_na(list(family = "Unknown", genus = "Unknown", species = "spp")) %>% # remove any NAs in taxa name
      dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
      dplyr::summarise(maxn = sum(count)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
      dplyr::slice(which.max(maxn)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(maxn)) %>%
      tidyr::replace_na(list(maxn = 0)) %>%
      dplyr::mutate(maxn = as.numeric(maxn)) %>%
      dplyr::filter(maxn > 0) %>%
      dplyr::inner_join(metadata.regions()) %>%
      dplyr::mutate(family = ifelse(family%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      dplyr::mutate(genus = ifelse(genus%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      dplyr::mutate(species = ifelse(species%in%c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
      dplyr::filter(successful_count %in% c("Yes", "Y", "y", "yes")) %>%
      dplyr::mutate(species = as.character(species)) %>%
      dplyr::mutate(genus = as.character(genus)) %>%
      dplyr::mutate(family = as.character(family)) %>%
      filter(!family %in% c("Unknown"))#%>% glimpse()

  })

  count.clean <- reactive({

    count.clean <- dplyr::full_join(count.raw(), metadata.regions()) %>%
      dplyr::left_join(., synonyms()) %>%
      dplyr::mutate(genus = ifelse(!genus_correct %in% c(NA), genus_correct, genus)) %>%
      dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
      dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
      dplyr::slice(which.max(maxn)) %>%
      dplyr::ungroup() %>%
      as_tibble()
  })

  ## ►  Create MaxN (Complete) -----
  count.complete <- reactive({

    print("count.complete")

    count.complete <- count.clean() %>%
      dplyr::full_join(metadata.regions()) %>%
      dplyr::select(c(campaignid, sample, family, genus, species, maxn, code)) %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(maxn = 0)) %>%
      dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
      dplyr::summarise(maxn = sum(maxn)) %>%
      dplyr::ungroup() #%>% glimpse()

  })

  ## ► Create filtered Count download -----
  count.complete.download <- reactive({

    if(!input$upload %in% "EM"){

      count <- full_join(count.raw(), metadata.regions()) # can't use clean as have already changed synonyms

      if (input$error.synonyms == TRUE) {
        count.complete <- dplyr::left_join(count, synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
          dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
          dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
          dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
          dplyr::slice(which.max(maxn)) %>%
          dplyr::ungroup() %>%
          dplyr::full_join(metadata.regions()) %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn, code)) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
          dplyr::summarise(maxn = sum(maxn)) %>%
          ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::mutate(scientific = paste(genus, species, sep = " "))

      } else {

        count.complete <- count %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn, code)) %>%
          dplyr::full_join(metadata.regions()) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
          dplyr::summarise(maxn = sum(maxn)) %>%
          ungroup() %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::mutate(scientific = paste(genus, species, sep = " "))
      }

      count.complete <- count.complete

      species.out.of.area <- life.history.expanded() %>%
        anti_join(count.clean(), ., by = c("family", "genus", "species", "marine_region")) %>%
        distinct(family, genus, species, marine_region) %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))

      # If "Remove species not observed in the area before" = FALSE, keep species, TRUE = remove
      if (input$error.area == FALSE) {
        count.area <- count.complete
      } else {
        count.area <- anti_join(count.complete, species.out.of.area)}

      # If "Remove extra columns" = TRUE
      if (input$error.extra.col == TRUE) {
        count.area <- count.area %>%
          dplyr::select(-c(zone, marine_region, scientific))}

      if (input$error.zeros == TRUE) {
        count.area <- count.area %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything()) %>%
          dplyr::select(!sample)

      } else {

        count.area <- count.area %>%
          dplyr::filter(!maxn %in% 0) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, code, maxn)} # remove metadata columns

      count.area <- count.area %>%
        dplyr::filter(!family %in% c("", NA, NULL))

    }

  })

  ## _______________________________________________________ ----
  ##                    GENERIC LENGTH                        ----
  ## _______________________________________________________ ----
  ## ► Read in length data ----
  gen.length <- reactive({
    # When folder chosen ----
    if(!is.null(input$folderdir)) {

      # Get all _Count.csv files in the folder
      files <- input$folderdir%>%
        dplyr::filter(grepl("_Length.csv", name))

      gen.length <- data.frame()

      if (is.null(files)) return(NULL)

      for (i in seq_along(files$datapath)) {
        tmp <- read_csv(files$datapath[i], col_types = cols(.default = "c"))  %>%
          dplyr::mutate(campaignid = files$name[i])

        gen.length <- bind_rows(gen.length, tmp)

        if("CampaignID" %in% colnames(gen.length))
        {
          gen.length <- gen.length %>%
            dplyr::select(-c(CampaignID))
        }
      }

      gen.length <- gen.length %>%
        clean_names() %>%
        dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Length.csv" = "")))
    }
    
    lookup <- c(length_mm = "length") # If user has used old length col will change to new length col
    gen.length <- gen.length %>% dplyr::rename(dplyr::any_of(lookup))
    #print("gen length")
    
    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {
      
      gen.length <- gen.length %>%
        dplyr::mutate(sample = opcode)
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {
      
      gen.length <- gen.length %>%
        dplyr::mutate(sample = period)
    }
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {
      
      gen.length <- gen.length %>%
        dplyr::mutate(sample = paste(opcode, period, sep = "_"))
      
    }
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {
      
      gen.length <- gen.length %>%
        dplyr::mutate(sample = period)
    }
    
    gen.length <- gen.length %>%
      mutate(sample = as.factor(sample)) %>%
      mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "spp."), "spp", as.character(species))) %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::mutate(species = as.character(tolower(species))) %>%
      dplyr::mutate(genus = as.character(ga.capitalise(genus))) %>%
      dplyr::mutate(family = as.character(ga.capitalise(family))) %>%
      dplyr::rename(number = count) %>%
      dplyr::mutate(number = as.numeric(number)) %>%
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number) %>%
      dplyr::left_join(all_data$lh.aus) %>%
      filter(!family %in% c("Unknown"))#%>% glimpse()

  })

  gen.length.clean <- reactive({

    print("gen length clean")
    gen.length.clean <-  dplyr::left_join(gen.length(), synonyms()) %>% #, by = c("family", "genus", "species")
      dplyr::mutate(genus = ifelse(!genus_correct %in% c(NA), genus_correct, genus)) %>%
      dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
      dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
      dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
      dplyr::full_join(metadata.regions()) %>% # add in all samples
      dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) %>%
      # glimpse() %>%
      tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
      replace_na(list(number = 0)) %>% # we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
      dplyr::ungroup() %>%
      dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) %>%
      dplyr::filter(!is.na(family))  #%>% glimpse()

  })

  ## ► Create filtered length download -----
  gen.length.complete.download <- reactive({

    if(!input$upload %in% "EM"){
      print("preview length data for downloading")
      length <- gen.length() %>% 
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) %>% glimpse() # can't use clean as have already changed synonyms

      if (input$error.synonyms == TRUE) {
        length.complete <- dplyr::left_join(length, synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
          dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
          dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
          dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
          dplyr::right_join(metadata.regions()) %>% # add in all samples
          dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
          dplyr::ungroup() %>%
          # dplyr::mutate(length = as.numeric(length)) %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::mutate(marine_region = as.character(marine_region)) %>%
          dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes"))
      }
      else{
        length.complete <- dplyr::left_join(length, synonyms()) %>% #, by = c("family", "genus", "species")
          dplyr::right_join(metadata.regions()) %>% # add in all samples
          dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) %>%
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
          replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
          dplyr::ungroup() %>%
          # dplyr::mutate(length = as.numeric(length)) %>%
          dplyr::left_join(metadata.regions()) %>%
          dplyr::filter(successful_length %in% c("Yes", "Y", "y", "yes")) %>%
          dplyr::mutate(marine_region = as.character(marine_region))
      }

      print("complete length")
      length.complete <- length.complete %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        glimpse()

      species.out.of.area <- life.history.expanded() %>%
        dplyr::mutate(marine_region = as.character(marine_region)) %>%
        anti_join(length.complete, ., by = c("family", "genus", "species", "marine_region")) %>%
        distinct(family, genus, species, marine_region) %>%
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))

      if (input$error.area == FALSE) {
        length.area <- length.complete
      }
      else{
        length.area <- anti_join(length.complete, species.out.of.area)
      }

      length.wrong <- left_join(length.area, life.history.min.max(), by = c("family", "genus", "species")) %>%
        dplyr::filter(length_mm<min_length|length_mm>fb_length_max) %>%
        mutate(reason = ifelse(length_mm<min_length, "too small", "too big"))

      length.too.small <- length.wrong %>%
        dplyr::filter(reason%in%c("too small"))

      length.too.big <- length.wrong %>%
        dplyr::filter(reason%in%c("too big"))

      if (input$error.length.small == TRUE) {
        length.small <- anti_join(length.area, length.too.small)
      }
      else{
        length.small <- length.area
      }

      length.small <- length.small

      if (input$error.length.big == TRUE) {
        length.big <- anti_join(length.small, length.too.big)
      }
      else{
        length.big <- length.small
      }

      print("another test")
      length.big <- length.big %>%
        dplyr::right_join(metadata.regions()) %>% # add in all samples
        dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(number = 0)) %>%
        # dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
        dplyr::left_join(metadata.regions()) %>%
        filter(!is.na(family)) %>%
        dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
        glimpse()

      # If "Remove extra columns" = TRUE
      if (input$error.extra.col == TRUE) {
        length.big <- length.big %>%
          dplyr::select(-c(zone, marine_region, scientific))#%>% glimpse()
      }

      if (input$error.zeros == TRUE) {
        length.big <- length.big %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), everything()) %>%
          dplyr::select(!sample)

      } else {
        length.big <- length.big %>%
          filter(!number %in% 0)%>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, number, code) # remove metadata columns
      }

      print("final length data for downloading")
      length.big <- length.big %>%  glimpse()
    }
  })
}