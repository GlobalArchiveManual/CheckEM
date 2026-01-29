function(input, output, session) {
  
  # TODO change the example dataset to 2022-05 Point cloates BRUVs
  
  # Increase size of files that can be uploaded
  options(shiny.maxRequestSize = 50*1024^2)
  
  # # 1) Apply URL query (?tab=...) to the active sidebar tab
  # observeEvent(session$clientData$url_search, ignoreInit = FALSE, {
  #   query <- parseQueryString(session$clientData$url_search)
  #   tab <- query[["tab"]]
  #   if (!is.null(tab) && nzchar(tab)) {
  #     updateTabItems(session, "tabs", tab)
  #   }
  # })
  # 
  # # 2) When the user changes tabs, reflect it in the URL
  # observeEvent(input$tabs, ignoreInit = TRUE, {
  #   # Merge/replace the ?tab=... parameter without reloading the page
  #   existing <- parseQueryString(session$clientData$url_search)
  #   existing[["tab"]] <- input$tabs
  #   qs <- paste(
  #     names(existing),
  #     vapply(existing, URLencode, FUN.VALUE = character(1), reserved = TRUE),
  #     sep = "=", collapse = "&"
  #   )
  #   updateQueryString(paste0("?", qs), mode = "push")
  # })

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
    
    if(input$lifehistory %in% c("aus")){
      lh <- all_data$lh.aus 
      
    } else if (input$lifehistory %in% c("imcra")){
      
      lh <- all_data$lh.aus %>%
        dplyr::mutate(marine_region = imcra_region)
      
    } else {
      lh <- all_data$lh.glo %>%
        dplyr::mutate(length_max_mm = 10 * length.max) %>%
        dplyr::mutate(length_max_type = "") %>% # TODO change this to actual data
        dplyr::mutate(code = "")# TODO change this to actual data
    }
    
    lh
    
  })
  
  life.history.expanded <- reactive({
    
    if(input$lifehistory %in% c("aus")){
      lh <- all_data$lh.aus.expanded
      
    } else if (input$lifehistory %in% c("imcra")){
      
      lh <- all_data$lh.imcra.expanded
      
    } else {
      lh <- all_data$lh.glo.expanded %>%
        dplyr::mutate(length_max_mm = 10 * length.max) %>%
        dplyr::mutate(length_max_type = "") %>% # TODO change this to actual data
        dplyr::mutate(code = "")# TODO change this to actual data
    }
    
    lh
    
  })
  
  life.history.min.max <- reactive({
    
    if(input$lifehistory %in% c("aus", "imcra")){
      lh <- all_data$lh.aus.min.max
    } else {
      lh <- all_data$lh.glo.min.max %>%
        dplyr::mutate(length_max_mm = 10 * length.max) %>%
        dplyr::mutate(length_max_type = "") # TODO change this to actual data
    }
    
    lh
    
  })
  
  ## _______________________________________________________ ----
  ##                        CREATE LAND SHAPEFILE              ----
  ## _______________________________________________________ ----
  
  land <- reactive({
    
    if(input$lifehistory %in% c("aus", "imcra")){
      land <- all_data$aus
    } else {
      land <- all_data$world
    }
    
    land
    
  })
  
  ## _______________________________________________________ ----
  ##                     CREATE SYNONYMS                     ----
  ## _______________________________________________________ ----
  synonyms <- reactive({
    
    #message("view synonyms list")
    
    if(input$lifehistory %in% c("aus", "imcra")){
      
      lh <- all_data$lh.aus.synonyms %>% 
        dplyr::distinct() #%>% dplyr::glimpse()
      
    } else {
      
      lh <- all_data$lh.glo.synonyms %>% 
        dplyr::distinct() %>% 
        dplyr::select(family_correct, genus_correct, species_correct, family, genus, species) #%>% dplyr::glimpse() 
      
    }
    
    lh
    
  })
  
  ## _______________________________________________________ ----
  ##            CREATE QUALTIY CONTROL SCORE PLOT            ----
  ## _______________________________________________________ ----
  
  # TODO make these work out per campaign not per upload!
  
  ## ► 1. Metadata format score ----
  # % of metadata rows that matches the metadata standard
  
  #### ~ table ----
  metadata_format_table <- reactive({
    
    cols_to_add <- c(
      passed = NA_real_,
      failed = NA_real_)
    
    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {
      
      # message("view metadata_format_table")
      
      metadata_format <- metadata() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = metadata_score, values_from = n) %>%
        tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
        tidyr::replace_na(list(failed = 0, passed = 0)) %>%
        dplyr::mutate(total = failed + passed) %>%
        dplyr::mutate(metadata_format = ((total-failed)/total)*100) %>%
        dplyr::select(campaignid, metadata_format) #%>% dplyr::glimpse()
      
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {
      
      metadata_format <- metadata() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = metadata_score, values_from = n) %>%
        tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
        tidyr::replace_na(list(failed = 0, passed = 0)) %>%
        dplyr::mutate(total = failed + passed) %>%
        dplyr::mutate(metadata_format = ((total-failed)/total)*100) %>%
        dplyr::select(campaignid, metadata_format)
      
    }
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {
      
      metadata_format <- metadata() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = metadata_score, values_from = n) %>%
        tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
        tidyr::replace_na(list(failed = 0, passed = 0)) %>%
        dplyr::mutate(total = failed + passed) %>%
        dplyr::mutate(metadata_format = ((total-failed)/total)*100) %>%
        dplyr::select(campaignid, metadata_format)
      
    }
    
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {
      
      metadata_format <- metadata() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = metadata_score, values_from = n) %>%
        tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
        tidyr::replace_na(list(failed = 0, passed = 0)) %>%
        dplyr::mutate(total = failed + passed) %>%
        dplyr::mutate(metadata_format = ((total-failed)/total)*100) %>%
        dplyr::select(campaignid, metadata_format)
      
    }
    
    metadata_format
    
  })
  
  #### ~ score ----
  metadata_format_score <- reactive({
    
    # If point method and samples are opcodes
    if(input$method == "point" & input$sample == "opcode") {
      
      metadata_format <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(metadata_score %in% "failed")
      
    }
    
    # If point method and samples are periods
    if(input$method == "point" & input$sample == "period") {
      
      metadata_format <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::group_by(campaignid, metadata_score) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(metadata_score %in% "failed")
      
    }
    
    # If transect method and sample = "opcode" + "period"
    if(input$method == "transect" & input$sample.t == "opcodeperiod") {
      
      metadata_format <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::filter(metadata_score %in% "failed")
      
    }
    
    # If transect method and sample = "period"
    if(input$method == "transect" & input$sample.t == "period") {
      
      metadata_format <- metadata.regions() %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
        dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
        dplyr::mutate(metadata_score = case_when(
          (
            is.na(longitude_dd) | 
              longitude_dd > 180 |
              longitude_dd < -180 |
              latitude_dd > 90 |
              latitude_dd < -90 |
              is.na(latitude_dd) | 
              is.na(date_time) | 
              is.na(depth_m) | 
              !status %in% c("Fished", "No-take", "I", "II", "III", "IV", "V", "VI") | 
              !successful_count %in% c("Yes", "No") | 
              !successful_length %in% c("Yes", "No") | 
              successful_count %in% c("Yes") & is.na(observer_count) |
              successful_length %in% c("Yes") & is.na(observer_length) |
              is.na(sample)) ~ "failed",
          .default = "passed")) %>%
        dplyr::filter(metadata_score %in% "failed")
      
    }
    
    #message("view metadata score")
    
    score <- round(((nrow(metadata.regions()) - nrow(metadata_format)) / nrow(metadata.regions())) * 100, 2) #%>% dplyr::glimpse()
    
  })
  
  ## ► 2. Metadata matches count ----
  # % of samples in the count data that have a match in the metadata
  
  #### ~ table ----
  metadata_matches_count_table <- reactive({
    
    if(input$upload %in% "EM"){
      
      samples <- points() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
      
    } else {
      
      samples <- count_data() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
    }
    
    unique_count_samples <- samples %>%
      dplyr::distinct(campaignid, sample)
    
    total_count_samples <- unique_count_samples %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(number_of_samples = n()) %>%
      dplyr::ungroup()
    
    # message("view: metadata_matches_count")
    
    metadata_matches_count <- anti_join(unique_count_samples, metadata()) %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(number_of_samples_without_match = n()) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(total_count_samples) %>%
      tidyr::replace_na(list(number_of_samples_without_match = 0)) %>%
      dplyr::mutate(metadata_matches_count = ((number_of_samples-number_of_samples_without_match)/number_of_samples)*100) %>%
      dplyr::select(campaignid, metadata_matches_count) #%>% dplyr::glimpse()
    
    return(metadata_matches_count)
    
  })
  
  #### ~ score ----
  metadata_matches_count_score <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$upload %in% "EM"){
      
      dat <- points()
      
      samples <- points() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
      
    } else {
      
      dat <- count_data()
      
      samples <- count_data() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
    }
    
    rows.missing.metadata <- anti_join(samples, metadata.samples) #%>%
      #dplyr::select(-sample)
    
    score <- round(((nrow(dat) - nrow(rows.missing.metadata))/nrow(dat))*100, 2)
    
  })
  
  ## ► 3. Metadata matches length ----
  # % of samples in the length data that have a match in the metadata
  
  #### ~ table ----
  metadata_matches_length_table <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        samples <- bind_rows(length(), threedpoints()) %>%
          dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")))
        
      } else {
        samples <- gen.length()
      }
      
      unique_length_samples <- samples %>%
        distinct(campaignid, sample)
      
      total_length_samples <- unique_length_samples %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_samples = n())
      
      metadata_matches_length <- anti_join(unique_length_samples, metadata.samples) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_samples_without_match = n()) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(total_length_samples) %>%
        tidyr::replace_na(list(number_of_samples_without_match = 0)) %>%
        dplyr::mutate(metadata_matches_length = ((number_of_samples-number_of_samples_without_match)/number_of_samples)*100) %>%
        dplyr::select(campaignid, metadata_matches_length)
    } else {
      
      metadata_matches_length <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(metadata_matches_length = NA)
      
    }
    
    metadata_matches_length
    
  })
  
  #### ~ score ----
  metadata_matches_length_score <- reactive({
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      distinct() %>%
      mutate(sample = as.factor(sample))
    
    if(input$length %in% "Yes"){
      
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
      
    } else {
      score <- NA
    }
    
  })
  
  ## ► 4. Count matches schema ----
  # % of species that match the schema
  
  #### ~ table ----
  count_matches_schema_table <- reactive({
    
    if(input$upload %in% "EM"){
      unique_species <- points() %>%
        dplyr::distinct(campaignid, family, genus, species)
      
    } else {
      unique_species <- count_data() %>%
        dplyr::distinct(campaignid, family, genus, species)
    }
    
    total_species <- unique_species %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(number_of_species = n())
    
    count_matches_schema <- anti_join(unique_species, life.history()) %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(number_of_species_dont_match = n()) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(total_species) %>%
      tidyr::replace_na(list(number_of_species_dont_match = 0)) %>%
      dplyr::mutate(count_matches_schema = ((number_of_species-number_of_species_dont_match)/number_of_species)*100) %>%
      dplyr::select(campaignid, count_matches_schema)
    
  })
  
  #### ~ score ----
  count_matches_schema_score <- reactive({
    
    if(input$upload %in% "EM"){
      species <- points() %>%
        dplyr::distinct(family, genus, species)
      
    } else {
      species <- count_data() %>%
        dplyr::distinct(family, genus, species)
    }
    
    species_list <- life.history() %>%
      dplyr::distinct(family, genus, species)
    
    species_missing <- anti_join(species, species_list)
    
    score <- ((nrow(species) - nrow(species_missing))/nrow(species))*100
    
    # pretty_score <- prettyNum(score, digits = 4)
    
    
  })
  
  ## ► 5. Count vs length ----
  # "% of MaxNs that are equal to the number of length measurements + number of 3D point measurements"
  
  #### ~ table ----
  count_vs_length_table <- reactive({
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_of_counts <- count %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_number_of_rows = n())
      
      campaigns_with_lengths <- metadata.regions() %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
      
      count_vs_length <- length %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(stereo_maxn = sum(number)) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(count) %>%
        dplyr::full_join(metadata.regions()) %>%
        dplyr::filter(successful_length %in% "Yes") %>% # only use samples that had lengths
        dplyr::filter(!count %in% stereo_maxn) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_errors = n()) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(number_of_counts) %>%
        tidyr::replace_na(list(number_of_errors = 0)) %>%
        dplyr::mutate(count_vs_length = ((total_number_of_rows-number_of_errors)/total_number_of_rows)*100) %>%
        dplyr::left_join(campaigns_with_lengths) %>%
        dplyr::mutate(count_vs_length = case_when(
          length_not_uploaded %in% TRUE ~ NA,
          .default = count_vs_length
        )) %>%
        dplyr::select(campaignid, count_vs_length)
      
    } else {
      
      count_vs_length <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(count_vs_length = NA)
      
    }
    
    count_vs_length
    
  })
  
  
  #### ~ score ----
  count_vs_length_score <- reactive({
    
    length.vs.maxn <- length.vs.maxn() %>%
      dplyr::filter(!(maxn %in% 0 & length_maxn %in% 0))%>%
      dplyr::mutate(difference = abs(difference)) %>%
      dplyr::mutate(error = if_else(difference > 0, 1, 0)) #%>% glimpse()
    
    total.rows <- nrow(length.vs.maxn) #%>% glimpse()
    total.errors <- sum(length.vs.maxn$error) #%>% glimpse()
    
    score <- round(((total.rows - total.errors)/ total.rows) * 100, 2)
    
  })
  
  ## ► 6. Count less length ----
  # % of lengths more than expected from MaxN (no 3D points)
  
  #### ~ table ----
  count_less_length_table <- reactive({
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      campaigns_with_lengths <- metadata.regions() %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
      
      number_in_counts <- count %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_count = sum(count))
      
      count_less_length <- length %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length > count) %>%
        dplyr::mutate(difference = number_length - count) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(extra_lengths = sum(difference)) %>%
        full_join(number_in_counts) %>%
        tidyr::replace_na(list(extra_lengths = 0)) %>%
        dplyr::mutate(count_less_length = extra_lengths/total_count*100) %>%
        dplyr::left_join(campaigns_with_lengths) %>%
        dplyr::mutate(count_less_length = case_when(
          length_not_uploaded %in% TRUE ~ NA,
          .default = count_less_length
        )) %>%
        dplyr::select(campaignid, count_less_length) %>%
        dplyr::mutate(count_less_length = 100 - count_less_length)
      
      
    } else {
      
      count_less_length <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(count_less_length = NA)
      
    }
    
    count_less_length
    
  })
  
  #### ~ score ----
  count_less_length_score <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_in_counts <- count %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_count = sum(count))
      
      count_less_length <- length %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length > count) %>%
        dplyr::mutate(difference = number_length - count)
      
      total_count <- sum(count$count) #%>% glimpse()
      missing_count <- sum(count_less_length$difference) #%>% glimpse()
      
      score <- round((missing_count/total_count) * 100, 2)
      
    } else {
      
      score <- NA
      
    }
    
    score
    
  })
  
  ## ► 7. Count greater length ----
  # % of lengths more than expected from MaxN (no 3D points)
  
  #### ~ table ----
  count_greater_length_table <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_in_counts <- count %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_count = sum(count))
      
      count_greater_length <- length %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length < count) %>%
        dplyr::mutate(difference = count - number_length) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(extra_count = sum(difference)) %>%
        full_join(number_in_counts) %>%
        tidyr::replace_na(list(extra_count = 0)) %>%
        dplyr::mutate(count_greater_length = extra_count/total_count*100) %>%
        dplyr::left_join(campaigns_with_lengths) %>%
        dplyr::mutate(count_greater_length = case_when(
          length_not_uploaded %in% TRUE ~ NA,
          .default = count_greater_length
        )) %>%
        dplyr::select(campaignid, count_greater_length) %>%
        dplyr::mutate(count_greater_length = 100 - count_greater_length)
      
    } else {
      
      count_greater_length <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(count_greater_length = NA)
      
    }
    
  })
  
  #### ~ score ----
  count_greater_length_score <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      count_greater_length <- length %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length < count) %>%
        dplyr::mutate(difference = count - number_length)
      
      total_count <- sum(count$count) #%>% glimpse()
      extra_count <- sum(count_greater_length$difference) #%>% glimpse()
      
      score <- round((extra_count/ total_count) * 100, 2) 
    } else {
      score <- NA
    }
    
    score
    
  })
  
  large_bodied_carnivores <- reactive({
    
    large_bodied_carnivores <- life.history() %>%
      dplyr::filter(fb_trophic_level > 2.8) %>%
      dplyr::filter(length_max_cm > 40) %>%
      dplyr::filter(class %in% "Actinopterygii") %>%
      dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes", 
                                  "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes", 
                                  "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
      dplyr::filter(!is.na(fb_length_at_maturity_cm)) %>%
      dplyr::select(family, genus, species)
    #dplyr::select(-caab)
    
  })
  
  ## ► 8. Count less length for larger bodied carnivores - score ----
  # % of lengths more than expected from MaxN (no 3D points)
  
  #### ~ table ----
  count_less_length_lbc_table <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_in_counts <- count %>%
        semi_join(large_bodied_carnivores()) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_count = sum(count))
      
      count_less_length_lbc <- length %>%
        dplyr::semi_join(large_bodied_carnivores()) %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length > count) %>%
        dplyr::mutate(difference = number_length - count) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(extra_lengths = sum(difference)) %>%
        full_join(number_in_counts) %>%
        tidyr::replace_na(list(extra_lengths = 0)) %>%
        dplyr::mutate(count_less_length_lbc = extra_lengths/total_count*100) %>%
        dplyr::left_join(campaigns_with_lengths) %>%
        dplyr::mutate(count_less_length_lbc = case_when(
          length_not_uploaded %in% TRUE ~ NA,
          .default = count_less_length_lbc
        )) %>%
        dplyr::select(campaignid, count_less_length_lbc) %>%
        dplyr::mutate(count_less_length_lbc = 100 - count_less_length_lbc)
    } else {
      
      count_less_length_lbc <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(count_less_length_lbc = NA)
      
    }
    
    count_less_length_lbc
    
  })
  
  ### ~ score ----
  count_less_length_lbc_score <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_in_counts <- count %>%
        semi_join(large_bodied_carnivores())
      
      count_less_length_lbc <- length %>%
        dplyr::semi_join(large_bodied_carnivores()) %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length > count) %>%
        dplyr::mutate(difference = number_length - count)
      
      total_count <- sum(number_in_counts$count) #%>% glimpse()
      extra_lengths <- sum(count_less_length_lbc$difference) #%>% glimpse()
      
      score <- round((extra_lengths/ total_count) * 100, 2) 
      
    } else {
      
      score <- NA
      
    }
    
    score
    
  })
  
  ## ► 9. Count greater length larger bodied carnivores - score ----
  # % of lengths more than expected from MaxN (no 3D points)
  
  ### ~ table ----
  count_greater_length_lbc_table <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        # message("view EM data")
        
        length <- length3dpoints.clean() #%>% dplyr::glimpse()
        
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn) #%>% dplyr::glimpse()
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      # message("view counts")
      
      number_in_counts <- count %>%
        dplyr::semi_join(large_bodied_carnivores()) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(total_count = sum(count)) %>%
        dplyr::ungroup() #%>% dplyr::glimpse()
      
      # message("view greater lbc")
      
      count_greater_length_lbc <- length %>%
        dplyr::semi_join(large_bodied_carnivores()) %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length < count) %>%
        dplyr::mutate(difference = count - number_length) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(extra_count = sum(difference)) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(number_in_counts) %>%
        tidyr::replace_na(list(extra_count = 0)) %>%
        dplyr::mutate(count_greater_length_lbc = extra_count/total_count*100) %>%
        dplyr::left_join(campaigns_with_lengths) %>%
        dplyr::mutate(count_greater_length_lbc = case_when(
          length_not_uploaded %in% TRUE ~ NA,
          .default = count_greater_length_lbc
        )) %>%
        dplyr::select(campaignid, count_greater_length_lbc)  %>%
        dplyr::mutate(count_greater_length_lbc = 100 - count_greater_length_lbc)
      
    } else {
      
      count_greater_length_lbc <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(count_greater_length_lbc = NA)
      
    }
    
  })
  
  ### ~ score ----
  count_greater_length_lbc_score <- reactive({
    
    campaigns_with_lengths <- metadata.regions() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        count <- maxn.clean() %>%
          dplyr::rename(count = maxn)
        
      } else {
        
        length <- gen.length.clean()
        count <- count.clean()
        
      }
      
      number_in_counts <- count %>%
        dplyr::semi_join(large_bodied_carnivores())
      
      count_greater_length_lbc <- length %>%
        dplyr::semi_join(large_bodied_carnivores()) %>%
        dplyr::filter(!is.na(length_mm)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>%
        dplyr::summarise(number_length = sum(number)) %>%
        ungroup() %>%
        dplyr::left_join(count) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::filter(successful_count %in% "Yes") %>%
        dplyr::filter(successful_length %in% "Yes") %>%
        dplyr::filter(number_length < count) %>%
        dplyr::mutate(difference = count - number_length)
      
      total_count <- sum(number_in_counts$count) #%>% dplyr::glimpse()
      extra_count <- sum(count_greater_length_lbc$difference) #%>% dplyr::glimpse()
      
      score <- round((extra_count/ total_count) * 100, 2) 
      
    } else {
      
      score <- NA
    }
    
  })
  
  ## ► 10. Lengths that are greater than their max size - score ----
  # TODO need to create a "synthesis score" and a per campaign table!
  # % of lengths that are greater than the maximum size limit
  
  ### ~ table ----
  length_greater_maximum_size_table <- reactive({
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        # message("Em")
        
        length <- length3dpoints.clean() #%>% dplyr::glimpse()
        
      } else {
        
        length <- gen.length.clean()
        
      }
      
      total_measurements <- length %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_lengths = sum(number)) #%>% dplyr::glimpse()
      
      length_greater_maximum_size <- length.wrong() %>%
        dplyr::filter(reason %in% c("too big")) %>%
        dplyr::filter(length_max_mm < length_mm) %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_lengths_over = sum(number)) %>%
        dplyr::full_join(total_measurements) %>%
        tidyr::replace_na(list(number_of_lengths_over = 0)) %>%
        dplyr::mutate(length_greater_maximum_size = (number_of_lengths_over/number_of_lengths)*100) %>%
        dplyr::select(campaignid, length_greater_maximum_size) %>%
        dplyr::mutate(length_greater_maximum_size = 100 - length_greater_maximum_size)
      
    } else {
      
      length_greater_maximum_size <- metadata() %>%
        dplyr::distinct(campaignid) %>%
        dplyr::mutate(length_greater_maximum_size = NA)
      
    }
    
    length_greater_maximum_size
    
    
  })
  
  ### ~ score ----
  length_greater_maximum_size_score <- reactive({
    
    if(input$length %in% "Yes"){
      
      if(input$upload %in% "EM"){
        
        length <- length3dpoints.clean()
        
      } else {
        
        length <- gen.length.clean()
        
      }
      
      total_measurements <- length %>%
        dplyr::group_by(campaignid) %>%
        dplyr::summarise(number_of_lengths = sum(number))
      
      length_greater_maximum_size <- length.wrong() %>%
        dplyr::filter(reason %in% c("too big")) %>%
        dplyr::filter(length_max_mm < length_mm) 
      
      over_size <- sum(length_greater_maximum_size$number) #%>% dplyr::glimpse()
      
      score <- round((over_size/ total_measurements$number_of_lengths) * 100, 2)
      
    } else {
      
      score <- NA
      
    }
    
  })
  
  ## ► Combine scores togehter  ----
  checkem_scores_table <- reactive({
    
    message("view final scores table")
    
    checkem_scores <- metadata_format_table() %>%
      dplyr::left_join(metadata_matches_count_table()) %>%
      dplyr::left_join(metadata_matches_length_table()) %>%
      dplyr::left_join(count_matches_schema_table()) %>%
      dplyr::left_join(count_vs_length_table()) %>%
      dplyr::left_join(count_less_length_table()) %>%
      dplyr::left_join(count_greater_length_table()) %>%
      dplyr::left_join(count_less_length_lbc_table()) %>%
      dplyr::left_join(count_greater_length_lbc_table()) %>%
      dplyr::left_join(length_greater_maximum_size_table()) %>%
      dplyr::mutate(metadata_format = round(metadata_format, 2),
                    metadata_matches_count = round(metadata_matches_count, 2),
                    metadata_matches_length = round(metadata_matches_length, 2),
                    count_matches_schema = round(count_matches_schema, 2),
                    count_vs_length = round(count_vs_length, 2),
                    count_less_length = round(count_less_length, 2),
                    count_greater_length = round(count_greater_length, 2),
                    count_less_length_lbc = round(count_less_length_lbc, 2),
                    count_greater_length_lbc = round(count_greater_length_lbc, 2),
                    length_greater_maximum_size = round(length_greater_maximum_size, 2)
      ) #%>% dplyr::glimpse()
    
  })
  
  ## ► View scores in dashboard ----
  output$scores.table <- renderDataTable(checkem_scores_table(), 
                                         rownames = FALSE, 
                                         options = list(paging = FALSE, 
                                                        searching = TRUE,
                                                        scrollX = TRUE,
                                                        initComplete = JS(# JS to modify the table headers
                                                          "function(settings, json) {",
                                                          "$('#table thead tr th:gt(0)').css({'writing-mode':'vertical-rl','text-align':'right'})",
                                                          "}")))
  
  # output$scores.table <- renderDataTable({
  #   checkem_scores_table()
  # })
  
  ## ► Create plot  ----
  output$score.plot <- renderPlot({
    
    cols <- c()
    
    ### 1. metadata_format ----
    # All plots use the same metadata score
    metadata_format_score <- round(metadata_format_score(), 2)
    cols["Metadata"] <- "#99D199"
    
    ### 4. count_matches_schema ----
    # All plots use the same count matches schema score
    count_matches_schema_score <- round(count_matches_schema_score(), 2)
    cols["Count schema"] <- "#ffe69c"
    
    ### 5. count_vs_length ----
    # Only campaigns that have length, otherwise = NA (display as 100% but grey)
    if(input$length %in% "Yes"){
      
      count_vs_length_score <- round(count_vs_length_score(), 2)
      cols["Count v Length"] <- "#F8BEB3"
      
      count_vs_length_goal <- "#EE5C42"
      count_vs_length_fill <- "#F8BEB3"
      
    } else {
      
      count_vs_length_score <- 100
      cols["Count v Length"] <- "darkgrey"
      count_vs_length_goal <- "darkgrey"
      count_vs_length_fill <- "darkgrey"
      
    }
    
    ### 8. count_less_length_large_bodied_carnivores ----
    # Only campaigns that have length, otherwise = NA (display as 100% but grey)
    if(input$length %in% "Yes"){
      
      count_less_length_lbc_score <- 100 - round(count_less_length_lbc_score(), 2)
      cols["Count < Length*"] <- "#99C3D1"
      count_less_length_lbc_goal <- "#00688B"
      count_less_length_lbc_fill <- "#99C3D1"
      
    } else {
      
      count_less_length_lbc_score <- 100
      cols["Count < Length*"] <- "darkgrey"
      count_less_length_lbc_goal <- "darkgrey"
      count_less_length_lbc_fill <- "darkgrey"
      
    }
    
    ### 9. count_greater_length_large_bodied_carnivores ----
    # Only campaigns that have length, otherwise = NA (display as 100% but grey)
    if(input$length %in% "Yes"){
      
      count_greater_length_lbc_score <- 100 - round(count_greater_length_lbc_score(), 2)
      cols["Count > Length*"] <- "#d6b3f8"
      count_greater_length_lbc_goal <- "#b778f5"
      count_greater_length_lbc_fill <- "#d6b3f8"
      
    } else {
      
      count_greater_length_lbc_score <- 100
      cols["Count > Length*"] <- "darkgrey"
      count_greater_length_lbc_goal <- "darkgrey"
      count_greater_length_lbc_fill <- "darkgrey"
      
    }
    
    ### 10. length_greater_maximum_size ----
    # Only campaigns that have length, otherwise = NA (display as 100% but grey)
    if(input$length %in% "Yes"){
      
      length_greater_maximum_size_score <- 100 - round(length_greater_maximum_size_score(), 2)
      cols["Length > Max Size"] <- "#bad0e8"
      length_greater_maximum_size_goal <- "#92bcea"
      length_greater_maximum_size_fill <- "#bad0e8"
      
    } else {
      
      length_greater_maximum_size_score <- 100
      cols["Length > Max Size"] <- "darkgrey"
      length_greater_maximum_size_goal <- "darkgrey"
      length_greater_maximum_size_fill <- "darkgrey"
      
    }
    
    
    dat <- data.frame(score = c("Metadata", 
                                "Count schema", 
                                
                                "Count v Length", 
                                "Count < Length*", 
                                "Count > Length*", 
                                
                                "Length > Max Size"),
                      value = c(metadata_format_score, 
                                count_matches_schema_score, 
                                
                                count_vs_length_score, 
                                count_less_length_lbc_score, 
                                count_greater_length_lbc_score, 
                                length_greater_maximum_size_score))
    
    dat <- dat %>%
      dplyr::mutate(label_position = value *0.85)
    # dplyr::mutate(label_position = if_else(value < 30, 20, value - 15)) #%>%
    # #tidyr::replace_na(list(value = "Length missing"))
    
    dat$score <- fct_relevel(dat$score, "Count schema", 
                             "Count v Length", 
                             "Count < Length*", 
                             "Count > Length*", 
                             "Length > Max Size",  
                             "Metadata")
    
    if(input$length %in% "Yes"){
      
      dat_filtered <- dat
      
    } else {
      
      dat_filtered <- dat %>%
        dplyr::filter(score %in% c("Metadata", "Count schema"))
      
    }
    
    
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
      
      geom_rect(xmin = 5.5, xmax = 6.5,
                ymin = 0,
                ymax = metadata_format_score, 
                fill = "#99D199", color = "white", size = 2) +
      
      draw_line(x = c(5.51, 6.49), y = c(100, 100), color = "#008B00", size = 2) +
      
      geom_rect(xmin = 0.5, xmax = 1.5,
                ymin = 0,
                ymax = count_matches_schema_score, 
                fill = "#ffe69c", color = "white", size = 2) +
      
      draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
      
      geom_rect(xmin = 1.5, xmax = 2.5,
                ymin = 0,
                ymax = count_vs_length_score, 
                fill = count_vs_length_fill, color = "white", size = 2) +
      
      draw_line(x = c(1.51, 2.49), y = c(100, 100), color = count_vs_length_goal, size = 2) +
      
      geom_rect(xmin = 2.5, xmax = 3.5,
                ymin = 0,
                ymax = count_less_length_lbc_score, 
                fill = count_less_length_lbc_fill, color = "white", size = 2) +
      
      draw_line(x = c(2.51, 3.49), y = c(100, 100), color = count_less_length_lbc_goal, size = 2) +
      
      geom_rect(xmin = 3.5, xmax = 4.5,
                ymin = 0,
                ymax = count_greater_length_lbc_score, 
                fill = count_greater_length_lbc_fill, color = "white", size = 2) +
      
      draw_line(x = c(3.51, 4.49), y = c(100, 100), color = count_greater_length_lbc_goal, size = 2) +
      
      geom_rect(xmin = 4.5, xmax = 5.5,
                ymin = 0,
                ymax = length_greater_maximum_size_score, fill = length_greater_maximum_size_fill, color = "white", size = 2) +
      
      draw_line(x = c(4.51, 5.49), y = c(100, 100), color = length_greater_maximum_size_goal, size = 2) +
      
      
      geom_rect(xmin = 5.5, xmax = 6.5,
                ymin = 110,
                ymax = 150, fill = "#008B00", color = "white", size = 2) +
      geom_rect(xmin = 0.5, xmax = 1.5,
                ymin = 110,
                ymax = 150, fill = "#FFD966", color = "white", size = 2) +
      geom_rect(xmin = 1.5, xmax = 2.5,
                ymin = 110,
                ymax = 150, fill = count_vs_length_goal, color = "white", size = 2) +
      geom_rect(xmin = 2.5, xmax = 3.5,
                ymin = 110,
                ymax = 150, fill = count_less_length_lbc_goal, color = "white", size = 2) +
      geom_rect(xmin = 3.5, xmax = 4.5,
                ymin = 110,
                ymax = 150, fill = count_greater_length_lbc_goal, color = "white", size = 2) +
      geom_rect(xmin = 4.5, xmax = 5.5,
                ymin = 110,
                ymax = 150, fill = length_greater_maximum_size_goal, color = "white", size = 2) +
      
      geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 5.5, fontface = "bold") + 
      
      geom_textpath(dat = dat_filtered, aes(y = label_position, 
                                            label = paste0(value, "%")), 
                    colour = "grey100", size = 5, fontface = "bold") +
      coord_polar() #start = 1.05
    
    # cols <- c("Metadata" = "#99D199", 
    #           "Length < Count" = "#99C3D1", 
    #           "Length > Count" = "#d6b3f8",
    #           "Count v Length" = "#F8BEB3",
    #           "Count" = "#ffe69c",
    #           "Length" = "#bad0e8")
    
    
    
    # TODO only include this if it is not a transect campaign
    # metadata_matches_count_score <- round(metadata_matches_count_score(), 2)
    
    # if(input$upload %in% "EM"){
    #   
    #   if(input$length %in% "Yes"){
    #     
    #     length.vs.maxn <- length.vs.maxn() %>%
    #       dplyr::filter(!(maxn %in% 0 & length_maxn %in% 0))%>%
    #       dplyr::mutate(difference = abs(difference)) %>%
    #       dplyr::mutate(error = if_else(difference > 0, 1, 0)) #%>% glimpse()
    # 
    #     total.rows <- nrow(length.vs.maxn) #%>% glimpse()
    #     total.errors <- sum(length.vs.maxn$error) #%>% glimpse()
    # 
    #     length.maxn.score <- round(((total.rows - total.errors)/ total.rows) * 100, 2)
    #     
    #     metadata_matches_length_score <- round(metadata_matches_length_score(), 2)
    #     
    #     length.v.3d <- length.v.3d()
    #     
    #     total.lengths <- sum(length.v.3d$number_of_length_measurements)
    #     total <- sum(length.v.3d$total_measurements)
    #     
    #     length.percent.score <- round((total.lengths/total) * 100, 2)
    #     
    #     print("new length missing maxn error score")
    #     
    #     length.missing.or.greater.maxn <- length.missing.or.greater.maxn()
    #     
    #     additional.maxns <- sum(length.missing.or.greater.maxn$maxn_greater_length)
    #     additional.lengths <- sum(length.missing.or.greater.maxn$maxn_smaller_length)
    #     total.maxns <- sum(length.missing.or.greater.maxn$maxn)
    #     
    #     # If there are heaps of additional lenths then replace with 100 (very bad score)
    #     if(additional.lengths > 100){
    #       additional.lengths <- 100
    #     }
    #     
    #     additional.maxns.score <- round(100 - (additional.maxns/total.maxns) * 100, 2)
    #     additional.lengths.score <- round(100 - (additional.lengths/total.maxns) * 100, 2)
    #     
    #     #print("view additional maxn score")
    #     #glimpse(additional.maxns.score)
    # 
    # dat <- data.frame(score = c("Metadata", "Length > Count", "Length < Count", "Count v Length", "Count", "Length"),
    #                   value = c(metadata_format_score, additional.lengths.score, additional.maxns.score, length.maxn.score, metadata_matches_count_score, metadata_matches_length_score))
    # 
    # dat$score <- fct_relevel(dat$score, "Count", "Count v Length", "Length > Count", "Length < Count", "Length",  "Metadata")
    # 
    # cols <- c("Metadata" = "#99D199", 
    #           "Length < Count" = "#99C3D1", 
    #           "Length > Count" = "#d6b3f8",
    #           "Count v Length" = "#F8BEB3",
    #           "Count" = "#ffe69c",
    #           "Length" = "#bad0e8")
    #     
    # p <- ggplot(dat, aes(score, value, fill = score)) +
    #   scale_fill_manual(values = cols) +
    #   xlab("") + ylab("") +
    #   theme_classic() +
    #   theme(legend.position = "none", 
    #         axis.line = element_blank(), 
    #         axis.text.y = element_blank() ,
    #         axis.text.x = element_blank() ,
    #         axis.ticks = element_blank()) +
    #   
    #   scale_y_continuous(limits = c(-50, 150)) + 
    #   
    #   geom_rect(xmin = 5.5, xmax = 6.5,
    #             ymin = 0,
    #             ymax = metadata_format_score, fill = "#99D199", color = "white", size = 2) +
    #   
    #   draw_line(x = c(5.51, 6.49), y = c(100, 100), color = "#008B00", size = 2) +
    #   
    #   geom_rect(xmin = 0.5, xmax = 1.5,
    #             ymin = 0,
    #             ymax = metadata_matches_count_score, fill = "#ffe69c", color = "white", size = 2) +
    #   
    #   draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
    #   
    #   geom_rect(xmin = 1.5, xmax = 2.5,
    #             ymin = 0,
    #             ymax = length.maxn.score, fill = "#F8BEB3", color = "white", size = 2) +
    #   
    #   draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#EE5C42", size = 2) +
    #   
    #   geom_rect(xmin = 2.5, xmax = 3.5,
    #             ymin = 0,
    #             ymax = additional.lengths.score, fill = "#99C3D1", color = "white", size = 2) +
    #   
    #   draw_line(x = c(2.51, 3.49), y = c(100, 100), color = "#00688B", size = 2) +
    #   
    #   geom_rect(xmin = 3.5, xmax = 4.5,
    #             ymin = 0,
    #             ymax = additional.maxns.score, fill = "#d6b3f8", color = "white", size = 2) +
    #   
    #   draw_line(x = c(3.51, 4.49), y = c(70, 70), color = "#b778f5", size = 2) +
    #   
    #   geom_rect(xmin = 4.5, xmax = 5.5,
    #             ymin = 0,
    #             ymax = metadata_matches_length_score, fill = "#bad0e8", color = "white", size = 2) +
    #   
    #   draw_line(x = c(4.51, 5.49), y = c(100, 100), color = "#92bcea", size = 2) +
    #   
    #   
    #   geom_rect(xmin = 5.5, xmax = 6.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#008B00", color = "white", size = 2) +
    #   geom_rect(xmin = 0.5, xmax = 1.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#FFD966", color = "white", size = 2) +
    #   geom_rect(xmin = 1.5, xmax = 2.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#EE5C42", color = "white", size = 2) +
    #   geom_rect(xmin = 2.5, xmax = 3.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#00688B", color = "white", size = 2) +
    #   geom_rect(xmin = 3.5, xmax = 4.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#b778f5", color = "white", size = 2) +
    #   geom_rect(xmin = 4.5, xmax = 5.5,
    #             ymin = 110,
    #             ymax = 150, fill = "#92bcea", color = "white", size = 2) +
    #   
    #   geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 5.5, fontface = "bold") + 
    # 
    #   geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
    #   coord_polar() #start = 1.05
    # 
    #   } else {
    #     
    #     dat <- data.frame(score = c("Metadata", "Count"),
    #                       value = c(metadata_format_score, metadata_matches_count_score))
    #     
    #     dat$score <- fct_relevel(dat$score, "Count", "Metadata")
    #     
    #     cols <- c("Metadata" = "#99D199", 
    #               "Count" = "#ffe69c")
    #     
    #     p <- ggplot(dat, aes(score, value, fill = score)) +
    #       scale_fill_manual(values = cols) +
    #       xlab("") + ylab("") +
    #       theme_classic() +
    #       theme(legend.position = "none", 
    #             axis.line = element_blank(), 
    #             axis.text.y = element_blank() ,
    #             axis.text.x = element_blank() ,
    #             axis.ticks = element_blank()) +
    #       
    #       scale_y_continuous(limits = c(-50, 150)) + 
    #       
    #       geom_rect(xmin = 1.5, xmax = 2.5,
    #                 ymin = 0,
    #                 ymax = metadata_format_score, fill = "#99D199", color = "white", size = 2) +
    #       
    #       draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#008B00", size = 2) +
    #       
    #       geom_rect(xmin = 0.5, xmax = 1.5,
    #                 ymin = 0,
    #                 ymax = metadata_matches_count_score, fill = "#ffe69c", color = "white", size = 2) +
    #       
    #       draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
    #     
    #       geom_rect(xmin = 1.5, xmax = 2.5,
    #                 ymin = 110,
    #                 ymax = 150, fill = "#008B00", color = "white", size = 2) +
    #       geom_rect(xmin = 0.5, xmax = 1.5,
    #                 ymin = 110,
    #                 ymax = 150, fill = "#FFD966", color = "white", size = 2) +
    #       
    #       geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 6.5, fontface = "bold") + 
    #       
    #       geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
    #       coord_polar() #start = 1.05
    #     
    #   }
    # 
    # } else {
    #   
    #   if(input$length %in% "Yes"){
    #     
    #     length.vs.maxn <- length.vs.maxn() %>%
    #       dplyr::filter(!(maxn %in% 0 & length_maxn %in% 0))%>%
    #       dplyr::mutate(difference = abs(difference)) %>%
    #       dplyr::mutate(error = if_else(difference > 0, 1, 0)) #%>% glimpse()
    #     
    #     total.rows <- nrow(length.vs.maxn) #%>% glimpse()
    #     total.errors <- sum(length.vs.maxn$error) #%>% glimpse()
    #     
    #     length.maxn.score <- round(((total.rows - total.errors)/ total.rows) * 100, 2)
    #     
    #     metadata_matches_length_score <- round(metadata_matches_length_score(), 2)
    #     
    #     length.v.3d <- length.v.3d()
    #     
    #     total.lengths <- sum(length.v.3d$number_of_length_measurements)
    #     total <- sum(length.v.3d$total_measurements)
    #     
    #     length.percent.score <- round((total.lengths/total) * 100, 2)
    #   
    #   dat <- data.frame(score = c("Metadata", "Count v Length", "Count", "Length"),
    #                     value = c(metadata_format_score, length.maxn.score, metadata_matches_count_score, metadata_matches_length_score))
    #   
    #   dat$score <- fct_relevel(dat$score, "Count", "Count v Length", "Length",  "Metadata")
    #   
    #   cols <- c("Metadata" = "#99D199", 
    #             # "% Length" = "#99C3D1", 
    #             "Count v Length" = "#F8BEB3",
    #             "Count" = "#ffe69c",
    #             "Length" = "#bad0e8")
    #   
    #   p <- ggplot(dat, aes(score, value, fill = score)) +
    #     scale_fill_manual(values = cols) +
    #     xlab("") + ylab("") +
    #     theme_classic() +
    #     theme(legend.position = "none", 
    #           axis.line = element_blank(), 
    #           axis.text.y = element_blank() ,
    #           axis.text.x = element_blank() ,
    #           axis.ticks = element_blank()) +
    #     
    #     scale_y_continuous(limits = c(-50, 150)) + 
    #     
    #     geom_rect(xmin = 3.5, xmax = 4.5,
    #               ymin = 0,
    #               ymax = metadata_format_score, fill = "#99D199", color = "white", size = 2) +
    #     
    #     draw_line(x = c(3.51, 4.49), y = c(100, 100), color = "#008B00", size = 2) +
    #     
    #     geom_rect(xmin = 0.5, xmax = 1.5,
    #               ymin = 0,
    #               ymax = metadata_matches_count_score, fill = "#ffe69c", color = "white", size = 2) +
    #     
    #     draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
    #     
    #     geom_rect(xmin = 1.5, xmax = 2.5,
    #               ymin = 0,
    #               ymax = length.maxn.score, fill = "#F8BEB3", color = "white", size = 2) +
    #     
    #     draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#EE5C42", size = 2) +
    #     geom_rect(xmin = 2.5, xmax = 3.5,
    #               ymin = 0,
    #               ymax = metadata_matches_length_score, fill = "#bad0e8", color = "white", size = 2) +
    #     
    #     draw_line(x = c(2.51, 3.49), y = c(100, 100), color = "#92bcea", size = 2) +
    #     
    #     
    #     geom_rect(xmin = 3.5, xmax = 4.5,
    #               ymin = 110,
    #               ymax = 150, fill = "#008B00", color = "white", size = 2) +
    #     geom_rect(xmin = 0.5, xmax = 1.5,
    #               ymin = 110,
    #               ymax = 150, fill = "#FFD966", color = "white", size = 2) +
    #     geom_rect(xmin = 1.5, xmax = 2.5,
    #               ymin = 110,
    #               ymax = 150, fill = "#EE5C42", color = "white", size = 2) +
    #     geom_rect(xmin = 2.5, xmax = 3.5,
    #               ymin = 110,
    #               ymax = 150, fill = "#92bcea", color = "white", size = 2) +
    #     
    #     geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 6.5, fontface = "bold") + 
    #     
    #     geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
    #     coord_polar() #start = 1.05
    #   
    #   } else {
    #     
    #     # If they have no measured fish 
    #     dat <- data.frame(score = c("Metadata", "Count"),
    #                       value = c(metadata_format_score, metadata_matches_count_score))
    #     
    #     dat$score <- fct_relevel(dat$score, "Count", "Metadata")
    #     
    #     cols <- c("Metadata" = "#99D199", 
    #               "Count" = "#ffe69c")
    #     
    #     p <- ggplot(dat, aes(score, value, fill = score)) +
    #       scale_fill_manual(values = cols) +
    #       xlab("") + ylab("") +
    #       theme_classic() +
    #       theme(legend.position = "none", 
    #             axis.line = element_blank(), 
    #             axis.text.y = element_blank() ,
    #             axis.text.x = element_blank() ,
    #             axis.ticks = element_blank()) +
    #       
    #       scale_y_continuous(limits = c(-50, 150)) + 
    #       
    #       geom_rect(xmin = 1.5, xmax = 2.5,
    #                 ymin = 0,
    #                 ymax = metadata_format_score, fill = "#99D199", color = "white", size = 2) +
    #       
    #       draw_line(x = c(1.51, 2.49), y = c(100, 100), color = "#008B00", size = 2) +
    #       
    #       geom_rect(xmin = 0.5, xmax = 1.5,
    #                 ymin = 0,
    #                 ymax = metadata_matches_count_score, fill = "#ffe69c", color = "white", size = 2) +
    #       
    #       draw_line(x = c(0.51, 1.49), y = c(100, 100), color = "#FFD966", size = 2) +
    #       
    #       geom_rect(xmin = 3.5, xmax = 4.5,
    #                 ymin = 110,
    #                 ymax = 150, fill = "#008B00", color = "white", size = 2) +
    #       geom_rect(xmin = 0.5, xmax = 1.5,
    #                 ymin = 110,
    #                 ymax = 150, fill = "#FFD966", color = "white", size = 2) +
    #       
    #       geom_textpath(y = 128, aes(x = score, label = score), colour = "white", size = 6.5, fontface = "bold") + 
    #       
    #       geom_textpath(aes(y = value - 15, label = paste0(value, "%")), colour = "white", size = 5, fontface = "bold") +
    #       coord_polar() #start = 1.05
    #     
    #   }
    #   
    # }
    
    
    plot <- ggdraw()  + draw_plot(p) + draw_image("https://globalarchivemanual.github.io/images/earth-globe.png", scale = 0.15, hjust = -0.0165, vjust = -0.020) # -0.025
    
    plot + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
  })
  
  ## _______________________________________________________ ----
  ##                     CREATE REGIONS                     ----
  ## _______________________________________________________ ----
  marine.regions <- reactive({
    
    if(input$lifehistory %in% "aus"){
      marine.regions <- all_data$aus.regions
    } else if (input$lifehistory %in% "imcra"){
      marine.regions <- all_data$imcra.regions
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
        
        message(paste("reading file:", files$datapath[i]))
        
        message("preview metadata")
        
        if("campaignid" %in% colnames(tmp))
        {
          
          message("file has a column called campaignid, removing column")
          
          tmp <- tmp %>%
            dplyr::select(-c(campaignid))
        }
        
        # message("metadata with campaignid")
        
        print(files$name[i])
        
        tmp <- tmp %>%
          dplyr::mutate(campaignid = files$name[i]) #%>% dplyr::glimpse()
        
        metadata <- bind_rows(metadata, tmp)
        
        if("CampaignID" %in% colnames(metadata))
        {
          metadata <- metadata %>%
            dplyr::select(-c(CampaignID))
        }
        
        
        
      }
      
      # print("previewed semi tidied names")
      
      metadata <- metadata %>%
        clean_names() #%>% dplyr::glimpse()
      
      message("metadata campaigns")
      print(unique(metadata$campaignid))
      
      # Rename any old names
      lookup <- c(depth_m = "depth",
                  visibility_m = "visibility",
                  latitude_dd = "latitude",
                  longitude_dd = "longitude")
      
      metadata <- metadata %>%
        dplyr::rename(dplyr::any_of(lookup))
      
      # message("metadata campaigns")
      # print(unique(metadata$campaignid))
      
      cols.missing <- ""
      
      # Check if any names for the standard metadata are missing
      if(!"latitude_dd" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>latitude_dd</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"longitude_dd" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>longitude_dd</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"date_time" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>date_time</b> column is missing or spelt wrong (if date and time are present, CheckEM will create date_time)</li>", "<br>")
      }
      
      if(!"site" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>site</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"location" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>location</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"status" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>status</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"depth_m" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>depth_m</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"successful_count" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>successful_count</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"successful_length" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>successful_length</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"observer_count" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>observer_count</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!"observer_length" %in% colnames(metadata)){
        cols.missing <- paste0(cols.missing, "<li>The <b>observer_length</b> column is missing or spelt wrong</li>", "<br>")
      }
      
      if(!cols.missing %in% ""){
        shinyalert("Metadata columns", paste0(cols.missing, "<br> Please check the spelling before proceeding"), type = "warning", html = TRUE)
      }
      
      
      # message("uploaded metadata")
      
      metadata <- metadata %>%
        dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Metadata.csv" = "", "_metadata.csv" = ""))) %>%
        dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>%
        dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) #%>% dplyr::glimpse()
      
      # message("metadata names")
      # print(names(metadata))
      # 
      # message("metadata campaigns")
      # print(unique(metadata$campaignid))
      
      original_metadata_names <- c(names(metadata))
      
      if(!"sample" %in% original_metadata_names){
        
        # message("metadata does not have sample as a column")
        
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
      
      metadata <-  read.csv(checkem_example_path("2024-05_Exmouth-Gulf_stereo-BRUVs_Metadata.csv")) %>%
        clean_names() %>%
        # dplyr::rename(latitude_dd = latitude,
        #               longitude_dd = longitude,
        #               depth_m = depth) %>%
        dplyr::mutate(sample = opcode) %>%
        dplyr::mutate(campaignid = "2024-05_Exmouth-Gulf_stereo-BRUVs") %>%
        dplyr::select(campaignid, opcode, sample, latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, observer_count, observer_length) %>% 
        tidyr::replace_na(list(depth_m = 1, successful_length = "No")) %>%
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
    
    message("viewing final metadata")
    metadata <- metadata %>%
      tibble::add_column(!!!metadata.cols[!names(metadata.cols) %in% names(.)]) %>%
      # dplyr::filter(successful_count %in% c("Yes", "Y", "y", "yes")) %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::mutate(date_time = as.character(date_time)) %>% 
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, observer_count, observer_length, inclusion_probability, visibility_m, dplyr::any_of(c("successful_habitat_forward", "successful_habitat_backward")))  %>% dplyr::glimpse()
  })  
  
  ## ► Find nearest marine regions add commonwealth and state zoning ----
  metadata.regions <- reactive({
    metadata <- metadata() %>%
      dplyr::distinct()
    
    # message("viedplyr::w metadata_sf")
    metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326) #%>% dplyr::glimpse()
    
    regions <- marine.regions()
    regions <- st_as_sf(regions, crs = st_crs(4326))
    regions <- st_transform(regions, 4326) %>%
      dplyr::select(REGION)
    
    # message("view nearest marine regions")
    
    metadata.2 <- st_join(metadata_sf, regions, join = st_nearest_feature) %>%
      dplyr::rename(marine_region = REGION) %>%
      dplyr::mutate(sample = as.character(sample)) %>%
      bind_cols(st_coordinates(.)) %>%
      as.data.frame() %>%
      dplyr::select(-c(geometry)) %>%
      dplyr::rename(longitude_dd = X, latitude_dd = Y) #%>% dplyr::glimpse()
    
    
    # add in marine parks
    if(input$lifehistory %in% c("aus", "imcra")){
      
      # message("view metadata.marineparks for Australia")
      # message("two glimpses on one dataframe")
      
      metadata.marineparks <- metadata_sf %>%
        dplyr::select(-status) %>%
        st_join(all_data$marineparks, left = TRUE) %>%
        dplyr::rename(zone = ZONE_TYPE) %>%
        dplyr::group_by(geometry) %>%
        dplyr::summarise(zone = paste(unique(zone), collapse = ", "),
                         status = paste(unique(status), collapse = ", ")) %>%
        tidyr::replace_na(list(status = "Fished")) %>%
        st_join(metadata_sf %>% dplyr::select(-status)) %>%
        bind_cols(st_coordinates(.)) %>%
        as.data.frame() %>%
        dplyr::select(-c(geometry)) %>%
        dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
        dplyr::full_join(metadata %>% dplyr::select(campaignid, sample, latitude_dd, longitude_dd), .) #%>% dplyr::glimpse()
      
      
    } else {
      
      # message("view metadata.marineparks for Global")
      
      # metadata.marineparks <- metadata_sf %>%
      #   dplyr::select(-status) %>%
      #   st_intersection(all_data$world_marineparks) 
      # 
      
      metadata.marineparks <- metadata_sf %>%
        dplyr::select(-status) %>%
        st_join(all_data$world_marineparks, left = TRUE) %>%
        dplyr::filter(!status %in% "Not Reported") %>%
        dplyr::group_by(geometry) %>%
        dplyr::summarise(status = paste(unique(status), collapse = ", "),
                         zone = paste(unique(zone), collapse = ", ")) %>%
        tidyr::replace_na(list(status = "Fished")) %>%
        st_join(metadata_sf %>% dplyr::select(-status)) %>%
        bind_cols(st_coordinates(.)) %>%
        as.data.frame() %>%
        dplyr::select(-c(geometry)) %>%
        dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
        dplyr::full_join(metadata %>% dplyr::select(campaignid, sample, latitude_dd, longitude_dd), .) #%>% dplyr::glimpse()
      
    }
    
    # If user wants to keep status column only join zone
    if(input$status %in% "uploaded"){
      metadata.marineparks <- metadata.marineparks %>%
        dplyr::select(zone)
      
    } else {
      
      metadata.2 <- metadata.2 %>%
        dplyr::select(-c(status))
      
      metadata.marineparks <- metadata.marineparks %>%
        dplyr::select(zone, status)
      
    }
    
    # Only bind in new columns if there is data
    if(nrow(metadata.marineparks) > 0) {
      
      # print("view metadata.regions")
      
      # message("TEST 1")
      # glimpse(metadata.2)
      # 
      # message("TEST 2")
      # glimpse(metadata.marineparks)
      # 
      # message("TEST 3")
      metadata.regions <- metadata.2 %>%
        bind_cols(metadata.marineparks) %>%
        # glimpse() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, date_time, site, location, status, depth_m, successful_count, successful_length, zone, marine_region, observer_count, observer_length, inclusion_probability, visibility_m) %>% 
        tidyr::replace_na(list(status = "Fished")) %>%
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
    #message("final metadata regions")
    #glimpse(metadata.regions)
    
    metadata.regions
    
    
  })
  
  ## ► Preview metadata in dashboard ----
  output$table.metadata <- renderDataTable({
    
    # Checks on metadata
    message("checking metadata")
    glimpse(metadata.regions())
    
    errors <- ""
    
    # TODO change this to opcode and period
    # NA in sample
    # if(nrow(metadata.regions() %>% dplyr::filter(is.na(sample))) > 0){
    #   
    #   errors <- paste0(errors, "<li>The <b>Sample</b> column is missing values</li>", "<br>")
    #   # shinyalert("Missing Metadata", "The Sample column is missing values", type = "error")
    # }
    
    message("testing longitude")
    test <- metadata.regions() %>% 
      dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
      dplyr::filter(is.na(longitude_dd))
    
    print(nrow(test))
    
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
    if(nrow(metadata() %>% dplyr::filter(is.na(successful_count))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_count column is missing values</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_count column is missing values", type = "error")
    }
    
    # NA in successful_length
    if(nrow(metadata() %>% dplyr::filter(is.na(successful_length))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_length</b> column is missing values</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_length column is missing values", type = "error")
    }
    
    # Format of successful_count
    if(nrow(metadata() %>% dplyr::filter(!successful_count %in% c("Yes", "No"))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_count</b> column has values that are not 'Yes' or 'No'</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_count column has values that are not 'Yes' or 'No'", type = "error")
    }
    
    # Format of successful_length
    if(nrow(metadata() %>% dplyr::filter(!successful_length %in% c("Yes", "No"))) > 0){
      errors <- paste0(errors, "<li>The <b>successful_length</b> column has values that are not 'Yes' or 'No'</li>", "<br>")
      # shinyalert("Missing Metadata", "The successful_length column has values that are not 'Yes' or 'No'", type = "error")
    }
    
    if(!errors == ""){
      # beepr::beep("coin")
      
      shinyalert("Issues with Metadata", text = HTML(paste0("Please fix these before continuing<br><br>", errors)), type = "error", html = TRUE)
    }
    
    # Show table
    metadata.regions() %>% 
      dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
      dplyr::select(!sample)#%>% glimpse()
  })
  
  # TODO need to add correct format for date
  # TODO add distinct lat and longtiude within campaign
  
  
  
  ## ► Metadata score - valueBox ----
  output$metadata.score <- renderValueBox({
    
    valueBox(width = 3, round(metadata_format_score(), 2), "Sample metadata score", 
             icon = icon("percent"), color = "blue"
    )
  })
  
  ## ► Metadata score transect - valueBox ----
  output$metadata.score.t <- renderValueBox({
    
    valueBox(width = 3, round(metadata_format_score(), 2), "Sample metadata score", 
             icon = icon("percent"), color = "blue"
    )
  })
  
  
  
  ## _______________________________________________________ ----
  ##        Metadata checking for single point campaigns     ----
  ## _______________________________________________________ ----
  
  ## ► Total number of samples - valueBox ----
  output$metadata.no.samples <- shinydashboard::renderValueBox({
    
    message("view number of samples")
    
    metadata.samples <- metadata() %>%
      dplyr::distinct(campaignid, sample) %>% dplyr::glimpse()
    
    shinydashboard::valueBox(width = 3, nrow(metadata.samples), "Samples in the Sample Metadata", 
                             icon = icon("list"), color = "blue"
    )
  })
  
  ## ► Samples without points - dataframe ----
  metadata.samples.without.fish <- reactive({
    
    message("XX metadata samples")
    
    metadata.samples <- metadata.regions() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      distinct() %>%
      mutate(sample = as.factor(sample)) %>%
      glimpse()
    
    if(input$upload %in% "EM"){
      samples <- points() %>%
        distinct(campaignid, sample) 
    } else {
      
      message("XX count samples")
      # print(str(count_data()))
      
      samples <- count_data() %>%
        distinct(campaignid, sample) %>% glimpse()
    }
    missing.fish <- anti_join(metadata.samples, samples) #%>%
      #dplyr::select(-sample)
    
    return(missing.fish)
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
      samples <- count_data() %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
    }
    missing.metadata <- anti_join(samples, metadata.samples) #%>%
      #dplyr::select(-sample)
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
    missing.fish <- anti_join(metadata.samples, samples) #%>%
      #dplyr::select(-sample)
    
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
  
  ## ► Metadata samples duplicated - dataframe ----
  metadata.samples.duplicated <- reactive({
    metadata.samples <- metadata()
    
    duplicates <- metadata.samples %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(number_of_repeats = n()) %>%
      dplyr::filter(number_of_repeats > 1) %>%
      dplyr::left_join(metadata.samples) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, everything())%>%
      dplyr::ungroup()
  })
  
  ## ► Metadata samples duplicated - valueBox ----
  output$metadata.samples.duplicated <- renderValueBox({
    
    if (dim(metadata.samples.duplicated())[1] > 0) {
      total <- nrow(metadata.samples.duplicated())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Duplicated Sample(s) in metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata samples duplicated - onclick ----
  onclick('click.metadata.samples.duplicated',
          showModal(modalDialog(
            title = "Duplicated Sample(s) in metadata",
            easyClose = TRUE,
            renderDataTable(metadata.samples.duplicated(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Metadata coordinates duplicated - dataframe ----
  metadata.coordinates.duplicated <- reactive({
    metadata.coordinates <- metadata()
    
    duplicates <- metadata.coordinates %>%
      dplyr::group_by(latitude_dd, longitude_dd) %>%
      dplyr::summarise(number_of_repeats = n()) %>%
      dplyr::filter(number_of_repeats > 1) %>%
      dplyr::left_join(metadata.coordinates) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, everything()) %>%
      dplyr::ungroup()
    
  })
  
  ## ► Metadata coordinates duplicated - valueBox ----
  output$metadata.coordinates.duplicated <- renderValueBox({
    
    if (dim(metadata.coordinates.duplicated())[1] > 0) {
      total <- nrow(metadata.coordinates.duplicated())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Duplicated coordinates in metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata coordinates duplicated - onclick ----
  onclick('click.metadata.coordinates.duplicated',
          showModal(modalDialog(
            title = "Duplicated coordinates in metadata",
            easyClose = TRUE,
            renderDataTable(metadata.coordinates.duplicated(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Metadata coordinates on land - dataframe ----
  metadata.on.land <- reactive({
    metadata <- metadata()
    metadata_sf <- st_as_sf(metadata(), coords = c("longitude_dd", "latitude_dd"), crs = 4326)
    
    # Perform spatial join to check if points are on land
    # message("land flags")
    land_flags <- st_intersection(metadata_sf, land()) #%>% dplyr::glimpse()
    
    # message("without geom")
    land_flags <- sf::st_drop_geometry(land_flags) #%>% dplyr::glimpse()
    
  })
  
  ## ► Metadata coordinates on land - valueBox ----
  output$metadata.on.land <- renderValueBox({
    
    if (dim(metadata.on.land())[1] > 0) {
      total <- nrow(metadata.on.land())
      col <- "yellow"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Coordinates in metadata potentially on land",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata coordinates on land  - onclick ----
  onclick('click.metadata.on.land',
          showModal(modalDialog(
            title = "Coordinates on land",
            easyClose = TRUE,
            renderDataTable(metadata.on.land(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  # ## ► Leaflet map ----
  
  # show shiny alert once user clicks on metadata tab
  
  observeEvent(input$tabs, {
    if(input$tabs == "checkmetadata"){
      if(input$lifehistory %in% c("aus", "imcra")){
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
    
    metadata <- metadata %>%
      dplyr::mutate(status = stringr::str_replace_all(status, c("No-Take" = "No-take")))
    
    message("view status")
    print(unique(metadata$status))
    
    map <- leaflet_basemap(data = metadata) %>%
      
      addAwesomeMarkers(icon = ~iconSet[status], label = ~as.character(sample), popup = ~content, ~longitude_dd, ~latitude_dd) %>%
      
      fitBounds(lng1 = min(metadata$longitude_dd),
                lat1 = min(metadata$latitude_dd),
                lng2 = max(metadata$longitude_dd),
                lat2 = max(metadata$latitude_dd))
    
    
    if (dim(metadata.on.land())[1] > 0) {
      
      points.potentially.on.land <- metadata.on.land() %>%
        mutate(content = "Potentially on land") %>%
        mutate(land = "Land") %>%
        left_join(metadata()) #%>% dplyr::glimpse()
      
      map <- map %>%
        addAwesomeMarkers(data = points.potentially.on.land,
                          icon = ~icon.on.land[land], label = ~as.character(sample), popup = ~content, ~longitude_dd, ~latitude_dd)%>%
        leafgl::addGlPolygons(
          data = all_data$marineparks.single,
          fillColor = ~ all_data$comm.pal(zone),
          popup = "ZONE_TYPE",
          group = "Marine parks",
          opacity = 0.9
        )
    }
    
    if(input$lifehistory %in% c("aus", "imcra")){
      #   
      #   req(all_data$marineparks.single)
      #   req(all_data$comm.pal)
      #   
      #   isolate({
      #   map <- map %>%
      #     leafgl::addGlPolygons(data =  all_data$marineparks.single, # Changed from clipped
      #                   fillColor = ~ all_data$comm.pal(zone),
      #                   popup =  "ZONE_TYPE",
      #                   group = "Marine parks",
      #                   opacity = 0.9) %>%
      #     addLayersControl(
      #       overlayGroups = c("Marine parks"),
      #       options = layersControlOptions(collapsed = FALSE))
      #   # hideGroup("Marine parks")
      #   })
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
  
  # observe({
  #   if (input$lifehistory %in% "aus") {
  #     leafletProxy("map.metadata") %>%
  #       clearGroup("Marine parks") %>%
  #       leafgl::addGlPolygons(
  #         data = all_data$marineparks.single,
  #         fillColor = ~ all_data$comm.pal(zone),
  #         popup = "ZONE_TYPE",
  #         group = "Marine parks",
  #         opacity = 0.9
  #       )
  #   }
  # })
  
  ## _______________________________________________________ ----
  ##           Metadata checking for transect campaigns      ----
  ## _______________________________________________________ ----
  
  ## ► Total number of samples - valueBox ----
  output$metadata.no.samples.t <- renderValueBox({
    
    # message("number of samples in transect metadata")
    
    metadata.samples <- metadata() %>%
      dplyr::distinct(campaignid, sample) #%>% dplyr::glimpse()
    
    valueBox(width = 3, nrow(metadata.samples), "Samples in the Sample Metadata",
             icon = icon("list"), color = "blue"
    )
  })
  
  ## ► Samples without lengths - dataframe----
  metadata.samples.without.fish.t <- reactive({
    
    message("metadata.samples.without.fish.t")
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), successful_count, successful_length) %>%
      dplyr::distinct() %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::filter(successful_length %in% c("Yes", "Y","YES"))
    
    length.samples <- length() %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
      dplyr::distinct()
    
    missing.fish <- anti_join(metadata.samples, length.samples) %>%
      dplyr::select(!sample) #%>% dplyr::glimpse()
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
  
  ## ► Metadata samples duplicated - dataframe ----
  metadata.samples.duplicated.t <- reactive({
    metadata.samples <- metadata()
    
    duplicates <- metadata.samples %>%
      dplyr::group_by(campaignid, sample) %>%
      dplyr::summarise(number_of_repeats = n()) %>%
      dplyr::filter(number_of_repeats > 1) %>%
      dplyr::left_join(metadata.samples) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, everything())%>%
      dplyr::ungroup()
  })
  
  ## ► Metadata samples duplicated - valueBox ----
  output$metadata.samples.duplicated.t <- renderValueBox({
    
    if (dim(metadata.samples.duplicated.t())[1] > 0) {
      total <- nrow(metadata.samples.duplicated.t())
      col <- "red"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Duplicated Sample(s) in metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata samples duplicated - onclick ----
  onclick('click.metadata.samples.duplicated.t',
          showModal(modalDialog(
            title = "Duplicated Sample(s) in metadata",
            easyClose = TRUE,
            renderDataTable(metadata.samples.duplicated.t(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Metadata coordinates duplicated - dataframe ----
  metadata.coordinates.duplicated.t <- reactive({
    metadata.coordinates.t <- metadata()
    
    duplicates <- metadata.coordinates.t %>%
      dplyr::group_by(latitude_dd, longitude_dd) %>%
      dplyr::summarise(number_of_repeats = n()) %>%
      dplyr::filter(number_of_repeats > 1) %>%
      dplyr::left_join(metadata.coordinates.t) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), latitude_dd, longitude_dd, everything()) %>%
      dplyr::ungroup()
    
  })
  
  ## ► Metadata coordinates duplicated - valueBox ----
  output$metadata.coordinates.duplicated.t <- renderValueBox({
    
    if (dim(metadata.coordinates.duplicated.t())[1] > 0) {
      total <- nrow(metadata.coordinates.duplicated.t())
      col <- "yellow"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Duplicated coordinates in metadata",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata coordinates duplicated - onclick ----
  onclick('click.metadata.coordinates.duplicated.t',
          showModal(modalDialog(
            title = "Duplicated coordinates in metadata",
            easyClose = TRUE,
            renderDataTable(metadata.coordinates.duplicated.t(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  ## ► Metadata coordinates on land - dataframe ----
  metadata.on.land.t <- reactive({
    metadata <- metadata()
    metadata_sf <- st_as_sf(metadata(), coords = c("longitude_dd", "latitude_dd"), crs = 4326)
    
    # Perform spatial join to check if points are on land
    land_flags <- st_intersection(metadata_sf, land())
    land_flags <- sf::st_drop_geometry(land_flags) #%>% glimpse()
    
  })
  
  ## ► Metadata coordinates on land - valueBox ----
  output$metadata.on.land.t <- renderValueBox({
    
    if (dim(metadata.on.land.t())[1] > 0) {
      total <- nrow(metadata.on.land.t())
      col <- "yellow"
      
    } else {
      total = 0
      col <- "green"
    }
    
    valueBox(width = 2,
             total,
             "Coordinates in metadata potentially on land",
             icon = icon("exclamation-circle"), color = col
    )
  })
  
  ## ► Metadata coordinates on land  - onclick ----
  onclick('click.metadata.on.land.t',
          showModal(modalDialog(
            title = "Coordinates on land",
            easyClose = TRUE,
            renderDataTable(metadata.on.land.t(), rownames = FALSE, options = list(paging = FALSE, searching = TRUE)))
          ))
  
  
  ## ► Leaflet map ----
  
  # show shiny alert once user clicks on metadata tab
  
  observeEvent(input$tabs, {
    if(input$tabs == "checkmetadatat"){
      if(input$lifehistory %in% c("aus", "imcra")){
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
    
    if (dim(metadata.on.land.t())[1] > 0) {
      
      points.potentially.on.land <- metadata.on.land.t() %>%
        mutate(content = "Potentially on land") %>%
        mutate(land = "Land") %>%
        left_join(metadata()) #%>%
      #glimpse()
      
      map <- map %>%
        addAwesomeMarkers(data = points.potentially.on.land,
                          icon = ~icon.on.land[land], label = ~as.character(sample), popup = ~content, ~longitude_dd, ~latitude_dd)
    }
    
    if(input$lifehistory %in% c("aus", "imcra")){
      map <- map #%>%
      # addGlPolygons(data =  all_data$marineparks.single, # Changed from clipped
      #               fillColor = ~ all_data$comm.pal(zone),
      #               # color = ~ all_data$comm.pal(ZoneName),
      #               popup =  "ZONE_TYPE",
      #               group = "Marine parks",
      #               opacity = 0.9) %>%
      # addLayersControl(
      #   overlayGroups = c("Marine parks"),
      #   options = layersControlOptions(collapsed = FALSE))
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
        dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Period.txt" = ""))) #%>% glimpse()
      
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
      
      periods <-  read.delim(checkem_example_path("2024-05_Exmouth-Gulf_stereo-BRUVs_Period.txt"), na.strings = "") %>%
        clean_names() %>%
        dplyr::mutate(sample = as.factor(opcode)) %>%
        dplyr::mutate(campaignid = "2024-05_Exmouth-Gulf_stereo-BRUVs") %>%
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
          dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Points.txt" = ""))) #%>% glimpse()
        
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
        
        points <-  read.delim(checkem_example_path("2024-05_Exmouth-Gulf_stereo-BRUVs_Points.txt"), na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2024-05_Exmouth-Gulf_stereo-BRUVs") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      }
      
      #message("checking points 1")
      # print(unique(points$family))
      
      
      points <- points %>%
        #glimpse() %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::rename(em_comment = comment, period_time = periodtime) #%>%
      #glimpse()
      
      #message("accents")
      
      # print(unique(points$genus))
      
      family_accents <- points %>%
        distinct(family) %>%
        dplyr::mutate(fail = str_detect(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE)# %>%
      #glimpse()
      
      if(nrow(family_accents > 0)){
        errors <- paste0("<li>In the <b>Family</b> column.", "</li>", "<br>")
      } else {
        errors = ""
      }
      
      genus_accents <- points %>%
        distinct(genus) %>%
        dplyr::mutate(fail = str_detect(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(genus_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Genus</b> column.", "</li>", "<br>")
      }
      
      species_accents <- points %>%
        distinct(species) %>%
        dplyr::mutate(fail = str_detect(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(species_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Species</b> column.", "</li>", "<br>")
      }
      
      accents <- bind_rows(family_accents, genus_accents, species_accents)
      
      if(nrow(accents > 0)){
        shinyalert("Points Data Contains Special Characters:",
                   paste0(errors, "<br> Please check the spelling before proceeding. The special characters will be removed, and the species names may not match your chosen vocabulary (life history information)"),
                   type = "warning", html = TRUE)
      }
      
      
      return(points)
      
      
    }
  })
  
  # ► Preview points ----
  #TODO change this to be points/count
  output$table.points <- renderDataTable({
    
    points()
    
    
  })
  
  ## ► Create MaxN (Raw) ----
  maxn.raw <- reactive({
    
    # message("checking maxn raw")
    # print(unique(points()$family))
    
    maxn <- points() %>%
      dplyr::mutate(species = str_remove_all(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(genus = str_remove_all(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(family = str_remove_all(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(species = as.character(tolower(species))) %>%
      dplyr::mutate(genus = as.character(stringr::str_to_sentence(genus))) %>%
      dplyr::mutate(family = as.character(stringr::str_to_sentence(family))) %>%
      dplyr::mutate(number = as.numeric(number)) %>%
      replace_na(list(family = "Unknown", genus = "Unknown", species = "spp"))
    
    
    if(input$stage %in% "No"){
      maxn <- maxn %>%
        dplyr::group_by(campaignid, sample, filename, period_time, frame, family, genus, species) %>% # removed comment 21/10/21 removed code 02/08/23
        dplyr::summarise(maxn = sum(number)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code 02/08/23
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup()
    } else {
      
      # message("maxn stage")
      
      maxn <- maxn %>%
        dplyr::group_by(campaignid, sample, filename, period_time, frame, family, genus, species, stage) %>% # removed comment 21/10/21 removed code 02/08/23
        dplyr::summarise(maxn = sum(number)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code 02/08/23
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup() #%>% glimpse()
    }
    
    
    maxn <- maxn %>%
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
      filter(!family %in% c("Unknown")) #%>% glimpse() # Added 2023-08-01
    
    
    
  })
  
  maxn.clean <- reactive({
    
    if(input$stage %in% "No"){
      
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
      
    } else {
      
      maxn.clean <- dplyr::full_join(maxn.raw(), metadata.regions()) %>%
        dplyr::left_join(., synonyms()) %>% #by = c("family", "genus", "species"),
        dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
        dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
        dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
        dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup() %>%
        as_tibble()
      
    }
  })
  
  ## ►  Create MaxN (Complete) -----
  maxn.complete <- reactive({
    if(input$upload %in% "EM"){
      
      if(input$stage %in% "No"){
        
        maxn.complete <- maxn.clean() %>%
          dplyr::full_join(metadata.regions()) %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn)) %>% # removed code
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>% # removed code
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species) %>% # removed code
          dplyr::summarise(maxn = sum(maxn)) %>%
          dplyr::ungroup()
      } else {
        maxn.complete <- maxn.clean() %>%
          dplyr::full_join(metadata.regions()) %>%
          dplyr::select(c(campaignid, sample, family, genus, species, maxn, stage)) %>% # removed code
          tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, stage)) %>% # removed code
          replace_na(list(maxn = 0)) %>%
          dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code
          dplyr::summarise(maxn = sum(maxn)) %>%
          dplyr::ungroup()
      }
    }
  })
  
  ## ► Create filtered MaxN download -----
  maxn.complete.download <- reactive({
    if(input$upload %in% "EM"){
      maxn <- full_join(maxn.raw(), metadata.regions()) # can't use clean as have already changed synonyms
      
      if (input$error.synonyms == TRUE) {
        
        if(input$stage %in% "No"){
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
        } else {
          maxn.complete <- dplyr::left_join(maxn, synonyms()) %>% #, by = c("family", "genus", "species")
            dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
            dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
            dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
            dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code
            dplyr::slice(which.max(maxn)) %>%
            dplyr::ungroup() %>%
            dplyr::full_join(metadata.regions()) %>%
            dplyr::select(c(campaignid, sample, family, genus, species, maxn, stage)) %>% # removed code
            tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, stage)) %>% # removed code
            replace_na(list(maxn = 0)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code
            dplyr::summarise(maxn = sum(maxn)) %>%
            ungroup() %>%
            dplyr::left_join(metadata.regions()) %>%
            dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
        }
        
      }
      else{
        
        if(input$stage %in% "No"){
          
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
        } else {
          
          maxn.complete <- maxn %>%
            dplyr::select(c(campaignid, sample, family, genus, species, maxn, stage)) %>% # removed code
            dplyr::full_join(metadata.regions()) %>%
            tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, stage)) %>% # removed code
            replace_na(list(maxn = 0)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, stage) %>% # removed code
            dplyr::summarise(maxn = sum(maxn)) %>%
            ungroup() %>%
            dplyr::left_join(metadata.regions()) %>%
            dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
          #glimpse()
          
        }
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
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "family", "genus", "species", "stage")), everything())
        
      } else {
        
        maxn.area <- maxn.area %>%
          dplyr::filter(!maxn %in% 0) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "family", "genus", "species", "stage")), maxn)
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
  
  
  ## ► Species richness - dataframe ----
  maxn.species.richness <- reactive({
    
    # Set a default value for `input$rich` if it is NULL
    rich_type <- if (!is.null(input$rich)) input$rich else "species"
    
    # Dynamically set grouping variables
    group_vars <- switch(rich_type,
                         family = c("family"),
                         genus = c("family", "genus"),
                         species = c("family", "genus", "species"))
    
    # Select columns dynamically based on the richness type
    lh_cols <- if (rich_type == "species" & input$lifehistory %in% c("aus", "imcra")) {
      c("class", "order", all_of(group_vars), "australian_common_name")  # TODO ,  include this for aus but not global # Include australian_common_name for species
    } else {
      c("class", "order", all_of(group_vars))  # Exclude australian_common_name for other richness types
    }
    
    lh <- life.history() %>%
      dplyr::select(all_of(lh_cols))  # Dynamically select columns based on richness type
    
    if (input$upload %in% "EM") {
      maxn <- maxn.clean() %>%
        dplyr::group_by(across(all_of(group_vars))) %>%
        dplyr::summarise(total_number_observed = sum(maxn), .groups = "drop") %>%
        dplyr::select(all_of(group_vars), total_number_observed) %>%
        left_join(lh, by = group_vars) %>%  # Join based on grouping columns
        dplyr::select(class, order, everything()) %>%
        distinct()
      
    } else {
      maxn <- count.clean() %>%
        dplyr::group_by(across(all_of(group_vars))) %>%
        dplyr::summarise(total_number_observed = sum(maxn), .groups = "drop") %>%
        dplyr::select(all_of(group_vars), total_number_observed) %>%
        left_join(lh, by = group_vars) %>%  # Join based on grouping columns
        dplyr::select(class, order, everything()) %>%
        distinct()
    }
    
    maxn %>%
      dplyr::filter(!is.na(class))
  })
  ## ►  Species Richness EM - value box ----
  output$maxn.species.richness.em <- renderValueBox({
    total <- nrow(maxn.species.richness())
    
    # Set a default value for `input$rich` if it is NULL
    rich_type <- if (!is.null(input$rich)) input$rich else "species"
    
    if(rich_type %in% "family"){
      
      text <- "Number of families observed"
      
    } else if (rich_type %in% "genus"){
      
      text <- "Number of genera observed"
      
    } else {
      
      text <- "Number of species observed"
      
    }
    
    valueBox(total,
             text,
             icon = icon("fish"), color = "blue"
    )
  })
  
  ## ►  Species Richness GEN - value box ----
  output$maxn.species.richness.gen <- renderValueBox({
    total <- nrow(maxn.species.richness())
    
    # Set a default value for `input$rich` if it is NULL
    rich_type <- if (!is.null(input$rich)) input$rich else "species"
    
    if(rich_type %in% "family"){
      
      text <- "Number of families observed"
      
    } else if (rich_type %in% "genus"){
      
      text <- "Number of genera observed"
      
    } else {
      
      text <- "Number of species observed"
      
    }
    
    valueBox(total,
             text,
             icon = icon("fish"), color = "blue"
    )
  })
  
  # Eventmeasure ----
  onclick('click.maxn.species.richness.em',
          {
            # Check if there are any rows in maxn.species.not.observed.lh()
            warning_rows <- dim(maxn.species.not.observed.lh() %>%
                                  distinct(family, genus, species))[1]
            
            # Show shinyalert warning if there are rows
            if (warning_rows > 0) {
              shinyalert("Warning",
                         "You have uploaded data that includes species that are not in the chosen life history list. These species are not listed below. Please check the 'Species not in life history list' check",
                         type = "warning", closeOnEsc = TRUE, closeOnClickOutside = FALSE,
                         showCancelButton = FALSE, confirmButtonText = "OK")
            }
            
            # Once the user closes the alert, show the original modal
            # observeEvent(input$`shinyalert-close`, {
            showModal(modalDialog(
              title = paste("Richness"),
              size = "l",
              easyClose = TRUE,
              
              # Radio buttons for richness type
              radioButtons("rich", "Type of richness:",
                           c("Family" = "family",
                             "Genus" = "genus",
                             "Species" = "species"), selected = "species", inline = TRUE),
              
              # Radio buttons for animal type
              radioButtons("fish.sharks.rays", "Animals:",
                           c("Only Fish, Sharks and Rays" = "fish",
                             "Other animals" = "other"), selected = "fish", inline = TRUE),
              
              # Conditionally display checkboxInput
              conditionalPanel(
                condition = "input.rich == 'species'",
                checkboxInput("species.richness.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE)
              ),
              
              renderDataTable(
                {
                  # Initialize the dataset
                  richness_data <- maxn.species.richness()
                  
                  # Apply species filtering if selected
                  if (input$rich == "species" && input$species.richness.filter.spp) {
                    richness_data <- richness_data %>%
                      filter(!species %in% c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))
                  }
                  
                  # Apply animal type filtering based on the radio button
                  if (input$fish.sharks.rays == "fish") {
                    richness_data <- richness_data %>%
                      filter(class %in% c("Elasmobranchii", "Actinopterygii", "Myxini"))
                  } else {
                    richness_data <- richness_data %>%
                      filter(!class %in% c("Elasmobranchii", "Actinopterygii", "Myxini"))
                  }
                  
                  # Return the final filtered dataset
                  richness_data
                }, options = list(paging = TRUE, row.names = FALSE, searching = TRUE)
              )
            ))
          })
  
  
  # Generic ----
  onclick('click.maxn.species.richness.gen',
          {
            # Check if there are any rows in maxn.species.not.observed.lh()
            warning_rows <- dim(maxn.species.not.observed.lh() %>%
                                  distinct(family, genus, species))[1]
            
            # Show shinyalert warning if there are rows
            if (warning_rows > 0) {
              shinyalert("Warning",
                         "You have uploaded data that includes species that are not in the chosen life history list. These species are not listed below. Please check the 'Species not in life history list' check",
                         type = "warning", closeOnEsc = TRUE, closeOnClickOutside = FALSE,
                         showCancelButton = FALSE, confirmButtonText = "OK")
            }
            
            # Once the user closes the alert, show the original modal
            # observeEvent(input$`shinyalert-close`, {
            showModal(modalDialog(
              title = paste("Richness"),
              size = "l",
              easyClose = TRUE,
              
              # Radio buttons for richness type
              radioButtons("rich", "Type of richness:",
                           c("Family" = "family",
                             "Genus" = "genus",
                             "Species" = "species"), selected = "species", inline = TRUE),
              
              # Radio buttons for animal type
              radioButtons("fish.sharks.rays", "Animals:",
                           c("Only Fish, Sharks and Rays" = "fish",
                             "Other animals" = "other"), selected = "fish", inline = TRUE),
              
              # Conditionally display checkboxInput
              conditionalPanel(
                condition = "input.rich == 'species'",
                checkboxInput("species.richness.filter.spp", label = "Filter out sp1, sp2, spp etc.", value = FALSE)
              ),
              
              renderDataTable(
                {
                  # Initialize the dataset
                  richness_data <- maxn.species.richness()
                  
                  # Apply species filtering if selected
                  if (input$rich == "species" && input$species.richness.filter.spp) {
                    richness_data <- richness_data %>%
                      filter(!species %in% c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp"))
                  }
                  
                  # Apply animal type filtering based on the radio button
                  if (input$fish.sharks.rays == "fish") {
                    richness_data <- richness_data %>%
                      filter(class %in% c("Elasmobranchii", "Actinopterygii", "Myxini"))
                  } else {
                    richness_data <- richness_data %>%
                      filter(!class %in% c("Elasmobranchii", "Actinopterygii", "Myxini"))
                  }
                  
                  # Return the final filtered dataset
                  richness_data
                }, options = list(paging = TRUE, row.names = FALSE, searching = TRUE)
              )
            ))
          })
  
  
  ## ► Species not observed in region- dataframe ----
  maxn.species.not.observed <- reactive({
    
    # message("view expanded LH")
    # glimpse(life.history.expanded())
    
    if(input$upload %in% "EM"){
      
      maxn <- dplyr::anti_join(maxn.clean(), life.history.expanded(), by = c("family", "genus", "species", "marine_region")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "stage")), family, genus, species, marine_region, maxn) %>%
        dplyr::group_by(campaignid, across(dplyr::any_of(c("opcode", "period", "stage"))), family, genus, species, marine_region) %>%
        dplyr::summarise(total_observed = sum(maxn)) %>%
        # dplyr::distinct() %>%
        dplyr::semi_join(., life.history.expanded(), by = c("family", "genus", "species"))
      
    } else {
      
      maxn <- life.history.expanded() %>%
        dplyr::anti_join(count.clean(), ., by = c("family", "genus", "species", "marine_region")) %>%
        dplyr::filter(maxn > 0) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "stage")), family, genus, species, marine_region, maxn) %>%
        # dplyr::distinct() %>%
        dplyr::group_by(campaignid, across(dplyr::any_of(c("opcode", "period", "stage"))), family, genus, species, marine_region) %>%
        dplyr::summarise(total_observed = sum(maxn)) %>%
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
        
        maxn.species.not.observed() %>% 
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>% 
        dplyr::group_by(campaignid, family, genus, species, marine_region) %>%
        dplyr::summarise(total_observed = sum(total_observed)) %>%
        dplyr::rename('marine region not observed in' = marine_region)#%>%
        # distinct(campaignid, family, genus, species)
      
      else if (input$maxn.filter.spp == TRUE & input$maxn.observed.distinct == FALSE)
        
        maxn.species.not.observed() %>% 
        filter(!species%in%c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10", "spp", "sp")) %>%
        dplyr::rename('marine region not observed in' = marine_region) 
      
      else if (input$maxn.filter.spp == FALSE & input$maxn.observed.distinct == TRUE)
        
        maxn.species.not.observed() %>% 
        dplyr::group_by(campaignid, family, genus, species, marine_region) %>%
        dplyr::summarise(total_observed = sum(total_observed)) %>%
        dplyr::rename('marine region not observed in' = marine_region) 
      #distinct(campaignid, family, genus, species)
      
      else
        
        maxn.species.not.observed() %>%
        dplyr::rename('marine region not observed in' = marine_region),  rownames = FALSE,
      
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
  
  ## ► MaxN spp - dataframe ----
  maxn_spp <- reactive({
    
    if(input$upload %in% "EM"){
      
      number_of_spps <- maxn.clean() %>%
        dplyr::filter(species %in% c("spp")) #, "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10"
      
    } else {
      
      number_of_spps <- count.clean() %>%
        dplyr::filter(species %in% c("spp")) #, "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10"
    }
    
    return(number_of_spps)
    
  })
  
  maxn_spp_summary <- reactive({
    
    total_fish <- maxn.total.abundances() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(total_abundance = sum(total_abundance))
    
    message("view maxn spps")
    
    maxn_spp() %>%
      dplyr::group_by(campaignid) %>%
      dplyr::summarise(number_of_spps = sum(maxn)) %>%
      dplyr::ungroup() %>%
      left_join(total_fish) %>%
      glimpse() %>%
      dplyr::mutate(percent_spp = round((number_of_spps/total_abundance)*100, digits = 1))
    
  })
  
  maxn_spp_observer <- reactive({
    
    total_fish <- maxn.total.abundances() %>%
      left_join(metadata()) %>%
      dplyr::group_by(campaignid, observer_count) %>%
      dplyr::summarise(total_fish_observed = sum(total_abundance))
    
    deployments_watched <- metadata() %>%
      dplyr::filter(successful_count %in% "Yes") %>%
      dplyr::group_by(observer_count) %>%
      dplyr::summarise(deployments_watched = n())
    
    maxn_spp() %>%
      dplyr::group_by(campaignid, observer_count) %>%
      dplyr::summarise(number_of_spps = sum(maxn)) %>%
      left_join(total_fish) %>%
      dplyr::mutate(percent_spp = round((number_of_spps/total_fish_observed)*100, digits = 1)) %>%
      left_join(deployments_watched)
    
  })
  
  
  ## ►  MaxN spp - onclick ----
  onclick('click.maxn.spp', showModal(modalDialog(
    
    title = "Number of spps", size = "l", easyClose = TRUE,
    checkboxInput("maxn.spp.observer", label = "Split by Observer_count", value = FALSE),
    renderDataTable(
      
      if(input$maxn.spp.observer == FALSE)
        maxn_spp_summary()
      else if(input$maxn.spp.observer == TRUE)
      maxn_spp_observer(),
      rownames = FALSE,
      options = list(paging = FALSE, row.names = FALSE, searching = TRUE)))))
  
  ## ►  MaxN spp - valuebox ----
  output$maxn.spp <- renderValueBox({
    
    total_spps <- sum(maxn_spp()$maxn)
    
    text <- paste0(total_spps, " (", round((total_spps/sum(maxn.total.abundances()$total_abundance))*100), "%)")
    
    valueBox(width = 2,
             text,
             "Number of individuals not identified to species level ",
             icon = icon("list"), color = "blue"
    )
  })
  
  ## SASHAS IDEA ----
  ## ► MaxN spp - dataframe ----
  species_that_have_spps <- reactive({
    
    if(input$upload %in% "EM"){
      maxn <- maxn.clean() 
    } else {
      maxn <- count.clean() 
    }
    
    spps_taxa <- maxn %>%
      dplyr::filter(species %in% c("spp", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8", "sp9", "sp10")) %>%
      dplyr::distinct(family, genus)
    
    all_spps <- maxn %>%
      semi_join(spps_taxa) %>%
      dplyr::group_by(campaignid, family, genus, species) %>%
      dplyr::summarise(total_observered = sum(maxn)) %>%
      glimpse()
    
    return(all_spps)
    
  })
  
  ## ►  MaxN spp - onclick ----
  onclick('click.species_that_have_spps', showModal(modalDialog(
    
    title = "species_that_have_spps", size = "l", easyClose = TRUE,
    # checkboxInput("maxn.spp.observer", label = "Split by Observer_count", value = FALSE),
    renderDataTable(
      
      species_that_have_spps(),
      rownames = FALSE,
      options = list(paging = FALSE, row.names = FALSE, searching = TRUE)))))
  
  ## ►  MaxN spp - valuebox ----
  output$species_that_have_spps <- renderValueBox({
    
    total_spps <- sum(maxn_spp()$maxn)
    
    text <- paste0(total_spps, " (", round((total_spps/sum(maxn.total.abundances()$total_abundance))*100), "%)")
    
    valueBox(width = 2,
             text,
             "Number of spps",
             icon = icon("list"), color = "blue"
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
  
  #'   ## ►  Site plot ----
  #'   output$maxn.site.plot <- renderPlot({
  #'     
  #'     if(input$upload %in% "EM"){
  #'       
  #'       maxn.per.sample <- maxn.complete() %>%
  #'         dplyr::left_join(metadata.regions()) %>%
  #'         dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  #'         dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
  #'         dplyr::group_by(campaignid, sample, site) %>%
  #'         dplyr::summarise(maxn = sum(maxn))
  #'       
  #'     } else {
  #'       
  #'       maxn.per.sample <- count.complete() %>%
  #'         dplyr::left_join(metadata.regions()) %>%
  #'         dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  #'         dplyr::filter(scientific %in% c(input$maxn.species.dropdown)) %>%
  #'         dplyr::group_by(campaignid, sample, site) %>%
  #'         dplyr::summarise(maxn = sum(maxn))
  #'       
  #'     }
  #'     
  #'     
  #'     scientific.name <- input$maxn.species.dropdown
  #'     grob.sci <- grobTree(textGrob(as.character(scientific.name), x = 0.01,  y = 0.97, hjust = 0, 
  #'                                   gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
  #'     
  #'     ggplot(maxn.per.sample, aes(x = site, y = maxn)) + 
  #'       stat_summary(fun.y = mean, geom = "bar", colour = "black", fill = "white") +
  #'       stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  #'       geom_hline(aes(yintercept = 0))+
  #'       xlab("Site")+
  #'       ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
  #'       scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  #'       annotation_custom(grob.sci)+ 
  #'       Theme1
  #'   })
  #'   
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
  #'   
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
        
        length <-  read.delim(checkem_example_path("2024-05_Exmouth-Gulf_stereo-BRUVs_Lengths.txt"), na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2024-05_Exmouth-Gulf_stereo-BRUVs") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      }
      
      
      family_accents <- length %>%
        distinct(family) %>%
        dplyr::mutate(fail = str_detect(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(family_accents > 0)){
        errors <- paste0("<li>In the <b>Family</b> column.", "</li>", "<br>")
      } else {
        errors = ""
      }
      
      genus_accents <- length %>%
        distinct(genus) %>%
        dplyr::mutate(fail = str_detect(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(genus_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Genus</b> column.", "</li>", "<br>")
      }
      
      species_accents <- length %>%
        distinct(species) %>%
        dplyr::mutate(fail = str_detect(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(species_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Species</b> column.", "</li>", "<br>")
      }
      
      accents <- bind_rows(family_accents, genus_accents, species_accents)
      
      if(nrow(accents > 0)){
        shinyalert("Length Data Contains Special Characters:",
                   paste0(errors, "<br> Please check the spelling before proceeding. The special characters will be removed, and the species names may not match your chosen vocabulary (life history information)"),
                   type = "warning", html = TRUE)
      }
      
      
      # return(length)
      
      
      length <- length %>%
        dplyr::mutate(species = str_remove_all(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::mutate(genus = str_remove_all(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::mutate(family = str_remove_all(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::mutate(species = as.character(tolower(species))) %>%
        dplyr::mutate(genus = as.character(stringr::str_to_sentence(genus))) %>%
        dplyr::mutate(family = as.character(stringr::str_to_sentence(family))) %>%
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
        
        threedpoints <-  read.delim(checkem_example_path("2024-05_Exmouth-Gulf_stereo-BRUVs_3DPoints.txt"), na.strings = "") %>%
          clean_names() %>%
          dplyr::mutate(sample = opcode) %>%
          mutate(sample = as.factor(sample)) %>%
          dplyr::mutate(campaignid = "2024-05_Exmouth-Gulf_stereo-BRUVs") %>%
          as.data.frame()
        
        # TODO add an example dataset for DOVs
      }
      
      
      
      family_accents <- threedpoints %>%
        distinct(family) %>%
        dplyr::mutate(fail = str_detect(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(family_accents > 0)){
        errors <- paste0("<li>In the <b>Family</b> column.", "</li>", "<br>")
      } else {
        errors = ""
      }
      
      genus_accents <- threedpoints %>%
        distinct(genus) %>%
        dplyr::mutate(fail = str_detect(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(genus_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Genus</b> column.", "</li>", "<br>")
      }
      
      species_accents <- threedpoints %>%
        distinct(species) %>%
        dplyr::mutate(fail = str_detect(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::filter(!fail %in% FALSE) #%>%
      #glimpse()
      
      if(nrow(species_accents > 0)){
        errors <- paste0(errors, "<li>In the <b>Species</b> column.", "</li>", "<br>")
      }
      
      accents <- bind_rows(family_accents, genus_accents, species_accents)
      
      if(nrow(accents > 0)){
        shinyalert("3D point Data Contains Special Characters:",
                   paste0(errors, "<br> Please check the spelling before proceeding. The special characters will be removed, and the species names may not match your chosen vocabulary (life history information)"),
                   type = "warning", html = TRUE)
      }
      
      
      
      threedpoints <- threedpoints %>%
        dplyr::mutate(species = str_remove_all(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::mutate(genus = str_remove_all(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        dplyr::mutate(family = str_remove_all(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
        mutate(sample = as.factor(sample)) %>%
        mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
        mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
        mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
        dplyr::filter(!is.na(family)) %>%
        dplyr::mutate(species = as.character(tolower(species))) %>%
        dplyr::mutate(genus = as.character(stringr::str_to_sentence(genus))) %>%
        dplyr::mutate(family = as.character(stringr::str_to_sentence(family))) %>%
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
        dplyr::filter(length_mm<min_length|length_mm>length_max_mm) %>%
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
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, number, period_time, frame_left, em_comment) %>%
      dplyr::mutate(em_comment = tolower(em_comment)) %>%
      dplyr::filter(!em_comment %in% c("sync"))
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
        dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, length_max_mm, reason, em_comment, frame_left, number, length_max_type) %>%
        mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
        dplyr::mutate(percent.of.fb.max = (length_mm/length_max_mm*100)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, length_max_mm, length_max_type, reason, em_comment, frame_left, number)
      
    } else {
      
      length.wrong <- left_join(gen.length.clean(), life.history.min.max(), by = c("family", "genus", "species")) %>%
        dplyr::filter(length_mm<min_length|length_mm>max_length) %>%
        mutate(reason = ifelse(length_mm<min_length, "too small", "too big")) %>%
        dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, length_max_mm, length_max_type, reason, number) %>%
        mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
        dplyr::mutate(percent.of.fb.max = (length_mm/length_max_mm*100)) %>%
        dplyr::left_join(metadata.regions()) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, length_max_mm, length_max_type, reason, number)
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
    renderDataTable(filter(length.wrong()%>% dplyr::select(-c(max_length)) %>%
                             dplyr::rename('15%_fb_maximum_length' = min_length,
                                           # '85%_fb_maximum_length' = max_length,
                                           'fb_maximum_length' = length_max_mm), reason == "too small"), rownames = FALSE,
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
    renderDataTable(filter(length.wrong()%>% dplyr::select(-c(min_length, max_length)) %>%
                             dplyr::rename(#'15%_fb_maximum_length' = min_length,
                               #'85%_fb_maximum_length' = max_length,
                               'fb_maximum_length' = length_max_mm), reason == "too big"), rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species wrong length bigger than 100% - valuebox ----
  output$length.wrong.big.100 <- renderValueBox({
    length.wrong.big <- length.wrong() %>%
      dplyr::filter(reason %in% c("too big")) %>%
      dplyr::filter(length_max_mm < length_mm) %>%
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
      write.csv(dplyr::filter(length.wrong()%>% dplyr::select(-c(min_length, max_length)), length_max_mm < length_mm), file, row.names = FALSE)
    }
  )
  
  ## ► Species wrong length bigger than 100% - onclick ----
  onclick('click.length.wrong.big.100', showModal(modalDialog(
    title = "Length measurements bigger than 100% of the fishbase maximum", size = "l", easyClose = TRUE,
    downloadButton("download.length.wrong.big.100", "Download as csv"),
    renderDataTable(filter(length.wrong() %>% dplyr::select(-c(min_length, max_length)) %>%
                             dplyr::rename(#'15%_fb_maximum_length' = min_length,
                               #'85%_fb_maximum_length' = max_length,
                               'fb_maximum_length' = length_max_mm), fb_maximum_length < length_mm), rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ► Out of range - dataframe ----
  length.out.of.range <- reactive({
    req(input$range.limit)
    
    range.limit <- (input$range.limit*1000)
    
    print("OG length out of range")
    length.out.of.range <- length3dpoints() %>%
      dplyr::filter(range > range.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, frame_left, frame_right, em_comment, rms, precision, code) #%>% dplyr::glimpse()
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
  
  ## ► Over RMS (both length and 3D point) - dataframe ----
  length.wrong.rms <- reactive({
    rms.limit <- (input$rms.limit)
    
    length.wrong.rms <- length3dpoints() %>%
      dplyr::filter(rms > rms.limit) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)
  })
  
  ## ► Over RMS (lengths) - valuebox ----
  output$length.wrong.rms <- renderValueBox({
    length.wrong.rms <- length.wrong.rms() %>%
      dplyr::mutate(count = 1)
    
    lengths <- length.wrong.rms %>%
      dplyr::filter(length_mm > 0)
    
    # If any RMS errors are lengths then make the box red
    if (dim(lengths)[1] > 0) {
      total <- sum(lengths$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3,
             total,
             "Length measurement(s) over RMS limit",
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over RMS (lengths) - onclick ----
  onclick('click.length.wrong.rms', showModal(modalDialog(
    title = "Length measurement(s) over RMS limit", size = "l", easyClose = TRUE,
    renderDataTable(lengths <- length.wrong.rms() %>%
                      dplyr::filter(length_mm > 0),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ► Over RMS (3D points) - valuebox ----
  output$length.wrong.rms.3dpoint <- renderValueBox({
    length.wrong.rms <- length.wrong.rms() %>%
      dplyr::mutate(count = 1)
    
    points3d <- length.wrong.rms %>%
      dplyr::filter(is.na(length_mm))
    
    # If any RMS errors are 3D points then make the box yellow
    if (dim(points3d)[1] > 0) {
      total <- sum(points3d$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3,
             total,
             "3D point(s) over RMS limit",
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over RMS (3D points) - onclick ----
  onclick('click.length.wrong.rms.3dpoint', showModal(modalDialog(
    title = "3D measurement(s) over RMS limit", size = "l", easyClose = TRUE,
    renderDataTable(length.wrong.rms() %>%
                      dplyr::filter(is.na(length_mm)),  rownames = FALSE,
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
             "Length measurements over precision limit",
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
      dplyr::distinct(scientific, length_max_mm, min_length, max_length)
    
    fishbase.max <- sum(sizes$length_max_mm)
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
      distinct(scientific, length_max_mm, min_length, max_length)
    
    fishbase.max <- sum(sizes$length_max_mm)
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
    
    message("view length 3D points from Transect data")
    
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
      filter(!family %in% c("Unknown")) %>%
      dplyr::mutate(family = as.character(family),
                    genus = as.character(genus),
                    species = as.character(species)) #%>% dplyr::glimpse()
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
      dplyr::filter(length_mm < min_length | length_mm > length_max_mm) %>%
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
      dplyr::select(campaignid, sample, family, genus, species, length_mm, min_length, max_length, length_max_mm, reason, frame_left, rms, precision) %>% # , code
      mutate(difference = ifelse(reason%in%c("too small"), (min_length-length_mm), (length_mm-max_length))) %>%
      dplyr::mutate(percent.of.fb.max = (length_mm/length_max_mm*100)) %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, min_length, max_length, length_max_mm, reason, frame_left, rms, precision)
    
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
                                                              'fb_maximum_length' = length_max_mm),
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
                                                                     'fb_maximum_length' = length_max_mm), reason == "too big"), rownames = FALSE,
                            options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Species wrong length 100% too big - valuebox ----
  output$length.wrong.big.100.t <- renderValueBox({
    length.wrong.big <- length.wrong.t() %>%
      dplyr::filter(reason %in% c("too big")) %>%
      dplyr::filter(length_max_mm < length_mm) %>%
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
      write.csv(dplyr::filter(length.wrong.t(), length_max_mm < length_mm), file, row.names = FALSE)
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
                                                                     'fb_maximum_length' = length_max_mm), fb_maximum_length < length_mm), rownames = FALSE,
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
    
    message("out of transect")
    
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
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, length_mm, frame_left, frame_right, midx, midy, x, y, em_comment, rms, precision, code) #%>%
      #glimpse()
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
  
  ## ► Over RMS (lengths) - valuebox ----
  output$length.wrong.rms.t <- renderValueBox({
    length.wrong.rms.t <- length.wrong.rms.t() %>%
      dplyr::mutate(count = 1)
    
    lengths <- length.wrong.rms.t %>%
      dplyr::filter(length_mm > 0)
    
    # If any RMS errors are lengths then make the box red
    if (dim(lengths)[1] > 0) {
      total <- sum(lengths$count)
      col = "red"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3,
             total,
             "Length measurement(s) over RMS limit",
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over RMS (lengths) - onclick ----
  onclick('click.length.wrong.rms.t', showModal(modalDialog(
    title = "Length measurement(s) over RMS limit", size = "l", easyClose = TRUE,
    renderDataTable(lengths <- length.wrong.rms.t() %>%
                      dplyr::filter(length_mm > 0),  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
  ## ► Over RMS (3D points) - valuebox ----
  output$length.wrong.rms.3dpoint.t <- renderValueBox({
    length.wrong.rms <- length.wrong.rms.t() %>%
      dplyr::mutate(count = 1)
    
    points3d <- length.wrong.rms %>%
      dplyr::filter(is.na(length_mm))
    
    # If any RMS errors are 3D points then make the box yellow
    if (dim(points3d)[1] > 0) {
      total <- sum(points3d$count)
      col = "yellow"
    }
    else{
      total = 0
      col = "green"
    }
    
    valueBox(width = 3,
             total,
             "3D point(s) over RMS limit",
             icon = icon("greater-than"), color = col
    )
  })
  
  ## ► Over RMS (3D points)- onclick ----
  onclick('click.length.wrong.rms.3dpoint.t', showModal(modalDialog(
    title = "3D measurement(s) over RMS limit", size = "l", easyClose = TRUE,
    renderDataTable(length.wrong.rms.t() %>%
                      dplyr::filter(is.na(length_mm)),  rownames = FALSE,
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
      distinct(scientific, length_max_mm, min_length, max_length)
    
    fishbase.max <- sum(sizes$length_max_mm)
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
      distinct(scientific, length_max_mm, min_length, max_length)
    
    fishbase.max <- sum(sizes$length_max_mm)
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
      dplyr::group_by(family, length_max_type) %>%
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
      dplyr::mutate(rank = ifelse(length_max_type == "FL", 1, ifelse(length_max_type == "TL", 2, 3))) %>%
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
      dplyr::mutate(all = ifelse(is.na(all)&length_max_type%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length_max_type%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
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
      dplyr::group_by(family, length_max_type) %>%
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
      dplyr::mutate(rank = ifelse(length_max_type == "FL", 1, ifelse(length_max_type == "TL", 2, 3))) %>%
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
      dplyr::mutate(all = ifelse(is.na(all)&length_max_type%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length_max_type%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
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
      dplyr::filter(length_mm<min_length|length_mm>length_max_mm) %>%
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
      dplyr::group_by(family, length_max_type) %>%
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
      dplyr::mutate(rank = ifelse(length_max_type == "FL", 1, ifelse(length_max_type == "TL", 2, 3))) %>%
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
      dplyr::mutate(all = ifelse(is.na(all)&length_max_type%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length_max_type%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
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
      dplyr::group_by(family, length_max_type) %>%
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
      dplyr::mutate(rank = ifelse(length_max_type == "FL", 1, ifelse(length_max_type == "TL", 2, 3))) %>%
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
      dplyr::mutate(all = ifelse(is.na(all)&length_max_type%in%c("TL", "FL", "SL"), 0, all)) %>% # Temporary fix, remove later
      dplyr::mutate(bll = ifelse(is.na(bll)&length_max_type%in%c("TL", "FL", "SL"), 1, bll)) %>% # Temporary fix, remove later
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
      dplyr::filter(length_mm<min_length|length_mm>length_max_mm) %>%
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
      
      message("length")
      
      length <- length3dpoints.clean()# %>%
       # glimpse()
      
      length_samples <- length %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
      
      message("maxn")
      
      maxn <- maxn.clean() #%>%
        #glimpse()
      
    } else {
      
      length <- gen.length.clean()
      maxn <- count.clean()
      
      length_samples <- length %>%
        dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period"))) %>%
        distinct()
      
    }
    
    # print("number of rows in length")
    # print(nrow(length))
    
    # print(nrow(maxn))
    
    # summarise length and then compare to maxn
    length.vs.maxn <- length %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>%
      dplyr::summarise(length_maxn = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(length_samples) %>% # to bring back in opcode, period for missing 3D points with no MaxN
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
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), family, genus, species, maxn, length_maxn, difference, percent_difference) #%>%
      #glimpse()
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
  
  
  length.missing.or.greater.maxn <- reactive({
    
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
    
    # print("number of rows in length")
    # print(nrow(length))
    
    length <- length %>%
      dplyr::filter(length_mm > 0)
    
    # print("number of rows in length")
    # print(nrow(length))
    
    # summarise length and then compare to maxn
    length.vs.maxn <- length %>%
      dplyr::group_by(campaignid, sample, family, genus, species) %>%
      dplyr::summarise(length_maxn = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(maxn) %>% # Changed to full join 22/08/2023
      replace_na(list(maxn = 0, length_maxn = 0)) %>%
      dplyr::mutate(maxn_greater_length = if_else(maxn > length_maxn, maxn-length_maxn, 0)) %>%
      dplyr::mutate(maxn_smaller_length = if_else(maxn < length_maxn, length_maxn-maxn, 0)) %>%
      dplyr::semi_join(length.sample) %>% # only keep ones where length was possible
      dplyr::left_join(metadata.regions()) %>%
      dplyr::select(campaignid, sample, dplyr::any_of(c("opcode", "period")), family, genus, species, maxn, length_maxn, maxn_greater_length, maxn_smaller_length)
    
  })
  
  ## ► Valuebox - Lengths missing from MaxN score ----
  output$length.missing.maxn <- renderValueBox({
    
    print("new length missing maxn error score")
    
    dat <- length.missing.or.greater.maxn()
    
    additional.maxns <- sum(dat$maxn_greater_length)
    total.maxns <- sum(dat$maxn)
    
    percentage <- (additional.maxns/total.maxns) * 100
    
    valueBox(width = 3,
             round(percentage, 2),
             "% of lengths missing from MaxN",
             icon = icon("percent"), color = "blue"
    ) # TODO change the colours
  })
  
  onclick('click.length.missing.maxn', showModal(modalDialog(
    title = "Rows where lengths are missing (This does not include 3D points)", size = "l", easyClose = TRUE,
    renderDataTable(length.missing.or.greater.maxn() %>% filter(maxn_greater_length > 0) %>% dplyr::select(!c(sample, maxn_smaller_length))
                    ,  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  ## ► Valuebox - Lengths missing from MaxN score ----
  output$length.more.maxn <- renderValueBox({
    
    dat <- length.missing.or.greater.maxn()
    
    additional.lengths <- sum(dat$maxn_smaller_length)
    total.maxns <- sum(dat$maxn)
    
    percentage <- (additional.lengths/total.maxns) * 100
    
    valueBox(width = 3,
             round(percentage, 2),
             "% of lengths more than expected from MaxN",
             icon = icon("percent"), color = "blue"
    ) # TODO change the colours
  })
  
  
  onclick('length.more.maxn', showModal(modalDialog(
    title = "Rows where there are more lengths than maxn (This does not include 3D points)", size = "l", easyClose = TRUE,
    renderDataTable(length.missing.or.greater.maxn() %>% filter(maxn_smaller_length > 0) %>% dplyr::select(!c(sample, maxn_greater_length))
                    ,  rownames = FALSE,
                    options = list(paging = FALSE, searching = TRUE)))))
  
  
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
          dplyr::filter(grepl("_Dot Point Measurements.txt", name)) #%>% dplyr::glimpse()
        
        points <- data.frame()
        
        if (is.null(files)) return(NULL)
        
        for (i in seq_along(files$datapath)) {
          tmp <- read_tsv(files$datapath[i], col_types = cols(.default = "c"), skip = 3,
                          na = "NA")
          points <- bind_rows(points, tmp) #%>% glimpse()
        }
        
        message("habitat points")
        
        points <- points %>%
          clean_names() #%>% dplyr::glimpse()
        
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
      
      
      # add columns that don't exist
      schema_cols <- c(
        # level_1 = NA_real_,
        level_2 = NA_real_,
        level_3 = NA_real_,
        level_4 = NA_real_,
        level_5 = NA_real_,
        level_6 = NA_real_,
        level_7 = NA_real_,
        level_8 = NA_real_,
        scientific = NA_real_
      )
      
      points <- points %>%
        tibble::add_column(!!!schema_cols[!names(schema_cols) %in% names(.)]) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        dplyr::select(campaignid, sample, opcode, period,
                      image_row, image_col,
                      starts_with("level_"),
                      scientific, any_of(c("qualifiers", "caab_code")),
                      relief_annotated) %>%
        dplyr::mutate(id = 1:nrow(.)) %>%
        dplyr::select(-any_of(c("caab_code"))) %>%
        dplyr::left_join(schema)
      
      
    }
  })
  
  
  ## ► Preview habitat in dashboard ----
  output$table.habitat <- renderDataTable({
    hab.points() %>% dplyr::select(-c(id, sample))
  })
  
  hab.annotations <- reactive({
    hab.points() %>%
      dplyr::filter(relief_annotated %in% c("no", "No", "N", "n")) %>%
      dplyr::select(campaignid, sample, id, starts_with("level"), scientific, any_of(c("caab_code")))
  })
  
  relief.annotations <- reactive({
    hab.points() %>%
      dplyr::filter(relief_annotated %in% c("yes", "Yes", "Y", "y")) %>%
      dplyr::select(campaignid, sample, id, starts_with("level"), scientific, any_of(c("caab_code")))
  })
  
  ## ► Samples without habitat - dataframe ----
  metadata.samples.without.hab <- reactive({
    
    metadata.samples <- metadata() %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")),
                    successful_count, successful_length,
                    dplyr::any_of(c("successful_habitat_forward", "successful_habitat_backward"))) %>%
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
                    dplyr::any_of(c("successful_habitat_forward", "successful_habitat_backward"))) %>%
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
    
    if(input$habdirection %in% "both"){
      
      
      wrong <- habitat.annotations.per.sample() %>%
        dplyr::distinct(campaignid, sample, number.of.annotations) %>%
        dplyr::left_join(metadata()) %>%
        dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 2,
                                           successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ input$number.of.annotations * 1,
                                           successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 1,
                                           successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ 0)) %>%
        dplyr::filter(!number.of.annotations == expected) %>%
        dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
      
    } else {
      
      wrong <- habitat.annotations.per.sample() %>%
        dplyr::distinct(campaignid, sample, number.of.annotations) %>%
        dplyr::left_join(metadata()) %>%
        dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" ~ input$number.of.annotations,
                                           successful_habitat_forward %in% "No" ~ 0)) %>%
        dplyr::filter(!number.of.annotations == expected) %>%
        dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
      
    }
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
    
    if(input$habdirection %in% "both"){
      
      wrong <- relief.annotations.per.sample() %>%
        dplyr::distinct(campaignid, sample, number.of.annotations) %>%
        dplyr::left_join(metadata()) %>%
        dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 2,
                                           successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ input$number.of.annotations * 1,
                                           successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ input$number.of.annotations * 1,
                                           successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ 0)) %>%
        dplyr::filter(!number.of.annotations == expected) %>%
        dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
      
    } else {
      
      wrong <- relief.annotations.per.sample() %>%
        dplyr::distinct(campaignid, sample, number.of.annotations) %>%
        dplyr::left_join(metadata()) %>%
        dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" ~ input$number.of.annotations,
                                           successful_habitat_forward %in% "No" ~ 0)) %>%
        dplyr::filter(!number.of.annotations == expected) %>%
        dplyr::select(campaignid, any_of(c("opcode", "period")), number.of.annotations, expected)
      
    }
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
  
  
  ## ► TIDY Relief - dataframe ----
  tidy.relief <- reactive({
    
    message("view tidy relief")
    relief.annotations() %>%
      dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>% # Remove Open water and Unknown entries from broad
      dplyr::mutate(number = 1) %>%
      dplyr::group_by(campaignid, sample, caab_code) %>%
      dplyr::tally(number, name = "number") %>%
      dplyr::ungroup()  %>%
      dplyr::select(campaignid, sample, everything()) %>% #level_1,
      dplyr::full_join(metadata.regions())%>%
      tidyr::complete(nesting(campaignid, sample), caab_code) %>%
      dplyr::select(campaignid, sample, caab_code, number) %>%
      replace_na(list(number = 0)) %>% # we add in zeros
      dplyr::left_join(metadata.regions()) %>%
      # filter(!is.na(level_2)) %>%
      dplyr::left_join(schema) #%>% dplyr::glimpse()
    
  })
  
  ## ► TIDY HABITAT - dataframe ----
  tidy.habitat <- reactive({
    
    message("test sample 4")
    
    hab.annotations() %>% filter(sample %in% "4") #%>% glimpse()
    
    message("view tidy habitat")
    # message("glimpse 3 times")
    tidy.habitat <- hab.annotations() %>%
      dplyr::mutate(number = 1) %>% # Add a count column to summarise the number of points
      dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
      dplyr::select(campaignid, sample, number, caab_code) %>% # family, genus, species,
      dplyr::group_by(campaignid, sample, caab_code) %>% #family, genus, species,
      dplyr::tally(number, name = "number") %>%
      dplyr::ungroup() %>%
      dplyr::select(campaignid, sample, everything()) %>%
      # dplyr::glimpse() %>%
      dplyr::full_join(metadata.regions())%>%
      tidyr::complete(nesting(campaignid, sample), caab_code) %>%
      dplyr::select(campaignid, sample, caab_code, number) %>%
      replace_na(list(number = 0)) %>% # we add in zeros
      # dplyr::glimpse() %>%
      dplyr::left_join(metadata.regions()) %>%
      dplyr::left_join(schema) %>%
      dplyr::filter(!is.na(caab_code)) #%>% dplyr::glimpse()
    
    message("end of tidy habitat")
    tidy.habitat
    
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
    
    options <- hab.points() %>%
      dplyr::select(starts_with("level_"))
    
    create_dropdown("habitat.levels", names(options), NULL)
  })
  
  # Habitat data for plot ----
  habitat.plot.data <- reactive({
    
    req(input$habitat.levels)
    
    message("habitat level selected")
    print(input$habitat.levels)
    
    dat <- tidy.habitat() %>%
      dplyr::mutate(level_8 = paste(level_2, level_3, level_4, level_5, level_6, level_7, level_8, sep = ": ")) %>%
      dplyr::mutate(level_7 = paste(level_2, level_3, level_4, level_5, level_6, level_7, sep = ": ")) %>%
      dplyr::mutate(level_6 = paste(level_2, level_3, level_4, level_5, level_6, sep = ": ")) %>%
      dplyr::mutate(level_5 = paste(level_2, level_3, level_4, level_5, sep = ": ")) %>%
      dplyr::mutate(level_4 = paste(level_2, level_3, level_4, sep = ": ")) %>%
      dplyr::mutate(level_3 = paste(level_2, level_3, sep = ": ")) %>%
      dplyr::select(campaignid, sample, starts_with(input$habitat.levels), number) %>%
      dplyr::group_by(campaignid, sample, across(starts_with(input$habitat.levels))) %>%
      dplyr::tally(number, name = "number") #%>%
    #glimpse()
    
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
      dplyr::tally(number, name = "number") #%>%
    #glimpse()
    
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
      dplyr::filter(!is.na(depth_m)) #%>%
    #glimpse()
    
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
        
        # print("glimpse downloading data")
        # glimpse(maxn.complete.download())
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        # EVENTMEASURE ----
        if(input$upload %in% "EM"){
          for(i in unique(maxn.complete.download()$campaignid)){
            
            print(i)
            
            dat <- maxn.complete.download() %>%
              dplyr::rename(count = maxn) %>%
              dplyr::filter(campaignid == i) #%>%
            #dplyr::glimpse()
            
            fileName <- paste(i, "_count.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          if(input$length %in% "Yes"){
            
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
              dplyr::filter(campaignid == i) #%>%
            #dplyr::glimpse()
            
            fileName <- paste(i, "_count.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          if(input$length %in% "Yes"){
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
        
        # IF habitat uploaded
        if(input$hab %in% c("YES", "Yes")){
          
          for(i in unique(tidy.habitat()$campaignid)){
            
            message("view habitat download")
            
            dat <- tidy.habitat() %>%
              dplyr::filter(campaignid == i) %>%
              dplyr::select(!sample) #%>% dplyr::glimpse()
            
            # if(input$error.zeros.hab %in% FALSE){
            #
            #   dat <- dat %>%
            #     dplyr::filter(number > 0)
            #
            # }
            
            dat <- dat[,base::colSums(is.na(dat))<nrow(dat)]
            
            fileName <- paste(i, "_benthos.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
          }
          
          
          for(i in unique(tidy.relief()$campaignid)){
            
            message("view relief download")
            
            dat <- tidy.relief() %>%
              dplyr::filter(campaignid == i)%>%
              dplyr::select(!sample) #%>% dplyr::glimpse()
            
            dat <- dat[,base::colSums(is.na(dat))<nrow(dat)]
            
            # if(input$error.zeros.hab %in% FALSE){
            #
            #   dat <- dat %>%
            #     dplyr::filter(number > 0)
            #
            # }
            
            fileName <- paste(i, "_relief.csv", sep = "")
            
            write.csv(dat, file.path(temp_directory, fileName), row.names = FALSE)
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
    
    if (dim(metadata.samples.duplicated())[1] > 0) {
      print("metadata.samples.duplicated")
      metadata.samples.duplicated <- metadata.samples.duplicated() %>%
        mutate(error = "sample.names.are.duplicated") %>%
        mutate(across(everything(), as.character)) #%>% dplyr::glimpse()
    } else {
      metadata.samples.duplicated <- data.frame()
    }
    
    if (dim(metadata.coordinates.duplicated())[1] > 0) {
      print("metadata.coordinates.duplicated")
      metadata.coordinates.duplicated <- metadata.coordinates.duplicated() %>%
        mutate(error = "sample.coordinates.are.duplicated") %>%
        mutate(across(everything(), as.character)) #%>% dplyr::glimpse()
    } else {
      metadata.coordinates.duplicated <- data.frame()
    }
    
    print("metadata.on.land")
    if (dim(metadata.on.land())[1] > 0) {
      metadata.on.land <- metadata.on.land() %>%
        mutate(error = "coordinates.potentially.on.land") %>%
        dplyr::left_join(metadata()) %>%
        dplyr::select(any_of(c("campaignid", "sample", "opcode", "period", "latitude_dd", "longitude_dd"))) %>%
        mutate(across(everything(), as.character)) #%>% dplyr::glimpse()
    } else {
      metadata.on.land <- data.frame()
    }
    
    if(input$upload %in% "EM"){
      print("samples.without.points ")
      
      if (dim(metadata.samples.without.fish())[1] > 0) {
        samples.without.points <- metadata.samples.without.fish() %>%
          mutate(error = "sample.without.points") %>%
          mutate(across(everything(), as.character)) %>% dplyr::glimpse()
      } else {
        samples.without.points <- data.frame()
      }
      
      print("points.samples.without.metadata")
      if (dim(points.samples.without.metadata())[1] > 0) {
        points.samples.without.metadata <- points.samples.without.metadata() %>%
          mutate(error = "sample.in.points.without.metadata") %>%
          mutate(across(everything(), as.character))# %>% dplyr::glimpse()
      } else {
        points.samples.without.metadata <- data.frame()
      }
      
      print("samples.without.periods")
      if (dim(samples.without.periods())[1] > 0) {
        samples.without.periods <- samples.without.periods()%>%
          mutate(error = "sample.without.period") %>%
          mutate(across(everything(), as.character)) #%>% dplyr::glimpse()
      } else {
        samples.without.periods <- data.frame()
      }
      
      print("periods.no.end")
      if (dim(periods.no.end())[1] > 0) {
        periods.no.end <- periods.no.end() %>%
          mutate(error = "period.with.no.end") %>%
          mutate(across(everything(), as.character)) #%>% glimpse()
      } else {
        periods.no.end <- data.frame()
      }
      
      print("periods.wrong")
      if (dim(periods())[1] > 0) {
        periods.wrong <- periods() %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), sample,
                        period, time_start, time_end, has_end)%>%
          
          distinct() %>%
          # distinct(campaignid, sample, period, time_start, time_end, has_end) %>%
          mutate(period_time = round(time_end - time_start)) %>%
          filter(!period_time %in% c(input$error.period.length)) %>%
          mutate(error = "period.wrong.length") %>%
          mutate(across(everything(), as.character)) #%>% glimpse()
      } else {
        periods.wrong <- data.frame()
      }
      
      print("points.outside.periods")
      if (dim(points.outside.periods())[1] > 0) {
        points.outside.periods <- points.outside.periods() %>%
          mutate(error = "point.outside.period") %>%
          mutate(across(everything(), as.character)) #%>% glimpse()
      } else {
        points.outside.periods <- data.frame()
      }
      
      print("points.no.number")
      if (dim(points.no.number())[1] > 0) {
        points.no.number <- points.no.number() %>%
          mutate(error = "point.without.a.number") %>%
          mutate(across(everything(), as.character)) #%>% glimpse()
      } else {
        points.no.number <- data.frame()
      }
      
      print("maxn.species.not.observed")
      if (dim(maxn.species.not.observed())[1] > 0) {
        maxn.species.not.observed <- maxn.species.not.observed() %>%
          mutate(error = "species.not.observed.in.region.before") %>%
          mutate(across(everything(), as.character))  #%>% glimpse()
      } else {
        maxn.species.not.observed <- data.frame()
      }
      
      print("maxn.species.not.in.list")
      if (dim(maxn.species.not.observed.lh())[1] > 0) {
        maxn.species.not.in.lh <- maxn.species.not.observed.lh() %>%
          mutate(error = "species.not.in.life.history.sheet") %>%
          mutate(across(everything(), as.character))  #%>% glimpse()
      } else {
        maxn.species.not.in.lh <- data.frame()
      }
      
      if(input$length %in% "Yes"){
        print("length.samples.without.metadata")
        if (dim(length.samples.without.metadata())[1] > 0) {
          length.samples.without.metadata <- length.samples.without.metadata() %>%
            mutate(error = "sample.in.lengths.without.metadata") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          length.samples.without.metadata <- data.frame()
        }
        
        print("lengths.outside.periods")
        if (dim(lengths.outside.periods())[1] > 0) {
          lengths.outside.periods <- lengths.outside.periods() %>%
            mutate(error = "length.or.3D.point.outside.period") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          lengths.outside.periods <- data.frame()
        }
        
        print("samples.without.length")
        if (dim(metadata.samples.without.length())[1] > 0) {
          samples.without.length <- metadata.samples.without.length() %>%
            mutate(error = "sample.without.length") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          samples.without.length <- data.frame()
        }
        
        print("lengths.no.number")
        if (dim(lengths.no.number())[1] > 0) {
          lengths.no.number <- lengths.no.number() %>%
            mutate(error = "length.without.a.number") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          lengths.no.number <- data.frame()
        }
        
        print("3d.no.number")
        if (dim(threedpoints.no.number())[1] > 0) {
          threedpoints.no.number <- threedpoints.no.number() %>%
            mutate(error = "3D.point.without.a.number") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          threedpoints.no.number <- data.frame()
        }
        
        print("length.species.not.observed")
        if (dim(length.species.not.observed())[1] > 0) {
          length.species.not.observed <- length.species.not.observed() %>%
            mutate(error = "species.not.observed.in.region.before") %>%
            mutate(across(everything(), as.character))  #%>% glimpse()
        } else {
          length.species.not.observed <- data.frame()
        }
        
        print("length.species.not.in.list")
        if (dim(length.species.not.observed.lh())[1] > 0) {
          length.species.not.in.lh <- length.species.not.observed.lh() %>%
            mutate(error = "species.not.in.life.history.sheet") %>%
            mutate(across(everything(), as.character))  #%>% glimpse()
        } else {
          length.species.not.in.lh <- data.frame()
        }
        
        range.limit <- (input$error.report.range*1000)
        
        message("length.out.of.range")
        
        if (dim(length3dpoints())[1] > 0) {
          length.out.of.range <- length3dpoints() %>%
            dplyr::filter(range > range.limit) %>%
            dplyr::select(campaignid, sample, family, genus, species, range, frame_left, frame_right, em_comment) %>%
            mutate(error = "out.of.range") %>%
            mutate(across(everything(), as.character)) #%>% glimpse()
        } else {
          length.out.of.range <- data.frame()
        }
        
        print("length.wrong.small")
        if (dim(length.wrong())[1] > 0) {
          length.wrong.small <- length.wrong() %>%
            dplyr::filter(reason%in%c("too small")) %>%
            dplyr::mutate(error = reason) %>%
            mutate(across(everything(), as.character))#%>% glimpse()
        } else {
          length.wrong.small <- data.frame()
        }
        
        print("length.wrong.big")
        if (dim(length.wrong())[1] > 0) {
          length.wrong.big <- length.wrong() %>%
            dplyr::filter(reason%in%c("too big")) %>%
            dplyr::mutate(error = reason)  %>%
            mutate(across(everything(), as.character))#%>% glimpse()
        } else {
          length.wrong.big <- data.frame()
        }
        
        rms.limit <- (input$error.report.rms)
        
        print("length.wrong.rms")
        length.wrong.rms <- length3dpoints() %>%
          dplyr::filter(rms > rms.limit) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)%>%
          mutate(error = if_else(!is.na(length_mm), "length.measurement.over.rms", "3D.point.measurement.over.rms"))  %>%
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
      }
      
      # all errors
      message("all errors")
      
      
      if(input$periods %in% "yes") {
        
        errors <- bind_rows(lapply(list(
          metadata.samples.duplicated,
          metadata.coordinates.duplicated,
          metadata.on.land,
          
          samples.without.points,
          points.samples.without.metadata,
          
          samples.without.periods,
          periods.no.end,
          periods.wrong,
          points.outside.periods,
          
          points.no.number,
          
          maxn.species.not.observed,
          maxn.species.not.in.lh), function(df) {
            # )
            df %>% mutate(across(everything(), as.character))}))# %>%
         # dplyr::glimpse()
        
        
        # bind_rows()
        
        if(input$length %in% "Yes"){
          
          message("length")
          
          errors <- bind_rows(errors,
                              samples.without.length,
                              length.samples.without.metadata,
                              lengths.outside.periods,
                              lengths.no.number,
                              threedpoints.no.number,
                              
                              length.species.not.observed,
                              length.species.not.in.lh,
                              
                              length.out.of.range,
                              length.wrong.rms,
                              length.wrong.precision,
                              
                              length.wrong.small,
                              length.wrong.big,
                              
                              stereo.maxn.does.not.equal.maxn)
          
        }
        
        all.errors <- errors %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), dplyr::any_of(c("error", "family", "genus", "species", "number", "length_mm", "frame", "frame_left", "range", "min_length", "max_length", "length_max_mm", "em_comment", "rms", "precision", "code"))) %>%
          distinct() %>%
          tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
          arrange(campaignid, opcode, period) %>%
          dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA
        
        
      } else {
        
        errors <- bind_rows(metadata.samples.duplicated,
                            metadata.coordinates.duplicated,
                            metadata.on.land,
                            samples.without.points,
                            points.samples.without.metadata,
                            
                            points.no.number,
                            
                            maxn.species.not.observed,
                            maxn.species.not.in.lh)
        
        if(input$length %in% "Yes"){
          
          errors <- bind_rows(errors,
                              samples.without.length,
                              length.samples.without.metadata,
                              
                              lengths.no.number,
                              threedpoints.no.number,
                              
                              length.species.not.observed,
                              length.species.not.in.lh,
                              
                              length.out.of.range,
                              length.wrong.rms,
                              length.wrong.precision,
                              
                              length.wrong.small,
                              length.wrong.big,
                              
                              stereo.maxn.does.not.equal.maxn)
        }
        
        message("ALL ERRORS")
        
        all.errors <- errors %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), dplyr::any_of(c("error",
                                                                                          "latitude_dd",
                                                                                          "longitude_dd",
                                                                                          "family",
                                                                                          "genus",
                                                                                          "species",
                                                                                          "number",
                                                                                          "length_mm",
                                                                                          "frame",
                                                                                          "frame_left",
                                                                                          "range",
                                                                                          "min_length",
                                                                                          "max_length",
                                                                                          "length_max_mm",
                                                                                          "em_comment",
                                                                                          "rms",
                                                                                          "precision",
                                                                                          "code"))) %>%
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
      
      print("count.samples.without.metadata")
      points.samples.without.metadata <- points.samples.without.metadata() %>%
        mutate(error = "sample.in.count.without.metadata") %>%
        mutate(across(everything(), as.character))#%>% glimpse()
      
      print("count.species.not.observed")
      maxn.species.not.observed <- maxn.species.not.observed() %>%
        mutate(error = "species.not.observed.in.region.before")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()
      
      print("maxn.species.not.in.list")
      maxn.species.not.in.lh <- maxn.species.not.observed.lh() %>%
        mutate(error = "species.not.in.life.history.sheet")  %>%
        mutate(across(everything(), as.character))#%>% glimpse()
      
      # all errors
      print("all errors")
      all.errors <- bind_rows(metadata.samples.duplicated,
                              metadata.coordinates.duplicated,
                              metadata.on.land,
                              samples.without.points,
                              points.samples.without.metadata,
                              
                              maxn.species.not.observed,
                              maxn.species.not.in.lh) %>%
        dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), dplyr::any_of(c("latitude_dd",
                                                                                        "longitude_dd",
                                                                                        "error",
                                                                                        "family",
                                                                                        "genus",
                                                                                        "species",
                                                                                        "length_mm",
                                                                                        "min_length",
                                                                                        "max_length",
                                                                                        "length_max_mm"))) %>%
        distinct() %>%
        tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
        arrange(campaignid, opcode, period) %>%
        dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA
      
      # Length errors - only include if fish were measured
      if(input$length %in% "Yes"){
        print("samples.without.length")
        samples.without.length <- metadata.samples.without.length() %>%
          mutate(error = "sample.without.length") %>%
          mutate(across(everything(), as.character))#%>% glimpse()
        
        print("length.samples.without.metadata")
        length.samples.without.metadata <- length.samples.without.metadata() %>%
          mutate(error = "sample.in.lengths.without.metadata") %>%
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
        all.errors <- bind_rows(metadata.samples.duplicated,
                                metadata.coordinates.duplicated,
                                metadata.on.land,
                                samples.without.points,
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
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), dplyr::any_of(c("latitude_dd", "longitude_dd", "error", "family", "genus", "species", "length_mm", "min_length", "max_length", "length_max_mm"))) %>%
          distinct() %>%
          tibble::add_column(!!!sample.cols[!names(sample.cols) %in% names(.)]) %>%
          arrange(campaignid, opcode, period) %>%
          dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # only select columns that are no all NA
        
      }
      
    }
    
    message("the end of errors")
    
    return(all.errors)
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
    
    if (dim(metadata.samples.duplicated.t())[1] > 0) {
      print("metadata.samples.duplicated.t")
      metadata.samples.duplicated.t <- metadata.samples.duplicated.t() %>%
        mutate(error = "sample.names.are.duplicated") %>%
        mutate(across(everything(), as.character)) #%>%
        #glimpse()
    } else {
      metadata.samples.duplicated.t <- data.frame()
    }
    
    if (dim(metadata.coordinates.duplicated.t())[1] > 0) {
      print("metadata.coordinates.duplicated.t")
      metadata.coordinates.duplicated.t <- metadata.coordinates.duplicated.t() %>%
        mutate(error = "sample.coordinates.are.duplicated") %>%
        mutate(across(everything(), as.character)) #%>%
        #glimpse()
    } else {
      metadata.coordinates.duplicated.t <- data.frame()
    }
    
    print("metadata.on.land")
    if (dim(metadata.on.land.t())[1] > 0) {
      metadata.on.land.t <- metadata.on.land.t() %>%
        mutate(error = "coordinates.potentially.on.land") %>%
        dplyr::left_join(metadata()) %>%
        dplyr::select(any_of(c("campaignid", "sample", "opcode", "period", "latitude_dd", "longitude_dd", "error"))) %>%
        mutate(across(everything(), as.character))# %>%
        #glimpse()
    } else {
      metadata.on.land.t <- data.frame()
    }
    
    
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
    
    message(paste("transect width in metres: ", input$error.report.transect.t))
    
    length.out.of.transect.t <- length3dpoints.t() %>%
      dplyr::mutate(x = as.numeric(x),
                    y = as.numeric(y),
                    z = as.numeric(z),
                    midx = as.numeric(midx),
                    midy = as.numeric(midy),
                    midz = as.numeric(midz)
      ) %>%
      
      dplyr::filter(c(midx > transect.limit | midx < -transect.limit | midy > transect.limit | midy < -transect.limit | x > transect.limit | x < -transect.limit | y > transect.limit | y < -transect.limit)) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, range, length_mm, frame_left, frame_right, midx, midy, x, y, em_comment) %>%
      dplyr::mutate(error = "out.of.transect")%>%
      mutate(across(everything(), as.character)) #%>%
      #glimpse()
    
    message(paste("number of rows in data: ", nrow(length.out.of.transect.t)))
    
    rms.limit <- (input$error.report.rms.t)
    
    length.wrong.rms <- length3dpoints.t() %>%
      dplyr::filter(rms > rms.limit) %>%
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period")), family, genus, species, length_mm, range, frame_left, frame_right, em_comment, rms, precision, code)%>%
      mutate(error = if_else(!is.na(length_mm), "length.measurement.over.rms", "3D.point.measurement.over.rms"))  %>%
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
    all.errors <- bind_rows(metadata.samples.duplicated.t,
                            metadata.coordinates.duplicated.t,
                            metadata.on.land.t,
                            
                            metadata.samples.without.lengths.t,
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
      
      dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "latitude_dd", "longitude_dd")), error, family, genus, species, number, length_mm, frame, frame_left, range, min_length, max_length, length_max_mm, em_comment, rms, precision) %>%
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
  count_data <- reactive({
    # When folder chosen ----
    if(!is.null(input$folderdir)) {
      
      # Get all _Count.csv files in the folder
      files <- input$folderdir%>%
        dplyr::filter(grepl("_Count.csv", name))
      
      count_data <- data.frame()
      
      if (is.null(files)) return(NULL)
      
      for (i in seq_along(files$datapath)) {
        tmp <- read_csv(files$datapath[i], col_types = cols(.default = "c"))  %>%
          dplyr::mutate(campaignid = files$name[i])
        
        count_data <- bind_rows(count_data, tmp)
        
        if("CampaignID" %in% colnames(count_data))
        {
          count_data <- count_data %>%
            dplyr::select(-c(CampaignID))
        }
      }
      
      count_data <- count_data %>%
        clean_names() %>%
        dplyr::mutate(campaignid = str_replace_all(.$campaignid, c("_Count.csv" = "")))
    }
    
    if(!"sample" %in% names(count_data)){
      
      # If point method and samples are opcodes
      if(input$method == "point" & input$sample == "opcode") {
        
        count_data <- count_data %>%
          dplyr::mutate(sample = opcode)
      }
      
      # If point method and samples are periods
      if(input$method == "point" & input$sample == "period") {
        
        count_data <- count_data %>%
          dplyr::mutate(sample = period)
      }
      
      # If transect method and sample = "opcode" + "period"
      if(input$method == "transect" & input$sample.t == "opcodeperiod") {
        
        count_data <- count_data %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "_"))
        
      }
      
      # If transect method and sample = "period"
      if(input$method == "transect" & input$sample.t == "period") {
        
        count_data <- count_data %>%
          dplyr::mutate(sample = period)
      }
    }
    
    # 
    # 
    # family_accents <- count_data %>%
    #   distinct(family) %>%
    #   dplyr::mutate(fail = str_detect(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
    #   dplyr::filter(!fail %in% FALSE)# %>%
    # #glimpse()
    # 
    # if(nrow(family_accents > 0)){
    #   errors <- paste0("<li>In the <b>Family</b> column.", "</li>", "<br>")
    # } else {
    #   errors = ""
    # }
    # 
    # genus_accents <- count_data %>%
    #   distinct(genus) %>%
    #   dplyr::mutate(fail = str_detect(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
    #   dplyr::filter(!fail %in% FALSE) #%>%
    # #glimpse()
    # 
    # if(nrow(genus_accents > 0)){
    #   errors <- paste0(errors, "<li>In the <b>Genus</b> column.", "</li>", "<br>")
    # }
    # 
    # species_accents <- count_data %>%
    #   distinct(species) %>%
    #   dplyr::mutate(fail = str_detect(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
    #   dplyr::filter(!fail %in% FALSE) #%>%
    # #glimpse()
    # 
    # if(nrow(species_accents > 0)){
    #   errors <- paste0(errors, "<li>In the <b>Species</b> column.", "</li>", "<br>")
    # }
    # 
    # accents <- bind_rows(family_accents, genus_accents, species_accents)
    # 
    # if(nrow(accents > 0)){
    #   shinyalert("Count Data Contains Special Characters:",
    #              paste0(errors, "<br> Please check the spelling before proceeding. The special characters will be removed, and the species names may not match your chosen vocabulary (life history information)"),
    #              type = "warning", html = TRUE)
    # }
    # 
    # message("view count data")
    # 
    count_data <- count_data %>%
      # dplyr::mutate(species = str_remove_all(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      # dplyr::mutate(genus = str_remove_all(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      # dplyr::mutate(family = str_remove_all(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(sample = as.factor(sample)) %>%
      dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "spp."), "spp", as.character(species))) %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::mutate(species = as.character(tolower(species))) %>%
      dplyr::mutate(genus = as.character(stringr::str_to_sentence(genus))) %>%
      dplyr::mutate(family = as.character(stringr::str_to_sentence(family))) %>%
      dplyr::left_join(life.history()) #%>%
      #dplyr::glimpse()
    
    return(count_data)
    
  })
  
  output$table.gencount <- renderDataTable({
    
    count_data()
    
    
  })
  
  ## ► Create Count (Raw) ----
  count.raw <- reactive({
    #TODO add code column with lifehistory sheet
    maxn <- count_data() %>%
      dplyr::mutate(count = as.numeric(count)) %>%
      replace_na(list(family = "Unknown", genus = "Unknown", species = "spp")) #%>% # remove any NAs in taxa name
    
    if(input$stage %in% "No"){
      
      maxn <- maxn %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
        dplyr::summarise(maxn = sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup()
      
    } else {
      
      maxn <- maxn %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
        dplyr::summarise(maxn = sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup()
      
    }
    
    message("view gen maxn")
    
    maxn <- maxn %>%
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
      filter(!family %in% c("Unknown"))#%>% 
      #glimpse()
    
    return(maxn)
    
  })
  
  count.clean <- reactive({
    
    if(input$stage %in% "No"){
      
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
      
    } else {
      
      count.clean <- dplyr::full_join(count.raw(), metadata.regions()) %>%
        dplyr::left_join(., synonyms()) %>%
        dplyr::mutate(genus = ifelse(!genus_correct %in% c(NA), genus_correct, genus)) %>%
        dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
        dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
        dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
        dplyr::slice(which.max(maxn)) %>%
        dplyr::ungroup() %>%
        as_tibble()
      
    }
    
    return(count.clean)
    
  })
  
  ## ►  Create MaxN (Complete) -----
  count.complete <- reactive({
    
    print("count.complete")
    
    if(input$stage %in% "No"){
      
      count.complete <- count.clean() %>%
        dplyr::full_join(metadata.regions()) %>%
        dplyr::select(c(campaignid, sample, family, genus, species, maxn, code)) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code)) %>%
        replace_na(list(maxn = 0)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        dplyr::ungroup() #%>% glimpse()
      
    } else {
      
      count.complete <- count.clean() %>%
        dplyr::full_join(metadata.regions()) %>%
        dplyr::select(c(campaignid, sample, family, genus, species, maxn, code, stage)) %>%
        tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code, stage)) %>%
        replace_na(list(maxn = 0)) %>%
        dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
        dplyr::summarise(maxn = sum(maxn)) %>%
        dplyr::ungroup() #%>% glimpse()
      
    }
    
    return(count.complete)
    
  })
  
  ## ► Create filtered Count download -----
  count.complete.download <- reactive({
    
    if(!input$upload %in% "EM"){
      
      count <- full_join(count.raw(), metadata.regions()) # can't use clean as have already changed synonyms
      
      if (input$error.synonyms == TRUE) {
        
        if(input$stage %in% "No"){
          
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
          
          message("count complete with stage")
          
          count.complete <- dplyr::left_join(count, synonyms()) %>% #, by = c("family", "genus", "species")
            dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
            dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
            dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
            dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
            dplyr::slice(which.max(maxn)) %>%
            dplyr::ungroup() %>%
            dplyr::full_join(metadata.regions()) %>%
            dplyr::select(c(campaignid, sample, family, genus, species, maxn, code, stage)) %>%
            tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code, stage)) %>%
            replace_na(list(maxn = 0)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
            dplyr::summarise(maxn = sum(maxn)) %>%
            ungroup() %>%
            dplyr::left_join(metadata.regions()) %>%
            dplyr::mutate(scientific = paste(genus, species, sep = " "))# %>%
          #glimpse()
          
        }
        
        
      } else {
        
        if(input$stage %in% "No"){
          
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
          
        } else {
          
          count.complete <- count %>%
            dplyr::select(c(campaignid, sample, family, genus, species, maxn, code, stage)) %>%
            dplyr::full_join(metadata.regions()) %>%
            tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species, code, stage)) %>%
            replace_na(list(maxn = 0)) %>%
            dplyr::group_by(campaignid, sample, family, genus, species, code, stage) %>%
            dplyr::summarise(maxn = sum(maxn)) %>%
            ungroup() %>%
            dplyr::left_join(metadata.regions()) %>%
            dplyr::mutate(scientific = paste(genus, species, sep = " "))
          
        }
      }
      
      # message(
      #   "last coint complete"
      # )
      
      count.complete <- count.complete #%>% glimpse()
      
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
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "stage")), everything()) %>%
          dplyr::select(!sample)
        
      } else {
        
        count.area <- count.area %>%
          dplyr::filter(!maxn %in% 0) %>%
          dplyr::select(campaignid, dplyr::any_of(c("opcode", "period", "stage")), family, genus, species, code, maxn)} # remove metadata columns
      
      message("count area")
      count.area <- count.area %>%
        dplyr::filter(!family %in% c("", NA, NULL))# %>%
      # glimpse()
      
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
    
    if(!"sample" %in% names(gen.length)){
      
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
      
    }
    
    family_accents <- gen.length %>%
      distinct(family) %>%
      dplyr::mutate(fail = str_detect(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::filter(!fail %in% FALSE) #%>%
    #glimpse()
    
    if(nrow(family_accents > 0)){
      errors <- paste0("<li>In the <b>Family</b> column.", "</li>", "<br>")
    } else {
      errors = ""
    }
    
    genus_accents <- gen.length %>%
      distinct(genus) %>%
      dplyr::mutate(fail = str_detect(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::filter(!fail %in% FALSE) #%>%
    #glimpse()
    
    if(nrow(genus_accents > 0)){
      errors <- paste0(errors, "<li>In the <b>Genus</b> column.", "</li>", "<br>")
    }
    
    species_accents <- gen.length %>%
      distinct(species) %>%
      dplyr::mutate(fail = str_detect(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::filter(!fail %in% FALSE) #%>%
    #glimpse()
    
    if(nrow(species_accents > 0)){
      errors <- paste0(errors, "<li>In the <b>Species</b> column.", "</li>", "<br>")
    }
    
    accents <- bind_rows(family_accents, genus_accents, species_accents)
    
    if(nrow(accents > 0)){
      shinyalert("Length Data Contains Special Characters:",
                 paste0(errors, "<br> Please check the spelling before proceeding. The special characters will be removed, and the species names may not match your chosen vocabulary (life history information)"),
                 type = "warning", html = TRUE)
    }
    
    
    gen.length <- gen.length %>%
      dplyr::mutate(species = str_remove_all(species, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(genus = str_remove_all(genus, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      dplyr::mutate(family = str_remove_all(family, "[^[:alnum:]]|á|é|ó|ū|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ý|Ý|à|è|ì|ò|ù|À|È|Ì|Ò|Ù|â|ê|î|ô|û|Â|Ê|Î|Ô|Û|ã|õ|Ã|Õ|ñ|Ñ|ä|ë|ï|ö|ü|Ä|Ë|Ï|Ö|Ü|ÿ|ç|Ç")) %>%
      mutate(sample = as.factor(sample)) %>%
      mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
      mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
      mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "spp."), "spp", as.character(species))) %>%
      dplyr::filter(!is.na(family)) %>%
      dplyr::mutate(species = as.character(tolower(species))) %>%
      dplyr::mutate(genus = as.character(stringr::str_to_sentence(genus))) %>%
      dplyr::mutate(family = as.character(stringr::str_to_sentence(family))) %>%
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
      if(input$length %in% "Yes"){
        print("preview length data for downloading")
        length <- gen.length() %>%
          dplyr::select(campaignid, sample, family, genus, species, length_mm, number, code) #%>% glimpse() # can't use clean as have already changed synonyms
        
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
          dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
        #glimpse()
        
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
          dplyr::filter(length_mm<min_length|length_mm>length_max_mm) %>%
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
          dplyr::mutate(scientific = paste(genus, species, sep = " ")) #%>%
        #glimpse()
        
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
        length.big <- length.big #%>%  glimpse()
      }
    }
  })
}
