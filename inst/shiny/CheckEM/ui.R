function(){
  tagList(
    useShinyjs(),
    dashboardPage(
      dbHeader,
      dashboardSidebar(
        sidebarMenu(id = "tabs",
                    menuItem("Upload data", tabName = "upload", icon = icon("upload")),
                    
                    # BRUV tabs
                    shiny::conditionalPanel("input.method == 'point'",
                                            # shiny::conditionalPanel(condition = "input.transect == 'Non-transect e.g. BRUV'", 
                                            sidebarMenu(
                                              menuItem("Check metadata & periods", tabName = "checkmetadata", icon = icon("check")),
                                              menuItem("Create & check MaxN", tabName = "createmaxn", icon = icon("check")),
                                              shiny::conditionalPanel("input.length == 'Yes'",
                                                                      sidebarMenu(menuItem("Check length & 3D points", tabName = "createlength", icon = icon("check"))),
                                                                      sidebarMenu(menuItem("Compare MaxN & length", tabName = "maxnlength", icon = icon("equals"))),
                                                                      sidebarMenu(menuItem("Create & check mass", tabName = "createmass", icon = icon("check")))),
                                              shiny::conditionalPanel("input.hab == 'Yes'",
                                                                      sidebarMenu(menuItem("Check habitat", tabName = "checkhab", icon = icon("check")))),
                                              
                                              menuItem("Download data and QC score", tabName = "downloads", icon = icon("download")))
                    ),
                    
                    # Transect tabs
                    shiny::conditionalPanel("input.method == 'transect'",
                                            # shiny::conditionalPanel(condition = "input.transect == 'Transect based e.g. DOV'", 
                                            sidebarMenu(
                                              menuItem("Check metadata & periods", tabName = "checkmetadatat", icon = icon("check")),
                                              menuItem("Check length & 3D points", tabName = "createlengtht", icon = icon("check")),
                                              
                                              shiny::conditionalPanel("input.hab == 'Yes'",
                                                                      sidebarMenu(menuItem("Check habitat", tabName = "checkhab", icon = icon("check")))),
                                              menuItem("Create & check mass", tabName = "createmasst", icon = icon("check")),
                                              menuItem("Download data", tabName = "downloadst", icon = icon("download")))
                    ),
                    menuItem("Schema downloads", tabName = "schema", icon = icon("download", lib="font-awesome")),
                    menuItem("User guide", tabName = "guide", icon = icon("info", lib="font-awesome")),
                    menuItem("Edit maximum lengths", tabName = "update", icon = icon("edit", lib="font-awesome")),
                    menuItem("Feedback", tabName = "feedback", icon = icon("comment", lib="font-awesome")),
                    menuItem("Change log", tabName = "change", icon = icon("edit", lib="font-awesome")),
                    menuItem("Acknowledgements", tabName = "acknowledgements", icon = icon("hands-helping", lib="font-awesome"))
        )
      ),
      
      dashboardBody(
        
        HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
        tags$script(HTML(
          '
    $(document).on("shiny:connected", function(){
      var newUser = Cookies.get("new_user");
      if(newUser === "false") return;
      Shiny.setInputValue("new_user", true);
      Cookies.set("new_user", false);
    });
    ')),
    
    
    
    # tags$head(includeHTML("google-analytics.html")),
    HTML("<script type='text/javascript' src='getFolders.js'></script>"),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }
      .bttn-unite { width: 300px;}
      .modal-dialog {
    border-radius: 20px;
}

.modal-body {
    border-radius: 20px;
}

.modal-content {
    border-radius: 20px;
}
      '
    ),

tabItems(
  # Upload data ----
  tabItem(tabName = "upload",
          fluidRow(tags$head(tags$style(type = 'text/css',  '.rpivotTable{ overflow-x: scroll; }')),
                   
                   column(width = 6,
                          
                          box(width = NULL, height = 730, status = "primary", collapsible = TRUE, title = "Aims", solidHeader = TRUE,
                              includeMarkdown("markdown/aims.Rmd")
                          )
                   ),
                   
                   box(width = 6, title = "Format of data", status = "primary", solidHeader = TRUE,
                       h4("Fish annotation data:"),
                       radioButtons("upload", "Choose the type of upload:",
                                    c("EventMeasure" = "EM",
                                      "Generic (only single point methods)" = "G"),
                                    selected = "EM",
                                    inline = TRUE),
                       
                       radioButtons("method", "Choose the type of sampling method used:",
                                    c("Single point e.g. BRUV & BOSS" = "point",
                                      "Transect e.g. DOV & ROV" = "transect"),
                                    selected = "point",
                                    inline = TRUE),
                       
                       shiny::conditionalPanel("input.method == 'point'",
                                               radioButtons("stage", "Did you MaxN by stage?",
                                                            c("Yes",
                                                              "No"),
                                                            selected = "No",
                                                            inline = TRUE)),
                       
                       radioButtons("length", "Did you measure fish?",
                                    c("Yes",
                                      "No"),
                                    selected = "Yes",
                                    inline = TRUE),
                       
                       
                       shiny::conditionalPanel("input.method == 'point'",
                                               radioButtons("sample", "How did you record the sample name in EventMeasure:",
                                                            c("OpCode" = "opcode",
                                                              "Period" = "period"),
                                                            selected = "opcode",
                                                            inline = TRUE),
                                               
                                               shiny::conditionalPanel("input.upload == 'EM'",
                                                                       shiny::conditionalPanel("input.sample == 'opcode'",
                                                                                               radioButtons("periods", "Did you use periods to standardise the sampling duration?",
                                                                                                            c("Yes" = "yes",
                                                                                                              "No" = "no"),
                                                                                                            selected = "yes",
                                                                                                            inline = TRUE)))),
                       
                       shiny::conditionalPanel("input.method == 'transect'",
                                               radioButtons("sample.t", "How did you record the sample name in EventMeasure:",
                                                            c("OpCode and Period" = "opcodeperiod",
                                                              "Period only" = "period"),
                                                            selected = "opcodeperiod",
                                                            inline = TRUE)),
                       # br(),
                       
                       h4("Habitat annotation data:"),
                       radioButtons("hab", "Are you uploading habitat and/or relief?",
                                    c("Yes",
                                      "No"),
                                    selected = "No",
                                    inline = TRUE),
                       
                       shiny::conditionalPanel("input.hab == 'Yes'",
                                               
                                               radioButtons("habdirection", "Which directions were annotated?",
                                                            c("Forwards and backwards" = "both",
                                                              "Forwards only" = "forwards"),
                                                            selected = "both",
                                                            inline = TRUE)
                       ),
                       
                       tags$div(tags$label("Select directory with sample metadata and EM exports", class="btn btn-primary",
                                           tags$input(id = "folderdir", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()")))#,
                       # tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress", tags$div(class="progress-bar"))
                       
                   ),
                   
                   
                   box(width = 6, title = "Life history Information, Marine Spatial Planning & Marine Regions", status = "primary", solidHeader = TRUE,
                       radioButtons("status", "Would you like to keep the uploaded 'status' column or generate one from shapefiles?",
                                    c("Uploaded" = "uploaded",
                                      "Generate from shapefiles" = "generate"),
                                    selected = "uploaded",
                                    inline = FALSE),
                       
                       
                       radioButtons("lifehistory", "Which life history list & regions would you like to check your annotations against?",
                                    c("Australian List (Based on the Codes for Australian Aquatic Biota) & Australian Marine Regions" = "aus",
                                      "Global List (Based on FishBase and the World Register of Marine Species) & FAO Major Fishing Areas" = "global"),
                                    selected = "aus",
                                    inline = FALSE),
                       
                       radioButtons("region", "How would you like to retrieve the marine region for your data?",
                                    c("One marine region per campaignID (uses the average lat/lon of a campaign)" = "campaignid",
                                      "One marine region per sample (uses the lat/lon for each sample, takes much longer to run but recommended if your data crosses multiple marine regions)" = "sample"),
                                    selected = "campaignid",
                                    inline = FALSE)
                       
                   ),
                   # box(width = 6, title = "Upload TransectMeasure exports (txt files)", status = "primary", solidHeader = FALSE,
                   #     
                   #     box(width = 6, height = 110, title = "Forward facing Dot Point Measurements", 
                   #         status = "primary", solidHeader = TRUE,
                   #         
                   #         fileInput("upload.f.dotpoints", NULL, multiple = TRUE,
                   #                   accept = c("image/vnd.txt",".txt"))),
                   #     
                   #     shiny::conditionalPanel("input.habdirection == 'both'",
                   #                             box(width = 6, height = 110, title = "Backward facing Dot Point Measurements", 
                   #                                 status = "primary", solidHeader = TRUE,
                   #                                 fileInput("upload.b.dotpoints", NULL, multiple = TRUE,
                   #                                           accept = c("image/vnd.txt",".txt")))
                   #                             ),
                   #     
                   #     shiny::conditionalPanel("input.habreliefsep == 'yes'",
                   #                             
                   #                             box(width = 6, height = 110, title = "Forwards Relief Dot Point Measurements", 
                   #                                 status = "primary", solidHeader = TRUE,
                   #                                 fileInput("upload.r.f.dotpoints", NULL, multiple = TRUE,
                   #                                           accept = c("image/vnd.txt",".txt"))),
                   #                             
                   #                             shiny::conditionalPanel("input.habdirection == 'both'",
                   #                                                     box(width = 6, height = 110, title = "Backwards Relief Dot Point Measurements", 
                   #                                                         status = "primary", solidHeader = TRUE,
                   #                                                         fileInput("upload.r.b.dotpoints", NULL, multiple = TRUE,
                   #                                                                   accept = c("image/vnd.txt",".txt")))
                   #                             )
                   #     )
                   #     ),
                   
                   tabBox(width = 12, #height = 800,
                          
                          title = tagList(shiny::icon("gear"), "Preview data"),
                          tabPanel("Sample Metadata", #div(style = 'overflow-x: scroll', 
                                   dataTableOutput("table.metadata")
                                   # )
                          ),
                          tabPanel("Points", dataTableOutput("table.points")),
                          tabPanel("Lengths", dataTableOutput("table.length")),
                          tabPanel("3D Points", dataTableOutput("table.3dpoints")),
                          tabPanel("Periods", dataTableOutput("table.periods")),
                          tabPanel("Habitat", dataTableOutput("table.habitat"))
                          
                   )
          )),
  
  # Check metadata - point based data -----
  tabItem(tabName = "checkmetadata",
          fluidRow(valueBoxOutput("metadata.score"),
                   
                   div(id="click.metadata.no.samples",
                       valueBoxOutput("metadata.no.samples")),
                   div(id="click.metadata.samples.without.fish",
                       valueBoxOutput("metadata.samples.without.fish")),
                   
                   shiny::conditionalPanel("input.length == 'Yes'",
                                           
                                           div(id="click.metadata.samples.without.length",
                                               valueBoxOutput("metadata.samples.without.length"))),
                   
                   div(id="click.points.samples.without.metadata",
                       valueBoxOutput("points.samples.without.metadata")),
                   
                   shiny::conditionalPanel("input.length == 'Yes'",
                                           div(id="click.length.samples.without.metadata",
                                               valueBoxOutput("length.samples.without.metadata"))),
                   
                   div(id="click.metadata.samples.duplicated",
                       valueBoxOutput("metadata.samples.duplicated")),
                   
                   div(id="click.metadata.coordinates.duplicated",
                       valueBoxOutput("metadata.coordinates.duplicated")),
                   
                   div(id="click.metadata.on.land",
                       valueBoxOutput("metadata.on.land")),
                   
                   
                   shiny::conditionalPanel("input.periods == 'yes'",
                                           
                                           shiny::conditionalPanel("input.upload == 'EM'",
                                                                   div(id="click.periods.no.end",
                                                                       valueBoxOutput("periods.no.end")),
                                                                   div(id="click.samples.without.periods",
                                                                       valueBoxOutput("samples.without.periods")),
                                                                   
                                                                   
                                                                   div(id="click.points.outside.periods",
                                                                       valueBoxOutput("points.outside.periods")),
                                                                   div(id="click.lengths.outside.periods",
                                                                       valueBoxOutput("lengths.outside.periods")),
                                                                   
                                                                   
                                                                   box(width = 4, title = "Enter your correct period time (mins):", status = "primary", solidHeader = TRUE,
                                                                       numericInput("period.limit", NULL, 60, min = 1, max = 300)),
                                                                   
                                                                   
                                                                   div(id="click.periods.wrong",
                                                                       valueBoxOutput("periods.wrong")))),
                   
                   
                   box(width=12, height = 825, withSpinner(leafletOutput("map.metadata", height = 800))))
  ),
  
  # Check metadata - transect based data ----
  tabItem(tabName = "checkmetadatat",
          fluidRow(
            
            shiny::conditionalPanel("input.upload == 'EM'",
                                    
                                    valueBoxOutput("metadata.score.t"),
                                    div(valueBoxOutput("metadata.no.samples.t")),

                                    
                                    div(id="click.metadata.samples.duplicated.t",
                                        valueBoxOutput("metadata.samples.duplicated.t")),
                                    
                                    div(id="click.metadata.coordinates.duplicated.t",
                                        valueBoxOutput("metadata.coordinates.duplicated.t")),
                                    
                                    div(id="click.metadata.on.land.t",
                                        valueBoxOutput("metadata.on.land.t")),
                                    
                                    
                                    
                                    div(id="click.metadata.samples.without.fish.t",
                                        valueBoxOutput("metadata.samples.without.fish.t")),
                                    div(id="click.metadata.samples.without.3dpoints.t",
                                        valueBoxOutput("metadata.samples.without.3dpoints.t")),
                                    div(id="click.length.samples.without.metadata.t",
                                        valueBoxOutput("length.samples.without.metadata.t")),
                                    
                                    
                                    div(id="click.samples.without.periods.t",
                                        valueBoxOutput("samples.without.periods.t")),
                                    
                                    div(id="click.periods.no.end.t",
                                        valueBoxOutput("periods.no.end.t")),
                                    
                                    div(id="click.points.outside.periods.t",
                                        valueBoxOutput("points.outside.periods.t")),
                                    
                                    div(id="click.lengths.outside.periods.t",
                                        valueBoxOutput("lengths.outside.periods.t")),
                                    
                                    div(id="click.periods.avg.t",
                                        valueBoxOutput("periods.avg.t"))),
            
            box(width = 12, height = 825, withSpinner(leafletOutput("map.metadata.t", height = 800))))
  ),
  
  # Create maxn -----
  tabItem(tabName = "createmaxn",
          fluidRow(
            
            shiny::conditionalPanel("input.upload == 'EM'",
                                    div(id = "click.maxn.total.number.em",
                                        valueBoxOutput(width = 6, "maxn.total.number.em")),
                                    div(id = "click.points.no.number",
                                        valueBoxOutput(width = 6, "points.no.number"))),
            
            shiny::conditionalPanel("input.upload != 'EM'",
                                    div(id="click.maxn.total.number.gen",
                                        valueBoxOutput(width = 12, "maxn.total.number.gen"))),
            
            div(id = "click.maxn.synonym",
                valueBoxOutput("maxn.synonym")),
            div(id = "click.maxn.species.not.observed",
                valueBoxOutput("maxn.species.not.observed")),
            div(id = "click.maxn.species.not.observed.lh",
                valueBoxOutput("maxn.species.not.observed.lh")),
            
            box(width = 12,title = "Species to plot", status = "primary", solidHeader = TRUE, numericInput("species.limit", "Number:", 15, min = 5, max = 20)),
            box(width = 12, height = 500,
                title = "Plot of most abundant species", status = "primary",
                plotOutput("maxn.top.species")),
            
            
            box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                htmlOutput("maxn.species.dropdown",multiple=TRUE)),
            box(width = 12,leafletOutput("maxn.spatial.plot")),
            box(width = 12,title = "Plot of abundance by Status", status = "primary", 
                
                shiny::conditionalPanel("input.lifehistory == 'global'",
                                        h5("Status column is generated from an intersection of the metadata coordinates with the World Database on Protected Areas")),
                
                plotOutput("maxn.status.plot", height = 250)),
            
            box(width = 12,title = "Plot of abundance by Zone", status = "primary", 
                
                shiny::conditionalPanel("input.lifehistory == 'global'",
                                        h5("IUCN Zone column is generated from an intersection of the metadata coordinates with the World Database on Protected Areas")),
                plotOutput("maxn.zone.simple", height = 250)),
            box(width = 12,title = "Plot of abundance by Location", status = "primary", plotOutput("maxn.location.plot", height = 250)),
            box(width=12,title = "Plot of abundance by Site", status = "primary", plotOutput("maxn.site.plot", height = 250))
          )
  )
  ,
  
  # Check length - point based data -----
  tabItem(tabName = "createlength",
          fluidRow(
            shiny::conditionalPanel("input.upload == 'G'",
                                    div(id="click.length.abundance.gen", 
                                        valueBoxOutput(width = 12,"length.abundance.gen"))),
            
            shiny::conditionalPanel("input.upload == 'EM'",
                                    div(width = 3, id="click.length.abundance.em",
                                        valueBoxOutput(width = 3, "length.abundance.em")),
                                    div(width = 3,  id="click.threedpoints.abundance",
                                        valueBoxOutput(width = 3, "threedpoints.abundance")),
                                    div(width = 3, id = "click.lengths.no.number",
                                        valueBoxOutput(width = 3,  "lengths.no.number")),
                                    div(width = 3, id = "click.threedpoints.no.number",
                                        valueBoxOutput(width = 3,  "threedpoints.no.number"))),
            
            div(width = 3, id="click.length.synonym",
                valueBoxOutput("length.synonym")),
            div(width = 3, id = "click.length.species.not.observed",
                valueBoxOutput("length.species.not.observed")),
            div(width = 3, id = "click.length.species.not.observed.lh",
                valueBoxOutput("length.species.not.observed.lh")),
            
            div(width = 3, id = "click.length.wrong.small",
                valueBoxOutput("length.wrong.small")),
            div(width = 3, id = "click.length.wrong.big",
                valueBoxOutput("length.wrong.big")),
            div(width = 3, id = "click.length.wrong.big.100",
                valueBoxOutput("length.wrong.big.100")),
            
            shiny::conditionalPanel("input.upload == 'EM'",
                                    # column(width = 12,
                                    box(width = 4, title = "RMS limit (mm)?", status = "primary", solidHeader = TRUE, 
                                        numericInput("rms.limit", NULL, 20, min = 1, max = 100)),
                                    div(width = 3, id = "click.length.wrong.rms",
                                        valueBoxOutput("length.wrong.rms")),
                                    div(width = 3, id = "click.length.wrong.rms.3dpoint",
                                        valueBoxOutput("length.wrong.rms.3dpoint"))
                                    # )
            ),
            column(width = 12),
            shiny::conditionalPanel("input.upload == 'EM'",
                                    box(width = 3, title = "Precision to length ratio (%)?", status = "primary", solidHeader = TRUE, 
                                        numericInput("precision.limit", NULL, 10, min = 1, max = 100)),
                                    div(width = 3, id = "click.length.wrong.precision",
                                        valueBoxOutput(width = 3, "length.wrong.precision"))
            ),
            
            shiny::conditionalPanel("input.upload == 'EM'",
                                    box(width = 3, title = "Range limit (m)?", status="primary", solidHeader = TRUE, 
                                        numericInput("range.limit", NULL, 10, min = 0.5, max = 10)),
                                    div(width = 3, id = "click.length.out.of.range",
                                        valueBoxOutput(width = 3, "length.out.of.range"))),
            
            box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                htmlOutput("length.species.dropdown", multiple = TRUE)),
            
            
            box(width = 12, title = "Length histogram", status = "primary", plotOutput("length.histogram", height = 250)),
            box(width = 12, title = "Length histogram status", status = "primary", plotOutput("length.histogram.status", height = 600)),
            box(width = 12, title = "Zone", status = "primary", plotOutput("length.status.plot", height = 250)),
            box(width = 12, title = "Status", status = "primary", plotOutput("length.zone.plot", height = 250))
          )
  ),
  
  
  
  
  # Check length - transect based data -----
  tabItem(tabName = "createlengtht",
          fluidRow(shiny::conditionalPanel("input.upload == 'G'",
                                           div(id="click.length.abundance.t.gen", 
                                               valueBoxOutput(width = 12, "length.abundance.t.gen"))),
                   
                   shiny::conditionalPanel("input.upload == 'EM'",
                                           div(width = 3, id="click.length.abundance.t.em",
                                               valueBoxOutput(width = 3, "length.abundance.t.em")),
                                           div(width = 3,  id="click.threedpoints.abundance.t",
                                               valueBoxOutput(width = 3, "threedpoints.abundance.t")),
                                           div(width = 3, id = "click.lengths.no.number.t",
                                               valueBoxOutput(width = 3,  "lengths.no.number.t")),
                                           div(width = 3, id = "click.threedpoints.no.number.t",
                                               valueBoxOutput(width = 3,  "threedpoints.no.number.t"))),
                   
                   div(width=3,id="click.length.synonym.t",
                       valueBoxOutput("length.synonym.t")),
                   
                   div(width=3,id="click.length.species.not.observed.t",
                       valueBoxOutput("length.species.not.observed.t")),
                   
                   div(width=3,id="click.length.species.not.observed.t.lh",
                       valueBoxOutput("length.species.not.observed.t.lh")),
                   
                   
                   div(width=3,id="click.length.wrong.small.t",
                       valueBoxOutput("length.wrong.small.t")),
                   
                   div(width=3,id="click.length.wrong.big.t",
                       valueBoxOutput("length.wrong.big.t")),
                   
                   div(width = 3, id = "click.length.wrong.big.100.t",
                       valueBoxOutput("length.wrong.big.100.t")),
                   
                   
                   shiny::conditionalPanel("input.upload == 'EM'",
                                           # column(width = 12,
                                           box(width = 4, title = "RMS limit (mm)?", status = "primary", solidHeader = TRUE, 
                                               numericInput("rms.limit.t", NULL, 20, min = 1, max = 100)),
                                           div(width = 3, id = "click.length.wrong.rms.t",
                                               valueBoxOutput("length.wrong.rms.t")),
                                           div(width = 3, id = "click.length.wrong.rms.3dpoint.t",
                                               valueBoxOutput("length.wrong.rms.3dpoint.t"))
                                           # )
                   ),
                   column(width = 12),
                   shiny::conditionalPanel("input.upload == 'EM'",
                                           box(width = 3, title = "Precision to length ratio (%)?", status = "primary", solidHeader = TRUE, 
                                               numericInput("precision.limit.t", NULL, 10, min = 1, max = 100)),
                                           div(width = 3, id = "click.length.wrong.precision.t",
                                               valueBoxOutput(width = 3, "length.wrong.precision.t"))
                   ),
                   
                   
                   box(width = 3, title = "Range limit (m)?", status = "primary", solidHeader = TRUE, 
                       numericInput("range.limit.t", NULL, 10, min = 0.5, max = 10)),
                   div(width = 3, id="click.length.out.of.range.t",
                       valueBoxOutput(width = 3, "length.out.of.range.t")),
                   column(width = 12),
                   
                   box(width = 6, title = "Set transect belt width (m)", status = "primary", solidHeader = TRUE,
                       numericInput("transect.limit.t", NULL, 5, min = 0.5, max = 10)),
                   div(width = 6, id="click.length.out.of.transect.t",
                       valueBoxOutput(width = 6, "length.out.of.transect.t")),
                   
                   box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                       htmlOutput("length.species.dropdown.t",multiple=TRUE)),
                   
                   
                   
                   
                   
                   
                   
                   box(width=12, title = "Length histogram", status = "primary", plotOutput("length.histogram.t", height = 250)),
                   box(width=12, title = "Length histogram status", status = "primary", plotOutput("length.histogram.status.t", height = 600)),
                   box(width=12, title = "Zone", status = "primary", plotOutput("length.status.plot.t", height = 250)),
                   box(width=12, title = "Status", status = "primary", plotOutput("length.zone.plot.t", height = 250))
          )
  ),
  
  
  
  #Create length vs maxn ----
  tabItem(tabName = "maxnlength",
          fluidRow(div(width = 12,id = "click.length.vs.maxn",
                       valueBoxOutput("length.vs.maxn")),
                   
                   valueBoxOutput("length.vs.maxn.score"),
                   
                   shiny::conditionalPanel("input.upload == 'EM'",
                                           div(width=12,id="click.prop.lengths",
                                               valueBoxOutput("prop.lengths"))),
                   
                   div(width = 12, id = "click.length.missing.maxn", valueBoxOutput("length.missing.maxn")),
                   div(width = 12, id = "click.length.more.maxn", valueBoxOutput("length.more.maxn")),
                   
                   box(width=12, title = "Length + 3D points vs. MaxN", status = "primary", 
                       plotOutput("length.vs.maxn.plot", height = 500)),
                   
                   # shiny::conditionalPanel("input.upload == 'EM'",
                   box(width = 12, title = "Lengths vs. 3D points", status = "primary", 
                       "A * denotes a sample where the sum of length measurements and 3D points is greater than the MaxN",
                       checkboxInput("length.vs.3d.plot.facet", label = "Facet by 'observer_length'?", value = FALSE), 
                       plotOutput("length.vs.3d.plot", height = 500)),
                   
                   box(width = 12, title = "Lengths vs. 3D points as proportion", status = "primary", 
                       "A * denotes a sample where the sum of length measurements and 3D points is greater than the MaxN",
                       checkboxInput("length.vs.3d.plot.prop.facet", label = "Facet by 'observer_length'?", value = FALSE), 
                       plotOutput("length.vs.3d.plot.prop", height = 500)), #)
                   
                   
                   
                   box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                       htmlOutput("length.vs.maxn.species.dropdown", multiple = TRUE)),
                   
                   box(width=12, title = "Length + 3D points vs. MaxN", status = "primary", 
                       plotOutput("length.vs.maxn.plot.species", height = 500)),
                   
                   # shiny::conditionalPanel("input.upload == 'EM'",
                   box(width = 12, title = "Lengths vs. 3D points", status = "primary", 
                       plotOutput("length.vs.3d.species.plot.stack", height = 300)),
                   
                   box(width = 12, title = "Lengths vs. 3D points as proportion", status = "primary", 
                       plotOutput("length.vs.3d.species.plot.prop", height = 300))
                   # )
          )),
  
  
  
  
  #Create mass - point based data ----
  tabItem(tabName = "createmass",
          fluidRow(box(width = 9,title = "Species to plot",status="primary",solidHeader = TRUE,numericInput("mass.species.limit", "Number:", 15, min = 5, max = 20)),
                   box(width = 3,title = "Include elasmobranchs?",status="primary",solidHeader = TRUE,selectInput("mass.include.sharks", "",
                                                                                                                  c("Yes" = "yes",
                                                                                                                    "No" = "no"))),
                   box(width = 12,height = 500,title = "Plot of top species by mass", status = "primary",
                       plotOutput("mass.top.species")),
                   box(width = 12,title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                       htmlOutput("mass.species.dropdown",multiple=TRUE)),
                   box(width = 12,leafletOutput("mass.spatial.plot")),
                   box(width = 12, title = "Status", status = "primary", plotOutput("mass.status.plot", height = 250)),
                   box(width = 12, title = "Zone", status = "primary", plotOutput("mass.zone.plot", height = 250))
          )),
  
  #Create mass  - transect based data ----
  tabItem(tabName = "createmasst",
          fluidRow(box(width = 9,title = "Species to plot",status="primary",solidHeader = TRUE,
                       numericInput("mass.species.limit.t", "Number:", 15, min = 5, max = 20)),
                   box(width = 3,title = "Include elasmobranchs?", status="primary", solidHeader = TRUE, selectInput("mass.include.sharks.t", "",
                                                                                                                     c("Yes" = "yes",
                                                                                                                       "No" = "no"))),
                   box(width = 12,height = 500,title = "Plot of top species by mass", status = "primary",
                       plotOutput("mass.top.species.t")),
                   
                   box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                       htmlOutput("mass.species.dropdown.t",multiple=TRUE)),
                   box(width = 12,leafletOutput("mass.spatial.plot.t")),
                   box(width = 12, title = "Status", status = "primary", plotOutput("mass.status.plot.t", height = 250)),
                   box(width = 12, title = "Zone", status = "primary", plotOutput("mass.zone.plot.t", height = 250))
          )),
  
  
  # Check habitat - point based data -----
  tabItem(tabName = "checkhab",
          fluidRow(
            div(width = 3, id="click.metadata.samples.without.hab",
                valueBoxOutput(width = 3, "metadata.samples.without.hab")),
            
            div(width = 3, id="click.metadata.samples.without.relief",
                valueBoxOutput(width = 3, "metadata.samples.without.relief")),
            
            div(width = 3, id="click.habitat.samples.without.metadata",
                valueBoxOutput(width = 3, "habitat.samples.without.metadata")),
            
            div(width = 3, id="click.relief.samples.without.metadata",
                valueBoxOutput(width = 3, "relief.samples.without.metadata")),
            # div(id="click.habitat.annotations.per.sample",
            #     valueBoxOutput("habitat.annotations.per.sample"))
            box(width = 3, title = "Enter the correct number of annotations per image:", status = "primary", solidHeader = TRUE,
                numericInput("number.of.annotations", NULL, 20, min = 1, max = 1000)),
            
            div(width = 3, id="click.habitat.wrong.annotations",
                valueBoxOutput(width = 3, "habitat.wrong.annotations")),
            
            div(width = 3, id="click.relief.wrong.annotations",
                valueBoxOutput(width = 3, "relief.wrong.annotations")),
            
            div(width = 3, id="click.hab.not.in.schema",
                valueBoxOutput(width = 3, "hab.not.in.schema")),
            
            
            
            box(width = 12, title = "Choose levels to investigate habitat", status = "primary",
                htmlOutput("habitat.levels", multiple = FALSE)),
            
            uiOutput("broad.box"),
            
            # box(width = 12, title = "Broad habitat", status = "primary",
            #     htmlOutput("habitat.levels", multiple = FALSE),
            #     plotOutput("habitat.broad.plot")),
            
            box(width = 12, title = "Relief", status = "primary",
                plotOutput("habitat.relief.plot", height = 250)),
            
            box(width = 12, title = "Depth distributions", status = "primary",
                numericInput("depth.bins", "Set bin width for histogram", 5, min = 1, max = 50),
                plotOutput("habitat.depth.plot", height = 750)),
            
            # FIXME figure out why they are breaking
            # box(width = 12, title = "Habitat pie chart", status = "primary",
            #     leafletOutput("hab.pies", height = 800)),
            
            box(width = 12,title = "Choose habitat type to plot below:", status = "primary", solidHeader = TRUE,
                htmlOutput("hab.dropdown", multiple = FALSE)),
            
            box(width = 12, title = "Habitat bubble plot", status = "primary",
                leafletOutput("hab.bubble", height = 800))
          )
  ),
  
  # Create downloads - point based data -----
  tabItem(tabName = "downloads",
          fluidRow(
            
            column(width = 4,
                   box(width = NULL, title = "1. Add project information",
                       status = "primary", solidHeader = TRUE,
                       
                       textInput("project.name", label = "Project name (this will be the prefix for all files)", value = "", placeholder = "add your own project name")),
                   
                   box(width = NULL, title = "2. Download all errors",
                       status = "primary", solidHeader = TRUE,
                       shiny::conditionalPanel("input.upload == 'EM'",
                                               shiny::conditionalPanel("input.periods == 'yes'",
                                                                       numericInput("error.period.length", "Enter correct period time (minutes):", 60, min = 0, max = 120)), 
                                               
                                               # shiny::conditionalPanel("input.upload == 'EM'",
                                               
                                               numericInput("error.report.range", "Enter range limit (meters):", 10, min = 0.5, max = 20),
                                               numericInput("error.report.rms", "Enter RMS limit (mm):", 20, min = 0, max = 100),
                                               numericInput("error.report.precision", "Enter precision:length ratio limit (%):", 10, min = 0, max = 100)),
                       
                       # br(),
                       div(style="display:inline-block;width:100%;text-align: center;", 
                           downloadBttn("download.all.errors", style = "unite", color = "danger", label = "All errors in EMobs"))
                   )),
            
            box(width = 4, title = "3. Select 'errors' to filter out of downloaded data",
                status="primary",solidHeader = TRUE,
                
                h4("Filters for MaxN, Length and Mass"),
                checkboxInput("error.synonyms", label = "Keep species names that have been updated", value = TRUE), 
                checkboxInput("error.area", label = "Remove species not observed in the area before (this will also remove sp1, sp2 etc.)", value = FALSE), 
                checkboxInput("error.zeros", label = "Add in zeros where species aren't present", value = TRUE), 
                checkboxInput("error.extra.col", label = "Remove CheckEM generated columns e.g. Zone & Marine Region", value = TRUE), 
                
                shiny::conditionalPanel("input.hab == 'Yes'",
                                        h4("Filters for Habitat"),
                                        checkboxInput("error.zeros.hab", label = "Add in zeros where habitat type is not present", value = TRUE)
                ),
                
                br(),
                h4("Filters for Length and Mass"),
                checkboxInput("error.length.small", label = "Filter out length measurements smaller than 15% of fishbase maximum", value = FALSE),
                checkboxInput("error.length.big", label = "Filter out length measurements larger than fishbase maximum", value = FALSE),
                numericInput("error.range.limit", "Remove 3D measurements greater than range limit (meters):", 10, min = 0.5, max = 20),
                numericInput("error.rms.limit", "Enter RMS limit (mm):", 20, min = 0, max = 100),
                numericInput("error.precision.limit", "Enter precision:length ratio limit (%):", 10, min = 0, max = 100)),
            
            
            box(width = 4, title = "4. Download final files",
                status = "primary", solidHeader = TRUE,
                div(style="display:inline-block;width:100%;text-align: center;", 
                    
                    downloadBttn("download.maxn", style = "unite", color = "primary", label = "Download all files"), br(), br()
                )),
            
            box(width = 4, title = "QC score",
                plotOutput("score.plot", width = "100%"))
            
          ),
          
          fluidRow(
            box(width = 12, title = "Quality control score table",
                dataTableOutput("scores.table")))
  ),
  
  # Create downloads - transect based data -----
  tabItem(tabName = "downloadst",
          fluidRow(
            column(width = 4,
                   box(width = NULL, title = "1. Add project information",
                       status = "primary", solidHeader = TRUE,
                       
                       textInput("project.name.t", label = "Project name (this will be the prefix for all files)", value = "")),
                   
                   box(width = NULL, title = "2. Download all errors",
                       status = "primary", solidHeader = TRUE,
                       
                       numericInput("error.report.range.t", "Enter range limit (meters):", 10, min = 0.5, max = 20),
                       numericInput("error.report.transect.t", "Enter transect belt width (metres):", 10, min = 0.5, max = 20),
                       numericInput("error.report.rms.t", "Enter RMS limit (mm):", 20, min = 0, max = 100),
                       numericInput("error.report.precision.t", "Enter precision:length ratio limit (%):", 10, min = 0, max = 100),
                       
                       
                       div(style="display:inline-block;width:100%;text-align: center;", 
                           downloadBttn("download.all.errors.t", style = "unite", color = "danger", label = "All errors in EMobs"))
                   )),
            
            box(width = 4, title = "3. Select 'errors' to filter out of downloaded data",
                status="primary",solidHeader = TRUE,
                
                checkboxInput("error.synonyms.t", label = "Keep species names that have been updated", value = TRUE), 
                checkboxInput("error.area.t", label = "Remove species not observed in the area before", value = FALSE), 
                checkboxInput("error.zeros.t", label = "Add in zeros where species aren't present", value = TRUE), 
                checkboxInput("error.extra.col.t", label = "Remove CheckEM generated columns e.g. Zone & Marine Region", value = TRUE), 
                
                checkboxInput("error.length.small.t", label = "Filter out length measurements smaller than 15% of fishbase maximum", value = FALSE),
                checkboxInput("error.length.big.t", label = "Filter out length measurements larger than fishbase maximum", value = FALSE),
                
                numericInput("error.range.limit.t", "Remove 3D measurements greater than range limit (meters):", 10, min = 0.5, max = 20),
                numericInput("error.transect.limit.t", "Remove 3D measurements outside transect bounds (meters):", 2.5, min = 0, max = 20),
                numericInput("error.rms.limit.t", "Enter RMS limit (mm):", 20, min = 0, max = 100),
                numericInput("error.precision.limit.t", "Enter precision:length ratio limit (%):", 10, min = 0, max = 100)),
            
            
            box(width = 4, title = "4. Download final files",
                status = "primary", solidHeader = TRUE,
                div(style="display:inline-block;width:100%;text-align: center;", 
                    
                    downloadBttn("download.transect", style = "unite", color = "primary", label = "Download all files"), br(), br()
                )
            )#,
            
            
            #   box(width = 4, title = "4. Download final files",
            #       status = "primary", solidHeader = TRUE,
            #       div(style="display:inline-block;width:100%;text-align: center;", 
            #           
            #   downloadBttn("download.length.t", style = "unite", color = "primary", label = "Length"), br(), br(),
            #   downloadBttn("download.mass.t", style = "unite", color = "primary", label = "Mass"), br(), br(),
            #   # downloadBttn("download.broad.habitat.t", style = "unite", color = "primary", label = "Habitat")
            #   )
            # )
          )
  ),
  
  tabItem(tabName = "acknowledgements",
          fluidRow(box(width = 4, status = "primary", height = 800,
                       "     ",HTML('<center><img src="logos-stacked.png" width="100%"></center>')
                       
          ),
          box(width = 8, status = "primary", height = 800, title = "Acknowledgements",
              "The Marine Biodiversity Hub is funded by the Australian Government's National Environmental Science Program.",
              br(),br(),
              "Ningaloo Marine Park example stereo-BRUV data was from the benchmark survey of deepwater fish in the Ningaloo Marine Park, Commonwealth waters. Funded by the Marine Biodiversity Hub, Parks Australia, the CSIRO and the University of Western Australia.",
              br(),br(),
              "GlobalArchive and CheckEM development has been supported by the Australian Research Data Commons and the
                  National Environmental Science Program's Marine Biodiversity Hub.", br(), br())
          )
  ),
  
  tabItem(tabName= "guide", 
          fluidRow(
            
            tags$iframe(style="height:800px; width:100%", src="CheckEM_user_guide.pdf")#,
            
            # box(width = 12, status = "primary", collapsible = TRUE, title = "How to use CheckEM", solidHeader = TRUE, 
            #     includeMarkdown("howto.Rmd")#,
            #     # includeHTML("howto.html")
            #     )
            
            #,
            
            # # TODO add a spinner
            # box(width = 4, height = 760, status = "primary", collapsible = TRUE, title = "Regions used in CheckEM", solidHeader = TRUE, 
            #     leafletOutput("regions.leaflet", height = 700))
          )
  ),
  
  tabItem(tabName= "update", 
          fluidRow(
            
            HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSfDIxlLuxzdsXgWtdp6YI8s_LxpFLANnSHDnk9Io5USOyKGrQ/viewform?embedded=true" width="100%" height="1000" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>')
          )
          
          
  ),
  
  tabItem(tabName= "feedback", 
          fluidRow(
            
            HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSeMIO3UIrkciATxmRA96xs36XejdO6GV-G6yHGXjxZOrzRBVA/viewform?embedded=true" width="100%" height="1000" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>')
          )
          
          
  ),
  
  tabItem(tabName = "change",
          column(width = 2),
          box(width = 8, status = "primary", title = NULL,
              includeMarkdown("markdown/changelog.Rmd")
          )
  ),
  
  tabItem(tabName = "schema",
          box(width = 8, status = "primary", title = NULL,
              downloadBttn("schema.fish", style = "unite", color = "primary", label = "Download Australian fish schema"),
              downloadBttn("schema.habitat", style = "unite", color = "primary", label = "Download habitat schema"),
              downloadBttn("schema.relief", style = "unite", color = "primary", label = "Download relief schema")
          )
  )
  
  
  
)

      )
    )
  )
}
# )