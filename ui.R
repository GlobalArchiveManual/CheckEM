tagList(
  useShinyjs(),
  dashboardPage(
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
    menuItem("Upload data", tabName = "upload", icon = icon("upload")),
    
    # BRUV tabs
    shiny::conditionalPanel("input.method == 'point'",
    # shiny::conditionalPanel(condition = "input.transect == 'Non-transect e.g. BRUV'", 
                            sidebarMenu(
                              menuItem("Check metadata & periods", tabName = "checkmetadata", icon = icon("check")),
                              menuItem("Create & check MaxN", tabName = "createmaxn", icon = icon("check")),
                              menuItem("Check length & 3D points", tabName = "createlength", icon = icon("check")),
                              menuItem("Compare MaxN & length", tabName = "maxnlength", icon = icon("equals")),
                              menuItem("Check habitat", tabName = "checkhab", icon = icon("check")),
                              menuItem("Create & check mass", tabName = "createmass", icon = icon("check")),
                              menuItem("Download data", tabName = "downloads", icon = icon("download")))
                            ),
    
    # Transect tabs
    shiny::conditionalPanel("input.method == 'transect'",
    # shiny::conditionalPanel(condition = "input.transect == 'Transect based e.g. DOV'", 
                            sidebarMenu(
                              menuItem("Check metadata & periods", tabName = "checkmetadatat", icon = icon("check")),
                              menuItem("Check length & 3D points", tabName = "createlengtht", icon = icon("check")),
                              menuItem("Check habitat", tabName = "checkhab", icon = icon("check")),
                              menuItem("Create & check mass", tabName = "createmasst", icon = icon("check")),
                              menuItem("Download data", tabName = "downloadst", icon = icon("download")))
                            ),
    
    menuItem("User guide", tabName = "guide", icon = icon("info", lib="font-awesome")),
    menuItem("Feedback", tabName = "feedback", icon = icon("comment", lib="font-awesome")),
    menuItem("Acknowledgements", tabName = "acknowledgements", icon = icon("hands-helping", lib="font-awesome"))
  )
  ),
  
  dashboardBody(
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),
    
    tabItems(
      # Upload data ----
      tabItem(tabName = "upload",
              fluidRow(tags$head(tags$style(type = 'text/css',  '.rpivotTable{ overflow-x: scroll; }')),
                       
                       column(width = 6,
                              
                              box(width = NULL, title = "Format of data", status = "primary", solidHeader = TRUE,
                                  
                                  radioButtons("method", "Choose the type of method:",
                                               c("Single point e.g. BRUV & BOSS" = "point",
                                                 "Transect e.g. DOV & ROV" = "transect"),
                                               selected = "point",
                                               inline = TRUE),
                                  
                                  shiny::conditionalPanel("input.method == 'point'",
                                                          radioButtons("sample", "How did you record the sample name in EventMeasure:",
                                                                       c("OpCode" = "opcode",
                                                                         "Period" = "period"),
                                                                       selected = "opcode",
                                                                       inline = TRUE)),
                                  
                                  shiny::conditionalPanel("input.method == 'transect'",
                                                          radioButtons("sample.t", "How did you record the sample name in EventMeasure:",
                                                                       c("OpCode and Period" = "opcodeperiod",
                                                                         "Period only" = "period"),
                                                                       selected = "opcodeperiod",
                                                                       inline = TRUE)),
                                  
                                  radioButtons("habdirection", "If checking habitat, which directions were annotated?",
                                               c("Forwards only" = "forwards",
                                                 "Forwards and backwards" = "both"),
                                               selected = "forwards",
                                               inline = TRUE),
                                  
                                  radioButtons("habreliefsep", "If checking habitat, was relief annotated separately?",
                                               c("No" = "no",
                                                 "Yes" = "yes"),
                                               selected = "no",
                                               inline = TRUE)
                                  ),
                              
                              box(width = NULL, height = 700, status = "primary", collapsible = TRUE, title = "Aims", solidHeader = TRUE,
                                  includeMarkdown("aims.Rmd"))
                              ),
                       
                       box(width = 6, height = 110, title = "Upload metadata (csv)", status = "primary", solidHeader = FALSE,
                           fileInput("upload.metadata", NULL, multiple = TRUE,
                                     accept = c("image/vnd.csv",".csv"))),
                       
                       box(width = 6, title = "Upload EventMeasure exports  (txt files)", status = "primary", solidHeader = FALSE,
                           
                           box(width = 6, height = 110, title = "Points file", status = "primary", solidHeader = TRUE,
                               fileInput("upload.points", NULL, multiple = TRUE,
                                         accept = c("image/vnd.txt",".txt"))),
                      
                           box(width = 6, height = 110, title = "Period file", status = "primary", solidHeader = TRUE,
                               fileInput("upload.period", NULL, multiple = TRUE,
                                          accept = c("image/vnd.txt",".txt"))),
                      
                           box(width = 6, height = 110, title = "Length file", status = "primary", solidHeader = TRUE,
                               fileInput("upload.length", NULL, multiple = TRUE,
                                          accept = c("image/vnd.txt",".txt"))),
                      
                           box(width = 6, height = 110, title = "3D points file", status = "primary", solidHeader = TRUE,
                               fileInput("upload.3dpoints", NULL, multiple = TRUE,
                                          accept = c("image/vnd.txt",".txt")))
                           ),
                       
                       box(width = 6, title = "Upload TransectMeasure exports (txt files)", status = "primary", solidHeader = FALSE,
                           
                           box(width = 6, height = 110, title = "Forward facing Dot Point Measurements", 
                               status = "primary", solidHeader = TRUE,
                               
                               fileInput("upload.f.dotpoints", NULL, multiple = TRUE,
                                         accept = c("image/vnd.txt",".txt"))),
                           
                           shiny::conditionalPanel("input.habdirection == 'both'",
                                                   box(width = 6, height = 110, title = "Backward facing Dot Point Measurements", 
                                                       status = "primary", solidHeader = TRUE,
                                                       fileInput("upload.b.dotpoints", NULL, multiple = TRUE,
                                                                 accept = c("image/vnd.txt",".txt")))
                                                   ),
                           
                           shiny::conditionalPanel("input.habreliefsep == 'yes'",
                                                   
                                                   box(width = 6, height = 110, title = "Forwards Relief Dot Point Measurements", 
                                                       status = "primary", solidHeader = TRUE,
                                                       fileInput("upload.r.f.dotpoints", NULL, multiple = TRUE,
                                                                 accept = c("image/vnd.txt",".txt"))),
                                                   
                                                   shiny::conditionalPanel("input.habdirection == 'both'",
                                                                           box(width = 6, height = 110, title = "Backwards Relief Dot Point Measurements", 
                                                                               status = "primary", solidHeader = TRUE,
                                                                               fileInput("upload.r.b.dotpoints", NULL, multiple = TRUE,
                                                                                         accept = c("image/vnd.txt",".txt")))
                                                   )
                           )
                           ),
              
                  tabBox(width = 12, height = 800,
                         
                    title = tagList(shiny::icon("gear"), "Preview data"),
                    tabPanel("Metadata", div(style = 'overflow-x: scroll', 
                                             tableOutput("table.metadata")
                                             )
                             ),
                    tabPanel("Points", tableOutput("table.points")),
                    tabPanel("Lengths", tableOutput("table.length")),
                    tabPanel("3D Points", tableOutput("table.3dpoints")),
                    tabPanel("Periods", tableOutput("table.periods")),
                    tabPanel("Habitat", tableOutput("table.habitat"))
                  
                  )
      )),
      
      # Check metadata - point based data -----
      tabItem(tabName = "checkmetadata",
              fluidRow(div(id="click.metadata.no.samples",
                           valueBoxOutput("metadata.no.samples")),
                       div(id="click.metadata.samples.without.fish",
                           valueBoxOutput("metadata.samples.without.fish")),
                       div(id="click.points.samples.without.metadata",
                           valueBoxOutput("points.samples.without.metadata")),
                       
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
                           valueBoxOutput("periods.wrong")),
                       

                       box(width=12, height = 825, leafletOutput("map.metadata", height = 800)))
      ),
      
      # Check metadata - transect based data ----
      tabItem(tabName = "checkmetadatat",
              fluidRow(div(id="click.metadata.no.samples.t",
                           valueBoxOutput("metadata.no.samples.t")),
                       div(id="click.metadata.samples.without.fish.t",
                           valueBoxOutput("metadata.samples.without.fish.t")),
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
                           valueBoxOutput("periods.avg.t")),
                       
                       box(width=12, height = 825, leafletOutput("map.metadata.t", height = 800)))
      ),
      
      # Create maxn -----
      tabItem(tabName = "createmaxn",
              fluidRow(div(id="click.maxn.total.number",
                           valueBoxOutput("maxn.total.number")),
                       
                       div(id="click.points.no.number",
                           valueBoxOutput("points.no.number")),
                       
                       div(id="click.maxn.synonym",
                           valueBoxOutput("maxn.synonym")),
                       div(id="click.maxn.species.not.observed",
                           valueBoxOutput("maxn.species.not.observed")),
                       box(width=10,height = 500,
                           title = "Plot of most abundant species", status = "primary",
                           plotOutput("maxn.top.species")),
                       
                       box(width=2,title = "Species to plot",status="primary",solidHeader = TRUE,numericInput("species.limit", "Number:", 15, min = 5, max = 20)),
                       box(width=12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("maxn.species.dropdown",multiple=TRUE)),
                       box(width=12,leafletOutput("maxn.spatial.plot")),
                       box(width=12,title = "Plot of abundance by Status", status = "primary", plotOutput("maxn.status.plot", height = 250)),
                       box(width=12,title = "Plot of abundance by Zone", status = "primary", plotOutput("maxn.zone.simple", height = 250)),
                       box(width=12,title = "Plot of abundance by Location", status = "primary", plotOutput("maxn.location.plot", height = 250)),
                       box(width=12,title = "Plot of abundance by Site", status = "primary", plotOutput("maxn.site.plot", height = 250))
                       )
      )
      ,
      
      # Check length - point based data -----
      tabItem(tabName = "createlength",
              fluidRow(div(width = 3, id="click.length.abundance",
                           valueBoxOutput("length.abundance")),
                       div(width = 3, id="click.threedpoints.abundance",
                           valueBoxOutput("threedpoints.abundance")),
                       div(width = 3, id="click.length.synonym",
                           valueBoxOutput("length.synonym")),
                       
                       div(width = 3, id = "click.lengths.no.number",
                           valueBoxOutput("lengths.no.number")),
                       div(width = 3, id = "click.threedpoints.no.number",
                           valueBoxOutput("threedpoints.no.number")),
                       
                       div(width = 3, id = "click.length.species.not.observed",
                           valueBoxOutput("length.species.not.observed")),
                       
                       div(width = 3, id = "click.length.wrong.small",
                           valueBoxOutput("length.wrong.small")),
                       div(width = 3, id = "click.length.wrong.big",
                           valueBoxOutput("length.wrong.big")),
                       div(width = 3, id = "click.length.wrong.big.100",
                           valueBoxOutput("length.wrong.big.100")),
                       
                       box(title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("length.species.dropdown", multiple=TRUE)),
                       box(width = 2, title = "Range limit (m)?", status="primary", solidHeader = TRUE, 
                           numericInput("range.limit", NULL, 10, min = 0.5, max = 10)),
                       div(width = 3, id = "click.length.out.of.range",
                           valueBoxOutput("length.out.of.range")),
              box(width = 12, title = "Length histogram", status = "primary", plotOutput("length.histogram", height = 250)),
              box(width = 12, title = "Length histogram status", status = "primary", plotOutput("length.histogram.status", height = 600)),
              box(width = 12, title = "Zone", status = "primary", plotOutput("length.status.plot", height = 250)),
              box(width = 12, title = "Status", status = "primary", plotOutput("length.zone.plot", height = 250))
              )
      ),
      
      
      
      
      # Check length - transect based data -----
      tabItem(tabName = "createlengtht",
              fluidRow(div(width=3,id="click.length.abundance.t",
                           valueBoxOutput("length.abundance.t")),
                       div(width=3,id="click.threedpoints.abundance.t",
                           valueBoxOutput("threedpoints.abundance.t")),
                       
                       
                       div(width=3,id="click.length.synonym.t",
                           valueBoxOutput("length.synonym.t")),
                       
                       div(width = 3, id = "click.lengths.no.number.t",
                           valueBoxOutput("lengths.no.number.t")),
                       div(width = 3, id = "click.threedpoints.no.number.t",
                           valueBoxOutput("threedpoints.no.number.t")),
                       
                       div(width=3,id="click.length.species.not.observed.t",
                           valueBoxOutput("length.species.not.observed.t")),
                       
                       
                       div(width=3,id="click.length.wrong.small.t",
                           valueBoxOutput("length.wrong.small.t")),
                       div(width=3,id="click.length.wrong.big.t",
                           valueBoxOutput("length.wrong.big.t")),
                       div(width = 3, id = "click.length.wrong.big.100.t",
                           valueBoxOutput("length.wrong.big.100.t")),
                       
                       box(width = 2, title = "Range limit (m)?", status = "primary", solidHeader = TRUE, 
                           numericInput("range.limit.t", NULL, 10, min = 0.5, max = 10)),
                       
                       div(width = 2,id="click.length.out.of.range.t",
                           valueBoxOutput("length.out.of.range.t")),
                       
                       box(width = 2, title = "Transect bounds (m)?", status = "primary", solidHeader = TRUE, 
                           numericInput("transect.limit.t", NULL, 2.5, min = 0.5, max = 5)),
                       
                       div(width = 2,id="click.length.out.of.transect.t",
                           valueBoxOutput("length.out.of.transect.t")),
                       
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
              fluidRow(div(width=12,id="click.length.vs.maxn",
                           valueBoxOutput("length.vs.maxn")),
                       box(width=12, title = "Length vs. MaxN", status = "primary", 
                           plotOutput("length.vs.maxn.plot", height = 600)),
                       box(title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("length.vs.maxn.species.dropdown",multiple=TRUE)),
                       box(width=12, title = "Length vs. MaxN", status = "primary", 
                           plotOutput("length.vs.maxn.species.plot", height = 600)))
              ),
      
      

      
      #Create mass - point based data ----
      tabItem(tabName = "createmass",
              fluidRow(box(width = 9,height = 500,title = "Plot of top species by mass", status = "primary",
                           plotOutput("mass.top.species")),
                       box(width = ,title = "Species to plot",status="primary",solidHeader = TRUE,numericInput("mass.species.limit", "Number:", 15, min = 5, max = 20)),
                       box(width = 3,title = "Include elasmobranchs?",status="primary",solidHeader = TRUE,selectInput("mass.include.sharks", "",
                                                                                                                    c("Yes" = "yes",
                                                                                                                      "No" = "no"))),
                       box(width = 12,title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("mass.species.dropdown",multiple=TRUE)),
                       box(width = 12,leafletOutput("mass.spatial.plot")),
                       box(width = 12, title = "Status", status = "primary", plotOutput("mass.status.plot", height = 250)),
                       box(width = 12, title = "Zone", status = "primary", plotOutput("mass.zone.plot", height = 250))
              )),
      
      #Create mass  - transect based data ----
      tabItem(tabName = "createmasst",
              fluidRow(box(width = 9,height = 500,title = "Plot of top species by mass", status = "primary",
                           plotOutput("mass.top.species.t")),
                       box(width = 3,title = "Species to plot",status="primary",solidHeader = TRUE,
                           numericInput("mass.species.limit.t", "Number:", 15, min = 5, max = 20)),
                       box(width = 3,title = "Include elasmobranchs?", status="primary", solidHeader = TRUE, selectInput("mass.include.sharks.t", "",
                                                                                                                       c("Yes" = "yes",
                                                                                                                         "No" = "no"))),
                       box(width = 12, title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("mass.species.dropdown.t",multiple=TRUE)),
                       box(width = 12,leafletOutput("mass.spatial.plot.t")),
                       box(width = 12, title = "Status", status = "primary", plotOutput("mass.status.plot.t", height = 250)),
                       box(width = 12, title = "Zone", status = "primary", plotOutput("mass.zone.plot.t", height = 250))
              )),
      
      
      # Check habitat - point based data -----
      tabItem(tabName = "checkhab",
              fluidRow(div(id="click.metadata.samples.without.hab",
                           valueBoxOutput("metadata.samples.without.hab")),
                       div(id="click.habitat.samples.without.metadata",
                           valueBoxOutput("habitat.samples.without.metadata")),
                       div(id="click.habitat.annotations.per.sample",
                           valueBoxOutput("habitat.annotations.per.sample"))
                       
                       )
      ),
      
      
      # Create downloads - point based data -----
      tabItem(tabName = "downloads",
              fluidRow(
                box(width=6,title = "Select 'errors' to filter out of downloaded data",
                    status="primary",solidHeader = TRUE,
                    
                    h4("Add project and campaign information"),
                    textInput("project.name", label = "Project name:", value = ""),
                    
                    h4("Filters for MaxN, Length and Mass"),
                    checkboxInput("error.synonyms", label = "Keep species names that have been updated", value = TRUE), 
                    checkboxInput("error.area", label = "Remove species not observed in the area before", value = FALSE), 
                    br(),
                    h4("Filters for Length and Mass"),
                    numericInput("error.range.limit", "Remove 3D measurements greater than range limit (meters):", 10, min = 0.5, max = 20),
                    checkboxInput("error.length.small", label = "Filter out length measurements smaller than 15% of fishbase maximum", value = FALSE),
                    checkboxInput("error.length.big", label = "Filter out length measurements larger than fishbase maximum", value = FALSE)),
                
                       div(id="click.download.maxn",
                       infoBoxOutput("info.download.maxn",width=6)),
                       
                       div(id="click.download.length",
                       infoBoxOutput("info.download.length",width=6)),
                
                       div(id="click.download.mass",
                       infoBoxOutput("info.download.mass",width=6))
              )
      ),
      
      # Create downloads - transect based data -----
      tabItem(tabName = "downloadst",
              fluidRow(
                box(width=6,title = "Select 'errors' to filter out of downloaded data",
                    status="primary",solidHeader = TRUE,
                    
                    h4("Add project and campaign information"),
                    textInput("project.name.t", label = "Project name:", value = ""),
                    
                    h4("Filters for MaxN, Length and Mass"),
                    checkboxInput("error.synonyms.t", label = "Keep species names that have been updated", value = TRUE), 
                    checkboxInput("error.area.t", label = "Remove species not observed in the area before", value = FALSE), 
                    br(),
                    h4("Filters for Length and Mass"),
                    numericInput("error.range.limit.t", "Remove 3D measurements greater than range limit (meters):", 10, min = 0.5, max = 20),
                    checkboxInput("error.length.small.t", label = "Filter out length measurements smaller than 15% of fishbase maximum", value = FALSE),
                    checkboxInput("error.length.big.t", label = "Filter out length measurements larger than fishbase maximum", value = FALSE)),
                
                div(id="click.download.length.t",
                    infoBoxOutput("info.download.length.t",width=6)),
                
                div(id="click.download.mass.t",
                    infoBoxOutput("info.download.mass.t",width=6))
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
                 
                 box(width = 6, status = "primary", collapsible = TRUE, title = "How to use CheckEM", solidHeader = TRUE, 
                     includeMarkdown("howto.Rmd")),
                 
                 box(width = 6,height = 760, status = "primary", collapsible = TRUE, title = "Marine Ecoregions of the World", solidHeader = TRUE, 
                     leafletOutput("world.regions.leaflet", height = 700)),
                 
                 box(width = 6, status = "primary", collapsible = TRUE, title = "Australia's Marine Regions", solidHeader = TRUE, 
                     leafletOutput("australia.regions"))
               )
      ),
      
      tabItem(tabName= "feedback", 
              fluidRow(
                
                HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSeMIO3UIrkciATxmRA96xs36XejdO6GV-G6yHGXjxZOrzRBVA/viewform?embedded=true" width="100%" height="1000" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>')
              )
              
              
      )
      
    )
  )
)
)