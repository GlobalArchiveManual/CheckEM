tagList(
  useShinyjs(),
  dashboardPage(
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
    menuItem("Upload data", tabName = "upload", icon = icon("upload")),
    
    # BRUV tabs
    shiny::conditionalPanel(condition = "input.transect == 'Non-transect e.g. BRUV'", 
                            sidebarMenu(
                              menuItem("Check metadata", tabName = "checkmetadata", icon = icon("check")),
                              menuItem("Create & check MaxN", tabName = "createmaxn", icon = icon("check")),
                              menuItem("Check length & 3D points", tabName = "createlength", icon = icon("check")),
                              menuItem("Compare MaxN & length", tabName = "maxnlength", icon = icon("equals")),
                              menuItem("Create & check mass", tabName = "createmass", icon = icon("check")),
                              menuItem("Download data", tabName = "downloads", icon = icon("download")))
                            ),
    
    # Transect tabs
    shiny::conditionalPanel(condition = "input.transect == 'Transect based e.g. DOV'", 
                            sidebarMenu(
                              menuItem("Check metadata", tabName = "checkmetadatat", icon = icon("check")),
                              menuItem("Check length & 3D points", tabName = "createlengtht", icon = icon("check")),
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
    tabItems(
      # Upload data ----
      tabItem(tabName = "upload",
              fluidRow(tags$head(tags$style(type = 'text/css',  '.rpivotTable{ overflow-x: scroll; }')),
                       
                  box(width = 6, height = 825, status = "primary", collapsible = TRUE, title = "Aims", solidHeader = TRUE, 
                           includeMarkdown("aims.Rmd")),     
                  
                  box(width = 6, title = "Single point or Transect based", status = "primary", solidHeader = TRUE,
                      radioButtons("transect", "Choose the type of data:",
                                   c("Non-transect e.g. BRUV",
                                     "Transect based e.g. DOV"), 
                                   selected = "Non-transect e.g. BRUV")),
                       
                  box(width = 6, title = "Upload metadata", status = "primary", solidHeader = TRUE,
                    fileInput("upload.metadata", ".csv only:", multiple = TRUE,
                                 accept = c("image/vnd.csv",".csv"))),
                       
                  
                  shiny::conditionalPanel(condition = "input.transect == 'Non-transect e.g. BRUV'", 
                                          box(width = 6, title = "Upload points file", status = "primary",solidHeader = TRUE,
                                              fileInput("upload.points", ".txt file only", multiple = TRUE,
                                                        accept = c("image/vnd.txt",".txt")))
                                          ),
                       
                  box(width = 6, title = "Upload length file", status = "primary",solidHeader = TRUE,
                      fileInput("upload.length", ".txt file only", multiple = TRUE,
                                 accept = c("image/vnd.txt",".txt"))),
                       
                  box(width = 6, title = "Upload 3D points file", status = "primary",solidHeader = TRUE,
                      fileInput("upload.3dpoints", ".txt file only", multiple = TRUE,
                                 accept = c("image/vnd.txt",".txt")))
                  ),
                  
                  tabBox(width = 12, height = 800,
                    # Title can include an icon
                    title = tagList(shiny::icon("gear"), "Preview data"),
                    tabPanel("Metadata", div(style = 'overflow-x: scroll', 
                                             tableOutput("table.metadata")
                                             )
                             ),
                    tabPanel("Points", tableOutput("table.points")),
                    tabPanel("Lengths", tableOutput("table.length")),
                    tabPanel("3D Points", tableOutput("table.3dpoints"))
                  
                  )
      ),
      
      # Check metadata - point based data -----
      tabItem(tabName = "checkmetadata",
              fluidRow(div(id="click.metadata.no.samples",
                           valueBoxOutput("metadata.no.samples")),
                       div(id="click.metadata.samples.without.fish",
                           valueBoxOutput("metadata.samples.without.fish")),
                       div(id="click.points.samples.without.metadata",
                           valueBoxOutput("points.samples.without.metadata")),
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
                       box(width=12, height = 825, leafletOutput("map.metadata.t", height = 800)))
      ),
      
      # Create maxn -----
      tabItem(tabName = "createmaxn",
              fluidRow(div(id="click.maxn.total.number",
                           valueBoxOutput("maxn.total.number")),
                       div(id="click.maxn.synonym",
                           valueBoxOutput("maxn.synonym")),
                       div(id="click.maxn.species.not.observed",
                           valueBoxOutput("maxn.species.not.observed")),
                       box(width=10,height = 500,
                           title = "Plot of most abundant species", status = "primary",
                           plotOutput("maxn.top.species")),
                       
                       box(width=2,title = "Species to plot",status="primary",solidHeader = TRUE,numericInput("species.limit", "Number:", 15, min = 5, max = 20)),
                       box(width=12,title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
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
              fluidRow(div(width=3,id="click.length.abundance",
                           valueBoxOutput("length.abundance")),
                       div(width=3,id="click.threedpoints.abundance",
                           valueBoxOutput("threedpoints.abundance")),
                       div(width=3,id="click.length.synonym",
                           valueBoxOutput("length.synonym")),
                       div(width=3,id="click.length.species.not.observed",
                           valueBoxOutput("length.species.not.observed")),
                       div(width=3,id="click.length.wrong.small",
                           valueBoxOutput("length.wrong.small")),
                       div(width=3,id="click.length.wrong.big",
                           valueBoxOutput("length.wrong.big")),
                       
                       box(title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("length.species.dropdown",multiple=TRUE)),
                       box(width=2,title = "Limit range",status="primary",solidHeader = TRUE,numericInput("range.limit", "Metres:", 10, min = 0.5, max = 10)),
                       div(width=3,id="click.length.out.of.range",
                           valueBoxOutput("length.out.of.range")),
              box(width=12, title = "Length histogram", status = "primary", plotOutput("length.histogram", height = 250)),
              box(width=12, title = "Length histogram status", status = "primary", plotOutput("length.histogram.status", height = 600)),
              box(width=12, title = "Zone", status = "primary", plotOutput("length.status.plot", height = 250)),
              box(width=12, title = "Status", status = "primary", plotOutput("length.zone.plot", height = 250))
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
                       div(width=3,id="click.length.species.not.observed.t",
                           valueBoxOutput("length.species.not.observed.t")),
                       div(width=3,id="click.length.wrong.small.t",
                           valueBoxOutput("length.wrong.small.t")),
                       div(width=3,id="click.length.wrong.big.t",
                           valueBoxOutput("length.wrong.big.t")),
                       
                       box(title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("length.species.dropdown.t",multiple=TRUE)),
                       box(width=2,title = "Limit range",status="primary",solidHeader = TRUE,numericInput("range.limit.t", "Metres:", 10, min = 0.5, max = 10)),
                       div(width=3,id="click.length.out.of.range.t",
                           valueBoxOutput("length.out.of.range.t")),
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
      
      
      #Create mass ----
      tabItem(tabName = "createmass",
              fluidRow(box(width=9,height = 500,title = "Plot of top species by mass", status = "primary",
                           plotOutput("mass.top.species")),
                       box(width=3,title = "Species to plot",status="primary",solidHeader = TRUE,numericInput("mass.species.limit", "Number:", 15, min = 5, max = 20)),
                       box(width=3,title = "Include elasmobranchs?",status="primary",solidHeader = TRUE,selectInput("mass.include.sharks", "",
                                   c("Yes" = "yes",
                                     "No" = "no"))),
                       box(width=12,title = "Choose species to plot below:", status = "primary", solidHeader = TRUE,
                           htmlOutput("mass.species.dropdown",multiple=TRUE)),
                       box(width=12,leafletOutput("mass.spatial.plot")),
                       box(width=12, title = "Status", status = "primary", plotOutput("mass.status.plot", height = 250)),
                       box(width=12, title = "Zone", status = "primary", plotOutput("mass.zone.plot", height = 250))
      )),
      
      
      # Create downloads -----
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

# fluidRow