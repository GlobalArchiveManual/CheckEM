navbarPage(
  #theme = shinytheme("cerulean"),
  "CheckEM",
  tabPanel("Upload",
    sidebarLayout(
      sidebarPanel(
        # File uploads ----
        fileInput("upload.metadata", "Upload metadata file:",
                  accept = c("image/vnd.csv",".csv")),
        
        fileInput("upload.points", "Upload points file:",
                  accept = c("image/vnd.txt",".txt")),
        
        fileInput("upload.length", "Upload length file:",
                  accept = c("image/vnd.txt",".txt")),
        
        fileInput("upload.3dpoints", "Upload 3D points file:",
                  accept = c("image/vnd.txt",".txt"))),
      mainPanel(
        h3("Preview data:"),
        h5("Will display once data has been uploaded"),
        tabsetPanel(type = "tabs",
                    tabPanel("Metadata", tableOutput("table.metadata")),
                    tabPanel("Points", tableOutput("table.points")),
                    tabPanel("Lengths", tableOutput("table.length")),
                    tabPanel("3D Points", tableOutput("table.3dpoints")))
      )
    )
  ),
  tabPanel("Metadata checks",
           sidebarLayout(
             sidebarPanel(),
             mainPanel(fluidRow(
               valueBoxOutput("metadata.no.samples"),
               valueBoxOutput("metadata.samples.without.fish"),
               valueBoxOutput("points.samples.without.metadata"))
             )
             )# side bar layout
             ) # end tab panel
) #end of navbar page
