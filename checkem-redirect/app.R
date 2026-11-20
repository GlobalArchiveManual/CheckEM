library(shiny)

new_url <- "https://marine-ecology.shinyapps.io/CheckEM/"

ui <- fluidPage(
  tags$head(
    # shinyapps.io loads apps inside an iframe, so redirect the top window.
    # Any ?query string is passed on to the new address.
    tags$script(HTML(sprintf(
      "var target = '%s' + window.top.location.search;
       try { window.top.location.replace(target); }
       catch (e) { window.location.replace(target); }",
      new_url
    )))
  ),
  # Fallback in case JavaScript is blocked
  div(style = "padding: 2em; font-family: sans-serif;",
      h3("CheckEM has moved"),
      p("Its new home is ", a(href = new_url, target = "_top", new_url)),
      p("Please update your bookmarks."))
)

server <- function(input, output, session) {}

shinyApp(ui, server)