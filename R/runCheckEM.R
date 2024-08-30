#' Run the CheckEM Shiny App Locally
#'
#' This function launches a local version of the CheckEM Shiny app in your default web browser. 
#' The app allows users to perform quality control checks on metadata and annotations for fish 
#' and benthic stereo-imagery datasets.
#'
#' @return Launches the CheckEM Shiny app in the web browser.
#' @export
#'
#' @examples 
#' \dontrun{
#' # To run the CheckEM Shiny app locally
#' CheckEM::runCheckEM()
#' }
runCheckEM <- function() {
  appDir <- system.file("shiny", "CheckEM", package = "CheckEM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CheckEM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}