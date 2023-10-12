#' @export
runCheckEM <- function() {
  appDir <- system.file("shiny", "CheckEM", package = "CheckEM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CheckEM`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
