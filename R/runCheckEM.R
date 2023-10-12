#' Run CheckEM shiny app locally
#'
#' @return Will launch a local version of CheckEM in the browser
#' @export
#'
#' @examples 
#' ## Not run:
#' \dontrun{CheckEM::runCheckEM()}
#' 
runCheckEM <- function() {
  appDir <- system.file("shiny", "CheckEM", package = "CheckEM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CheckEM`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
