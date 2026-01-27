#' Run the CheckEM Shiny App Locally
#'
#' Launches the CheckEM Shiny app in the default web browser. Optionally installs
#' required packages if they are missing.
#'
#' @param install_missing Logical. If TRUE, install missing packages automatically.
#' @param ask Logical. If TRUE, ask before installing packages.
#' @param repos Character. CRAN repo to use.
#' @return Launches the CheckEM Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' CheckEM::runCheckEM()
#' }
runCheckEM <- function(install_missing = TRUE,
                       ask = interactive(),
                       repos = getOption("repos")) {
  
  # ---- 1) Define requirements ----
  cran_pkgs <- c(
    "shiny", "shinyalert", "shinythemes", "shinydashboard", "shinyBS", "shinyjs",
    "shinyWidgets", "DT", "shinycssloaders",
    "rpivotTable", "data.table",
    "plyr", "dplyr", "stringr", "tidyr", "readr", "forcats",
    "ggplot2", "grid", "ggbeeswarm", "RColorBrewer",
    "leaflet", "leaflet.minicharts", "leafgl", "rnaturalearth",
    "googlesheets4",
    "rmarkdown", "markdown", "sf",
    "reactlog",
    "geomtextpath",
    "png", "cowplot", "devtools"
  )
  
  # GitHub remotes used by the app (only include if truly needed at runtime)
  github_pkgs <- list(
    # package_name = "owner/repo"
    GlobalArchive   = "UWAMEGFisheries/GlobalArchive",
    FSSgam_package  = "beckyfisher/FSSgam_package"
  )
  
  # ---- 2) Install missing CRAN packages ----
  missing_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(missing_cran) > 0) {
    if (!install_missing) {
      stop(
        "Missing required packages: ", paste(missing_cran, collapse = ", "),
        "\nRe-run with install_missing = TRUE or install them manually.",
        call. = FALSE
      )
    }
    
    if (ask) {
      msg <- paste0(
        "CheckEM needs to install missing CRAN packages:\n",
        paste0("  - ", missing_cran, collapse = "\n"),
        "\n\nInstall now?"
      )
      ok <- utils::menu(c("Yes", "No"), title = msg) == 1
      if (!ok) stop("Cancelled: required packages not installed.", call. = FALSE)
    }
    
    utils::install.packages(missing_cran, repos = repos)
  }
  
  # ---- 3) Install missing GitHub packages (optional but often necessary) ----
  # Only attempt if remotes is available (install if needed)
  missing_github <- names(github_pkgs)[
    !vapply(names(github_pkgs), requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_github) > 0) {
    if (install_missing) {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        utils::install.packages("remotes", repos = repos)
      }
      
      if (ask) {
        msg <- paste0(
          "CheckEM can also install missing GitHub packages:\n",
          paste0("  - ", missing_github, " (", unlist(github_pkgs[missing_github]), ")", collapse = "\n"),
          "\n\nInstall now?"
        )
        ok <- utils::menu(c("Yes", "No"), title = msg) == 1
        if (!ok) {
          warning("Skipped installing GitHub packages; the app may fail if they are required.")
        } else {
          for (pkg in missing_github) {
            remotes::install_github(github_pkgs[[pkg]])
          }
        }
      } else {
        for (pkg in missing_github) {
          remotes::install_github(github_pkgs[[pkg]])
        }
      }
    } else {
      warning(
        "Missing GitHub packages (not installing automatically): ",
        paste(missing_github, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  # ---- 4) Run app ----
  appDir <- system.file("shiny", "CheckEM", package = "CheckEM")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `CheckEM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
