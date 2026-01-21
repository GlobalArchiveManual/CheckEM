#' Load an RDS File from a Private GitHub Repository
#'
#' @param url Raw URL to the file (raw.githubusercontent.com/...)
#' @param pat GitHub Personal Access Token. If NULL, tries env vars then gitcreds.
#' @return R object, or NULL if not available (non-interactive w/out PAT) / access denied.
#'
#' @import httr
#' @import gitcreds
#' @export
#' @examples
#' \dontrun{
#' rds_url <- "https://raw.githubusercontent.com/GlobalArchiveManual/australia-synthesis-2024/main/data/tidy/australian-synthesis_covariates.RDS"
#' data <- load_rds_from_github(rds_url)
#' }
load_rds_from_github <- function(url, pat = NULL) {
  
  # 1) Get PAT from safest places first (common in CI), then gitcreds, then prompt (interactive only)
  if (is.null(pat) || !nzchar(pat)) {
    pat <- Sys.getenv("GITHUB_PAT", unset = "")
    if (!nzchar(pat)) pat <- Sys.getenv("GITHUB_TOKEN", unset = "")
  }
  
  if (!nzchar(pat)) {
    pat <- tryCatch(gitcreds::gitcreds_get()$password, error = function(e) "")
  }
  
  # Never prompt during R CMD check / non-interactive
  if (!nzchar(pat)) {
    if (!interactive()) {
      message(
        "No GitHub PAT available in non-interactive session.\n",
        "Set env var GITHUB_PAT (or GITHUB_TOKEN) or configure gitcreds.\n",
        "Returning NULL."
      )
      return(NULL)
    }
    
    message("GitHub PAT not found. Create one with 'repo' scope, then paste it below.")
    pat <- readline(prompt = "Enter your GitHub Personal Access Token (PAT): ")
    if (!nzchar(pat)) return(NULL)
  }
  
  # 2) Request with Authorization header (works better than basic auth for GitHub tokens)
  resp <- httr::GET(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", pat),
      `User-Agent` = "CheckEM (R; httr)"
    )
  )
  
  code <- httr::status_code(resp)
  
  if (code == 200) {
    # Read content to a temp file then readRDS (avoids gzcon/rawConnection edge cases)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(httr::content(resp, "raw"), tmp)
    return(readRDS(tmp))
  }
  
  # Helpful messages for common cases
  if (code %in% c(401, 403)) {
    message("Access denied (", code, "). Your token may lack scope, or you lack repo access.")
    message("If needed, contact Brooke Gibbons at brooke.gibbons@uwa.edu.au with your GitHub username.")
    return(NULL)
  }
  
  if (code == 404) {
    message("File not found (404) OR you don't have access (private repo can also look like 404).")
    message("If needed, contact Brooke Gibbons at brooke.gibbons@uwa.edu.au with your GitHub username.")
    return(NULL)
  }
  
  # Fallback: include server message if available
  msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
  stop("Failed to download the RDS file. Status code: ", code, if (nzchar(msg)) paste0("\n", msg))
}
