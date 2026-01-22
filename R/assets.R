checkem_cache_dir <- function() {
  dir <- rappdirs::user_data_dir("CheckEM")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

checkem_asset_path <- function(...) {
  file.path(checkem_cache_dir(), ...)
}

download_if_missing <- function(url, dest) {
  if (!file.exists(dest)) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  }
  dest
}

ensure_all_data <- function(version = utils::packageVersion("CheckEM")) {
  # Keep the version string simple, like "0.1.0"
  ver <- as.character(version)
  tag <- paste0("v", ver)
  
  # IMPORTANT: update these two values to your repo owner/name
  owner <- "GlobalArchiveManual"
  repo  <- "CheckEM"
  
  url <- sprintf("https://github.com/%s/%s/releases/download/%s/all_data.Rdata",
                 owner, repo, tag)
  
  dest <- checkem_asset_path("data", ver, "all_data.Rdata")
  download_if_missing(url, dest)
}
