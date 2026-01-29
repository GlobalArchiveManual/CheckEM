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

ensure_all_data <- function(version = "1.0.0") {
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

ensure_examples_dir <- function(version = "1.0.0") {
  ver <- as.character(version)
  tag <- paste0("v", ver)
  
  owner <- "GlobalArchiveManual"
  repo  <- "CheckEM"
  
  zip_url <- sprintf(
    "https://github.com/%s/%s/releases/download/%s/examples.zip",
    owner, repo, tag
  )
  
  # This matches what you currently have on disk:
  zip_dest <- file.path(rappdirs::user_data_dir("CheckEM"), "examples", ver, "examples.zip")
  unzip_root <- file.path(rappdirs::user_data_dir("CheckEM"), "examples", ver, "examples")
  
  if (!file.exists(zip_dest)) {
    dir.create(dirname(zip_dest), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(zip_url, destfile = zip_dest, mode = "wb", quiet = TRUE)
  }
  
  # Unzip if needed
  if (!dir.exists(unzip_root) || length(list.files(unzip_root, recursive = TRUE)) == 0) {
    dir.create(unzip_root, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(zip_dest, exdir = unzip_root)
  }
  
  # FIX: return the folder that actually contains the files
  file_dir <- file.path(unzip_root, "examples")
  if (dir.exists(file_dir)) return(file_dir)
  
  unzip_root
}

# Convenience: return full path to a specific example file inside the unzipped folder
checkem_example_path <- function(..., version = "1.0.0") {
  examples_dir <- ensure_examples_dir(version = version)
  file.path(examples_dir, ...)
}
