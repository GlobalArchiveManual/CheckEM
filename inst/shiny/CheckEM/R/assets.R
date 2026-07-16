checkem_cache_dir <- function() {
  dir <- rappdirs::user_data_dir("CheckEM")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

checkem_asset_path <- function(...) {
  file.path(checkem_cache_dir(), ...)
}

# download_if_missing <- function(url, dest) {
#   if (!file.exists(dest)) {
#     dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
#     utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
#   }
#   dest
# }
download_if_missing <- function(url, dest, timeout = 1800) {
  
  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }
  
  dir.create(
    dirname(dest),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  temp_dest <- paste0(dest, ".download")
  
  # Remove any previous incomplete download
  unlink(temp_dest, force = TRUE)
  
  old_timeout <- getOption("timeout")
  
  on.exit({
    options(timeout = old_timeout)
    
    # Remove incomplete temporary file if the download failed
    if (file.exists(temp_dest)) {
      unlink(temp_dest, force = TRUE)
    }
  }, add = TRUE)
  
  options(timeout = max(timeout, old_timeout))
  
  utils::download.file(
    url = url,
    destfile = temp_dest,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
  
  if (!file.exists(temp_dest) || file.info(temp_dest)$size == 0) {
    stop("The downloaded file is missing or empty: ", url)
  }
  
  if (!file.rename(temp_dest, dest)) {
    stop(
      "Downloaded the file but could not move it into the cache: ",
      dest
    )
  }
  
  dest
}

# ensure_all_data <- function(version = "1.0.2") {
#   # Keep the version string simple, like "0.1.0"
#   ver <- as.character(version)
#   tag <- paste0("V", ver)
#   
#   # IMPORTANT: update these two values to your repo owner/name
#   owner <- "GlobalArchiveManual"
#   repo  <- "CheckEM"
#   
#   options(timeout = 300)  # 5 minutes?
#   
#   url <- sprintf("https://github.com/%s/%s/releases/download/%s/all_data.Rdata",
#                  owner, repo, tag)
#   
#   dest <- checkem_asset_path("data", ver, "all_data.Rdata")
#   download_if_missing(url, dest)
# }

ensure_all_data <- function(version = "1.0.2") {
  
  ver <- as.character(version)
  tag <- paste0("V", ver)
  
  owner <- "GlobalArchiveManual"
  repo  <- "CheckEM"
  
  url <- sprintf(
    "https://github.com/%s/%s/releases/download/%s/all_data.Rdata",
    owner,
    repo,
    tag
  )
  
  dest <- checkem_asset_path(
    "data",
    ver,
    "all_data.Rdata"
  )
  
  download_if_missing(
    url = url,
    dest = dest,
    timeout = 300
  )
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
