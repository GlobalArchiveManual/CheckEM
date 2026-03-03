# make_precision_variants.R
# Creates 2_decimals/, 3_decimals/, 4_decimals/ folders copied from original_data/
# and truncates latitude_dd / longitude_dd in the metadata CSV in each folder.

options(stringsAsFactors = FALSE)

# -----------------------------
# User-configurable settings
# -----------------------------
original_dir <- "I:/CheckEM/data-raw/release-assets/Exmouth-Manuscript/original_data"
metadata_file <- "2024-05_Exmouth-Gulf_stereo-BRUVs_Metadata.csv"
lat_col <- "latitude_dd"
lon_col <- "longitude_dd"
decimals_to_make <- c(1, 2, 3, 4)

# If TRUE: keep original numeric type (will write as numeric in CSV).
# If FALSE: write formatted strings with fixed decimals (e.g., -21.23 exactly).
write_as_character <- FALSE

# If TRUE: write a small audit file into each folder showing old/new coords.
write_audit_file <- TRUE

# -----------------------------
# Helpers
# -----------------------------

# Truncate a numeric vector to N decimal places (not rounding).
# Works for negatives correctly by truncating toward zero.
truncate_decimals <- function(x, n) {
  if (!is.numeric(x)) x <- suppressWarnings(as.numeric(x))
  if (any(is.na(x))) {
    # leave NAs as-is
  }
  factor <- 10^n
  trunc(x * factor) / factor
}

# Format to fixed decimals (optional, if you want stable textual precision)
format_fixed <- function(x, n) {
  formatC(x, format = "f", digits = n)
}

stop_if_not_exists <- function(path, what = "path") {
  if (!file.exists(path)) stop(sprintf("Cannot find %s: %s", what, path), call. = FALSE)
}

# -----------------------------
# Main
# -----------------------------
stop_if_not_exists(original_dir, "folder")
stop_if_not_exists(file.path(original_dir, metadata_file), "metadata CSV")

cat("Original data folder:", normalizePath(original_dir), "\n")
cat("Metadata file:", metadata_file, "\n")
cat("Will create variants for decimals:", paste(decimals_to_make, collapse = ", "), "\n\n")

# Read metadata once
meta_path <- file.path(original_dir, metadata_file)
meta <- read.csv(meta_path, check.names = FALSE)

if (!all(c(lat_col, lon_col) %in% names(meta))) {
  stop(sprintf(
    "Missing expected columns in metadata. Need: '%s' and '%s'. Found: %s",
    lat_col, lon_col, paste(names(meta), collapse = ", ")
  ), call. = FALSE)
}

# Preserve originals for audit
orig_lat <- meta[[lat_col]]
orig_lon <- meta[[lon_col]]

# Create each precision variant
for (n in decimals_to_make) {
  out_dir <- paste0(n, "_decimals")
  cat("----\nCreating:", out_dir, "\n")
  
  # Create folder (cleanly)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Copy everything from original_data into out_dir
  # (overwrites existing files if already present)
  files_to_copy <- list.files(original_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  # Recreate directory structure
  rel_paths <- substring(files_to_copy, nchar(original_dir) + 2)
  dir_paths <- unique(dirname(rel_paths))
  dir_paths <- dir_paths[dir_paths != "."]
  for (d in dir_paths) dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
  
  # Copy files
  file.copy(from = files_to_copy, to = file.path(out_dir, rel_paths), overwrite = TRUE, recursive = FALSE)
  
  # Load the copied metadata (so we only modify inside variant folder)
  variant_meta_path <- file.path(out_dir, metadata_file)
  stop_if_not_exists(variant_meta_path, "copied metadata CSV")
  
  m <- read.csv(variant_meta_path, check.names = FALSE)
  
  # Truncate
  new_lat <- truncate_decimals(m[[lat_col]], n)
  new_lon <- truncate_decimals(m[[lon_col]], n)
  
  if (write_as_character) {
    m[[lat_col]] <- format_fixed(new_lat, n)
    m[[lon_col]] <- format_fixed(new_lon, n)
  } else {
    m[[lat_col]] <- new_lat
    m[[lon_col]] <- new_lon
  }
  
  # Write back
  write.csv(m, variant_meta_path, row.names = FALSE)
  
  # Optional audit file
  if (write_audit_file) {
    audit <- data.frame(
      latitude_original = orig_lat,
      longitude_original = orig_lon,
      latitude_truncated = if (write_as_character) format_fixed(truncate_decimals(orig_lat, n), n) else truncate_decimals(orig_lat, n),
      longitude_truncated = if (write_as_character) format_fixed(truncate_decimals(orig_lon, n), n) else truncate_decimals(orig_lon, n)
    )
    write.csv(audit, file.path(out_dir, paste0("coord_audit_", n, "_decimals.csv")), row.names = FALSE)
  }
  
  cat("Done:", out_dir, "\n")
}

cat("\nAll variants created.\n")
cat("Next step: run your analysis app against each folder and compare outputs.\n")
