# ─────────────────────────────────────────────────────
# Config: Set to "laptop" or "desktop" on each machine
# ─────────────────────────────────────────────────────
mode <- "desktop"  # ← Change to "laptop" or "desktop"

# Load packages
library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(progressr)
library(tidyr)
library(furrr)
library(sf)

log_file <- "scraper-log.txt"

log_message <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  write(msg, file = log_file, append = TRUE)
}


# -------------------------------------------------------------------
# Helper: safely extract the next sibling’s text from an <h4> element
# -------------------------------------------------------------------
next_text <- function(node) {
  if (length(node) == 0) return(NA_character_)
  sib <- xml2::xml_find_first(node, "following-sibling::*[1]")
  if (is.na(sib)) return(NA_character_)
  sib %>% html_text2() %>% str_squish()
}

# -------------------------------------------------------------------
# Main function: get IBRA and IMCRA from AFD page by ID ----
# -------------------------------------------------------------------
get_regions <- function(id, pause = 0.5) {
  url <- sprintf("https://biodiversity.org.au/afd/taxa/%s", id)
  Sys.sleep(pause)
  
  page <- tryCatch(read_html(url, timeout = 20), error = function(e) NA)
  if (is.na(page)[1]) {
    return(tibble(id = id, ibra = NA_character_, imcra = NA_character_))
  }
  
  ibra_node  <- html_node(page,
                          xpath = "//h4[translate(normalize-space(.),'ibra','IBRA')='IBRA']")
  imcra_node <- html_node(page,
                          xpath = "//h4[translate(normalize-space(.),'imcra','IMCRA')='IMCRA']")
  
  ibra  <- next_text(ibra_node)
  imcra <- next_text(imcra_node)
  
  tibble(id = id, ibra = na_if(ibra, ""), imcra = na_if(imcra, ""))
}

# Safe version
safe_get <- function(id) {
  log_message("👷 Worker PID:", Sys.getpid(), "- starting ID:", id)
  
  tryCatch(
    get_regions(id, pause = 0.1),
    error = function(e) {
      log_message("❌ Error for ID:", id, "|", conditionMessage(e))
      tibble(id = id, ibra = NA_character_, imcra = NA_character_)
    }
  )
}


# -------------------------------------------------------------------
# Prep your input/output ----
# -------------------------------------------------------------------
output_file <- "annotation-schema/data/staging/australian-faunal-directory-imcra.csv"

# Load all IDs to process
all_ids <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS")$caab_code

# Create output file if it doesn’t exist
if (!file.exists(output_file)) {
  write_csv(tibble(id = character(), ibra = character(), imcra = character()), output_file)
}

# Load already completed rows
done_ids <- (read_csv(output_file, show_col_types = FALSE)%>%
  dplyr::filter(!imcra %in% NA))$id
todo_ids <- setdiff(all_ids, done_ids)

cat("✅ Already done:", length(done_ids), "\n")
cat("🔁 Still to do:", length(todo_ids), "\n")

output_file <- sprintf("annotation-schema/data/staging/australian-faunal-directory-imcra-%s.csv", mode)

if (mode == "desktop") {
  all_ids <- rev(all_ids)
}

# -------------------------------------------------------------------
# Scrape and append to CSV row-by-row ----
# -------------------------------------------------------------------

## THIS WORKS - but slow
# handlers(global = TRUE)
# 
# with_progress({
#   p <- progressor(along = todo_ids)
# 
#   for (id in todo_ids) {
#     result <- tryCatch(get_regions(id),
#                        error = function(e) tibble(id = id, ibra = NA, imcra = NA))
#     write_csv(result, output_file, append = TRUE)
#     p()
#   }
# })

#######################################################
# -------------------------------
# Parallel scraping setup
# -------------------------------
plan(multisession, workers = 12)  # Adjust workers based on your CPU

chunk_size <- 100
chunk_ids  <- split(todo_ids, ceiling(seq_along(todo_ids) / chunk_size))

# -------------------------------
# Scrape and append in chunks
# -------------------------------
handlers(global = TRUE)
with_progress({
  p <- progressor(along = chunk_ids)
  
  for (i in seq_along(chunk_ids)) {
    ids <- chunk_ids[[i]]
    log_message("🔁 Starting chunk", i, "with", length(ids), "IDs")
    
    res_chunk <- future_map_dfr(
      ids, safe_get,
      .options  = furrr_options(packages = c("rvest", "xml2", "stringr")),
      .progress = TRUE
    )
    
    log_message("✅ Finished chunk", i, "- Writing to CSV")
    
    first_write <- !file.exists(output_file) || file.size(output_file) == 0
    write_csv(res_chunk, output_file,
              append    = !first_write,
              col_names =  first_write)
    
    log_message("💾 CSV write complete for chunk", i)
    p()
  }
  
  log_message("🏁 All chunks complete!")
})

# ---------------------------------------------------------------------------

# -------------------------------------------------------------------
# After scraping: extract IMCRA names and codes cleanly ----
# -------------------------------------------------------------------
results <- read_csv(output_file, show_col_types = FALSE)

# Use regex to handle names with inner parentheses
pattern <- "\\s*([^,]+?)\\s*\\((\\d+)\\)"

results <- results %>%
  dplyr::filter(!imcra %in% NA) %>%
  mutate(
    imcra_match = str_match_all(imcra, pattern),
    imcra_names = map(imcra_match, ~ str_trim(.x[, 2])),
    imcra_codes = map(imcra_match, ~ .x[, 3])
  ) %>%
  select(-imcra_match)

# -------------------------------------------------------------------
# Optional: save one-row-per-region format ----
# -------------------------------------------------------------------
results_long <- results %>%
  filter(lengths(imcra_names) == lengths(imcra_codes)) %>%
  unnest(c(imcra_names, imcra_codes)) %>%
  dplyr::select(id, imcra_names, imcra_codes) %>%
  dplyr::filter(!is.na(imcra_names)) %>%
  dplyr::rename(imcra_region = imcra_names)

# # Need to convert these IMCRA regions to Aus Marine Regions ---------
# # Spatial files ----
# wgs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# aus_regions <- st_read("annotation-schema/data/spatial/marine_regions_with_gbr.shp")
# 
# aus_regions$region <- as.character(aus_regions$FULL_NAME)
# st_crs(aus_regions) <- wgs_84
# 
# imcra_regions <- st_read("annotation-schema/data/spatial/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.shp")
# 
# imcra_regions$region <- as.character(imcra_regions$PB_NAME)
# st_crs(imcra_regions) <- wgs_84
# 
# # Intersection ----
# aus_regions <- st_as_sf(aus_regions) %>%
#   dplyr::rename(marine_region = region)
# 
# imcra_regions <- st_as_sf(imcra_regions) %>%
#   dplyr::rename(imcra_region = region)
# 
# imcra_with_aus_regions <- data.frame()
# 
# # # Takes 30 minutes to run ----
# for (OBJECTID in unique(imcra_regions$OBJECTID)) {
#   
#   polygons.to.test <- imcra_regions %>% 
#     filter(OBJECTID == OBJECTID) 
#   
#   dat <- st_intersection(polygons.to.test, aus_regions) %>%
#     st_set_geometry(NULL)#%>%
#     # dplyr::distinct(region) %>%
#     # dplyr::summarise(marine.region = toString(region)) %>%
#     # dplyr::mutate(spcode = CAAB)
#   
#   imcra_with_aus_regions <- bind_rows(imcra_with_aus_regions, dat) %>%
#     distinct(imcra_region, marine_region)
#   
# }
# 
# saveRDS(imcra_with_aus_regions, "annotation-schema/data/staging/imcra_with_aus_regions.RDS")
imcra_with_aus_regions <- readRDS("annotation-schema/data/staging/imcra_with_aus_regions.RDS") 

# Join to the results ----
caabs_with_all_regions <- left_join(results_long, imcra_with_aus_regions) %>%
  dplyr::rename(caab_code = id)
# 
# all_animals <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS") %>%
#   distinct(caab_code, family, genus, species)

extra_imcras <- caabs_with_all_regions %>%
  distinct(caab_code, imcra_region)

extra_aus <- caabs_with_all_regions %>%
  distinct(caab_code, marine_region) %>%
  dplyr::filter(!is.na(marine_region))

# Save files -----
write_csv(extra_imcras, "annotation-schema/data/staging/australian-fanual-directory_distributions_extra_imcras.csv")
write_csv(extra_aus, "annotation-schema/data/staging/australian-fanual-directory_distributions_extra_aus.csv")

cat("✅ Scraping complete!\n")
