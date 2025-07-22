# ─────────────────────────────────────────────────────
# Config: Set to "laptop" or "desktop" on each machine
# ─────────────────────────────────────────────────────
mode <- "desktop"  # ← Change to "laptop" or "desktop"

# ─────────────────────────────────────
# Load packages
# ─────────────────────────────────────
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

# ─────────────────────────────────────
# Setup paths
# ─────────────────────────────────────
log_file <- sprintf("scraper-log-%s.txt", mode)
output_file <- sprintf("annotation-schema/data/staging/australian-faunal-directory-imcra-%s.csv", mode)

# ─────────────────────────────────────
# Logging function
# ─────────────────────────────────────
log_message <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  write(msg, file = log_file, append = TRUE)
}

# ─────────────────────────────────────
# Helper to extract next sibling text
# ─────────────────────────────────────
next_text <- function(node) {
  if (length(node) == 0) return(NA_character_)
  sib <- xml2::xml_find_first(node, "following-sibling::*[1]")
  if (is.na(sib)) return(NA_character_)
  sib %>% html_text2() %>% str_squish()
}

# ─────────────────────────────────────
# Scraping function
# ─────────────────────────────────────
get_regions <- function(id, pause = 0.1) {
  url <- sprintf("https://biodiversity.org.au/afd/taxa/%s", id)
  Sys.sleep(pause)
  
  page <- tryCatch(read_html(url, timeout = 20), error = function(e) NA)
  if (is.na(page)[1]) {
    return(tibble(id = id, ibra = NA_character_, imcra = NA_character_))
  }
  
  ibra_node  <- html_node(page, xpath = "//h4[translate(normalize-space(.),'ibra','IBRA')='IBRA']")
  imcra_node <- html_node(page, xpath = "//h4[translate(normalize-space(.),'imcra','IMCRA')='IMCRA']")
  
  ibra  <- next_text(ibra_node)
  imcra <- next_text(imcra_node)
  
  tibble(id = id, ibra = na_if(ibra, ""), imcra = na_if(imcra, ""))
}

# Safe wrapper
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

# ─────────────────────────────────────
# Load and split ID list
# ─────────────────────────────────────
all_ids <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS")$caab_code

if (mode == "desktop") {
  all_ids <- rev(all_ids)
}

if (!file.exists(output_file)) {
  write_csv(tibble(id = character(), ibra = character(), imcra = character()), output_file)
}

# Load local progress
done_ids_this <- if (file.exists(output_file)) {
  read_csv(output_file, show_col_types = FALSE)$id
} else {
  character()
}

# Load other machine’s progress
other_mode <- ifelse(mode == "laptop", "desktop", "laptop")
other_output <- sprintf("annotation-schema/data/staging/australian-faunal-directory-imcra-%s.csv", other_mode)
done_ids_other <- if (file.exists(other_output)) {
  read_csv(other_output, show_col_types = FALSE)$id
} else {
  character()
}

old_ids <- read_csv("annotation-schema/data/staging/australian-faunal-directory-imcra.csv", show_col_types = FALSE)$id

# Combine and filter
done_ids <- union(union(done_ids_this, done_ids_other), old_ids)
todo_ids <- setdiff(all_ids, done_ids)

cat("🔧 Mode:", mode, "\n")
cat("✅ Already done:", length(done_ids), "\n")
cat("🔁 Still to do:", length(todo_ids), "\n")

# ─────────────────────────────────────
# Parallel scraping config
# ─────────────────────────────────────
total_cores <- future::availableCores()
workers <- max(1, total_cores - 20)  # leave one core free
plan(multisession, workers = workers)

cat("💻 Using", workers, "of", total_cores, "available cores\n")

chunk_size <- 75
chunk_ids  <- split(todo_ids, ceiling(seq_along(todo_ids) / chunk_size))

# ─────────────────────────────────────
# Scraping loop
# ─────────────────────────────────────
handlers(global = TRUE)

with_progress({
  p <- progressor(along = chunk_ids)
  
  for (i in seq_along(chunk_ids)) {
    ids <- chunk_ids[[i]]
    log_message("🔁 Starting chunk", i, "with", length(ids), "IDs")
    
    res_chunk <- tryCatch({
      future_map_dfr(
        ids, safe_get,
        .options  = furrr_options(packages = c("rvest", "xml2", "stringr")),
        .progress = TRUE
      )
    }, error = function(e) {
      log_message("❌ Chunk error (chunk", i, "):", conditionMessage(e))
      tibble(id = ids, ibra = NA_character_, imcra = NA_character_)
    })
    
    tryCatch({
      first_write <- !file.exists(output_file) || file.size(output_file) == 0
      write_csv(res_chunk, output_file,
                append    = !first_write,
                col_names =  first_write)
      log_message("💾 CSV write complete for chunk", i)
    }, error = function(e) {
      log_message("❌ Write error (chunk", i, "):", conditionMessage(e))
    })
    
    p()
  }
  
  log_message("🏁 All chunks complete!")
})


# MERGE dataframes together ----
old <- read_csv("annotation-schema/data/staging/australian-faunal-directory-imcra-laptop.csv", show_col_types = FALSE)
laptop <- read_csv("annotation-schema/data/staging/australian-faunal-directory-imcra-laptop.csv", show_col_types = FALSE)
desktop <- read_csv("annotation-schema/data/staging/australian-faunal-directory-imcra-desktop.csv", show_col_types = FALSE)

combined <- bind_rows(laptop, desktop) %>%
  bind_rows(., old) %>%
  distinct(id, .keep_all = TRUE)

write_csv(combined, "annotation-schema/data/staging/australian-faunal-directory-imcra-final.csv")
