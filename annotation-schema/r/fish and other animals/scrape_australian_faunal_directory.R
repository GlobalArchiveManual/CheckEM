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
# Main function: get IBRA and IMCRA from AFD page by ID
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

# -------------------------------------------------------------------
# Prep your input/output
# -------------------------------------------------------------------
output_file <- "annotation-schema/data/staging/australian-faunal-directory-imcra.csv"

# Load all IDs to process
all_ids <- readRDS("annotation-schema/output/fish/schema/australia_life-history.RDS")$caab_code

# Create output file if it doesn’t exist
if (!file.exists(output_file)) {
  write_csv(tibble(id = character(), ibra = character(), imcra = character()), output_file)
}

# Load already completed rows
done_ids <- read_csv(output_file, show_col_types = FALSE)$id
todo_ids <- setdiff(all_ids, done_ids)

cat("✅ Already done:", length(done_ids), "\n")
cat("🔁 Still to do:", length(todo_ids), "\n")

# -------------------------------------------------------------------
# Scrape and append to CSV row-by-row
# -------------------------------------------------------------------
# ----- Parallel scraper with checkpointing ----------------------------------

plan(multisession, workers = 4)   # tweak workers to suit your CPU/RAM

safe_get <- purrr::possibly(
  \(id) get_regions(id, pause = 0.5),
  otherwise = tibble(id = id, ibra = NA_character_, imcra = NA_character_)
)

chunk_size <- 200                      # write every 200 species
chunk_ids  <- split(todo_ids, ceiling(seq_along(todo_ids) / chunk_size))

handlers(global = TRUE)
with_progress({
  p <- progressor(along = chunk_ids)
  
  for (ids in chunk_ids) {
    res_chunk <- future_map_dfr(ids, safe_get, .progress = FALSE)
    write_csv(res_chunk, output_file )
    p()
  }
})
# ---------------------------------------------------------------------------

# -------------------------------------------------------------------
# After scraping: extract IMCRA names and codes cleanly
# -------------------------------------------------------------------
results <- read_csv(output_file, show_col_types = FALSE)

# Use regex to handle names with inner parentheses
pattern <- "\\s*([^,]+?)\\s*\\((\\d+)\\)"

results <- results %>%
  mutate(
    imcra_match = str_match_all(imcra, pattern),
    imcra_names = map(imcra_match, ~ str_trim(.x[, 2])),
    imcra_codes = map(imcra_match, ~ .x[, 3])
  ) %>%
  select(-imcra_match)

# -------------------------------------------------------------------
# Optional: save one-row-per-region format
# -------------------------------------------------------------------
results_long <- results %>%
  filter(lengths(imcra_names) == lengths(imcra_codes)) %>%
  unnest(c(imcra_names, imcra_codes)) %>%
  dplyr::select(id, imcra_names, imcra_codes) %>%
  dplyr::filter(!is.na(imcra_names))

write_csv(results_long, "annotation-schema/data/staging/australian-fanual-directory_distributions_long.csv")

cat("✅ Scraping complete!\n")
