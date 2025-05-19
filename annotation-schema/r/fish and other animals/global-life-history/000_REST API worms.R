library(httr2)
library(jsonlite)
library(future.apply)

num_cores <- parallel::detectCores() - 1
plan(multisession, workers = num_cores)  # Parallel execution

base_url <- "https://www.marinespecies.org/rest/AphiaChildrenByAphiaID/"

# Cache to avoid redundant API calls
cache <- list()

# Function to get children AphiaIDs with error handling
get_children_aphiaIDs <- function(aphia_id) {
  if (aphia_id %in% names(cache)) {
    return(cache[[aphia_id]])  # Use cached result
  }
  
  url <- paste0(base_url, aphia_id)
  cat(sprintf("📡 Fetching: %s\n", url))
  
  # Try API request with error handling
  response <- tryCatch({
    req <- request(url)
    resp <- req_perform(req)
    
    # Check if response is valid
    if (resp_status(resp) != 200) {
      cat(sprintf("❌ API error for AphiaID %d: HTTP %d\n", aphia_id, resp_status(resp)))
      return(NULL)
    }
    
    # Parse JSON response
    resp_body_json(resp, simplifyVector = TRUE)
  }, error = function(e) {
    cat(sprintf("⚠️ Request failed for AphiaID %d: %s\n", aphia_id, e$message))
    return(NULL)
  })
  
  # Extract AphiaIDs
  if (is.null(response) || length(response) == 0) {
    cache[[aphia_id]] <- NULL
    return(NULL)
  }
  
  child_ids <- response$AphiaID
  cache[[aphia_id]] <- child_ids
  return(child_ids)
}

# Recursive function to get all AphiaIDs
get_all_aphiaIDs_rest <- function(aphia_ids, collected_ids = NULL, depth = 1) {
  collected_ids <- unique(c(collected_ids, aphia_ids))
  
  cat(sprintf("\n🌍 Depth %d | Processing %d AphiaIDs... (Total collected: %d)\n", 
              depth, length(aphia_ids), length(collected_ids)))
  flush.console()
  
  # Fetch data in parallel
  new_ids_list <- future_lapply(aphia_ids, get_children_aphiaIDs)
  
  # Combine and filter new IDs
  new_ids <- unique(unlist(new_ids_list))
  new_ids <- setdiff(new_ids, collected_ids)  # Remove already processed IDs
  
  if (length(new_ids) == 0) {
    cat("\n✅ No more children found. Stopping recursion.\n")
    return(collected_ids)
  }
  
  cat(sprintf("🔍 Found %d new AphiaIDs\n", length(new_ids)))
  
  # Recursively get more AphiaIDs
  collected_ids <- get_all_aphiaIDs_rest(new_ids, collected_ids, depth + 1)
  
  return(collected_ids)
}

# Start from Animalia (AphiaID = 2)
animalia_id <- 2
cat("\n🚀 Starting AphiaID collection from Animalia...\n")

all_aphia_ids <- get_all_aphiaIDs_rest(animalia_id)

# Save results
write.csv(data.frame(AphiaID = all_aphia_ids), "all_aphia_ids.csv", row.names = FALSE)
cat("\n🎉 AphiaID collection complete! Saved to 'all_aphia_ids.csv'.\n")
