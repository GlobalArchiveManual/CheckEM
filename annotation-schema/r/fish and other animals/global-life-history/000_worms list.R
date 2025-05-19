library(worrms)
library(parallel)
library(memoise)

# Memoize wm_children to avoid redundant API calls
wm_children_cached <- memoise(wm_children)

# Detect available cores (Windows uses clusters)
num_cores <- detectCores() - 1  # Use all but one core

# Function to recursively fetch all AphiaIDs in parallel (Windows-compatible)
get_all_aphiaIDs_parallel <- function(aphia_ids, collected_ids = NULL, depth = 1) {
  collected_ids <- unique(c(collected_ids, aphia_ids))
  
  # Print progress message
  cat(sprintf("\n[Depth %d] Processing %d AphiaIDs... (Total collected: %d)\n", 
              depth, length(aphia_ids), length(collected_ids)))
  flush.console()  # Ensure immediate output in R console
  
  # Set up parallel cluster for Windows
  cl <- makeCluster(num_cores)
  clusterExport(cl, varlist = c("wm_children_cached"), envir = environment())
  
  # Get children in parallel with memoized function
  children <- parLapply(cl, aphia_ids, function(id) {
    tryCatch(wm_children_cached(id), error = function(e) NULL)
  })
  
  stopCluster(cl)  # Shut down cluster
  
  # Combine results
  children <- do.call(rbind, children)
  
  # Stop if no new children found
  if (is.null(children) || nrow(children) == 0) {
    cat("\n✅ No more children found. Stopping recursion.\n")
    return(collected_ids)
  }
  
  # Get new AphiaIDs that haven't been processed
  new_ids <- setdiff(children$AphiaID, collected_ids)
  
  # Print status update
  cat(sprintf("🔍 Found %d new AphiaIDs\n", length(new_ids)))
  
  if (length(new_ids) > 0) {
    collected_ids <- get_all_aphiaIDs_parallel(new_ids, collected_ids, depth + 1)
  }
  
  return(collected_ids)
}

# Start from Animalia (or any other taxon)
animalia_id <- wm_records_name(name = "Animalia")$AphiaID[1]
cat(sprintf("\n🚀 Starting AphiaID collection from Animalia (ID: %d)\n", animalia_id))

all_aphia_ids <- get_all_aphiaIDs_parallel(animalia_id)

# Save results to CSV
write.csv(data.frame(AphiaID = all_aphia_ids), "all_aphia_ids.csv", row.names = FALSE)

cat("\n🎉 AphiaID collection complete! Saved to 'all_aphia_ids.csv'.\n")
