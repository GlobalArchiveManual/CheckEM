find_lw <- function(sp, mirror = "us") {
  
  # check_name_fishbase(sp)
  
  sp_ <- gsub(" ", "-", sp)
  url <- paste("https://www.fishbase.", mirror, "/summary/", sp_, ".html", sep = "")
  
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(url)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) { 
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  
  # ready to read page
  page <-  readLines(url)
  
  # print(page)
  
  l <- grep("Bayesian length-weight:", page)
  line <- page[l]
  line <- gsub("\t", "", line)
  line <- gsub("Bayesian length-weight: ", "", line)
  line <- gsub("(Ref. <A href='../references/FBRefSummary.php?ID=93245'>93245</A>)", "", line)
  
  metric <- gsub(", in cm total length, ", "", line)
  
  ob <- strsplit(line, " ")
  # print(ob)
  
  try(ob <- ob[[1]])
  
  # print(ob)
  
  lwa_m <- as.numeric(gsub("a=", "", ob[1]))
  lwa_up <- as.numeric(gsub("),", "", ob[4]))
  lwa_sd <- (lwa_up - lwa_m) / 1.96
  
  lwb_m <- as.numeric(gsub("b=", "", ob[5]))
  lwb_up <- as.numeric(gsub("),", "", ob[8]))
  lwb_sd <- (lwb_up - lwb_m) / 1.96
  
  try(data.frame(species = sp, lwa_m = lwa_m,
             lwa_sd = lwa_sd, lwb_m = lwb_m,
             lwb_sd = lwb_sd, metric = metric))
}
