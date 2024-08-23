#' Retrieve Bayesian Length-Weight Equation from FishBase
#'
#' This function retrieves the Bayesian length-weight equation parameters for a given species
#' from FishBase. It fetches data from the FishBase website and parses the Bayesian length-weight
#' parameters including the mean (m), standard deviation (sd), and the metric used.
#'
#' @param sp A character string representing the species name. The species name should be provided 
#' as it appears on FishBase, but spaces will be replaced with hyphens.
#' @param mirror A character string specifying the FishBase mirror site to use. Defaults to "us" 
#' (FishBase USA). Other mirrors can be specified if needed.
#'
#' @return A data frame containing the Bayesian length-weight parameters for the species. The data frame 
#' includes the following columns:
#' \itemize{
#'   \item \strong{species}: The species name.
#'   \item \strong{lwa_m}: The parameter for the length-weight equation (a).
#'   \item \strong{lwa_sd}: The standard deviation of the parameter for the length-weight equation (a).
#'   \item \strong{lwb_m}: The parameter for the length-weight equation (b).
#'   \item \strong{lwb_sd}: The standard deviation of the parameter for the length-weight equation (b).
#'   \item \strong{metric}: The metric used for length measurement (e.g., "based on LWR estimates for this species & (Sub)family-body").
#' }
#' If there is no internet connection or if the species page cannot be retrieved, the function returns 
#' `NULL`.
#' 
#' @export
#'
#' @examples
#' # Retrieve Bayesian length-weight parameters for a species
#' lw_params <- find_lw("Lutjanus campechanus")
#' print(lw_params)
#'
find_lw <- function(sp, mirror = "us") {
  
  # Replace spaces with hyphens for URL formatting
  sp_ <- gsub(" ", "-", sp)
  url <- paste("https://www.fishbase.", mirror, "/summary/", sp_, ".html", sep = "")
  
  # Function to handle GET requests with error and warning catching
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
  
  # Try fetching the page content
  resp <- try_GET(url)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Stop if status > 400
  if (httr::http_error(resp)) { 
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  
  # Read page content
  page <-  readLines(url)

  l <- grep("Bayesian length-weight:", page)
  line <- page[l]
  line <- gsub("\t", "", line)
  line <- gsub("Bayesian length-weight: ", "", line)
  line <- gsub("(Ref. <A href='../references/FBRefSummary.php?ID=93245'>93245</A>)", "", line)
  
  metric <- gsub(", in cm total length, ", "", line)
  
  ob <- strsplit(line, " ")
  try(ob <- ob[[1]])
  
  # Extract parameters
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
