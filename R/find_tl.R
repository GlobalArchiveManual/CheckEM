#' Retrieve Trophic Level and Standard Error from FishBase
#'
#' This function retrieves the trophic level and its standard error for a given species from FishBase. 
#' It fetches data from the FishBase website, extracts the trophic level information, and formats it
#' into a data frame.
#'
#' @param sp A character string representing the species name. The species name should be provided
#' as it appears on FishBase, but spaces will be replaced with hyphens.
#' @param mirror A character string specifying the FishBase mirror site to use. Defaults to "us" 
#' (FishBase USA). Other mirrors can be specified if needed.
#'
#' @return A data frame containing the trophic level information for the species. The data frame 
#' includes the following columns:
#' \itemize{
#'   \item \strong{species}: The species name.
#'   \item \strong{trophic_level}: The trophic level of the species.
#'   \item \strong{se}: The standard error of the trophic level.
#'   \item \strong{metric}: The source used (if provided) e.g. "based on food items".
#' }
#' If there is no internet connection, or if the species page cannot be retrieved or parsed, the function 
#' returns `NULL`.
#' 
#' @export
#' 
#' @import stringr
#'
#' @examples
#' # Retrieve trophic level and standard error for a species
#' tl_info <- find_tl("Lutjanus campechanus")
#' print(tl_info)
#'
find_tl <- function(sp, mirror = "us") {

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
  
  l <- grep("Trophic level", page)
  line <- page[l]
  line
  line <- gsub("\t", "", line)
  line <- gsub("Trophic level ", "", line)
  line <- gsub("(Ref. <a href='../manual/English/fishbasetrophic_ecology00002692.htm'>69278</a>)", "", line)
  line <- gsub("&nbsp;", "", line)
  line <- gsub("\\/", "", line)
  line <- gsub(".<div>", "", line)
  line <- gsub("\\():", "", line)
  line <- trimws(line)
  line
  
  try(value <- stringr::str_split(line, 'se; ', simplify = TRUE)[,1])
  
  try(level <- trimws(stringr::str_split(value, '&plusmn;', simplify = TRUE)[,1]))
  try(se <- trimws(stringr::str_split(value, '&plusmn;', simplify = TRUE)[,2]))
  
  try(metric <- stringr::str_split(line, 'se; ', simplify = TRUE)[,2])
  
  try(data.frame(species = sp, 
                 trophic_level = level,
                 se = se,
                 metric = metric))
}
