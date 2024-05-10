#' Find Bayesian length-weight equation
#'
#' @param sp 
#' @param mirror 
#'
#' @return
#' @export
#'
#' @examples
find_tl <- function(sp, mirror) {
  
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
  
  try(value <- str_split(line, 'se; ', simplify = TRUE)[,1])
  
  try(level <- trimws(str_split(value, '&plusmn;', simplify = TRUE)[,1]))
  try(se <- trimws(str_split(value, '&plusmn;', simplify = TRUE)[,2]))
  
  try(metric <- str_split(line, 'se; ', simplify = TRUE)[,2])
  
  try(data.frame(species = sp, 
                 trophic_level = level,
                 se = se,
                 metric = metric))
}