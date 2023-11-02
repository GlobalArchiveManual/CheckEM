#' A function to read .txt exports from SeaGIS TransectMeasure
#'
#' @param dir The directory where the .txt files are saved
#' @param sample Either character value to define if sample is recorded as "opcode" or "period"
#'
#' @return A dataframe with all a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_TM <- function(dir, sample) {
  if (sample %in% "opcode") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      # dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = opcode) %>%
      glimpse()
  }
  
  else if (sample %in% "period") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      # dplyr::mutate(id = 1:nrow(.)) %>%
      ga.clean.names() %>%
      dplyr::rename(sample = period) %>%
      glimpse()
  }
  
  else {
    stop("Sample must be one of: c('opcode', 'period')")
  }}