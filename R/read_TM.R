#' Read .txt Exports from SeaGIS TransectMeasure
#'
#' This function reads and processes .txt files exported from SeaGIS TransectMeasure located in the specified directory.
#' The files contain data from dot point measurements, which are read into a single dataframe with a column for the 
#' campaign ID and a specified sample type (either "opcode" or "period").
#'
#' @param dir The directory where the .txt files are saved.
#' @param sample A character string indicating the sample type; either "opcode" or "period".
#'
#' @return A dataframe containing combined data from all .txt files in the specified directory, including a column for the sample type.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read .txt files with sample recorded as "opcode"
#' data <- read_TM("path/to/directory", sample = "opcode")
#'
#' # To read .txt files with sample recorded as "period"
#' data <- read_TM("path/to/directory", sample = "period")
#' }
read_TM <- function(dir, sample) {
  if (sample %in% "opcode") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      clean_names() %>%
      dplyr::rename(sample = opcode)
  }
  
  else if (sample %in% "period") {
    list.files(path = dir,    
               recursive = F,
               pattern = "_Dot Point Measurements.txt",
               full.names = T) %>%
      purrr::map(~read.delim(., header = T, skip = 4, stringsAsFactors = FALSE, 
                             colClasses = "character", na.strings = "")) %>%
      purrr::list_rbind() %>%
      clean_names() %>%
      dplyr::rename(sample = period)
  }
  
  else {
    stop("Sample must be one of: c('opcode', 'period')")
  }}
