#' A function to read EventMeasure format point data (_Points.txt)
#'
#' @param dir  The directory where the .txt files are saved
#'
#' @return A data frame which contains all Point annotations, with a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_points <- function(dir) {
  
  read_dat <- function(flnm){
    read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Points.txt" = ""))) %>%
    dplyr::rename(sample = opcode) # TODO fix this if the sample is not defined by opcode
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "_Points.txt",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}