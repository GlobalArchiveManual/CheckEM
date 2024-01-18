#' A function to read EventMeasure format point data (_Points.txt)
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_counts <- function(dir) {
  
  read_dat <- function(flnm){
    read_csv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Count.csv" = ""))) %>%
      dplyr::rename(sample = opcode)
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "_Count.csv",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}