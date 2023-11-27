#' Title
#'
#' @param flnm 
#'
#' @return
#' @export
#'
#' @examples
read_files_txt <- function(dir) {
  
  read_dat <- function(flnm){
    read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Points.txt" = ""))) %>%
    dplyr::rename(sample = opcode)
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "_Points.txt",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}