#' A function to read "Generic" format length data _Length.csv
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_gen_length <- function(dir) {
  
  read_dat <- function(flnm){
    read_csv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Length.csv" = "",
                                                              "_length.csv" = ""))) %>%
      dplyr::rename(sample = opcode)
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "ength.csv",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}