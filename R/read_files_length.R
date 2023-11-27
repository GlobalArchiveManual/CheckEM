#' A function to read EventMeasure format length data (_3DPoints.txt & _Lengths.txt)
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_files_length <- function(dir) {
  
  read_dat <- function(flnm){
    read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_3DPoints.txt" = "",
                                                            "_Lengths.txt" = ""))) %>%
    dplyr::rename(sample = opcode)
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "_Lengths.txt|_3DPoints",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}