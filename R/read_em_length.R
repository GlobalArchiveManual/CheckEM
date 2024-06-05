#' A function to read EventMeasure format length data (_3DPoints.txt & _Lengths.txt)
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_em_length <- function(dir, method = "BRUVs") {
  
  read_dat <- function(flnm){
    read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_3DPoints.txt" = "",
                                                            "_Lengths.txt" = "")))
  }
  
  dat <- list.files(path = dir,      
             recursive = F,
             pattern = "_Lengths.txt|_3DPoints",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
  
  if(method %in% "DOVs"){
    
    dat <- dat %>%
      dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
      dplyr::select(-c(opcode, period))
    
  } else {
    
    dat <- dat %>%
      dplyr::mutate(sample = opcode)
    
  }
  
  
  return(dat)
  
}