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
             full.names = T) 

  dat <- data.frame()
  
  for(file in unique(files)){
    
    message(paste("reading length file:", file))
    
    temp_dat <- read_dat(file) %>%
      clean_names() %>%
      dplyr::glimpse()
    
    # TODO add BRUVs
    
    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
          dplyr::select(-c(opcode, period))
        
      }
      
    }
    
    dat <- bind_rows(dat, temp_dat)
    
  }
  
  return(dat)
  
}