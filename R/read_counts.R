#' A function to read EventMeasure format point data (_Points.txt)
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_counts <- function(dir, method = "BRUVs") {
  
  read_dat_csv <- function(flnm){
    readr::read_csv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid,c("_Count.csv" = "", "_count.csv" = ""))) #%>%
      # dplyr::rename(sample = opcode)
  }
  
  read_dat_tsv <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid,c("_Count.csv" = "", "_count.csv" = ""))) #%>%
    # dplyr::rename(sample = opcode)
  }
  
  files <- list.files(path = dir,      
             recursive = F,
             pattern = "ount.csv|ount.txt",
             full.names = T)
  
  dat <- data.frame()
  
  for(file in unique(files)){
    
    message(paste("reading count file:", file))
    
    if(stringr::str_detect(file, ".csv")){
      
      message("file is a csv")
      
      temp_dat <- read_dat_csv(file) %>%
        CheckEM::clean_names() %>%
        dplyr::mutate(count = as.numeric(count))
      
    } else {
      
      message("file is a txt")
      
      temp_dat <- read_dat_tsv(file) %>%
        CheckEM::clean_names() %>%
        dplyr::mutate(count = as.numeric(count))
      
    }

    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
          dplyr::select(-c(opcode, period)) 
        
      }
      
    }
    
    
    if(method %in% c("BRUVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = opcode) 
        
      }
      
    }
  
    dat <- dplyr::bind_rows(dat, temp_dat) 
    
  }
  
  return(dat)
  
}
