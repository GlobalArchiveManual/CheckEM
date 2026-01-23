#' Read Count Data from Generic Count Data (_Count.csv)
#'
#' This function reads and processes point count data from Generic files 
#' in either CSV or TXT format. It can handle both BRUVs (Baited Remote Underwater Video Systems) 
#' and DOVs (Diver Operated Video) methods, ensuring that the data is correctly cleaned and 
#' formatted for further analysis.
#'
#' @param dir A character string specifying the directory containing the count files.
#' @param method A character string specifying the method used: either \code{"BRUVs"} or \code{"DOVs"}. 
#' Defaults to \code{"BRUVs"}.
#' @param recursive A logical value indicating whether to read files recursively from the directory. 
#' Defaults to \code{FALSE}.
#'
#' @return A data frame containing the cleaned and formatted count data from the specified files.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read all count files from the directory for BRUVs method
#' counts_data <- read_counts(dir = "path/to/directory", method = "BRUVs", recursive = TRUE)
#' 
#' # Read all count files from the directory for DOVs method
#' counts_data <- read_counts(dir = "path/to/directory", method = "DOVs", recursive = TRUE)
#' }
read_counts <- function(dir, method = "BRUVs", recursive = FALSE) {
  
  read_dat_csv <- function(flnm){
    readr::read_csv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      # CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid, c("_Count.csv" = "", 
                                                                       "_count.csv" = ""))) #%>%
      # dplyr::rename(sample = opcode)
  }
  
  read_dat_tsv <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid,c("_Count.txt" = "", "_count.txt" = ""))) #%>%
    # dplyr::rename(sample = opcode)
  }
  
  files <- list.files(path = dir,      
             recursive = recursive,
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
