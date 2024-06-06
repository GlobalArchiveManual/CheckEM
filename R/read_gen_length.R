#' A function to read "Generic" format length data _Length.csv
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
read_gen_length <- function(dir, method = "BRUVs") {
  
  read_dat <- function(flnm){
    read_csv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Length.csv" = "",
                                                              "_length.csv" = ""))) #%>%
      #dplyr::rename(sample = opcode)
  }
  
  files <- list.files(path = dir,      
             recursive = F,
             pattern = "ength.csv",
             full.names = T) 

  dat <- data.frame()
  
  lookup <- c(length_mm = "length")
  
  for(file in unique(files)){
    
    message(paste("reading length file:", file))
    
    temp_dat <- read_dat(file) %>%
      clean_names() %>%
      dplyr::rename(any_of(lookup)) %>%
      dplyr::glimpse()
    
    # TODO add BRUVs
    
    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
          dplyr::select(-c(opcode, period))
        
      }
      
    } 
    
    if(method %in% c("BRUVs")){
      
      temp_dat <- temp_dat %>%
        dplyr::mutate(sample = opcode)
      
    }
    
    if("count" %in% names(dat)){
      
      dat <- dat %>%
        dplyr::mutate(number = count)
      
    }
    
    dat <- bind_rows(dat, temp_dat)
  }
    

    
  
  return(dat)
  
}
