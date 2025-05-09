#' Read "Generic" length data
#'
#' This function reads "Generic" length data files (either .csv or .txt) from a specified directory 
#' and processes them into a single data frame. The data frame contains information such as campaignID, sample, length (mm) and species information.
#'
#' @param dir The directory where the length data files are saved.
#' @param method A character string indicating the method used ("BRUVs" or "DOVs"). Default is "BRUVs".
#' @param recursive Logical, whether to search for files recursively in subdirectories. Default is `FALSE`.
#'
#' @return A data frame containing all length data from the specified files, with standardised column names and formats.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read length data from a directory using the BRUVs method
#' length_data <- read_gen_length("path/to/directory")
#'
#' # To read length data from a directory recursively using the DOVs method
#' length_data <- read_gen_length("path/to/directory", method = "DOVs", recursive = TRUE)
#' }
read_gen_length <- function(dir, method = "BRUVs", recursive = FALSE) {
  
  read_dat_csv <- function(flnm){
    readr::read_csv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid, 
                                                          c("_Length.csv" = "",
                                                            "_length.csv" = ""))) 
  }
  
  read_dat_tsv <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid,c("_Length.txt" = "",
                                                                       "_length.txt" = ""))) 
  }
  
  files <- list.files(path = dir,      
                      recursive = recursive,
                      pattern = "ength.csv|ength.txt",
                      full.names = T) 
  
  dat <- data.frame()
  
  lookup <- c(length_mm = "length")
  
  for(file in unique(files)){
    
    message(paste("reading length file:", file))
      
      if(stringr::str_detect(file, ".csv")){
        
        message("file is a csv")
        
        temp_dat <- read_dat_csv(file) %>%
          CheckEM::clean_names() %>%
          dplyr::rename(any_of(lookup)) 
        
      } else {
        
        message("file is a txt")
        
        temp_dat <- read_dat_tsv(file) %>%
          CheckEM::clean_names() %>%
          dplyr::rename(any_of(lookup)) 
        
      }
    
    if(nrow(temp_dat > 0)){
      
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
      
      if("count" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(number = count)
        
      }
    }
    
    dat <- dplyr::bind_rows(dat, temp_dat)
    
  }
  
  cols_to_add <- c(
    campaignid = NA_real_,
    sample = NA_real_,
    length_mm = NA_real_,
    family = NA_real_,
    genus = NA_real_,
    species = NA_real_,
    number = NA_real_,
    range = NA_real_,
    rms = NA_real_,
    precision = NA_real_)
  
  dat <- dat %>%
    tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
    dplyr::mutate(campaignid = as.character(campaignid)) %>%
    dplyr::mutate(sample = as.character(sample))%>%
    dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(rms = as.numeric(rms)) %>%
    dplyr::mutate(range = as.numeric(range)) %>%
    dplyr::mutate(precision = as.numeric(precision)) %>%
    dplyr::mutate(family = as.character(family)) %>%
    dplyr::mutate(genus = as.character(genus)) %>%
    dplyr::mutate(species = as.character(species))
  
  return(dat)
  
}
