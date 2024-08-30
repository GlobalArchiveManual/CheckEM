#' Read Period Data from EventMeasure files (_Period.txt)
#'
#' This function reads EventMeasure period data files (_Period.txt) from a specified directory 
#' and processes them into a single dataframe. The dataframe contains period annotations, including 
#' columns for the campaignID, sample, and other relevant information.
#'
#' @param dir The directory where the .txt files are saved.
#' @param method A character string indicating the method used ("BRUVs" or "DOVs"). Default is "BRUVs".
#' @param recursive Logical, whether to search for files recursively in subdirectories. Default is `FALSE`.
#'
#' @return A data frame containing all period annotations from the specified files, with standardised columns 
#' for campaign ID, sample, opcode, period, start time, end time, and whether there is an end time.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read period data from a directory using the BRUVs method
#' periods_data <- read_periods("path/to/directory")
#'
#' # To read period data from a directory recursively using the DOVs method
#' periods_data <- read_periods("path/to/directory", method = "DOVs", recursive = TRUE)
#' }
read_periods <- function(dir, method = "BRUVs", recursive = FALSE) {
  
  read_dat <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid, c("_Period.txt" = "",
                                                                        "_Period.TXT" = "")))
  }
  
  cols_to_add <- c(
    campaignid = NA_real_,
    sample = NA_real_,
    opcode = NA_real_,
    period = NA_real_,
    timestart = NA_real_,
    timeend = NA_real_,
    hasend = NA_real_)
  
  dat <- list.files(path = dir,      
                    recursive = recursive,
                    pattern = "_Period.txt|_Period.TXT",
                    full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind() 
  
  if(nrow(dat > 0)){
    
    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(dat)){
        
        dat <- dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) 
      }
    } 
    
    if(method %in% c("BRUVs")){
      
      dat <- dat %>%
        dplyr::mutate(sample = opcode)
      
    }
  }
  
  dat <- dat %>%
    tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
    dplyr::rename(time_start = timestart,
                  time_end = timeend,
                  has_end = hasend) %>%
    dplyr::mutate(time_start	= as.numeric(time_start)) %>%
    dplyr::mutate(time_end	= as.numeric(time_end)) %>%
    dplyr::mutate(campaignid = as.character(campaignid)) %>%
    dplyr::mutate(sample = as.character(sample))
  
  return(dat)
  
}