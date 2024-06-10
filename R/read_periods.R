#' A function to read EventMeasure format period data (_Period.txt)
#'
#' @param dir  The directory where the .txt files are saved
#'
#' @return A data frame which contains all Period annotations, with a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_periods <- function(dir, method = "BRUVs") {
  
  read_dat <- function(flnm){
    read_tsv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Period.txt" = ""))) #%>%
    #dplyr::rename(sample = opcode) # TODO fix this if the sample is not defined by opcode
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
                    recursive = F,
                    pattern = "_Period.txt",
                    full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind() 
  
  if(nrow(dat > 0)){
    
    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        dat <- dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) #%>%
          #dplyr::select(-c(opcode, period))
        
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