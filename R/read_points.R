#' A function to read EventMeasure format point data (_Points.txt)
#'
#' @param dir  The directory where the .txt files are saved
#'
#' @return A data frame which contains all Point annotations, with a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_points <- function(dir, recursive = FALSE, method = "BRUVs") {
  
  read_dat <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Points.txt" = ""))) %>%
    dplyr::rename(sample = opcode) # TODO fix this if the sample is not defined by opcode
  }

  # lookup <- c(length_mm = "length")
  
  dat <- list.files(path = dir,      
                    recursive = recursive,
                    pattern = "_Points.txt|_Points.TXT",
                    full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind() #%>%
    #dplyr::rename(any_of(lookup))
  
  
  
  if(nrow(dat > 0)){
    
    if(method %in% "DOVs"){
      
      dat <- dat %>%
        dplyr::mutate(sample = paste(opcode, period, sep = "-")) #%>%
      #dplyr::select(-c(opcode, period))
      
    } else {
      
      dat <- dat %>%
        dplyr::mutate(sample = opcode)
      
    }
  }
  
  cols_to_add <- c(
    campaignid = NA_real_,
    sample = NA_real_,
    # length_mm = NA_real_,
    family = NA_real_,
    genus = NA_real_,
    species = NA_real_,
    number = NA_real_,
    period = NA_real_#,
    # rms = NA_real_,
    # range = NA_real_,
    # precision = NA_real_,
    # x = NA_real_,
    # y = NA_real_,
    # z = NA_real_,
    # midx = NA_real_,
    # midy = NA_real_,
    # midz = NA_real_
  )
  
  dat <- dat %>%
    tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
    dplyr::mutate(campaignid = as.character(campaignid)) %>%
    dplyr::mutate(sample = as.character(sample)) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(family = as.character(family)) %>%
    dplyr::mutate(genus = as.character(genus)) %>%
    dplyr::mutate(species = as.character(species)) %>%
    dplyr::mutate(period = as.character(period))
  
  return(dat)
  
  
  
  
}