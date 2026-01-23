#' Read Point Data from EventMeasure files (_Points.txt)
#'
#' This function reads EventMeasure point data files (_Points.txt) from a specified directory 
#' and processes them into a single data frame. The data frame contains point annotations, including 
#' columns for the campaignID, sample, and other relevant information.
#'
#' @param dir The directory where the .txt files are saved.
#' @param recursive Logical, whether to search for files recursively in subdirectories. Default is `FALSE`.
#' @param method A character string indicating the method used ("BRUVs" or "DOVs"). Default is "BRUVs".
#'
#' @return A data frame containing all point annotations from the specified files, with standardised columns 
#' for campaign ID, sample, family, genus, species, number, and period.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read point data from a directory using the BRUVs method
#' points_data <- read_points("path/to/directory")
#'
#' # To read point data from a directory recursively using the DOVs method
#' points_data <- read_points("path/to/directory", recursive = TRUE, method = "DOVs")
#' }
read_points <- function(dir, recursive = FALSE, method = "BRUVs") {
  
  read_dat <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(campaignid,c("_Points.txt" = "", 
                                                                     "_Points.TXT" = ""))) 
  }
  
  dat <- list.files(path = dir,      
                    recursive = recursive,
                    pattern = "_Points.txt|_Points.TXT",
                    full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind() 
  
  if(nrow(dat > 0)){
    
    if(method %in% "DOVs"){
      
      dat <- dat %>%
        dplyr::mutate(sample = paste(opcode, period, sep = "-")) 
      
    } else {
      
      dat <- dat %>%
        dplyr::mutate(sample = opcode)
      
    }
  }
  
  cols_to_add <- c(
    campaignid = NA_real_,
    sample = NA_real_,
    family = NA_real_,
    genus = NA_real_,
    species = NA_real_,
    number = NA_real_,
    period = NA_real_
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