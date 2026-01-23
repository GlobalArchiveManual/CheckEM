#' Read EventMeasure length and 3D point data (_3DPoints.txt & _Lengths.txt)
#'
#' This function reads length and 3D point information from EventMeasure export files (`_3DPoints.txt` and `_Lengths.txt`) from a specified directory 
#' and processes them into a single dataframe. The dataframe includes details such as length, species, sample, and additional metadata.
#'
#' @param dir The directory where the EventMeasure length data files are located.
#' @param method A character string indicating the method used ("BRUVs" or "DOVs"). Default is "BRUVs".
#' @param recursive Logical, whether to search for files recursively in subdirectories. Default is `FALSE`.
#'
#' @return A data frame containing all length data from the specified files, with standardised column names and formats.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read EventMeasure length data from a directory using the BRUVs method
#' length_data <- read_em_length("path/to/directory")
#'
#' # To read length data from a directory recursively using the DOVs method
#' length_data <- read_em_length("path/to/directory", method = "DOVs", recursive = TRUE)
#' }
read_em_length <- function(dir, method = "BRUVs", recursive = FALSE) {
  
  read_dat <- function(flnm){
    readr::read_tsv(flnm, col_types =  readr::cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    # CheckEM::clean_names() %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(campaignid, 
                                                        c("_3DPoints.txt" = "",
                                                          "_Lengths.txt" = "",
                                                          "_Lengths.TXT" = "",
                                                          "_3DPoints.TXT" = "")))
  }
  
  lookup <- c(length_mm = "length")
  
  dat <- list.files(path = dir,      
             recursive = recursive,
             pattern = "_Lengths.txt|_3DPoints.txt|_Lengths.TXT|_3DPoints.TXT",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind() %>%
    dplyr::rename(any_of(lookup))
  
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
    length_mm = NA_real_,
    family = NA_real_,
    genus = NA_real_,
    species = NA_real_,
    number = NA_real_,
    period = NA_real_,
    rms = NA_real_,
    range = NA_real_,
    precision = NA_real_,
    x = NA_real_,
    y = NA_real_,
    z = NA_real_,
    midx = NA_real_,
    midy = NA_real_,
    midz = NA_real_
    )
  
  dat <- dat %>%
    tibble::add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>%
    dplyr::mutate(campaignid = as.character(campaignid)) %>%
    dplyr::mutate(sample = as.character(sample)) %>%
    dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
    dplyr::mutate(rms = as.numeric(rms)) %>%
    dplyr::mutate(range = as.numeric(range)) %>%
    dplyr::mutate(precision = as.numeric(precision)) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(family = as.character(family)) %>%
    dplyr::mutate(genus = as.character(genus)) %>%
    dplyr::mutate(species = as.character(species)) %>%
    dplyr::mutate(period = as.character(period)) %>%
    dplyr::mutate(x = as.numeric(x)) %>%
    dplyr::mutate(y = as.numeric(y)) %>%
    dplyr::mutate(z = as.numeric(z)) %>%
    dplyr::mutate(midx = as.numeric(midx)) %>%
    dplyr::mutate(midy = as.numeric(midy)) %>%
    dplyr::mutate(midz = as.numeric(midz)) 
  
  return(dat)
  
}