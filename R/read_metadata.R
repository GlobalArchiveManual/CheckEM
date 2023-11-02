#' A function to read GlobalArchive format metadata
#'
#' @param dir The directory where the .txt files are saved
#'
#' @return A dataframe with all a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_metadata <- function(dir) {
  
  read_dat <- function(flnm) {
    read_csv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid, c("_Metadata.csv" = "")))
  }
  
  list.files(path = dir,      
             recursive = F,
             pattern = "_Metadata.csv",
             full.names = T) %>%
    purrr::map(~read_dat(.)) %>%
    purrr::list_rbind()
}
