#' Title
#'
#' @param flnm 
#'
#' @return
#' @export
#'
#' @examples
read_files_txt <- function(flnm) {
  read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    clean_names() %>%
    dplyr::mutate(campaignid = str_replace_all(campaignid,c("_Points.txt"="","_3DPoints.txt"="","_Lengths.txt"=""))) %>%
    dplyr::rename(sample = opcode)
}