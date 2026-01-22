#' Read Metadata
#'
#' This function reads Metadata in a GlobalArchive format (as .csv files) from a specified directory 
#' and processes them into a single dataframe. The dataframe includes campaignID, sample, depth_m, time, coordinates and other important information collected when deploying and annotating stere-video imagery.
#'
#' @param dir The directory where the .csv files are saved.
#' @param method A character string indicating the method used ("BRUVs" or "DOVs"). Default is "BRUVs".
#' @param recursive Logical, whether to search for files recursively in subdirectories. Default is `FALSE`.
#'
#' @return A data frame containing all metadata from the specified files, with standardised columns 
#' for campaignID, sample, and various other metadata details.
#' @export
#'
#' @examples
#' \dontrun{
#' # To read metadata from a directory using the BRUVs method
#' metadata <- read_metadata("path/to/directory")
#'
#' # To read metadata from a directory recursively using the DOVs method
#' metadata <- read_metadata("path/to/directory", method = "DOVs", recursive = TRUE)
#' }
read_metadata <- function(dir, method = "BRUVs", recursive = FALSE) {
  
  read_dat <- function(flnm) {
    readr::read_csv(flnm, col_types =  readr::cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = stringr::str_replace_all(campaignid, c("_Metadata.csv" = "", "_metadata.csv" = "")))
  }
  
  files <- list.files(path = dir,      
                      recursive = recursive,
                      pattern = "etadata.csv",
                      full.names = T) 
  
  dat <- data.frame()
  
  for(file in unique(files)){
    
    message(paste("reading metadata file:", file))
    
    # TODO fix these so it cleans names first
    
    # # Rename any old columns to new names
    # lookup <- c(sample = "Sample", # Need to figure out what to do here
    #             latitude_dd = "Latitude", 
    #             latitude_dd = "latitude", 
    #             longitude_dd = "Longitude",
    #             longitude_dd = "longitude",
    #             location = "Location", 
    #             status = "Status",
    #             site = "Site",
    #             depth_m = "Depth",
    #             depth_m = "depth",
    #             date_time = "date.time",
    #             date_time = "Date.time",
    #             observer_count = "Observer",
    #             observer_count = "observer",
    #             successful_count = "Successful.count",
    #             successful_length	 = "Successful.length")
    
    temp_dat <- read_dat(file) #%>%
      # CheckEM::clean_names() #%>%
      # dplyr::rename(any_of(lookup))
    
    if(method %in% c("DOVs")){
      
      if("opcode" %in% names(temp_dat)){
        
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
          dplyr::select(-c(opcode, period))
        
      }
      
    } 
    
  #   if(method %in% c("BRUVs")){
  #     
  #     if("opcode" %in% names(temp_dat)){
  #       
  #       temp_dat <- temp_dat %>%
  #         dplyr::mutate(sample = opcode)
  #     }
  #     
  #   }
  #   
  #   if(!"date_time" %in% names(temp_dat)){
  #     
  #     message(paste0("the date_time column is missing from: ", file, ". Creating column now...."))
  #     
  #     # TODO time fix ones that are decimal
  #     # TODO remove AM PM from times
  #     # TODO fill blank times with midnight
  #     
  #     temp_dat <- temp_dat %>%
  #       tidyr::separate(time, into = c("hour", "min", "sec"), sep = ":") %>%
  #       dplyr::mutate(hour = stringr::str_pad(hour, 2, side = "left", pad = "0")) %>%
  #       dplyr::mutate(min = stringr::str_pad(min, 2, side = "left", pad = "0")) %>%
  #       dplyr::mutate(sec = stringr::str_pad(sec, 2, side = "left", pad = "0")) %>%
  #       dplyr::mutate(year = stringr::str_sub(date, 1,4)) %>%
  #       dplyr::mutate(month = stringr::str_sub(date, 5,6)) %>%
  #       dplyr::mutate(day = stringr::str_sub(date, 7,8)) %>%
  #       tidyr::replace_na(list(sec = "00", depth = 0)) %>%
  #       dplyr::mutate(time = paste(hour, min, sec, sep = ":")) %>%
  #       dplyr::mutate(date_time = paste(year, "-", month, "-", day, "T", time, sep = "")) %>%
  #       dplyr::mutate(date = paste(year, "-", month, "-", day, sep = "")) 
  #     
  #     coords_met <- temp_dat %>%
  #       dplyr::distinct(latitude_dd, longitude_dd)
  #     
  #     metadata_sf <- sf::st_as_sf(x = temp_dat, coords = c("longitude_dd", "latitude_dd")) %>%
  #       dplyr::mutate(timezone = lutz::tz_lookup(., crs = NULL, method = "accurate", warn = TRUE))
  #     
  #     timezones_to_add <- metadata_sf %>%
  #       dplyr::distinct(campaignid, sample, timezone)
  #     
  #     unique_timezones <- metadata_sf %>%
  #       dplyr::distinct(date, timezone)
  #     
  #     # Create a blank dataframe
  #     timezone_offsets <- data.frame()
  #     
  #     for(i in 1:nrow(unique_timezones)){
  #       temp_i <- unique_timezones[i,]
  #       temp <- lutz::tz_offset(temp_i$date, tz = temp_i$timezone) %>%
  #         dplyr::mutate(date = temp_i$date)
  #       timezone_offsets <- dplyr::bind_rows(temp, timezone_offsets)
  #     }
  #     
  #     timezone_offsets_format <- timezone_offsets %>%
  #       dplyr::mutate(hours = floor(utc_offset_h)) %>%
  #       dplyr::mutate(minutes = (utc_offset_h - hours) * 60) %>%
  #       dplyr::mutate(hours = stringr::str_pad(hours, 2, side = "left", pad = "0")) %>%
  #       dplyr::mutate(minutes = stringr::str_pad(minutes, 2, side = "left", pad = "0")) %>%
  #       dplyr::mutate(utc_offset = paste("+", hours, ":", minutes, sep = "")) %>% # TODO come up with a way to do plus or negative
  #       dplyr::select(tz_name, date, utc_offset) %>%
  #       dplyr::rename(timezone = tz_name) %>%
  #       dplyr::distinct()
  #     
  #     temp_dat <- temp_dat %>%
  #       dplyr::left_join(timezones_to_add) %>%
  #       dplyr::left_join(., timezone_offsets_format) %>%
  #       dplyr::mutate(date_time = paste(date_time, utc_offset, sep = "")) %>%
  #       dplyr::select(-c(hour, min, sec, year, month, day, time, timezone, utc_offset))
  #     
  #   }
  #   
    dat <- dplyr::bind_rows(dat, temp_dat)

  }
  
  return(dat)
  
}