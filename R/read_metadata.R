#' A function to read GlobalArchive format metadata
#'
#' @param dir The directory where the .csv files are saved
#'
#' @return A dataframe with all a column for campaignID and sample
#' @export
#'
#' @examples
#' 
#' 
#' 

read_metadata <- function(dir, method = "BRUVs") {
  
  # dir <- here::here("r-workflows/data/raw/")
  
  read_dat <- function(flnm) {
    read_csv(flnm, col_types = cols(.default = "c")) %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%
      dplyr::mutate(campaignid = str_replace_all(campaignid, c("_Metadata.csv" = "", "_metadata.csv" = "")))
  }
  
  files <- list.files(path = dir,      
             recursive = F,
             pattern = "etadata.csv",
             full.names = T) 
  
  dat <- data.frame()
  
  for(file in unique(files)){
    
    message(paste("reading metdata file:", file))
    
    # If Sample exists keep sample, if opcode and period exist make sample = opcode-period
    
    # TODO fix these so it cleans names first
    
    # Rename any old columns to new names
    lookup <- c(sample = "Sample", # Need to figure out what to do here
                latitude_dd = "Latitude", 
                latitude_dd = "latitude", 
                longitude_dd = "Longitude",
                longitude_dd = "longitude",
                location = "Location", 
                status = "Status",
                site = "Site",
                depth_m = "Depth",
                depth_m = "depth",
                date_time = "date.time",
                observer_count = "Observer",
                successful_count = "Successful.count",
                successful_length	 = "Successful.length")
    
    # Need to fix date and time to make them consistent
    
    temp_dat <- read_dat(file) %>%
      clean_names() %>%
      dplyr::rename(any_of(lookup)) #%>%
      #dplyr::glimpse()
    
    # TODO add BRUVs
    
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
    
    if(!"date_time" %in% names(temp_dat)){
      
      message(paste0("the date_time column is missing from: ", file, ". Creating column now...."))
      
      # TODO time fix ones that are decimal
      # TODO remove AM PM from times
      # TODO fill blank times with midnight
      
      temp_dat <- temp_dat %>%
        tidyr::separate(time, into = c("hour", "min", "sec"), sep = ":") %>%
        dplyr::mutate(hour = str_pad(hour, 2, side = "left", pad = "0")) %>%
        dplyr::mutate(min = str_pad(min, 2, side = "left", pad = "0")) %>%
        dplyr::mutate(sec = str_pad(sec, 2, side = "left", pad = "0")) %>%
        dplyr::mutate(year = str_sub(date, 1,4)) %>%
        dplyr::mutate(month = str_sub(date, 5,6)) %>%
        dplyr::mutate(day = str_sub(date, 7,8)) %>%
        replace_na(list(sec = "00", depth = 0)) %>%
        dplyr::mutate(time = paste(hour, min, sec, sep = ":")) %>%
        dplyr::mutate(date_time = paste(year, "-", month, "-", day, "T", time, sep = "")) %>%
        dplyr::mutate(date = paste(year, "-", month, "-", day, sep = "")) #%>%
        #dplyr::glimpse()

      coords_met <- temp_dat %>%
        dplyr::distinct(latitude_dd, longitude_dd)

      metadata_sf <- sf::st_as_sf(x = temp_dat, coords = c("longitude_dd", "latitude_dd")) %>%
        dplyr::mutate(timezone = lutz::tz_lookup(., crs = NULL, method = "accurate", warn = TRUE))
      
      timezones_to_add <- metadata_sf %>%
        dplyr::distinct(campaignid, sample, timezone)

      unique_timezones <- metadata_sf %>%
        dplyr::distinct(date, timezone)

      # Create a blank dataframe
      timezone_offsets <- data.frame()

      for(i in 1:nrow(unique_timezones)){
        temp_i <- unique_timezones[i,]
        temp <- lutz::tz_offset(temp_i$date, tz = temp_i$timezone) %>%
          dplyr::mutate(date = temp_i$date)
        timezone_offsets <- bind_rows(temp, timezone_offsets)
      }

      timezone_offsets_format <- timezone_offsets %>%
        dplyr::mutate(hours = floor(utc_offset_h)) %>%
        dplyr::mutate(minutes = (utc_offset_h - hours) * 60) %>%
        dplyr::mutate(hours = str_pad(hours, 2, side = "left", pad = "0")) %>%
        dplyr::mutate(minutes = str_pad(minutes, 2, side = "left", pad = "0")) %>%
        dplyr::mutate(utc_offset = paste("+", hours, ":", minutes, sep = "")) %>% # IF i turn this into a function will have to come up with a way to do plus or negative
        dplyr::select(tz_name, date, utc_offset) %>%
        dplyr::rename(timezone = tz_name) %>%
        dplyr::distinct()
        #dplyr::glimpse()

      temp_dat <- temp_dat %>%
        dplyr::left_join(timezones_to_add) %>%
        dplyr::left_join(., timezone_offsets_format) %>%
        dplyr::mutate(date_time = paste(date_time, utc_offset, sep = "")) %>%
        dplyr::select(-c(hour, min, sec, year, month, day, time, timezone, utc_offset))

    }
    
    dat <- bind_rows(dat, temp_dat)
    
  }
  
  return(dat)
    
}
