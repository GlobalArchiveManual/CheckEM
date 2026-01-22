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
  
  # helper: fail early with useful message (great for CI logs)
  assert_good_names <- function(x, file) {
    bad <- is.na(names(x)) | names(x) == ""
    if (any(bad)) {
      stop(
        "Bad column names (NA/blank) in file: ", basename(file), "\n",
        "Names: ", paste0("[", names(x), "]", collapse = ", ")
      )
    }
    x
  }
  
  read_dat <- function(flnm) {
    dat <- readr::read_csv(
      flnm,
      col_types = readr::cols(.default = "c"),
      name_repair = "unique" # <-- KEY: fixes blank/NA/duplicate names
    )
    
    dat <- tibble::as_tibble(dat, .name_repair = "unique")
    dat <- assert_good_names(dat, flnm)
    
    dat %>%
      dplyr::mutate(campaignid = basename(flnm)) %>%
      CheckEM::clean_names() %>%  # your standardisation
      dplyr::mutate(
        campaignid = stringr::str_replace_all(
          campaignid,
          c("_Metadata.csv" = "", "_metadata.csv" = "")
        )
      )
  }
  
  files <- list.files(
    path = dir,
    recursive = recursive,
    pattern = "etadata\\.csv$",
    full.names = TRUE
  )
  
  dat <- tibble::tibble()
  
  # Map *old column names* -> *new standard names*
  # IMPORTANT: names() are the EXISTING possibilities, values are your desired standard names.
  lookup <- c(
    "Sample" = "sample",
    "Latitude" = "latitude_dd",
    "latitude" = "latitude_dd",
    "Longitude" = "longitude_dd",
    "longitude" = "longitude_dd",
    "Location" = "location",
    "Status" = "status",
    "Site" = "site",
    "Depth" = "depth_m",
    "depth" = "depth_m",
    "date.time" = "date_time",
    "Date.time" = "date_time",
    "Observer" = "observer_count",
    "observer" = "observer_count",
    "Successful.count" = "successful_count",
    "Successful.length" = "successful_length"
  )
  
  for (file in unique(files)) {
    
    message("read_metadata(): reading ", file)
    
    temp_dat <- read_dat(file)
    
    # Rename FIRST using known variants, then clean again (so downstream uses standard names)
    temp_dat <- temp_dat %>%
      dplyr::rename(!!!stats::setNames(names(lookup), lookup)) %>%
      CheckEM::clean_names()
    
    # Method-specific sample logic
    if (method %in% c("DOVs")) {
      if ("opcode" %in% names(temp_dat)) {
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = paste(opcode, period, sep = "-")) %>%
          dplyr::select(-dplyr::any_of(c("opcode", "period")))
      }
    }
    
    if (method %in% c("BRUVs")) {
      if ("opcode" %in% names(temp_dat)) {
        temp_dat <- temp_dat %>%
          dplyr::mutate(sample = opcode)
      }
    }
    
    # If date_time missing, create it
    if (!"date_time" %in% names(temp_dat)) {
      
      message("read_metadata(): date_time missing in ", basename(file), " — creating it")
      
      temp_dat <- temp_dat %>%
        tidyr::separate(time, into = c("hour", "min", "sec"), sep = ":", fill = "right") %>%
        dplyr::mutate(
          hour = stringr::str_pad(hour, 2, side = "left", pad = "0"),
          min  = stringr::str_pad(min,  2, side = "left", pad = "0"),
          sec  = stringr::str_pad(sec,  2, side = "left", pad = "0"),
          year = stringr::str_sub(date, 1, 4),
          month = stringr::str_sub(date, 5, 6),
          day = stringr::str_sub(date, 7, 8)
        ) %>%
        tidyr::replace_na(list(sec = "00")) %>%
        dplyr::mutate(
          time = paste(hour, min, sec, sep = ":"),
          date_time = paste0(year, "-", month, "-", day, "T", time),
          date = paste0(year, "-", month, "-", day)
        )
      
      # timezone work (unchanged, just keep it)
      metadata_sf <- sf::st_as_sf(temp_dat, coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
        dplyr::mutate(timezone = lutz::tz_lookup(., crs = NULL, method = "accurate", warn = TRUE))
      
      timezones_to_add <- metadata_sf %>%
        dplyr::distinct(campaignid, sample, timezone)
      
      unique_timezones <- metadata_sf %>%
        dplyr::distinct(date, timezone)
      
      timezone_offsets <- data.frame()
      for (i in seq_len(nrow(unique_timezones))) {
        temp_i <- unique_timezones[i, ]
        temp <- lutz::tz_offset(temp_i$date, tz = temp_i$timezone) %>%
          dplyr::mutate(date = temp_i$date)
        timezone_offsets <- dplyr::bind_rows(temp, timezone_offsets)
      }
      
      timezone_offsets_format <- timezone_offsets %>%
        dplyr::mutate(
          hours = floor(utc_offset_h),
          minutes = (utc_offset_h - hours) * 60,
          hours = stringr::str_pad(hours, 2, side = "left", pad = "0"),
          minutes = stringr::str_pad(minutes, 2, side = "left", pad = "0"),
          utc_offset = paste0("+", hours, ":", minutes)
        ) %>%
        dplyr::select(tz_name, date, utc_offset) %>%
        dplyr::rename(timezone = tz_name) %>%
        dplyr::distinct()
      
      temp_dat <- temp_dat %>%
        dplyr::left_join(timezones_to_add, by = c("campaignid", "sample")) %>%
        dplyr::left_join(timezone_offsets_format, by = c("timezone", "date")) %>%
        dplyr::mutate(date_time = paste0(date_time, utc_offset)) %>%
        dplyr::select(-dplyr::any_of(c("hour", "min", "sec", "year", "month", "day", "time", "timezone", "utc_offset")))
    }
    
    dat <- dplyr::bind_rows(dat, temp_dat)
  }
  
  dat
}
