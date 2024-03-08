# load libraries
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(tidyverse)

## This script scrapes the maximum size limits and depth limits from fishes on australia
## Unhash this first part to re-run the scraping

# # Create a blank data frame with the columns you want
# data <- data.frame(family = character(),
#                    genus = character(),
#                    species = character(),
#                    url = character(),
#                    num = numeric(),
#                    caab = character(),
#                    max_size = character(),
#                    depth = character(),
#                    habitat = character(),
#                    stringsAsFactors = FALSE)
# 
# for(num in 1:5579) {
#   print(num)
#   
#   # Construct the URL
#   url <- paste0("https://fishesofaustralia.net.au/home/species/", num)
#   
#   # Set a user agent header to identify our scraper
#   headers <- c('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36')
#   
#   # Send a GET request to the URL
#   print(paste0("GET-ing ", url, " ..."))
#   response <- httr::GET(url, headers = headers)
#   httr::stop_for_status(response)
#   print("Success.")
#   
#   options(xmlMaxDepth = 4000)
#   
#   # Parse the HTML content of the response with xml2
#   html <- read_html(content(response, "text"), options = "HUGE")
#   
#   # Use xpath to extract the ul element
#   ul_element <- html %>% html_node(xpath = '//*[@id="content"]/div[1]/div/ul')
#   
#   length(ul_element)
#   
#   # Check that the ul element is present
#   # if (is.null(ul_element)) {
#   if(length(ul_element) == 0) {
#     print("Error: could not find ul element")
#   } else {
#     # Extract the last three li elements from the ul element
#     li_elements <- ul_element %>% html_nodes("li") %>% tail(3)
#     
#     # Check that there are at least three li elements
#     if (length(li_elements) < 3) {
#       stop("Error: could not find three li elements")
#     } else {
#       # Extract the family, genus, and species from the text of the li elements, or the text of the link it contains
#       family <- li_elements[[1]] %>% html_node("a") %>% html_text(trim = TRUE)
#       genus <- li_elements[[2]] %>% html_node("a") %>% html_text(trim = TRUE)
#       species <- li_elements[[3]] %>% html_text(trim = TRUE)
#       
#       # Print the results
#       cat("Family: ", str_to_title(family), "\n")
#       cat("Genus: ", str_to_title(genus), "\n")
#       cat("Species: ", tolower(species), "\n")
#       
#       # ready to read page
#       # url <- paste0("https://fishesofaustralia.net.au/home/species/", 678)
#       page <-  readLines(url)
#       
#       caab <- grep("CAAB Code:", page)
#       caab <- page[caab]
#       caab <- gsub("</span>", "", caab)
#       substring_to_remove <- "                        <span class=\"term\">CAAB Code:<span class=\"data\">"
#       caab <- gsub(substring_to_remove, "", caab)
#       caab <- gsub(" ", "", caab)
#       caab
#       
#       
#         
#       l <- grep("Max Size:", page)
#       max_size <- page[l]
#       max_size <- gsub("                        <span class=\"term\">Max Size:</span><span class=\"data\">", "", max_size)
#       max_size <- gsub("</span>", "", max_size)
#       
#       max_size
#       
#       max_size <- max_size[1]
#       
#       d <- grep("Depth:", page)
#       depth <- page[d]
#       depth <- gsub("</span>", "", depth)
#       substring_to_remove <- "                        <span class=\"term\">Depth:<span class=\"data\">"
#       depth <- gsub(substring_to_remove, "", depth)
#       depth
#       
#       
#       depth <- depth[1]
#       
#       h <- grep("Habitat:", page)
#       habitat <- page[h]
#       habitat <- gsub("</span>", "", habitat)
#       substring_to_remove <- "                        <span class=\"term\">Habitat:<span class=\"data\">"
#       habitat <- gsub(substring_to_remove, "", habitat)
#       habitat
#       
#       habitat <- habitat[1]
#       
#       dat <- list(family = str_to_title(family),
#                   genus = str_to_title(genus),
#                   species = tolower(species),
#                   url = url,
#                   number = num,
#                   caab = caab,
#                   max_size = max_size, 
#                   depth = depth,
#                   habitat = habitat)
#       
#       dat <- Filter(function(x) !is.na(x) && length(x) > 0 && x != "", dat)
#       
#       # Convert the list to a data frame and write to CSV
#       df <- as.data.frame(dat, stringsAsFactors = FALSE)
#       
#       data <- dplyr::bind_rows(data, df)
#       
#       # filename <- paste0(num, "_test.csv")
#       # write.csv(df, file = filename, row.names = FALSE)
#       # cat("Data saved to file: ", filename, "\n")
#     }
#   }
# }
# 
# saveRDS(data, "annotation-schema/data/staging/temporary_fishes-of-australia_maximum-sizes.RDS")

data<- readRDS("annotation-schema/data/staging/temporary_fishes-of-australia_maximum-sizes.RDS")

clean_data <- data %>%
  dplyr::mutate(max_size = str_replace_all(max_size, c("To " = "", "to " = "",
                                           "\\+" = "",
                                           "Height: 12 cm" = "",
                                           "\\&lt;4 cm TL" = "4 cm TL",
                                           "At least " = "",
                                           " \\s*\\(Australia\\)" = "",
                                           "; 18 kg" = "",
                                           "Reef associated" = "",
                                           "1-20 m" = "",
                                           "; 75 kg" = "",
                                           "&gt;" = "",
                                           "; 15 kg" = "",
                                           "; 10.5 kg" = "",
                                           " \\s*\\(holotype\\)" = "",
                                           "; 37 kg" = "",
                                           " &amp; 40 kg" = "",
                                           "&gt;" = "",
                                           "; 4.5 kg" = "",
                                           ", 4 kg" = "",
                                           "; 20 kg" = "",
                                           "; 25 kg" = "",
                                           ", 8 kg" = "",
                                           "; 100 kg" = "",
                                           "; 708 kg" = "", 
                                           " 62" = "62",
                                           " \\s*\\(commonly 35-45cm\\)" = "",
                                           "; 12.3 kg" = "",
                                           "; 34.5 kg" = "",
                                           "; 70 kg" = "",
                                           "; 10 kg" = "",
                                           "; 60.3 kg" = "",
                                           "; 183.7 kg" = "",
                                           " \\s*\\(FL\\); 200 kg" = "FL",
                                           ", 200 kg" = "",
                                           "30-110 m" = "",
                                           "; 50 kg" = "",
                                           "; 7 kg" = "",
                                           "; 2000 kg" = "",
                                           "; 2.7 tonnes" = "",
                                           " \\s*\\(usually 15 cm\\)" = "",
                                           "cmSL" = "cm SL",
                                           "38cm TL" = "38 cm TL",
                                           "0cm" = "0 cm",
                                           "\\~" = "",
                                           "1-30 m" = "",
                                           "3.9 cm SL; 4.2 cm TL" = "4.2 cm TL",
                                           "; 11.3 kg" = "",
                                           " \\s*\\/ 15.4 kg" = "",
                                           "20–78 m" = "",
                                           " \\s*\\(fork length\\)" = "",
                                           "; 7 kg" = "",
                                           "; 60 cm TL" = "",
                                           "1cm" = "1 cm",
                                           "2cm" = "2 cm",
                                           "3cm" = "3 cm",
                                           "4cm" = "4 cm",
                                           "5cm" = "5 cm",
                                           "6cm" = "6 cm",
                                           "7cm" = "7 cm",
                                           "8cm" = "8 cm",
                                           "9cm" = "9 cm",
                                           "metres" = "m",
                                           "metre" = "m",
                                           "Mesopelagic, bathypelagic" = "",
                                           "cmTL" = "cm TL",
                                           "; 30 kg" = "",
                                           "17mmTL; height 14mm" = "17 mm TL",
                                           "Height 10 cm" = "",
                                           "22 cm \\s*\\(height\\)" = "",
                                           "Height 12 cm" = "",
                                           "Height 9 cm" = "",
                                           "Height 11 cm" = "",
                                           "Height 18 cm" = "",
                                           "Males 30 cm; females 20 cm" = "30 cm",
                                           "; 41 kg" = "",
                                           "; 3.5 kg" = "",
                                           "; 100 cm TL" = "",
                                           "; 96.8 kg" = "",
                                           "; 120 cm TL" = "",
                                           " ; 40 kg" = "",
                                           "; 270 kg" = "",
                                           " \\s*\\(female\\)" = "",
                                           "Fem 5.8 cm SL; Male 4.6 cm SL" = "4.6 cm SL",
                                           "; 167 kg" = "",
                                           "; 12.6 kg" = "",
                                           " \\s*\\(possibly 180 cm TL\\) " = "",
                                           "4.3m TL; 2.1m DW; 350 kg" = "4.3 m TL",
                                           "24 cm DW; 45 cm TL" = "45 cm TL",
                                           "220 cm DW; 320 cm TL" = "320 cm TL",
                                           "183 cm DW; 350 cm TL " = "350 cm TL",
                                           "300 cm TL; 150 cm DW" = "300 cm TL",
                                           "300 cm TL; disc width 180 cm" = "300 cm TL",
                                           "70 cm TL; 35 cm DW" = "70 cm TL",
                                           "147 cm TL; 100 cm DW" =  "147 cm TL",
                                           "disc width" = "DW",
                                           "Disc width 130 cm" = "130 cm DW",
                                           "8.5 cm SL; 10 cm TL" = "10 cm TL",
                                           "53 cm SL; 88 cm TL" = "88 cm TL",
                                           "Height " = "",
                                           "Height:" = "",
                                           "Mesopelagic" = "",
                                           "Disc width 520 cm" = "520 cm DW",
                                           "Length 6.4 m; weight 3000 kg" = "6.4 m",
                                           "3.65m TL; 350 kg" = "3.65 m TL",
                                           "about " = "",
                                           "7.5 cm SL \\s*\\(11.5 cm TL\\)" = "11.5 cm TL",
                                           "500 cm TL; 900 kg" = "500 cm TL",
                                           "; 52 kg" = "",
                                           "; 10.2 kg" = "",
                                           "; 12.2 kg" = "",
                                           "; 450 kg" = "",
                                           "; 600 kg" = "",
                                           " \\s*\\(immature male\\)" = "",
                                           "480-700 m" = "",
                                           "115-605 m" = "",
                                           "51 cm DW; 110 cm TL" = "110 cm TL",
                                           "19 cm DW; 37 cm TL" = "37 cm TL",
                                           "180 cm TL; 92 cmDW" = "180 cm TL",
                                           "140 cm DW; 410 cm TL" = "410 cm TL",
                                           "Height 15 cm" = "",
                                           " \\s*\\(females\\)" = "",
                                           "36.5 mm SL, 47 mm TL" = "47 mm TL",
                                           "500 cm DW \\s*\\(DW\\)" = "500 cm DW",
                                           "8.5 cm SL, 10 cm TL" = "10 cm TL",
                                           "; 5.5 kg" = "",
                                           "; 190 kg" = "",
                                           ", commonly 15 cm TL" = "",
                                           "Pelagic, inshore" = "",
                                           "; 40 kg" = "",
                                           "; 150 cmDW" = "",
                                           " \\s*\\(commonly 25 cm\\)" = "",
                                           "; 0.5 kg" = "",
                                           "; 3.8 kg" = "",
                                           "1.5 m SL; 1.8 m TL" = "1.8 m TL",
                                           "at least " = "",
                                           "; 1.3 kg" = "",
                                           "; 5 kg" = "",
                                           "; 4 kg" = "",
                                           "; 2 kg" = "",
                                           "; 3 kg" = "",
                                           "; 8 kg" = "",
                                           " \\s*\\(male 16 cm SL\\)" = "",
                                           "4.8 cm SL; 5 cm TL" = "5 cm TL",
                                           "Sandy areas" = "",
                                           " \\s*\\(males 1.6 cm\\)" = "",
                                           "435-1440 m" = "",
                                           "DW \\s*\\(DW\\)" = "",
                                           "170 Cm TL; 82 cm DW" = "170 cm TL",
                                           "180 cm DW; 330 cm TL" = "330 cm TL",
                                           " \\s*\\(in Europe\\)" = "",
                                           "Females 6 cm; males 3.5 cm" = "6 cm",
                                           "; 10.4 kg" = "",
                                           "; 4 kg" = "",
                                           "24 cm SL, 30 cm TL" = "30 cm TL",
                                           "9 cm TL, 7 cm SL" = "9 cm TL",
                                           "46.5 cm SL/56.5 cm TL" = "56.5 cm TL",
                                           "6.7 cm SL; 7.5 cm TL" = "7.5 cm TL",
                                           "; 9 kg" = "",
                                           "Height 10.5 cm" = "",
                                           "2.2 cm \\s*\\(height\\)" = "",
                                           "30 cm SL; 40 cm TL" = "40 cm TL",
                                           "4-5 cm TL" = "5 cm TL",
                                           "72 cm TL; commonly 35 cm" = "72 cm TL",
                                           "; 14 kg" = "",
                                           "; 86.6 kg" = "",
                                           "0-350 m" = "",
                                           "37 cm SL, 40 cm TL" = "40 cm TL",
                                           "Males 15 cm TL; females 7 cm TL" = "15 cm TL",
                                           "; 9 kg" = "",
                                           "; 68 kg" = "",
                                           "; 2.7 kg" = "",
                                           "; 32 kg" = "",
                                           ", 7 kg" = "",
                                           "; 23.6 kg" = "",
                                           "; 12 kg" = "",
                                           "; 26 kg" = "",
                                           "; 60 kg" = "",
                                           "; 16 kg" = "",
                                           "; 23 kg" = "",
                                           "; 68 kg" = "",
                                           "; 400 kg" = "",
                                           "Disc width" = "DW",
                                           "330-1950 m" = "",
                                           "; 2 kg" = "",
                                           "16.3 cm TL; 2.6 cm HL " = "16.3 cm TL",
                                           "15 cm TL; 3 cm HL" = "15 cm TL",
                                           "30.6 mm SL; 42 mm TL" = "42 mm TL",
                                           "12.2 cm TL; 8.8 cm SL" = "12.2 cm TL",
                                           "4 cm HL, 20 cm TL" = "20 cm TL",
                                           "46 cm TL; 12 cm HL" = "46 cm TL",
                                           "57 cm TL \\s*\\(46.5 cm SL\\)" = "57 cm TL",
                                           "112 cm DW; 240 cm TL" = "240 cm TL",
                                           "; 6 kg" = "",
                                           "cmFL" = "cm FL",
                                           "CM" = "cm"
                                           ))) %>%
  tidyr::separate(max_size, into = c("new_maximum_length_in_cm", "metric", "length_type"), sep = " ") %>%
  dplyr::mutate(new_maximum_length_in_cm = as.numeric(new_maximum_length_in_cm)) %>%
  dplyr::mutate(new_maximum_length_in_cm = if_else(metric == "mm", new_maximum_length_in_cm / 10, new_maximum_length_in_cm)) %>%
  dplyr::mutate(new_maximum_length_in_cm = if_else(metric == "m", new_maximum_length_in_cm * 100, new_maximum_length_in_cm)) %>%
  dplyr::filter(!is.na(new_maximum_length_in_cm)) %>%
  dplyr::select(!metric) %>%
  
  dplyr::mutate(depth = str_replace_all(depth, c("–" = "-",
                                                 " to" = "-",
                                                 "To " = "0-",
                                                 "to " = "0-",
                                                 "surface" = "0",
                                                 "Surface" = "0",
                                                 " metres" = "",
                                                 " m" = "",
                                                 "m" = "",
                                                 " \\(AUS\\)" = "",
                                                 " \\(Aus\\)" = "",
                                                 "," = "",
                                                 "-about "= "-",
                                                 "about " = "0-",
                                                 "Usually below 15" = "0-15",
                                                 "Inshore" = "0",
                                                 "inshore" = "0",
                                                 "Intertidal" = "0",
                                                 "Below 60" = "0-60",
                                                 "Usually 500-2100" = "500-2100",
                                                 "usually above 70" = "0-70",
                                                 "Venoous spine on shoulder" = "",
                                                 "Surge zone 0-3" = "0-3",
                                                 "0 zone" = "",
                                                 "~" = ""))) %>%
  
  tidyr::separate(depth, into = c("min_depth", "max_depth"), sep = "-", remove = FALSE) %>%
  dplyr::mutate(max_depth = gsub("^\\s+", "", max_depth)) %>%
  tidyr::separate(max_depth, into = c("max_depth", "extra"), sep = " ") %>%
  dplyr::mutate(max_depth = gsub("\\+", "", max_depth)) %>%
  dplyr::select(-c(extra, num))
  
unique(clean_data$min_depth) %>% sort()
unique(clean_data$max_depth) %>% sort()
unique(clean_data$new_maximum_length_in_cm) # looks ok
unique(clean_data$metric) # looks ok
unique(clean_data$length_type)

duplicates <- clean_data %>%
  dplyr::group_by(caab) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)
  
final_data <- clean_data %>%
  anti_join(duplicates)

duplicates <- final_data %>%
  dplyr::group_by(caab) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

duplicates <- final_data %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

saveRDS(final_data, "annotation-schema/data/staging/fishes-of-australia_maximum-sizes.RDS")
