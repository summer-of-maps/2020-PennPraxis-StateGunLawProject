##########################################################################
# This script:
# 1. Splits the gun crimes 
# 2. Cleans the gun crime data by:
#   (a) parsing occurrence date and reported date 
#   (b) documenting cleaning steps for every city
#   (c) turns lon/lat columns into numeric
# 3. Flattens the list into a dataframe
# 4. Creates an sf version of the crime data in list form
#
# Exports: 
# 1. guns_list as 21_guns_list.rds
# 2. guns_clean as 21_guns_clean.rds
#
# To-do:
# 1. Baton Rouge
##########################################################################

## 1. ----
guns_list <- split(guns_df,
                   f = guns_df$city)

## 2. ----
plan(multiprocess)
# 2(a) - this takes a while
guns_list <- future_map(guns_list,
                        ~ .x %>% 
                          mutate(clean_occur_date = anydate(occurdate), # use built-in formats from anytime package
                                 # correct some incorrectly parsed observations
                                 clean_occur_date = case_when(occurdate == "1" ~ as.Date(NA),
                                                              clean_occur_date < as.Date("1900-01-01") ~ as.Date(NA), 
                                                              is.na(clean_occur_date) &
                                                                str_detect(occurdate,
                                                                           ".*\\d+/\\d+/\\d+.*") ~ # e.g "12/3/15", "12/3/15 1600" 
                                                                as.Date(occurdate, "%m/%d/%y"),
                                                              TRUE ~ clean_occur_date),
                                 clean_report_date = anydate(reportdate),
                                 clean_report_date = case_when(reportdate == "1" ~ as.Date(NA),
                                                               clean_report_date < as.Date("1900-01-01") ~ as.Date(NA),
                                                               is.na(clean_report_date) &
                                                                 str_detect(reportdate,
                                                                            ".*\\d+/\\d+/\\d+.*") ~ # e.g "12/3/15", "12/3/15 1600"
                                                                 as.Date(reportdate, "%m/%d/%y"),
                                                               TRUE ~ clean_report_date)),
                        .progress = TRUE)





#!!!!! 2(b) - DO THIS AFTER FIXING ALL THE city-by-city data
lon = as.numeric(lon),
lat = as.numeric(lat)
#!!!!!





# 2(b)
### Atlanta - looks fine

### Auburn - add lon and lat info from loc column with the below
guns_list$Auburn <- guns_list$Auburn %>% 
  mutate(lon = str_match(loc, "[:digit:], (.*?)\\)")[, 2],
         lat = str_match(loc, "\\((.*?),")[, 2])

### Baltimore - looks fine

### Baton Rouge
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$`Baton Rouge`)
guns_df %>% 
  filter(city == "Baton Rouge") %>% 
  View()

### Boston - lon/lat columns contain both coords in format (123, 456)
guns_list$Boston <- guns_list$Boston %>%
  mutate(coord_tmp = lon,
         lon = str_match(coord_tmp, "[:digit:], (.*?)\\)")[, 2],
         lon = ifelse(lon == "0.0", NA, lon),
         lat = str_match(coord_tmp, "\\((.*?),")[, 2],
         lat = ifelse(lat == "0.0", NA, lat)) %>%
  dplyr::select(-coord_tmp)

### Chicago - some NAs, but otherwise good

### Cincinnati - some NAs, but otherwise good

### Columbia
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Columbia)

### Dallas - in some sort of projected coordinate system
# possibly EPSG code 2276 - based on: https://gis.dallascityhall.com/documents/DWU-PRO-013-GIS_CityGISDataStandards.pdf
# and https://spatialreference.org/ref/epsg/2276/
# also, lat/lon are flipped in the raw data
guns_list$Dallas <- guns_list$Dallas %>% 
  mutate(tmp_lon = lat, # correct the flipped lat/lon columns
         tmp_lat = lon,
         lon = tmp_lon,
         lat = tmp_lat) %>% 
  dplyr::select(-c(tmp_lon, tmp_lat))

guns_list$Dallas <- replace_lat_lon_cols(data = guns_list$Dallas,
                                         current_crs = 2276,
                                         new_crs = 4326)

### Denver - looks fine

### Detroit - looks fine

### Gainesville - looks fine

### Hartford
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Hartford)

### Indianapolis
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Indianapolis)

### Kansas City
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$`Kansas City`)

### Lincoln
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Lincoln)

### Little Rock - add lon and lat info from loc column with the below
guns_list$`Little Rock` <- guns_list$`Little Rock` %>% 
  mutate(lon = str_match(loc, "[:digit:], (.*?)\\)")[, 2],
         lat = str_match(loc, "\\((.*?),")[, 2])

### Los Angeles - looks fine

### Louisville
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Louisville)

### Madison
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Madison)

### Minneapolis - looks fine

### Nashville - looks fine

### New York - looks fine

### Phoenix
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Phoenix)

### Portland
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Portland)

### Raleigh
# some coords are (0, 0). Change those to NA.
guns_list$Raleigh <- guns_list$Raleigh %>% 
  mutate(lat = ifelse(lat == "0", NA, lat),
         lon = ifelse(lon == "0", NA, lon))

### Sacramento County
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$`Sacramento County`)

### Saint Paul
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$`Saint Paul`)

### Salt Lake City - same as Dallas
# also, a few of the coordinates are not valid, so make those to NA
guns_list$`Salt Lake City` <- guns_list$`Salt Lake City` %>% 
  mutate(tmp_lon = lat, # correct the flipped lat/lon columns
         tmp_lat = lon,
         lon = tmp_lon,
         lat = tmp_lat) %>% 
  dplyr::select(-c(tmp_lon, tmp_lat))

guns_list$`Salt Lake City` <- replace_lat_lon_cols(data = guns_list$`Salt Lake City`,
                                                   current_crs = 32043,
                                                   new_crs = 4326) %>% 
  mutate(lat = ifelse(lat < 0, NA, lat), # make invalid coordinates NA
         lon = ifelse(lon > 0, NA, lon))

### San Francisco
# lon/lat columns are flipped for some observations. some obs are also NAs
guns_list$`San Francisco` <- guns_list$`San Francisco` %>% 
  mutate(lon_tmp = lon,
         lat_tmp = lat,
         # 2(b)(i)
         lon_tmp = ifelse(lon_tmp == 90, NA, lon_tmp), # treat these are NA observations
         # 2(b)(ii)
         lon = ifelse(lon_tmp > 0, lat_tmp, lon_tmp), # lat/lon got flipped incorrectly for some observations 
         lat = ifelse(lon_tmp > 0, lon_tmp, lat_tmp)) %>% 
  dplyr::select(-c("lon_tmp", "lat_tmp")) # remove helper columns

### St Louis County
# lon/lat are flipped
# also mark (0, 0) as NA
guns_list$`St Louis County` <- guns_list$`St Louis County` %>% 
  mutate(tmp_lon = lat, # correct the flipped lat/lon columns
         tmp_lat = lon,
         lon = tmp_lon,
         lat = tmp_lat,
         lon = ifelse(lon == "0", NA, lon),
         lat = ifelse(lat == "0", NA, lat)) %>% 
  dplyr::select(-c(tmp_lon, tmp_lat))

### Tucson
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$Tucson)
guns_df %>% 
  filter(city == "Tucson") %>% 
  View

### Virginia Beach
# TO-DO all locations are missing. Need to look at the city file.
View(guns_list$`Virginia Beach`)
guns_df %>% 
  filter(city == "Virginia Beach") %>% 
  View

tmp_dir <- "C:/Users/echong/Dropbox/SGLP_Azavea/Individual_City_Dataset/"
tmp_csv <- read_csv(file.path(tmp_dir, "VirginiaBeach_Virginia/raw_data/Police+Incident+Reports.csv"))

## 3. ----
guns_clean <- bind_rows(guns_list)

## 1. Export as rds ----
# saveRDS(guns_list,
#         "~outputs/20/21_guns_list.rds")

## 2. Export as rds ----
# saveRDS(guns_clean,
#          "~outputs/20/21_guns_clean.rds")
