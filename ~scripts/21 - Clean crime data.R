##########################################################################
# This script:
# 1. Splits the gun crimes 
# 2. Cleans the gun crime data by:
#   (a) parsing occurrence date and reported date 
#   (b) turns lon/lat columns into numeric
#   (c) San Francisco
#     (i) fixes NA coordinate values
#     (ii) fixes lat/long values in the raw data that had been flipped
#
# Exports: 
# 1. guns_list as 21_guns_list.rds
# 2. guns_clean as 21_guns_clean.rds
#
# To-do:
#
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
                                                               TRUE ~ clean_report_date),
                                 # 2(b)
                                 lon = as.numeric(lon),
                                 lat = as.numeric(lat)))

# 2(b) - San Francisco
guns_list$`San Francisco` <- guns_list$`San Francisco` %>% 
  mutate(lon_tmp = lon,
         lat_tmp = lat,
         # 2(b)(i)
         lon_tmp = ifelse(lon_tmp == 90, NA, lon_tmp), # treat these are NA observations
         # 2(b)(ii)
         lon = ifelse(lon_tmp > 0, lat_tmp, lon_tmp), # lat/lon got flipped incorrectly for some observations 
         lat = ifelse(lon_tmp > 0, lon_tmp, lat_tmp)) %>% 
  dplyr::select(-c("lon_tmp", "lat_tmp")) # remove raw and helper columns

guns_clean <- bind_rows(guns_list)

## 1. Export as rds ----
# saveRDS(guns_list,
#         "~outputs/20/21_guns_list.rds")

## 2. Export as rds ----
# saveRDS(guns_clean,
#          "~outputs/20/21_guns_clean.rds")
