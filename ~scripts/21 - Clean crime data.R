##########################################################################
# This script:
# 1. Splits the gun crimes 
# 2. Cleans the gun crime data by
# 1a. parsing crime occurrence and reported dates 
#
# Exports: 
# 1. gun_crimes_list as 21_gun_crimes_list.rds
#
# To-do:
# 1. Flatten list export to df instead? 
##########################################################################

## 1. ----
gun_crimes_list <- split(gun_crimes_df,
                         f = gun_crimes_df$city)

## 2. ----
plan(multiprocess)
gun_crimes_list <- future_map(gun_crimes_list,
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
                                                                     TRUE ~ clean_report_date)))

gun_crimes_clean <- bind_rows(gun_crimes_list)

## 1. Export as rds ----
# saveRDS(gun_crimes_clean,
#         "~outputs/20/21_gun_crimes_clean.rds")
