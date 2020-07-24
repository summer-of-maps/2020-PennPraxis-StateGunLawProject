##########################################################################
# This script:
# 1. Counts the total crimes in every city by year 
#
# Exports: 
# 1. allCrimes_countByCity as 36_allCrimes_countByCity.rds
#
# To-do:
# 1. 
##########################################################################


## 1. ----
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")

allCrimes_countByCity <- vector("list", length(years_byCity)) %>% 
  set_names(names(years_byCity))

for (city in seq_len(length(years_byCity))[c(15:34)]) {
  cityName <- names(years_byCity)[[city]]
  cityYears <- years_byCity[[city]]
  
  print(cityName)
  
  allCrimes_countByCity[[city]] <- allCrimes_df %>% 
    filter(city == cityName) %>% 
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
           year = ifelse(!is.na(clean_occur_date),
                         lubridate::year(clean_occur_date),
                         lubridate::year(clean_report_date))) %>% 
    filter(year %in% cityYears) %>%
    group_by(year) %>%
    summarize(totalCrimes = n())
  
}

## 1. Export as rds ----
saveRDS(allCrimes_countByCity,
        "~outputs/30/36_allCrimes_countByCity.rds")
