##########################################################################
# This script:
# 1. Counts the total crimes in every city by year 
# 2. Finds and plots percentage of crimes that are gun crimes by year
#
# Exports: 
# 1. allCrimes_countByCity as 36_allCrimes_countByCity.rds
# 2. gunPercentage_byYear as 36_gunPercentage_byYear.rds
#
# To-do:
# 1. 
##########################################################################


## 1. ----
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
gunCount_byYear_list <- readRDS("~outputs/30/31_gunCount_byYear_list.rds")

# allCrimes_countByCity <- readRDS("~outputs/30/36_allCrimes_countByCity.rds")
allCrimes_countByCity <- vector("list", length(years_byCity)) %>% 
  set_names(names(years_byCity))

for (city in seq_len(length(years_byCity))) {
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

## 2. ----
gunPercentage_byYear <- map2(
  allCrimes_countByCity,
  gunCount_byYear_list,
  ~ left_join(.x, .y, by = "year") %>% 
    mutate(
      gun_perc = gun_count / totalCrimes
    )
)

gunPercentage_byYear_plots <- map(gunPercentage_byYear,
                                ~ .x %>% 
                                  ggplot(aes(x = year,
                                             y = gun_perc)) +
                                  geom_bar(position = "dodge",
                                           stat = "identity") +
                                  scale_x_continuous(breaks = seq(min(.x$year), max(.x$year), 2)) + 
                                  plotTheme() +
                                  labs(title = "Gun crimes as % of crimes by year",
                                       x = "Year",
                                       y = "% Gun Crimes"))

## 1. Export as rds ----
saveRDS(allCrimes_countByCity,
        "~outputs/30/36_allCrimes_countByCity.rds")

## 2. Export as rds ----
saveRDS(gunPercentage_byYear,
        "~outputs/30/36_gunPercentage_byYear.rds")
saveRDS(gunPercentage_byYear_plots,
        "~outputs/30/36_gunPercentage_byYear_plots.rds")
