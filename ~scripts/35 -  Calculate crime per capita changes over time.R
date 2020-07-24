##########################################################################
# This script:
# 1. Calculates the changes over time in crimes per capita over the following geographies
#     a. Block groups
#
# Exports: 
# 1. BGs_crimeChange as 35_BGs_crimeChange.rds
# 
# To-do:
# 1.  
#
##########################################################################

## 1a. ----

# test with Philly
# BGs_crimeCounts_byYear <- readRDS("~outputs/30/33_BGs_crimeCounts_byYear.rds")

BGs_crimeChange <- map(names(BGs_crimeCounts_byYear),
                       function(city) {
                         print(city)
                         
                         bind_rows(BGs_crimeCounts_byYear[[city]],
                                   .id = "Year") %>%
                           mutate(Year = as.numeric(Year)) %>% 
                           nest(data = c(Year, gun_count, worldPop, guns_per100, ntile_guns_per100)) %>% 
                           mutate(
                             fit = map(data, ~ lm(guns_per100 ~ Year, data = .x)),
                             tidied = map(fit, tidy)
                           ) %>% 
                           unnest(tidied) %>% 
                           dplyr::select(-c(data, fit, std.error, statistic)) %>% 
                           filter(term == "Year") %>% 
                           rename(YearCoefficient = estimate) %>% 
                           mutate(CrimeIncrease = case_when(YearCoefficient > 0 & 
                                                              p.value < 0.1 ~ "Yes (significant)",
                                                            YearCoefficient > 0 & 
                                                              p.value >= 0.1 | is.na(p.value) ~ "Yes (not significant)",
                                                            YearCoefficient <= 0 & 
                                                              p.value < 0.1 ~ "No (significant)",
                                                            YearCoefficient <= 0 & 
                                                              p.value >= 0.1 | is.na(p.value) ~ "No (not significant)",
                                                            # YearCorrelation <= 0 ~ "No",
                                                            TRUE ~ as.character(YearCoefficient)))
                         }
                       ) %>% 
  set_names(.,
            names(BGs_crimeCounts_byYear))


## 1a. Export as rds ----
# saveRDS(BGs_crimeChange,
#         "~outputs/30/35_BGs_crimeChange.rds")
