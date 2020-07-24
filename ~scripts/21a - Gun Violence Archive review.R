##########################################################################
# This script:
# 1. Reads in the SGLP's Gun Violence Archive data
# 2. Counts observations for some cities for comparison with SGLP data
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
GVA_1 <- vroom(file.path(data_dir, "Gun Violence Archive Data/GVA_2013-2018.csv"))
GVA_2 <- vroom(file.path(data_dir, "Gun Violence Archive Data/GVA_2020.csv"))


## 2. ----
# Atlanta

GVA_1 %>% 
  filter(city_or_county == "Atlanta",
         !is.na(address) | !is.na(latitude)) %>% 
  nrow

GVA_2 %>% 
  filter(city == "Atlanta",
         !is.na(address)) %>% 
  nrow

# Chicago
GVA_1 %>% 
  # filter(city_or_county == "Atlanta",
  #        !is.na(address) | !is.na(latitude)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(count = n())

GVA_1 %>% 
  # filter(city_or_county == "Chicago",
  #        !is.na(address) | !is.na(latitude)) %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2018) %>%
  .$date %>% summary
  group_by(year) %>% 
  summarize(count = n())

GVA_2 %>% 
  filter(city == "Atlanta",
         !is.na(address)) %>%
  mutate(year = year(date)) %>% 
  group_by(year) %>% summarize(count = n())
  .$date %>% summary
