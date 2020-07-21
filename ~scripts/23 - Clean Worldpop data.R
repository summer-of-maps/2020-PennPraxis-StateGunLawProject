##########################################################################
# This script:
# 1. Crops and masks all of the worldpop rasters by each city for each year for 
#     which that city has crime data
#
# Exports: 
# 1. years_byCity as 23_years_byCity.rds
# 2. worldpop_byCity as 23_worldpop_byCity.rds
#
# To-do:
# 1. 
##########################################################################

## 1. ----
years_byCity <- map(gunCount_byYear_list,
                    ~ sort(.x$year) %>% 
                      set_names(., nm = .))

# for every year for which we have crime data for every city
# crop and mask the relevant worldpop USA raster file
plan(multiprocess)

worldpop_byCity <- future_map2(BG_selection_list$byCaveHull,
             years_byCity,
             function(BGs, cityTimes)
               map(cityTimes,
                   function(year) 
                     crop(worldpop_list[[as.character(year)]],
                          BGs) %>% 
                     mask(BGs)),
             .progress = TRUE)

## 1. Export as rds ----
# saveRDS(years_byCity,
#         "~outputs/20/23_years_byCity.rds")
# saveRDS(worldpop_byCity,
#         "~outputs/~large_files/23_worldpop_byCity.rds")
