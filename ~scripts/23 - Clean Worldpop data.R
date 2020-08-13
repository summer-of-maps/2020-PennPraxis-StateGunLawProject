##########################################################################
# This script:
# 1. Crops and masks all of the worldpop rasters by each city for each year for 
#     which that city has crime data
# 2. Creates grid cells using the rasters:
#     a. worldpop_grids has the 100m x 100m geometries - same across years
#     b. worldpop_data contains dataframes with cell populations for every year
# 3. Aggregates population by 2018 census block for city for every year with crime date
#
# Exports: 
# 1. years_byCity as 23_years_byCity.rds
# 2. worldpop_byCity_raster as 23_worldpop_byCity_raster.rds
# 3. worldpop_grids as 23_worldpop_grids.rds
# 4. worldpop_data as 23_worldpop_data.rds
#
# To-do:
# 1. 
##########################################################################

## 1. ----
# years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
years_byCity <- map(guns_list_shp,
                    ~ .x %>% 
                      pull(year) %>% 
                      unique() %>% 
                      sort() %>% 
                      set_names()
                    )

# for every year for which we have crime data for every city
# crop and mask the relevant worldpop USA raster file
plan(multiprocess)

# worldpop_byCity <- future_map2(BG_selection_list$byPlace,
#              years_byCity,
#              function(BGs, cityTimes)
#                map(cityTimes,
#                    function(year) 
#                      crop(worldpop_list[[as.character(year)]],
#                           BGs) %>% 
#                      mask(BGs)),
#              .progress = TRUE)

worldpop_byCity_raster <- vector("list", length(BG_selection_list$byPlace)) %>%
  set_names(names(BG_selection_list$byPlace))

for (city in seq_len(length(BG_selection_list$byPlace))[16:34]){
  
  print(names(BG_selection_list$byPlace)[city])
  
  BGs <- BG_selection_list$byPlace[[city]]
  
  city_years <- years_byCity[[city]]
  worldpop_byCity_raster[[city]] <- vector("list",
                                           length(city_years)) %>% 
    set_names(names(city_years))
  
  for (year in seq_len(length(city_years))) {
    
    print(names(city_years)[year])
    
    worldpop_byCity_raster[[city]][[year]] <- 
      crop(worldpop_list[[year]],
           BGs) %>% 
      mask(BGs)
    
  }
  
  
}


## 2. ----
# years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
# worldpop_byCity_raster <- readRDS("~outputs/~large_files/23_worldpop_byCity_raster.rds")

worldpop_grids <- vector("list", length(worldpop_byCity_raster))
worldpop_data <- vector("list", length(worldpop_byCity_raster))

for(city in seq_len(length(worldpop_byCity_raster))) {
  print(names(worldpop_byCity_raster)[city])

  worldpop_grids[[city]] <- st_as_sf(st_as_stars(worldpop_byCity_raster[[city]][[1]]),
                                    as_points = FALSE, merge = FALSE) %>% 
    rownames_to_column(var = "cell_ID") %>% 
    dplyr::select(cell_ID)
  
  worldpop_data[[city]] <- map2(worldpop_byCity_raster[[city]],
                               names(worldpop_byCity_raster[[city]]),
                               function(raster, year){
                                 print(year)
                                 st_as_sf(st_as_stars(raster),
                                          as_points = FALSE, merge = FALSE) %>%
                                 st_drop_geometry() %>% 
                                 rownames_to_column(var = "cell_ID") %>%
                                 # rename the population column that is different for every year to something standard
                                 rename(worldPop = 2) 
                                 }
                                 ) %>% 
    set_names(nm = names(.))
  
}

worldpop_grids <- worldpop_grids %>% 
  set_names(nm = names(worldpop_byCity_raster))
worldpop_data <- worldpop_data %>% 
  set_names(nm = names(worldpop_byCity_raster))

## 3. ----
# BG_pops_byYear <- readRDS("~outputs/20/23_BG_pops_byYear.rds")
BG_pops_byYear <- vector("list", length(BG_selection_list$byPlace)) %>% 
  set_names(names(BG_selection_list$byPlace))

for (city in seq_len(length(BG_pops_byYear))) {
  print(names(BG_pops_byYear)[city])
  
  BG_pops_byYear[[city]] <- vector("list", length(years_byCity[[city]])) %>% 
    set_names(names(years_byCity[[city]]))
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])

    
    BG_pops_byYear[[city]][[year]] <- 
      suppressMessages(aggregate(st_as_stars(worldpop_byCity_raster[[city]][[year]]),
                                 BG_selection_list$byPlace[[city]],
                                 FUN = sum,
                                 na.rm = TRUE)) %>% 
      st_as_sf() %>% 
      st_drop_geometry() %>% 
      rename(worldPop = 1) %>% 
      mutate(worldPop = round(worldPop)) %>% 
      cbind(BG_selection_list$byPlace[[city]]) %>% 
      dplyr::select(GEOID, worldPop)
    
  }
  
}

## 1. Export as rds ----
# saveRDS(years_byCity,
#         "~outputs/20/23_years_byCity.rds")
saveRDS(worldpop_byCity_raster,
        "~outputs/~large_files/23_worldpop_byCity_raster.rds")

## 2. Export as rds ----
# saveRDS(worldpop_grids,
#         "~outputs/20/23_worldpop_grids.rds")
# saveRDS(worldpop_data,
#         "~outputs/~large_files/23_worldpop_data.rds")

## 3. Export as rds ----
# saveRDS(BG_pops_byYear,
#         "~outputs/20/23_BG_pops_byYear.rds")
