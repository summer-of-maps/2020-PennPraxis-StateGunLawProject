##########################################################################
# This script:
# 1. Counts all geolocated crimes and crimes per 100 people by geographies collected in 12.R
#   a. Tracts
#   b. Block groups
# 2. Counts by year for block groups selected by concave hull
# 3. Counts for worldPop grid cells.
#   NB: Use these on an aggregate basis, e.g. in the area around schools or within parks.
#   a. Overall
#   b. By year
#
# Exports: 
# 1. tracts_crimeCounts: tract_selection_list with a new column counting crimes committed in the tract
# 2. BGs_crimeCounts: BG_selection_list with a new column counting crimes committed in the BG
# 3. BGs_crimeCounts_byYear as 33_BGs_crimeCounts_byYear.rds
# 4. worldpop_crimeCounts as 33_worldpop_crimeCounts.rds
# 5. worldpop_crimeCounts_byYear as 33_worldpop_crimeCounts_byYear.rds
# 
# To-do:
# 1. 
#
##########################################################################

## 1a. ----
# tract_selection_list <- readRDS("~outputs/20/22_tract_selection_list.rds")
tracts_crimeCounts <- map(tract_selection_list, # loop over the 4 sets of tracts
                          .f = function(a)
                            map2(a, # loop over all cities and crimes for the cities
                                 guns_list_shp,
                                 function(x, y) 
                                   x %>%
                                   dplyr::select(GEOID,
                                                 NAME,
                                                 variable,
                                                 pop = estimate,
                                                 geometry) %>%
                                   mutate(gun_count = lengths(st_intersects(x,
                                                                            y)),
                                          guns_per100 = (gun_count / pop) * 100,
                                          guns_per100 = ifelse(is.na(guns_per100), 0,
                                                               # turn inf values to avg of non-inf values
                                                               ifelse(is.infinite(guns_per100),
                                                                      mean(guns_per100 *
                                                                             is.finite(guns_per100), 
                                                                           na.rm = TRUE),
                                                                      guns_per100)),
                                          guns_per100 = ifelse(guns_per100 > quantile(guns_per100, 0.995),
                                                               mean(guns_per100 *
                                                                      is.finite(guns_per100)),
                                                               guns_per100))))

## 1b. ----
# BG_selection_list <- readRDS("~outputs/20/22_BG_selection_list.rds")
BGs_crimeCounts <- map(BG_selection_list, # loop over the 4 sets of BGs
                       .f = function(a)
                         map2(a, # loop over all cities and crimes for the cities
                              guns_list_shp,
                              function(x, y) 
                                x %>%
                                dplyr::select(GEOID,
                                              NAME,
                                              variable,
                                              pop = estimate,
                                              geometry) %>%
                                mutate(gun_count = lengths(st_intersects(x,
                                                                         y)),
                                       guns_per100 = (gun_count / pop) * 100,
                                       guns_per100 = ifelse(is.na(guns_per100), 0,
                                                            # turn inf values to avg of non-inf values
                                                            ifelse(is.infinite(guns_per100),
                                                                   mean(guns_per100 *
                                                                          is.finite(guns_per100), 
                                                                        na.rm = TRUE),
                                                                   guns_per100)),
                                       guns_per100 = ifelse(guns_per100 > quantile(guns_per100, 0.995),
                                                            mean(guns_per100 *
                                                                   is.finite(guns_per100)),
                                                            guns_per100))))

## 2. ----
# note that this uses worldPop population data for every year rather than census geographies to ensure that
# geometries are the same every year.

# BG_pops_byYear <- readRDS("~outputs/20/23_BG_pops_byYear.rds")
BGs_crimeCounts_byYear <- vector("list", length(BG_pops_byYear)) %>% 
  set_names(names(BG_pops_byYear))

for (city in seq_len(length(BGs_crimeCounts_byYear))) {
  print(names(BGs_crimeCounts_byYear)[city])
  
  BGs_crimeCounts_byYear[[city]] <- vector("list", length(years_byCity[[city]])) %>% 
    set_names(names(years_byCity[[city]]))
  
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    
    BGs_crimeCounts_byYear[[city]][[year]] <- BG_selection_list$byPlace[[city]] %>% 
      dplyr::select(GEOID, geometry) %>% 
      left_join(BG_pops_byYear[[city]][[year]],
                by = "GEOID") %>% 
      {suppressMessages(mutate(.,
                               gun_count = lengths(st_intersects(., 
                                                                 guns_list_shp_byYear[[city]][[year]])),
                               guns_per100 = (gun_count / worldPop) * 100,
                               guns_per100 = ifelse(is.na(guns_per100), 0,
                                                    # turn inf values to avg of non-inf values
                                                    ifelse(is.infinite(guns_per100),
                                                           mean(guns_per100 * is.finite(guns_per100),
                                                                na.rm = TRUE),
                                                           guns_per100)),
                               ntile_guns_per100 = ntile(guns_per100, 100),
                               # turn values above 99th percentile to avg of all other vals
                               guns_per100 = ifelse(guns_per100 > quantile(guns_per100, 0.99), 
                                                    mean(guns_per100, na.rm = TRUE),
                                                    guns_per100)))
      } %>% 
      st_drop_geometry()
    
  }
  
}

## 3a. ----
worldpop_grids <- readRDS("~outputs/20/23_worldpop_grids.rds")
worldpop_data <- readRDS("~outputs/~large_files/23_worldpop_data.rds")
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")

worldpop_crimeCounts <- vector("list", length(worldpop_grids))

for (city in seq_len(length(worldpop_grids))) {
  print(names(worldpop_grids)[city])
  
  worldpop_crimeCounts[[city]] <- worldpop_grids[[city]] %>% 
    mutate(
      gun_count = lengths({suppressMessages(st_intersects(.,
                                                          guns_list_shp[[city]]))})) %>% 
    st_drop_geometry()
  }

worldpop_crimeCounts <- set_names(worldpop_crimeCounts,
                                  names(worldpop_grids))

## 3b. ----
worldpop_crimeCounts_byYear <- vector("list", length(worldpop_grids))

for (city in seq_len(length(worldpop_grids))) {
  print(names(worldpop_grids)[city])
  
  worldpop_crimeCounts_byYear[[city]] <- vector("list", length(years_byCity[[city]]))
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    worldpop_crimeCounts_byYear[[city]][[year]] <- worldpop_grids[[city]] %>% 
      mutate(
        gun_count = lengths({suppressMessages(st_intersects(.,
                                          guns_list_shp_byYear[[city]][[year]]))})) %>% 
      st_drop_geometry()
    
  }
  
  
}

worldpop_crimeCounts_byYear <- set_names(worldpop_crimeCounts_byYear,
                                         names(worldpop_grids))
worldpop_crimeCounts_byYear <- map2(worldpop_crimeCounts_byYear,
                                    years_byCity,
                                    ~ set_names(.x,
                                                names(.y)))

for (city in seq_len(length(worldpop_crimeCounts_byYear))) {
  print(names(worldpop_crimeCounts_byYear)[city])
  
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    
    worldpop_crimeCounts_byYear[[city]][[year]] <- 
      left_join(worldpop_crimeCounts_byYear[[city]][[year]],
                worldpop_data[[city]][[year]],
                by = "cell_ID")
  }
  
}


## 1a. Export as rds ----
# saveRDS(tracts_crimeCounts,
#         "~outputs/30/33_tracts_crimeCounts.rds")

## 1b. Export as rds ----
# saveRDS(BGs_crimeCounts,
#         "~outputs/30/33_BGs_crimeCounts.rds")

## 2. Export as rds ----
# saveRDS(BGs_crimeCounts_byYear,
#         "~outputs/30/33_BGs_crimeCounts_byYear.rds")

## 3a. Export as rds ----
# saveRDS(worldpop_crimeCounts,
#         "~outputs/~large_files/33_worldpop_crimeCounts.rds")

## 3b. Export as rds ----
# saveRDS(worldpop_crimeCounts_byYear,
#         "~outputs/~large_files/33_worldpop_crimeCounts_byYear.rds")