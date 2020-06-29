##########################################################################
# This script:
# 1. Counts all geolocated crimes by geographies collected in 12.R
#   a. Tracts
#   b. Block groups
# 2. Counts crimes per 100 people.
#
# Exports: 
# 1. tracts_crimeCounts: tract_selection_list with a new column counting crimes committed in the tract
# 2. BGs_crimeCounts: BG_selection_list with a new column counting crimes committed in the BG
# 
# To-do:
# 1. Count crimes by rolling window 
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
                                                                         guns_per100)))))

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
                                                                   guns_per100)))))

## 2a. ----




## 1. Export as rds ----
# saveRDS(tracts_crimeCounts,
#         "~outputs/30/33_tracts_crimeCounts.rds")

## 2. Export as rds ----
# saveRDS(BGs_crimeCounts,
#         "~outputs/30/33_BGs_crimeCounts.rds")
