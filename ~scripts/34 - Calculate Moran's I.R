##########################################################################
# This script:
# 1. Calculates Moran's I for:
#   a. Census tracts (gun crimes and population)
#   b. Block groups (gun crimes and population)
#
# Exports: 
# 1. tracts_I as 34_tracts_I.rds
# 2. BGs_I as 34_BGs_I.rds
#
# To-do:
# 1. Calculate by time period
##########################################################################

# Source: https://crd150.github.io/lab5.html

## 1a. ----
# tracts_crimeCounts <- readRDS("~outputs/30/33_tracts_crimeCounts.rds")
tracts_I <- map(tracts_crimeCounts,
                function(x) map(x,
                                function(y) find_Moran_I(shp = y,
                                                         var_name = "gun_count",
                                                         queen = TRUE,
                                                         style = "W")))

tracts_pop_I <- map(tracts_crimeCounts,
                    function(x) map(x,
                                    function(y) find_Moran_I(shp = y,
                                                             var_name = "estimate",
                                                             queen = TRUE,
                                                             style = "W")))



## 1b. ----
# BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")
BGs_I <- map(BGs_crimeCounts,
             function(x) map(x,
                             function(y) find_Moran_I(shp = y,
                                                      var_name = "gun_count",
                                                      queen = TRUE,
                                                      style = "W")))

BGs_pop_I <- map(BGs_crimeCounts,
             function(x) map(x,
                             function(y) find_Moran_I(shp = y,
                                                      var_name = "estimate",
                                                      queen = TRUE,
                                                      style = "W")))

## 1. Export as rds ----
# saveRDS(tracts_I,
#         file = "~outputs/30/34_tracts_I.rds")
# saveRDS(tracts_pop_I,
#         file = "~outputs/30/34_tracts_pop_I.rds")

## 2. Export as rds ----
# saveRDS(BGs_I,
#         file = "~outputs/30/34_BGs_I.rds")
# saveRDS(BGs_pop_I,
#         file = "~outputs/30/34_BGs_pop_I.rds")