##########################################################################
# This script:
# 1. Defines utility functions for use in the analysis.
#
# Exports:
# 1. base_map_bb(): Get a bounding box for use with the ggmap package's get_map() function
# 2. 
#
# To-do:
# 1. Moran's I calculator a given set of points and polygons containing those points
# 2. 
##########################################################################

## 1. ----
base_map_bb <- function(sf # this should be an sf object
                        ) {
  tmaptools::bb(sf, output = "matrix")
}

## 2. ----