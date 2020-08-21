##########################################################################
# This script:
# 1. Reads in the worldpop data
#   a. For 2018
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
#
##########################################################################

## 1a. ----
tmp <- list.files("~outputs/~large_files/worldPop/", 
                  full.names = TRUE)
tmp_names <- sapply(tmp,
                    function(x) str_extract_all(x,
                                                "[0-9]+"))

worldpop_list <- map(tmp,
            ~ raster(.x)) %>% 
  set_names(tmp_names)


worldpop_2018 <- raster("~outputs/~large_files/worldPop/usa_ppp_2018_UNadj.tif")

