##########################################################################
# This script:
# 1. Finds Siegel Score by sum of laws
#
# Exports: 
# 1. siegelSum as 20_siegelSum.RDS
# 
# To-do:
# 1. 
##########################################################################

## 1. ----
# siegelSum <- readRDS("~outputs/20/20_siegelSum.RDS")
siegelSum <- siegel_raw %>% 
  dplyr::select(1:4) %>% 
  mutate(exist = as.numeric(exist)) %>% 
  spread(key = law,
         value = exist) %>% 
  mutate(score = rowSums(.[3:136])) %>% 
  dplyr::select(state, year, score, everything())

siegelSum_list <- split(siegelSum,
                        siegelSum$state)

## 1. Export as RDS ----
# saveRDS(siegelSum,
#         "~outputs/20/20_siegelSum.RDS")

test <- siegel_raw %>% filter(year == 2019,
                              str_detect(law, "pree"))
