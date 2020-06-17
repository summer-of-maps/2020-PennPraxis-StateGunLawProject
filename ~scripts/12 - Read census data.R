##########################################################################
# This script:
# 1. Reads in census geographies for San Francisco
#
# Exports: 
# 1. 
# 
# To-do:
# 1. Generalize to all cities
# 2. Generalize to all years
#
##########################################################################

## 1. ----
sf_tracts_2018 <- get_acs(geography = "tract",
                          variables = "B01003_001E", # population
                          year = 2018,
                          state = "California",
                          county = "San Francisco",
                          survey = "acs5",
                          geometry = TRUE)


