##########################################################################
# This script:
# 1. Reads in census geographies for San Francisco
# 2. Reads in census tracts for all states
# 3. Reads in census block groups for all states
# 4. Reads in demographic and socio-economic data for block gorups
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
#
##########################################################################

## 1. ----
# sf_tracts_2018 <- get_acs(geography = "tract",
#                           variables = "B01003_001E", # population
#                           year = 2018,
#                           state = "California",
#                           county = "San Francisco",
#                           survey = "acs5",
#                           geometry = TRUE)

## 2. ----
# states and cities
states_list <- map(guns_list,
                   ~ .x$state[1])

cities_list <- map(guns_list,
                   ~ .x$city[1])

# counties
counties <- map(states_list,
                ~ counties(state = .x,
                           year = 2018,
                           class = "sf") %>% 
                  st_transform(4326))

# saveRDS(counties,
#         file = "~outputs/10/12_counties.rds")

# tracts
allStateTracts <- map(states_list,
                      # possibly() catches errors if the API is down
                      possibly(~ get_acs(geography = "tract",
                                year = 2018,
                                variables = "B01003_001E", # population
                                state = .x,
                                survey = "acs5",
                                geometry = TRUE) %>% 
                                 st_transform(4326),
                               otherwise = NA_character_))

# saveRDS(allStateTracts,
#         file = "~outputs/10/12_allStateTracts.rds")

# places
# places <- map(states_list,
#               ~ places(state = .x,
#                        year = 2018,
#                        class = "sf") %>% 
#                 st_transform(4326))


### USE CITY_BOUNDS FROM SCRIPT 14 INSTEAD
places <-  readRDS("~outputs/10/14_city_bounds.rds")

saveRDS(places,
        file = "~outputs/~large_files/12_places.rds",
        compress = TRUE)

## 3. ----
allStateBGs <- readRDS("~outputs/10/12_allStateBGs.rds")
allStateBGs <- map(states_list,
                   possibly(~ get_acs(geography = "block group",
                             year = 2018,
                             variables = "B01003_001E", # population
                             state = .x,
                             survey = "acs5",
                             geometry = TRUE) %>% 
                              st_transform(4326),
                            otherwise = NA_character_))

# saveRDS(allStateBGs,
#         file = "~outputs/10/12_allStateBGs.rds",
#         compress = TRUE)

## 4. ----
# List of 2018 ACS variables: https://api.census.gov/data/2018/acs/acs5/variables.html
census_df <- data.frame(vars =     c("B01003_001", 
                                     "B19013_001", 
                                     "B01002_001", 
                                     "B02001_002",
                                     "B02001_003",
                                     "B03001_003",
                                     "B06009_005",
                                     "B06009_001"),
                        
                        colNames = c("TotPop",
                                     "MdHHInc",
                                     "MdAge",
                                     "White_Pop",
                                     "Black_Pop",
                                     "Hisp_Pop",
                                     "CollGrad",
                                     "EduPop"),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

# Download for all states
allState_censusDat_BGs_raw <- map(states_list,
                   possibly(~ get_acs(geography = "block group",
                                      year = 2018,
                                      variables = census_vars, # population
                                      state = .x,
                                      output = "wide",
                                      survey = "acs5",
                                      geometry = FALSE) %>%
                              rename_census_cols(x = .,
                                                 vars = census_vars,
                                                 names = census_colNames,
                                                 drop_MOE = TRUE),
                            otherwise = NA_character_))

                   
## 4. Export as rds ----
# saveRDS(allState_censusDat_BGs_raw,
#         "~outputs/10/12_allState_censusDat_BGs_raw.rds")
