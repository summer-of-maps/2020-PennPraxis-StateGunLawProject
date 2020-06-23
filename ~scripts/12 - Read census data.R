##########################################################################
# This script:
# 1. Reads in census geographies for San Francisco
# 2. Reads in census tracts for all states
# 3. Reads in census block groups for all states
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
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

saveRDS(counties,
        file = "~outputs/10/12_counties.rds")
counties <- readRDS("~outputs/10/12_counties.rds")

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

saveRDS(allStateTracts,
        file = "~outputs/10/12_allStateTracts.rds")
allStateTracts <- readRDS("~outputs/10/12_allStateTracts.rds")

# places
places <- map(states_list,
              ~ places(state = .x,
                       year = 2018,
                       class = "sf") %>% 
                st_transform(4326))

saveRDS(places,
        file = "~outputs/~large_files/12_places.rds",
        compress = TRUE)
places <- readRDS("~outputs/~large_files/12_places.rds")

## 3. ----
allStateBGs <- map(states_list,
                   possibly(~ get_acs(geography = "block group",
                             year = 2018,
                             variables = "B01003_001E", # population
                             state = .x,
                             survey = "acs5",
                             geometry = TRUE) %>% 
                              st_transform(4326),
                            otherwise = NA_character_))

saveRDS(allStateBGs,
        file = "~outputs/10/12_allStateBGs.rds",
        compress = TRUE)
allStateTracts <- readRDS("~outputs/10/12_allStateBGs.rds")