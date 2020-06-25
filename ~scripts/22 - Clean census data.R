##########################################################################
# This script:
# 1. Selects census tract "study areas" for calculating Moran's I. 
#     Tracts were selected using:
#   a. Concave hull of gun crime observations
#   b. Convex hull of gun crime observations
#   c. All tracts in any county with at least 1 gun crime observation
#   d. Census-designated places matching the city/county name
# 2. Selects census block groups "study areas" for same purpose. 
#     BGs were selected using:
#   a. Concave hull of gun crime observations
#   b. Convex hull of gun crime observations
#   c. All BGs in any county with at least 1 gun crime observation
#   d. Census-designated places matching the city/county name
#
# Exports: 
# 1. tract_selection_list: a list containing 1a-1d above.
# 2. BG_selection_list: a list containing 2a-2d above.
# 
# To-do:
# 1. 
#
##########################################################################

## 1a. ----
# Create a concave hull of gun crime observations
# Select census tracts within the concave hull

  # allStateTracts <- readRDS("~outputs/10/12_allStateTracts.rds")
cave_hulls <- map(guns_list_shp, concaveman)

tracts_byCaveHull <- intersect_geo_lists(allStateTracts, cave_hulls)

## 1b. ----
# Create a convex hull of gun crime observations
# Select census tracts within the convex hull
plan(multiprocess)
vex_hulls <- future_map(guns_list_shp,
                        ~ st_union(.x) %>%
                          st_convex_hull() %>%
                          st_sf(),
                        .progress = TRUE)

tracts_byVexHull <- intersect_geo_lists(allStateTracts, vex_hulls)

## 1c. ----
# Select counties that have at least one gun crime observations
# Then select tracts in those counties

  # counties <- readRDS("~outputs/10/12_counties.rds")
counties_trim <- intersect_geo_lists(counties, guns_list_shp)

tracts_byCounty <- intersect_geo_lists(allStateTracts, counties_trim)

## 1d. ----
# Select by Census "place". Manually add cities/counties where necessary
# Then select tracts in those places/counties

  # places <- readRDS("~outputs/~large_files/12_places.rds")
places2 <- map2(places,
                cities_list,
                ~ .x %>% 
                  filter(str_detect(NAME, 
                                    .y)))

places2$Indianapolis <- filter(places$Indianapolis,
                               NAME == "Indianapolis city (balance)")
places2$Nashville <- filter(places$Nashville,
                            NAME == "Nashville-Davidson metropolitan government (balance)")
places2$`Saint Paul` <- filter(places$`Saint Paul`,
                               NAME == "St. Paul")
places2$`Sacramento County` <- filter(counties$`Sacramento County`,
                                      NAMELSAD == "Sacramento County")
# note that the St. Louis data contains only a couple points inside St. Louis city (which is separate from St. Louis County)
places2$`St Louis County` <- filter(counties$`St Louis County`, 
                                      NAMELSAD == "St. Louis County")

tracts_byPlace <- intersect_geo_lists(allStateTracts, places2)

## Combine into a single list
tract_selection_list <- list(byCaveHull = tracts_byCaveHull,
                             byVexHull = tracts_byVexHull,
                             byCounty = tracts_byCounty,
                             byPlace = tracts_byPlace)

## 2a. ----
# Create a concave hull of gun crime observations
# Select census block groups within the concave hull

  # allStateBGs <- readRDS("~outputs/10/12_allStateBGs.rds")
BGs_byCaveHull <- intersect_geo_lists(allStateBGs, cave_hulls)

## 2b. ----
# Create a convex hull of gun crime observations
# Select census block groups within the convex hull
BGs_byVexHull <- intersect_geo_lists(allStateBGs, vex_hulls)

## 2c. ----
# Select counties that have at least one gun crime observations
# Then select block groups in those counties
BGs_byCounty <- intersect_geo_lists(allStateBGs, counties_trim)

## 2d. ----
# Select by Census "place". Manually add cities/counties where necessary
# Then select tracts in those places/counties
BGs_byPlace <- intersect_geo_lists(allStateBGs, places2)

## Combine into a single list
BG_selection_list <- list(byCaveHull = BGs_byCaveHull,
                          byVexHull = BGs_byVexHull,
                          byCounty = BGs_byCounty,
                          byPlace = BGs_byPlace)

## 1. Export as rds ----
# saveRDS(tract_selection_list,
#         "~outputs/20/22_tract_selection_list.rds")

## 1. Export as rds ----
# saveRDS(BG_selection_list,
#         "~outputs/20/22_BG_selection_list.rds")