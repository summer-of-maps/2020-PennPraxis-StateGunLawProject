##########################################################################
# This script:
# 1. Creates bboxes with a buffer for map context
# 2. Downloads hydrology for each city's context bbox
# 3. Downloads roads for each city's context bbox
#
# Exports: 
# 1. context_bbox_list as 15_context_bbox_list.rds
# 2. context_mask_list as 15_context_mask_list.rds
# 3. hydrology_list as 15_hydrology_list.rds
# 4. roads_list as 15_roads_list.rds
# 5. land_list as 15_land_list.rds
# 
# To-do:
# 1. 
#
##########################################################################

## 1. ----
city_bounds <- readRDS("~outputs/10/15_city_bounds.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")

context_buffer_size <- 5280 * 1 # one mile

context_bbox_list <- map2(
  city_bounds,
  proj_list,
  ~ .x %>% 
    st_convex_hull() %>% 
    st_transform(.y) %>% 
    st_make_grid(n = 1) %>% 
    st_sf() %>% 
    st_buffer(context_buffer_size) %>% 
    st_transform(4326) %>% 
    st_make_grid(n = 1) %>% 
    st_sf()
  )

context_mask_list <- map2(
  context_bbox_list,
  city_bounds,
  ~ st_difference(.x, 
                  st_union(.y))
)

## 2. ----
hydrology_list <- map2(
  city_bounds,
  context_bbox_list,
  function(city, context) {
    
    state <- city$STATEFP[1]
    
    counties <- counties(state,
                         class = "sf",
                         cb = TRUE) %>% 
      st_transform(4326) %>% 
      mutate(overlaps = lengths(st_overlaps(., context)),
             equals = lengths(st_equals(., city))) %>% 
      filter(overlaps > 0 | equals > 0) %>% 
      .$COUNTYFP
    
    map_df(counties,
           function(county) {
             area_water(state,
                        county = county,
                        class = "sf") %>% 
               st_transform(4326) %>% 
               st_intersection(context)
           }
           )
    
  }
)

cincy_tmp <- 
  counties("Kentucky",
           class = "sf",
           cb = TRUE) %>% 
  st_transform(4326) %>% 
  mutate(overlaps = lengths(st_overlaps(., context_bbox_list$Cincinnati)),
         equals = lengths(st_equals(., city_bounds$Cincinnati))) %>% 
  filter(overlaps > 0 | equals > 0) %>% 
  .$COUNTYFP
cincy_tmp <- 
  map_df(cincy_tmp,
         function(county) {
           area_water("Kentucky",
                      county = county,
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$Cincinnati)
         }
  )
hydrology_list$Cincinnati <- bind_rows(
  hydrology_list$Cincinnati,
  cincy_tmp
)

hydrology_list$Gainesville <- area_water("Florida",
                                      county = "Alachua",
                                      class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Gainesville)

hydrology_list$Hartford <- area_water("Connecticut",
           county = "Hartford",
           class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Hartford)

kc_tmp <- 
  counties("Kansas",
           class = "sf",
           cb = TRUE) %>% 
  st_transform(4326) %>% 
  mutate(overlaps = lengths(st_overlaps(., context_bbox_list$`Kansas City`)),
         equals = lengths(st_equals(., city_bounds$`Kansas City`))) %>% 
  filter(overlaps > 0 | equals > 0) %>% 
  .$COUNTYFP
kc_tmp <- 
  map_df(kc_tmp,
         function(county) {
           area_water("Kansas",
                      county = county,
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$`Kansas City`)
         }
  )
hydrology_list$`Kansas City` <- bind_rows(
  hydrology_list$`Kansas City`,
  kc_tmp
)

hydrology_list$Lincoln <- area_water("Nebraska",
                                         county = "Lancaster",
                                         class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Lincoln)

hydrology_list$Hartford <- area_water("Connecticut",
                                      county = "Hartford",
                                      class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Hartford)

hydrology_list$Madison <- area_water("Wisconsin",
                                     county = "Dane",
                                     class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Madison)

philly_tmp <- 
  counties("New Jersey",
           class = "sf",
           cb = TRUE) %>% 
  st_transform(4326) %>% 
  mutate(overlaps = lengths(st_overlaps(., context_bbox_list$Philadelphia)),
         equals = lengths(st_equals(., city_bounds$Philadelphia))) %>% 
  filter(overlaps > 0 | equals > 0) %>% 
  .$COUNTYFP
philly_tmp <- 
  map_df(philly_tmp,
         function(county) {
           area_water("New Jersey",
                      county = county,
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$Philadelphia)
         }
  )
hydrology_list$Philadelphia <- bind_rows(
  hydrology_list$Philadelphia,
  philly_tmp)

portland_tmp <- 
  counties("Washington",
           class = "sf",
           cb = TRUE) %>% 
  st_transform(4326) %>% 
  mutate(overlaps = lengths(st_overlaps(., context_bbox_list$Portland)),
         equals = lengths(st_equals(., city_bounds$Portland))) %>% 
  filter(overlaps > 0 | equals > 0) %>% 
  .$COUNTYFP
portland_tmp <- 
  map_df(portland_tmp,
         function(county) {
           area_water("Washington",
                      county = county,
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$Portland)
         }
  )
hydrology_list$Portland <- bind_rows(
  hydrology_list$Portland,
  portland_tmp)

NYC_tmp <- 
  counties("New Jersey",
           class = "sf",
           cb = TRUE) %>% 
  st_transform(4326) %>% 
  mutate(overlaps = lengths(st_overlaps(., context_bbox_list$`New York`)),
         equals = lengths(st_equals(., city_bounds$`New York`))) %>% 
  filter(overlaps > 0 | equals > 0) %>% 
  .$COUNTYFP
NYC_tmp <- 
  map_df(NYC_tmp,
         function(county) {
           area_water("New Jersey",
                      county = county,
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$`New York`)
         }
  )
NYC_tmp2 <- area_water("New York",
                      county = "Richmond",
                      class = "sf") %>% 
             st_transform(4326) %>% 
             st_intersection(context_bbox_list$`New York`)
NYC_tmp3 <- area_water("New York",
                       county = "New York",
                       class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`New York`)
NYC_tmp4 <- area_water("New York",
                       county = "Bronx",
                       class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`New York`)
NYC_tmp5 <- area_water("New Jersey",
                       county = "Hudson",
                       class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`New York`)
hydrology_list$`New York` <- bind_rows(
  hydrology_list$`New York`,
  NYC_tmp,
  NYC_tmp2,
  NYC_tmp3,
  NYC_tmp4,
  NYC_tmp5)

hydrology_list$Tucson <- area_water("Arizona",
                                     county = "Pima",
                                     class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Tucson)


## 3. ----
roads_list <- map2(
  city_bounds,
  context_bbox_list,
  function(city, context) {
    
    state <- city$STATEFP[1]
    
    counties <- primary_secondary_roads(state,
                         class = "sf") %>% 
      st_transform(4326) %>% 
      st_intersection(context) %>% 
      st_make_valid() %>% 
      filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
    }
)

cincy_tmp <- primary_secondary_roads("Kentucky",
                                  class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Cincinnati) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
roads_list$Cincinnati <- bind_rows(
  roads_list$Cincinnati,
  cincy_tmp)

KC_tmp <- primary_secondary_roads("Kansas",
                                  class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`Kansas City`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
roads_list$`Kansas City` <- bind_rows(
  roads_list$`Kansas City`,
  KC_tmp)
  
philly_tmp <- primary_secondary_roads("New Jersey",
                                  class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Philadelphia) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
roads_list$Philadelphia <- bind_rows(
  roads_list$Philadelphia,
  philly_tmp)







portland_tmp <- primary_secondary_roads("Washington",
                                      class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Portland) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
roads_list$Portland <- bind_rows(
  roads_list$Portland,
  portland_tmp)

NYC_tmp <- primary_secondary_roads("New Jersey",
                                      class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`New York`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
roads_list$`New York` <- bind_rows(
  roads_list$`New York`,
  NYC_tmp
)

roads_list <- map(
  roads_list,
  ~ .x %>% 
    mutate(road_type = case_when(RTTYP %in% c("I", "S") ~ "Major",
                                 RTTYP %in% c("U", "M", "C", "O") ~ "Minor",
                                 TRUE ~ "Minor")))

## 4. ----
land_list <- map2(
  city_bounds,
  context_bbox_list,
  function(city, context) {
    
    state <- city$STATEFP[1]
    
    counties <- counties(state,
                         class = "sf") %>% 
      st_transform(4326) %>% 
      st_intersection(context)
    
  }
)

cincy_tmp <- counties("Kentucky",
                   class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Cincinnati)
land_list$Cincinnati <- bind_rows(
  land_list$Cincinnati,
  cincy_tmp)

KC_tmp <- counties("Kansas",
                   class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`Kansas City`)
land_list$`Kansas City` <- bind_rows(
  land_list$`Kansas City`,
  KC_tmp)

philly_tmp <- counties("New Jersey",
                       class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Philadelphia)
land_list$Philadelphia <- bind_rows(
  land_list$Philadelphia,
  philly_tmp)

portland_tmp <- counties("Washington",
                       class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$Portland)
land_list$Portland <- bind_rows(
  land_list$Portland,
  portland_tmp)

NYC_tmp <- counties("New Jersey",
                    class = "sf") %>% 
  st_transform(4326) %>% 
  st_intersection(context_bbox_list$`New York`)
land_list$`New York` <- bind_rows(
  land_list$`New York`,
  NYC_tmp
)

## 1. Export as rds ----
saveRDS(context_bbox_list,
        "~outputs/10/15_context_bbox_list.rds")
saveRDS(context_mask_list,
        "~outputs/10/15_context_mask_list.rds")

## 2. Export as rds ----
saveRDS(hydrology_list,
        "~outputs/10/15_hydrology_list.rds")

## 3. Export as rds ----
saveRDS(roads_list,
        "~outputs/10/15_roads_list.rds")

## 4. Export as rds ----
saveRDS(land_list,
        "~outputs/10/15_land_list.rds")

