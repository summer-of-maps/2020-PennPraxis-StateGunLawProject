##########################################################################
# This script:
# 1. Find city boundaries for reading and trimming OSM data
# 2. Sets projections for each city
# 3. Creates matrix bboxes for querying OSM data
# 4. Reads in OSM data
#   a. Bars (and liquor stores?)
#   b. Parks
# 5. Reads in Joe's prepared list of city gun laws
#
# Exports: 
# 1. city_bounds as 14_city_bounds.rds
# 2. proj_list as 14_proj_list.rds
# 
# To-do:
# 1. 
#
##########################################################################

## 1. ----
city_bounds <- vector("list",
                      length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

# Atlanta
city_bounds$Atlanta <- places("Georgia", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Atlanta") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Auburn
city_bounds$Auburn <- places("Washington", 
                             class = "sf",
                             cb = TRUE) %>% 
  filter(NAME == "Auburn") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Baltimore
city_bounds$Baltimore <- places("Maryland", 
                                class = "sf",
                                cb = TRUE) %>% 
  filter(NAME == "Baltimore") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Baton Rouge
city_bounds$`Baton Rouge` <- places("Louisiana", 
                                    class = "sf",
                                    cb = TRUE) %>% 
  filter(NAME == "Baton Rouge") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Boston
city_bounds$Boston <- places("Massachusetts", 
                             class = "sf",
                             cb = TRUE) %>% 
  filter(NAME == "Boston") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Chicago
city_bounds$Chicago <- places("Illinois", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Chicago") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Cincinnati
city_bounds$Cincinnati <- places("Ohio", 
                                 class = "sf",
                                 cb = TRUE) %>% 
  filter(NAME == "Cincinnati") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Columbia
city_bounds$Columbia <- places("South Carolina", 
                               class = "sf",
                               cb = TRUE) %>% 
  filter(NAME == "Columbia") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Dallas
city_bounds$Dallas <- places("Texas", 
                             class = "sf",
                             cb = TRUE) %>% 
  filter(NAME == "Dallas") %>% 
  st_cast("POLYGON") %>% 
  .[2:4,] %>% 
  st_transform(4326)

# Denver
city_bounds$Denver <- places("Colorado", 
                             class = "sf",
                             cb = TRUE) %>% 
  filter(NAME == "Denver") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Detroit
city_bounds$Detroit <- places("Michigan", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Detroit") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Gainesville
city_bounds$Gainesville <- places("Florida", 
                                  class = "sf",
                                  cb = TRUE) %>% 
  filter(NAME == "Gainesville") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Hartford
city_bounds$Hartford <- places("Connecticut", 
                               class = "sf",
                               cb = TRUE) %>% 
  filter(NAME == "Hartford") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Indianapolis
city_bounds$Indianapolis <- places("Indiana", 
                                   class = "sf",
                                   cb = TRUE) %>% 
  filter(NAME == "Indianapolis city (balance)") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Kansas City
city_bounds$`Kansas City` <- places("Missouri", 
                                    class = "sf",
                                    cb = TRUE) %>% 
  filter(NAME == "Kansas City") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Lincoln
city_bounds$Lincoln <- places("Nebraska", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Lincoln") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Little Rock
city_bounds$`Little Rock` <- places("Arkansas", 
                                    class = "sf",
                                    cb = TRUE) %>% 
  filter(NAME == "Little Rock") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Los Angeles
city_bounds$`Los Angeles` <- places("California", 
                                    class = "sf",
                                    cb = TRUE) %>% 
  filter(NAME == "Los Angeles") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Louisville
city_bounds$Louisville <- places("Kentucky", 
                                 class = "sf",
                                 cb = TRUE) %>% 
  filter(NAME == "Louisville") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Madison
city_bounds$Madison <- places("Wisconsin", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Madison") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Minneapolis
city_bounds$Minneapolis <- places("Minnesota", 
                                  class = "sf",
                                  cb = TRUE) %>% 
  filter(NAME == "Minneapolis") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Nashville
city_bounds$Nashville <- places("Tennessee", 
                                class = "sf",
                                cb = TRUE) %>% 
  filter(NAME == "Nashville-Davidson metropolitan government (balance)") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# New York City
city_bounds$`New York` <- counties("New York", 
                                   class = "sf",
                                   cb = TRUE) %>% 
  filter(NAME %in% c("Bronx",
                     "New York",
                     "Kings",
                     "Queens", 
                     "Richmond")) %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Philadelphia
city_bounds$Philadelphia <- places("Pennsylvania", 
                                   class = "sf",
                                   cb = TRUE) %>% 
  filter(NAME == "Philadelphia") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Phoenix
city_bounds$Phoenix <- places("Arizona", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Phoenix") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Portland
city_bounds$Portland <- places("Oregon", 
                               class = "sf",
                               cb = TRUE) %>% 
  filter(NAME == "Portland") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Raleigh
city_bounds$Raleigh <- places("North Carolina", 
                              class = "sf",
                              cb = TRUE) %>% 
  filter(NAME == "Raleigh") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Sacramento County
city_bounds$`Sacramento County` <- counties("California", 
                                            class = "sf",
                                            cb = TRUE) %>% 
  filter(NAME == "Sacramento") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Saint Paul
city_bounds$`Saint Paul` <- places("Minnesota", 
                                   class = "sf",
                                   cb = TRUE) %>% 
  filter(NAME == "St. Paul") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Salt Lake City
city_bounds$`Salt Lake City` <- places("Utah", 
                                       class = "sf",
                                       cb = TRUE) %>% 
  filter(NAME == "Salt Lake City") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# San Francisco
city_bounds$`San Francisco` <- places("California", 
                                      class = "sf",
                                      cb = TRUE) %>% 
  filter(NAME == "San Francisco") %>% 
  st_cast("POLYGON") %>% 
  rownames_to_column() %>% 
  filter(rowname %in% c("1.1", "1.3")) %>% 
  dplyr::select(-rowname) %>% 
  st_transform(4326)

# St. Louis County
city_bounds$`St Louis County` <- counties("Missouri", 
                                          class = "sf",
                                          cb = TRUE) %>% 
  filter(NAME == "St. Louis") %>% 
  st_cast("POLYGON") %>% 
  .[1,] %>% 
  st_transform(4326)

# Tucscon
city_bounds$Tucson <- places("Arizona", 
                             class = "sf",
                             cb = TRUE) %>% 
  filter(NAME == "Tucson") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Virginia Beach
city_bounds$`Virginia Beach` <- places("Virginia", 
                                       class = "sf",
                                       cb = TRUE) %>% 
  filter(NAME == "Virginia Beach") %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

## 2. ----
proj_list <- vector("list",
                   length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

suggested_crs_list <- map(city_bounds,
                          ~ suggest_crs(.x, units = "us-ft"))

proj_list$Atlanta <- 2240
proj_list$Auburn <- 2285
proj_list$Baltimore <- 2248
proj_list$`Baton Rouge` <- 3452
proj_list$Boston <- 2249
proj_list$Chicago <- 3435
proj_list$Cincinnati <- 2246
proj_list$Columbia <- 3642
proj_list$Dallas <- 2276
proj_list$Denver <- 2231
proj_list$Detroit <- 2253
proj_list$Gainesville <- 2238
proj_list$Hartford <- 2234
proj_list$Indianapolis <- 2244
proj_list$`Kansas City` <- 26798
proj_list$Lincoln <- 26852
proj_list$`Little Rock` <- 3434
proj_list$`Los Angeles` <- 2229
proj_list$Louisville <- 2246
proj_list$Madison <- 2289
proj_list$Minneapolis <- 26821
proj_list$Nashville <- 2274
proj_list$`New York` <- 2263
proj_list$Philadelphia <- 2272
proj_list$Phoenix <- 2223
proj_list$Portland <- 2269
proj_list$Raleigh <- 2264
proj_list$`Sacramento County` <- 2226
proj_list$`Saint Paul` <- 26821
proj_list$`Salt Lake City` <- 2560
proj_list$`San Francisco` <- 2227
proj_list$`St Louis County` <- 26796
proj_list$Tucson <- 2222
proj_list$`Virginia Beach` <- 2284

## 3. ----
bbox_mats <- map(city_bounds,
                 ~ .x %>% 
                   base_map_bb() %>% 
                   opq(timeout = 900, # 900 seconds
                       memsize = 1073741824 * 3))

## 4a. ----
alcohol_outlets <- vector("list",
                          length(bbox_mats)) %>% 
  set_names(names(bbox_mats))

for (city in seq_len(length(alcohol_outlets))) {
  
  print(names(alcohol_outlets)[city])
  
  mat <- bbox_mats[[city]]
  bounds <- city_bounds[[city]]
  
  bars <- mat %>% 
    add_osm_feature(key = "amenity",
                    value = c("bar",
                              "nightclubs")) %>% 
    osmdata_sf() %>% 
    unique_osmdata()
  
  liquor <- mat %>% 
    add_osm_feature(key = "shop",
                    value = "alcohol") %>% 
    osmdata_sf() %>% 
    unique_osmdata()
  
  outlets <- c(
    bars,
    liquor
  )
  
  if(!is.null(outlets$osm_polygons)) {
  
  alcohol_outlets[[city]] <- bind_rows(
    outlets$osm_points,
    outlets$osm_polygons %>% st_centroid()
    )
  
  } else {
    
    alcohol_outlets[[city]] <- outlets$osm_points
  
  }
  
  alcohol_outlets[[city]] <- alcohol_outlets[[city]] %>% 
    st_intersection(bounds)
  
}

alcohol_outlets <- map(
  alcohol_outlets,
  ~ .x %>% 
    mutate(legend = "Alcohol Outlet")
)

## 4b. ----
parks <- vector("list",
                length(bbox_mats)) %>% 
  set_names(names(bbox_mats))

for (city in seq_len(length(parks))) {
  
  print(names(parks)[city])
  
  mat <- bbox_mats[[city]]
  bounds <- city_bounds[[city]]
  
  parks_tmp <- mat %>% 
    add_osm_feature(key = "leisure",
                    value = c("park",
                              "pitch",
                              "dog_park",
                              "nature_reserve",
                              "playground")) %>% 
    osmdata_sf() %>% 
    unique_osmdata()
  
  if(!is.null(parks_tmp$osm_polygons) & !is.null(parks_tmp$osm_multipolygons)) {
    
    parks[[city]] <- bind_rows(
      parks_tmp$osm_polygons,
      parks_tmp$osm_multipolygons
    )
    
  } else if (!is.null(parks_tmp$osm_polygons) & is.null(parks_tmp$osm_multipolygons)) {
    
    parks[[city]] <- parks_tmp$osm_polygons
    
  } else if (is.null(parks_tmp$osm_polygons) & !is.null(parks_tmp$osm_multipolygons)) {
  
  parks[[city]] <- parks_tmp$osm_multipolygons
  
  }
  
  parks[[city]] <- parks[[city]] %>% 
    st_make_valid() %>% 
    st_intersection(bounds)
  
}

cincinnati_tmp <- bbox_mats$Cincinnati %>% 
  add_osm_feature(key = "leisure",
                  value = c("park",
                            "pitch",
                            "dog_park",
                            "nature_reserve",
                            "playground")) %>% 
  osmdata_sf() %>% 
  unique_osmdata()
cincinnati_tmp2 <- cincinnati_tmp$osm_polygons
cincinnati_tmp3 <- cincinnati_tmp$osm_multipolygons
parks$Cincinnati <- bind_rows(
  cincinnati_tmp2,
  cincinnati_tmp3
) %>% 
  st_make_valid() %>%
  st_intersection(city_bounds$Cincinnati) %>% 
  filter(st_is(., "POLYGON"))

parks <- map(
  parks,
  ~ .x %>% 
    mutate(legend = "Park")
)

## 5. ----
city_laws <- read_csv("City_Gun_Laws.csv") %>% 
  mutate(Parks_text = case_when(`Parks Allowed` == 1 ~ "Permitted",
                                `Parks Allowed` == 0 ~ "Banned",
                                TRUE ~ "Unknown"),
         Bars_text = case_when(`Bars Allowed` == 1 ~ "Permitted",
                               `Bars Allowed` == 0 ~ "Banned",
                               TRUE ~ "Unknown"))

city_laws_list <- city_laws %>% 
  split(city_laws$City)

## 1. Export as rds ----
saveRDS(city_bounds,
        "~outputs/10/14_city_bounds.rds")

## 2. Export as rds ----
saveRDS(proj_list,
        "~outputs/10/14_proj_list.rds")

## 3. Export as rds ----
saveRDS(bbox_mats,
        "~outputs/10/14_bbox_mats.rds")

## 4a. Export as rds ----
saveRDS(alcohol_outlets,
        "~outputs/10/14_alcohol_outlets.rds")

## 4b. Export as rds ----
saveRDS(parks,
        "~outputs/10/14_parks.rds")

## 5. Export as rds ----
saveRDS(city_laws_list,
        "~outputs/10/14_city_laws_list.rds")
