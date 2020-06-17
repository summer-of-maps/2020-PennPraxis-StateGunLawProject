##########################################################################
# This script:
# 1. Defines utility functions for use in the analysis.
#
# Exports:
# 1. base_map_bb(): Get a bounding box for use with the ggmap package's get_map() function
# 2. replace_lat_lon_cols(): some cities in the guns dataset have lon/lat in projected coordinates.
#     This replaces them with coordinates in a new crs (default WGS84)
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
replace_lat_lon_cols <- function(data, # e.g., df with lon/lat columns guns_list$Dallas
                                 current_crs, # the current proj for the columns
                                 new_crs = 4326 # the desired proj
) {
  
  tmp_col_names <- names(data)
  data$tmp_ID <- 1:nrow(data)
  
  tmp <- data %>%
    filter(!is.na(lon),
           !is.na(lat)) %>% 
    # make sf object using raw coordinates, which are in 2276 projection
    st_as_sf(.,
             # lon/lat are flipped in the raw data. "lat" column contains the longitude coordinates.
             coords = c("lon", "lat"),
             crs = current_crs,
             remove = FALSE) %>% 
    # reproject to WGS84
    st_transform(crs = new_crs)
  
  tmp_coords <- st_coordinates(tmp) # get coordinates of tmp object
  
  tmp <- tmp %>% 
    # replace lon/lat column of tmp objects with the coordinates from tmp_coords
    mutate(lon = tmp_coords[, 1],
           lat = tmp_coords[, 2]) %>% 
    dplyr::select(lon, lat, tmp_ID) %>% 
    st_drop_geometry()
  
  data %>% 
    # merge new WGS84 coordinates back into Dallas dataframe
    dplyr::select(-c(lon, lat)) %>% 
    left_join(tmp, by = "tmp_ID") %>% 
    # reset the order of the columns and drop the tmp_ID column
    dplyr::select(all_of(tmp_col_names))
}
