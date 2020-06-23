##########################################################################
# This script:
# 1. Collects ggmap basemaps for every city 
# 2. Creates maps of all geo-located guns crimes in every city
#
# Exports: 
# 1. basemap_list as 31a_basemap_list.rds
# 2. ggmap_list as 31a_ggmap_list.rds (large file)
# 3. Plots/31a_gunCrimes/31a_[city]_gunCrimes.png
# 
# To-do:
# 1. 
##########################################################################

## 1. ----
basemap_list <- map(guns_list_shp,
                    ~ get_map(base_map_bb(.x),
                              source = "stamen",
                              maptype = "toner-background",
                              color = "bw"))

l <- list(basemaps = basemap_list,
         cityNames = names(basemap_list),
         gun_points = guns_list_shp)

# 2. ----
ggmap_list <- pmap(.l = l,
                  ~ with(list(...),
                         ggmap(basemaps) +
                           geom_sf(data = gun_points,
                                   color = "#d7301f",
                                   alpha = 0.25,
                                   inherit.aes = FALSE) +
                           labs(title = paste("Gun crime incidents in", cityNames))))

## 1. Export basemap_list as RDS ----
saveRDS(basemap_list,
        file = "~outputs/30/31a_basemap_list.rds")
basemap_list <- readRDS("~outputs/30/31a_basemap_list.rds")

## 2. Export ggmap_list as RDS ----
saveRDS(ggmap_list,
        file = "~outputs/~large_files/31a_ggmap_list.rds")

## 3. Export ggmap_list as PNGs ----
map2(ggmap_list,
     names(ggmap_list),
    ~ ggsave(plot = .x,
             filename = paste("~outputs/Plots/31a_gunCrimes/31a_",
                              .y,
                              "_gunCrimes.png",
                              sep = "")),
    units = "in",
    height = 8)