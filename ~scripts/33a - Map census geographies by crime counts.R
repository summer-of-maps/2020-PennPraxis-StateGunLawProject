##########################################################################
# This script:
# 1. Maps geographies by total crime count
#   a. Tracts
#   b. Block groups
#
# Exports: 
# 1. basemap_list as 31a_basemap_list.rds
# 2. ggmap_list as 31a_ggmap_list.rds (large file)
# 3. Plots/33a_tract_crime_maps/[geography]/33a_[City]_tract_crime_maps_.png
# 4. Plots/33a_BG_crime_maps/[geography]/33a_[City]_BG_crime_maps_.png
# 
# To-do:
# 1. Fix ggmap extents
##########################################################################

# import basemaps
# basemap_list <- readRDS("~outputs/30/31a_basemap_list.rds")

## 1a. ----
# tracts_crimeCounts <- readRDS("~outputs/30/33_tracts_crimeCounts.rds")

tract_crime_maps <- map(tracts_crimeCounts,
                        .f = function(geo_set) 
                          map2(geo_set,
                               basemap_list,
                               .f = function(cityCrime, cityBasemap)
                                 ggmap(cityBasemap) +
                                 ## below has quintiles
                                 geom_sf(data = cityCrime,
                                         aes(fill = q5(gun_count)),
                                         color = "lightgray",
                                         alpha = 0.5,
                                         inherit.aes = FALSE) +
                                 scale_fill_viridis_d(labels = qBr(cityCrime,
                                                                   "gun_count"),
                                                      name = "Quintile\nBreaks") +
                                 ## below has continuous
                                 # geom_sf(data = cityCrime,
                                 #         aes(fill = gun_count),
                                 #         color = "lightgray",
                                 #         alpha = 0.5,
                                 #         inherit.aes = FALSE) +
                                 # scale_fill_viridis_c(name = "Gun Crime/nCount") +
                                 mapTheme() +
                                 labs(title = "Gun Crimes by Census Tract")))
                        )

## 1b. ----
# BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")

BG_crime_maps <- map(BGs_crimeCounts,
                        .f = function(geo_set) 
                          map2(geo_set,
                               basemap_list,
                               .f = function(cityCrime, cityBasemap)
                                 ggmap(cityBasemap) +
                                 ## below has quintiles
                                 geom_sf(data = cityCrime,
                                         aes(fill = q5(gun_count)),
                                         color = "lightgray",
                                         alpha = 0.5,
                                         inherit.aes = FALSE) +
                                 scale_fill_viridis_d(labels = qBr(cityCrime,
                                                                   "gun_count"),
                                                      name = "Quintile\nBreaks") +
                                 ## below has continuous
                                 # geom_sf(data = cityCrime,
                                 #         aes(fill = gun_count),
                                 #         color = "lightgray",
                                 #         alpha = 0.5,
                                 #         inherit.aes = FALSE) +
                                 # scale_fill_viridis_c(name = "Gun Crime/nCount") +
                                 mapTheme() +
                                 labs(title = "Gun Crimes by Block Group")))
)

## 2a. Export maps as pngs ----
map2(tract_crime_maps,
     names(tract_crime_maps),
    function(geo, geoType)
      map2(geo,
           names(geo),
           function(a, b) ggsave(plot = a,
              filename = paste("~outputs/Plots/33a_tract_crime_maps/",
                               geoType,
                               "/33a_",
                               b,
                               "_tract_crime_maps_",
                               # geoType,
                               ".png",
                               sep = ""),
              device = "png",
              units = "in",
              dpi = 72,
              width = 7,
              height = 7)))

## 2b. Export maps as pngs ----
map2(BG_crime_maps,
     names(BG_crime_maps),
     function(geo, geoType)
       map2(geo,
            names(geo),
            function(a, b) ggsave(plot = a,
                                  filename = paste("~outputs/Plots/33a_BG_crime_maps/",
                                                   geoType,
                                                   "/33a_",
                                                   b,
                                                   "_BG_crime_maps_",
                                                   # geoType,
                                                   ".png",
                                                   sep = ""),
                                  device = "png",
                                  units = "in",
                                  dpi = 72,
                                  width = 7,
                                  height = 7)))
