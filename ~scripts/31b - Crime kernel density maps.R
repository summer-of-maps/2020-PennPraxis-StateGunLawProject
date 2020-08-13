##########################################################################
# This script:
# 1. Makes heatmaps
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
#
##########################################################################


# https://crd150.github.io/lab6.html#point_intensity_heat_maps


## 1. ----
philly_bbox <- bbox_sfs$Philadelphia %>% 
  st_transform(proj_list$Philadelphia)
philly_tmp <- guns_list_shp_byYear$Philadelphia$`2018` %>% 
  st_transform(proj_list$Philadelphia)
philly_tmp2 <- guns_list_shp$Philadelphia %>% 
  st_transform(proj_list$Philadelphia)
# philly_tmp_sp <- as(philly_tmp, "Spatial") 
sample <- slice_sample(philly_tmp2,
                       n = 10000)



library(tmaptools)
devtools::install_github("mtennekes/oldtmaptools")
library(oldtmaptools)
ds <- oldtmaptools::smooth_map(philly_tmp, bandwidth = 2, cover = philly_bbox, unit = "mi", style = "pretty", nlevels = 25)
ds2 <- oldtmaptools::smooth_map(philly_tmp2, bandwidth = 2, cover = philly_bbox, unit = "mi", style = "pretty", nlevels = 25)
ds3 <- oldtmaptools::smooth_map(philly_tmp2, bandwidth = 2.5, cover = philly_bbox, unit = "mi", style = "pretty", nlevels = 25)


tm_shape(ds$polygons) + tm_fill(col = "level", palette = "YlOrRd", border.alpha = 0) +
  tm_shape(alcohol_outlets$Philadelphia) + tm_dots(col = "black")

tm_shape(ds2$polygons) + tm_fill(col = "level", palette = "YlOrRd", border.alpha = 0) +
  tm_shape(alcohol_outlets$Philadelphia) + tm_dots(col = "black")

ggplot() +
  geom_sf(data = parks$Philadelphia,
          fill = "green") +
  geom_sf(data = ) +
  geom_sf(data = ds3$polygons,
          aes(fill = level),
          alpha = 0.3,
          color = NA) +
  scale_fill_viridis_d(option = 'inferno') +
  # geom_sf(data = sample,
  #         color = "red",
  #         alpha = 0.2) +
  theme(legend.position = "none") +
  mapTheme()

