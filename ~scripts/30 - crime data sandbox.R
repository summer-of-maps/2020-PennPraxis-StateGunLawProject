##########################################################################
# This script:
# 1. is for experimenting with the crime data.
#
# Exports: all code written here should be exported to a separate script.
#
# To-do:
# 1. Consider moving the sf_incidents_shp code to 21.R and generalizing it to all cities.
# 2. Render the ggplots as separate pngs and write code for them to be pasted in individually
#     into the markdown. Too slow to render live.
##########################################################################



# kernel density of crimes in San Francisco
library(spatstat)
library(raster)


sf_shp <- guns_list_shp$`San Francisco`

sf_ppp <- as.ppp(st_coordinates(sf_shp), W = st_bbox(sf_shp))
sf_KD.2000 <- density.ppp(sf_ppp, 2000)
# sf_KD.1500 <- density.ppp(sf_ppp, 1500)
# sf_KD.2000 <- density.ppp(sf_ppp, 2000,
#                           dimyx = 1000)

sf_KD.2000.raster <- raster(x = sf_KD.2000,
                            crs = st_crs(sf_shp)$proj4string)

sf_KD.2000.raster.min <- cellStats(sf_KD.2000.raster, "min")
sf_KD.2000.raster.max <- cellStats(sf_KD.2000.raster, "max")

sf_KD.2000.raster_scaled <- (sf_KD.2000.raster - sf_KD.2000.raster.min) / (sf_KD.2000.raster.max - sf_KD.2000.raster.min)

sf_KD_map <- data.frame(rasterToPoints(mask(sf_KD.2000.raster, 
                             as(sf_tracts_2018 %>% 
                                  st_transform(st_crs(sf_shp)) %>% 
                                  .[sf_shp,], 'Spatial'))))

ggplot() +
  geom_raster(data = sf_KD_map, aes(x=x, y=y, fill=layer))