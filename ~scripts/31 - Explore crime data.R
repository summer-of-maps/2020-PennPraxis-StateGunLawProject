##########################################################################
# This script:
# 1. Counts and plots gun crimes by city
# 2. Maps all incidents in San Francisco that have coordinates
#
# Exports: 
#
# To-do:
# 1. Consider moving the sf_incidents_shp code to 21.R and generalizing it to all cities.
##########################################################################

## 1. ----
# allCrimes_count <- allCrimes_df %>% 
#   group_by(city) %>% 
#   summarize(count = n()) %>% 
#   arrange(desc(count))
# saveRDS(allCrimes_count,
#         file = "~outputs/~scratch/allCrimes_count.rds")
allCrimes_count <- readRDS("~outputs/~scratch/allCrimes_count.rds")

gunIncident_count <- guns_clean %>% 
  group_by(city) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

gunIncident_summary <- gunIncident_count %>% 
  rename(gun_count = count) %>% 
  left_join(allCrimes_count, by = "city") %>% 
  rename(all_crimes_count = count) %>% 
  mutate(prop = gun_count / all_crimes_count) %>% 
  arrange(desc(gun_count))

gunIncidentsByCityPlot <- ggplot(gunIncident_summary,
                           aes(x = reorder(city, gun_count), y = gun_count)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  coord_flip() +
  plotTheme()


## 2. SF crime map ----
sf_incidents_shp <- guns_list$`San Francisco` %>% 
  filter(!is.na(lon),
         !is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE)

sf_basemap <- get_map(base_map_bb(sf_incidents_shp), 
                      source = "stamen",
                      maptype = "toner-background",
                      color = "bw")

sf_incident_map <- ggmap(sf_basemap) +
  geom_sf(data = sf_tracts_2018,
          inherit.aes = FALSE) +
  geom_sf(data = sf_incidents_shp,
          color = "#d7301f",
          alpha = 0.3,
          inherit.aes = FALSE) +
  plotTheme()

## 3. all cities crime maps ----
# basemap_list <- map(guns_list_shp,
#                     ~ get_map(base_map_bb(.x),
#                               source = "stamen",
#                               maptype = "toner-background",
#                               color = "bw"))
# saveRDS(basemap_list,
#         file = "~outputs/~scratch/basemap_list.rds")
basemap_list <- readRDS("~outputs/~scratch/basemap_list.rds")

# l <- list(basemaps = basemap_list,
#          cityNames = names(basemap_list),
#          gun_points = guns_list_shp)
# saveRDS(l,
#         file = "~outputs/~scratch/l.rds")
l <- readRDS("~outputs/~scratch/l.rds")

# ggmap_list <- pmap(.l = l,
#                   ~ with(list(...),
#                          ggmap(basemaps) +
#                            geom_sf(data = gun_points,
#                                    color = "#d7301f",
#                                    alpha = 0.25,
#                                    inherit.aes = FALSE) +
#                            labs(title = paste("Gun crime incidents in", cityNames))))
# 
# saveRDS(ggmap_list,
#         file = "~outputs/~scratch/ggmap_list.rds")
ggmap_list <- readRDS("~outputs/~scratch/ggmap_list.rds")


## Export as PNG
# plots <- arrangeGrob(grobs = ggmap_list,
#                      ncol = 2)
# ggsave(plot = plots,
#        filename = "~outputs/Plots/~scratch/ggmap_test.png",
#        units = "in",
#        width = 24,
#        height = 120,
#        limitsize = FALSE)


