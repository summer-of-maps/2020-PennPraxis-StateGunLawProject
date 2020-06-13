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
incident_count <- guns_clean %>% 
  group_by(city) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

incidentsByCityPlot <- ggplot(incident_count,
                           aes(x = reorder(city, count), y = count)) +
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
  geom_sf(data = sf_incidents_shp,
          color = "#d7301f",
          alpha = 0.3,
          inherit.aes = FALSE) +
  plotTheme()
  
