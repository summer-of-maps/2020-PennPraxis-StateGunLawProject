##########################################################################
# This script:
# 1. Counts and plots gun crimes by city
# 2. Maps all incidents in San Francisco that have coordinates
#
# Exports: 
# 1. gunIncident_summary as 31_gunIncident_summary.rds
#
# To-do:
# 1. 
##########################################################################

## 1. ----
allCrimes_count <- readRDS("~outputs/30/30_allCrimes_count.rds")
# allCrimes_count <- allCrimes_df %>%
#   group_by(city) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))

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

## 1. Export gunIncident_summary as rds ----
# saveRDS(allCrimes_count,
#         file = "~outputs/30/30_allCrimes_count.rds")
