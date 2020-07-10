##########################################################################
# This script:
# 1. Counts gun crimes:
#   (a) total by city
#   (b) by year and by city
# 2. Maps all incidents in San Francisco that have coordinates
#
# Exports: 
# 1. allCrimes_count as 31_allCrimes_count.rds
# 2. gunIncident_summary as 31_gunIncident_summary.rds
# 3. gunCount_byYear as 31_gunCount_byYear.rds
# 4. gunCount_byYear_list as 31_gunCount_byYear_list.rds
#
# To-do:
# 1. 
##########################################################################

## 1a. ----
allCrimes_count <- readRDS("~outputs/30/31_allCrimes_count.rds")
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

## 1b. ----
gunCount_byYear <-  guns_clean %>% 
  group_by(city, state, year) %>% 
  summarize(gun_count = n()) %>% 
  arrange(desc(gun_count))

gunCount_byYear_list <- split(gunCount_byYear,
                              gunCount_byYear$city)


## 2. SF crime map ----
# sf_incidents_shp <- guns_list$`San Francisco` %>% 
#   filter(!is.na(lon),
#          !is.na(lat)) %>% 
#   st_as_sf(coords = c("lon", "lat"),
#            crs = 4326,
#            remove = FALSE)
# 
# sf_basemap <- get_map(base_map_bb(sf_incidents_shp), 
#                       source = "stamen",
#                       maptype = "toner-background",
#                       color = "bw")
# 
# sf_incident_map <- ggmap(sf_basemap) +
#   geom_sf(data = sf_tracts_2018,
#           inherit.aes = FALSE) +
#   geom_sf(data = sf_incidents_shp,
#           color = "#d7301f",
#           alpha = 0.3,
#           inherit.aes = FALSE) +
#   plotTheme()

## 1. Export allCrimes_count as rds ----
# saveRDS(allCrimes_count,
#         file = "~outputs/30/31_allCrimes_count.rds")

## 2. Export gunIncident_summary as rds ----
# saveRDS(gunIncident_summary,
#         file = "~outputs/30/31_gunIncident_summary.rds")

## 3. Export gunCount_byYear as rds ----
# saveRDS(gunCount_byYear,
#         file = "~outputs/30/31_gunCount_byYear.rds")
# saveRDS(gunCount_byYear_list,
#         file = "~outputs/30/31_gunCount_byYear_list.rds")
