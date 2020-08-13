##########################################################################
# This script:
# 1. Makes heatmap objects (output of smooth_map())
# 2. Makes the maps with parks and liquor stores overlaid
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
city_bounds <- readRDS("~outputs/10/15_city_bounds.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")

heatmap_objs <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

for (city in seq_len(length(heatmap_objs))[16:34]) {
  
  print(names(heatmap_objs)[city])
  
  gun <- guns_list_shp[[city]]
  bounds <- city_bounds[[city]]
  proj <- proj_list[[city]]
  
  heatmap_objs[[city]] <- smooth_map(gun %>% st_transform(proj),
                                     cover = bounds %>% st_transform(proj),
                                     unit = "mi",
                                     bandwidth = 2.5,
                                     style = "pretty",
                                     nlevels = 20)
  }


## 2. ----
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
land_list <- readRDS("~outputs/10/15_land_list.rds")

OSM_maps_list <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

city_laws_list <- readRDS("~outputs/10/14_city_laws_list.rds")

# city <- 24

for (city in seq_len(length(OSM_maps_list))[c(24)]) {
  
  print(names(OSM_maps_list)[city])
  
  OSM_maps_list[[city]] <- 
    ggplot() +
    geom_sf(data = land_list[[city]],
            # fill = "darkgray",
            fill = "#252525",
            alpha = 0.9,
            color = NA) +
    # geom_sf(data = city_bounds[[city]],
    #         fill = "#E1E1E1",
    #         color = NA) +
    # geom_sf(data = hydrology_list[[city]],
    #         fill = "#97DBF2",
    #         color = NA) +
    geom_sf(data = roads_list[[city]],
            # color = "#feb24c",
            color = "#737373"
            # color = "gray"
    ) +
    geom_sf(data = st_transform(heatmap_objs[[city]]$polygons, 4326),
            aes(fill = level),
            alpha = 0.7,
            color = NA) +
    scale_fill_viridis_d(guide = FALSE,
                         option = "inferno") +  
    new_scale_fill() +
    # geom_sf(data = st_transform(heatmap_objs[[city]]$polygons, 4326) %>% 
    #           mutate(dummy = sample(c(0, 0.5, 1),
    #                                 n(),
    #                                 replace = T)),
    #         aes(fill = dummy),
    #         alpha = 0,
    #         color = NA) +
    # scale_fill_viridis_c(name = "Gun Crime Density",
    #                      option = "inferno",
    #                      breaks = c(0, 1),
    #                      labels = c("Low", 
    #                                 "High")) +  
    # new_scale_fill() +
    geom_sf(data = parks[[city]],
            aes(fill = legend),
            alpha = 0.8,
            color = NA) +
    scale_fill_manual(labels = c("Park"),
                      breaks = c("Park"),
                      values = c("#41ab5d")) +
    geom_sf(data = alcohol_outlets[[city]],
            size = 0.6,
            alpha = 0.8,
            aes(color = legend)) +
    geom_sf(data = hydrology_list[[city]],
            fill = "#525252",
            # fill = "#97DBF2",
            color = NA) +
    # geom_sf(data = context_mask_list[[city]],
    #         # fill = "gray",
    #         fill = "#535353",
    #         color = NA,
    #         alpha = 0.7) +
    # geom_sf(data = city_bounds[[city]],
    #         fill = NA,
    #         # linetype = "dashed",
    #         size = 0.2,
    #         # color = "#bdbdbd",
    #         color = "#9ecae1") +
    scale_color_manual(labels = c("Liquor Store / Bar / Nightclub"),
                       breaks = c("Alcohol Outlet"),
                       values = c("red")) +
    scale_size_manual(guide = FALSE,
                      breaks = c("Major", "Minor"),
                      values = c(0.5, 0.25)) +
    mapTheme() +
    theme(legend.title = element_blank(),
          panel.border = element_blank()) + 
    theme(legend.key = element_rect(fill = "white")) +
    labs(title = paste0("Parks, Alcohol Outlets, and the Density of Gun Crimes in ", 
                        names(city_bounds)[city]),
         subtitle = paste0(format(round(guns_list_shp[[city]] %>% nrow(), -3), big.mark = ","),
                           " estimated gun crimes from ",
                           min(years_byCity[[city]]),
                           " to ",
                           max(years_byCity[[city]]),
                           "\nGuns in Parks: ", city_laws_list[[city]]$Parks_text[1], 
                           "\nGuns in Bars and Liquor Stores: ", city_laws_list[[city]]$Bars_text[1]),
         caption = "Parks and alcohol outlets data from OpenStreetMap")
  
}

test <- st_transform(heatmap_objs[[18]]$polygons, 4326)

city <- 18
# OSM_maps_list[[city]] <- 
  ggplot() +
  geom_sf(data = land_list[[city]],
          # fill = "darkgray",
          fill = "#252525",
          alpha = 0.9,
          color = NA) +
  # geom_sf(data = city_bounds[[city]],
  #         fill = "#E1E1E1",
  #         color = NA) +
  # geom_sf(data = hydrology_list[[city]],
  #         fill = "#97DBF2",
  #         color = NA) +
  geom_sf(data = roads_list[[city]],
          # color = "#feb24c",
          color = "#737373"
          # color = "gray"
  ) +
  geom_sf(data = ,
          aes(fill = level),
          alpha = 0.7,
          color = NA) +
  scale_fill_viridis_d(guide = FALSE,
                       option = "viridis") +  
  new_scale_fill() +
  # geom_sf(data = st_transform(heatmap_objs[[city]]$polygons, 4326) %>% 
  #           mutate(dummy = sample(c(0, 0.5, 1),
  #                                 n(),
  #                                 replace = T)),
  #         aes(fill = dummy),
  #         alpha = 0,
  #         color = NA) +
  # scale_fill_viridis_c(name = "Gun Crime Density",
  #                      option = "inferno",
  #                      breaks = c(0, 1),
  #                      labels = c("Low", 
#                                 "High")) +  
# new_scale_fill() +
geom_sf(data = parks[[city]],
        aes(fill = legend),
        alpha = 0.8,
        color = NA) +
  scale_fill_manual(labels = c("Park"),
                    breaks = c("Park"),
                    values = c("#41ab5d")) +
  geom_sf(data = alcohol_outlets[[city]],
          size = 0.6,
          alpha = 0.8,
          aes(color = legend)) +
  geom_sf(data = hydrology_list[[city]],
          fill = "#525252",
          # fill = "#97DBF2",
          color = NA) +
  # geom_sf(data = context_mask_list[[city]],
  #         # fill = "gray",
  #         fill = "#535353",
  #         color = NA,
  #         alpha = 0.7) +
  # geom_sf(data = city_bounds[[city]],
  #         fill = NA,
  #         # linetype = "dashed",
  #         size = 0.2,
  #         # color = "#bdbdbd",
  #         color = "#9ecae1") +
scale_color_manual(labels = c("Liquor Store / Bar / Nightclub"),
                   breaks = c("Alcohol Outlet"),
                   values = c("red")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("Major", "Minor"),
                    values = c(0.5, 0.25)) +
  mapTheme() +
  theme(legend.title = element_blank(),
        panel.border = element_blank()) + 
  theme(legend.key = element_rect(fill = "white")) +
  labs(title = paste0("Parks, Alcohol Outlets, and the Density of Gun Crimes in ", 
                      names(city_bounds)[city]),
       subtitle = paste0(format(round(guns_list_shp[[city]] %>% nrow(), -3), big.mark = ","),
                         " estimated gun crimes from ",
                         min(years_byCity[[city]]),
                         " to ",
                         max(years_byCity[[city]]),
                         "\nGuns in Parks: ", city_laws_list[[city]]$Parks_text[1], 
                         "\nGuns in Bars and Liquor Stores: ", city_laws_list[[city]]$Bars_text[1]),
       caption = "Parks and alcohol outlets data from OpenStreetMap")

OSM_maps_list$`Los Angeles`

## 1. Export as rds ----
saveRDS(heatmap_objs,
        "~outputs/30/37_heatmap_objs.rds")

heatmap_objs <- readRDS("~outputs/30/37_heatmap_objs.rds")

## 2. Export as pdf ----
for (city in seq_len(length(OSM_maps_list))[c(24)]) {
  
  print(names(OSM_maps_list)[city])
  
  ggsave(plot = OSM_maps_list[[city]],
         filename = paste("~outputs/Plots/37a_OSM_heat_maps/37a_",
                          names(OSM_maps_list)[city],
                          "_OSM_heat_map.pdf",
                          sep = ""),
         device = "pdf",
         units = "in",
         dpi = 300,
         width = 8.5,
         height = 11)
}
