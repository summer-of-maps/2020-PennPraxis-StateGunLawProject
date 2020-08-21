##########################################################################
# This script:
# 1. Maps geographies by total crime count
#   a. Tracts
#   b. Block groups
# 2. Maps geographies by gun crimes per 100 people
#   a. Tracts
#   b. Block groups
#     (i) Whole time period
#     (ii) Year-by-year
# 3. Creates deliverable maps
#   a. Crimes per capita, block groups, whole time period
#   b. Maps for each year with GIFs
#
# Exports: 
# 1. basemap_list as 31a_basemap_list.rds
# 2. ggmap_list as 31a_ggmap_list.rds (large file)
# 3. Plots/33a_tract_crime_maps/[geography]/33a_[City]_tract_crime_maps_.png
# 4. Plots/33a_BG_crime_maps/[geography]/33a_[City]_BG_crime_maps_.png
# 
# To-do:
# 1. 
##########################################################################

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

## 2a. ----
# tracts_crimeCounts <- readRDS("~outputs/30/33_tracts_crimeCounts.rds")
tract_per100_maps <- map(tracts_crimeCounts,
                        .f = function(geo_set) 
                          map2(geo_set,
                               basemap_list,
                               .f = function(cityCrime, cityBasemap)
                                 ggmap(cityBasemap) +
                                 ## below has quintiles
                                 geom_sf(data = cityCrime,
                                         aes(fill = q5(guns_per100)),
                                         color = "lightgray",
                                         alpha = 0.5,
                                         inherit.aes = FALSE) +
                                 scale_fill_viridis_d(labels = qBr(cityCrime,
                                                                   "guns_per100"),
                                                      name = "Quintile\nBreaks") +
                                 ## below has continuous
                                 # geom_sf(data = cityCrime,
                                 #         aes(fill = guns_per100),
                                 #         color = "lightgray",
                                 #         alpha = 0.5,
                                 #         inherit.aes = FALSE) +
                                 # scale_fill_viridis_c(name = "Gun Crimes /nper 100 People") +
                                 mapTheme() +
                                 labs(title = "Gun Crimes per 100 People by Census Tract")))

## 2b(i). ----
# BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")
BG_per100_maps <- map(BGs_crimeCounts,
                     .f = function(geo_set) 
                       map2(geo_set,
                            basemap_list,
                            .f = function(cityCrime, cityBasemap)
                              ggmap(cityBasemap) +
                              ## below has quintiles
                              geom_sf(data = cityCrime,
                                      aes(fill = q5(guns_per100)),
                                      color = "lightgray",
                                      alpha = 0.5,
                                      inherit.aes = FALSE) +
                              scale_fill_viridis_d(labels = qBr(cityCrime,
                                                                "guns_per100"),
                                                   name = "Quintile\nBreaks") +
                              ## below has continuous
                              # geom_sf(data = cityCrime,
                              #         aes(fill = guns_per100),
                              #         color = "lightgray",
                              #         alpha = 0.5,
                              #         inherit.aes = FALSE) +
                              # scale_fill_viridis_c(name = "Gun Crime /nper 100 People") +
                              mapTheme() +
                              labs(title = "Gun Crimes per 100 People by Block Group")))

## 2b(ii). ----
# BGs_crimeCounts_byYear <- readRDS("~outputs/30/33_BGs_crimeCounts_byYear.rds")
BG_per100_byYear_maps <- vector("list", length(BGs_crimeCounts_byYear)) %>% 
  set_names(names(BGs_crimeCounts_byYear))

for (city in seq_len(length(BGs_crimeCounts_byYear))) {
  print(names(BGs_crimeCounts_byYear)[city])
  
  BG_per100_byYear_maps[[city]] <- vector("list", length(years_byCity[[city]])) %>% 
    set_names(names(years_byCity[[city]]))
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    
    tmp <- left_join(BG_selection_list$byCaveHull[[city]],
                     BGs_crimeCounts_byYear[[city]][[year]],
                     by = "GEOID")
    
    BG_per100_byYear_maps[[city]][[year]] <-
      suppressMessages(
        ggmap(basemap_list[[city]]) +
          # geom_sf(data = tmp,
          #         aes(fill = q5(guns_per100)),
          #         size = 0.01,
          #         color = NA,
          #         alpha = 0.5,
          #         inherit.aes = FALSE) +
          # scale_fill_viridis_d(labels = qBr(tmp,
          #                                   "guns_per100"),
          #                      name = "Quintile\nBreaks") +
      ## below has continuous
      geom_sf(data = tmp,
              aes(fill = guns_per100),
              size = 0.01,
              color = NA,
              alpha = 0.5,
              inherit.aes = FALSE) +
      scale_fill_viridis_c(name = "Gun Crimes\nper 100 People") +
        mapTheme() +
        labs(title = "Gun Crimes per 100 People by Block Group",
             subtitle = print(years_byCity[[city]][[year]]))
      )
  }
  
}

## 3a. ----
city_bounds <- readRDS("~outputs/10/14_city_bounds.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")
BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")
land_list <- readRDS("~outputs/10/15_land_list.rds")
context_bbox_list <- readRDS("~outputs/10/15_context_bbox_list.rds")
context_mask_list <- readRDS("~outputs/10/15_context_mask_list.rds")
hydrology_list <- readRDS("~outputs/10/15_hydrology_list.rds")
roads_list <- readRDS("~outputs/10/15_roads_list.rds")
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")

BGs_perCapita <- BGs_crimeCounts$byPlace

BGs_perCapita_maps_list <- vector("list", length(BGs_perCapita)) %>% 
  set_names(names(BGs_perCapita))

# city <- 6

for (city in seq_len(length(BGs_perCapita_maps_list))) {

  name <- names(BGs_perCapita_maps_list)[city]
  print(name)
    
  max <- (max(BGs_perCapita[[city]]$guns_per100))/length(years_byCity[[city]])
  
  tmp <- ggplot() +
    geom_sf(data = land_list[[city]],
            # fill = "darkgray",
            fill = "#252525",
            alpha = 0.9,
            color = NA) +
    geom_sf(data = BGs_perCapita[[city]],
            aes(fill = guns_per100 / length(years_byCity[[city]])),
            color = "#999999",
            size = 0.01,
            # color = NA,
            alpha = 0.8,
            inherit.aes = FALSE) +
    # scale_fill_viridis_c(na.value = "#252525",
    #                      name = paste0(min(years_byCity[[city]]),
    #                                    " to ",
    #                                    max(years_byCity[[city]])),
    #                      limits = c(0, max(BGs_perCapita[[city]]$guns_per100)),
    #                      breaks = c(0, max(BGs_perCapita[[city]]$guns_per100)/2, max(BGs_perCapita[[city]]$guns_per100)),
    #                      labels = c(0, round(max(BGs_perCapita[[city]]$guns_per100)/2), ceiling(max(BGs_perCapita[[city]]$guns_per100)))) +
    scale_fill_viridis_c(na.value = "#252525",
                         name = paste0(min(years_byCity[[city]]),
                                       " to ",
                                       max(years_byCity[[city]])),
                         limits = c(0, max),
                         breaks = c(0, max/2, max),
                         labels = c(0, round(max/2), ceiling(max))) +
    geom_sf(data = hydrology_list[[city]],
            fill = "#525252",
            # fill = "#97DBF2",
            color = NA) +
    geom_sf(data = roads_list[[city]],
            # color = "#feb24c",
            color = "#737373"
            # color = "gray"
    ) +
    scale_size_manual(guide = FALSE,
                      breaks = c("Major", "Minor"),
                      values = c(0.5, 0.25)) +
    # geom_sf(data = BGs_perCapita[[city]],
    #         aes(fill = q5(guns_per100)),
    #         # color = "lightgray",
    #         color = NA,
    #         alpha = 0.5,
    #         inherit.aes = FALSE) +
    # scale_fill_viridis_d(labels = qBr(BGs_perCapita[[city]],
    #                                   "guns_per100"),
    #                      name = "Quintile\nBreaks") +
  
    mapTheme() +
    theme(legend.title = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#525252"),
          panel.border = element_blank())
  
  legend_tmp <- get_legend(tmp)
  legend <- ggpubr::as_ggplot(legend_tmp)
  
  ggsave(plot = legend,
         filename = paste0("~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_allYears/",
                           name,
                           "_legend.pdf"),
         device = "pdf",
         units = "in",
         dpi = 300,
         width = (75/72),
         height = (113/72))
  
  BGs_perCapita_maps_list[[city]] <- tmp +
    theme(legend.position = "none")
    
  ggsave(plot = BGs_perCapita_maps_list[[city]],
         filename = paste0("~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_allYears/",
                           name,
                           "_map.pdf"), 
         device = "pdf",
         units = "in",
         dpi = 300,
         width = 8.5,
         height = 11)


}


avg_crimesPerYear <-
  map_df(guns_list_shp_byYear, 
                         function (city) 
                           map(city,
                               nrow) %>% 
                           bind_rows() %>% 
                           gather(key = "Year",
                                  value = "GunCrimes") %>% 
        filter(Year != "2020") %>% 
        summarize(average = round(sum(GunCrimes) / nrow(.))),
        .id = "City")

population_2020 <-
  map_df(BG_selection_list$byPlace,
         function(city) round(sum(city$estimate), -3)) %>% 
  gather(key = "City",
         value = "Population")

summary <- left_join(avg_crimesPerYear, population_2020, by = "City") %>% 
  mutate(per100 = average * 100 / Population)


## 3b. ----


BG_per100_byYear_Deliverables <- vector("list", length(BGs_crimeCounts_byYear)) %>% 
  set_names(names(BGs_crimeCounts_byYear))

# walk(paste0("~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_byYear_GIF/",
#             names(BG_per100_byYear_Deliverables)),
#      dir.create)

for (city in seq_len(length(BGs_crimeCounts_byYear))) {
  
  name <- names(BGs_crimeCounts_byYear)[city]
  
  print(name)
  
  BG_per100_byYear_Deliverables[[city]] <- vector("list", length(years_byCity[[city]])) %>% 
    set_names(names(years_byCity[[city]]))
  
  width_ratio <- get_asp_ratio(context_bbox_list[[city]],
                               is.projected = FALSE)
  
  for (year in seq_len(length(years_byCity[[city]]))) {
    
    print_year <- years_byCity[[city]][[year]]
    
    print(print_year)
    
    tmp <- left_join(BG_selection_list$byPlace[[city]],
                     BGs_crimeCounts_byYear[[city]][[year]],
                     by = "GEOID")
    
    max <- round(max(tmp$guns_per100), 2)
    
    BG_per100_byYear_Deliverables[[city]][[year]] <-
      ggplot() +
      geom_sf(data = land_list[[city]],
              # fill = "darkgray",
              fill = "#252525",
              alpha = 0.9,
              color = NA) +
      geom_sf(data = tmp,
              aes(fill = guns_per100),
              # color = "#999999",
              size = 0.01,
              color = NA,
              alpha = 0.8) +
      scale_fill_viridis_c(na.value = "#252525",
                           name = "Gun Crimes\n(per 100 people)",
                           limits = c(0, max),
                           breaks = c(0, max/2, max),
                           labels = c(0, round(max/2, 2), ceiling(max))) +
      geom_sf(data = hydrology_list[[city]],
              fill = "#525252",
              # fill = "#97DBF2",
              color = NA) +
      geom_sf(data = roads_list[[city]],
              # color = "#feb24c",
              color = "#737373"
              # color = "gray"
      ) +
      scale_size_manual(guide = FALSE,
                        breaks = c("Major", "Minor"),
                        values = c(0.5, 0.25)) +
      # geom_sf(data = BGs_perCapita[[city]],
      #         aes(fill = q5(guns_per100)),
      #         # color = "lightgray",
      #         color = NA,
      #         alpha = 0.5,
      #         inherit.aes = FALSE) +
      # scale_fill_viridis_d(labels = qBr(BGs_perCapita[[city]],
      #                                   "guns_per100"),
      #                      name = "Quintile\nBreaks") +
      
      mapTheme() +
      theme(plot.title = element_text(color = "white"),
            plot.subtitle = element_text(color = "white"),
            plot.caption = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.background = element_rect(fill = "#252525"),
            plot.background = element_rect(fill = "#252525"),
            panel.background = element_rect(fill = "#252525"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      labs(title = paste0(name, " - Gun Crimes per 100 People"),
           caption = "Geographies: census block groups",
           subtitle = paste(years_byCity[[city]][[year]]))
    
    filename <- paste0("~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_byYear_GIF/",
                       name,
                       "/",
                       name,
                       "_",
                       print_year,
                       ".png")
    
    ggsave(plot = BG_per100_byYear_Deliverables[[city]][[year]],
           filename = filename, 
           device = "png",
           units = "in",
           dpi = 150,
           width = 12 * width_ratio,
           height = 11)
    
    tmp <- image_trim(image_read(filename))
    
    image_write(tmp, filename)
  }
  
}

tmp_folders <- list.dirs(path = "~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_byYear_GIF/",
                         recursive = FALSE,
                          full.names = TRUE)

for (city in seq_len(length(BG_per100_byYear_Deliverables[c(1:14, 16:34)]))) {
  
  name <- names(BG_per100_byYear_Deliverables[c(1:14, 16:34)])[city]
  
  print(name)
  
  tmp_files <- list.files(path = tmp_folders[city],
                          pattern = ".*png$",
                          full.names = TRUE)
  
  tmp_image <- image_read(tmp_files[1])
  tmp_width <- image_info(tmp_image)$width
  tmp_height <- image_info(tmp_image)$height 
  
  gifski::gifski(tmp_files, 
                 gif_file = paste0("~outputs/Plots/~Deliverable maps/33a_BlockGroups_CrimesPerCapita_byYear_GIF/",
                                   name,
                                   ".gif"), 
                 width = tmp_width, height = tmp_height, delay = 1.5)
  
  
}

## 1a. Export maps as pngs ----
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

## 1b. Export maps as pngs ----
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

## 2a. Export maps as pngs ----
map2(tract_per100_maps,
     names(tract_per100_maps),
     function(geo, geoType)
       map2(geo,
            names(geo),
            function(a, b) ggsave(plot = a,
                                  filename = paste("~outputs/Plots/33a_tract_per100_maps/",
                                                   geoType,
                                                   "/33a_",
                                                   b,
                                                   "_tract_per100_maps_",
                                                   # geoType,
                                                   ".png",
                                                   sep = ""),
                                  device = "png",
                                  units = "in",
                                  dpi = 72,
                                  width = 7,
                                  height = 7)))

## 2b(i). Export maps as pngs ----
map2(BG_per100_maps,
     names(BG_per100_maps),
     function(geo, geoType)
       map2(geo,
            names(geo),
            function(a, b) ggsave(plot = a,
                                  filename = paste("~outputs/Plots/33a_BG_per100_maps/",
                                                   geoType,
                                                   "/33a_",
                                                   b,
                                                   "_BG_per100_maps_",
                                                   # geoType,
                                                   ".png",
                                                   sep = ""),
                                  device = "png",
                                  units = "in",
                                  dpi = 72,
                                  width = 7,
                                  height = 7)))

## 2b(ii). Export maps as pngs ----

# create directories for each city
# walk(paste0("~outputs/Plots/33a_BG_per100_byYear_maps/",
#             names(BG_per100_byYear_maps)),
#      dir.create)

for (city in seq_len(length(BG_per100_byYear_maps))) {
  print(names(BG_per100_byYear_maps)[city])
  
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    
    ggsave(plot = BG_per100_byYear_maps[[city]][[year]],
           filename = paste("~outputs/Plots/33a_BG_per100_byYear_maps/",
                            names(BG_per100_byYear_maps)[city],
                            "/33a_",
                            years_byCity[[city]][[year]],
                            "_BG_per100_byYear_maps.png",
                            # geoType,
                            # ".png",
                            sep = ""),
           device = "png",
           units = "in",
           dpi = 144,
           width = 7,
           height = 7)
    
  }
  
  
}
  
## Make GIFs
tmp_folders <- list.files(path = "~outputs/Plots/33a_BG_per100_byYear_maps",
             full.names = TRUE)[2:35]

# run piece-by-piece (4 or 5 at a time)
# running whole loop stalls R
for (city in seq_len(length(BG_per100_byYear_maps))) {
  print(names(BG_per100_byYear_maps)[city])
  
  tmp_files <- list.files(path = tmp_folders[city],
                          full.names = TRUE)
  
  tmp_animation <- map(tmp_files,
                    image_read) %>% 
    image_join() %>% 
    image_animate(fps = 0.5)
  
  image_write(tmp_animation,
              paste0("~outputs/Plots/33a_BG_per100_byYear_maps/~gifs/",
                     names(BG_per100_byYear_maps)[city],
                     "_BG_per100_byYear.gif"))
  rm(tmp_animation)
  
}


