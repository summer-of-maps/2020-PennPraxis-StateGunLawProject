##########################################################################
# This script:
# 1. Plots Moran's I for each city by year
# 2. Plots Siegel Score, Crime Count, and Moran's I for each city in a single plot
# 3. Local Moran's I hotspot maps
#   a. All years
#   b. By year
# 4. Local Moran's I deliverable maps
#   a. All Years
#   b. By year
#
# Exports: 
# 1. BGs_per100_I_byYear_plots as 34a_BGs_per100_I_byYear_plots.rds
# 2. Siegel_gunCount_MoransI_plots as pngs
# 
# To-do:
# 1. PCA scores?
##########################################################################

## 1. ----
# BGs_per100_I_byYear <- readRDS("~outputs/30/34_BGs_per100_I_byYear.rds")
BGs_per100_I_byYear_list <- map(BGs_per100_I_byYear,
                function (city)
                  map_dfr(city,
                      function(year) 
                        data.frame("Morans_I" = year$estimate[1], 
                                   "p_value_raw" = year$p.value[1]),
                      .id = "year") %>% 
  mutate(year = as.numeric(year),
         p_value = ifelse(p_value_raw < 0.05,
                          "< 0.05",
                          "> 0.05")) %>% 
  `rownames<-`(seq_len(nrow(.))))

BGs_per100_I_byYear_plots <- map(BGs_per100_I_byYear_list,
                                 ~ .x %>% 
                                   ggplot(aes(x = year,
                                              y = Morans_I)) +
                                   geom_line(size = 1) +
                                   geom_point(size = 4,
                                              aes(color = factor(p_value))) +
                                   plotTheme() +
                                   scale_color_manual(name = "P Value",
                                                      values = c("#00BFC4", "#F8766D")) +
                                   scale_x_continuous(breaks = seq(min(.x$year), max(.x$year), 2)) +
                                   scale_y_continuous(breaks = seq(-1, 1, by = 0.25),
                                                      limits = c(-1, 1)) +
                                   labs(title = "Moran's I for Gun Crimes per 100 People",
                                        subtitle = "Geographic unit of analysis: census block groups",
                                        x = "Year",
                                        y = "Moran's I") + 
                                   theme(legend.position = c(0.8, 0.2),
                                         legend.text = element_text(size = 10,
                                                                    face = "bold")))

## 2. ----
# gunIncidentsByYear_plots <- readRDS("~outputs/30/31c_gunIncidentsByYear_plots.rds")
# gunCount_byYear_list <- readRDS("~outputs/30/31_gunCount_byYear_list.rds")
# siegelSum_plots <- readRDS("~outputs/30/30_siegelSum_plots.rds")
# gunPercentage_byYear_plots <- readRDS("~outputs/30/36_gunPercentage_byYear_plots.rds") # note this comes from a later script
BGs_per100_I_byYear_plots <- readRDS("~outputs/30/34a_BGs_per100_I_byYear_plots.rds")
Siegel_gunCount_MoransI_plots <- pmap(.l = list(BGs_per100_I_byYear_plots,
                                      gunIncidentsByYear_plots[c(1:14, 16:34)],
                                      gunPercentage_byYear_plots[c(1:14, 16:34)],
                                      gunCount_byYear_list[c(1:14, 16:34)]),
                                      function(a, b, c, d) 
                                        plot_grid_diffAxes(a,
                                                           b,
                                                           c,
                                                           siegelSum_plots[[d[1,] %>% pull(state)]],
                                                           ncol = 1,
                                                           align = "v"))

## 3a. ----
make_cluster_map <- function(x, y, z, darken = 0.0) {
  
  siegel <- siegelSum_list[[z[1,] %>% pull(state)]] %>% # get siegel score for plot label
    filter(year == max(year)) %>% 
    pull(score)
  
  siegel_max <- max(siegelSum$score)
  siegel_min <- min(siegelSum$score)
  
  max_year <- max(z$year)
  min_year <- min(z$year)
  
  tmp_summary <- y %>% 
    st_drop_geometry() %>% 
    group_by(cluster, .drop = FALSE) %>% 
    summarize(count = n()) %>% 
    mutate(prop = count / sum(count))
  
  insignificant <- tmp_summary %>% 
    filter(cluster == "insignificant") %>% 
    pull(count)
  high_high <- tmp_summary %>% 
    filter(cluster == "high-high") %>% 
    pull(count)
  high_low <- tmp_summary %>% 
    filter(cluster == "high-low") %>% 
    pull(count)
  low_high <- tmp_summary %>% 
    filter(cluster == "low-high") %>% 
    pull(count)
  low_low <- tmp_summary %>% 
    filter(cluster == "low-low") %>% 
    pull(count)
  insignificant_prop <- tmp_summary %>% 
    filter(cluster == "insignificant") %>% 
    pull(prop)
  high_high_prop <- tmp_summary %>% 
    filter(cluster == "high-high") %>% 
    pull(prop)
  high_low_prop <- tmp_summary %>% 
    filter(cluster == "high-low") %>% 
    pull(prop)
  low_high_prop <- tmp_summary %>% 
    filter(cluster == "low-high") %>% 
    pull(prop)
  low_low_prop <- tmp_summary %>% 
    filter(cluster == "low-low") %>% 
    pull(prop)
  
  subtitle <- paste("Siegel Score: ", siegel, ". ",
                    high_high, " high-high clusters (", 
                   round(high_high_prop * 100, 0), "% of BGs) and ",
                   high_low, " high-low clusters (",
                   round(high_low_prop * 100, 0), "%)",
                   sep = "")
  
  caption <- paste("Crime data from ", min_year,
                   " to ", max_year, ". State Siegel Scores range from ",
                   siegel_min, " to ", siegel_max, 
                   sep = "")
  
  ggmap(x,
        darken = darken
        ) +
    geom_sf(data = y, aes(fill = cluster),
            color = "gray",
            inherit.aes = FALSE) + 
    scale_fill_manual(name = "Signif. Local Clusters/Outliers\np value < 0.05",
                      values = c("white", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                      limits = c("insignificant", "high-high", "high-low", "low-high", "low-low")) +
    mapTheme() +
    labs(title = "Gun Crime Clusters and Outliers",
         subtitle = subtitle,
         caption = caption)
} # function for the loop below

BG_cluster_maps <- pmap(list(basemap_list,
                        BGs_per100_localI,
                        gunCount_byYear_list),
                        make_cluster_map,
                        darken = 0.3)

siegel_tmp <- map_dfr(gunCount_byYear_list,
              ~ siegelSum_list[[.x[1,] %>% pull(state)]] %>% # get siegel score for plot label
  filter(year == max(year)) %>% 
  pull(score),
  .id = "City")

## 3b. ----
basemap_list <- readRDS("~outputs/30/31a_basemap_list.rds")
BGs_per100_localI_byYear <- readRDS("~outputs/30/34_BGs_per100_localI_byYear.rds")
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
gunCount_byYear_list <- readRDS("~outputs/30/31_gunCount_byYear_list.rds")


BG_cluster_maps_byYear <- vector("list", length(BGs_per100_localI_byYear)) %>% 
  set_names(names(BGs_per100_localI_byYear))

for (city in seq_len(length(BG_cluster_maps_byYear))) {
  print(names(BG_cluster_maps_byYear)[city])
  
  BG_cluster_maps_byYear[[city]] <- vector("list", length(years_byCity[[city]])) %>% 
    set_names(names(years_byCity[[city]]))
  
  basemap_tmp <- basemap_list[[city]]
  siegel_max <- max(siegelSum$score)
  siegel_min <- min(siegelSum$score)
  
  for (year in seq_len(length(BG_cluster_maps_byYear[[city]]))) {
    
    data_tmp <- BGs_per100_localI_byYear[[city]][[year]]
    year_tmp <- years_byCity[[city]][[year]]
    siegel_tmp <- siegelSum_list[[gunCount_byYear_list[[city]][1,]$state]][year,]$score
    
    BG_cluster_maps_byYear[[city]][[year]] <- ggmap(
      basemap_tmp,
      dark = 0.3) +
      geom_sf(data = data_tmp,
              aes(fill = factor(cluster,
                                levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))),
              color = "gray",
              inherit.aes = FALSE) +
      scale_fill_manual(name = "Local Clusters/Outliers",
                        values = c("white", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                        limits = c("insignificant", "high-high", "high-low", "low-high", "low-low")) +
      mapTheme() +
      labs(title = paste0(names(BG_cluster_maps_byYear)[city], ": Gun Crime Clusters and Outliers"),
           subtitle = paste0(year_tmp, ". Siegel Score: ", siegel_tmp),
           caption = paste("State Siegel Scores range from ",
                           siegel_min, " to ", siegel_max, 
                           sep = ""))
      
  }
  
}


## 4a. ----
city_bounds <- readRDS("~outputs/10/14_city_bounds.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")
BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")
land_list <- readRDS("~outputs/10/15_land_list.rds")
context_bbox_list <- readRDS("~outputs/10/15_context_bbox_list.rds")
context_mask_list <- readRDS("~outputs/10/15_context_mask_list.rds")
hydrology_list <- readRDS("~outputs/10/15_hydrology_list.rds")
roads_list <- readRDS("~outputs/10/15_roads_list.rds")
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")


city <- 6



legend_plot <- ggplot() +
  geom_sf(data = land_list[[city]],
          # fill = "darkgray",
          fill = "#252525",
          alpha = 0.9,
          color = NA) +
  geom_sf(data = BGs_per100_localI[[city]], aes(fill = cluster),
          color = "#999999",
          size = 0.01,
          # color = NA,
          alpha = 0.8,
          inherit.aes = FALSE) + 
  scale_fill_manual(name = "Statistically Significant\nClusters / Outliers\n(p value < 0.05)",
                    values = c("#252525", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                    limits = c("insignificant", "high-high", "high-low", "low-high", "low-low"),
                    labels = c("Not Significant", "Hot Spot", "High Crime in a\nLower Crime Area", "Low Crime in a\nHigher Crime Area", "Cold Spot")) +
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
  mapTheme() +
  theme(legend.spacing.y = unit(0.5, 'cm'),
        # legend.key.size = unit(1.5, "cm"),
        legend.key.size = unit(2, 'lines'),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#525252"),
        panel.border = element_blank())

legend_tmp <- get_legend(legend_plot)
legend <- ggpubr::as_ggplot(legend_tmp)

ggsave(plot = legend,
       filename = paste0("~outputs/Plots/~Deliverable maps/34a_BG_cluster_maps_allYears/Illustrator/",
                         # name,
                         "legend.pdf"),
       device = "pdf",
       units = "in",
       dpi = 300,
       width = (120/72),
       height = (200/72)
       )

BGs_clusters_maps_list <- vector("list", length(BGs_per100_localI)) %>% 
  set_names(names(BGs_per100_localI))

for (city in seq_len(length(BGs_clusters_maps_list))) {
  
  name <- names(BGs_clusters_maps_list)[city]
  print(name)
  
  BGs_clusters_maps_list[[name]] <- ggplot() +
    geom_sf(data = land_list[[name]],
            # fill = "darkgray",
            fill = "#252525",
            alpha = 0.9,
            color = NA) +
    geom_sf(data = BGs_per100_localI[[name]], aes(fill = cluster),
            color = "#999999",
            size = 0.01,
            # color = NA,
            alpha = 0.8,
            inherit.aes = FALSE) + 
    scale_fill_manual(name = "Statistically Significant\nClusters / Outliers\n(p value < 0.05)",
                      values = c("#252525", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                      limits = c("insignificant", "high-high", "high-low", "low-high", "low-low"),
                      labels = c("Not Significant", "Hot Spot", "High Crime in a\nLower Crime Area", "Low Crime in a\nHigher Crime Area", "Cold Spot")) +
    geom_sf(data = hydrology_list[[name]],
            fill = "#525252",
            # fill = "#97DBF2",
            color = NA) +
    geom_sf(data = roads_list[[name]],
            # color = "#feb24c",
            color = "#737373"
            # color = "gray"
    ) +
    scale_size_manual(guide = FALSE,
                      breaks = c("Major", "Minor"),
                      values = c(0.5, 0.25)) +
    mapTheme() +
    theme(legend.position = "none",
          panel.border = element_blank())
  
  ggsave(plot = BGs_clusters_maps_list[[name]],
         filename = paste0("~outputs/Plots/~Deliverable maps/34a_BG_cluster_maps_allYears/",
                           name,
                           "_map.pdf"), 
         device = "pdf",
         units = "in",
         dpi = 300,
         width = 8.5,
         height = 11)
  
}








city <- 15
name <- names(BGs_clusters_maps_list)[city]

ggplot() +
  geom_sf(data = land_list[[city]],
          # fill = "darkgray",
          fill = "#252525",
          alpha = 0.9,
          color = NA) +
  geom_sf(data = BGs_per100_localI[[city]], aes(fill = cluster),
          color = "#999999",
          size = 0.01,
          # color = NA,
          alpha = 0.8,
          inherit.aes = FALSE) + 
  scale_fill_manual(name = "Statistically Significant\nClusters / Outliers\n(p value < 0.05)",
                    values = c("#252525", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                    limits = c("insignificant", "high-high", "high-low", "low-high", "low-low"),
                    labels = c("Not Significant", "Hot Spot", "High Crime in a\nLower Crime Area", "Low Crime in a\nHigher Crime Area", "Cold Spot")) +
  geom_sf(data = hydrology_list[[city]],
          fill = "#525252",
          # fill = "#97DBF2",
          color = NA) +
  geom_sf(data = roads_list[[city]],
          # color = "#feb24c",
          color = "#737373"
          # color = "gray"
  ) +
  # geom_sf(data = border,
  #         fill = NA,
  #         size = 0.3,
  #         col = "8e8e8e") +
  scale_size_manual(guide = FALSE,
                    breaks = c("Major", "Minor"),
                    values = c(0.5, 0.25)) +
  mapTheme() +
  theme(legend.position = "none",
        panel.border = element_blank())




















































## 1. Export as rds ----
# saveRDS(BGs_per100_I_byYear_plots,
#         "~outputs/30/34a_BGs_per100_I_byYear_plots.rds")

## 2. Export as png ----
walk2(Siegel_gunCount_MoransI_plots,
     names(Siegel_gunCount_MoransI_plots),
     ~ save_plot(plot = .x,
                 filename = paste("~outputs/Plots/34a_siegel_gunCount_MoransI_plots/34a_",
                                  .y,
                                  "Siegel_gunCount_MoransI.png",
                                  sep = ""),
                 dpi = 72,
                 nrow = 3,
                 base_height = 5,
                 base_width = 8))

## 3a. Export as png ----
walk2(BG_cluster_maps,
     names(BG_cluster_maps),
     ~ save_plot(plot = .x,
                 filename = paste("~outputs/Plots/34a_BG_cluster_maps/34a_",
                                  .y,
                                  "BG_cluster_map.png",
                                  sep = ""),
                 dpi = 72,
                 # nrow = 3,
                 base_height = 8,
                 base_width = 8))

## 3b. Export as png ----

# create directories for each city
# walk(paste0("~outputs/Plots/34a_BG_cluster_maps_byYear/",
#             names(BG_cluster_maps_byYear)),
#      dir.create)

for (city in seq_len(length(BG_cluster_maps_byYear))) {
  print(names(BG_cluster_maps_byYear)[city])
  
  for (year in seq_len(length(years_byCity[[city]]))) {
    print(years_byCity[[city]][[year]])
    
    ggsave(plot = BG_cluster_maps_byYear[[city]][[year]],
           filename = paste("~outputs/Plots/34a_BG_cluster_maps_byYear/",
                            names(BG_cluster_maps_byYear)[city],
                            "/34a_",
                            years_byCity[[city]][[year]],
                            "_BG_cluster_map_byYear.png",
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

years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")

tmp_folders <- list.files(path = "~outputs/Plots/34a_BG_cluster_maps_byYear",
                          full.names = TRUE)[2:35]

# run piece-by-piece (4 or 5 at a time)
# running whole loop stalls R
for (city in seq_len(length(years_byCity))[34]) {
  print(names(years_byCity)[city])
  
  tmp_files <- list.files(path = tmp_folders[city],
                          full.names = TRUE)
  
  tmp_animation <- map(tmp_files,
                       image_read) %>% 
    image_join() %>% 
    image_animate(fps = 0.5)
  
  image_write(tmp_animation,
              paste0("~outputs/Plots/34a_BG_cluster_maps_byYear/~gifs/",
                     names(years_byCity)[city],
                     "_BG_per100_byYear.gif"))
  rm(tmp_animation)
  
}
