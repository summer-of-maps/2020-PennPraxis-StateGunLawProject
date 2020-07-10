##########################################################################
# This script:
# 1. Plots Moran's I for each city by year
# 2. Plots Siegel Score, Crime Count, and Moran's I for each city in a single plot
# 3. Local Moran's I hotspot maps
#   a. All years
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
# BGs_per100_I_byYear <- readRDS("~outputs/30/33_BGs_crimeCounts_byYear.rds")
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
Siegel_gunCount_MoransI_plots <- pmap(.l = list(BGs_per100_I_byYear_plots,
                                      gunIncidentsByYear_plots,
                                      gunCount_byYear_list),
                                      function(a, b, c) 
                                        plot_grid_diffAxes(a,
                                                           b,
                                                           siegelSum_plots[[c[1,] %>% pull(state)]],
                                                           ncol = 1,
                                                           align = "v"))

## 3a. ----
BG_cluster_maps <- map2(basemap_list,
     BGs_per100_localI,
     ~ ggmap(.x,
             darken = 0.5) +
       geom_sf(data = .y, aes(fill = factor(cluster,
                                              levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))),
               color = "gray",
               inherit.aes = FALSE) + 
       scale_fill_manual(name = "Local Clusters/Outliers",
                         values = c("white", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                         limits = c("insignificant", "high-high", "high-low", "low-high", "low-low")) +
       mapTheme() +
       labs(title = "Gun Crime Clusters and Outliers",
            subtitle = "Significant at p value < 0.05"))

## 3b. ----
BG_cluster_maps_byYear <- map2(basemap_list,
                               BGs_per100_localI_byYear,
                               function(basemap, city) 
                                 map(city,
                                     function(year) 
                                       ggmap(basemap,
                                             darken = 0.5) +
                                       geom_sf(data = year, aes(fill = factor(cluster,
                                                                              levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))),
                                               color = "gray",
                                               inherit.aes = FALSE) +
                                       scale_fill_manual(name = "Local Clusters/Outliers",
                                                         values = c("white", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                                                         limits = c("insignificant", "high-high", "high-low", "low-high", "low-low")) +
                                       mapTheme() +
                                       labs(title = "Gun Crime Clusters and Outliers",
                                            subtitle = "Significant at p value < 0.05")))

# blah <- BGs_per100_localI_byYear$Atlanta %>% bind_rows(.id = "year")
# 
# plt <- ggmap(basemap_list$Atlanta) +
#   geom_sf(data = blah, aes(fill = factor(cluster,
#                                        levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))),
#           color = "gray",
#           inherit.aes = FALSE) + 
#   scale_fill_manual(name = "Local Clusters/Outliers",
#                     values = c("white", "#ca0020", "#f4a582", "#92c5de", "#0571b0"),
#                     limits = c("insignificant", "high-high", "high-low", "low-high", "low-low")) +
#   mapTheme() +
#   labs(title = "Gun Crime Clusters and Outliers",
#        subtitle = "Significant at p value < 0.05")
# 
# tm_shape(blah) + tm_polygons(col = "cluster") + 
#   tm_facets(along = "year", free.coords = FALSE)

## 1. Export as rds ----
# saveRDS(BGs_per100_I_byYear_plots,
#         "~outputs/30/34a_BGs_per100_I_byYear_plots.rds")

## 2. Export as png ----
map2(Siegel_gunCount_MoransI_plots,
     names(Siegel_gunCount_MoransI_plots),
     ~ save_plot(plot = .x,
                 filename = paste("~outputs/Plots/34a_siegel_gunCount_MoransI_plots/34a_",
                                  .y,
                                  "Siegel_gunCount_MoransI.png",
                                  sep = ""),
                 dpi = 72,
                 nrow = 3,
                 base_height = 4,
                 base_width = 8))

## 3a. Export as png ----
map2(BG_cluster_maps,
     names(BG_cluster_maps),
     ~ save_plot(plot = .x,
                 filename = paste("~outputs/Plots/34a_BG_cluster_maps/34a_",
                                  .y,
                                  "BG_cluster_map.png",
                                  sep = ""),
                 dpi = 72,
                 nrow = 3,
                 base_height = 4,
                 base_width = 8))
