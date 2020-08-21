##########################################################################
# This script:
# 1. Maps changes in crimes per capita
#   a. By block group
# 2. Creates deliverable maps
#   a. Increases in crimes per capita over the duration of the data
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
##########################################################################

## 1a. ----
BG_change_maps <- vector("list", length(BGs_crimeChange)) %>% 
  set_names(names(BGs_crimeChange))

for (city in seq_len(length(BG_change_maps))) {
  print(names(BG_change_maps)[city])
  
  changes_tmp <- BGs_crimeChange[[city]]
  basemap_tmp <- basemap_list[[city]]
  years_tmp <- years_byCity[[city]]
  
  geo_tmp <- BG_selection_list$byCaveHull[[city]] %>% 
    dplyr::select(GEOID, geometry, pop = estimate) %>% 
    left_join(changes_tmp,
              by = "GEOID") %>% 
    mutate(CrimeIncrease = ifelse(pop == 0,
                                  NA,
                                  CrimeIncrease),
           CrimeMap = case_when(CrimeIncrease %in% c("Yes (significant)", "Yes (not significant)") ~ CrimeIncrease,
                                CrimeIncrease %in% c("No (not significant)", "No (significant)") ~ "No",
                                TRUE ~ NA_character_))
  
    BG_change_maps[[city]] <-   
      ggmap(basemap_tmp) +
      geom_sf(data = geo_tmp,
              aes(fill = factor(CrimeMap),
                  color = factor(CrimeMap)),
              # color = NA,
              alpha = 0.5,
              size = 1.25,
              inherit.aes = FALSE) +
      scale_fill_manual(name = "Did Crime Increase?",
                        values = c("#ca0020", "#ca0020", "lightgray"),
                        limits = c("Yes (significant)", "Yes (not significant)", "No"),
                        labels = c("Increase (p<0.1)", "Increase (p>0.1)", "Decrease")) +
      scale_color_manual(name = "Did Crime Increase?",
                         values = c("black", NA, NA),
                         limits = c("Yes (significant)", "Yes (not significant)", "No"),
                         labels = c("Increase (p<0.1)", "Increase (p>0.1)", "Decrease")) +
      mapTheme() +
      labs(title = "Change in Gun Crimes by Census Block Group",
           subtitle = paste("Study period:", min(years_tmp),
                            "-", max(years_tmp)))
  
}

## 2. ----
city_bounds <- readRDS("~outputs/10/14_city_bounds.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")
land_list <- readRDS("~outputs/10/15_land_list.rds")
context_bbox_list <- readRDS("~outputs/10/15_context_bbox_list.rds")
context_mask_list <- readRDS("~outputs/10/15_context_mask_list.rds")
hydrology_list <- readRDS("~outputs/10/15_hydrology_list.rds")
roads_list <- readRDS("~outputs/10/15_roads_list.rds")
years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
BGs_crimeChange <- readRDS("~outputs/30/35_BGs_crimeChange.rds")

# legend
city <- 6
name <- names(BG_change_maps_deliverables)[city]
changes_tmp <- BGs_crimeChange[[name]]
years_tmp <- years_byCity[[name]]
geo_tmp <- BG_selection_list$byPlace[[name]] %>% 
  dplyr::select(GEOID, geometry, pop = estimate) %>% 
  left_join(changes_tmp,
            by = "GEOID") %>% 
  mutate(CrimeIncrease = ifelse(pop == 0,
                                NA,
                                CrimeIncrease),
         CrimeMap = case_when(CrimeIncrease %in% c("Yes (significant)", "Yes (not significant)") ~ CrimeIncrease,
                              CrimeIncrease %in% c("No (not significant)", "No (significant)") ~ "No",
                              TRUE ~ NA_character_))

legend_plot <-   
  ggplot() +
  geom_sf(data = land_list[[name]],
          # fill = "darkgray",
          fill = "#252525",
          alpha = 0.9,
          color = NA) +
  geom_sf(data = geo_tmp,
          aes(fill = factor(CrimeMap),
              color = factor(CrimeMap)),
          alpha = 0.8,
          inherit.aes = FALSE) +
  scale_fill_manual(name = "Did Crime Increase?",
                    na.value = "#252525", 
                    values = c("#ca0020", "#ca0020", "#0571b0"),
                    limits = c("Yes (significant)", "Yes (not significant)", "No"),
                    labels = c("Increase (p<0.1)", "Increase (p>0.1)", "Decrease")) +
  scale_color_manual(name = "Did Crime Increase?",
                     values = c("yellow", "#999999", "#999999"),
                     limits = c("Yes (significant)", "Yes (not significant)", "No"),
                     labels = c("Increase (p<0.1)", "Increase (p>0.1)", "Decrease")) +
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
  labs(title = "Change in Gun Crimes by Census Block Group",
       subtitle = paste("Study period:", min(years_tmp),
                        "-", max(years_tmp)))

legend_tmp <- get_legend(legend_plot)
legend <- ggpubr::as_ggplot(legend_tmp)

ggsave(plot = legend,
       filename = paste0("~outputs/Plots/~Deliverable maps/35a_BG_crimeIncrease_maps/Illustrator/",
                         # name,
                         "legend.pdf"),
       device = "pdf",
       units = "in",
       dpi = 300,
       width = (120/72),
       height = (200/72)
)
#


BG_change_maps_deliverables <- vector("list", length(BGs_crimeChange)) %>% 
  set_names(names(BGs_crimeChange))

for (city in seq_len(length(BG_change_maps_deliverables))) {
  
  name <- names(BG_change_maps_deliverables)[city]
  
  print(name)
  
  changes_tmp <- BGs_crimeChange[[name]]
  years_tmp <- years_byCity[[name]]
  
  geo_tmp <- BG_selection_list$byPlace[[name]] %>% 
    dplyr::select(GEOID, geometry, pop = estimate) %>% 
    left_join(changes_tmp,
              by = "GEOID") %>% 
    mutate(CrimeIncrease = ifelse(pop == 0,
                                  NA,
                                  CrimeIncrease),
           CrimeMap = factor(case_when(CrimeIncrease %in% c("Yes (significant)", "Yes (not significant)") ~ CrimeIncrease,
                                CrimeIncrease %in% c("No (not significant)", "No (significant)") ~ "No",
                                TRUE ~ NA_character_),
                             levels = c("No", "Yes (not significant)", "Yes (significant)")))
  
  BG_change_maps_deliverables[[city]] <-   
    ggplot() +
    geom_sf(data = land_list[[name]],
            # fill = "darkgray",
            fill = "#252525",
            alpha = 0.9,
            color = NA) +
    geom_sf(data = geo_tmp,
            aes(fill = CrimeMap),
            color = NA,
            alpha = 0.8,
            inherit.aes = FALSE) +
    scale_fill_manual(name = "Did Crime Increase?",
                      na.value = "#252525", 
                      values = c("#ca0020", "#ca0020", "#0571b0"),
                      limits = c("Yes (significant)", "Yes (not significant)", "No"),
                      labels = c("Increase (p<0.1)", "Increase (p>0.1)", "Decrease")) +
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
                      values = c(0.25, 0.125)) +
    geom_sf(data = geo_tmp,
            color = "#999999",
            size = 0.01,
            fill = NA,
            alpha = 0.8,
            inherit.aes = FALSE) +
    geom_sf(data = geo_tmp,
            aes(color = CrimeMap),
            size = 0.5,
            fill = NA,
            alpha = 0.8,
            inherit.aes = FALSE) +
    scale_color_manual(name = "Did Crime Increase?",
                       values = c(NA, NA, "yellow"),
                       limits = c("Yes (not significant)", "No", "Yes (significant)"),
                       labels = c("Increase (p>0.1)", "Decrease", "Increase (p<0.1)")) +
    mapTheme() +
    # labs(title = "Change in Gun Crimes by Census Block Group",
    #      subtitle = paste("Study period:", min(years_tmp),
    #                       "-", max(years_tmp))) +
    theme(legend.position = "none",
          panel.border = element_blank())
  
  
  ggsave(plot = BG_change_maps_deliverables[[name]],
         filename = paste0("~outputs/Plots/~Deliverable maps/35a_BG_crimeIncrease_maps/",
                           name,
                           "_map.pdf"), 
         device = "pdf",
         units = "in",
         dpi = 300,
         width = 8.5,
         height = 11)
  
}

## 1a. Export as png ----
# saveRDS(BG_change_maps,
#         "~outputs/~large_files/35a_BG_change_maps.rds")

BG_change_maps <- readRDS("~outputs/~large_files/35a_BG_change_maps.rds")

for (city in seq_len(length(BG_change_maps))) {
  print(names(BG_change_maps)[city])
  
  ggsave(plot = BG_change_maps[[city]],
         filename = paste("~outputs/Plots/35a_BG_crimeChange_maps/35a_",
                          names(BG_change_maps)[city],
                          "_BG_crimeChange_map.png",
                          sep = ""),
         device = "png",
         units = "in",
         dpi = 144,
         width = 7,
         height = 7)
  
}
