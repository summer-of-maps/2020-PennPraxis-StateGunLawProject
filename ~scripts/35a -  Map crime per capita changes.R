##########################################################################
# This script:
# 1. Maps changes in crimes per capita
#   a. By block group
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
##########################################################################

## 1a. ----

# test with New York
changes_tmp <- BGs_crimeChange$`New York`
geo_tmp <- BG_selection_list$byCaveHull$`New York`
basemap_tmp <- basemap_list$`New York`
years_tmp <- years_byCity$`New York`

NY_tmp <- geo_tmp %>% 
  dplyr::select(GEOID, geometry, pop = estimate) %>% 
  left_join(changes_tmp,
            by = "GEOID") %>% 
  mutate(CrimeIncrease = ifelse(pop == 0,
                                NA,
                                CrimeIncrease),
         CrimeMap = case_when(CrimeIncrease %in% c("Yes (significant)", "Yes (not significant)") ~ CrimeIncrease,
                              CrimeIncrease %in% c("No (not significant)", "No (significant)") ~ "No",
                              TRUE ~ NA_character_))

ggmap(basemap_tmp) +
  geom_sf(data = NY_tmp,
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
