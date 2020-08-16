##########################################################################
# This script:
# 1. Plots gun crimes:
#   (a) total by city
#   (b) by year and by city
#
# Exports: 
# 1. gunIncidentsByCityPlot as 31c_gunIncidentsByCityPlot.rds
# 2. gunIncidentsByYear_plots as 31c_gunIncidentsByYear_plots.rds
#
# To-do:
# 1. 
##########################################################################

## 1a. ----
gunIncidentsByCityPlot <- ggplot(gunIncident_summary,
                                 aes(x = reorder(city, gun_count), y = gun_count)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  coord_flip() +
  plotTheme()

## 1b. ----
# gunIncidentsByYear <- ggplot(gunCount_byYear,
#                                  aes(x = year, y = gun_count)) +
#   geom_bar(position = "dodge",
#            stat = "identity") +
#   facet_wrap(~ city, scales = "free",
#              ncol = 5) +
#   scale_x_continuous(breaks = scales::pretty_breaks()) +
#   plotTheme() +
#   labs(title = "Gun crimes by year",
#        x = "Year",
#        y = "Gun crimes")

gunCount_byYear_list <- readRDS(file = "~outputs/30/31_gunCount_byYear_list.rds")
gunIncidentsByYear_plots <- readRDS("~outputs/30/31c_gunIncidentsByYear_plots.rds")

gunIncidentsByYear_plots <- map(gunCount_byYear_list,
                                ~ .x %>% 
                                  ggplot(aes(x = year,
                                             y = gun_count)) +
                                  geom_bar(position = "dodge",
                                           stat = "identity") +
                                  scale_x_continuous(breaks = seq(min(.x$year), max(.x$year), 2)) + 
                                  plotTheme() +
                                  labs(title = "Gun crimes by year",
                                       x = "Year",
                                       y = "Gun crimes"))


gunIncidentsByYear_plots_deliverable <- vector("list", length(gunCount_byYear_list)) %>% 
  set_names(names(gunCount_byYear_list))

for (city in seq_len(length(gunCount_byYear_list))){

  name <- names(gunCount_byYear_list)[city]
  print(name)
  
  gunIncidentsByYear_plots_deliverable[[name]] <- 
    ggplot(gunCount_byYear_list[[name]],
           aes(x = year,
           y = gun_count)) +
    geom_line(size = 2,
              color = "yellow") +
    scale_y_continuous(limits = c(0, NA),
                       label = comma) +
    scale_x_continuous(limits = c(min(gunCount_byYear_list[[name]]$year), min(max(gunCount_byYear_list[[name]]$year), 2019)),
                       breaks = seq(min(gunCount_byYear_list[[name]]$year), min(max(gunCount_byYear_list[[name]]$year), 2019), 2)) + 
    # ylim(0, NA) +
    plotTheme() +
    labs(title = "Annual Gun Crimes") +
    theme(plot.title = element_text(color = "white", face = "bold", size = 24),
          axis.title = element_blank(),
          axis.text = element_text(colour = "white", size = 18),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          rect = element_rect(fill = "transparent"))
  
  ggsave(plot = gunIncidentsByYear_plots_deliverable[[name]] ,
         filename = paste0("~outputs/Plots/~Deliverable maps/35a_BG_crimeIncrease_maps/",
                           name,
                           "_lineGraph.pdf"), 
         device = "pdf",
         units = "in",
         dpi = 300,
         width = 8,
         height = 4)
  
}


gunIncidentsByYear_plots_deliverable$Auburn <-
  ggplot(gunCount_byYear_list$Auburn,
         aes(x = year,
             y = gun_count)) +
  geom_line(size = 2,
            color = "yellow") +
  scale_y_continuous(limits = c(0, NA),
                     label = comma) +
  scale_x_continuous(limits = c(min(gunCount_byYear_list$Auburn$year), 2019),
                     labels = c(2018, 2019),
                     breaks = c(2018, 2019)) + 
  # ylim(0, NA) +
  plotTheme() +
  labs(title = "Annual Gun Crimes") +
  theme(plot.title = element_text(color = "white", face = "bold", size = 24),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", size = 18),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        rect = element_rect(fill = "transparent"))

ggsave(plot = gunIncidentsByYear_plots_deliverable$Auburn ,
       filename = paste0("~outputs/Plots/~Deliverable maps/35a_BG_crimeIncrease_maps/",
                         # name,
                         "Auburn_lineGraph.pdf"), 
       device = "pdf",
       units = "in",
       dpi = 300,
       width = 8,
       height = 4)



## 1a. Export as rds ----
# saveRDS(gunIncidentsByCityPlot,
#         "~outputs/30/31c_gunIncidentsByCityPlot.rds")

## 1b. Export as rds ----
# saveRDS(gunIncidentsByYear_plots,
#         "~outputs/30/31c_gunIncidentsByYear_plots.rds")
        
