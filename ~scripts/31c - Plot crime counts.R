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

## 1a. Export as rds ----
# saveRDS(gunIncidentsByCityPlot,
#         "~outputs/30/31c_gunIncidentsByCityPlot.rds")

## 1b. Export as rds ----
# saveRDS(gunIncidentsByYear_plots,
#         "~outputs/30/31c_gunIncidentsByYear_plots.rds")
        
