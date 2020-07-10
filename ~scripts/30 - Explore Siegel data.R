##########################################################################
# This script:
# 1. Plots Siegel Scores by year for each state
#   (a) sum-of-laws score
#
# Exports: 
# 1. siegelSum_plots as 30_siegelSum_plots.rds
# 
# To-do:
# 1. PCA scores?
##########################################################################

## 1a. ----
siegelSum_plots <- map(siegelSum_list,
                       ~ .x %>% 
      ggplot(aes(x = year,
                 y = score)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      plotTheme() +
      scale_x_continuous(breaks = seq(min(siegelSum$year), max(siegelSum$year), 2)) +
      scale_y_continuous(breaks = seq(min(siegelSum$score), max(siegelSum$score), 10),
                         limits = c(min(siegelSum$score), max(siegelSum$score))) +
      labs(title = "Siegel Scores",
           x = "Year",
           y = "Siegel Score (sum of gun laws)"))

## 1a. Export as rds ----
# saveRDS(siegelSum_plots,
#         "~outputs/30/30_siegelSum_plots.rds")
