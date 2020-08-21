##########################################################################
# This script:
# 1. Reads in the raw crimes data
# 2. Filters for only the gun crimes
#
# Exports: 
# 1. gun_crimes_df as 11_gun_crimes_df.rds
# 
# To-do:
#
##########################################################################

## 1-2. ----
# guns_df <- vroom(file.path(data_dir, "Full_City_Dataset/alldatahierarchies.csv"),
#                        col_select = -1) %>%  # don't import column 1
#   filter(allgun == 1)

allCrimes_df <- vroom(file.path(data_dir, "Full_City_Dataset/alldatahierarchies_Azavea.csv"))

guns_df <- allCrimes_df %>%
  filter(allgun == 1) %>% 
  mutate(lat = as.character(lat),
         lon = as.character(lon),
         size = as.character(size))

## 1. Export as rds ----
# saveRDS(guns_df,
#         "~outputs/10/11_guns_df.rds")