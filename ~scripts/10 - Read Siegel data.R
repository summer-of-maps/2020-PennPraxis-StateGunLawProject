##########################################################################
# This script:
# 1. Reads in the raw Siegel data
##########################################################################

siegel_raw <- read_csv(file.path(data_dir, "Azavea_Siegel_Data.csv")) %>% 
  dplyr::select(-1) %>% 
  mutate(law = as.factor(law),
         state = as.factor(state),
         exist = as.factor(exist),
         Category.Code = as.factor(Category.Code),
         Category = as.factor(Category),
         Sub.Category = as.factor(Sub.Category))
