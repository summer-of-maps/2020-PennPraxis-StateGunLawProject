##########################################################################
# This script:
# 1. Calculates Moran's I for:
#   a. Census tracts (gun crimes and population)
#   b. Block groups (gun crimes and population)
# 2. Calculates Moran's I of gun crimes per 100 for block groups by year
# 3. Calculate Local Moran's I of gun crimes per 100 for block groups
#   Reference: https://rpubs.com/quarcs-lab/spatial-autocorrelation
#   a. Overall
#   b. By Year
# 4. Joins Moran's I tables with census variables dataframe
#
# Exports: 
# 1a. tracts_I as 34_tracts_I.rds
# 1b. BGs_I as 34_BGs_I.rds
# 2. BGs_per100_I_byYear as 34_BGs_per100_I_byYear.rds
# 3a. BGs_per100_localI as 34_BGs_per100_localI.rds
# 3b. BGs_per100_localI_byYear as 34_BGs_per100_localI_byYear.rds
# 4. BGs_per100_localI_census as 34_BGs_per100_localI_census.rds
#
# To-do:
# 1. 
##########################################################################

## 1a. ----
# tracts_crimeCounts <- readRDS("~outputs/30/33_tracts_crimeCounts.rds")
tracts_I <- map(tracts_crimeCounts,
                function(x) map(x,
                                function(y) find_Moran_I(shp = y,
                                                         var_name = "gun_count",
                                                         queen = TRUE,
                                                         style = "W")))

tracts_pop_I <- map(tracts_crimeCounts,
                    function(x) map(x,
                                    function(y) find_Moran_I(shp = y,
                                                             var_name = "pop",
                                                             queen = TRUE,
                                                             style = "W")))

tracts_per100_I <- map(tracts_crimeCounts,
                    function(x) map(x,
                                    function(y) find_Moran_I(shp = y,
                                                             var_name = "guns_per100",
                                                             queen = TRUE,
                                                             style = "W")))

## 1b. ----
# BGs_crimeCounts <- readRDS("~outputs/30/33_BGs_crimeCounts.rds")
BGs_I <- map(BGs_crimeCounts,
             function(x) map(x,
                             function(y) find_Moran_I(shp = y,
                                                      var_name = "gun_count",
                                                      queen = TRUE,
                                                      style = "W")))

BGs_pop_I <- map(BGs_crimeCounts,
             function(x) map(x,
                             function(y) find_Moran_I(shp = y,
                                                      var_name = "pop",
                                                      queen = TRUE,
                                                      style = "W")))

BGs_per100_I <- map(BGs_crimeCounts,
                 function(x) map(x,
                                 function(y) find_Moran_I(shp = y,
                                                          var_name = "guns_per100",
                                                          queen = TRUE,
                                                          style = "W")))


## 2. ----
# BGs_crimeCounts_byYear <- readRDS("~outputs/30/33_BGs_crimeCounts_byYear.rds")
plan(multiprocess)
BGs_per100_I_byYear <- future_map2(BGs_crimeCounts_byYear[c(1:14, 16:34)],
                                   BG_selection_list$byPlace[c(1:14, 16:34)],
                           function(counts, BGs) 
                             map(counts,
                                 function(y) BGs %>% 
                                   dplyr::select(GEOID, geometry) %>% 
                                   left_join(y,
                                             by = "GEOID") %>% 
                                   find_Moran_I(shp = .,
                                                var_name = "guns_per100",
                                                queen = TRUE,
                                                style = "W")))


## 3a. ----
BGs_per100_localI <- future_map(BGs_crimeCounts$byPlace[c(1:14, 16:34)],
                         ~ find_localMoran(.x,
                                           var_name = "guns_per100",
                                           queen = TRUE,
                                           style = "W") %>% 
                           as.data.frame() %>% 
                           cbind(.,
                                 .x) %>% 
                           dplyr::select(GEOID, 
                                         NAME,
                                         variable, 
                                         pop, 
                                         gun_count, 
                                         guns_per100, 
                                         Local_Morans_I = Ii,
                                         P_Value = `Pr(z > 0)`,
                                         geometry) %>% 
                           mutate(guns_per100_norm = guns_per100 - mean(guns_per100),
                                  Local_Morans_I_norm = Local_Morans_I - mean(Local_Morans_I),
                                  significant = ifelse(P_Value <= 0.05, "Yes", "No"),
                                  cluster = case_when(significant == "No" ~ "insignificant",
                                                      significant == "Yes" & guns_per100_norm > 0 & Local_Morans_I_norm > 0 ~ "high-high",
                                                      significant == "Yes" & guns_per100_norm < 0 & Local_Morans_I_norm < 0 ~ "low-low",
                                                      significant == "Yes" & guns_per100_norm > 0 & Local_Morans_I_norm < 0 ~ "high-low",
                                                      significant == "Yes" & guns_per100_norm < 0 & Local_Morans_I_norm > 0 ~ "low-high",
                                                      TRUE ~ NA_character_),
                                  cluster = factor(cluster,
                                                   levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))) %>% 
                           st_sf())

## 3b. ----
BGs_per100_localI_byYear <- future_map2(BGs_crimeCounts_byYear[c(1:14, 16:34)],
                                        BG_selection_list$byPlace[c(1:14, 16:34)],
                                  function(counts, BGs) map(counts,
                                                  function(y) BGs %>% 
                                                    dplyr::select(GEOID, geometry) %>% 
                                                    left_join(y,
                                                              by = "GEOID") %>% 
                                                    find_localMoran(shp = .,
                                                                           var_name = "guns_per100",
                                                                           queen = TRUE,
                                                                           style = "W") %>% 
                                                    as.data.frame() %>%
                                                    cbind(.,
                                                          y) %>% 
                                                    right_join(BGs %>% 
                                                                 dplyr::select(GEOID, geometry),
                                                               by = "GEOID") %>% 
                                                    dplyr::select(GEOID, 
                                                                  # NAME, 
                                                                  # variable, 
                                                                  pop = worldPop, 
                                                                  gun_count, 
                                                                  guns_per100, 
                                                                  Local_Morans_I = Ii,
                                                                  P_Value = `Pr(z > 0)`,
                                                                  geometry) %>% 
                                                    mutate(guns_per100_norm = guns_per100 - mean(guns_per100),
                                                           Local_Morans_I_norm = Local_Morans_I - mean(Local_Morans_I),
                                                           significant = ifelse(P_Value <= 0.05, "Yes", "No"),
                                                           cluster = case_when(significant == "No" ~ "insignificant",
                                                                               significant == "Yes" & guns_per100_norm > 0 & Local_Morans_I_norm > 0 ~ "high-high",
                                                                               significant == "Yes" & guns_per100_norm < 0 & Local_Morans_I_norm < 0 ~ "low-low",
                                                                               significant == "Yes" & guns_per100_norm > 0 & Local_Morans_I_norm < 0 ~ "high-low",
                                                                               significant == "Yes" & guns_per100_norm < 0 & Local_Morans_I_norm > 0 ~ "low-high",
                                                                               TRUE ~ NA_character_),
                                                           cluster = factor(cluster,
                                                                            levels = c("insignificant", "high-high", "high-low", "low-high", "low-low"))) %>% 
                                                    st_sf()))

## 4. ----
BGs_per100_localI <- readRDS("~outputs/30/34_BGs_per100_localI.rds")
allState_censusDat_BGs <- readRDS("~outputs/20/22_allState_censusDat_BGs.rds")

BGs_per100_localI_census <- map2(
  BGs_per100_localI,
  allState_censusDat_BGs[c(1:14, 16:34)],
  ~ .x %>% 
    dplyr::select(-c(NAME, variable, pop)) %>% 
    left_join(.y,
              by = "GEOID")
  
)


## 1a. Export as rds ----
# saveRDS(tracts_I,
#         file = "~outputs/30/34_tracts_I.rds")
# saveRDS(tracts_pop_I,
#         file = "~outputs/30/34_tracts_pop_I.rds")
# saveRDS(tracts_per100_I,
#         file = "~outputs/30/34_tracts_per100_I.rds")

## 1b. Export as rds ----
# saveRDS(BGs_I,
#         file = "~outputs/30/34_BGs_I.rds")
# saveRDS(BGs_pop_I,
#         file = "~outputs/30/34_BGs_pop_I.rds")
# saveRDS(BGs_per100_I,
#         file = "~outputs/30/34_BGs_per100_I.rds")

## 2. Export as rds ----
# saveRDS(BGs_per100_I_byYear,
#         file = "~outputs/30/34_BGs_per100_I_byYear.rds")

## 3a. Export as rds ----
# saveRDS(BGs_per100_localI,
#         file = "~outputs/30/34_BGs_per100_localI.rds")

## 3b. Export as rds ----
# saveRDS(BGs_per100_localI_byYear,
#         file = "~outputs/30/34_BGs_per100_localI_byYear.rds")

## 4. Export as rds ----
# saveRDS(BGs_per100_localI_census,
#         "~outputs/30/34_BGs_per100_localI_census.rds")
