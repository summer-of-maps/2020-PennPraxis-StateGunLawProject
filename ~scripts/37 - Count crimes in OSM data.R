##########################################################################
# This script:
# 1. Counts gun crimes in the area of the OSM features collected in 14.R
#   a. Alcohol outlets (100ft buffer)
#   b. Parks
#
# Exports: 
# 1. alcohol_buffer100ft_list as 37_alcohol_buffer100ft_list.rds
# 2. parks_gunCount_list as 37_parks_gunCount_list.rds
# 
# To-do:
# 1. 
#
##########################################################################

## 1a. ----
bbox_sfs <- readRDS("~outputs/10/14_bbox_sfs.rds")
alcohol_outlets <- readRDS("~outputs/10/14_alcohol_outlets.rds")
proj_list <- readRDS("~outputs/10/14_proj_list.rds")
city_laws_list <- readRDS("~outputs/10/14_city_laws_list.rds")


guns_proj_list <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))
alcohol_buffer100ft_list <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

## All cities ----
for (city in seq_len(length(alcohol_buffer100ft_list))) {
  
  name <- names(alcohol_buffer100ft_list)[city]
  print(name)
  
  guns_proj_list[[name]] <- guns_list_shp[[name]] %>% 
    st_transform(proj_list[[name]])
  
  tmp <- guns_proj_list[[name]]
  
  alcohol_buffer100ft_list[[name]] <- alcohol_outlets[[name]] %>% 
    st_transform(proj_list[[name]]) %>% 
    st_buffer(dist = 100) %>% # 100 ft
    dplyr::select(osm_id, name, amenity, geometry) %>% 
    mutate(gun_count = lengths(st_intersects(.,
                                             tmp)
                               )
           )
  
}

# Philadelphia ----
guns_proj_list$Philadelphia <- guns_list_shp$Philadelphia %>% 
  st_transform(proj_list$Philadelphia)

alcohol_buffer100ft_list$Philadelphia <- alcohol_outlets$Philadelphia %>% 
  st_buffer(dist = 100) %>% 
  dplyr::select(osm_id, name, amenity, shop, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$Philadelphia)))

# guns are allowed in Philly alcohol outlets
guns_list_shp$Philadelphia$clean_report_date %>% summary() # gun crimes from Jan 2006 to April 2020
phl_gunSum <- guns_list_shp$Philadelphia %>% nrow() # 258,000 total gun crimes during that time
phl_alcohol_count <- alcohol_buffer100ft_list$Philadelphia %>% nrow() # 188 alcohol outlets in Philly per OSM
phl_alcohol_gunSum <- sum(alcohol_buffer100ft_list$Philadelphia$gun_count) # 2,462 gun crimes within 100 ft of Philly alcohol outlets
phl_alcohol_gunPerc <- phl_alcohol_gunSum / phl_gunSum # which is 1.0% of all gun crimes

phl_alcohol_gunSum / phl_alcohol_count # So 13 gun crimes per outlet during that ~ 15 yr period

# San Francisco ----
guns_proj_list$`San Francisco` <- guns_list_shp$`San Francisco` %>% 
  st_transform(proj_list$`San Francisco`)

alcohol_buffer100ft_list$`San Francisco` <- alcohol_outlets$`San Francisco` %>% 
  st_buffer(dist = 100) %>% 
  dplyr::select(osm_id, name, amenity, shop, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$`San Francisco`)))

# guns are not allowed in SF alcohol outlets
guns_list_shp$`San Francisco`$clean_occur_date %>% summary() # gun crimes from Jan 2003 to April 2020
sf_gunSum <- guns_list_shp$`San Francisco` %>% nrow() # 72,500 total gun crimes during that time
sf_alcohol_count <- alcohol_buffer100ft_list$`San Francisco` %>% nrow() # 347 alcohol outlets in SF per OSM
sf_alcohol_gunSum <- sum(alcohol_buffer100ft_list$`San Francisco`$gun_count) # 4,432 gun crimes within 100 ft of SF alcohol outlets
sf_alcohol_gunPerc <- sf_alcohol_gunSum / sf_gunSum # which is 6.1% of all gun crimes

sf_alcohol_gunSum / sf_alcohol_count # So 13 gun crimes per outlet during that ~ 15 yr period

# Louisville ----
guns_proj_list$Louisville <- guns_list_shp$Louisville %>% 
  st_transform(proj_list$Louisville)

alcohol_buffer100ft_list$Louisville <- alcohol_outlets$Louisville %>% 
  st_buffer(dist = 100) %>% 
  dplyr::select(osm_id, name, amenity, shop, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$Louisville)))

# guns are not allowed in Louisville alcohol outlets
guns_list_shp$Louisville$clean_report_date %>% summary() # gun crimes from March 2006 to April 2020
lou_gunSum <- guns_list_shp$Louisville %>% nrow() # 29,000 total gun crimes during that time
lou_alcohol_count <- alcohol_buffer100ft_list$Louisville %>% nrow() # 101 alcohol outlets in Louisville per OSM
lou_alcohol_gunSum <- sum(alcohol_buffer100ft_list$Louisville$gun_count) # 178 gun crimes within 100 ft of Louisville alcohol outlets
lou_alcohol_gunPerc <- lou_alcohol_gunSum / lou_gunSum # which is 0.6% of all gun crimes

lou_alcohol_gunSum / lou_alcohol_count # So 1.8 gun crimes per outlet during that ~ 15 yr period

## 1b. ----
parks <- readRDS("~outputs/10/14_parks.rds")
parks_gunCount_list <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

## All cities ----
for (city in seq_len(length(parks_gunCount_list))) {
  
  name <- names(parks_gunCount_list)[city]
  print(name)
  
  tmp <- guns_proj_list[[name]]
  
  parks_gunCount_list[[name]] <- parks[[name]] %>% 
    st_transform(proj_list[[name]]) %>% 
    dplyr::select(osm_id, name, leisure, geometry) %>% 
    mutate(gun_count = lengths(st_intersects(.,
                                             tmp)))
  
}

alcohol_Parks_summary <- vector("list", length(guns_list_shp)) %>% 
  set_names(names(guns_list_shp))

for (city in seq_len(length(alcohol_Parks_summary))){
  
  name <- names(alcohol_Parks_summary)[city]
  
  gun_count <- guns_list_shp[[name]] %>% nrow()
  alcohol_outlet_count <- alcohol_buffer100ft_list[[name]] %>% nrow()
  alcohol_gunSum <- sum(alcohol_buffer100ft_list[[name]]$gun_count)
  alcohol_gunPerc <- alcohol_gunSum / gun_count
  crimes_per_alcoholOutlet <- alcohol_gunSum / alcohol_outlet_count
  alcohol_allowed <- city_laws_list[[name]]$`Bars Allowed`
  
  
  parks_allowed <- city_laws_list[[name]]$`Parks Allowed`
  parks_gunSum <- sum(parks_gunCount_list[[name]]$gun_count)
  parks_gunPerc <- parks_gunSum / gun_count
  parks_mileage <- parks_gunCount_list[[name]] %>% 
    st_union() %>%  
    st_area() %>%
    sum %>%
    as.numeric() %>%  
    conv_unit(from = "ft2", to = "mi2")
  parks_gun_perSqMi <- parks_gunSum / parks_mileage
  city_mileage <- city_bounds[[name]] %>% 
    st_transform(proj_list[[name]]) %>% 
    st_area() %>% 
    as.numeric %>% # 
    conv_unit(from = "ft2", to = "mi2")
  
  park_percentage <- parks_mileage / city_mileage
  
  
  alcohol_Parks_summary[[name]] <- data.frame(
    totalCrimes = gun_count,
    alcohol_allowed = alcohol_allowed,
    alcohol_outlet_count = alcohol_outlet_count,
    alcohol_crime_count = alcohol_gunSum,
    alcohol_crime_percentage = round(alcohol_gunPerc, 5),
    crimes_per_alcohol = crimes_per_alcoholOutlet,
    
    parks_allowed = parks_allowed,
    park_mileage = parks_mileage,
    park_crime_count = parks_gunSum,
    park_crime_percentage = parks_gunPerc,
    park_crimes_perSqMi = parks_gun_perSqMi,
    
    stringsAsFactors = FALSE)
  
}


# Philadelphia ----
parks_gunCount_list$Philadelphia <- parks$Philadelphia %>% 
  dplyr::select(osm_id, name, leisure, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$Philadelphia)))


# no guns allowed in Philly parks
guns_list_shp$Philadelphia$clean_report_date %>% summary() # gun crimes from Jan 2006 to April 2020
phl_gunSum <- guns_list_shp$Philadelphia %>% nrow() # 258,000 total gun crimes during that time
phl_parks_gunSum <- sum(parks_gunCount_list$Philadelphia$gun_count) # 1,000 gun crimes in philly parks
phl_parks_gunPerc <- phl_parks_gunSum / phl_gunSum # which is 0.4% of all gun crimes
phl_parks_totalArea <- parks_gunCount_list$Philadelphia %>% # 16.3 sq miles of parks in Philly
  st_union() %>%  
  st_area() %>%
  sum %>%
  as.numeric() %>%  
  conv_unit(from = "ft2", to = "mi2")

phl_parks_gunSum / phl_parks_totalArea # So 63 gun crimes per square mile in parks during that ~ 15 yr period

phl_totalArea <- bbox_sfs$Philadelphia %>% st_transform(proj_list$Philadelphia) %>% st_area() %>% as.numeric %>% # Overall, Philly is 143 sq miles
  conv_unit(from = "ft2", to = "mi2")
phl_parks_perc <- phl_parks_totalArea / phl_totalArea # So parks are 11.4% of the city 
phl_gunSum / phl_totalArea # Overall, there are 1,811 gun crimes per square mile in that ~ 15 yr period

# San Francisco  ----
parks_gunCount_list$`San Francisco` <- parks$`San Francisco` %>% 
  dplyr::select(osm_id, name, leisure, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$`San Francisco`)))

# no guns allowed in SF parks
guns_list_shp$`San Francisco`$clean_occur_date %>% summary() # Gun crimes from Jan 2003 to April 2020
sf_gunSum <- guns_list_shp$`San Francisco` %>% nrow() # 72,500 total crimes 
sf_parks_gunSum <- sum(parks_gunCount_list$`San Francisco`$gun_count) # 915 occurred in parks
sf_parks_gunPerc <- sf_parks_gunSum / sf_gunSum # or roughly 1.3% of all gun crimes
sf_parks_totalArea <- parks_gunCount_list$`San Francisco` %>% # 8.4 sq miles of parks in San Francisco
  st_union() %>%  
  st_area() %>%
  sum %>%
  as.numeric() %>%  
  conv_unit(from = "ft2", to = "mi2")

sf_parks_gunSum / sf_parks_totalArea # So 109 gun crimes per square mile in parks during the ~ 18 year period

sf_totalArea <- bbox_sfs$`San Francisco` %>% st_transform(proj_list$`San Francisco`) %>% st_area() %>% sum %>% as.numeric %>% # Overall, SF is 49 sq miles
  conv_unit(from = "ft2", to = "mi2")
sf_parks_perc <- sf_parks_totalArea / sf_totalArea # So parks are 17.2% of the city
sf_gunSum / sf_totalArea # Overall, there are 1,488 gun crimes per square mile in that ~ 18 year period


# Louisville ----
parks_gunCount_list$Louisville <- parks$Louisville %>% 
  dplyr::select(osm_id, name, leisure, geometry) %>% 
  mutate(gun_count = lengths(st_intersects(.,
                                           guns_proj_list$Louisville)))

# guns are allowed in SF parks
guns_list_shp$Louisville$clean_occur_date %>% summary() # Gun crimes from Feb 2003 to April 2020
lou_gunSum <- guns_list_shp$Louisville %>% nrow() # 29,000 total crimes 
lou_parks_gunSum <- sum(parks_gunCount_list$Louisville$gun_count) # 110 occurred in parks
lou_parks_gunPerc <- lou_parks_gunSum / lou_gunSum # or roughly 0.4% of all gun crimes
lou_parks_totalArea <- parks_gunCount_list$Louisville %>% # 3.2 sq miles of parks in San Francisco
  st_union() %>%  
  st_area() %>%
  sum %>%
  as.numeric() %>%  
  conv_unit(from = "ft2", to = "mi2")

lou_parks_gunSum / lou_parks_totalArea # So 35 gun crimes per square mile in parks during the ~ 18 year period

lou_totalArea <- bbox_sfs$Louisville %>% st_transform(proj_list$Louisville) %>% st_area() %>% sum %>% as.numeric %>% # Overall, Louisville is 66 sq miles
  conv_unit(from = "ft2", to = "mi2")
lou_parks_perc <- lou_parks_totalArea / lou_totalArea # So parks are 4.7% of the city
lou_gunSum / lou_totalArea # Overall, there are 441 gun crimes per square mile in that ~ 18 year period

## 2. ----

## 1a. Export as rds ----
# saveRDS(alcohol_buffer100ft_list,
#         "~outputs/30/37_alcohol_buffer100ft_list.rds")

## 1b. Export as rds ----
# saveRDS(parks_gunCount_list,
#         "~outputs/30/37_parks_gunCount_list.rds")
