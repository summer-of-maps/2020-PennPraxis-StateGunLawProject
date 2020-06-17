##########################################################################
# This script:
# 1. Loads packages 
# 2.
#   (a) Sets the file path for the synced Dropbox data folder
#   (b) Sets Census API key
# 3. Turns off scientific notation
# 4. Sets visualization aesthetics
#
# Exports:
#
# To-do:
# 1. Consider putting aesthetics in separate script and sourcing here
##########################################################################

## 1. ----

# Data reading and wrangling
library(tidyverse)
library(vroom) # read large csvs
library(anytime) # datetime manipulation
library(furrr) # parallel processing
library(readxl)
library(data.table)

# spatial data
library(sf)
library(tmap) # thematic mapping
library(tmaptools) # spatial utility functions

# census
library(tidycensus)

# visualization
library(ggmap) # basemaps

# debugging
library(rbenchmark) # time processing speed

## 2. ----
# (a)
data_dir <- "C:/Users/echong/Dropbox/SGLP_Azavea"

# (b)
census_api_key("3ef31f05bc4961d45eaa1d3e4787a9b4be486b9f")

## 3. ----
options(scipen=999)

## 4. ----
plotTheme <- function(){
  theme_bw()
}
