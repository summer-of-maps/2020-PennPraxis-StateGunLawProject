##########################################################################
# This script:
# 1. Sets the file path for the synced Dropbox data folder
# 2. Turns off scientific notation
# 3. Loads packages
# 4. Sets visualization aesthetics
#
# Exports:
#
# To-do:
# 1. Consider putting aesthetics in separate script and sourcing here
##########################################################################

## 1. ----
data_dir <- "C:/Users/echong/Dropbox/SGLP_Azavea"

## 2. ----

options(scipen=999)

## 3. ----

# os tools


# Data reading and wrangling
library(tidyverse)
library(vroom)
library(anytime)
library(furrr) # parallel processing

# spatial data
library(sf)

# visualization

# debugging
library(rbenchmark)

## 4. ----
plotTheme <- function(){
  theme_bw()
}
