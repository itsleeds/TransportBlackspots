if(clear_all) {
  rm(list = ls())
} else {
  env_list_no_onspd <- ls()[ls() != "onspd"]
  rm(list = env_list_no_onspd)
  rm(env_list_no_onspd)
}

# load packages
load_packages <- function() {
  library(UK2GTFS)
  library(tidyverse)
  library(lubridate)
  library(tmap)
  library(future.apply)
  library(sf)
  library(tictoc)
  library(pracma)
  library(openxlsx)
  library(fpp2)
  #library(zoo)
  library(tsoutliers)
}

load_packages()

#' source required function building scripts.
source("scripts/toby-analysis/final/process-lsoa-data.R")
source("scripts/toby-analysis/final/process-la-data.R")
source("scripts/toby-analysis/final/clean-data.R")
source("scripts/toby-analysis/final/summarise-data.R")

source("scripts/toby-analysis/final/onspd.R")

options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = TRUE)
