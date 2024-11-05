if(clear_all) {
  rm_list <- ls()[ls() != "reload"]
  rm(list = rm_list)
} else {
  env_list_no_onspd <- ls()[ls() != "onspd" & ls() != "reload"]
  rm(list = env_list_no_onspd)
  rm(env_list_no_onspd)
}

# load packages
load_packages <- function() {
  #library(UK2GTFS)
  library(tidyverse)
  #library(lubridate)
  library(tmap)
  #library(future.apply)
  library(sf)
  library(tictoc)
  #library(pracma)
  library(openxlsx)
  #library(fpp2)
  #library(zoo)
  #library(tsoutliers)
}

load_packages()

#' source required function building scripts.
source("scripts/november-24/process-lsoa-data.R")

source("scripts/november-24/onspd.R")

options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = TRUE)
