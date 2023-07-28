if(clear_all) {
  rm(list = ls())
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

#' TODO: source required function building scripts.

#source("")
#source("")
