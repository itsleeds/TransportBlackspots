remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive/GTFS"
source("R/stops_per_week_functions.R")


for(yr in 2015:2019){
  #yr = 2014
  message(yr)
  files = list.files(file.path(base_path,paste0(yr," Oct")))

  gtfs_all <- list()

  for(i in 1:length(files)){
    message(files[i])
    gtfs <- gtfs_read(file.path(base_path,paste0(yr," Oct"),files[i]))

    wks <- gsub(".zip","",files[i])
    wks <- strsplit(wks," ")
    wks <- sapply(wks, function(x){x[length(x)]})
    wks <- lubridate::ymd(wks)

    date_start <- wks
    date_end <- wks + 6

    gtfs <- gtfs_trim_dates(gtfs, startdate = date_start, enddate = date_end)
    gtfs_all[[i]] <- gtfs

  }

  gtfs_merged <- gtfs_merge(gtfs_all, force = TRUE)
  gtfs_merged <- gtfs_clean(gtfs_merged)

  gtfs_write(gtfs_merged,file.path(base_path), name = paste0(yr,"_merged"))
  rm(gtfs_all, gtfs, gtfs_merged)
}



