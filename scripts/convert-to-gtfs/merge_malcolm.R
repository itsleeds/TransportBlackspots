#remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)
source("R/stops_per_week_functions.R")
base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS"

yrs = list.dirs(base_path)
yrs = yrs[yrs != base_path]
yrs = yrs[!grepl("v1",yrs)]
#yrs = yrs[!grepl("20201102",yrs)]
#yrs = yrs[6]


for(j in seq_along(yrs)){

  fls = list.files(yrs[j], full.names = TRUE)

  wks <- substr(yrs[j],nchar(yrs[j])-7,nchar(yrs[j]))
  date_start <- lubridate::ymd(wks) - 31
  date_end <- date_start + 62

  gtfs_all <- list()

  for(i in 1:length(fls)){
    message(fls[i])
    gtfs <- gtfs_read(fls[i])
    gtfs <- gtfs_trim_dates(gtfs, startdate = date_start, enddate = date_end)
    gtfs_all[[i]] <- gtfs
  }

  gtfs_all = gtfs_merge(gtfs_all, force = TRUE)
  gtfs_write(gtfs_all,
             "D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS/",
             name = paste0(wks,"_merged"))


}
