
library(UK2GTFS)
library(sf)
sf_use_s2(FALSE)

#source("../UK2GTFS/R/gtfs_to_sf.R")
#path = "C:/Users/malco/OneDrive - University of Leeds/Data/UK2GTFS"
path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/"
#for(i in c(2019:2023)){
for(i in c(2004:2011,2014:2023)){
  message(i)
  if(i < 2012){
    gtfs <- gtfs_read(file.path(path,paste0("NPTDR/GTFS/NPTDR_",i,".zip")))
  } else if (i >= 2014 & i <= 2017) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
  } else if (i == 2018) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0515_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-16.zip")))
  } else if (i == 2019) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1008_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-08-31.zip")))
  } else if (i == 2020) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0701_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-26.zip")))
  }else if (i == 2021) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1012_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-09.zip")))
  }else if (i == 2022) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1102_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-02.zip")))
  }else if (i == 2023) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0503_merged.zip")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-05-03.zip")))
  } else {
    stop()
  }

  routes <- gtfs_routes_sf(gtfs)

  if(exists("gtfs_rail")){
    routes_rail <- gtfs_routes_sf(gtfs_rail)
    routes$route_desc <- NULL
    routes <- rbind(routes,routes_rail)
  }

  sf::st_write(routes, paste0("data/routes_",i,".geojson"), delete_dsn = TRUE)

  rm(gtfs, gtfs_rail, routes, routes_rail)
  gc()
}
