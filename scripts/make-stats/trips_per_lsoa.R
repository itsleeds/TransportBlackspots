library(UK2GTFS)
library(sf)
sf_use_s2(FALSE)

lamode = FALSE

source("R/stops_per_week_functions.R")
if(lamode){
  zone = readRDS("data/LA_bounds_2023.Rds")
} else {
  zone = readRDS("data/GB_LSOA_2011_full_or_500mBuff.Rds")
}
zone = st_transform(zone, 4326)
#path = "C:/Users/malco/OneDrive - University of Leeds/Data/UK2GTFS"
path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/"
#for(i in c(2004:2011,2014:2023)){
for(i in c(2014:2017)){
  message(i)
  if(i < 2012){
    gtfs <- gtfs_read(file.path(path,paste0("NPTDR/GTFS/NPTDR_",i,".zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-01")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))

  } else if (i == 2014) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-07")),
                                enddate = lubridate::ymd(paste(i,"-11-03")))

  } else if (i == 2015) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-06")),
                                enddate = lubridate::ymd(paste(i,"-11-02")))

  } else if (i == 2016) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-04")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))
  } else if (i == 2017) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-03")),
                                enddate = lubridate::ymd(paste(i,"-10-30")))

  } else if (i == 2018) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0515_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-05-15")),
                                enddate = lubridate::ymd(paste(i,"-06-15")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-16.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-10-16")),
                                     enddate = lubridate::ymd(paste(i,"-11-16")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)

    rm(gtfs_rail)
  } else if (i == 2019) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1008_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-08")),
                                enddate = lubridate::ymd(paste(i,"-11-08")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-08-31.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-08-31")),
                                     enddate = lubridate::ymd(paste(i,"-09-30")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)
    rm(gtfs_rail)
  } else if (i == 2020) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0701_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-07-01")),
                                enddate = lubridate::ymd(paste(i,"-07-31")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-26.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-11-26")),
                                     enddate = lubridate::ymd(paste(i,"-12-26")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)
    rm(gtfs_rail)
  }else if (i == 2021) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1012_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-10-12")),
                                enddate = lubridate::ymd(paste(i,"-11-12")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-09.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-10-09")),
                                     enddate = lubridate::ymd(paste(i,"-11-09")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)
    rm(gtfs_rail)
  }else if (i == 2022) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1102_merged.zip")))
    gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
    gtfs = gtfs_clean(gtfs)
    gtfs <- gtfs_trips_per_zone(gtfs, zone = zone,
                                startdate = lubridate::ymd(paste(i,"-11-02")),
                                enddate = lubridate::ymd(paste(i,"-12-03")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-02.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-11-02")),
                                     enddate = lubridate::ymd(paste(i,"-12-02")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)
    rm(gtfs_rail)
  }else if (i == 2023) {

    summ_after <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0503_merged.zip")))
    summ_after$stops <- summ_after$stops[!is.na(summ_after$stops$stop_lon),]
    summ_after = gtfs_clean(summ_after)
    dt = lubridate::ymd("2023-05-01")
    summ_after <- gtfs_trips_per_zone(summ_after, zone = zone,
                                startdate = dt,
                                enddate = dt + 31)


    summ_before <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1101_merged.zip")))
    summ_before$stops <- summ_before$stops[!is.na(summ_before$stops$stop_lon),]
    summ_before = gtfs_clean(summ_before)
    dt = lubridate::ymd("2023-11-01")
    summ_before <- gtfs_trips_per_zone(summ_before, zone = zone,
                                      startdate = dt,
                                      enddate = dt + 31)

    names(summ_before) = paste0(names(summ_before),"_b4")

    summ_both <- dplyr::full_join(summ_after,summ_before, by = c("zone_id" = "zone_id_b4","route_type" ="route_type_b4"))
    nms = names(summ_after)[3:ncol(summ_after)]
    for(j in 1:length(nms)){
      message(nms[j])
      sub = pmax(summ_both[nms[j]],summ_both[paste0(nms[j],"_b4")], na.rm = TRUE)
      summary(sub > summ_both[nms[j]])
      summ_both[nms[j]] = sub
    }
    gtfs = summ_both[names(summ_after)]

    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-05-03.zip")))
    gtfs_rail$stops <- gtfs_rail$stops[!is.na(gtfs_rail$stops$stop_lon),]
    gtfs_rail = gtfs_clean(gtfs_rail)
    gtfs_rail <- gtfs_trips_per_zone(gtfs_rail, zone = zone,
                                     startdate = lubridate::ymd(paste(i,"-05-01")),
                                     enddate = lubridate::ymd(paste(i,"-06-01")))
    gtfs = rbind(gtfs, gtfs_rail)
    gtfs = dplyr::group_by(gtfs, zone_id, route_type)
    gtfs = dplyr::summarise_all(gtfs, sum, na.rm = TRUE)
    rm(gtfs_rail)
  } else {
    stop()
  }

  if(lamode){
    saveRDS(gtfs,paste0("data/trips_per_la_by_mode_",i,".Rds"))
  } else {
    saveRDS(gtfs,paste0("data/trips_per_lsoa_by_mode_",i,".Rds"))
  }

  rm(gtfs)
  gc()
}
