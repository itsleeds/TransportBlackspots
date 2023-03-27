# Check GTFS
# Some basic checks on the historical GTFS data
library(UK2GTFS)
library(dplyr)
library(sf)
library(tmap)
tmap_mode("plot")
path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/"
source("R/stops_per_week_functions.R")

sf_use_s2(FALSE)
bounds = st_read("data/UK_bounds.geojson")

make_plot = function(x, yr, bounds, title = ""){
  x <- x[order(x$stops_per_week),]
  breaks = c(0,1,50,100,200,400,1000,5000,10000,200000)
  pal = c("#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4")
  m1 = tm_shape(bounds) +
    tm_fill("#656565") +
    tm_shape(x) +
    tm_dots(size = 0.001,
            shape = 15,
            col = "stops_per_week",
            breaks = breaks,
            title = yr,
            palette = pal,
            style = "fixed",
            colorNA = "#000000",
            legend.show = FALSE) +
    tm_add_legend(type = "fill",
                  col = c(pal,"#000000"),
                  labels = c(paste0(breaks[1:9]," to ",
                                    format(breaks[2:10],
                                           scientific = FALSE,
                                           trim = TRUE)), "Missing"),
                  title = "Stops per week") +
    tm_layout(frame = FALSE, title = title)
  tmap_save(m1, paste0("plots/checks/stops_per_week_",yr,".png"), dpi = 600)
}


for(i in c(2004:2011,2014:2022)){
#for(i in c(2020:2022)){
  message(i)
  if(i < 2012){
    gtfs <- gtfs_read(file.path(path,paste0("NPTDR/GTFS/NPTDR_",i,".zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-01")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))
    title = paste0("NPTDR from ",paste(i,"-10-01")," to ",paste(i,"-10-31"))
  } else if (i == 2014) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-07")),
                                enddate = lubridate::ymd(paste(i,"-11-03")))
    title = paste0("Bus Archive from ",paste(i,"-10-07")," to ",paste(i,"-11-03"))
  } else if (i == 2015) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-06")),
                                enddate = lubridate::ymd(paste(i,"-11-02")))
    title = paste0("Bus Archive from ",paste(i,"-10-06")," to ",paste(i,"-11-02"))
  } else if (i == 2016) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-04")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))
    title = paste0("Bus Archive from ",paste(i,"-10-04")," to ",paste(i,"-10-31"))
  } else if (i == 2017) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-03")),
                                enddate = lubridate::ymd(paste(i,"-10-30")))
    title = paste0("Bus Archive from ",paste(i,"-10-03")," to ",paste(i,"-10-30"))
  } else if (i == 2018) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0515_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-05-15")),
                                enddate = lubridate::ymd(paste(i,"-06-15")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-16.zip")))
    gtfs_rail <- gtfs_stop_frequency(gtfs_rail,
                                startdate = lubridate::ymd(paste(i,"-10-16")),
                                enddate = lubridate::ymd(paste(i,"-11-16")))
    gtfs = rbind(gtfs, gtfs_rail)
    rm(gtfs_rail)
    title = paste0("Traveline from ",paste(i,"-05-15")," to ",paste(i,"-06-15"),
                   " and ATOC from ",paste(i,"-10-16")," to ",paste(i,"-11-16"))
  } else if (i == 2019) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1008_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-08")),
                                enddate = lubridate::ymd(paste(i,"-11-08")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-08-31.zip")))
    gtfs_rail <- gtfs_stop_frequency(gtfs_rail,
                                     startdate = lubridate::ymd(paste(i,"-08-31")),
                                     enddate = lubridate::ymd(paste(i,"-09-30")))
    gtfs = rbind(gtfs, gtfs_rail)
    rm(gtfs_rail)
    title = paste0("Traveline from ",paste(i,"-10-08")," to ",paste(i,"-11-08"),
                   " and ATOC from ",paste(i,"-08-31")," to ",paste(i,"-09-30"))
  } else if (i == 2020) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"0701_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-07-01")),
                                enddate = lubridate::ymd(paste(i,"-07-07")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-26.zip")))
    gtfs_rail <- gtfs_stop_frequency(gtfs_rail,
                                     startdate = lubridate::ymd(paste(i,"-11-26")),
                                     enddate = lubridate::ymd(paste(i,"-12-26")))
    gtfs = rbind(gtfs, gtfs_rail)
    rm(gtfs_rail)
    title = paste0("Traveline from ",paste(i,"-07-01")," to ",paste(i,"-07-07"),
                   " and ATOC from ",paste(i,"-11-26")," to ",paste(i,"-12-26"))
  }else if (i == 2021) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1012_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-12")),
                                enddate = lubridate::ymd(paste(i,"-11-12")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-10-09.zip")))
    gtfs_rail <- gtfs_stop_frequency(gtfs_rail,
                                     startdate = lubridate::ymd(paste(i,"-10-09")),
                                     enddate = lubridate::ymd(paste(i,"-11-09")))
    gtfs = rbind(gtfs, gtfs_rail)
    rm(gtfs_rail)
    title = paste0("Traveline from ",paste(i,"-10-12")," to ",paste(i,"-11-12"),
                   " and ATOC from ",paste(i,"-10-09")," to ",paste(i,"-11-09"))
  }else if (i == 2022) {
    gtfs <- gtfs_read(file.path(path,paste0("TransXChange/GTFS/",i,"1102_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-11-02")),
                                enddate = lubridate::ymd(paste(i,"-12-03")))
    gtfs_rail <- gtfs_read(file.path(path,paste0("ATOC/GTFS/",i,"-11-02.zip")))
    gtfs_rail <- gtfs_stop_frequency(gtfs_rail,
                                     startdate = lubridate::ymd(paste(i,"-11-02")),
                                     enddate = lubridate::ymd(paste(i,"-12-02")))
    gtfs = rbind(gtfs, gtfs_rail)
    rm(gtfs_rail)
    title = paste0("Traveline from ",paste(i,"-11-02")," to ",paste(i,"-12-03"),
                   " and ATOC from ",paste(i,"-11-02")," to ",paste(i,"-12-02"))
  } else {
    stop()
  }
  gtfs <- gtfs[!is.na(gtfs$stop_lon),]
  gtfs <- st_as_sf(gtfs, coords = c("stop_lon", "stop_lat"), crs = 4326)

  saveRDS(gtfs,paste0("data/stops_per_week_",i,".Rds"))

  make_plot(gtfs, i, bounds, title)
  rm(gtfs)

}




