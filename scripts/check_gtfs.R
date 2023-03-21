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
bounds = atco_areas
bounds = bounds[5:147,]
bounds = st_simplify(bounds, dTolerance = 0.001)
bounds = st_union(bounds)

make_plot = function(x, yr, bounds){
  x <- x[order(x$stops_per_week),]
  breaks = c(-10000,0,1,50,100,200,400,1000,5000,10000,200000)
  pal = c("#762a83","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4")
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
                  labels = c(paste0(breaks[1:10]," to ",
                                    format(breaks[2:11],
                                           scientific = FALSE,
                                           trim = TRUE)), "Missing"),
                  title = "Stops per week") +
    tm_layout(frame = FALSE)
  tmap_save(m1, paste0("plots/checks/stops_per_week_",yr,".png"), dpi = 600)
}


#for(i in c(2004:2011,2014:2019)){
for(i in c(2015:2019)){
  message(i)
  if(i < 2012){
    gtfs <- gtfs_read(file.path(path,paste0("NPTDR/GTFS/NPTDR_",i,".zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-01")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))
  } else if (i == 2014) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-07")),
                                enddate = lubridate::ymd(paste(i,"-11-03")))
  } else if (i == 2015) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-06")),
                                enddate = lubridate::ymd(paste(i,"-11-02")))
  } else if (i == 2016) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-04")),
                                enddate = lubridate::ymd(paste(i,"-10-31")))
  } else if (i == 2017) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-03")),
                                enddate = lubridate::ymd(paste(i,"-10-30")))
  } else if (i == 2018) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-02")),
                                enddate = lubridate::ymd(paste(i,"-10-29")))
  } else if (i == 2019) {
    gtfs <- gtfs_read(file.path(path,paste0("Bus Archive/GTFS/",i,"_merged.zip")))
    gtfs <- gtfs_stop_frequency(gtfs,
                                startdate = lubridate::ymd(paste(i,"-10-01")),
                                enddate = lubridate::ymd(paste(i,"-10-28")))
  } else {
    stop()
  }
  gtfs <- gtfs[!is.na(gtfs$stop_lon),]
  gtfs <- st_as_sf(gtfs, coords = c("stop_lon", "stop_lat"), crs = 4326)

  make_plot(gtfs, i, bounds)
  rm(gtfs)

}




