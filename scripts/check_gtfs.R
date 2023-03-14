# Check GTFS
# Some basic checks on the hisotrical GTFS data
library(UK2GTFS)
library(dplyr)
library(sf)
library(tmap)
tmap_mode("plot")
path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/GTFS/"

# Read in GTFS
gtfs_2004 <- gtfs_read(file.path(path,"NPTDR_2004.zip"))
gtfs_2005 <- gtfs_read(file.path(path,"NPTDR_2005.zip"))
gtfs_2006 <- gtfs_read(file.path(path,"NPTDR_2006.zip"))
gtfs_2007 <- gtfs_read(file.path(path,"NPTDR_2007.zip"))
gtfs_2008 <- gtfs_read(file.path(path,"NPTDR_2008.zip"))
gtfs_2009 <- gtfs_read(file.path(path,"NPTDR_2009.zip"))
gtfs_2010 <- gtfs_read(file.path(path,"NPTDR_2010.zip"))

# Measure Service
stops_2004_count = count_stops(gtfs_2004,
                               startdate = lubridate::ymd("2006-10-01"),
                               enddate = lubridate::ymd("2006-10-31"))
stops_2005_count = count_stops(gtfs_2005,
                               startdate = lubridate::ymd("2005-10-01"),
                               enddate = lubridate::ymd("2005-10-31"))
stops_2006_count = count_stops(gtfs_2006,
                               startdate = lubridate::ymd("2006-10-01"),
                               enddate = lubridate::ymd("2006-10-31"))
stops_2007_count = count_stops(gtfs_2007,
                               startdate = lubridate::ymd("2007-10-01"),
                               enddate = lubridate::ymd("2007-10-31"))
stops_2008_count = count_stops(gtfs_2008,
                               startdate = lubridate::ymd("2008-10-01"),
                               enddate = lubridate::ymd("2008-10-31"))
stops_2009_count = count_stops(gtfs_2009,
                               startdate = lubridate::ymd("2009-10-01"),
                               enddate = lubridate::ymd("2009-10-31"))
stops_2010_count = count_stops(gtfs_2010,
                               startdate = lubridate::ymd("2010-10-01"),
                               enddate = lubridate::ymd("2010-10-31"))


# Convert to sf
stops_2004_count <- st_as_sf(stops_2004_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2005_count <- st_as_sf(stops_2005_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2006_count <- st_as_sf(stops_2006_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2007_count <- st_as_sf(stops_2007_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2008_count <- st_as_sf(stops_2008_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2009_count <- st_as_sf(stops_2009_count, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_2010_count <- st_as_sf(stops_2010_count, coords = c("stop_lon", "stop_lat"), crs = 4326)



# Make plots

bbox = c(-9.887695,49.837982,2.526855,61.480760)

make_plot = function(x, yr, bbox){
  breaks = c(-10000,0,1,50,100,200,400,1000,5000,10000,200000)
  pal = c("#e31a1c","#ffffe5","#f7fcb9","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#006837","#004529")
  m1 = tm_shape(x, bbox = bbox) +
    tm_dots(col = "stops_per_week", breaks = breaks, title = yr, palette = pal, style = "fixed")
  tmap_save(m1, paste0("plots/checks/stops_per_week_",yr,".png"))
}


make_plot(stops_2004_count, 2004, bbox)
make_plot(stops_2005_count, 2005, bbox)
make_plot(stops_2006_count, 2006, bbox)
make_plot(stops_2007_count, 2007, bbox)
make_plot(stops_2008_count, 2008, bbox)
make_plot(stops_2009_count, 2009, bbox)
make_plot(stops_2010_count, 2010, bbox)
make_plot(stops_2011_count, 2011, bbox)




