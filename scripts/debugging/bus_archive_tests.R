# Investigate weeks
library(UK2GTFS)
library(tmap)
tmap_mode("view")
base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive/GTFS/2014 Oct/"

gtfs1 <- gtfs_read(file.path(base_path,"EA 20141007.zip"))
gtfs2 <- gtfs_read(file.path(base_path,"EA 20141014.zip"))
gtfs3 <- gtfs_read(file.path(base_path,"EA 20141021.zip"))
gtfs4 <- gtfs_read(file.path(base_path,"EA 20141028.zip"))

summary(gtfs1$calendar$start_date)
summary(gtfs1$calendar$end_date)

summary(gtfs2$calendar$start_date)
summary(gtfs2$calendar$end_date)

head(gtfs1$stop_times)
head(gtfs2$stop_times)

stops1 <- gtfs_stops_sf(gtfs1)
qtm(stops1)

test_stop = "0500SGREA004"
test_trips1 = unique(gtfs1$stop_times$trip_id[gtfs1$stop_times$stop_id == test_stop])
gtfs1_test = gtfs_split_ids(gtfs1, test_trips1)
gtfs1_test = gtfs1_test$true

test_trips2 = unique(gtfs2$stop_times$trip_id[gtfs2$stop_times$stop_id == test_stop])
gtfs2_test = gtfs_split_ids(gtfs2, test_trips2)
gtfs2_test = gtfs2_test$true

trips1 <- gtfs_trips_sf(gtfs1_test)
trips2 <- gtfs_trips_sf(gtfs2_test)
qtm(trips1, lines.lwd = 2, lines.col = "red") +
  qtm(trips2, lines.lwd = 2, lines.col = "black")

summary(gtfs1_test$calendar$start_date)
summary(gtfs1_test$calendar$end_date)

summary(gtfs2_test$calendar$start_date)
summary(gtfs2_test$calendar$end_date)

identical(gtfs1_test$routes[2:6], gtfs2_test$routes[2:6])

