library(UK2GTFS)

gtfs_bus = gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS/20191008_merged.zip")
gtfs_rail = gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/ATOC/GTFS/2019-08-31.zip")

summary(gtfs_bus$calendar$start_date)
summary(gtfs_rail$calendar$start_date)
