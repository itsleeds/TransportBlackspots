# Investigate weeks
library(UK2GTFS)
library(tmap)
tmap_mode("view")
source("R/stops_per_week_functions.R")
base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive/GTFS/2014 Oct/"

gtfs_single <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive/GTFS/2014 Oct/EA 20141007.zip")
gtfs_merged <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive/GTFS/2014_merged.zip")

count_single <- gtfs_stop_frequency(gtfs_single,
                                    startdate = lubridate::ymd("20141007"),
                                    enddate = lubridate::ymd("20141013"))

count_merged <- gtfs_stop_frequency(gtfs_merged,
                                    startdate = lubridate::ymd("20141007"),
                                    enddate = lubridate::ymd("20141013"))

count_merged = count_merged[count_merged$stop_id %in% count_single$stop_id,]
count_single = count_single[count_single$stop_id %in% count_merged$stop_id,]

count_merged = count_merged[order(count_merged$stop_id), ]
count_single = count_single[order(count_single$stop_id), ]

summary(count_merged$stop_id == count_single$stop_id)
summary(count_merged$stops_per_week == count_single$stops_per_week)

count_merged$stops_per_week_single = count_single$stops_per_week
qtm(count_merged)

foo = count_merged[count_merged$stops_per_week != count_single$stops_per_week, ]
stops = gtfs_stops_sf(gtfs_single)
stops = stops[stops$stop_id %in% foo$stop_id,]
foo = foo[,c("stop_id","stops_per_week","stops_per_week_single")]

stops = left_join(stops, foo)
qtm(stops)

example_stop = "0590PSS249"

gtfs_single_trips = unique(gtfs_single$stop_times$trip_id[gtfs_single$stop_times$stop_id == example_stop])
gtfs_merged_trips = unique(gtfs_merged$stop_times$trip_id[gtfs_merged$stop_times$stop_id == example_stop])

gtfs_single_example = gtfs_split_ids(gtfs_single, gtfs_single_trips)$true
gtfs_merged_example = gtfs_split_ids(gtfs_merged, gtfs_merged_trips)$true

trips_single = gtfs_trips_sf(gtfs_single_example)
trips_merged = gtfs_trips_sf(gtfs_merged_example)

trips_single = trips_single[!duplicated(trips_single$geometry),]
trips_merged = trips_merged[!duplicated(trips_merged$geometry),]

qtm(trips_merged) + qtm(trips_single, lines.col = "red")


count_merged_long <- gtfs_stop_frequency(gtfs_merged,
                                    startdate = lubridate::ymd("20141007"),
                                    enddate = lubridate::ymd("20141105"))

count_merged_p2 <- gtfs_stop_frequency(gtfs_merged,
                                         startdate = lubridate::ymd("20141014"),
                                         enddate = lubridate::ymd("20141020"))
count_merged_p3 <- gtfs_stop_frequency(gtfs_merged,
                                       startdate = lubridate::ymd("20141021"),
                                       enddate = lubridate::ymd("20141027"))
count_merged_p4 <- gtfs_stop_frequency(gtfs_merged,
                                       startdate = lubridate::ymd("20141028"),
                                       enddate = lubridate::ymd("20141105"))

summary(count_merged$stops_per_week)
summary(count_merged_p2$stops_per_week)
summary(count_merged_p3$stops_per_week)
summary(count_merged_p4$stops_per_week)
summary(count_merged_long$stops_per_week)
