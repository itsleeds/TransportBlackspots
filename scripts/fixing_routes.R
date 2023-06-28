stops = gtfs_stops_sf(gtfs)
library(tmap)
tmap_mode("view")
qtm(stops)

# 0100BRA16909
# The Centre (HN)
# bstpagd

foo = gtfs$stop_times[gtfs$stop_times$stop_id == "0100BRA16909",]

st_write(stops,"stops_temp.gpkg")

naptan_replace[naptan_replace$stop_id == "5610ANZ12812",]

stops_fixed = st_read("stops_temp.gpkg")

stops_fixed = stops_fixed[stops_fixed$geom != stops$geometry,]


fast_trips = gtfs_fast_trips(gtfs)

gtfs_fast =  gtfs_split_ids(gtfs, fast_trips$trip_id)
gtfs_fast = gtfs_fast$true

fast_lines = gtfs_trips_sf(gtfs_fast)
fast_lines = fast_lines[st_is_valid(fast_lines),]
qtm(fast_lines)


foo = times[times$min_distance > 100000,]
st_write(foo,"stops_temp2.gpkg", append = FALSE)

stops_fixed = st_read("stops_temp2.gpkg")
stops_fixed = stops_fixed[names(stops_fixed) %in% names(naptan_replace)]
head(stops_fixed)

stops_fixed = cbind(st_drop_geometry(stops_fixed), st_coordinates(stops_fixed))
names(stops_fixed) = names(naptan_replace)

naptan_replace = rbind(naptan_replace, stops_fixed)

saveRDS(naptan_replace,"naptan_replace.Rds")

