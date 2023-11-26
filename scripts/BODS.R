library(UK2GTFS)
library(tidyr)
source("R/stops_per_week_functions.R")
# path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/OpenBusData/GTFS/20231101/itm_all_gtfs.zip"
#
zone = readRDS("data/LA_bounds_2023.Rds")
#
# gtfs <- gtfs_read(path)
# gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
# gtfs = gtfs_clean(gtfs)
# summ <- gtfs_trips_per_zone(gtfs, zone = zone, by_mode = TRUE,
#                             startdate = lubridate::ymd(paste(2023,"-11-01")),
#                             enddate = lubridate::ymd(paste(2023,"-12-01")))
#
#

gtfs_old <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS_oldversion/GTFS_20230503/NE.zip")
gtfs_old$stops <- gtfs_old$stops[!is.na(gtfs_old$stops$stop_lon),]
gtfs_old = gtfs_clean(gtfs_old)
old_raw <- gtfs_trips_per_zone(gtfs_old, zone = zone, by_mode = TRUE,
                           startdate = lubridate::ymd(paste(2023,"-05-01")),
                           enddate = lubridate::ymd(paste(2023,"-06-01")))

#gtfs_new <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS/20230503_merged.zip")
routes = gtfs_routes_sf(gtfs_new)
routes = routes[lengths(routes$geometry) > 2,]
qtm(routes)

gtfs_new <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS/GTFS_20230503/NE.zip")
gtfs_new$stops <- gtfs_new$stops[!is.na(gtfs_new$stops$stop_lon),]
gtfs_new = gtfs_clean(gtfs_new)
new_raw <- gtfs_trips_per_zone(gtfs_new, zone = zone, by_mode = TRUE,
                           startdate = lubridate::ymd(paste(2023,"-05-01")),
                           enddate = lubridate::ymd(paste(2023,"-06-01")))

gtfs_2019 <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS_oldversion/GTFS_20191008/NE.zip")
gtfs_2019$stops <- gtfs_2019$stops[!is.na(gtfs_2019$stops$stop_lon),]
gtfs_2019 = gtfs_clean(gtfs_2019)
o2019_raw <- gtfs_trips_per_zone(gtfs_2019, zone = zone, by_mode = TRUE,
                               startdate = lubridate::ymd(paste(2023,"-05-01")),
                               enddate = lubridate::ymd(paste(2023,"-06-01")))



new_raw <- readRDS("data/trips_per_la_by_mode_2004_2023.Rds")
new1_raw <- readRDS("data/trips_per_la_by_mode_2004_2023_onemonth.Rds")
old_raw <- readRDS("../TransportBlackspots-prebug/TransportBlackspots-main/data/trips_per_la_by_mode_2004_2023.Rds")

new = new_raw[new_raw$route_type == 3,]
new1 = new1_raw[new1_raw$route_type == 3,]
old = old_raw[old_raw$route_type == 3,]
#o2019 = o2019_raw[o2019_raw$route_type == 3,]

new1 = new1[new1$year %in% c(2010,2023),]
new = new[new$year %in% c(2010,2023),]
old = old[old$year %in% c(2010,2023),]

new1 = new1[,c("zone_id","year","tph_daytime_avg")]
new = new[,c("zone_id","year","tph_daytime_avg")]
old = old[,c("zone_id","year","tph_daytime_avg")]
#
new1 = pivot_wider(new1, id_cols = "zone_id", names_from = "year", values_from = "tph_daytime_avg")
new = pivot_wider(new, id_cols = "zone_id", names_from = "year", values_from = "tph_daytime_avg")
old = pivot_wider(old, id_cols = "zone_id", names_from = "year", values_from = "tph_daytime_avg")
names(new1) = c("zone_id","new1_tph_2010", "new1_tph_2023")
names(new) = c("zone_id","new_tph_2010", "new_tph_2023")
names(old) = c("zone_id","old_tph_2010", "old_tph_2023")
#
both = dplyr::left_join(new, old, by = "zone_id")
both$new_change = round((both$new_tph_2023 - both$new_tph_2010) / both$new_tph_2010 * 100,1)
both$old_change = round((both$old_tph_2023 - both$old_tph_2010) / both$old_tph_2010 * 100,1)
both$old_new_change = round(both$new_tph_2023 - both$old_tph_2023,1)
both = dplyr::left_join(both, new1, by = "zone_id")
both$new1_change = round((both$new1_tph_2023 - both$new1_tph_2010) / both$new1_tph_2010 * 100,1)

both = both[order(both$new_change),]
write.csv(both[,c("zone_id","new_tph_2010","new_tph_2023","old_tph_2010","old_tph_2023","new_change","old_change")],"data/old_vs_new_method.csv")

new = new[,c("zone_id","tph_Mon_Morning Peak")]
old = old[,c("zone_id","tph_Mon_Morning Peak")]
o2019 = o2019[,c("zone_id","tph_Mon_Morning Peak")]

names(new) = c("zone_id","new_tph_2023")
names(old) = c("zone_id","old_tph_2023")
names(o2019) = c("zone_id","old_tph_2019")

both = left_join(new, old, by = "zone_id")
both$change = both$new_tph_2023 - both$old_tph_2023

both = left_join(both, o2019, by = "zone_id")
both$change = both$new_tph_2023 - both$old_tph_2023

summary(summ$`runs_Mon_Morning Peak` - comp$`runs_Mon_Morning Peak`)
summary(comp$`runs_Mon_Morning Peak`)



summ = summ[order(summ$zone_id),]
comp = comp[order(comp$zone_id),]




summary(s1$zone_id == c1$zone_id)
summary(s1$`tph_Mon_Morning Peak` - c1$`tph_Mon_Morning Peak`)


foo = dplyr::left_join(s1, c1, by = "zone_id")




