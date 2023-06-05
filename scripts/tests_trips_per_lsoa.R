library(UK2GTFS)
library(tmap)
library(sf)

path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/"
# gtfs <- gtfs_read(file.path(path,paste0("NPTDR/GTFS/NPTDR_",2004,".zip")))
# gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
# gtfs = gtfs_clean(gtfs)

dir.create("tmp")
unzip(file.path("D:/OneDrive - University of Leeds/Data/OA Bounadries/EW_LSOA_2011_Centroids.zip"),
      exdir = "tmp")
ew_cents = read_sf("tmp/Lower_Layer_Super_Output_Areas__December_2011__Population_Weighted_Centroids.shp")
unlink("tmp", recursive = T)

dir.create("tmp")
unzip(file.path("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_DataZoneCent_2011.zip"),
      exdir = "tmp")
sc_cents = read_sf("tmp/SG_DataZone_Cent_2011.shp")
unlink("tmp", recursive = T)

ew_cents = ew_cents[,"lsoa11cd"]
sc_cents = sc_cents[,"DataZone"]
names(ew_cents)[1] = "code"
names(sc_cents)[1] = "code"
lsoa_cents = rbind(ew_cents, sc_cents)

#source("R/stops_per_week_functions.R")
#stops_count = gtfs_stop_frequency(gtfs)
#summary(stops_count)

sf_use_s2(FALSE)

if(file.exists("data/GB_LSOA_2011_full_or_500mBuff.Rds")){
  zone = readRDS("data/GB_LSOA_2011_full_or_500mBuff.Rds")
} else {
  lsoa = readRDS("data/GB_LSOA_2011_full.Rds")
  lsoa = lsoa[lsoa$code %in% lsoa_cents$code,] # Remove NI
  lsoa = st_transform(lsoa, 27700)
  #lsoa_cents = st_centroid(lsoa)
  lsoa_cents = lsoa_cents[order(lsoa_cents$code),]
  lsoa = lsoa[order(lsoa$code),]
  lsoa$area = as.numeric(st_area(lsoa))
  lsoa_buff = st_buffer(lsoa_cents, 500)
  as.numeric(st_area(lsoa_buff[1,]))

  lsoa_big = lsoa[lsoa$area > 785000,]
  lsoa_small = lsoa[!lsoa$code %in% lsoa_big$code,]
  lsoa_buff_small = lsoa_buff[!lsoa_buff$code %in% lsoa_big$code,]
  lsoa_big$area <- NULL

  # To maintain continotus areas merge buff and LSOA
  res <- list()
  if(!identical(lsoa_small$code, lsoa_buff_small$code)){
    stop("LSOA and cents codes don't match")
  }
  for(i in 1:nrow(lsoa_small)){
    if(i %% 5000 == 0){message(i," / ",nrow(lsoa_small))}
    res[[i]] <- st_union(st_buffer(lsoa_small$geometry[i],0), lsoa_buff_small$geometry[i])[[1]]
    # plot(lsoa_small$geometry[i])
    # plot(lsoa_buff_small$geometry[i], add = T)
    # plot(res[[i]])
  }
  res = st_as_sfc(res, crs = 27700)
  lsoa_buff_small$geometry <- res
  zone = rbind(lsoa_big, lsoa_buff_small)
  #Sometimes stops are just off the cost (slight location error)
  zone = st_buffer(zone, 100)
  saveRDS(zone,"data/GB_LSOA_2011_full_or_500mBuff.Rds")

}


#stops_sf = gtfs_stops_sf(gtfs)
#qtm(stops_sf)


# TODO: NA lsoa returned
# zone_service = gtfs_trips_per_zone(gtfs, zone)
# zone2 = readRDS("data/GB_LSOA_2011_super_generalised.Rds")
# zone2 = dplyr::left_join(zone2, zone_service, by = c("code" =  "zone_id"))
#
# tmap_mode("plot")
# m1 = tm_shape(zone2[,"runs_Mon_Morning Peak"]) +
#   tm_fill("runs_Mon_Morning Peak",
#               style = "fixed",
#               breaks = quantile(zone2$`runs_Mon_Morning Peak`, probs = seq(0,1,0.1), na.rm = T),
#               palette = tmaptools::get_brewer_pal("RdBu", n = 10))
# tmap_save(m1, "plots/checks_GB_LSOA_test.png")
#


