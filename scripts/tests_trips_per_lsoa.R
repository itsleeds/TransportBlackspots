library(UK2GTFS)
library(tmap)
library(sf)

gtfs = gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/GTFS/20221102_merged.zip")
gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon),]
gtfs = gtfs_clean(gtfs)


source("R/stops_per_week_functions.R")
#stops_count = gtfs_stop_frequency(gtfs)
#summary(stops_count)

sf_use_s2(FALSE)

if(file.exists("data/GB_LSOA_2011_full_or_500mBuff.Rds")){
  zone = readRDS("data/GB_LSOA_2011_full_or_500mBuff.Rds")
} else {
  lsoa = readRDS("data/GB_LSOA_2011_full.Rds")
  lsoa = st_transform(lsoa, 27700)
  lsoa_cents = st_centroid(lsoa)
  lsoa$area = as.numeric(st_area(lsoa))
  lsoa_buff = st_buffer(lsoa_cents, 500)
  as.numeric(st_area(lsoa_buff[1,]))

  lsoa_big = lsoa[lsoa$area > 785000,]
  lsoa_buff_small = lsoa_buff[!lsoa_buff$code %in% lsoa_big$code,]
  lsoa_big$area <- NULL
  zone = rbind(lsoa_big, lsoa_buff_small)
  saveRDS(zone,"data/GB_LSOA_2011_full_or_500mBuff.Rds")
}


#stops_sf = gtfs_stops_sf(gtfs)
#qtm(stops_sf)


# TODO: NA lsoa returned
zone_service = gtfs_trips_per_zone(gtfs, zone)
zone2 = readRDS("data/GB_LSOA_2011_super_generalised.Rds")
zone2 = dplyr::left_join(zone2, zone_service, by = c("code" =  "zone_id"))

tmap_mode("plot")
m1 = tm_shape(zone2[,"runs_Mon_Morning Peak"]) +
  tm_fill("runs_Mon_Morning Peak",
              style = "fixed",
              breaks = quantile(zone2$`runs_Mon_Morning Peak`, probs = seq(0,1,0.1), na.rm = T),
              palette = tmaptools::get_brewer_pal("RdBu", n = 10))
tmap_save(m1, "plots/checks_GB_LSOA_test.png")


gtfs_trips_per_zone <- function(gtfs,
                                zone,
                                startdate = min(gtfs$calendar$start_date),
                                enddate = min(gtfs$calendar$start_date) + 31,
                                zone_id = 1){

  if(!sf::st_is_longlat(zone)){
    zone <- sf::st_transform(zone, 4326)
  }

  zone <- zone[,zone_id]
  names(zone)[1] <- "zone_id"

  # Join Zone id onto stop
  stops_zids <- gtfs$stops
  stops_zids <- stops_zids[!is.na(stops_zids$stop_lon),]

  stops_zids <- sf::st_as_sf(stops_zids,
                             coords = c("stop_lon","stop_lat"),
                             crs = 4326)
  stops_zids <- sf::st_join(stops_zids, zone) # Some stops in multiple Zones
  stops_zids <- stops_zids[,c("stop_id","zone_id")]

  # Trim GTFS to study period
  gtfs2 <- gtfs_trim_dates(gtfs, startdate = startdate, enddate = enddate)

  # Get the summaries for calendar
  calendar_dates_summary <- gtfs2$calendar_dates
  calendar_dates_summary$weekday = as.character(lubridate::wday(calendar_dates_summary$date, label = TRUE))
  calendar_dates_summary <- dplyr::group_by(calendar_dates_summary, service_id, weekday)
  calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                    extra = sum(exception_type == 1),
                                    canceled = sum(exception_type == 2))

  calendar_dates_summary_missing = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  calendar_dates_summary_missing = calendar_dates_summary_missing[!calendar_dates_summary_missing %in% unique(calendar_dates_summary$weekday)]
  if(length(calendar_dates_summary_missing) > 0){
    calendar_dates_summary_missing = data.frame(service_id = NA,
                                                weekday = calendar_dates_summary_missing,
                                                extra = NA,
                                                canceled = NA)
    calendar_dates_summary = rbind(calendar_dates_summary, calendar_dates_summary_missing)
  }

  calendar_dates_summary <- tidyr::pivot_wider(calendar_dates_summary,
                                               names_from = "weekday",
                                               values_from = c("extra","canceled"),
                                               values_fill = 0)
  calendar_dates_summary <- calendar_dates_summary[!is.na(calendar_dates_summary$service_id),]
  calendar <- count_weekday_runs(gtfs2$calendar)
  calendar <- calendar[,c("service_id","runs_monday","runs_tuesday",
                          "runs_wednesday","runs_thursday",
                          "runs_friday","runs_saturday","runs_sunday")]
  names(calendar) <- c("service_id","runs_Mon","runs_Tue",
                       "runs_Wed","runs_Thu",
                       "runs_Fri","runs_Sat","runs_Sun")

  #Join to Trips
  trips <- dplyr::left_join(gtfs2$trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_dates_summary, by = "service_id")

  trips[4:ncol(trips)] <- lapply(trips[4:ncol(trips)], function(x){
    ifelse(is.na(x),0,x)
  })

  trips$runs_Mon <- trips$runs_Mon + trips$extra_Mon - trips$canceled_Mon
  trips$runs_Tue <- trips$runs_Tue + trips$extra_Tue - trips$canceled_Tue
  trips$runs_Wed <- trips$runs_Wed + trips$extra_Wed - trips$canceled_Wed
  trips$runs_Thu <- trips$runs_Thu + trips$extra_Thu - trips$canceled_Thu
  trips$runs_Fri <- trips$runs_Fri + trips$extra_Fri - trips$canceled_Fri
  trips$runs_Sat <- trips$runs_Sat + trips$extra_Sat - trips$canceled_Sat
  trips$runs_Sun <- trips$runs_Sun + trips$extra_Sun - trips$canceled_Sun

  # trim out unneded data
  trips <- trips[,c("trip_id","route_id","service_id",
                    "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                    "runs_Fri","runs_Sat","runs_Sun")]

  # Join on trip info to stop times
  stop_times <- dplyr::left_join(gtfs2$stop_times, trips, by = "trip_id")

  # -1 so that time between 00:00 and 00:59 are not NA
  # +35 for any service in GTFS that runs past midnight (note that some may arrive following morning but a counted as evening)
  message("Stops that run past midnight are recorded in Night regardless of the time")
  stop_times$time_bands <- cut(lubridate::hour(stop_times$departure_time),
                               breaks = c(-1, 6, 10, 15, 18, 20, 36),
                               labels = c("Night", "Morning Peak", "Midday","Afternoon Peak","Evening","Night"))

  stop_times <- stop_times[,c(c("trip_id","stop_id","time_bands",
                                "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                                "runs_Fri","runs_Sat","runs_Sun"))]

  stop_times <- dplyr::left_join(stop_times, stops_zids, by = "stop_id", multiple = "all")
  stop_times <- sf::st_drop_geometry(stop_times)
  stop_times$geometry <- NULL

  res <- dplyr::group_by(stop_times, zone_id)
  res <- dplyr::group_split(res)
  future::plan(future::multisession)
  res <- future.apply::future_lapply(res, internal_trips_per_zone)
  future::plan(future::sequential)
  res <- dplyr::bind_rows(res)
  res[2:ncol(res)] <- lapply(res[2:ncol(res)],function(x){ifelse(is.na(x),0,x)})


  return(res)

  # lsoa2 = dplyr::left_join(lsoa, bar, by = c("code" = "zone_id"))
  # lsoa2 = lsoa2[!is.na(lsoa2$runs_Mon_Night),]
  # tm_shape(lsoa2) +
  #   tm_fill("runs_Mon_Morning Peak", style = "jenks")
}


internal_trips_per_zone <- function(x){
  x <- x[!duplicated(x$trip_id),]
  #zone_id = x$zone_id[1]
  #x <- x[,c("time_bands","runs_Mon","runs_Tue","runs_Wed","runs_Thu","runs_Fri","runs_Sat","runs_Sun")]
  x <- dplyr::group_by(x,zone_id, time_bands)
  suppressMessages({
    x <- dplyr::summarise(x,
                          runs_Mon = sum(runs_Mon),
                          runs_Tue = sum(runs_Tue),
                          runs_Wed = sum(runs_Wed),
                          runs_Thu = sum(runs_Thu),
                          runs_Fri = sum(runs_Sat),
                          runs_Sat = sum(runs_Sat),
                          runs_Sun = sum(runs_Sun))
  })

  y <- tidyr::pivot_wider(x,
                          id_cols = "zone_id",
                          values_from = c(runs_Mon:runs_Sun),
                          names_from = c(time_bands)
                          )
  return(y)
}
