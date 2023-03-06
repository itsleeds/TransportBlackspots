library(UK2GTFS)
library(dplyr)
library(tmap)
tmap_mode("view")


gtfs = gtfs_read("C:/Users/earmmor/Downloads/itm_south_west_gtfs.zip")
calendar = gtfs$calendar
summary(calendar$start_date)
summary(calendar$end_date)






stop_counts <- sf::st_as_sf(stop_counts, coords = c("stop_lon","stop_lat"), crs = 4326)

qtm(stop_counts, dots.col = "stops_per_week")

tm_shape(stop_counts) +
  tm_dots(col = "stops_per_week", style = "quantile")


countwd2 <- function(startdate, enddate, weekday){
  if(is.na(startdate)){
    return(0)
  }
  if(is.na(enddate)){
    return(0)
  }
  d <- as.integer(enddate - startdate) + 1
  d %/% 7 +
    (weekday %in% weekdays(seq(startdate, length.out=d %% 7, by=1)))
}


count_stops <- function(gtfs,
                        startdate = lubridate::ymd("2020-03-01"),
                        enddate = lubridate::ymd("2020-04-30")){
  message("Only using stops between ",startdate," and ",enddate)
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_days <- gtfs$calendar_dates

  calendar <- calendar[calendar$start_date <= enddate,]
  calendar <- calendar[calendar$end_date >= startdate,]

  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)

  calendar_days <- calendar_days[calendar_days$service_id %in% calendar$service_id,]
  calendar_days <- calendar_days[calendar_days$date >= startdate,]
  calendar_days <- calendar_days[calendar_days$date <= enddate,]

  calendar_days <- calendar_days %>%
    dplyr::group_by(service_id) %>%
    dplyr::summarise(runs_extra = sum(exception_type == 1),
                     runs_canceled = sum(exception_type == 2))

  trips <- trips[trips$service_id %in% calendar$service_id, ]
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  # work out how many times the trip in run
  trips <- dplyr::left_join(trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_days, by = "service_id")

  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0

  message("Counting trips on each day")
  future::plan("future::multisession")

  trips$n_monday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Monday")
  trips$n_tuesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Tuesday")
  trips$n_wednesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Wednesday")
  trips$n_thursday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Thursday")
  trips$n_friday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Friday")
  trips$n_saturday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Saturday")
  trips$n_sunday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Sunday")

  future::plan("future::sequential")

  trips$runs_monday <- trips$monday * trips$n_monday
  trips$runs_tuesday <- trips$tuesday * trips$n_tuesday
  trips$runs_wednesday <- trips$wednesday * trips$n_wednesday
  trips$runs_thursday <- trips$thursday * trips$n_thursday
  trips$runs_friday <- trips$friday * trips$n_friday
  trips$runs_saturday <- trips$saturday * trips$n_saturday
  trips$runs_sunday <- trips$sunday * trips$n_sunday

  message("Summariseing results")
  trips$runs_total <- trips$runs_monday + trips$runs_tuesday +
    trips$runs_wednesday + trips$runs_thursday + trips$runs_friday +
    trips$runs_saturday + trips$runs_sunday + trips$runs_extra - trips$runs_canceled

  trips$runs_per_week <- trips$runs_total / ((as.numeric(trips$end_date - trips$start_date) + 1)/7)

  # Catch Single Day services
  trips$runs_per_week <- ifelse(trips$start_date == trips$end_date, 1, trips$runs_per_week)

  trips <- trips[,c("trip_id","start_date","end_date","runs_total","runs_per_week")]
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")
  stop_times_summary <- stop_times %>%
    dplyr::group_by(stop_id) %>%
    dplyr::summarise(stops_total = sum(runs_total),
                     stops_per_week = sum(runs_per_week))

  stops <- dplyr::left_join(gtfs$stops, stop_times_summary, by = "stop_id")
  return(stops)
}



stop_counts = count_stops(gtfs,
                          startdate = lubridate::ymd("2023-03-01"),
                          enddate = lubridate::ymd("2023-04-01"))
