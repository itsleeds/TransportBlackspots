
# create sf trip object from GTFS data
make_trip_geoms <- function(gtfs){

  # select stops, stop times and trips from GTFS object
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  #routes <- gtfs$routes
  trips <- gtfs$trips
  #calendar <- gtfs$calendar

  # keep only stop id and geometry details
  stops <- stops %>%
    select(stop_id,
           stop_lon,
           stop_lat)
  # join stop geometry to stop times
  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")

  # split stop data by trip id
  stop_times_split <- split(stop_times, stop_times$trip_id)

  # internal function to make line geometry of stop data
  make_geom <- function(x){
    geom <- x[,c("stop_lon", "stop_lat")] # keep only coordinates of bus stop locations for each trip
    geom <- as.matrix(geom) # convert to a matrix (which is what st_linestring requires)
    geom <- sf::st_linestring(geom, dim = "XY") # makes a geometry object from a matrix; dim options are 2-3 dimensional variations
    return(geom)
  }

  # apply function across all stops
  geom <- pbapply::pblapply(stop_times_split, make_geom)

  # create an sf object/data.frame from the trip ids and line geometries of trip from stop locations
  trips_geom <- sf::st_as_sf(data.frame(trip_id = names(stop_times_split), # because we have split by trip id, names of the split are the trip ids
                                        geometry = sf::st_sfc(geom, crs = 4326), # set geometry features of sf object as the geom from lon/lat fields, set crs as the same time.
                                        stringsAsFactors = FALSE))

  return(trips_geom)

}

# function to determine number of a given day of the week between two dates
# start dates and end dates need to be in date format, i.e. not character strings
countwd2 <- function(startdate, enddate, weekday){

  #check no NA values
  if(is.na(startdate)){
    return(0)
  }
  if(is.na(enddate)){
    return(0)
  }

  # determine the total number of days between days (inclusive, so + 1 needed)
  d <- as.integer(enddate - startdate) + 1
  # find number of complete weeks in the interval using integer division
  # then add the number of times the weekday appears in the remaining incomplete week.
  d %/% 7 +
    (weekday %in% weekdays(seq(startdate, length.out=d %% 7, by=1))) ### HELP: I don't understand this use of seq()

}

# countwd2(startdate = as.Date("1980-01-14"), enddate = as.Date("1980-04-28"), "Sunday")

count_stops <- function(gtfs,
                        startdate = lubridate::ymd("2023-03-01"),
                        enddate = lubridate::ymd("2023-03-31")){

  message("Only using stops between ",startdate," and ",enddate)

  # extract data.frames needed for analysis from gtfs object
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_days <- gtfs$calendar_dates

  # filter calendar to include only period of time between start date
  # start date of a route must be before the end date and end date of a route must be after the start date
  calendar <- calendar %>%
    dplyr::filter(start_date <= enddate & end_date >= startdate)

  # we are okay with routes that started before the time period and continue through the time window of interest
  # we are also okay with routes that continue on after time window of interest.
  # but we need to set the dates to be within the time window of interest, otherwise it will mess things up during the analysis.
  # rememer these are just the timetable routes and how long that time table runs for.
  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)

  # Now, only keep the calendar days date for services which are running in the time window of interest
  # and which correspond to service ids running at the time
  calendar_days <- calendar_days %>%
    dplyr::filter(service_id %in% calendar$service_id)
  calendar_days <- calendar_days %>%
    dplyr::filter(date <= enddate & date >= startdate)

  # sum the number of extra and cancelled days for each service over the time period
  calendar_days <- calendar_days %>%
    mutate(day_of_week = weekdays(date)) %>%
    mutate(weekday = weekdays(date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
           weekend = weekdays(date) %in% c("Saturday", "Sunday")) %>%
    mutate(weekend_extra = ifelse(weekend & exception_type == 1, 1, 0),
           weekend_cancelled = ifelse(weekend & exception_type == 2, 1, 0),
           weekday_extra = ifelse(weekday & exception_type == 1, 1, 0),
           weekday_cancelled = ifelse(weekday & exception_type == 2, 1, 0))

  calendar_days <- calendar_days %>%
    dplyr::group_by(service_id) %>%
    dplyr::summarise(runs_extra = sum(exception_type == 1),
                     runs_canceled = sum(exception_type == 2),
                     runs_weekend_extra = sum(weekend_extra),
                     runs_weekend_cancelled = sum(weekend_cancelled),
                     runs_weekday_extra = sum(weekday_extra),
                     runs_weekday_cancelled = sum(weekday_cancelled)) %>%
    dplyr::ungroup()

  # similarly to above, filter the trip and subsequently the stop times data to within the time period window
  trips <- trips %>%
    dplyr::filter(service_id %in% calendar$service_id)
  stop_times <- stop_times %>%
    dplyr::filter(trip_id %in% trips$trip_id)

  # join calendar and calendar days to trip data (using bespoke gtfs join function)
  trips <- gtfs_join(join.type = "left",
                     join.var = "service_id",
                     trips,
                     calendar,
                     calendar_days)

  # set any NA values for extra and cancels to 0
  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0
  trips$runs_weekend_extra[is.na(trips$runs_weekend_extra)] <- 0
  trips$runs_weekend_cancelled[is.na(trips$runs_weekend_cancelled)] <- 0
  trips$runs_weekday_extra[is.na(trips$runs_weekday_extra)] <- 0
  trips$runs_weekday_cancelled[is.na(trips$runs_weekday_cancelled)] <- 0

  message("Counting trips on each day")
  future::plan("future::multisession")

  # count the number of days of the week in the time period
  trips$n_monday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Monday")
  trips$n_tuesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Tuesday")
  trips$n_wednesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Wednesday")
  trips$n_thursday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Thursday")
  trips$n_friday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Friday")
  trips$n_saturday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Saturday")
  trips$n_sunday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Sunday")

  # with this information, we can now easily calculate the number of weekdays and weekend days in the period
  trips <- trips %>%
    mutate(n_weekdays = n_monday + n_tuesday + n_wednesday + n_thursday + n_friday,
           n_weekends = n_saturday + n_sunday)

  future::plan("future::sequential")

  # multiply the number of days of each week in each time period with the number of trips on each corresponding day of the week
  trips <- trips %>%
    dplyr::mutate(runs_monday = monday * n_monday,
                  runs_tuesday = tuesday * n_tuesday,
                  runs_wednesday = wednesday * n_wednesday,
                  runs_thursday = thursday * n_thursday,
                  runs_friday = friday * n_friday,
                  runs_saturday = saturday * n_saturday,
                  runs_sunday = sunday * n_sunday)

  message("Summarising results")
  trips <- trips %>%
    dplyr::mutate(runs_total = runs_monday + runs_tuesday + runs_wednesday +
                                runs_thursday + runs_friday + runs_saturday +
                                runs_sunday + runs_extra - runs_canceled,
                  runs_weekday = runs_monday + runs_tuesday + runs_wednesday +
                                  runs_thursday + runs_friday + runs_weekday_extra -
                                  runs_weekday_cancelled,
                  runs_weekend = runs_saturday + runs_sunday + runs_weekend_extra -
                                  runs_weekend_cancelled) %>%
    dplyr::mutate(runs_per_week = runs_total /
                                  ((as.numeric(trips$end_date - trips$start_date) + 1)/7),
                  runs_per_weekday = runs_weekday /
                                      (n_weekdays / 5),
                  runs_per_weekend = runs_weekend /
                                      (n_weekends / 2)) %>%
    # Catch Single Day services
    dplyr::mutate(single_day_service = start_date == end_date) %>%
    dplyr::mutate(runs_per_week = ifelse(single_day_service, 1, runs_per_week)) ## HELP: Is this right - could have multiple runs in one day?
                                                                                ## Yes: because this is trip data not route data. A route will have multiple trips.

  # select key fields of interest only
  trips <- trips %>%
    select(trip_id,
           start_date,
           end_date,
           runs_total,
           runs_weekday,
           runs_weekend,
           runs_per_week,
           runs_per_weekday,
           runs_per_weekend♣)

  # Now we join the trip id to the stop times
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")

}



summarise_stops <- function(stop_times_with_trips, gtfs) {

  stop_times_summary <- stop_times_with_trips %>%
    dplyr::group_by(stop_id) %>%
    dplyr::summarise(stops_total = sum(runs_total),
                     stops_per_week = sum(runs_per_week))

  stops <- dplyr::left_join(gtfs$stops, stop_times_summary, by = "stop_id")
  return(stops)

}


gtfs_join <- function(join.type = "left", join.var, ...) {
  if(!join.type %in% c("left", "right", "inner", "full")) {
    stop("Incorrect join specified. Must be one of: 'left', 'right', 'inner', or 'full'")
  }
  if(join.type == "left") {
    joined.data <- Reduce(function(x, y) dplyr::left_join(x, y, by = join.var),
                          list(...))
  }
  if(join.type == "right") {
    joined.data <- Reduce(function(x, y) dplyr::right_join(x, y, by = join.var),
                          list(...))
  }
  if(join.type == "inner") {
    joined.data <- Reduce(function(x, y) dplyr::inner_join(x, y, by = join.var),
                          list(...))
  }
  if(join.type == "full") {
    joined.data <- Reduce(function(x, y) dplyr::full_join(x, y, by = join.var),
                          list(...))
  }
  return(joined.data)
}
