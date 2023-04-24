
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
  trips$n_monday    <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Monday")
  trips$n_tuesday   <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Tuesday")
  trips$n_wednesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Wednesday")
  trips$n_thursday  <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Thursday")
  trips$n_friday    <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Friday")
  trips$n_saturday  <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Saturday")
  trips$n_sunday    <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Sunday")

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
           runs_per_weekend)

  # Now we join the trip id to the stop times
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")

}



summarise_stops <- function(stop_times_with_trips, gtfs) {

  stop_times_summary <- stop_times_with_trips %>%
    dplyr::group_by(stop_id) %>%
    dplyr::summarise(stops_total = sum(runs_total),
                     stops_per_week = sum(runs_per_week),
                     stops_total_weekdays = sum(runs_weekday),
                     stops_per_weekday = sum(runs_per_weekday),
                     stops_total_weekends = sum(runs_weekend),
                     stops_per_weekend = sum(runs_per_weekend))

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

# make data frame of trips, stops and calendars
# get stop times - left join trip table, by trip id. Show stop times for each trip.
# then left join calendar by schedule id. then trip, with stops and days of the week.
# ideally process this with less than an hour runtime.

stop_timetables <- function(gtfs,
                            startdate = lubridate::ymd("2023-03-01"),
                            enddate = lubridate::ymd("2023-03-31")) {

  # start timer
  tic("Stops, trips and calendar data processed and combined")

  # select stop times, trips and calendar from the GTFS file
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- calendar_days_of_week_count(gtfs,
                                          startdate,
                                          enddate)

  calendar_days <- extra_and_cancelled_services(gtfs,
                                                startdate,
                                                enddate)

  #str(stop_times)
  #str(trips)
  #str(calendar)

  # join files together
  stoptimes_trips <- left_join(stop_times, trips, by = "trip_id")
  stoptimes_trips_cal <- left_join(stoptimes_trips, calendar, by = "service_id")

  # clean - keep only fields needed
  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    select(stop_id,
           trip_id,
           service_id,
           route_id,
           arrival_time,
           departure_time,
           stop_sequence,
           start_date,
           end_date,
           #stop_headsign,
           #pickup_type,
           #drop_off_type,
           #shape_dist_traveled,
           #timepoint,
           #trip_headsign,
           #block_id,
           #shape_id,
           #wheelchair_accessible,
           #vehicle_journey_code,
           monday,
           tuesday,
           wednesday,
           thursday,
           friday,
           saturday,
           sunday,
           #n_monday,
           #n_tuesday,
           #n_wednesday,
           #n_thursday,
           #n_friday,
           #n_saturday,
           #n_sunday,
           #n_weekdays,
           #n_working_weeks,
           #n_weekends,
           runs_monday,
           runs_tuesday,
           runs_wednesday,
           runs_thursday,
           runs_friday,
           runs_saturday,
           runs_sunday,
           runs_weekdays)


  # identify is a weekday service (can also run on weekends).
  # and calculate the total number of weekdays the service runs on
  weekday_cols <- c("monday", "tuesday", "wednesday", "thursday", "friday")
  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    mutate(weekday_service = ifelse(rowSums(stoptimes_trips_cal[weekday_cols]) > 0, TRUE, FALSE)) %>%
    mutate(weekdays_total = rowSums(stoptimes_trips_cal[weekday_cols]))

  # filter calendar to include only period of time between start date
  # start date of a route must be before the end date and end date of a route must be after the start date
  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    dplyr::filter(start_date <= enddate & end_date >= startdate)

  # we are okay with routes that started before the time period and continue through the time window of interest
  # we are also okay with routes that continue on after time window of interest.
  # but we need to set the dates to be within the time window of interest, otherwise it will mess things up during the analysis.
  # rememer these are just the timetable routes and how long that time table runs for.
  stoptimes_trips_cal$start_date <- dplyr::if_else(stoptimes_trips_cal$start_date < startdate,
                                                   startdate,
                                                   stoptimes_trips_cal$start_date)
  stoptimes_trips_cal$end_date <- dplyr::if_else(stoptimes_trips_cal$end_date > enddate,
                                                 enddate,
                                                 stoptimes_trips_cal$end_date)

  # convert arrival and departure times to seconds
  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    mutate(arrival_seconds = period_to_seconds(arrival_time),
           departure_seconds = period_to_seconds(departure_time))
  # if times go into the following day then covert back into 24 hour limit
  secs_in_day = 60 * 60 * 24
  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    mutate(arrival_seconds = ifelse(arrival_seconds > secs_in_day - 1, arrival_seconds - secs_in_day, arrival_seconds),
           departure_seconds = ifelse(departure_seconds > secs_in_day - 1, departure_seconds - secs_in_day, departure_seconds))

  # identify time slot
  hours_to_secs <- function(x) x * 60 * 60

  stoptimes_trips_cal <- stoptimes_trips_cal %>%
    mutate(rush_hour = between(arrival_seconds, hours_to_secs(6), hours_to_secs(9)) | between(arrival_seconds, hours_to_secs(16), hours_to_secs(19))) %>%
    mutate(evening = (arrival_seconds >= hours_to_secs(19)) | (arrival_seconds <= hours_to_secs(1)))

  # end timer
  toc()

  return(stoptimes_trips_cal)

}

#' we now want a function that takes the stop/trips/calendar data and summarises the stop frequency by:
#'  - all trips throughout the week
#'  - all weekday rushhour trips
#'  - all saturday trips
#'  - all sunday trips
summarise_stops_by_day <- function(gtfs,
                                   stops_calendar,
                                   startdate,
                                   enddate) {

  # summarise total number of stops by service by each weekday
  stops_summary <- stops_calendar %>%
    group_by(stop_id,
             service_id) %>%
    summarise(stops_monday = sum(runs_monday),
              stops_tuesday = sum(runs_tuesday),
              stops_wednesday = sum(runs_wednesday),
              stops_thursday = sum(runs_thursday),
              stops_friday = sum(runs_friday),
              stops_saturday = sum(runs_saturday),
              stops_sunday = sum(runs_sunday)) %>%
    ungroup()

  # get extra and cancelled services for each service ID
  calendar_days <- extra_and_cancelled_services(gtfs,
                                                startdate,
                                                enddate)

  # join to stops summary
  stops_summary <- left_join(stops_summary, calendar_days, by = "service_id")
  # complete missing data (i.e. for services with no cancellations or extra services)
  stops_summary[is.na(stops_summary)] <- 0

  # make adjustments for these extra services and cancellations
  stops_summary <- stops_summary %>%
    mutate(stops_monday = stops_monday + monday_extra - monday_cancelled,
           stops_tuesday = stops_tuesday + tuesday_extra - tuesday_cancelled,
           stops_wednesday = stops_wednesday + wednesday_extra - wednesday_cancelled,
           stops_thursday = stops_thursday + thursday_extra - thursday_cancelled,
           stops_friday = stops_friday + friday_extra - friday_cancelled,
           stops_saturday = stops_saturday + saturday_extra - saturday_cancelled,
           stops_sunday = stops_sunday + sunday_extra - sunday_cancelled)

  # now summarise just by stop ID for adjusted service details
  stops_summary <- stops_summary %>%
    group_by(stop_id) %>%
    summarise(stops_monday = sum(stops_monday),
              stops_tuesday = sum(stops_tuesday),
              stops_wednesday = sum(stops_wednesday),
              stops_thursday = sum(stops_thursday),
              stops_friday = sum(stops_friday),
              stops_saturday = sum(stops_saturday),
              stops_sunday = sum(stops_sunday)) %>%
    ungroup() %>%
    mutate(stops_weekdays = stops_monday + stops_tuesday + stops_wednesday + stops_thursday + stops_friday)

  # calculate total numbers of each day of the week between two dates
  all_dates <- seq(from = startdate, to = enddate, by = "days")

  n_mon = length(which(wday(all_dates, label = TRUE) == "Mon"))
  n_tue = length(which(wday(all_dates, label = TRUE) == "Tue"))
  n_wed = length(which(wday(all_dates, label = TRUE) == "Wed"))
  n_thu = length(which(wday(all_dates, label = TRUE) == "Thu"))
  n_fri = length(which(wday(all_dates, label = TRUE) == "Fri"))
  n_sat = length(which(wday(all_dates, label = TRUE) == "Sat"))
  n_sun = length(which(wday(all_dates, label = TRUE) == "Sun"))

  # calculate runs per weekday
  stops_summary <- stops_summary %>%
    mutate(stops_per_monday = stops_monday / n_mon,
           stops_per_tuesday = stops_tuesday / n_tue,
           stops_per_wednesday = stops_wednesday / n_wed,
           stops_per_thursday = stops_thursday / n_thu,
           stops_per_friday = stops_friday / n_fri,
           stops_per_saturday = stops_saturday / n_sat,
           stops_per_sunday = stops_sunday / n_sun) %>%
    mutate(stops_per_weekday = stops_per_monday + stops_per_tuesday + stops_per_wednesday + stops_per_thursday + stops_per_friday) %>%
    mutate(stops_per_week = stops_per_weekday + stops_per_saturday + stops_per_sunday)

}

# take the stops data for all of
summarise_all_stop_data <- function(gtfs,
                                    stops_calendar,
                                    startdate,
                                    enddate) {

  # start timer
  tic("Summarise stops by day of the week")

  # summarise all services by day of the week
  stops_by_day <- summarise_stops_by_day(gtfs,
                                         stops_calendar,
                                         startdate,
                                         enddate)

  # keep only the stops by days fields
  stops_by_day <- stops_by_day %>%
    select(stop_id,
           stops_weekdays,
           stops_saturday,
           stops_sunday,
           stops_per_week,
           stops_per_weekday,
           stops_per_saturday,
           stops_per_sunday)

  # get only services which stop at stops during rush hour period
  stops_calendar_rushhour <- stops_calendar %>%
    filter(rush_hour)
  # and summarise these
  stops_by_day_rushhour <- summarise_stops_by_day(gtfs,
                                                  stops_calendar_rushhour,
                                                  startdate,
                                                  enddate)

  # rename the rush hour stop details
  stops_by_day_rushhour <- stops_by_day_rushhour %>%
    select(stop_id,
           rushhour_stops_weekdays = stops_weekdays,
           #runs_saturday,
           #runs_sunday,
           rushhour_stops_per_weekday = stops_per_weekday,
           #runs_per_saturday,
           #runs_per_sunday
    )

  # join the full data set and the rush hour data set together
  stops_runs <- left_join(stops_by_day, stops_by_day_rushhour, by = "stop_id")
  # set any na values to 0.
  stops_runs[is.na(stops_runs)] <- 0

  # join summary stop data to main gtfs stop file.
  stops_runs <- left_join(gtfs$stops, stops_runs, by = "stop_id")

  # turn into sf object.
  stops_runs <- sf::st_as_sf(stops_runs, coords = c("stop_lon","stop_lat"), crs = 4326)

  # end timer and return stop data
  toc()
  return(stops_runs)

}


calendar_days_of_week_count <- function(gtfs,
                                        startdate,
                                        enddate) {

  calendar <- gtfs$calendar

  # filter and correct dates for time period
  # filter calendar to include only period of time between start date
  # start date of a route must be before the end date and end date of a route must be after the start date
  calendar <- calendar %>%
    dplyr::filter(start_date <= enddate & end_date >= startdate)

  # we are okay with routes that started before the time period and continue through the time window of interest
  # we are also okay with routes that continue on after time window of interest.
  # but we need to set the dates to be within the time window of interest, otherwise it will mess things up during the analysis.
  # remember these are just the timetable routes and how long that time table runs for.
  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)

  # UPDATE: use function from stops_per_week_functions.R
  calendar <- count_weekday_runs(calendar)

  # # count the number of days of the week in the time period
  # message("Counting number of each weekday in the time period")
  # future::plan("future::multisession")
  #
  # calendar$n_monday    <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Monday")
  # calendar$n_tuesday   <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Tuesday")
  # calendar$n_wednesday <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Wednesday")
  # calendar$n_thursday  <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Thursday")
  # calendar$n_friday    <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Friday")
  # calendar$n_saturday  <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Saturday")
  # calendar$n_sunday    <- future.apply::future_mapply(countwd2, startdate = calendar$start_date, enddate = calendar$end_date, weekday = "Sunday")
  #
  # # with this information, we can now easily calculate the number of weekdays and weekend days in the period
  # calendar <- calendar %>%
  #   mutate(n_weekdays = n_monday + n_tuesday + n_wednesday + n_thursday + n_friday) %>%
  #   mutate(n_working_weeks = n_weekdays / 5,
  #          n_weekends = (n_saturday + n_sunday) / 2)
  #
  # future::plan("future::sequential")
  #
  # # multiply the number of days of each week in each time period with the number of trips on each corresponding day of the week
  # message("Counting number of runs each week day during the time period")
  # calendar <- calendar %>%
  #   dplyr::mutate(runs_monday = monday * n_monday,
  #                 runs_tuesday = tuesday * n_tuesday,
  #                 runs_wednesday = wednesday * n_wednesday,
  #                 runs_thursday = thursday * n_thursday,
  #                 runs_friday = friday * n_friday,
  #                 runs_saturday = saturday * n_saturday,
  #                 runs_sunday = sunday * n_sunday) %>%
  #   dplyr::mutate(runs_weekdays = runs_monday + runs_tuesday + runs_wednesday + runs_thursday + runs_friday)

}


extra_and_cancelled_services <- function(gtfs,
                                         startdate,
                                         enddate) {

  # get calendar dates data frame from GTFS file
  calendar_days <- gtfs$calendar_dates

  # filter calendar days within period of interest
  calendar_days <- calendar_days %>%
    filter(between(date, startdate, enddate))

  # identify the days of the week from dates and simplify weekdays
  weekday_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  calendar_days <- calendar_days %>%
    mutate(weekday = weekdays(date)) %>%
    mutate(weekday_simple = ifelse(weekday %in% weekday_days, "Weekday", weekday))

  # label the extra/cancellation days
  calendar_days <- calendar_days %>%
    mutate(day_type = case_when(exception_type == 1 ~ "extra",
                                exception_type == 2 ~ "cancelled"))

  # summarise extra/cancelled days by service and weekday/sat/sun
  calendar_days <- calendar_days %>%
    group_by(service_id,
             day_type,
             weekday) %>%
    summarise(n = n()) %>%
    ungroup()

  # set weekdays to lower case, unite with day type and spread wide
  calendar_days <- calendar_days %>%
    mutate(weekday = tolower(weekday)) %>%
    unite(day_and_type, weekday, day_type, sep = "_") %>%
    spread(key = day_and_type,
           value = n,
           fill = 0)

  # some fields may be missing, so add and set as 0
  if(!"sunday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(sunday_extra = 0)
  }
  if(!"monday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(monday_extra = 0)
  }
  if(!"tuesday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(tuesday_extra = 0)
  }
  if(!"wednesday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(wednesday_extra = 0)
  }
  if(!"thursday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(thursday_extra = 0)
  }
  if(!"friday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(friday_extra = 0)
  }
  if(!"saturday_extra" %in% names(calendar_days)) {
    calendar_days <- calendar_days %>%
      mutate(saturday_extra = 0)
  }

  return(calendar_days)

}

# summarise stop data by LSOA by intersecting stop location sf data with LSOA boundaries (or 500m radius from centroid)
summarise_stops_by_lsoa <- function(stops_runs) {

  # start timer
  tic("Stops summarised by LSOA")

  # change crs system to BNG to align with LSOA boundary CRS
  stops_runs <- st_transform(stops_runs, crs = 27700)

  # get boundaries (which decides whether to use LSOA boundaries or radius from centroid - whichever is bigger)
  lsoa_transit_boundary <- make_lsoa_boundary_file(radius = 500)

  # intersect LSOA boundaries
  stops_lsoa <- st_intersection(lsoa_transit_boundary, stops_runs)

  # TODO: how to summarise stop data by lsoa?
  stops_lsoa_summary <- stops_lsoa %>%
    st_drop_geometry() %>%
    group_by(lsoa11cd) %>%
    summarise(stops_weekdays = sum(stops_weekdays, na.rm = TRUE),
              stops_saturday = sum(stops_saturday, na.rm = TRUE),
              stops_sunday = sum(stops_sunday, na.rm = TRUE),
              stops_per_week = sum(stops_per_week, na.rm = TRUE),
              stops_per_weekday = sum(stops_per_weekday, na.rm = TRUE),
              stops_per_saturday = sum(stops_per_saturday, na.rm = TRUE),
              stops_per_sunday = sum(stops_per_sunday, na.rm = TRUE),
              rushhour_stops_weekdays = sum(rushhour_stops_weekdays, na.rm = TRUE),
              rushhour_stops_per_weekday = sum(rushhour_stops_per_weekday, na.rm = TRUE),
              number_of_bus_stops = n())


  # get actual lsoa boundaries
  lsoa_boundary <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Full_Clipped_(BFC)_EW_V3.shp",
                           quiet = TRUE)

  stops_lsoa_summary <- inner_join(lsoa_boundary, stops_lsoa_summary, by = c("LSOA11CD" = "lsoa11cd"))

  # end timer and return output
  toc()
  return(stops_lsoa_summary)
}


# summarise stops by LSOA

make_lsoa_stop_summary <- function(gtfs,
                                   startdate,
                                   enddate) {

  stops_calendar <- stop_timetables(gtfs,
                                    startdate,
                                    enddate)

  stops_runs <- summarise_all_stop_data(gtfs,
                                        stops_calendar,
                                        startdate,
                                        enddate)

  stops_lsoa_summary <- summarise_stops_by_lsoa(stops_runs)

}
