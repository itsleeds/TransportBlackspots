#' Count the number of week days between two dates
#'
#'
#' @param cal GTFS calendar
#'
#' @return a GTFS calenadr data frame with addtional columms e.g. "runs_monday"
#'
#' @export
count_weekday_runs <- function(cal){
  cal$TMP_d <- as.integer(cal$end_date - cal$start_date) + 1
  cal$TMP_d[is.na(cal$TMP_d)] <- 0

  dow = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  res <- purrr::map2(cal$start_date,cal$TMP_d, function(startdate, d){
    dys <- weekdays(seq(startdate, length.out=d, by=1))
    dys <- as.data.frame.matrix(t(table(dys)))
    if(ncol(dys) < 7){
      dysmiss <- dow[!dow %in% names(dys)]
      dysmiss2 <- rep(0, length(dysmiss))
      names(dysmiss2) <- dysmiss
      dysmiss2 <- data.frame(as.list(dysmiss2))
      dys <- cbind(dys, dysmiss2)
    }
    dys <- dys[,dow]
  })

  res <- dplyr::bind_rows(res)
  names(res) <- paste0("n_",dow)
  cal <- cbind(cal, res)

  cal$runs_monday <- cal$monday * cal$n_Monday
  cal$runs_tuesday <- cal$tuesday * cal$n_Tuesday
  cal$runs_wednesday <- cal$wednesday * cal$n_Wednesday
  cal$runs_thursday <- cal$thursday * cal$n_Thursday
  cal$runs_friday <- cal$friday * cal$n_Friday
  cal$runs_saturday <- cal$saturday * cal$n_Saturday
  cal$runs_sunday <- cal$sunday * cal$n_Sunday

  cal <- cal %>%
    mutate(runs_weekdays = runs_monday + runs_tuesday + runs_wednesday + runs_thursday + runs_friday)

  cal <- cal[,c("service_id",
                "monday","tuesday","wednesday","thursday","friday",
                "saturday","sunday","start_date","end_date",
                "runs_monday","runs_tuesday","runs_wednesday", "runs_thursday",
                "runs_friday","runs_saturday","runs_sunday", "runs_weekdays")]
  return(cal)

}



#' Count the number of trips spotting at each stop between two dates#'
#'
#' @param gtfs GTFS object from gtfs_read()
#' @param startdate Start date
#' @param enddate End date
#'
#' @export
gtfs_stop_frequency <- function(gtfs,
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

  #summary(calendar$end_date >= calendar$start_date)

  calendar_days <- calendar_days[calendar_days$service_id %in% calendar$service_id,]
  calendar_days <- calendar_days[calendar_days$date >= startdate,]
  calendar_days <- calendar_days[calendar_days$date <= enddate,]


  calendar_days <- calendar_days %>%
    dplyr::group_by(service_id) %>%
    dplyr::summarise(runs_extra = sum(exception_type == 1),
                     runs_canceled = sum(exception_type == 2))

  trips <- trips[trips$service_id %in% calendar$service_id, ]
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  message("Counting trips on each day")
  calendar <- count_weekday_runs(calendar)

  # work out how many times the trip in run
  trips <- dplyr::left_join(trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_days, by = "service_id")

  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0



  message("Summarising results")
  trips$runs_days <- trips$runs_monday + trips$runs_tuesday +
    trips$runs_wednesday + trips$runs_thursday + trips$runs_friday +
    trips$runs_saturday + trips$runs_sunday

  trips$runs_total <-  trips$runs_days + trips$runs_extra - trips$runs_canceled

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
