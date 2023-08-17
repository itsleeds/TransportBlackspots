#' --------------------------------------------------------------------------------
#'
#' logic of processing:
#' For each time of day/week series:
#'  1. make sure each LSOA has a complete number of years in the data set, even if
#'     these are NA values. This is needed for treating each value of a time series
#'
#'  2. remove pandemic year (assume the data is complete for this, but that it is a
#'     natural and to be kept 'outlier')
#'
#'  3. perform some basic clean that sets any obviously missing data to NAs as
#'     provisional clean.
#'
#'  4. remove LSOAs that don't have enough data points to conduct an outlier analysis
#'
#'  5. Run outlier function,
#'
#'  6. then combine 2020 and cleaned data with original and identify source of data
#'     (i.e. is it an outlier/interpolated
#'
#'     )
#' --------------------------------------------------------------------------------

# add missing years to time series
# (we will run a time-based outlier detection process so need a point in the time
#  series for each year, even if these are NAs - interpolation will estimate these values)
add_missing_years <- function(trip_data_period) {

  # generate missing year data to have a complete time series
  area_list <- unique(trip_data_period$zone_id)

  # generate complete set of lsoas and years
  missing_years <- data.frame(expand.grid(zone_id = area_list,
                                          year = c(2004:2023)))
  # add to main data
  trip_data_period <- left_join(missing_years, trip_data_period, by = c("zone_id", "year"))


}

# select out the data for the pandemic year
# if not this will be treated as an outlier, so we need to save these true values
# and add back in after outlier detection.
extract_pandemic_year <- function(trip_data_period) {

  pandemic_year_data <- trip_data_period %>%
    filter(year == 2020) %>%
    transmute(zone_id,
              year,
              runs_pandemic = runs)

}

# clean the data.
# some clearly incomplete data is not treated as an outlier, even though they clearly are.
# this is most noticeable for the early yearsp (pre-2008). So using z-score and some manual
# cleaning logic, we will set some of these values to NAs. the tsoutlier function will then
# interpolate these values.
clean_data_for_outlier_analysis <- function(trip_data_period) {

  # remove pandemic year and arrange by lsoa and year
  trip_data_period <- trip_data_period %>%
    mutate(runs = ifelse(year == 2020, NA, runs)) %>%
    arrange(zone_id,
            year)

  # remove 2004 as so much data missing that it is not useful time point
  trip_data_period <- trip_data_period %>%
    filter(year != 2004)

  # remove really low values before 2020 as these are not removed by tsoutliers automatically
  # this is done for all value with a zscore of less that 1, before 2020.
  # the logic being that the data after 2020 should be complete in the GTFS files we have, but
  # that services in some areas could have been reduced so that their zscore is less than 1 but this
  # is a legitimate value.
  trip_data_period <- trip_data_period %>%
    group_by(zone_id) %>%
    mutate(runs_max_pct = runs / max(runs, na.rm = TRUE),
           runs_zscore = (runs - mean(runs, na.rm = TRUE)) / sd(runs, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(runs = ifelse(runs == 0 & is.nan(runs_max_pct),
                         0,
                         ifelse(year < 2020 & runs_zscore < -1, NA, runs)))
}

# some LSOAs will not have sufficient data points in order to detect outliers or even to be used for
# an overall time series and trend.
# tsoutliers is unhappy when there is only 1 data point. It also seems unreliable to keep LSOAs that
# have less than 4 data points between 2005-2023, so these are removed from the analysis.
remove_areas_with_insufficient_datapoints <- function(trip_data_period) {

  # find lsoas where there is only 3 or less data points in the series
  areas_with_insufficient_datapoints <- trip_data_period %>%
    filter(!is.na(runs)) %>%
    group_by(zone_id) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n <= 3)

  # find LSOAs which have all NA values
  areas_with_all_null_datapoints <- trip_data_period %>%
    mutate(runs_na = ifelse(is.na(runs), 1, 0)) %>%
    group_by(zone_id) %>%
    summarise(nas = sum(runs_na),
              n = n()) %>%
    ungroup() %>%
    mutate(na_pct = nas / n) %>%
    filter(na_pct == 1)

  # remove these lsoas: a) with 3 or less data points or will all NA values.
  trip_data_period <- trip_data_period %>%
    filter(!zone_id %in% areas_with_insufficient_datapoints$zone_id) %>%
    filter(!zone_id %in% areas_with_all_null_datapoints$zone_id)

}

# this function takes each area in term and converts the years and runs into a
# time series which it then detects outliers and interpolates for NA values.
# this is then converted back into a data frame and appended onto the previous
# area results, until all areas in the time series have been processed
run_outlier_function <- function(trips_data, message_row_no) {

  # set timer using tictoc function
  tictoc::tic(msg = "Outliers identified")

  # make list of distinct areas with good data (having been cleaned as above)
  #area_list <- unique(trips_data$zone_id)
  # make new empty data data set which will be populated by the loop below
  clean_ts_all <- list()

  trips_data <- trips_data %>%
    select(year, runs, zone_id) %>%
    dplyr::group_by(zone_id) %>%
    dplyr::group_split()

  # run a loop on the data for each area, using the tsoutliers function
  for(r in 1:length(trips_data)) {

    # print progress (which helps identifying location of data errors given by tsoutliers)
    if(r %% message_row_no == 0){
      message(paste0(Sys.time()," Running outlier analysis on ", r))
    }

    # select 'raw' runs data
    raw <- trips_data[[r]]

    # turn into a ts object
    raw_ts <- ts(raw$runs,
                 start = 2005)

    # identify outliers and turn into a new data frame
    # .preformat.ts turns the time series into a matrix, which is then turned into a data frame,
    # using the rownames (which is the calendar values from .preformat.ts) as a column field.
    new_clean_ts <- rownames_to_column(data.frame(zone_id = raw$zone_id[1],
                                                  runs_cleaned = .preformat.ts(tsclean(raw_ts),
                                                                               calendar = TRUE)),
                                       "year")

    # append this to the previous results... and repeat till complete!
    clean_ts_all[[r]] <- new_clean_ts

  }

  clean_ts_all <- dplyr::bind_rows(clean_ts_all)

  # convert year into an integer value
  clean_ts_all <- clean_ts_all %>%
    mutate(year = as.integer(year))

  # stop timer
  tictoc::toc()

  # return finished data set
  return(clean_ts_all)

}

# function to combine all the data (original, 2020 values and cleaned/outliers removed)
finalise_trip_data <- function(trip_data_period,
                               trip_data_period_cleaned,
                               trip_data_2020) {

  # combined original, cleaned and pandemic year data together
  trips_final <- inner_join(trip_data_period, trip_data_period_cleaned, by = c("year", "zone_id"))
  trips_final <- trips_final %>%
    filter(year != 2020)

  # remove any areas with pandemic 2020 data but that have been omitted from the main data set during cleaning
  trip_data_2020 <- trip_data_2020 %>%
    filter(zone_id %in% unique(trips_final$zone_id))

  # add pandemic year data to main data frame
  trips_final <- bind_rows(trips_final, trip_data_2020) %>%
    arrange(zone_id, year)

  # make note of which points have outliers and interpolated results
  trips_final <- trips_final %>%
    mutate(runs = ifelse(year == 2020, runs_pandemic, runs)) %>%
    mutate(runs_cleaned = ifelse(year == 2020, runs_pandemic, runs_cleaned)) %>%
    mutate(outlier = runs != runs_cleaned) %>%
    mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>%
    mutate(interpolation = is.na(runs) & !is.na(runs_cleaned))

  # select final set of data
  trips_final <- trips_final %>%
    select(zone_id,
           year,
           runs,
           runs_cleaned,
           outlier,
           interpolation)

}

clean_trips_data_for_each_period <- function(trips_data,
                                             period,
                                             message_row_no) {

  trip_data_period <- trips_data %>%
    select(zone_id,
            year,
            runs = period)


    # add missing years for each area (NAs added if year not included)
    trip_data_period <- add_missing_years(trip_data_period)
    # filter out and keep pandemic year (2020) data
    trip_data_period_2020 <- extract_pandemic_year(trip_data_period)
    # clean data (manual removal of data points if below zscore of 1 and <2020)
    trip_data_period <- clean_data_for_outlier_analysis(trip_data_period)
    # filter out any areas with insufficient data points in time series, or that have all NA values.
    trip_data_period <- remove_areas_with_insufficient_datapoints(trip_data_period)
    # run main function to produce cleaned, outlier removed, interpolated results.
    trip_data_period_cleaned <- run_outlier_function(trip_data_period, message_row_no)
    # combine all data sets to make a complete set of data
    trip_data_period_final <- finalise_trip_data(trip_data_period,
                                                 trip_data_period_cleaned,
                                                 trip_data_period_2020)
    # identify which time of day/week trip data has been used
    trip_data_period_final <- trip_data_period_final %>%
       mutate(period_name = period)

}

clean_all_trips_data <- function(trips_data,
                                 periods,
                                 message_n_rows = 100,
                                 gss_name) {

  old_names <- names(trips_data)
  new_names <- gsub(" ", "_", old_names)
  colnames(trips_data) <- new_names

  # create an empty list
  cleaned_trips_data <- list()

  # run a loop that runs the main function above over each time of day/week in the list above
  # and combine into one list adding an new element of that list each time
  for(p in periods) {
    cleaned_trips_data[[p]] <-  clean_trips_data_for_each_period(trips_data,
                                                                 period = p,
                                                                 message_row_no = message_n_rows)
  }

  # bind all rows of list and thus create a data.frame from the list
  cleaned_trips_data <- dplyr::bind_rows(cleaned_trips_data)

  # remame zone id to lsoa
  cleaned_trips_data <- cleaned_trips_data %>%
    rename({{ gss_name }} := zone_id)

  # if any values below zero, set to zero
  cleaned_trips_data <- cleaned_trips_data %>%
    mutate(runs = ifelse(runs < 0, 0, runs),
           runs_cleaned = ifelse(runs_cleaned < 0, 0, runs_cleaned))

}

make_clean_lsoa_bustrips_data <- function() {

  # get all bus trip data by lsoa
  lsoa_bustrips <- load_lsoa_bustrips()

  # define list of fields to analyse
  periods <- c("tph_weekday_Morning_Peak",
               "tph_weekday_Midday",
               "tph_weekday_Afternoon_Peak",
               "tph_weekday_Evening",
               "tph_weekday_Night",
               "tph_Sat_Morning_Peak",
               "tph_Sat_Midday",
               "tph_Sat_Afternoon_Peak",
               "tph_Sat_Evening",
               "tph_Sat_Night",
               "tph_Sun_Morning_Peak",
               "tph_Sun_Midday",
               "tph_Sun_Afternoon_Peak",
               "tph_Sun_Evening",
               "tph_Sun_Night",
               "tph_daytime_avg")

  # clean data to remove all outliers and missing data (interpolate)
  cleaned_bustrips_lsoa <- clean_all_trips_data(lsoa_bustrips,
                                                periods,
                                                message_n_rows = 10000,
                                                gss_name = lsoa11)

  saveRDS(cleaned_bustrips_lsoa,
          "data/bustrips_lsoa_2004_2008_cleaned.rds")

  # make wide table
  cleaned_bustrips_lsoa_wide <- cleaned_bustrips_lsoa %>%
    #filter(!is.na(runs_cleaned)) %>%
    select(lsoa11,
           period_name,
           year,
           runs_cleaned) %>%
    spread(key = period_name,
           value = runs_cleaned)

  # identify metro/london underground lsoa and lsoa
  metro_lsoas <- lsoa_bustrips %>%
    rename(region_name = RGN11NM) %>%
    mutate(london_tube = case_when(region_name == "London" & london_underground ~ "London: On tube",
                                   region_name == "London" & !london_underground ~ "London: Off tube",
                                   TRUE ~ "Outside London")) %>%
    distinct(lsoa11 = zone_id,
             region_name,
             london_tube)

  cleaned_bustrips_lsoa_wide <- left_join(cleaned_bustrips_lsoa_wide, metro_lsoas, by = "lsoa11")

  saveRDS(cleaned_bustrips_lsoa_wide,
          "data/bustrips_lsoa_2004_2008_cleaned_wide.rds")

  cleaned_bustrips_lsoa_wide_years <- cleaned_bustrips_lsoa %>%
    #filter(!is.na(runs_cleaned)) %>%
    select(lsoa11,
           period_name,
           year,
           runs_cleaned) %>%
    spread(key = year,
           value = runs_cleaned)

  cleaned_bustrips_lsoa_wide_years <- left_join(cleaned_bustrips_lsoa_wide_years, metro_lsoas, by = "lsoa11")

  saveRDS(cleaned_bustrips_lsoa_wide_years,
          "data/bustrips_lsoa_2004_2008_cleaned_wide_years.rds")

}

# BIVARIATE MAP DATA
# find average of 2006 to 2008 and compare with 2023
simplify_tph_trends <- function(cleaned_bustrips_lsoa_wide_years) {

  bustrips_trends <- cleaned_bustrips_lsoa_wide_years %>%
    transmute(lsoa11,
              london_tube,
              region_name,
              period_name,
              tph_2006_08 = (`2006` + `2007` + `2008`) / 3,
              tph_2023 = `2023`) %>%
    mutate(tph_2023 = ifelse(tph_2023 < 0, 0, tph_2023)) %>%
    mutate(tph_2006_2023_change = tph_2023 - tph_2006_08) %>%
    mutate(tph_2006_2023_change_pct = tph_2006_2023_change / tph_2006_08)

  # for those values with 0 services in 2006-08 and some in 2023, set these to pct = 2 to represent an increase
  bustrips_trends <- bustrips_trends %>%
    mutate(tph_2006_2023_change_pct = ifelse(tph_2006_08 == 0 & is.infinite(tph_2006_2023_change_pct),
                                             2,
                                             tph_2006_2023_change_pct))

  # for those values with 0 services in 2006-08 and 0 in 2023, set these to pct = 0 to represent not change
  # Otherwise it comes out as NaN
  bustrips_trends <- bustrips_trends %>%
    mutate(tph_2006_2023_change_pct = ifelse(tph_2006_08 == 0 & is.nan(tph_2006_2023_change_pct),
                                             0,
                                             tph_2006_2023_change_pct))

}


#  local authority cleaning data functions --------------------------------

make_clean_la_bustrips_data <- function() {

  # get all bus trip data by lsoa
  la_bustrips <- load_la_bustrips()

  # define list of fields to analyse
  periods <- c("tph_weekday_Morning_Peak",
               "tph_weekday_Midday",
               "tph_weekday_Afternoon_Peak",
               "tph_weekday_Evening",
               "tph_weekday_Night",
               "tph_Sat_Morning_Peak",
               "tph_Sat_Midday",
               "tph_Sat_Afternoon_Peak",
               "tph_Sat_Evening",
               "tph_Sat_Night",
               "tph_Sun_Morning_Peak",
               "tph_Sun_Midday",
               "tph_Sun_Afternoon_Peak",
               "tph_Sun_Evening",
               "tph_Sun_Night")

  # clean data to remove all outliers and missing data (interpolate)
  cleaned_bustrips_la <- clean_all_trips_data(la_bustrips,
                                              periods,
                                              message_n_rows = 100,
                                              gss_name = LAD23NM)

  cleaned_bustrips_la <- cleaned_bustrips_la %>%
    mutate(runs = ifelse(runs < 0, 0, runs),
           runs_cleaned = ifelse(runs_cleaned < 0, 0, runs_cleaned))


}



