
# set up
rm(list = ls())
source("scripts/check-data/lsoa-trips-analysis-functions.R")

load_packages()
library(fpp2)
#library(zoo)
library(tsoutliers)


#  functions to clean, process and find trends for trips in each l --------

# get data and filter for mode of transport and select a given time of day/week
get_lsoa_mode_runs <- function(mode_no = 3, trips_col = "runs_weekday_Morning_Peak") {

  # get full lsoa data set
  runs_mode_lsoa_2004_2023 <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")

  # filter for a mode of transport type
  runs_mode_lsoa_2004_2023 <- runs_mode_lsoa_2004_2023 %>%
    filter(route_type == mode_no)

  # remove any NA lsoas
  runs_mode_lsoa_2004_2023 <- runs_mode_lsoa_2004_2023 %>%
    filter(!is.na(zone_id))

  # select the time period of interest
  runs_mode_lsoa_2004_2023 <- runs_mode_lsoa_2004_2023 %>%
    select(lsoa11 = zone_id,
           year,
           runs = all_of(trips_col))


}

# add missing years to time series
# (we will run a time-based outlier detection process so need a point in the time
#  series for each year, even if these are NAs - interpolation will estimate these values)
add_missing_years <- function(trip_lsoa_data) {

  # generate missing year data to have a complete time series
  lsoa_list <- unique(trip_lsoa_data$lsoa11)

  # generate complete set of lsoas and years
  missing_years <- data.frame(expand.grid(lsoa11 = lsoa_list,
                                          year = c(2004:2023)))
  # add to main data
  trip_lsoa_data <- left_join(missing_years, trip_lsoa_data, by = c("lsoa11", "year"))


}

# select out the data for the pandemic year
# if not this will be treated as an outlier, so we need to save these true values
# and add back in after outlier detection.
extract_pandemic_year <- function(trip_lsoa_data) {

  pandemic_year_data <- trip_lsoa_data %>%
    filter(year == 2020) %>%
    transmute(lsoa11,
              year,
              runs_pandemic = runs)

}

# clean the data.
# some clearly incomplete data is not treated as an outlier, even though they clearly are.
# this is most noticeable for the early yearsp (pre-2008). So using z-score and some manual
# cleaning logic, we will set some of these values to NAs. the tsoutlier function will then
# interpolate these values.
clean_data_for_outlier_analysis <- function(trip_lsoa_data) {

  # remove pandemic year and arrange by lsoa and year
  trip_lsoa_data <- trip_lsoa_data %>%
    mutate(runs = ifelse(year == 2020, NA, runs)) %>%
    arrange(lsoa11,
            year)

  # remove 2004 as so much data missing that it is not useful time point
  trip_lsoa_data <- trip_lsoa_data %>%
    filter(year != 2004)

  # remove really low values before 2020 as these are not removed by tsoutliers automatically
  # this is done for all value with a zscore of less that 1, before 2020.
  # the logic being that the data after 2020 should be complete in the GTFS files we have, but
  # that services in some areas could have been reduced so that their zscore is less than 1 but this
  # is a legitimate value.
  trip_lsoa_data <- trip_lsoa_data %>%
    group_by(lsoa11) %>%
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
remove_lsoas_with_insufficient_datapoints <- function(trip_lsoa_data) {

  # find lsoas where there is only 3 or less data points in the series
  lsoa_with_insufficient_datapoints <- trip_lsoa_data %>%
    filter(!is.na(runs)) %>%
    group_by(lsoa11) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n <= 3)

  # find LSOAs which have all NA values
  lsoas_with_all_null_datapoints <- trip_lsoa_data %>%
    mutate(runs_na = ifelse(is.na(runs), 1, 0)) %>%
    group_by(lsoa11) %>%
    summarise(nas = sum(runs_na),
              n = n()) %>%
    ungroup() %>%
    mutate(na_pct = nas / n) %>%
    filter(na_pct == 1)

  # remove these lsoas: a) with 3 or less data points or will all NA values.
  trip_lsoa_data <- trip_lsoa_data %>%
    filter(!lsoa11 %in% lsoa_with_insufficient_datapoints$lsoa11) %>%
    filter(!lsoa11 %in% lsoas_with_all_null_datapoints$lsoa11)

}

# this function takes each lsoa in term and converts the years and runs into a
# time series which it then detects outliers and interpolates for NA values.
# this is then converted back into a data frame and appended onto the previous
# lsoa results, until all LSOAs in the time series have been processed
run_outlier_function <- function(trip_lsoa_data) {

  # set timer using tictoc function
  tictoc::tic(msg = "Outliers identified")

  # make list of distinct lsoas with good data (having been cleaned as above)
  #lsoa_list <- unique(trip_lsoa_data$lsoa11)
  # make new empty data data set which will be populated by the loop below
  clean_ts_all <- list()

  trip_lsoa_data <- trip_lsoa_data %>%
    select(year, runs, lsoa11) %>%
    dplyr::group_by(lsoa11) %>%
    dplyr::group_split()

  # run a loop on the data for each lsoa, using the tsoutliers function
  for(r in 1:length(trip_lsoa_data)) {

    # print progress (which helps identifying location of data errors given by tsoutliers)
    if(r %% 10000 == 0){
      message(paste0(Sys.time()," Running outlier analysis on ", r))
    }

    # select 'raw' runs data
    raw <- trip_lsoa_data[[r]]

    # turn into a ts object
    raw_ts <- ts(raw$runs,
                 start = 2005)

    # identify outliers and turn into a new data frame
    # .preformat.ts turns the time series into a matrix, which is then turned into a data frame,
    # using the rownames (which is the calendar values from .preformat.ts) as a column field.
    new_clean_ts <- rownames_to_column(data.frame(lsoa11 = raw$lsoa11[1],
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
finalise_trip_data <- function(trip_lsoa_data,
                               trip_lsoa_data_cleaned,
                               trip_lsoa_2020) {

  # combined original, cleaned and pandemic year data together
  trips_final <- inner_join(trip_lsoa_data, trip_lsoa_data_cleaned, by = c("year", "lsoa11"))
  trips_final <- trips_final %>%
    filter(year != 2020)

  # remove any lsoas with pandemic 2020 data but that have been omitted from the main data set during cleaning
  trip_lsoa_2020 <- trip_lsoa_2020 %>%
    filter(lsoa11 %in% unique(trips_final$lsoa11))

  # add pandemic year data to main data frame
  trips_final <- bind_rows(trips_final, trip_lsoa_2020) %>%
    arrange(lsoa11, year)

  # make note of which points have outliers and interpolated results
  trips_final <- trips_final %>%
    mutate(runs = ifelse(year == 2020, runs_pandemic, runs)) %>%
    mutate(runs_cleaned = ifelse(year == 2020, runs_pandemic, runs_cleaned)) %>%
    mutate(outlier = runs != runs_cleaned) %>%
    mutate(outlier = ifelse(is.na(outlier), FALSE, outlier)) %>%
    mutate(interpolation = is.na(runs) & !is.na(runs_cleaned))

  # select final set of data
  trips_final <- trips_final %>%
    select(lsoa11,
           year,
           runs,
           runs_cleaned,
           outlier,
           interpolation)

}


# analyse trends over time for selected point in the time series.
# in this instance, we've used 2006-2008 (and found the maximum value within this time frame),
# plus a prepandemic point (2019) and the latest available data (2023)
assess_trip_trends <- function(trips_final) {

  #Find max value in period 2006-2008
  #Use value in 2019
  #Use value in 2023

  trips_trends <- trips_final %>%
    filter(year %in% c(2006, 2007, 2008, 2019, 2023)) %>%
    mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "2006-2008",
                                 year == 2019 ~ "2019",
                                 year == 2023 ~ "2023"))

  # summarise runs_cleaned - so all points will have a value whether original, interpolated or outlier removed.
  trips_trends <- trips_trends %>%
    group_by(lsoa11,
             year_band) %>%
    summarise(runs_max = max(runs_cleaned, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(key = year_band,
           value = runs_max)

  # calculate the trends between years.
  trips_trends <- trips_trends %>%
    mutate(trips_change_2008_2019 = `2019` - `2006-2008`,
           trips_change_2008_2023 = `2023` - `2006-2008`,
           trips_change_2019_2023 = `2023` - `2019`) %>%
    mutate(trips_change_2008_2019_pct = trips_change_2008_2019 / `2006-2008`,
           trips_change_2008_2023_pct = trips_change_2008_2023 / `2006-2008`,
           trips_change_2019_2023_pct = trips_change_2019_2023 / `2019`)

  # finalise the data set
  trips_trends <- trips_trends %>%
    select(lsoa11,
           trips_2006_08 = `2006-2008`,
           trips_2019 = `2019`,
           trips_2023 = `2023`,
           trips_change_2008_2019,
           trips_change_2008_2019_pct,
           trips_change_2008_2023,
           trips_change_2008_2023_pct,
           trips_change_2019_2023,
           trips_change_2019_2023_pct)

}

# MAIN FUNCTION TO COMBINE ALL OTHER PROCESS FUNCTIONS ABOVE
find_lsoa_trip_trends <- function(period) {

  trips_lsoa_2004_2023 <- get_lsoa_mode_runs(mode_no = 3,
                                                trips_col = period)
  trips_lsoa_2004_2023 <- add_missing_years(trips_lsoa_2004_2023)
  trips_lsoa_2020 <- extract_pandemic_year(trips_lsoa_2004_2023)
  trips_lsoa_2004_2023 <- clean_data_for_outlier_analysis(trips_lsoa_2004_2023)
  trips_lsoa_2004_2023 <- remove_lsoas_with_insufficient_datapoints(trips_lsoa_2004_2023)

  trips_lsoa_2004_2023_cleaned <- run_outlier_function(trips_lsoa_2004_2023)

  trips_lsoa_2004_2023_final <- finalise_trip_data(trips_lsoa_2004_2023,
                                                   trips_lsoa_2004_2023_cleaned,
                                                   trips_lsoa_2020)

  trips_lsoa_2004_2023_trends <- assess_trip_trends(trips_lsoa_2004_2023_final)

  trips_lsoa_2004_2023_trends <- trips_lsoa_2004_2023_trends %>%
    mutate(period_name = period)

}

periods <- c("runs_weekday_Night",
             "runs_weekday_Morning_Peak",
             "runs_weekday_Afternoon_Peak",
             "runs_weekday_Midday",
             "runs_weekday_Evening",
             "runs_Sat_Night",
             "runs_Sat_Morning_Peak",
             "runs_Sat_Midday",
             "runs_Sat_Afternoon_Peak",
             "runs_Sat_Evening",
             "runs_Sun_Night",
             "runs_Sun_Morning_Peak",
             "runs_Sun_Midday",
             "runs_Sun_Afternoon_Peak",
             "runs_Sun_Evening")

all_trend_data <- list()

for(p in periods) {

  all_trend_data[[p]] <-  find_lsoa_trip_trends(period = p)

}

all_trend_data <- dplyr::bind_rows(all_trend_data)

saveRDS(object = all_trend_data,
        file = "data/lsoa_bustrip_trends_2008_2023.rds")

#  OLD TESTS AND WORKINGS -------------------------------------------------

#
#
# bustrips_lsoa_2004_2023 <- get_lsoa_mode_runs(mode_no = 3,
#                                               trips_col = "runs_weekday_Morning_Peak")
# bustrips_lsoa_2004_2023 <- add_missing_years(bustrips_lsoa_2004_2023)
# bustrips_lsoa_2020 <- extract_pandemic_year(bustrips_lsoa_2004_2023)
# bustrips_lsoa_2004_2023 <- clean_data_for_outlier_analysis(bustrips_lsoa_2004_2023)
# bustrips_lsoa_2004_2023 <- remove_lsoas_with_insufficient_datapoints(bustrips_lsoa_2004_2023)
#
# bustrips_lsoa_2004_2023_cleaned <- run_outlier_function(bustrips_lsoa_2004_2023)
#
# bustrips_lsoa_2004_2023_final <- finalise_trip_data(bustrips_lsoa_2004_2023,
#                                                     bustrips_lsoa_2004_2023_cleaned,
#                                                     bustrips_lsoa_2020)



# lsoa_list <- unique(bustrips_lsoa_2004_2023$lsoa11)
# raw <- bustrips_lsoa_2004_2023 %>%
#   filter(lsoa11 == lsoa_list[5000]) %>%
#   select(year,
#          runs)
#
# # turn into a ts object
# raw_ts <- ts(raw$runs,
#              start = 2005,
#              end = 2023)
#
# .preformat.ts(raw_ts,
#               calendar = TRUE)
#
# outliers <- data.frame(tsoutliers(raw_ts))
# outliers <- outliers %>%
#   mutate(year = 2004 + index)
#
# p <- autoplot(tsclean(raw_ts), series="clean", color='red', lwd=0.9) +
#   autolayer(raw_ts, series="original", color='gray', lwd=1) +
#   geom_point(data = outliers,
#              aes(x=year, y=replacements), col='blue') +
#   labs(x = "year", y = "runs")
# plot(p)
#
#
# test <- bustrips_weekam_lsoa_2004_2023 %>%
#   filter(lsoa11 == "E01003805")
#
# qplot(x = test$year,
#       y = test$runs,
#       geom = "line")
#
# cor(test$year,
#     test$runs,
#     use = "complete.obs")
#
# model <- lm(runs ~ year, data = test)
# summary(model)
# plot(test$year, test$runs)
# abline(a = coef(model)[1], b = coef(model)[2])
