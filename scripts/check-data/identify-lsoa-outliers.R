
# set up
rm(list = ls())
source("scripts/check-data/lsoa-trips-analysis-functions.R")

load_packages()
library(fpp2)
#library(zoo)
library(tsoutliers)

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
add_missing_years <- function(trip_lsoa_data) {

  # generate missing year data to have a complete time series
  lsoa_list <- unique(trip_lsoa_data$lsoa11)

  # generate complete set of lsoas and years
  missing_years <- data.frame(expand.grid(lsoa11 = lsoa_list,
                                          year = c(2004:2023)))
  # add to main data
  trip_lsoa_data <- left_join(missing_years, trip_lsoa_data, by = c("lsoa11", "year"))


}

extract_pandemic_year <- function(trip_lsoa_data) {

  pandemic_year_data <- trip_lsoa_data %>%
    filter(year == 2020) %>%
    transmute(lsoa11,
              year,
              runs_pandemic = runs)

}

clean_data_for_outlier_analysis <- function(trip_lsoa_data) {

  # remove pandemic year and arange by lsoa and year
  trip_lsoa_data <- trip_lsoa_data %>%
    mutate(runs = ifelse(year == 2020, NA, runs)) %>%
    arrange(lsoa11,
            year)

  # remove 2004 as so much data missing
  trip_lsoa_data <- trip_lsoa_data %>%
    filter(year != 2004)

  # remove really low values before 2020 as these are not removed by tsoutliers automatically
  trip_lsoa_data <- trip_lsoa_data %>%
    group_by(lsoa11) %>%
    mutate(runs_max_pct = runs / max(runs, na.rm = TRUE),
           runs_zscore = (runs - mean(runs, na.rm = TRUE)) / sd(runs, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(runs = ifelse(runs == 0 & is.nan(runs_max_pct),
                         0,
                         ifelse(year < 2020 & runs_zscore < -1, NA, runs)))
}

remove_lsoas_with_insufficient_datapoints <- function(trip_lsoa_data) {

  # remove lsoas where there is only 1 or two data points in the series
  lsoa_with_insufficient_datapoints <- trip_lsoa_data %>%
    filter(!is.na(runs)) %>%
    group_by(lsoa11) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n <= 3)

  lsoas_with_all_null_datapoints <- trip_lsoa_data %>%
    mutate(runs_na = ifelse(is.na(runs), 1, 0)) %>%
    group_by(lsoa11) %>%
    summarise(nas = sum(runs_na),
              n = n()) %>%
    ungroup() %>%
    mutate(na_pct = nas / n) %>%
    filter(na_pct == 1)

  trip_lsoa_data <- trip_lsoa_data %>%
    filter(!lsoa11 %in% lsoa_with_insufficient_datapoints$lsoa11) %>%
    filter(!lsoa11 %in% lsoas_with_all_null_datapoints$lsoa11)

}


run_outlier_function <- function(trip_lsoa_data) {

  tictoc::tic(msg = "Outliers identified")

  # make list of distinct lsoas with good data
  lsoa_list <- unique(trip_lsoa_data$lsoa11)
  # make cleaned data data set
  clean_ts_all <- data.frame()

  for(r in 1:length(lsoa_list)) {

    message(paste0("Running outlier analysis on ", lsoa_list[r]))

    # select raw data
    raw <- trip_lsoa_data %>%
      filter(lsoa11 == lsoa_list[r]) %>%
      select(year,
             runs)

    # turn into a ts object
    raw_ts <- ts(raw$runs,
                 start = 2005)

    #outliers <- data.frame(tsoutliers(raw_ts))
    #outliers <- outliers %>%
    #  mutate(year = 2004 + index)

    #p <- autoplot(tsclean(raw_ts), series="clean", color='red', lwd=0.9) +
    #  autolayer(raw_ts, series="original", color='gray', lwd=1) +
    #  geom_point(data = outliers,
    #             aes(x=year, y=replacements), col='blue') +
    #  labs(x = "year", y = "runs")
    #plot(p)

    # identify outliers and turn into a new data base
    new_clean_ts <- rownames_to_column(data.frame(lsoa11 = lsoa_list[r],
                                                  runs_cleaned = .preformat.ts(tsclean(raw_ts),
                                                                               calendar = TRUE)),
                                       "year")

    clean_ts_all <- bind_rows(clean_ts_all,
                              new_clean_ts)

  }

  clean_ts_all <- clean_ts_all %>%
    mutate(year = as.integer(year))

  tictoc::toc()

  return(clean_ts_all)

}



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
# # look to analyse trends over time
# assess_trip_trends <- function(trips_final) {
#
#   #Find max value in period 2006-2008
#   #Use value in 2019
#   #Use value in 2023
#
#   trips_trends <- trips_final %>%
#     filter(year %in% c(2006, 2007, 2008, 2019, 2023)) %>%
#     mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "2006-2008",
#                                  year == 2019 ~ "2019",
#                                  year == 2023 ~ "2023"))
#
#   # summarise runs_cleaned - so all points will have a value whether original, interpolated or outlier removed.
#   trips_trends <- trips_trends %>%
#     group_by(lsoa11,
#              year_band) %>%
#     summarise(runs_max = max(runs_cleaned, na.rm = TRUE)) %>%
#     ungroup() %>%
#     spread(key = year_band,
#            value = runs_max)
#
#   # calculate the trends between years.
#   trips_trends <- trips_trends %>%
#     mutate(trips_change_2008_2019 = `2019` - `2006-2008`,
#            trips_change_2008_2023 = `2023` - `2006-2008`,
#            trips_change_2019_2023 = `2023` - `2019`) %>%
#     mutate(trips_change_2008_2019_pct = trips_change_2008_2019 / `2006-2008`,
#            trips_change_2008_2023_pct = trips_change_2008_2023 / `2006-2008`,
#            trips_change_2019_2023_pct = trips_change_2019_2023 / `2019`)
#
#   # finalise the data set
#   trips_trends <- trips_trends %>%
#     select(lsoa11,
#            trips_2006_08 = `2006-2008`,
#            trips_2019 = `2019`,
#            trips_2023 = `2023`,
#            trips_change_2008_2019,
#            trips_change_2008_2019_pct,
#            trips_change_2008_2023,
#            trips_change_2008_2023_pct,
#            trips_change_2019_2023,
#            trips_change_2019_2023_pct)


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

period <- c("runs_weekday_Night")

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

all_trend_data <- data.frame()

for(p in periods) {

  period_trend_data <- find_lsoa_trip_trends(period = p)

  all_trend_data <- bind_rows(all_trend_data,
                              period_trend_data)

}


#  OLD TESTS AND WORKINGS -------------------------------------------------


lsoa_list <- unique(bustrips_lsoa_2004_2023$lsoa11)
raw <- bustrips_lsoa_2004_2023 %>%
  filter(lsoa11 == lsoa_list[5000]) %>%
  select(year,
         runs)

# turn into a ts object
raw_ts <- ts(raw$runs,
             start = 2005,
             end = 2023)

.preformat.ts(raw_ts,
              calendar = TRUE)

outliers <- data.frame(tsoutliers(raw_ts))
outliers <- outliers %>%
  mutate(year = 2004 + index)

p <- autoplot(tsclean(raw_ts), series="clean", color='red', lwd=0.9) +
  autolayer(raw_ts, series="original", color='gray', lwd=1) +
  geom_point(data = outliers,
             aes(x=year, y=replacements), col='blue') +
  labs(x = "year", y = "runs")
plot(p)


test <- bustrips_weekam_lsoa_2004_2023 %>%
  filter(lsoa11 == "E01003805")

qplot(x = test$year,
      y = test$runs,
      geom = "line")

cor(test$year,
    test$runs,
    use = "complete.obs")

model <- lm(runs ~ year, data = test)
summary(model)
plot(test$year, test$runs)
abline(a = coef(model)[1], b = coef(model)[2])
