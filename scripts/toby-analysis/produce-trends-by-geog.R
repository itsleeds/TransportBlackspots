
# set up
rm(list = ls())
# load scripts with relevant functions
source("scripts/toby-analysis/bus-trips-analysis-functions.R")
source("scripts/toby-analysis/onspd.R")
source("scripts/toby-analysis/la-amalgamation-2023.R")

# load packages
load_packages()

# quiten group summarise
options(dplyr.summarise.inform = FALSE)

# get onspd
onspd <- get_onspd(keep.only.current = TRUE)

#  functions to clean, process and find trends for trips in each l --------

# get data and filter for mode of transport and select a given time of day/week
get_mode_runs <- function(mode_no = 3, trips_col = "runs_weekday_Morning_Peak", geog) {

  # get full lsoa data set
  trips_2004_2023 <- load_trips_data(geog = geog,
                                     mode_no = 3)

  # remove any NA lsoas
  trips_2004_2023 <- trips_2004_2023 %>%
    filter(!is.na(zone_id))

  # select the time period of interest
  trips_2004_2023 <- trips_2004_2023 %>%
    select(zone_id,
           year,
           runs = all_of(trips_col))


}

# add missing years to time series
# (we will run a time-based outlier detection process so need a point in the time
#  series for each year, even if these are NAs - interpolation will estimate these values)
add_missing_years <- function(trips_data) {

  # generate missing year data to have a complete time series
  lsoa_list <- unique(trips_data$zone_id)

  # generate complete set of lsoas and years
  missing_years <- data.frame(expand.grid(zone_id = lsoa_list,
                                          year = c(2004:2023)))
  # add to main data
  trips_data <- left_join(missing_years, trips_data, by = c("zone_id", "year"))


}

# select out the data for the pandemic year
# if not this will be treated as an outlier, so we need to save these true values
# and add back in after outlier detection.
extract_pandemic_year <- function(trips_data) {

  pandemic_year_data <- trips_data %>%
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
clean_data_for_outlier_analysis <- function(trips_data) {

  # remove pandemic year and arrange by lsoa and year
  trips_data <- trips_data %>%
    mutate(runs = ifelse(year == 2020, NA, runs)) %>%
    arrange(zone_id,
            year)

  # remove 2004 as so much data missing that it is not useful time point
  trips_data <- trips_data %>%
    filter(year != 2004)

  # remove really low values before 2020 as these are not removed by tsoutliers automatically
  # this is done for all value with a zscore of less that 1, before 2020.
  # the logic being that the data after 2020 should be complete in the GTFS files we have, but
  # that services in some areas could have been reduced so that their zscore is less than 1 but this
  # is a legitimate value.
  trips_data <- trips_data %>%
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
remove_areas_with_insufficient_datapoints <- function(trips_data) {

  # find lsoas where there is only 3 or less data points in the series
  geog_with_insufficient_datapoints <- trips_data %>%
    filter(!is.na(runs)) %>%
    group_by(zone_id) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n <= 3)

  # find LSOAs which have all NA values
  geog_with_all_null_datapoints <- trips_data %>%
    mutate(runs_na = ifelse(is.na(runs), 1, 0)) %>%
    group_by(zone_id) %>%
    summarise(nas = sum(runs_na),
              n = n()) %>%
    ungroup() %>%
    mutate(na_pct = nas / n) %>%
    filter(na_pct == 1)

  # remove these lsoas: a) with 3 or less data points or will all NA values.
  trips_data <- trips_data %>%
    filter(!zone_id %in% geog_with_insufficient_datapoints$zone_id) %>%
    filter(!zone_id %in% geog_with_all_null_datapoints$zone_id)

}

# this function takes each lsoa in term and converts the years and runs into a
# time series which it then detects outliers and interpolates for NA values.
# this is then converted back into a data frame and appended onto the previous
# lsoa results, until all LSOAs in the time series have been processed
run_outlier_function <- function(trips_data, message_row_no) {

  # set timer using tictoc function
  tictoc::tic(msg = "Outliers identified")

  # make list of distinct lsoas with good data (having been cleaned as above)
  #lsoa_list <- unique(trips_data$zone_id)
  # make new empty data data set which will be populated by the loop below
  clean_ts_all <- list()

  trips_data <- trips_data %>%
    select(year, runs, zone_id) %>%
    dplyr::group_by(zone_id) %>%
    dplyr::group_split()

  # run a loop on the data for each lsoa, using the tsoutliers function
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
finalise_trip_data <- function(trips_data,
                               trips_data_cleaned,
                               trips_data_2020) {

  # combined original, cleaned and pandemic year data together
  trips_final <- inner_join(trips_data, trips_data_cleaned, by = c("year", "zone_id"))
  trips_final <- trips_final %>%
    filter(year != 2020)

  # remove any lsoas with pandemic 2020 data but that have been omitted from the main data set during cleaning
  trips_data_2020 <- trips_data_2020 %>%
    filter(zone_id %in% unique(trips_final$zone_id))

  # add pandemic year data to main data frame
  trips_final <- bind_rows(trips_final, trips_data_2020) %>%
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


# analyse trends over time for selected point in the time series.
# in this instance, we've used 2006-2008 (and found the maximum value within this time frame),
# plus a prepandemic point (2019) and the latest available data (2023)
assess_trip_trends <- function(trips_final) {

  #Find max value in period 2006-2008
  #Use value in 2019
  #Use value in 2023

  trips_trends <- trips_final %>%
    filter(year %in% c(2006, 2007, 2008, 2019, 2022, 2023)) %>%
    mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "2006-2008",
                                 year == 2019 ~ "2019",
                                 year == 2022 ~ "2022",
                                 year == 2023 ~ "2023"))

  # summarise runs_cleaned - so all points will have a value whether original, interpolated or outlier removed.
  trips_trends <- trips_trends %>%
    group_by(zone_id,
             year_band) %>%
    summarise(runs_max = max(runs_cleaned, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(key = year_band,
           value = runs_max)

  # calculate the trends between years.
  trips_trends <- trips_trends %>%
    mutate(trips_change_2008_2019 = `2019` - `2006-2008`,
           trips_change_2008_2022 = `2022` - `2006-2008`,
           trips_change_2008_2023 = `2023` - `2006-2008`,
           trips_change_2019_2023 = `2023` - `2019`,
           trips_change_2022_2023 = `2023` - `2022`,
           ) %>%
    mutate(trips_change_2008_2019_pct = trips_change_2008_2019 / `2006-2008`,
           trips_change_2008_2022_pct = trips_change_2008_2022 / `2006-2008`,
           trips_change_2008_2023_pct = trips_change_2008_2023 / `2006-2008`,
           trips_change_2019_2023_pct = trips_change_2019_2023 / `2019`,
           trips_change_2022_2023_pct = trips_change_2022_2023 / `2022`)

  # finalise the data set
  trips_trends <- trips_trends %>%
    select(zone_id,
           trips_2006_08 = `2006-2008`,
           trips_2019 = `2019`,
           trips_2022 = `2022`,
           trips_2023 = `2023`,
           trips_change_2008_2019,
           trips_change_2008_2022,
           trips_change_2008_2023,
           trips_change_2019_2023,
           trips_change_2022_2023,
           trips_change_2008_2019_pct,
           trips_change_2008_2022_pct,
           trips_change_2008_2023_pct,
           trips_change_2019_2023_pct,
           trips_change_2022_2023_pct)

}

# MAIN FUNCTION TO COMBINE ALL OTHER PROCESS FUNCTIONS ABOVE
find_trip_trends <- function(period,
                             geog,
                             message_row_no) {

  # get data (specify mode and period. default is bus)
  trips_2004_2023 <- get_mode_runs(mode_no = 3,
                                   trips_col = period,
                                   geog = geog)

  # add missing years for each lsoa (NAs added if year not included)
  trips_2004_2023 <- add_missing_years(trips_2004_2023)
  # filter out and keep pandemic year (2020) data
  trips_2020 <- extract_pandemic_year(trips_2004_2023)
  # clean data (manual removal of data points if below zscore of 1 and <2020)
  trips_2004_2023 <- clean_data_for_outlier_analysis(trips_2004_2023)
  # filter out any LSOAs with insufficient data points in time series, or that have all NA values.
  trips_2004_2023 <- remove_areas_with_insufficient_datapoints(trips_2004_2023)
  # run main function to produce cleaned, outlier removed, interpolated results.
  trips_2004_2023_cleaned <- run_outlier_function(trips_2004_2023, message_row_no)
  # combine all data sets to make a complete set of data
  trips_2004_2023_final <- finalise_trip_data(trips_2004_2023,
                                                   trips_2004_2023_cleaned,
                                                   trips_2020)
  # find trends between a select number of yearly intervals
  trips_2004_2023_trends <- assess_trip_trends(trips_2004_2023_final)
  # identify which time of day/week trip data has been used
  trips_2004_2023_trends <- trips_2004_2023_trends %>%
    mutate(period_name = period)

}

# RUN FUNCTIONS -----------------------------------------------------------

make_trend_data <- function(periods, geog, geog_name) {

  # create an empty list
  trend_data <- list()

  # run a loop that runs the main function above over each time of day/week in the list above
  # and combine into one list adding an new element of that list each time
  for(p in periods) {
    trend_data[[p]] <-  find_trip_trends(period = p,
                                         geog = geog,
                                         message_row_no = 100)
  }

  # bind all rows of list and thus create a data.frame from the list
  trend_data <- dplyr::bind_rows(trend_data)

  # remame zone id to lsoa
  trend_data <- trend_data %>%
    rename({{geog_name}} := zone_id)

}

make_lsoa_trend_data <- function() {

  # create an empty list
  lsoa_trend_data <- list()

  # run a loop that runs the main function above over each time of day/week in the list above
  # and combine into one list adding an new element of that list each time
  for(p in periods) {
    lsoa_trend_data[[p]] <-  find_trip_trends(period = p,
                                              geog = "lsoa",
                                              message_row_no = 10000)
  }

  # bind all rows of list and thus create a data.frame from the list
  lsoa_trend_data <- dplyr::bind_rows(lsoa_trend_data)

  # remame zone id to lsoa
  lsoa_trend_data <- lsoa_trend_data %>%
    rename(lsoa11 = zone_id)

}


add_lacode_region <- function(la_trend_data) {

  oslaua_to_rgn <- make_oslaua_to_rgn_lup(add_la_names = TRUE)

  la_trend_data <- left_join(la_trend_data, oslaua_to_rgn, by = "local_authority_name")

  inner_london <- c("Camden",
                    "Hackney",
                    "Hammersmith and Fulham",
                    "Islington",
                    "Kensington and Chelsea",
                    "Lambeth",
                    "Lewisham",
                    "Southwark",
                    "Tower Hamlets",
                    "Wandsworth",
                    "Westminster",
                    "City of London",
                    "Haringey",
                    "Newham")

  la_trend_data <- la_trend_data %>%
    mutate(london = ifelse(region_name == "London", "London", "Outside London")) %>%
    mutate(london = case_when(london == "London" & local_authority_name %in% inner_london ~ "Inner London",
                              london == "London" & !local_authority_name %in% inner_london ~ "Outer London",
                              TRUE ~ london)) %>%
    select(oslaua,
           local_authority_name,
           rgn,
           region_name,
           london,
           period_name,
           everything())
}



add_la_rural_urban_class <- function(la_trend_data) {

  la_rurality <- readODS::read_ods("data/rural-urban/Rural_Urban_Classification_2011_lookup_tables_for_local_authority_areas.ods",
                                   sheet = "LAD21",
                                   skip = 2)

  la_rurality <- la_rurality %>%
    select(oslaua = `Local Authority District Area 2021 Code`,
           local_authority_name = `Local Authority District Area 2021 Name`,
           rururb_6 = `Rural Urban Classification 2011 (6 fold)`,
           rururb_3 = `Rural Urban Classification 2011 (3 fold)`)

  la_rurality <- la_consolidation_april_2023(la_rurality)

  la_rurality <- la_rurality %>%
    group_by(oslaua,
             local_authority_name,
             rururb_3) %>%
    summarise(n = n()) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(rural_urban = case_when(rururb_3 == "Predominantly Urban" ~ "Urban",
                                   rururb_3 == "Predominantly Rural" ~ "Rural",
                                   rururb_3 == "Urban with Significant Rural" ~ "Urban")) %>%
    select(oslaua,
           rural_urban)

  # add to la_trend data set
  la_trend_data <- left_join(la_trend_data, la_rurality, by = "oslaua")

}

# list all times of days/weeks (these are column headings in "data/trips_per_lsoa_by_mode_2004_2023.Rds")
periods <- c(#"runs_weekday_Night",
             #"runs_weekday_Morning_Peak",
             #"runs_weekday_Afternoon_Peak",
             #"runs_weekday_Midday",
             #"runs_weekday_Evening",
             #"runs_Sat_Night",
             #"runs_Sat_Morning_Peak",
             #"runs_Sat_Midday",
             #"runs_Sat_Afternoon_Peak",
             #"runs_Sat_Evening",
             #"runs_Sun_Night",
             #"runs_Sun_Morning_Peak",
             #"runs_Sun_Midday",
             #"runs_Sun_Afternoon_Peak",
             #"runs_Sun_Evening"
             "tph_weekday_Night",
             "tph_weekday_Morning_Peak",
             "tph_weekday_Afternoon_Peak",
             "tph_weekday_Midday",
             "tph_weekday_Evening",
             "tph_Sat_Night",
             "tph_Sat_Morning_Peak",
             "tph_Sat_Midday",
             "tph_Sat_Afternoon_Peak",
             "tph_Sat_Evening",
             "tph_Sun_Night",
             "tph_Sun_Morning_Peak",
             "tph_Sun_Midday",
             "tph_Sun_Afternoon_Peak",
             "tph_Sun_Evening"
             )

la_trend_data <- make_trend_data(periods, geog = "la", geog_name = local_authority_name)
la_trend_data <- add_lacode_region(la_trend_data)
la_trend_data <- add_la_rural_urban_class(la_trend_data)
region_trend_data <- summarise_trend_data(la_trend_data, period_name, rgn, region_name)

la_trend_data_eng <- la_trend_data %>%
  filter(!region_name %in% c("Wales", "Scotland"))

london_trend_data <- summarise_trend_data(la_trend_data_eng, period_name, london, rural_urban)

lsoa_trend_data <- make_trend_data(periods, geog = "lsoa", geog_name = lsoa11)

# summarise by london and non-london averages for weekday_peaks (aka rush hour)
national_trend_data <- london_trend_data %>%
  mutate(rush_hour = ifelse(grepl("weekday_Morning_Peak|weekday_Afternoon_Peak", period_name), TRUE, FALSE)) %>%
  filter(rush_hour) %>%
  group_by(london) %>%
  summarise_trend_data(london, rural_urban)

national_trend_graph_data <- national_trend_data %>%
  unite(location, london, rural_urban, sep = ": ") %>%
  select(location,
         `2008` = trips_2006_08,
         `2019` = trips_2019,
         `2022` = trips_2022,
         `2023` = trips_2023) %>%
  gather(key = year,
         value = trips_per_hour,
         -location) %>%
  mutate(year = as.integer(year))

national_trend_graph_data <- national_trend_graph_data %>%
  group_by(location) %>%
  mutate(trips_per_hour_pctmax = trips_per_hour / trips_per_hour[year == 2008]) %>%
  ungroup()

ggplot(data = national_trend_graph_data, aes(x = year, y = trips_per_hour, col = location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  theme_bw() +
  ylab("Average number of trips per hour (rush hour)")

ggplot(data = national_trend_graph_data, aes(x = year, y = trips_per_hour_pctmax, col = location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  theme_bw() +
  xlab("Year") +
  ylab("Average number of trips per hour (rush hour)") +
  ylim(c(0,1.2))


# add rural classification to la.


# produce statistics for region and also for london/non-london
summarise_trend_data <- function(la_trend_data, ...) {

  region_trend_data <- la_trend_data %>%
    group_by(...) %>%
    summarise(trips_2006_08 = mean(trips_2006_08),
              trips_2019 = mean(trips_2019),
              trips_2022 = mean(trips_2022),
              trips_2023 = mean(trips_2023)) %>%
    ungroup() %>%

    mutate(trips_change_2008_2019 = trips_2019 - trips_2006_08,
           trips_change_2008_2022 = trips_2022 - trips_2006_08,
           trips_change_2008_2023 = trips_2023 - trips_2006_08,
           trips_change_2019_2023 = trips_2023 - trips_2019,
           trips_change_2022_2023 = trips_2023 - trips_2022) %>%

    mutate(trips_change_2008_2019_pct  = trips_change_2008_2019  / trips_2006_08,
           trips_change_2008_2022_pct = trips_change_2008_2022 / trips_2006_08,
           trips_change_2008_2023_pct  = trips_change_2008_2023  / trips_2006_08,
           trips_change_2019_2023_pct  = trips_change_2019_2023  / trips_2019,
           trips_change_2022_2023_pct = trips_change_2022_2023 / trips_2022,
    )

}

# SAVE OUTPUTS ------------------------------------------------------------



# save output of loop as an RDS file
saveRDS(object = all_trend_data,
        file = "data/lsoa_bustrip_trends_2008_2023.rds")


# SAVE DATA AT GPKG FOR ANALYSIS IN QGIS ----------------------------------

lsoa_bustrips_weekday_am_peak_trends <- all_trend_data %>%
  filter(period_name == "runs_weekday_Morning_Peak")

lsoa_boundaries <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/")
lsoa_boundaries <- lsoa_boundaries %>%
  select(OBJECTID,
         lsoa11 = LSOA11CD,
         LSOA11NM)

lsoa_bustrips_weekday_am_peak_trends <- left_join(lsoa_boundaries, lsoa_bustrips_weekday_am_peak_trends, by = "lsoa11")

st_write(obj = lsoa_bustrips_weekday_am_peak_trends,
         dsn = "../gis-data/transport/busstops.gpkg",
         layer = "lsoa-bustrips-weekday-am-trends",
         delete.layer = TRUE)


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
