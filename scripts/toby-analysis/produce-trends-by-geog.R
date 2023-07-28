
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
# get lsoa to LA and Region lookups
lsoa_to_la_lup <- make_lsoa_to_oslaua_lup()
la_to_rgn_lup <- make_oslaua_to_rgn_lup(add_la_names = TRUE)


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

make_lsoa_trend_data <- function(periods) {

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

identify_inner_outer_london <- function(trend_data) {

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

  trend_data <- trend_data %>%
    mutate(london = ifelse(region_name == "London", "London", "Outside London")) %>%
    mutate(london = case_when(london == "London" & local_authority_name %in% inner_london ~ "Inner London",
                              london == "London" & !local_authority_name %in% inner_london ~ "Outer London",
                              TRUE ~ london))

}

add_lacode_region <- function(la_trend_data) {

  # join oslaua and region by la name
  oslaua_to_rgn <- make_oslaua_to_rgn_lup(add_la_names = TRUE)
  la_trend_data <- left_join(la_trend_data, oslaua_to_rgn, by = "local_authority_name")

  # indentity/label inner and outer london
  la_trend_data <- identify_inner_outer_london(la_trend_data)

  # reorder columns
  la_trend_data <- la_trend_data %>%
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


# ADD GEOGRAPHY AND LONDON UNDERGROUND DATA for a particular period period name
add_lsoa_geog_details <- function(lsoa_trend_data, period = "tph_weekday_Morning_Peak") {

  # filter to keep just one period
  lsoa_trips_trend <- lsoa_trend_data %>%
    filter(grepl(period, period_name))

  # join LA and region data
  lsoa_trips_trend <- left_join(lsoa_trips_trend, lsoa_to_la_lup, by = "lsoa11")
  lsoa_trips_trend <- left_join(lsoa_trips_trend, la_to_rgn_lup, by = "oslaua")

  # add london details
  lsoa_trips_trend <- add_london_metro_lsoas(lsoa_trips_trend)
  lsoa_trips_trend <- identify_inner_outer_london(lsoa_trips_trend)

  lsoa_trips_trend <- lsoa_trips_trend %>%
    mutate(london_tube = case_when(region_name == "London" & london_underground ~ "London: On tube",
                                   region_name == "London" & !london_underground ~ "London: Off tube",
                                   TRUE ~ "Outside London"))

  lsoa_trips_trend <- lsoa_trips_trend %>%
    select(lsoa11,
           oslaua,
           local_authority_name,
           rgn,
           region_name,
           london,
           london_underground,
           london_tube,
           everything())

}

calculate_routes_per_lsoa <- function(lsoa_boundaries, year) {

  #routes_2023 <- st_read("data/routes/routes_2023.geojson")
  file.path.full <- paste0("data/route-stops/route-stops-", year, "/route-stops-", year, ".shp")
  routestops <- st_read(file.path.full)

  # clean and keep only route_id (and geom)
  routestops <- routestops %>%
    filter(route_type == 3) %>%
    select(route_id)

  routestops <- routestops %>%
    st_transform(crs = 27700)

  lsoa_stops <- st_intersection(lsoa_boundaries, routestops)
  routes_per_lsoa <- lsoa_stops %>%
    st_drop_geometry() %>%
    distinct(lsoa11,
             route_id) %>%
    group_by(lsoa11) %>%
    summarise(number_of_routes = n()) %>%
    ungroup()

  # name column for year
  colnames(routes_per_lsoa)[2] <- paste0("number_of_routes_", year)

  return(routes_per_lsoa)

}

calculate_number_of_routes_per_lsoa <- function() {

  # start timer
  tic("make routes per LSOA")

  # intersect routestops with lsoas to determine how many different routes pass through each lsoa
  # Use LSOA centroid data (i.e. within 500m)
  lsoa_boundaries <- readRDS("data/lsoa/GB_LSOA_2011_full_or_500mBuff.Rds")
  lsoa_boundaries <- lsoa_boundaries %>%
    select(lsoa11 = code,
           geometry)

  routes_per_lsoa_2008 <- calculate_routes_per_lsoa(lsoa_boundaries, "2008")
  routes_per_lsoa_2019 <- calculate_routes_per_lsoa(lsoa_boundaries, "2019")
  routes_per_lsoa_2022 <- calculate_routes_per_lsoa(lsoa_boundaries, "2022")
  routes_per_lsoa_2023 <- calculate_routes_per_lsoa(lsoa_boundaries, "2023")

  saveRDS(routes_per_lsoa_2008,
          "data/route-stops/route-stops-2008/routes_per_lsoa_2008.rds")
  saveRDS(routes_per_lsoa_2019,
          "data/route-stops/routes_per_lsoa_2019.rds")
  saveRDS(routes_per_lsoa_2022,
          "data/route-stops/routes_per_lsoa_2022.rds")
  saveRDS(routes_per_lsoa_2023,
          "data/route-stops/routes_per_lsoa_2023.rds")

  lsoas <- st_drop_geometry(lsoa_boundaries)

  routes_per_lsoa_all <- Reduce(function(x, y) left_join(x, y, by = "lsoa11"),
                                list(lsoas,
                                     routes_per_lsoa_2008,
                                     routes_per_lsoa_2019,
                                     routes_per_lsoa_2022,
                                     routes_per_lsoa_2023))

  saveRDS(routes_per_lsoa_all,
          "data/route-stops/routes_per_lsoa_2008_2023.rds")

  routes_per_lsoa_all %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE))

  # end timer
  toc()

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

# LSOA trend data ---------------------------------------------------------

lsoa_trips_trend_for_bivariate_map <- function(lsoa_trips_trend,
                                               threshold_c,
                                               threshold_b,
                                               threshold_3,
                                               threshold_2,
                                               label_1,
                                               label_2,
                                               label_3) {


  routes_per_lsoa_2008_2023 <- read_rds("data/route-stops/routes_per_lsoa_2008_2023.rds")
  lsoa_trips_trend <- left_join(lsoa_trips_trend, routes_per_lsoa_2008_2023, by = "lsoa11")

  # calculate the number of tph per route
  # PLACEHOLDER: number of routes per LSOA is wrong for later years, waiting for data to be redone.
  # In the meantime, just work with 2008 number of routes (which will over estimate it for later years)
  lsoa_trips_trend <- lsoa_trips_trend %>%
    mutate(tph_per_route_2008 = trips_2006_08 / number_of_routes_2008,
           tph_per_route_2019 = trips_2019 / number_of_routes_2008,
           tph_per_route_2022 = trips_2022 / number_of_routes_2008,
           tph_per_route_2023 = trips_2023 / number_of_routes_2008)

  #categorise tph per route as follows:
  # > summary(lsoa_trips_trend$tph_per_route_2008)
  # over 4 = good
  # 2 - 4 = okay
  # less than 2 poor
  #lsoa_trips_trend <- lsoa_trips_trend %>%
  #  mutate(service_frequency_2008 = case_when(tph_per_route_2008 < threshold_c ~ "C",
  #                                            between(tph_per_route_2008, threshold_b, threshold_c) ~ "B",
  #                                            tph_per_route_2008 > threshold_b ~ "A"),
  #         service_frequency_2008_label = case_when(tph_per_route_2008 < threshold_c ~ "Poor",
  #                                                  between(tph_per_route_2008, threshold_b, threshold_c) ~ "OK",
  #                                                  tph_per_route_2008 > threshold_b ~ "Good"))

  # trips per hour assessment
  # less than 12 = poor (less than one every 5 mins)
  # 12 - 60 = okay
  # more than 60 = good (more than one every min)
  lsoa_trips_trend <- lsoa_trips_trend %>%
    mutate(service_frequency_2008 = case_when(trips_2006_08 > threshold_c ~ "C",
                                              between(trips_2006_08, threshold_b, threshold_c) ~ "B",
                                              trips_2006_08 < threshold_b ~ "A"),
           service_frequency_2008_label = case_when(trips_2006_08 > threshold_c ~ "Good",
                                                    between(trips_2006_08, threshold_b, threshold_c) ~ "OK",
                                                    trips_2006_08 < threshold_b ~ "Poor"))


  # check: table(lsoa_trips_trend$service_frequency_2008)

  #categorise change in service as follow:
  #summary(lsoa_trips_trend$trips_change_2008_2023_pct, na.rm = TRUE)
  #hist(lsoa_trips_trend$trips_change_2008_2023_pct, breaks = 100)
  #table(lsoa_trips_trend$trips_change_2008_2023_pct > 0)
  #table(between(lsoa_trips_trend$trips_change_2008_2023_pct, -0.3, 0))
  #table(lsoa_trips_trend$trips_change_2008_2023_pct < -0.3)
  # over 0 = improved
  # between -0.3 and 0 = reduced somewhat
  # below -0.3 reduced significantly

  lsoa_trips_trend <- lsoa_trips_trend %>%
    mutate(service_reduction_2008_23 = case_when(trips_change_2008_2023_pct >= threshold_3 ~ "3",
                                                 between(trips_change_2008_2023_pct, threshold_2, threshold_3) ~ "2",
                                                 trips_change_2008_2023_pct < threshold_2 ~ "1"),
           service_reduction_2008_23_label = case_when(trips_change_2008_2023_pct >= threshold_3 ~ label_3,
                                                       between(trips_change_2008_2023_pct, threshold_2, threshold_3) ~ label_2,
                                                       trips_change_2008_2023_pct < threshold_2 ~ label_1))

  # check: table(lsoa_trips_trend$service_reduction_2008_23)
  table(lsoa_trips_trend$service_frequency_2008,
        lsoa_trips_trend$service_reduction_2008_23)

  lsoa_trips_bivariate <- lsoa_trips_trend %>%
    filter(!is.na(service_reduction_2008_23)) %>%
    filter(!is.na(service_frequency_2008)) %>%
    unite(trips_bi_variate, service_frequency_2008, service_reduction_2008_23, sep = "", remove = FALSE) %>%
    unite(trips_bi_variate_label, service_frequency_2008_label, service_reduction_2008_23_label, sep = ": ", remove = FALSE) %>%
    mutate(trips_bi_variate = as.factor(trips_bi_variate),
           trips_bi_variate_label = as.factor(trips_bi_variate_label)) %>%
    transmute(lsoa11,
              trips_bi_variate,
              trips_bi_variate_label,
              service_frequency_2008,
              service_frequency_2008_label,
              service_reduction_2008_23,
              service_reduction_2008_23_label,
              tph_per_route_2008,
              #tph_per_route_2019,
              #tph_per_route_2022,
              tph_per_route_2023,
              trips_2006_08,
              trips_2023,
              trips_change_2008_2023_pct = round(trips_change_2008_2023_pct * 100, 1))

  # Sort levels out for bivariate_label field (so graph plots with right colour scheme)
  lsoa_trips_bivariate$trips_bi_variate_label <- factor(lsoa_trips_bivariate$trips_bi_variate_label,
                                                        levels =  c(
                                                          paste("Poor", label_1, sep = ": "),
                                                          paste("OK", label_1, sep = ": "),
                                                          paste("Good", label_1, sep = ": "),
                                                          paste("Poor", label_2, sep = ": "),
                                                          paste("OK", label_2, sep = ": "),
                                                          paste("Good", label_2, sep = ": "),
                                                          paste("Poor", label_3, sep = ": "),
                                                          paste("OK", label_3, sep = ": "),
                                                          paste("Good", label_3, sep = ": ")
                                                          ))

  lsoa_boundaries <- readRDS("../gis-data/boundaries/lsoa/GB_LSOA_2011_super_generalised.Rds")
  lsoa_boundaries <- rename(lsoa_boundaries, lsoa11 = code)
  lsoa_trips_bivariate <- left_join(lsoa_boundaries, lsoa_trips_bivariate, by = "lsoa11")
  #class(lsoa_trips_bivariate)

}

lsoa_trend_data <- make_lsoa_trend_data(periods)
saveRDS(object = lsoa_trend_data,
        file = "data/lsoa_bustrip_trends_2008_2023.rds")
lsoa_trend_data <- readRDS("data/lsoa_bustrip_trends_2008_2023.rds") # here's one I did earlier...
lsoa_trips_trend <- add_lsoa_geog_details(lsoa_trend_data, "tph_weekday_Morning_Peak")

summary(lsoa_trips_trend$trips_2006_08)
hist(lsoa_trips_trend$trips_2006_08, breaks = 100)

lsoa_trips_bivariate <- lsoa_trips_trend_for_bivariate_map(lsoa_trips_trend,
                                                           threshold_c = 60,
                                                           threshold_b = 15,
                                                           threshold_3 = 0.5,
                                                           threshold_2 = -0.40,
                                                           label_1 = "Worse",
                                                           label_2 = "Same",
                                                           label_3 = "Better")

table(lsoa_trips_bivariate$service_frequency_2008_label,
      lsoa_trips_bivariate$service_reduction_2008_23_label)

table(lsoa_trips_bivariate$trips_bi_variate_label,
      lsoa_trips_bivariate$trips_bi_variate,
      useNA = "ifany")

table(lsoa_trips_bivariate$trips_bi_variate_label)

summary(lsoa_trips_bivariate$tph_per_route_2008)
summary(lsoa_trips_bivariate$tph_per_route_2023)

lsoa_tph_per_route <- lsoa_trips_bivariate %>%
  st_drop_geometry() %>%
  select(lsoa11,
         tph_per_route_2008,
         tph_per_route_2023) %>%
  gather(key = indicator,
         value = tprph,
         -lsoa11) %>%
  mutate(year = as.factor(as.numeric(gsub("tph_per_route_", "", indicator))))

ggplot(data = lsoa_tph_per_route, aes(x = tprph, col = year, fill = year)) +
  geom_histogram(position = "dodge", binwidth = 0.2)

hist(lsoa_trips_bivariate$tph_per_route_2008, breaks = 100)
hist(lsoa_trips_bivariate$tph_per_route_2023, breaks = 100)

#3A #cc0024  #3B #8a274a  #3C #4b264d
#2A #dd7c8a  #2B #8d6c8f  #2C #4a4779
#1A #dddddd  #1B #7bb3d1  #1C #016eae

myPalette <- c("#dddddd", #A1 - "Good: Same or improved"
               "#dd7c8a", #A2 - "OK: Same or improved"
               "#cc0024", #A3 - "Poor: Same or improved"
               "#7bb3d1", #B1 - "Good: Reduced slightly"
               "#8d6c8f", #B2 - "OK: Reduced slightly"
               "#8a274a", #B3 - "Poor: Reduced slightly"
               "#016eae", #C1 - "Good: Reduced significantly"
               "#4a4779", #C2 - "OK: Reduced significantly"
               "#4b264d") #C3 - "Poor: Reduced significantly"


myPalette2 <- c("#f3f3f3",#A1 - "Poor: Worse"
               "#eac5dd", #B1 - "OK: Worse"
               "#e6a3d0", #C1 - "Good: Worse"
               "#c2f1ce", #A2 - "Poor: Same"
               "#9ec6d3", #B2 - "OK: Same"
               "#bc9fce", #C2 - "Good: Same"
               "#8be2af", #A3 - "Poor: Better"
               "#7fc6b1", #B3 - "OK: Better"
               "#7b8eaf") #C3 - "Good: Better"

lsoa_trips_bivariate <- lsoa_trips_bivariate %>%
  filter(!is.na(trips_bi_variate))

tmap_mode("view")
tm_shape(lsoa_trips_bivariate) +
  tm_polygons(col = "trips_bi_variate_label",
              alpha = 1,
              border.col = NA,
              border.alpha = 0,
              palette = myPalette2,
              title = "Public tranport (2008 - 2023)",
              popup.vars = c("Frequency (2008)" = "service_frequency_2008_label",
                "Change in service (2008-2023)" = "service_reduction_2008_23_label",
                "Trips per route per hour (2008)" = "tph_per_route_2008",
                "Trips per hour (2008)" = "trips_2006_08",
                "Trips per hour (2023)" = "trips_2023",
                "Change in trips per hour (%)" = "trips_change_2008_2023_pct"))


tmap_mode("plot")
b_plot <- tm_shape(lsoa_trips_bivariate) +
  tm_polygons(col = "trips_bi_variate_label",
              alpha = 1,
              border.col = NA,
              border.alpha = 0,
              palette = myPalette,
              title = "Public tranport (2008 - 2023)") +
  tm_layout(
    legend.title.size = 1.0,
    legend.text.size = 0.75,
    legend.outside = TRUE,
    legend.outside.size = 0.5,
    legend.outside.position = "right")


tmap_save(b_plot,
          "plots/july-23/bus-service-bivariate-2008-2023.png",
          width = 500,
          height = 450,
          units = "mm",
          dpi = 600)

# lets count how many types of LSOA are in each LA and region
lsoa_trips_bivariate <- left_join(lsoa_trips_bivariate, lsoa_to_la_lup, by = "lsoa11")
lsoa_trips_bivariate <- left_join(lsoa_trips_bivariate, la_to_rgn_lup, by = "oslaua")

summarise_trends_by_geog_n <- function(.lsoa_trips_bivariate,
                                       geog_code,
                                       geog_name) {

  bivariate_summary <- .lsoa_trips_bivariate %>%
    st_drop_geometry() %>%
    filter(!is.na(trips_bi_variate_label)) %>%
    filter(!substring(lsoa11, 1, 1) == "S") %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             trips_bi_variate_label) %>%
    summarise(#trips_2006_08 = mean(trips_2006_08, na.rm = TRUE),
              #trips_2023 = mean(trips_2023, na.rm = TRUE),
              #trips_change_2008_2023_pct = mean(trips_change_2008_2023_pct, na.rm = TRUE),
              lsoas_n = n()) %>%
    group_by({{ geog_code }},
             {{ geog_name }}) %>%
    mutate(lsoas_pct = lsoas_n / sum(lsoas_n)) %>%
    ungroup()

  bivariate_summary <- bivariate_summary %>%
    gather(key = indicator,
           value = val,
           -trips_bi_variate_label,
           -{{ geog_code }},
           -{{ geog_name }}) %>%
    unite(indicator_full, trips_bi_variate_label, indicator, sep = ": ") %>%
    spread(key = indicator_full,
           value = val,
           fill = 0)

  bivariate_summary <- bivariate_summary %>%
    mutate(`Good - Improved: Rank` = rank(desc(`Good: Improved: lsoas_pct`), ties.method = "random"),
           `Poor - Significant reduction: Rank` = rank(desc(`Poor: Significant reduction: lsoas_pct`), ties.method = "random"))

}

summarise_trends_by_geog_pct_change <- function(.lsoa_trips_bivariate,
                                                ...) {

  bivariate_summary <- .lsoa_trips_bivariate %>%
    st_drop_geometry() %>%
    filter(!is.na(trips_bi_variate_label)) %>%
    filter(!substring(lsoa11, 1, 1) == "S") %>%
    group_by(...,
             trips_bi_variate_label) %>%
    summarise(trips_change_2008_2023_pct = mean(trips_change_2008_2023_pct, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(key = trips_bi_variate_label,
           value = trips_change_2008_2023_pct,
           fill = 0)

  bivariate_summary <- bivariate_summary %>%
    select(...,
           `Good: Improved (% change in service)` = `Good: Improved`,
           `OK: Improved (% change in service)` = `OK: Improved`,
           `Poor: Improved (% change in service)` = `Poor: Improved`,
           `Good: Same or moderate reduction (% change in service)` = `Good: Same or moderate reduction`,
           `OK: Same or moderate reduction (% change in service)` = `OK: Same or moderate reduction`,
           `Poor: Same or moderate reduction (% change in service)` = `Poor: Same or moderate reduction`,
           `Good: Significant reduction (% change in service)` = `Good: Significant reduction`,
           `OK: Significant reduction (% change in service)` = `OK: Significant reduction`,
           `Poor: Significant reduction (% change in service)` = `Poor: Significant reduction`)

}

region_overall_summary <- function(lsoa_trips_bivariate) {

  region_bivariate_summary_lsoacount <- summarise_trends_by_geog_n(lsoa_trips_bivariate,
                                                                   rgn,
                                                                   region_name)

  region_bivariate_summary_pct <- summarise_trends_by_geog_pct_change(lsoa_trips_bivariate,
                                                                      rgn,
                                                                      region_name)

  region_summary <- inner_join(region_bivariate_summary_lsoacount, region_bivariate_summary_pct, by = c("rgn", "region_name"))

}


la_overall_summary <- function(lsoa_trips_bivariate) {

  la_bivariate_summary_lsoacount <- summarise_trends_by_geog_n(lsoa_trips_bivariate,
                                                               oslaua,
                                                               local_authority_name)
  la_bivariate_summary_pct <- summarise_trends_by_geog_pct_change(lsoa_trips_bivariate,
                                                                  oslaua,
                                                                  local_authority_name)

  la_summary <- inner_join(la_bivariate_summary_lsoacount, la_bivariate_summary_pct, by = c("oslaua", "local_authority_name"))

}

region_bivariate_summary <- region_overall_summary(lsoa_trips_bivariate)
la_bivariate_summary <- la_overall_summary(lsoa_trips_bivariate)

# standardised function to save one or more data.frames in a spreadsheet with one or more tabs
save_xlsx_file <- function(xlsx_name, list_of_tabs_and_dfs) {

  headerStyle <- createStyle(
    fontSize = 12,
    fontColour = "black",
    halign = "center",
    fgFill = "grey",
    border = "TopBottomLeftRight",
    borderColour = "black",
    textDecoration = "bold",
    wrapText = TRUE
  )

  write.xlsx(list_of_tabs_and_dfs,
             file = xlsx_name,
             #colWidths = specific.col.width,
             startRow = 1,
             startCol = 1,
             headerStyle = headerStyle,
             borders = "columns",
             withFilter = TRUE)
}

tabs <- list("region" = region_bivariate_summary,
             "local-authority" = la_bivariate_summary)
save_xlsx_file("plots/july-23/bus-service-trend-summary-2008-2023.xlsx",
               tabs)

# save outputs ------------------------------------------------------------


# save main outputs as RDS
saveRDS(object = la_trend_data,
        file = "data/la_bustrip_trends_2008_2023.rds")
saveRDS(object = lsoa_trend_data,
        file = "data/lsoa_bustrip_trends_2008_2023.rds")




# LA trend data -----------------------------------------------------------

la_trend_data <- make_trend_data(periods, geog = "la", geog_name = local_authority_name)
la_trend_data <- add_lacode_region(la_trend_data)
la_trend_data <- add_la_rural_urban_class(la_trend_data)
la_trend_data_eng <- la_trend_data %>%
  filter(!region_name %in% c("Wales", "Scotland"))

london_trend_data <- summarise_trend_data(la_trend_data_eng, period_name, london, rural_urban)
london_tube_summary <- summarise_trend_data(lsoa_trips_trend, london_tube)

london_tube_graphdata <- london_tube_summary %>%
  select(london_tube,
         `2008` = trips_2006_08,
         `2019` = trips_2019,
         `2022` = trips_2022,
         `2023` = trips_2023) %>%
  gather(key = year,
         value = trips_per_hour,
         -london_tube) %>%
  mutate(year = as.integer(year))

london_tube_graphdata <- london_tube_graphdata %>%
  group_by(london_tube) %>%
  mutate(trips_per_hour_pctmax = trips_per_hour / trips_per_hour[year == 2008]) %>%
  ungroup() %>%
  rename(Location = london_tube)

london_tube_graph_pct <- ggplot(data = london_tube_graphdata, aes(x = year, y = trips_per_hour_pctmax, col = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  ylim(c(0,1.2)) +
  theme_bw() +
  #theme_classic() +
  #theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  #theme(axis.line = element_line(colour = "black", linewidth = 1)) +
  #scale_x_continuous(expand=c(0,0)) +
  xlab("Year") +
  ylab("Average service level index (2008 == 1.0)")

london_tube_graph_trips <- ggplot(data = london_tube_graphdata, aes(x = year, y = trips_per_hour, col = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  #ylim(c(0,1.2)) +
  theme_bw() +
  #theme_classic() +
  #theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  #theme(axis.line = element_line(colour = "black", linewidth = 1)) +
  #scale_x_continuous(expand=c(0,0)) +
  xlab("Year") +
  ylab("Average trips per hour (rush hour)")


ggsave(plot = london_tube_graph_pct,
       filename = "plots/july-23/london-tube-outside-trend-pct-2008-23.png")

ggsave(plot = london_tube_graph_trips,
       filename = "plots/july-23/london-tube-outside-trend-trips-2008-23.png")


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
  ylim(c(0,1.2)) +
  theme_bw() +
  xlab("Year") +
  ylab("Average number of trips per hour (rush hour)") +
  scale_color_brewer(palette = "Dark2")



#  regional table ---------------------------------------------------------

library(RColorBrewer)
library(ggsci)
display.brewer.all()

table(region_trend_data$period_name)

region_trend_data <- summarise_trend_data(la_trend_data, period_name, rgn, region_name)
region_summary_rushhour <- region_trend_data %>%
  filter(grepl("Sat_Evening", period_name))

region_summary_rushhour_graphdata <- region_summary_rushhour %>%
  select(region_name,
         `2008` = trips_2006_08,
         `2019` = trips_2019,
         `2022` = trips_2022,
         `2023` = trips_2023) %>%
  gather(key = year,
         value = trips_per_hour,
         -region_name) %>%
  mutate(year = as.integer(year))

region_summary_rushhour_graphdata <- region_summary_rushhour_graphdata %>%
  group_by(region_name) %>%
  mutate(trips_per_hour_pctmax = trips_per_hour / trips_per_hour[year == 2008]) %>%
  ungroup()

region_summary_rushhour_graph_pct <- ggplot(data = region_summary_rushhour_graphdata, aes(x = year, y = trips_per_hour_pctmax, col = region_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  ylim(c(0,1.2)) +
  theme_bw() +
  #theme_classic() +
  #theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  #theme(axis.line = element_line(colour = "black", linewidth = 1)) +
  #scale_x_continuous(expand=c(0,0)) +
  xlab("Year") +
  ylab("Average service level index (2008 == 1.0)") +
  scale_color_brewer(palette = "Paired")
  #scale_color_npg()

plot(region_summary_rushhour_graph_pct)

region_summary_rushhour_graph_trips <- ggplot(data = region_summary_rushhour_graphdata, aes(x = year, y = trips_per_hour, col = region_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = "circle open") +
  theme_bw() +
  xlab("Year") +
  ylab("Average trips per hour (rush hour)") +
  scale_color_brewer(palette = "Paired")

plot(region_summary_rushhour_graph_trips)

ggsave(plot = region_summary_rushhour_graph_pct,
       filename = "plots/july-23/region-trend-pct-2008-23.png")

ggsave(plot = region_summary_rushhour_graph_trips,
       filename = "plots/july-23/region-trend-trips-2008-23.png")




# SAVE OUTPUTS ------------------------------------------------------------



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
