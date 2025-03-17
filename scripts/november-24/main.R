#' main script to run all functions for November 2024 - March 2025 analysis
#'
#' ## Summary of processing approach for Quality of service
#'
#'  - Use LSOA data for 2004-2023
#'
#'  - Identify LSOAs by rurality (using ONSPD) for four classifications:
#'      Urban major conurbation
#'      Urban minor conurbation
#'      Rural Town and Fringe
#'      Rural Village and Dispersed
#'
#'  - Derive a metric for each class on a good, average and poor level of service
#'    (this is likely to be guided by data for a 'good' year but also using
#'     literature on other ?European? countries. A good year in this instance
#'     refers to a year when services where better, but also when there was good
#'     data coverage. So)
#'    This needs to:
#'        - Identify a year in the data when coverage is good and gaps are minimal,
#'          with which to help derive this metric
#'        - Consider which time intervals to work with. Provisionally, look at all
#'          times for: 6-10am, 10am-3pm, 3pm-6pm, 6pm-10pm (and when possible 10pm to midnight)
#'
#'  - Using this derived metric then identify the level of service in each LSOAs
#'    in 2023 for each time interval
#'
#'  - Produce a national, regional, constituency and local authority analysis
#'    summarising the number of LSOAs in each rating for each time interval


#' edit:
#'
#' Reduce slots into 4 time periods
#'  - Weekday day 6am-6pm
#'  - Weekday eve 6pm-10pm
#'  - Weekend day 6am-6pm
#'  - Weekend eve 6pm-10pm
#'  [CHECK THIS MAKES SENSE FOR SUNDAY, if not split Sunday and Saturday]
#'
#' Calculate current quintiles
#' Compare with 2007-10 quintiles
#' Just use 1-5 with 1 being the best service. Could convert to letters? A-E?


# set up ------------------------------------------------------------------
clear_all = FALSE
source("scripts/november-24/set-up.R")

onspd <- load_onspd(keep_only_current = FALSE)

# run functions -----------------------------------------------------------
lsoa_bustrips_2023 <- load_lsoa_bustrips(onspd, year_list = 2023)

lsoa_bustrips_2023_service <- classify_service_quality(lsoa_bustrips_2023)
lsoa_bustrips_2023_service <- add_geography(lsoa_bustrips_2023_service)
lsoa_bustrips_2023_service <- add_ethnicity_imd_carownership_lsoa(lsoa_bustrips_2023_service)

lsoa_bustrips_2023_quintiles <- classify_service_quality_in_quintiles(lsoa_bustrips_2023)
lsoa_bustrips_2023_quintiles <- add_geography(lsoa_bustrips_2023_quintiles)
lsoa_bustrips_2023_quintiles <- add_ethnicity_imd_carownership_lsoa(lsoa_bustrips_2023_quintiles)

# check results -----------------------------------------------------------

hist(lsoa_bustrips_2023_quintiles$weeklong_vgood_duration_pct)
hist(lsoa_bustrips_2023_quintiles$weeklong_okay_duration_pct)
hist(lsoa_bustrips_2023_quintiles$weeklong_poor_duration_pct)

# percentage of the time at which services are at least 'poor' or higher
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = weeklong_poor_duration_pct,
                        type = "score",
                        pct = TRUE)

# percentage of the time at which services are at least 'okay' or higher
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = weeklong_okay_duration_pct,
                        type = "score",
                        pct = TRUE)

# percentage of the time at which services are 'very good'
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = weeklong_vgood_duration_pct,
                        type = "score",
                        pct = TRUE)

# percentage of the time at which services are 'good'
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = weeklong_good_duration_pct,
                        type = "score",
                        pct = TRUE)

# count of times at which services are 'good'
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = weeklong_good_duration,
                        type = "score")


# duration of services
# how many hours of the week do LSOA have a service
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = weeklong_duration,
                        tph_service = weeklong_duration,
                        type = "score")

# what proportion of the week (i.e. hours / 48) do LSOAs have a service
make_map_of_bus_service(lsoa_bustrips = lsoa_bustrips_2023_quintiles,
                        tph = weeklong_duration,
                        tph_service = weeklong_duration_pct,
                        type = "score",
                        pct = TRUE)




# review results ----------------------------------------------------------

service_by_region <- summarise_service_by_period_and_geog(lsoa_bustrips_2023_service, geog_name = region_name, group_var = rurality, service = tph_daytime_avg_service, measure = "service")
quintiles_by_region <- summarise_service_by_period_and_geog(lsoa_bustrips_2023_quintiles, geog_name = region_name, group_var = rurality, service = tph_daytime_avg_quintile)
quintile_service_by_region <- summarise_service_by_period_and_geog(lsoa_bustrips_2023_quintiles, geog_name = region_name, group_var = rurality, service = tph_daytime_avg_service, measure = "quintiles")

ethnicity_summary <- make_ethnicity_summary(lsoa_bustrips_2023_quintiles, geog_name = region_name, service = tph_daytime_avg_service)
car_ownership_summary <- make_carownership_summary(lsoa_bustrips_2023_quintiles, geog_name = region_name, service = tph_daytime_avg_service)
imd_summary <- make_imd_summary(lsoa_bustrips_2023_quintiles, service = tph_daytime_avg_service)
imd_rurality_summary <- make_imd_summary_by_region(lsoa_bustrips_2023_quintiles, service = tph_daytime_avg_service, measure = "quintiles")

#   -----------------------------------------------------------------------

summary(lsoa_bustrips_2023$max_number_routes)

table(substring(lsoa_bustrips_2023$lsoa11, 1, 1),
      lsoa_bustrips_2023$rurality, useNA = "ifany")
table(substring(lsoa_bustrips_2023$lsoa11, 1, 1),
      is.na(lsoa_bustrips_2023$tph_weekday_Morning_Peak), useNA = "ifany")

hist(lsoa_bustrips_2023_service$overall_service_score)
hist(lsoa_bustrips_2023_service$weeklong_good_duration, breaks = 20)
hist(lsoa_bustrips_2023_service$weeklong_duration, breaks = 20)

hist(lsoa_bustrips_2023_quintiles$overall_service_score)
hist(lsoa_bustrips_2023_quintiles$weeklong_good_duration, breaks = 20)
hist(lsoa_bustrips_2023_quintiles$weeklong_duration, breaks = 20)


tmap_mode(mode = "plot")
# Weekday daytime average
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_daytime_avg,
                        tph_service = tph_daytime_avg_service)

# weekday morning peak
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_weekday_Morning_Peak,
                        tph_service = tph_weekday_Morning_Peak_service)

# weekday midday
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_weekday_Midday,
                        tph_service = tph_weekday_Midday_service)

# weekday afternoon peak
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_weekday_Afternoon_Peak,
                        tph_service = tph_weekday_Afternoon_Peak_service)

# weekday evening
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_weekday_Evening,
                        tph_service = tph_weekday_Evening_service)

# saturday morning
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sat_Morning_Peak,
                        tph_service = tph_Sat_Morning_Peak_service)

# saturday midday
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sat_Midday,
                        tph_service = tph_Sat_Midday_service)

# saturday afternoon
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sat_Afternoon_Peak,
                        tph_service = tph_Sat_Afternoon_Peak_service)

# saturday evening
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sat_Evening,
                        tph_service = tph_Sat_Evening_service)

# sunday morning
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sun_Morning_Peak,
                        tph_service = tph_Sun_Morning_Peak_service)

# sunday midday
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sun_Midday,
                        tph_service = tph_Sun_Midday_service)

# sunday afternoon
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sun_Afternoon_Peak,
                        tph_service = tph_Sun_Afternoon_Peak_service)

# sunday Evening
make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = tph_Sun_Evening,
                        tph_service = tph_Sun_Evening_service)

#   -----------------------------------------------------------------------

make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = overall_service_score,
                        tph_service = overall_service_score,
                        type = "score")

make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = weekday_service_score,
                        tph_service = weekday_service_score,
                        type = "score")

make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = saturday_service_score,
                        tph_service = saturday_service_score,
                        type = "score")

make_map_of_bus_service(lsoa_bustrips_2023_quintiles,
                        tph = sunday_service_score,
                        tph_service = sunday_service_score,
                        type = "score")

#   -----------------------------------------------------------------------

make_map_of_bus_good_service_duration(lsoa_bustrips_2023)
make_map_of_bus_all_service_duration(lsoa_bustrips_2023)

