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

# set up ------------------------------------------------------------------

clear_all = TRUE
source("scripts/november-24/set-up.R")

onspd <- load_onspd(keep_only_current = TRUE)

# run functions -----------------------------------------------------------

lsoa_bustrips_2023 <- load_lsoa_bustrips(onspd, year_list = 2023)

lsoa_bustrips_2023_service <- classify_service_quality(lsoa_bustrips_2023)
lsoa_bustrips_2023_service <- add_geography(lsoa_bustrips_2023_service)
lsoa_bustrips_2023_service <- add_ethnicity_imd_carownership_lsoa(lsoa_bustrips_2023_service)

lsoa_bustrips_2023_quintiles <- classify_service_quality_in_quintiles(lsoa_bustrips_2023)
lsoa_bustrips_2023_quintiles <- add_geography(lsoa_bustrips_2023_quintiles)
lsoa_bustrips_2023_quintiles <- add_ethnicity_imd_carownership_lsoa(lsoa_bustrips_2023_quintiles)

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


tmap_mode(mode = "view")
make_map_of_bus_service(lsoa_bustrips_2023_quintiles)
make_map_of_bus_service_sun_pm(lsoa_bustrips_2023_quintiles)
make_map_of_bus_service_wkday_eve(lsoa_bustrips_2023_quintiles)

make_map_of_bus_service_score(lsoa_bustrips_2023_quintiles)

str(lsoa_bustrips_2023_quintiles)

make_map_of_bus_good_service_duration(lsoa_bustrips_2023)
make_map_of_bus_all_service_duration(lsoa_bustrips_2023)

