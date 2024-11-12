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

clear_all = FALSE
source("scripts/november-24/set-up.R")

# run functions -----------------------------------------------------------

lsoa_bustrips_2023 <- load_lsoa_bustrips(onspd, year_list = 2023)
lsoa_bustrips_2023 <- classify_service_quality(lsoa_bustrips_2023)

# review results ----------------------------------------------------------

summary(lsoa_bustrips_2023$max_number_routes)

table(substring(lsoa_bustrips_2023$lsoa11, 1, 1),
      lsoa_bustrips_2023$rurality, useNA = "ifany")
table(substring(lsoa_bustrips_2023$lsoa11, 1, 1),
      is.na(lsoa_bustrips_2023$tph_weekday_Morning_Peak), useNA = "ifany")

hist(lsoa_bustrips_2023$overall_service_score)
hist(lsoa_bustrips_2023$weeklong_good_duration, breaks = 20)
hist(lsoa_bustrips_2023$weeklong_duration, breaks = 20)

make_map_of_bus_service(lsoa_bustrips_2023)
make_map_of_bus_service_score(lsoa_bustrips_2023)
make_map_of_bus_good_service_duration(lsoa_bustrips_2023)
make_map_of_bus_all_service_duration(lsoa_bustrips_2023)

