#' -----------------------------------------------------------------------------
#'
#' Script from which to run all processing options to update Friends of the Earth
#' analysis of Leeds University's transport data.
#'
#' Author: Toby Bridgeman
#' University of Leeds data: Malcolm Morgan
#'
#' -----------------------------------------------------------------------------
#'
#' Source data:
#' - LSOA21-level details on public transport frequency for 15 different time periods across the week
#'    - Annual data: 2007 to 2024.
#' - ONSPD (up to date version with LSOA21 and rural urban classification 2021)
#' - Socio-demographics (TBC), but to include car ownership levels...
#'
#' -----------------------------------------------------------------------------
#'
#' Processing summary:
#'
#' 1. Create baseline year data for 2010, averaging 3-4 years across 2007-2012
#' 2. Simplify time periods to:
#'    - weekday daytime
#'    - weekday evening
#'    - saturday daytime
#'    - saturday evening
#'    - sunday daytime
#'    - sunday evening
#'    (Ignore nightime for now...)
#' 3. Add rural urban classification for 2021, from ONSPD
#' 4. summarise 2024 LSOA data to show the following:
#'    a) Show average bustrips per hour for each time period for each quintile
#'    b) Identify how many LSOAs have no bus services for each time period and
#'        summarise by rural/urban class
#'
#' -----------------------------------------------------------------------------

# set up
clear_all <- FALSE
source("scripts/march-25/set-up.R")
onspd <- load_onspd()

#' -----------------------------------------------------------------------------

# fetch lsoa21 level data for buses for baseline year (2010) and latest year (2024)
#lsoa21_buses_200710 <- make_simplified_bustrips_lsoa21_200710()
lsoa21_buses_2010_new <- make_simplified_bustrips_lsoa21_single_year(data_year = 2010, new = TRUE)
lsoa21_buses_2024_new <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024, new = TRUE)
lsoa21_buses_2023_bods <- make_simplified_bustrips_lsoa21_single_year(data_year = "BODS2023", new = TRUE)

lsoa21_buses_2010 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2010, new = FALSE)
lsoa21_buses_2023 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2023, new = FALSE)
lsoa21_buses_2024 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024, new = FALSE)



#' -----------------------------------------------------------------------------

# make outputs
headlines <- make_rurality_comparison_2010_2024(bus_lsoa_2010 = lsoa21_buses_2023,
                                                bus_lsoa_2024 = lsoa21_buses_2024,
                                                time_period_filter = FALSE)

#t1 <- t1_regional_all_trends(lsoa21_buses_200710, lsoa21_buses_2024)
t1 <- t1_regional_all_trends(lsoa21_buses_2023, lsoa21_buses_2024)

t2 <- t2_la_daytime_trends(bus_lsoa21_2010 = lsoa21_buses_2010_new, bus_lsoa21_2024 = lsoa21_buses_2024_new)

t3_bus_quintiles(lsoa21_buses_2010_new, lsoa21_buses_2024_new)

# set threshold for zero buses. Any LSOA with service below this will count as having zero service
zero_bus_threshold = 0.5
t4 <- t4_zero_tph_lsoas(lsoa21_buses_2024_new, threshold = zero_bus_threshold)
# get more detailed analysis by diffent geographies
t4_las <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = zero_bus_threshold, oslaua, local_authority)
t4_pcon24 <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = zero_bus_threshold, pcon24, pcon24_name)
t4_region <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = zero_bus_threshold, rgn, region_name)

# t5
t5 <- t5_best_worst_quintiles_cars(lsoa21_buses_2024_new, time_period_name = "weekday_daytime")

# graph data
graph1_data <- graph1_summarise_2010_2024_quintile_counts(lsoa21_buses_2024_new,
                                                          lsoa21_buses_2010_new,
                                                          time_period_name = "weekday_daytime")


#   -----------------------------------------------------------------------

# Make map outputs (csvs)
make_and_save_geog_trends_csv(lsoa21_buses_2024_new,
                              lsoa21_buses_2010_new,
                              time_period_label = "weekday_daytime")

#' -----------------------------------------------------------------------------

# save outputs
save_article_main_tables(t1, t2, t3a = t3d_2024_daytime_range, t3b = t3c_2024_daytime, t4, t5, graph1_data, headlines)
save_no_service_geogs(t4_region, t4_las, t4_pcon24)


