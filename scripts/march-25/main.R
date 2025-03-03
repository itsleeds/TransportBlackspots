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

lsoa21_buses_2024 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024)
lsoa21_buses_200710 <- make_simplified_bustrips_lsoa21_200710()

lsoa21_buses_2024_new <- load_lsoa21_bustrips_newbands(data_year = 2024)

str(lsoa21_buses_2024)

lsoa21_buses_2024 <- make_tph_quintiles(lsoa21_buses_2024)

#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
