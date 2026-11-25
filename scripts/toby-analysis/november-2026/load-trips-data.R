rm(list = ls())
library(tidyverse)
library(sf)
library(tictoc)

source("../foe-utils/scripts/foe-common-functions.R")
source("../geography-lookups/scripts/lookups.R")
source("../geography-lookups/scripts/filter-geographies.R")
source("../geography-lookups/scripts/boundaries.R")

load_onspd = TRUE
source("../geography-lookups/scripts/onspd.R")

#' TODO:
#' - [x] get all lsoas as spine and set NAs to 0 for averaging across regions. NA for LSOA level data.
#' - [x] get lsoa population data and do a population weighted average
#' - [x] add more urban rural detail to the LSOA data (big city, town, rural)
#' - [ ] Make national summary based on rurality 3fold

bustrips_2010_2026_lsoa21 <- make_bustrips_change_2010_2026_df()
lad_change <- average_lsoa_change_by_geog(bustrips_2010_2026_lsoa21, oslaua, local_authority)
pcon_change <- average_lsoa_change_by_geog(bustrips_2010_2026_lsoa21, pcon24, pcon24_name)
rgn_change <- average_lsoa_change_by_geog(bustrips_2010_2026_lsoa21, rgn, region_name)

# TODO: national summary
table(bustrips_2010_2026_lsoa21$london_underground,
      bustrips_2010_2026_lsoa21$rurality_3fold)

bustrips_2010_2026_lsoa21 <- bustrips_2010_2026_lsoa21 |>
  mutate(rurality_3fold_london = case_when(london_underground == TRUE ~ "London on underground",
                                           region_name == "London" ~ "London not on underground",
                                           TRUE ~ rurality_3fold))

bustrips_2010_2026_lsoa21 |>
  group_by(rurality_3fold_london) |>
  summarise(tph_weekday_daytime_mean_2010 = weighted.mean(tph_weekday_daytime_mean_2010, w = population, na.rm = TRUE),
            tph_weekday_daytime_mean_2026 = weighted.mean(tph_weekday_daytime_mean_2026, w = population, na.rm = TRUE)) |>
  ungroup() |>
  mutate(weekday_daytime_change_2010_2026 = tph_weekday_daytime_mean_2026 - tph_weekday_daytime_mean_2010,
         weekday_daytime_change_2010_2026_pct = round(weekday_daytime_change_2010_2026 / tph_weekday_daytime_mean_2010, 4))

lsoa_boundaries <- get_lsoa21_boundaries(res = "super-generalised")
bustrips_2010_2026_lsoa21_sf <- left_join(lsoa_boundaries, bustrips_2010_2026_lsoa21, by = "lsoa21")
st_write(bustrips_2010_2026_lsoa21_sf,
         dsn = "../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/bus-services.gpkg",
         layer = "bus-services-2010-2026-lsoa21",
         delete_layer = TRUE)

table(is.na(bustrips_2010_2026_lsoa21$weekday_daytime_change_pct_2010_2026))


################################################################################################
# Functions
################################################################################

# average change by geog
average_lsoa_change_by_geog <- function(bustrips_2010_2026_lsoa21, geog_code, geog_name) {

  # set NA values to 0 for regional aggregations
  bustrips_2010_2026_lsoa21 <- bustrips_2010_2026_lsoa21 |>
    mutate(weekday_daytime_change_2010_2026 = is_null_zero(weekday_daytime_change_2010_2026),
           weekday_daytime_change_pct_2010_2026 = is_null_zero(weekday_daytime_change_pct_2010_2026))

  # calculate pop weighted averages
  region_week_daytime_change_summary <- bustrips_2010_2026_lsoa21 |>
    group_by({{ geog_code }},
             {{ geog_name }}) |>
    summarise(tph_weekday_daytime_mean_2010 = round(weighted.mean(tph_weekday_daytime_mean_2010, w = population, na.rm = TRUE), 2),
              tph_weekday_daytime_mean_2026 = round(weighted.mean(tph_weekday_daytime_mean_2026, w = population, na.rm = TRUE), 2)) |>
    ungroup() |>
    mutate(weekday_daytime_change_2010_2026 = tph_weekday_daytime_mean_2026 - tph_weekday_daytime_mean_2010,
           weekday_daytime_change_2010_2026_pct = round(weekday_daytime_change_2010_2026 / tph_weekday_daytime_mean_2010, 4)) |>
    arrange(weekday_daytime_change_2010_2026_pct)

  return(region_week_daytime_change_summary)

}


make_bustrips_change_2010_2026_df <- function() {

  # get data for a given year
  bustrips_2026_lsoa21 <- load_annual_lsoa21_trips_data(mode_no = 3, year = 2026)
  bustrips_2010_lsoa21 <- load_annual_lsoa21_trips_data(mode_no = 3, year = 2010)
  # simplify fields kept and calc a weekday average
  bustrips_2026_lsoa21 <- calculate_daytime_weekday_averages(bustrips_2026_lsoa21)
  bustrips_2010_lsoa21 <- calculate_daytime_weekday_averages(bustrips_2010_lsoa21)
  # add year to column names
  bustrips_2026_lsoa21 <- add_year_to_tph_colnames(bustrips_2026_lsoa21, 2026)
  bustrips_2010_lsoa21 <- add_year_to_tph_colnames(bustrips_2010_lsoa21, 2010)

  lsoa21_spine <- get_lsoa21_spine()
  bustrips_2010_2026_lsoa21 <- left_join(lsoa21_spine, bustrips_2010_lsoa21, by = "lsoa21")
  bustrips_2010_2026_lsoa21 <- left_join(bustrips_2010_2026_lsoa21, bustrips_2026_lsoa21, by = "lsoa21")

  # calculate change
  bustrips_2010_2026_lsoa21 <- calculate_tph_change(bustrips_2010_2026_lsoa21)
  bustrips_2010_2026_lsoa21[bustrips_2010_2026_lsoa21 == "Inf"] <- 1
  bustrips_2010_2026_lsoa21[bustrips_2010_2026_lsoa21 == "NaN"] <- 0

  # add rurality, london metro LSOAs id, geography
  bustrips_2010_2026_lsoa21 <- add_lsoa21_rurality(bustrips_2010_2026_lsoa21, onspd)
  bustrips_2010_2026_lsoa21 <- add_london_metro_lsoas(bustrips_2010_2026_lsoa21)
  bustrips_2010_2026_lsoa21 <- add_geog_to_lsoa21_df(bustrips_2010_2026_lsoa21, add_pcon = TRUE)

  # add population
  bustrips_2010_2026_lsoa21 <- add_population_lsoa21(bustrips_2010_2026_lsoa21)

  bustrips_2010_2026_lsoa21 <- bustrips_2010_2026_lsoa21 |>
    select(lsoa21,
           msoa21,
           msoa21_name,
           pcon24,
           pcon24_name,
           oslaua,
           local_authority,
           rgn,
           region_name,
           rurality,
           rurality_3fold,
           london_underground,
           tph_weekday_morning_peak_2010,
           #tph_weekday_midday_2010,
           #tph_weekday_afternoon_peak_2010,
           #tph_weekday_evening_2010,
           #tph_weekday_night_2010,
           tph_weekday_daytime_mean_2010,
           #tph_sat_afternoon_peak_2010,
           #tph_sat_evening_2010,
           #tph_sat_midday_2010,
           #tph_sat_morning_peak_2010,
           #tph_sat_night_2010,
           #tph_sun_afternoon_peak_2010,
           #tph_sun_evening_2010,
           #tph_sun_midday_2010,
           #tph_sun_morning_peak_2010,
           #tph_sun_night_2010,
           routes_morning_peak_2010,
           #routes_midday_2010,
           #routes_afternoon_peak_2010,
           #routes_evening_2010,
           #routes_night_2010,
           tph_weekday_morning_peak_2026,
           #tph_weekday_midday_2026,
           #tph_weekday_afternoon_peak_2026,
           #tph_weekday_evening_2026,
           #tph_weekday_night_2026,
           tph_weekday_daytime_mean_2026,
           #tph_sat_afternoon_peak_2026,
           #tph_sat_evening_2026,
           #tph_sat_midday_2026,
           #tph_sat_morning_peak_2026,
           #tph_sat_night_2026,
           #tph_sun_afternoon_peak_2026,
           #tph_sun_evening_2026,
           #tph_sun_midday_2026,
           #tph_sun_morning_peak_2026,
           #tph_sun_night_2026,
           routes_morning_peak_2026,
           #routes_midday_2026,
           #routes_afternoon_peak_2026,
           #routes_evening_2026,
           #routes_night_2026,
           weekday_morning_change_2010_2026,
           weekday_morning_change_pct_2010_2026,
           #weekday_midday_change_2010_2026,
           #weekday_midday_change_pct_2010_2026,
           #weekday_afternoon_change_2010_2026,
           #weekday_afternoon_change_pct_2010_2026,
           #weekday_evening_change_2010_2026,
           #weekday_evening_change_pct_2010_2026,
           #weekday_night_change_2010_2026,
           #weekday_night_change_pct_2010_2026,
           weekday_daytime_change_2010_2026,
           weekday_daytime_change_pct_2010_2026)
}



get_lsoa21_spine <- function() {

  lsoa21_boundaries <- get_lsoa21_boundaries(res = "super-generalised")

  lsoa21_spine <- st_drop_geometry(lsoa21_boundaries)

}

load_annual_lsoa21_trips_data <- function(mode_no = 3, year = 2026) {

  #read in data for a given year
  trips_lsoa_data_list <- list.files("../PublicTransportAnalysis/data",
                                pattern = "trips_per_lsoa21_22_by_mode_.+Rds",
                                ignore.case = TRUE,
                                full.names = TRUE)
  trips_lsoa_data_location <- trips_lsoa_data_list[grepl(year, trips_lsoa_data_list)]
  trips_lsoa_data <- readRDS(trips_lsoa_data_location)

  # select bus trip data only
  bus_trips_year <- trips_lsoa_data %>%
      filter(route_type == mode_no)

  # clean up column names
  bus_trips_year <- tidy_column_names(bus_trips_year)

  names(bus_trips_year)

  # keep only TPH cols
  tph_cols <- names(bus_trips_year)[grepl("tph", names(bus_trips_year))]
  bus_trips_year <- bus_trips_year |>
    select(lsoa21 = zone_id,
           #route_type,
           all_of(tph_cols),
           routes_afternoon_peak,
           routes_evening,
           routes_midday,
           routes_morning_peak,
           routes_night) |>
    mutate(across(all_of(tph_cols), \(x) round(x, 2)))

}

calculate_daytime_weekday_averages <- function(bustrips_lsoa21) {

  # simplified weekday daytime mean option
  #weekday_daytime_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(morning|midday|afternoon|evening)", names(bustrips_lsoa21))]
  #weekday_daytime_cols_n = length(weekday_daytime_cols)

  weekday_morning_peak_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(morning)", names(bustrips_lsoa21))]
  weekday_midday_peak_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(midday)", names(bustrips_lsoa21))]
  weekday_afternoon_peak_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(afternoon)", names(bustrips_lsoa21))]
  weekday_evening_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(evening)", names(bustrips_lsoa21))]
  weekday_night_cols <- names(bustrips_lsoa21)[grepl("^tph_(mon|tue|wed|thu|fri)_(night)", names(bustrips_lsoa21))]

  bustrips_lsoa21 <- bustrips_lsoa21 |>
    mutate(tph_weekday_morning_peak = rowSums(bustrips_lsoa21[weekday_morning_peak_cols] / 5),
           tph_weekday_midday = rowSums(bustrips_lsoa21[weekday_midday_peak_cols] / 5),
           tph_weekday_afternoon_peak = rowSums(bustrips_lsoa21[weekday_afternoon_peak_cols] / 5),
           tph_weekday_evening = rowSums(bustrips_lsoa21[weekday_evening_cols] / 5),
           tph_weekday_night = rowSums(bustrips_lsoa21[weekday_night_cols] / 5),
           tph_weekday_daytime_mean = ((tph_weekday_morning_peak * 4) + (tph_weekday_midday * 5) + (tph_weekday_afternoon_peak * 3) + (tph_weekday_evening * 4)) / 16,
           #tph_weekday_daytime_test = rowSums(bustrips_lsoa21[weekday_daytime_cols]) / weekday_daytime_cols_n)
           )

  # keep only TPH cols
  tph_weekday_weekend_cols <- names(bustrips_lsoa21)[grepl("tph_(weekday|sat|sun)", names(bustrips_lsoa21))]
  bustrips_lsoa21 <- bustrips_lsoa21 |>
    select(lsoa21,
           all_of(tph_weekday_weekend_cols),
           routes_afternoon_peak,
           routes_evening,
           routes_midday,
           routes_morning_peak,
           routes_night) |>
    mutate(across(all_of(tph_weekday_weekend_cols), \(x) round(x, 2)))


}


#' identify london underground lsoas
identify_london_metro_lsoa21 <- function() {

  # use most recent year of data - should include new Elizabeth line?
  trips_lsoa21 <- readRDS("../PublicTransportAnalysis/data/trips_per_lsoa21_22_by_mode_2026.Rds")

  metro_lsoa21 <- trips_lsoa21 %>%
    filter(route_type == 1) |>
    select(lsoa21 = zone_id,
           route_type)

  # make lsoa21 to region
  lsoa21_to_rgn <- make_lsoa21_to_rgn_lup(filter_for_eng_wales = FALSE)
  lsoa21_to_rgn <- lsoa21_to_rgn |>
    select(lsoa21,
           rgn25cd = rgn,
           rgn25nm = region_name)
  names(metro_lsoa21)

  # keep only lsoas for year with most LSOAs
  # and only those on the london underground area (i.e. not Tyneside metro)
  london_metro_lsoa21 <- metro_lsoa21 %>%
    left_join(lsoa21_to_rgn, by = "lsoa21") |>
    filter(rgn25nm == "London") %>%
    transmute(lsoa21,
              #route_type,
              #year,
              #LAD17NM,
              #RGN11NM,
              london_underground = TRUE)

}


add_year_to_tph_colnames <- function(bustrips_lsoa21, year_of_data = 2026) {

  # for tph and routes cols, add a year suffix
  tph_routes_colnames <- names(bustrips_lsoa21)[grepl("^tph|^routes", names(bustrips_lsoa21))]
  tph_routes_colnames_year <- paste0(tph_routes_colnames, "_", year_of_data)

  # set these names to new suffixed data
  colnames(bustrips_lsoa21)[grepl("^tph|^routes", names(bustrips_lsoa21))] <- tph_routes_colnames_year

  return(bustrips_lsoa21)

}


calculate_tph_change <- function(bustrips_lsoa21, year1 = 2010, year2 = 2026,
                                 change_label = paste0(year1, "_", year2)) {

  # Output name stem = input name stem
  periods <- c(morning   = "morning_peak",
               midday    = "midday",
               afternoon = "afternoon_peak",
               evening   = "evening",
               night     = "night",
               daytime   = "daytime_mean")

  cols1 <- paste0("tph_weekday_", periods, "_", year1)
  cols2 <- paste0("tph_weekday_", periods, "_", year2)

  # Fail early and clearly if any expected column is missing
  missing <- setdiff(c(cols1, cols2), names(bustrips_lsoa21))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "))
  }

  for (i in seq_along(periods)) {
    new_col <- paste0("weekday_", names(periods)[i], "_change_", change_label)
    new_col_pct <- paste0("weekday_", names(periods)[i], "_change_pct_", change_label)
    bustrips_lsoa21 <- bustrips_lsoa21 |>
      mutate("{new_col}" := .data[[cols2[i]]] - .data[[cols1[i]]]) |>
      mutate("{new_col_pct}" := (.data[[cols2[i]]] - .data[[cols1[i]]]) / .data[[cols1[i]]])
  }

  bustrips_lsoa21
}


add_london_metro_lsoas <- function(bustrips_lsoa21) {

  # make lsoa to london underground lookup
  london_metro_lsoa21 <- identify_london_metro_lsoa21()

  # add to lsoa data
  bustrips_lsoa21 <- left_join(bustrips_lsoa21, london_metro_lsoa21, by = "lsoa21")

  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    mutate(london_underground = ifelse(is.na(london_underground), FALSE, london_underground))

}


# add rurality from ONSPD
add_lsoa21_rurality <- function(bustrips_lsoa21, onspd) {

  lsoa_rurality <- onspd %>%
    group_by(lsoa21,
             ru11ind) %>%
    summarise(postcode_count = n()) %>%
    group_by(lsoa21) %>%
    slice_max(postcode_count,
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    select(lsoa21,
           ru11ind) %>%
    filter(grepl("^E|^W", substring(lsoa21, 1, 1)))

  rurality_lookup <- data.frame(ru11ind = c("A1", "B1", "C1", "C2", "D1", "D2", "E1", "E2", "F1", "F2"),
                                urban_rural_cat = c("Urban: Major Conurbation",
                                                    "Urban: Minor Conurbation",
                                                    "Urban: City and Town",
                                                    "Urban: City and Town in a Sparse Setting",
                                                    "Rural: Town and Fringe",
                                                    "Rural: Town and Fringe in a Sparse Setting",
                                                    "Rural: Village",
                                                    "Rural: Village in a Sparse Setting",
                                                    "Rural: Hamlets and Isolated Dwellings",
                                                    "Rural: Hamlets and Isolated Dwellings in a Sparse Setting"),
                                rurality_3fold = c("Urban: Major city",
                                                   "Urban: Major city",
                                                   "Urban: Smaller city and Town",
                                                   "Urban: Smaller city and Town",
                                                   "Rural",
                                                   "Rural",
                                                   "Rural",
                                                   "Rural",
                                                   "Rural",
                                                   "Rural"),
                                rurality = c("Urban: Conurbation",
                                             "Urban: Conurbation",
                                             "Urban: City and Town",
                                             "Urban: City and Town",
                                             "Rural: Town and Fringe",
                                             "Rural: Town and Fringe",
                                             "Rural: Village/Hamlets/Isolated Dwellings",
                                             "Rural: Village/Hamlets/Isolated Dwellings",
                                             "Rural: Village/Hamlets/Isolated Dwellings",
                                             "Rural: Village/Hamlets/Isolated Dwellings"))

  lsoa_rurality <- left_join(lsoa_rurality, rurality_lookup, by = "ru11ind")
  #table(substring(lsoa_rurality$lsoa11, 1, 1),
  #      lsoa_rurality$rurality,
  #      useNA = "ifany")

  trips_lsoa  <- left_join(lsoa_rurality, bustrips_lsoa21, by = "lsoa21")

}


add_population_lsoa21 <- function(df_lsoa21) {

  # add LSOA population to do pop-weighted averages
  source("../social-demographic-data/scripts/population/population-and-age-bands.R")
  pop_lsoa21 <- get_smallarea_population_by_ageband("lsoa21")
  pop_lsoa21 <- pop_lsoa21 |>
    select(lsoa21,
           population = all_ages)

  df_lsoa21_with_pop <- left_join(df_lsoa21, pop_lsoa21, by = "lsoa21")

}

map_lsoa21_rurality <- function() {

  lsoa_rurality_sf <- left_join(lsoa_boundaries, lsoa_rurality, by = "lsoa21")
  st_write(lsoa_rurality_sf,
           dsn = "../geography-lookups/derived-lookups/lsoa-rurality.gpkg",
           delete_dsn = TRUE)

}
