# get individual year data for LSOA21
load_lsoa21_bustrips <- function(data_year = 2007) {

  # read in all data
  rds_file_name <- paste0("data/trips_per_lsoa21_by_mode_", data_year, ".Rds")
  trips_lsoa21 <- readRDS(rds_file_name)

  # filter for buses only
  bustrips_lsoa21 <- trips_lsoa21 %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    filter(!is.na(zone_id))

  # identify rurality (keeps only england and wales LSOAs)
  bustrips_lsoa21 <- identify_lsoa_rurality(bustrips_lsoa21, onspd, lsoa_code = lsoa21)

  # remove gaps in colnames
  old_names <- names(bustrips_lsoa21)
  new_names <- gsub(" ", "_", old_names)
  colnames(bustrips_lsoa21) <- new_names

  # keep fields of interest only
  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    transmute(lsoa21 = zone_id,
              route_type,
              year = data_year,
              #london_underground,
              ru11ind,
              urban_rural_cat,
              rurality,
              routes_Morning_Peak,
              routes_Midday,
              routes_Afternoon_Peak,
              routes_Evening,
              routes_Night,

              tph_weekday_Morning_Peak = round((tph_Mon_Morning_Peak + tph_Tue_Morning_Peak + tph_Wed_Morning_Peak + tph_Thu_Morning_Peak + tph_Fri_Morning_Peak) / 5, 1),
              tph_weekday_Midday = round((tph_Mon_Midday + tph_Tue_Midday + tph_Wed_Midday + tph_Thu_Midday + tph_Fri_Midday) / 5, 1),
              tph_weekday_Afternoon_Peak = round((tph_Mon_Afternoon_Peak + tph_Tue_Afternoon_Peak + tph_Wed_Afternoon_Peak + tph_Thu_Afternoon_Peak + tph_Fri_Afternoon_Peak) / 5, 1),
              tph_weekday_Evening = round((tph_Mon_Evening + tph_Tue_Evening + tph_Wed_Evening + tph_Thu_Evening + tph_Fri_Evening) / 5, 1),
              tph_weekday_Night = round((tph_Mon_Night + tph_Tue_Night + tph_Wed_Night + tph_Thu_Night + tph_Fri_Night) / 5, 1),

              tph_Sat_Morning_Peak,
              tph_Sat_Midday,
              tph_Sat_Afternoon_Peak,
              tph_Sat_Evening,
              tph_Sat_Night,
              tph_Sun_Morning_Peak,
              tph_Sun_Midday,
              tph_Sun_Afternoon_Peak,
              tph_Sun_Evening,
              tph_Sun_Night,

    )

  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    mutate(max_number_routes = pmax(routes_Morning_Peak, routes_Midday, routes_Afternoon_Peak, routes_Evening, routes_Night)) %>%
    select(-routes_Morning_Peak,
           -routes_Midday,
           -routes_Afternoon_Peak,
           -routes_Evening,
           -routes_Night)

  return(bustrips_lsoa21)

}


# get individual year data for LSOA21
load_lsoa21_bustrips_newbands <- function(data_year = 2007) {

  # read in all data
  rds_file_name <- paste0("data/trips_per_lsoa21_by_mode_newbands_", data_year, ".Rds")
  trips_lsoa21 <- readRDS(rds_file_name)

  # filter for buses only
  bustrips_lsoa21 <- trips_lsoa21 %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    filter(!is.na(zone_id))

  # identify rurality (keeps only england and wales LSOAs)
  bustrips_lsoa21 <- identify_lsoa_rurality(bustrips_lsoa21, onspd, lsoa_code = lsoa21)

  # remove gaps in colnames
  old_names <- names(bustrips_lsoa21)
  new_names <- gsub("Peak", "_Peak", old_names)
  colnames(bustrips_lsoa21) <- new_names

  # keep fields of interest only
  bustrips_lsoa21 <- bustrips_lsoa21 %>%
    transmute(lsoa21 = zone_id,
              route_type,
              year = data_year,
              #london_underground,
              ru11ind,
              urban_rural_cat,
              rurality,
              # routes_Morning_Peak,
              # routes_Midday,
              # routes_Afternoon_Peak,
              # routes_LateEvening,
              # routes_Evening,
              # routes_Night,

              # tph_Mon_Afternoon_Peak,
              # tph_Mon_Evening,
              # tph_Mon_LateEvening,
              # tph_Mon_Midday,
              # tph_Mon_Morning_Peak,
              # tph_Mon_Night,
              # tph_Tue_Afternoon_Peak,
              # tph_Tue_Evening,
              # tph_Tue_LateEvening,
              # tph_Tue_Midday,
              # tph_Tue_Morning_Peak,
              # tph_Tue_Night,
              # tph_Wed_Afternoon_Peak,
              # tph_Wed_Evening,
              # tph_Wed_LateEvening,
              # tph_Wed_Midday,
              # tph_Wed_Morning_Peak,
              # tph_Wed_Night,
              # tph_Thu_Afternoon_Peak,
              # tph_Thu_Evening,
              # tph_Thu_LateEvening,
              # tph_Thu_Midday,
              # tph_Thu_Morning_Peak,
              # tph_Thu_Night,
              # tph_Fri_Afternoon_Peak,
              # tph_Fri_Evening,
              # tph_Fri_LateEvening,
              # tph_Fri_Midday,
              # tph_Fri_Morning_Peak,
              # tph_Fri_Night,

              tph_weekday_Morning_Peak = round((tph_Mon_Morning_Peak + tph_Tue_Morning_Peak + tph_Wed_Morning_Peak + tph_Thu_Morning_Peak + tph_Fri_Morning_Peak) / 5, 1),
              tph_weekday_Midday = round((tph_Mon_Midday + tph_Tue_Midday + tph_Wed_Midday + tph_Thu_Midday + tph_Fri_Midday) / 5, 1),
              tph_weekday_Afternoon_Peak = round((tph_Mon_Afternoon_Peak + tph_Tue_Afternoon_Peak + tph_Wed_Afternoon_Peak + tph_Thu_Afternoon_Peak + tph_Fri_Afternoon_Peak) / 5, 1),
              tph_weekday_Evening = round((tph_Mon_Evening + tph_Tue_Evening + tph_Wed_Evening + tph_Thu_Evening + tph_Fri_Evening) / 5, 1),
              tph_weekday_LateEvening = round((tph_Mon_LateEvening + tph_Tue_LateEvening + tph_Wed_LateEvening + tph_Thu_LateEvening + tph_Fri_LateEvening) / 5, 1),
              tph_weekday_Night = round((tph_Mon_Night + tph_Tue_Night + tph_Wed_Night + tph_Thu_Night + tph_Fri_Night) / 5, 1),

              tph_Sat_Morning_Peak,
              tph_Sat_Midday,
              tph_Sat_Afternoon_Peak,
              tph_Sat_Evening,
              tph_Sat_LateEvening,
              tph_Sat_Night,
              tph_Sun_Morning_Peak,
              tph_Sun_Midday,
              tph_Sun_Afternoon_Peak,
              tph_Sun_Evening,
              tph_Sun_LateEvening,
              tph_Sun_Night,
  )

  # bustrips_lsoa21 <- bustrips_lsoa21 %>%
  #   mutate(max_number_routes = pmax(routes_Morning_Peak, routes_Midday, routes_Afternoon_Peak, routes_Evening, routes_Night)) %>%
  #   select(-routes_Morning_Peak,
  #          -routes_Midday,
  #          -routes_Afternoon_Peak,
  #          -routes_Evening,
  #          -routes_Night)

  return(bustrips_lsoa21)

}


# Simplify time period data
simplify_time_periods <- function(lsoa_bustrips, new = FALSE) {

  if(!new) {
    lsoa_bustrips_simplified <- lsoa_bustrips %>%
      transmute(lsoa21,
                year,
                rurality,
                urban_rural_cat,
                tph_weekday_daytime = round(((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12, 2),
                tph_weekday_Evening,
                tph_weekday_Night,
                tph_Sat_daytime = round(((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12, 2),
                tph_Sat_Evening,
                tph_Sat_Night,
                tph_Sun_daytime = round(((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12, 2),
                tph_Sun_Evening,
                tph_Sun_Night)
  }
  if(new) {
    lsoa_bustrips_simplified <- lsoa_bustrips %>%
      transmute(lsoa21,
                year,
                rurality,
                urban_rural_cat,
                tph_weekday_daytime = round(((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12, 2),
                tph_weekday_Evening = round(((tph_weekday_Evening * 4) + (tph_weekday_LateEvening * 2)) / 6, 2),
                tph_weekday_Night,
                tph_Sat_daytime = round(((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12, 2),
                tph_Sat_Evening = round(((tph_Sat_Evening * 4) + (tph_Sat_LateEvening * 2)) / 6, 2),
                tph_Sat_Night,
                tph_Sun_daytime = round(((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12, 2),
                tph_Sun_Evening = round(((tph_Sun_Evening * 4) + (tph_Sun_LateEvening * 2)) / 6, 2),
                tph_Sun_Night)
  }

  return(lsoa_bustrips_simplified)

}

#' this function takes four years worth of historical data, combines into one file,
#' simplifies the 15 time periods into 9, and then calculates the average trips per hour
#' over the four years (2007-2010) to provide a baseline average
make_simplified_bustrips_lsoa21_200710 <- function() {

  # get individual year data
  lsoa21_bustrips_07 <- load_lsoa21_bustrips(data_year = 2007)
  lsoa21_bustrips_08 <- load_lsoa21_bustrips(data_year = 2008)
  lsoa21_bustrips_09 <- load_lsoa21_bustrips(data_year = 2009)
  lsoa21_bustrips_10 <- load_lsoa21_bustrips(data_year = 2010)

  # combine
  lsoa21_bustrips_0710 <- bind_rows(lsoa21_bustrips_07,
                                    lsoa21_bustrips_08,
                                    lsoa21_bustrips_09,
                                    lsoa21_bustrips_10)

  lsoa21_bustrips_0710 <- lsoa21_bustrips_0710 %>%
    mutate(year = "2007-10")

    # calculate simplified time periods, combining the three daytime periods between 6am and 6pm into one
  lsoa21_bustrips_simplified_0710 <- simplify_time_periods(lsoa21_bustrips_0710)

  # aggregate and calculate average across 2007-10 to get a baseline tph in each LSOA
  # generate list of cols to aggregate
  tph_cols <- c("tph_weekday_daytime",
                "tph_weekday_Evening",
                "tph_weekday_Night",
                "tph_Sat_daytime",
                "tph_Sat_Evening",
                "tph_Sat_Night",
                "tph_Sun_daytime",
                "tph_Sun_Evening",
                "tph_Sun_Night")
  # aggregation process
  lsoa21_bustrips_simplified_0710 <- lsoa21_bustrips_simplified_0710 %>%
    group_by(lsoa21,
             year = "2007-10",
             rurality,
             urban_rural_cat) %>%
    summarise(across(all_of(tph_cols), \(x) mean(x, na.rm = TRUE))) %>%
    ungroup()

  return(lsoa21_bustrips_simplified_0710)

}

make_simplified_bustrips_lsoa21_single_year <- function(data_year, new = FALSE) {

  # get single years worth of bus data
  if(!new) {
    lsoa21_bustrips <- load_lsoa21_bustrips(data_year)
  }
  if(new) {
    lsoa21_bustrips <- load_lsoa21_bustrips_newbands(data_year)
  }
  # simplify
  lsoa21_bustrips_simplified <- simplify_time_periods(lsoa21_bustrips, new = new)

}

# add rurality from ONSPD
identify_lsoa_rurality <- function(trips_lsoa, onspd, lsoa_code = lsoa11) {

  lsoa_rurality <- onspd %>%
    group_by({{ lsoa_code }},
             ru11ind) %>%
    summarise(postcode_count = n()) %>%
    group_by({{ lsoa_code }}) %>%
    slice_max(postcode_count,
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    select({{ lsoa_code }},
           ru11ind) %>%
    filter(grepl("^E|^W", substring({{ lsoa_code }}, 1, 1)))

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

  lsoa_rurality <- lsoa_rurality %>%
    rename(zone_id = {{ lsoa_code }})

  trips_lsoa  <- left_join(lsoa_rurality, trips_lsoa, by = "zone_id")

  trips_lsoa$rurality <- factor(trips_lsoa$rurality,
                                levels = c("Urban: Conurbation",
                                           "Urban: City and Town",
                                           "Rural: Town and Fringe",
                                           "Rural: Village/Hamlets/Isolated Dwellings"))

  return(trips_lsoa)

}

# convert wide table into long table - easier to process
make_bustrips_long <- function(bustrips) {

  bustrips_long <- bustrips %>%
    gather(value = tph,
           key = time_period,
           -lsoa21,
           -year,
           -rurality,
           -urban_rural_cat) %>%
    mutate(time_period = gsub("tph_", "", time_period))

}
