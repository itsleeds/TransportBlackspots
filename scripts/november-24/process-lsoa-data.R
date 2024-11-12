#' Script that produces analysis of london on the tube, off the tube and rest of
#' the country for all years and for all times of the data.
#'
#' Processing needs to omit any outliers in the lsoa data first

# read in lsoa bus data
load_lsoa_bustrips <- function(onspd, year_list) {

  # read in all data
  trips_lsoa <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")

  # filter for buses only
  bustrips_lsoa <- trips_lsoa %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_lsoa <- bustrips_lsoa %>%
    filter(!is.na(zone_id))

  # select year
  bustrips_lsoa <- bustrips_lsoa %>%
    filter(year %in% year_list)

  # identify london underground lsoas
  bustrips_lsoa <- add_london_metro_lsoas(bustrips_lsoa, trips_lsoa)

  # identify rurality (keeps only england and wales LSOAs)
  bustrips_lsoa <- identify_lsoa_rurality(bustrips_lsoa, onspd)

  # keep fields of interest only
  bustrips_lsoa <- bustrips_lsoa %>%
    select(lsoa11 = zone_id,
           route_type,
           year,
           london_underground,
           ru11ind,
           urban_rural_cat,
           rurality,
           #routes_Morning_Peak,
           #routes_Midday,
           #routes_Afternoon_Peak,
           #routes_Evening,
           #routes_Night,
           #runs_weekday_Morning_Peak,
           #runs_weekday_Midday,
           #runs_weekday_Afternoon_Peak,
           #runs_weekday_Evening,
           #runs_weekday_Night,
           #runs_Sat_Morning_Peak,
           #runs_Sat_Midday,
           #runs_Sat_Afternoon_Peak,
           #runs_Sat_Evening,
           #runs_Sat_Night,
           #runs_Sun_Morning_Peak,
           #runs_Sun_Midday,
           #runs_Sun_Afternoon_Peak,
           #runs_Sun_Evening,
           #runs_Sun_Night,
           tph_weekday_Morning_Peak,
           tph_weekday_Midday,
           tph_weekday_Afternoon_Peak,
           tph_weekday_Evening,
           tph_weekday_Night,
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
           tph_daytime_avg,
           #LAD17NM,
           #RGN11NM,
           )

  # keep only england and wales LSOAs
  # lsoa_list <- onspd %>%
  #   distinct(lsoa11) %>%
  #   filter(grepl("^E|^W", substring(lsoa11, 1, 1)))

  # bustrips_lsoa <- left_join(lsoa_list, bustrips_lsoa, by = "lsoa11")

  return(bustrips_lsoa)

}

#' identify london underground lsoas
identify_london_metro_lsoas <- function(trips_lsoa) {

  metro_trips <- trips_lsoa %>%
    filter(route_type == 1)

  #check which years have most data and keep that year
  max_year <- metro_trips %>%
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    slice_max(year, n = 1, with_ties = FALSE)

  # keep only lsoas for year with most LSOAs
  # and only those on the london underground area (i.e. not Tyneside metro)
  london_metro_lsoas <- metro_trips %>%
    filter(year == max_year$year)  %>%
    filter(RGN11NM %in% c("London")) %>%
    transmute(zone_id,
              #route_type,
              #year,
              #LAD17NM,
              #RGN11NM,
              london_underground = TRUE)

}

add_london_metro_lsoas <- function(bustrips_lsoa, trips_lsoa) {

  # make lsoa to london underground lookup
  london_metro_lsoas <- identify_london_metro_lsoas(trips_lsoa)

  # add to lsoa data
  bustrips_lsoa <- left_join(bustrips_lsoa, london_metro_lsoas, by = "zone_id")

  bustrips_lsoa <- bustrips_lsoa %>%
    mutate(london_underground = ifelse(is.na(london_underground), FALSE, london_underground))

}


# add rurality from ONSPD
identify_lsoa_rurality <- function(trips_lsoa, onspd) {

  lsoa_rurality <- onspd %>%
    group_by(lsoa11,
             ru11ind) %>%
    summarise(postcode_count = n()) %>%
    group_by(lsoa11) %>%
    slice_max(postcode_count,
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    select(lsoa11,
           ru11ind) %>%
    filter(grepl("^E|^W", substring(lsoa11, 1, 1)))

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
    rename(zone_id = lsoa11)

  trips_lsoa  <- left_join(lsoa_rurality, trips_lsoa, by = "zone_id")

}
