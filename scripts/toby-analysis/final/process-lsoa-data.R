#' Script that produces analysis of london on the tube, off the tube and rest of
#' the country for all years and for all times of the data.
#'
#' Processing needs to omit any outliers in the lsoa data first

# read in lsoa bus data
load_lsoa_bustrips <- function() {

  # read in all data
  trips_lsoa <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")

  # filter for buses only
  bustrips_lsoa <- trips_lsoa %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_lsoa <- bustrips_lsoa %>%
    filter(!is.na(zone_id))

  # rename fields
  #bustrips_lsoa <- bustrips_lsoa %>%
  #  rename(lsoa11 = zone_id)

  # identify london underground lsoas
  bustrips_lsoa <- add_london_metro_lsoas(bustrips_lsoa, trips_lsoa)

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

# join required geographical fields (existing data has LAD17 data) - do we need this
#lsoa_bustrips <- load_lsoa_bustrips()

#table(lsoa_bustrips$RGN11NM,
#      lsoa_bustrips$london_underground,
#      useNA = "ifany")

# identify outliers and remove from the data

# then find average value in each year by london and non london
# then make bivariate map
