
# read in local authority bus data - appear to include all 2023 boundaries. All good there.
load_la_bustrips <- function() {

  # Old block of code to combine all individual files. Could probably delete now...
  # # read in all data
  # all_la_filepaths <- list.files(path = "data",
  #                                pattern = "trips_per_la_by_mode_20",
  #                                full.names = TRUE)
  # all_la_filepaths <- all_la_filepaths[all_la_filepaths != "data/trips_per_la_by_mode_2004_2023.Rds"]
  #
  # trips_la <- list()
  #
  # for(f in 1:length(all_la_filepaths)) {
  #
  #   new <- readRDS(all_la_filepaths[f])
  #   year_value <- gsub("[.]Rds", "", gsub("data/trips_per_la_by_mode_", "", all_la_filepaths[f]))
  #   new <- new %>%
  #     #filter(route_type == 3) %>%
  #     mutate(year = as.numeric(year_value)) %>%
  #     select(zone_id,
  #            route_type,
  #            year,
  #            everything())
  #
  #   trips_la[[f]] <- new
  # }
  #
  # trips_la <- bind_rows(trips_la)

  trips_la <- readRDS("data/trips_per_la_by_mode_2004_2023.Rds")

  # filter for buses only
  bustrips_la <- trips_la %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_la <- bustrips_la %>%
    filter(!is.na(zone_id))

  bustrips_la <- add_la_code(bustrips_la)

}

# join required geographical fields (existing data has LAD17 data) - do we need this
add_la_code <- function(la_bustrips) {

  la_list <- read.csv("../ons-geog-data/onspd/Documents/LA_UA names and codes UK as at 04_23.csv",
                      stringsAsFactors = FALSE)
  la_list <- la_list[1:2]
  la_list <- rename(la_list, zone_id = "LAD23NM")

  la_bustrips <- left_join(la_bustrips, la_list, by = "zone_id")
  la_bustrips <- la_bustrips %>%
    select(LAD23CD,
           zone_id,
           year,
           everything())

  if(any(is.na(la_bustrips$LAD23CD))) {
    message("ERROR: some local authorities missing GSS code.")
  }

  return(la_bustrips)

}

add_la_code_2 <- function(la_bustrips) {

  la_list <- read.csv("../ons-geog-data/onspd/Documents/LA_UA names and codes UK as at 04_23.csv",
                      stringsAsFactors = FALSE)
  la_list <- la_list[1:2]

  la_bustrips <- left_join(la_bustrips, la_list, by = "LAD23NM")
  la_bustrips <- la_bustrips %>%
    select(LAD23CD,
           LAD23NM,
           year,
           everything())

  if(any(is.na(la_bustrips$LAD23CD))) {
    message("ERROR: some local authorities missing GSS code.")
  }

  return(la_bustrips)

}

# clean data - identifying locations and years with incomplete data and interpolating
make_la_bustrips <- function() {

  la_bustrips_cleaned <- make_clean_la_bustrips_data()
  la_bustrips_cleaned <- add_la_code_2(la_bustrips_cleaned)
  la_bustrips_cleaned <- add_region_by_la_code(la_bustrips_cleaned)
  la_bustrips_cleaned <- join_metro_by_la_lup(la_bustrips_cleaned)
  la_bustrips_cleaned <- la_bustrips_cleaned %>%
    select(LAD23CD,
           LAD23NM,
           RGN20CD,
           RGN20NM,
           CAUTH23CD,
           CAUTH23NM,
           everything())

  }
