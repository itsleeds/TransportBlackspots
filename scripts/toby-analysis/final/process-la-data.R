
# read in local authority bus data - appear to include all 2023 boundaries. All good there.
load_la_bustrips <- function() {

  # read in all data
  all_la_filepaths <- list.files(path = "data",
                                 pattern = "trips_per_la_by_mode_20",
                                 full.names = TRUE)
  all_la_filepaths <- all_la_filepaths[all_la_filepaths != "data/trips_per_la_by_mode_2004_2023.Rds"]

  trips_la <- list()

  for(f in 1:length(all_la_filepaths)) {

    new <- readRDS(all_la_filepaths[f])
    year_value <- gsub("[.]Rds", "", gsub("data/trips_per_la_by_mode_", "", all_la_filepaths[f]))
    new <- new %>%
      #filter(route_type == 3) %>%
      mutate(year = as.numeric(year_value)) %>%
      select(zone_id,
             route_type,
             year,
             everything())

    trips_la[[f]] <- new
  }

  trips_la <- bind_rows(trips_la)

  #trips_la <- readRDS("data/trips_per_la_by_mode_2004_2023.Rds")

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


# now lets summarise at regional level.



# and make a loop that saves graphs for all regions for all times of the week



summarise_trips_by_geog <- function(trips_data,
                                    runs_field = runs_weekday_Morning_Peak,
                                    post_pandemic = "clean",
                                    geog_field) {

  trips_summary <- trips_data %>%
    group_by({{ geog_field }},
             year) %>%
    summarise(runs = sum({{ runs_field }}, na.rm = TRUE)) %>%
    group_by({{ geog_field }}) %>%
    mutate(runs_pct_max = runs / max(runs),
           runs_zscore = (runs - mean(runs)) / sd(runs)) %>%
    ungroup()

  trips_summary_wide <- trips_summary %>%
    gather(key = indicator,
           value = val,
           -{{ geog_field }},
           -year) %>%
    unite(indicator_year, indicator, year, sep = "_") %>%
    spread(key = indicator_year,
           value = val,
           fill = 0)

  trips_summary_status <- trips_summary %>%
    mutate(status = case_when(year == 2020 ~ "Pandemic",
                              runs_zscore <= -1 ~ "Incomplete data", # more than one standard deviation below zscore
                              runs_pct_max <= 0.25 ~ "Incomplete data", # below 25% of peak
                              runs_pct_max <= 0.4 & runs_zscore <= -0.7 ~ "Incomplete data", # capture some anomalies
                              year < 2008 & runs_pct_max < 0.6 ~ "Incomplete data",
                              between(runs_pct_max, 0.25, 0.5) ~ "Possibly incomplete data",
                              TRUE ~ ""))

  # do not clean post pandemic data and assume is complete, if post_pandemic == "keep"
  if(post_pandemic == "keep") {
    trips_2004_2022_status <- trips_2004_2022_status %>%
      mutate(status = ifelse(year > 2020, "", status))
  }

  return(trips_summary_status)

}
