
# read in lsoa bus data
load_la_bustrips <- function() {

  # read in all data
  trips_la <- readRDS("data/trips_per")

  # filter for buses only
  bustrips_la <- trips_la %>%
    filter(route_type == 3)

  # remove any data not associated with a lsoa
  bustrips_la <- bustrips_la %>%
    filter(!is.na(zone_id))


}

# join required geographical fields (existing data has LAD17 data) - do we need this
la_bustrips <- load_la_bustrips()





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
