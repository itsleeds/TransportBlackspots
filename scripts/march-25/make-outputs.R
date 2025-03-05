# SUMMARISE DATA BY HIGHER GEOGRAPHIES ------------------------------------

# summary stats between rural and urban, london and not london
make_london_rurality_summary <- function(bus_lsoa,
                                         time_period_name = "weekday_daytime",
                                         year) {

  # convert wide table to long table
  bus_lsoa_long <- make_bustrips_long(bus_lsoa)
  # add geography by lsoa21 lookups
  bus_lsoa_long <- add_geography21(bus_lsoa_long)

  # simplify region to london, not london and urban/rural
  bus_lsoa_long <- bus_lsoa_long %>%
    mutate(london_code = ifelse(region_name == "London", "L01", "L02"),
           london = ifelse(region_name == "London", "London", "Outside London")) %>%
    mutate(rural_urban_2x = ifelse(grepl("Rural", rurality), "Rural", "Urban"))

  # remove the 8 LSOAs that are rural London
  bus_lsoa_long <- bus_lsoa_long %>%
    filter(!(rural_urban_2x == "Rural" & london == "London"))

  # summarise the data for daytime weekdays by rurality and region
  london_rural <- summarise_tph_by_geog(bus_lsoa = bus_lsoa_long,
                                        geog_code = london_code,
                                        geog_name = london,
                                        filter_by = time_period_name,
                                        rural_urban_2x,
                                        time_period)
  london_rural <- london_rural %>%
    rename("tph_mean_{{ year }}" := tph_mean)

  london_rural <- london_rural %>%
    filter(!grepl("Night", time_period))

}

make_rurality_comparison_2010_2024 <- function(bus_lsoa_2010,
                                               bus_lsoa_2024,
                                               time_period_filter = FALSE) {

  london_2010 <- make_london_rurality_summary(lsoa21_buses_2010_new,
                                              time_period_name = time_period_filter,
                                              year = `2010`)
  london_2024 <- make_london_rurality_summary(lsoa21_buses_2024_new,
                                              time_period_name = time_period_filter,
                                              year = `2024`)

  london_2010_2024_comparison <- inner_join(london_2010, london_2024, by = c("london_code", "london", "rural_urban_2x", "time_period"))

  london_2010_2024_comparison <- london_2010_2024_comparison %>%
    mutate(tph_change_2010_2024 = tph_mean_2024 - tph_mean_2010) %>%
    mutate(tph_change_2010_2024_pct = round(tph_change_2010_2024 / tph_mean_2010, 4))

}


# region summaries --------------------------------------------------------
summarise_tph_by_geog <- function(bus_lsoa, geog_code, geog_name, filter_by = FALSE, ...) {

  bus_lsoa_summary <- bus_lsoa %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             ...) %>%
    summarise(tph_mean = round(mean(tph, na.rm = TRUE), 1)) %>%
    ungroup()

  if(filter_by != FALSE) {
    bus_lsoa_summary <- bus_lsoa_summary %>%
      filter(time_period == filter_by)
  }

  return(bus_lsoa_summary)

}


make_regional_rurality_daytime_weekday_summary <- function(bus_lsoa21) {

  # convert wide table to long table
  bus_lsoa21_long <- make_bustrips_long(bus_lsoa21)
  # add geography by lsoa21 lookups
  bus_lsoa21_long <- add_geography21(bus_lsoa21_long)

  # summarise the data for daytime weekdays by rurality and region
  daytime_region_rurality <- summarise_tph_by_geog(bus_lsoa = bus_lsoa21_long,
                                                   geog_code = rgn,
                                                   geog_name = region_name,
                                                   filter_by = "weekday_daytime",
                                                   rurality,
                                                   time_period)

  # summarise the data for daytime weekdays by region (i.e. a totals section)
  daytime_region_all <- summarise_tph_by_geog(bus_lsoa = bus_lsoa21_long,
                                              geog_code = rgn,
                                              geog_name = region_name,
                                              filter_by = "weekday_daytime",
                                              time_period)

  # combine the two tables
  daytime_region_rurality <- bind_rows(daytime_region_rurality,
                                       daytime_region_all)

  # complete rurality for 'all neighbourhoods'
  levels(daytime_region_rurality$rurality) <- c(levels(daytime_region_rurality$rurality), "All neighbourhoods")
  daytime_region_rurality$rurality[is.na(daytime_region_rurality$rurality)] <-  "All neighbourhoods"

  # spread to a more digestible format
  daytime_region_rurality <- daytime_region_rurality %>%
    select(-time_period) %>%
    spread(key = rurality,
           value = tph_mean,
           fill = 0)


}


make_regional_summary <- function(bus_lsoa21) {

  # convert wide table to long table
  bus_lsoa21_long <- make_bustrips_long(bus_lsoa21)
  # add geography by lsoa21 lookups
  bus_lsoa21_long <- add_geography21(bus_lsoa21_long)

  # summarise the data for daytime weekdays by rurality and region
  region_all <- summarise_tph_by_geog(bus_lsoa = bus_lsoa21_long,
                                      geog_code = rgn,
                                      geog_name = region_name,
                                      filter_by = FALSE,
                                      time_period)

  # spread to a more digestible format
  region_all <- region_all %>%
    filter(!grepl("Night", time_period)) %>%
    spread(key = time_period,
           value = tph_mean,
           fill = 0) %>%
    select(rgn,
           region_name,
           weekday_daytime,
           weekday_Evening,
           Sat_daytime,
           Sat_Evening,
           Sun_daytime,
           Sun_Evening)
}


t1_regional_all_trends <- function(lsoa21_buses_200710,
                                   lsoa21_buses_2024) {

  #get regional summary for 2010
  regional_200710 <- make_regional_summary(lsoa21_buses_200710)
  colnames(regional_200710)[3:8] <- paste0(names(regional_200710)[3:8], "_2010")

  # get regional summary for 2024
  regional_2024 <- make_regional_summary(lsoa21_buses_2024)
  colnames(regional_2024)[3:8] <- paste0(names(regional_2024)[3:8], "_2024")

  # join and calculate difference in % terms
  regional_comp_2010_2024 <- inner_join(regional_200710, regional_2024, by = c("rgn", "region_name"))
  regional_comp_2010_2024 <- regional_comp_2010_2024 %>%
    mutate(weekday_daytime_change_2010_2024 = weekday_daytime_2024 - weekday_daytime_2010,
           weekday_evening_change_2010_2024 = weekday_Evening_2024 - weekday_Evening_2010,
           sat_daytime_change_2010_2024 = Sat_daytime_2024 - Sat_daytime_2010,
           sat_evening_change_2010_2024 = Sat_Evening_2024 - Sat_Evening_2010,
           Sun_daytime_change_2010_2024 = Sun_daytime_2024 - Sun_daytime_2010,
           Sun_evening_change_2010_2024 = Sun_Evening_2024 - Sun_Evening_2010) %>%
    transmute(rgn,
              region_name,
              `Weekday daytime 2010-2024 (% change)` = round(weekday_daytime_change_2010_2024 / weekday_daytime_2010, 4),
              `Weekday evening 2010-2024 (% change)` = round(weekday_evening_change_2010_2024 / weekday_Evening_2010, 4),
              `Saturday daytime 2010-2024 (% change)` = round(sat_daytime_change_2010_2024 / Sat_daytime_2010, 4),
              `Saturday evening 2010-2024 (% change)` = round(sat_evening_change_2010_2024 / Sat_Evening_2010, 4),
              `Sunday daytime 2010-2024 (% change)` = round(Sun_daytime_change_2010_2024 / Sun_daytime_2010, 4),
              `Sunday evening 2010-2024 (% change)` = round(Sun_evening_change_2010_2024 / Sun_Evening_2010, 4))

}

# local authority summary -------------------------------------------------

# Table 2: local authorities with the greatest reduction in weekday daytime services since 2010
t2_la_daytime_trends <- function(bus_lsoa21_2010,
                                 bus_lsoa21_2024) {

  # convert wide table to long table

  bus_lsoa21_2010_long <- make_bustrips_long(bus_lsoa21_2010)
  bus_lsoa21_2024_long <- make_bustrips_long(bus_lsoa21_2024)
  # add geography by lsoa21 lookups
  bus_lsoa21_2010_long <- add_geography21(bus_lsoa21_2010_long)
  bus_lsoa21_2024_long <- add_geography21(bus_lsoa21_2024_long)

  # summarise the data for daytime weekdays by rurality and region
  daytime_la_2010 <- summarise_tph_by_geog(bus_lsoa = bus_lsoa21_2010_long,
                                           geog_code = oslaua,
                                           geog_name = local_authority,
                                           filter_by = "weekday_daytime",
                                           time_period)

  daytime_la_2024 <- summarise_tph_by_geog(bus_lsoa = bus_lsoa21_2024_long,
                                           geog_code = oslaua,
                                           geog_name = local_authority,
                                           filter_by = "weekday_daytime",
                                           time_period)

  daytime_la_2010 <- daytime_la_2010 %>%
    rename(tph_mean_2010 = tph_mean) %>%
    select(-time_period)
  daytime_la_2024 <- daytime_la_2024 %>%
    rename(tph_mean_2024 = tph_mean) %>%
    select(-time_period)

  daytime_la_2010_2024_comp <- inner_join(daytime_la_2010, daytime_la_2024, by = c("oslaua", "local_authority"))

  daytime_la_2010_2024_comp <- daytime_la_2010_2024_comp %>%
    mutate(tph_change_2010_2024 = tph_mean_2024 - tph_mean_2010) %>%
    mutate(tph_change_2010_2024_pct = tph_change_2010_2024 / tph_mean_2010) %>%
    arrange(tph_change_2010_2024_pct)

  # finalise column names
  daytime_la_2010_2024_comp <- daytime_la_2010_2024_comp %>%
    select(`LA code` = oslaua,
           `Local authority` = local_authority,
           `Average bus frequency 2010 (TPH)` = tph_mean_2010,
           `Average bus frequency 2024 (TPH)` = tph_mean_2024,
           `Change in average bus frequency 2010-24 (TPH)` = tph_change_2010_2024,
           `Change in average bus frequency 2010-24 (%)` = tph_change_2010_2024_pct)


  }


# QUINTILES OUTPUTS -------------------------------------------------------

# Table 3: Bus service frequencies at neighbourhood (LSOA) in 2024 for weekday daytime
#   (Note: this is the frequency of all buses within a neighbourhood or within 500m of the centre of the neighbourhood)


make_tph_quintiles <- function(bus_lsoa) {

  bus_lsoa_long <- make_bustrips_long(bus_lsoa)

  bus_lsoa_long <- bus_lsoa_long %>%
    mutate(tph = ifelse(is.na(tph), 0, tph)) %>%
    group_by(rurality,
             time_period) %>%
    mutate(tph_qt = ntile(desc(tph), n = 5)) %>%
    ungroup()

  bus_lsoa_long$tph_qt_id <- LETTERS[bus_lsoa_long$tph_qt]

  return(bus_lsoa_long)

}

rename_quintile_cols <- function(buses_qt_summary) {
  buses_qt_summary <- buses_qt_summary %>%
    select(`Rural urban classification` = rurality,
           `Worst 20%: Quintile 1` = weekday_daytime_E,
           `Quintile 2` = weekday_daytime_D,
           `Quintile 3` = weekday_daytime_C,
           `Quintile 4` = weekday_daytime_B,
           `Best 20%: Quintile 5` = weekday_daytime_A)
}

summarise_tph_quintiles <- function(bus_qt, filter_by = FALSE, stats = "mean", round_fig = 0) {

  bus_qt_summary <- bus_qt %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             time_period,
             #tph_qt,
             tph_qt_id) %>%
    summarise(tph_mean = round(mean(tph, na.rm = TRUE), round_fig),
              tph_min = round(min(tph, na.rm = TRUE), round_fig),
              tph_max = round(max(tph, na.rm = TRUE), round_fig)) %>%
    ungroup() %>%
    mutate(tph_stats = paste0(tph_mean, "\n(", tph_min, "-", tph_max, ")")) %>%
    select(-tph_min,
           -tph_max)

  if(filter_by != FALSE & stats == "mean") {
    bus_qt_summary <- bus_qt_summary %>%
      filter(time_period == filter_by) %>%
      select(-tph_stats) %>%
      unite(col_headings, time_period, tph_qt_id, sep = "_") %>%
      spread(key = col_headings,
             value = tph_mean,
             fill = 0)
  }
  if(filter_by != FALSE & stats == "all") {
    bus_qt_summary <- bus_qt_summary %>%
      filter(time_period == filter_by) %>%
      select(-tph_mean) %>%
      unite(col_headings, time_period, tph_qt_id, sep = "_") %>%
      spread(key = col_headings,
             value = tph_stats,
             fill = 0)
  }

  bus_qt_summary <- rename_quintile_cols(bus_qt_summary)

  return(bus_qt_summary)

}

t3_bus_quintiles <- function(lsoa21_buses_200710, lsoa21_buses_2024_new) {

  lsoa21_buses_2010_qt <- make_tph_quintiles(lsoa21_buses_200710)
  lsoa21_buses_2024_qt <- make_tph_quintiles(lsoa21_buses_2024_new)

  t3a_2010_daytime <<- summarise_tph_quintiles(lsoa21_buses_2010_qt,
                                               filter_by = "weekday_daytime",
                                               stats = "mean",
                                               round_fig = 1)

  t3b_2010_daytime_range <<- summarise_tph_quintiles(lsoa21_buses_2010_qt,
                                                     filter_by = "weekday_daytime",
                                                     stats = "all",
                                                     round_fig = 1)

  t3c_2024_daytime <<- summarise_tph_quintiles(lsoa21_buses_2024_qt,
                                               filter_by = "weekday_daytime",
                                               stats = "mean",
                                               round_fig = 1)

  t3d_2024_daytime_range <<- summarise_tph_quintiles(lsoa21_buses_2024_qt,
                                                     filter_by = "weekday_daytime",
                                                     stats = "all",
                                                     round_fig = 1)



}


# NEIGHBOURHOODS WITH NO BUS SERVICES DURING THE WEEK ---------------------

# identify number of neighbourhoods without any buses at different times of the week
# express as number of neighbourhoods and % of neighbourhoods?
t4_zero_tph_lsoas <- function(bus_lsoa21_2024, threshold = 1) {

  bus_lsoa21_2024_long <- make_bustrips_long(bus_lsoa21_2024)

  bus_lsoa21_2024_long <- bus_lsoa21_2024_long %>%
    mutate(tph = ifelse(is.na(tph), 0, tph)) %>%
    mutate(no_service = tph < threshold)

  no_service_summary <- bus_lsoa21_2024_long %>%
    group_by(rurality,
             time_period,
             no_service) %>%
    summarise(n = n()) %>%
    group_by(rurality,
             time_period) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(no_service) %>%
    rename(`No bus service (No. of neighbourhoods)` = n,
           `No bus service (% of neighbourhoods)` = pct) %>%
    filter(!grepl("Night", time_period))

  no_service_summary <- no_service_summary %>%
    select(-no_service) %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -time_period) %>%
    unite(full_indicator, time_period, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val,
           fill = 0)

  no_service_summary <- no_service_summary %>%
    select(`Rural urban classification` = rurality,
           `weekday_daytime: No bus service (No. of neighbourhoods)`,
           `weekday_daytime: No bus service (% of neighbourhoods)`,
           `weekday_Evening: No bus service (No. of neighbourhoods)`,
           `weekday_Evening: No bus service (% of neighbourhoods)`,
           `Sat_daytime: No bus service (No. of neighbourhoods)`,
           `Sat_daytime: No bus service (% of neighbourhoods)`,
           `Sat_Evening: No bus service (No. of neighbourhoods)`,
           `Sat_Evening: No bus service (% of neighbourhoods)`,
           `Sun_daytime: No bus service (No. of neighbourhoods)`,
           `Sun_daytime: No bus service (% of neighbourhoods)`,
           `Sun_Evening: No bus service (No. of neighbourhoods)`,
           `Sun_Evening: No bus service (% of neighbourhoods)`)

  # finalise column names for xlsx
  no_service_summary <- finalise_t4_colnames(no_service_summary)

  return(no_service_summary)

  }

finalise_t4_colnames <- function(t4) {

  # collect current names
  old_names <- names(t4)
  # tidy up common columns
  new_names <- gsub("_", " ", old_names)
  new_names <- paste0(toupper(substring(new_names, 1, 1)), substring(new_names, 2))

  # make geography names nicer
  new_names <- gsub("Pcon24 name", "Constituency", new_names)
  new_names <- gsub("Pcon24", "Constituency code", new_names)

  new_names <- gsub("Region name", "Region", new_names)
  new_names <- gsub("Rgn", "Region code", new_names)

  new_names <- gsub("Oslaua", "LA code", new_names)

  colnames(t4) <- new_names

  return(t4)

}

t4_zero_tph_ruralurban_summary <- function(bus_lsoa21_2024, threshold = 1) {

  bus_lsoa21_2024_long <- make_bustrips_long(bus_lsoa21_2024)

  bus_lsoa21_2024_long <- bus_lsoa21_2024_long %>%
    mutate(tph = ifelse(is.na(tph), 0, tph)) %>%
    mutate(no_service = tph < threshold)

  no_service_summary <- bus_lsoa21_2024_long %>%
    group_by(rurality,
             time_period,
             no_service) %>%
    summarise(n = n()) %>%
    group_by(rurality,
             time_period) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(no_service) %>%
    rename(`No bus service (No. of neighbourhoods)` = n,
           `No bus service (% of neighbourhoods)` = pct) %>%
    filter(!grepl("Night", time_period))

  no_service_summary <- no_service_summary %>%
    select(-no_service) %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -time_period) %>%
    unite(full_indicator, time_period, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val,
           fill = 0)

  no_service_summary <- no_service_summary %>%
    select(`Rural urban classification` = rurality,
           `weekday_daytime: No bus service (No. of neighbourhoods)`,
           `weekday_daytime: No bus service (% of neighbourhoods)`,
           `weekday_Evening: No bus service (No. of neighbourhoods)`,
           `weekday_Evening: No bus service (% of neighbourhoods)`,
           `Sat_daytime: No bus service (No. of neighbourhoods)`,
           `Sat_daytime: No bus service (% of neighbourhoods)`,
           `Sat_Evening: No bus service (No. of neighbourhoods)`,
           `Sat_Evening: No bus service (% of neighbourhoods)`,
           `Sun_daytime: No bus service (No. of neighbourhoods)`,
           `Sun_daytime: No bus service (% of neighbourhoods)`,
           `Sun_Evening: No bus service (No. of neighbourhoods)`,
           `Sun_Evening: No bus service (% of neighbourhoods)`)

  # finalise column names for xlsx
  no_service_summary <- finalise_t4_colnames(no_service_summary)

  return(no_service_summary)

}


t4_zero_tph_geog_summary <- function(bus_lsoa21_2024, threshold = 1, geog_code, geog_name) {

  bus_lsoa21_2024_long <- make_bustrips_long(bus_lsoa21_2024)
  bus_lsoa21_2024_long <- add_geography21(bus_lsoa21_2024_long)

  bus_lsoa21_2024_long <- bus_lsoa21_2024_long %>%
    mutate(tph = ifelse(is.na(tph), 0, tph)) %>%
    mutate(no_service = tph < threshold)

  no_service_summary <- bus_lsoa21_2024_long %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             time_period,
             no_service) %>%
    summarise(n = n()) %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             time_period) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(no_service) %>%
    rename(`No bus service (No. of neighbourhoods)` = n,
           `No bus service (% of neighbourhoods)` = pct) %>%
    filter(!grepl("Night", time_period))

  no_service_summary <- no_service_summary %>%
    select(-no_service) %>%
    gather(key = indicator,
           value = val,
           -{{ geog_code }},
           -{{ geog_name }},
           -time_period) %>%
    unite(full_indicator, time_period, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val,
           fill = 0)

  no_service_summary <- no_service_summary %>%
    select({{ geog_code }},
           `weekday_daytime: No bus service (No. of neighbourhoods)`,
           `weekday_daytime: No bus service (% of neighbourhoods)`,
           `weekday_Evening: No bus service (No. of neighbourhoods)`,
           `weekday_Evening: No bus service (% of neighbourhoods)`,
           `Sat_daytime: No bus service (No. of neighbourhoods)`,
           `Sat_daytime: No bus service (% of neighbourhoods)`,
           `Sat_Evening: No bus service (No. of neighbourhoods)`,
           `Sat_Evening: No bus service (% of neighbourhoods)`,
           `Sun_daytime: No bus service (No. of neighbourhoods)`,
           `Sun_daytime: No bus service (% of neighbourhoods)`,
           `Sun_Evening: No bus service (No. of neighbourhoods)`,
           `Sun_Evening: No bus service (% of neighbourhoods)`)

  # some geogs have no zero services, so make a complete list of geogs,
  geog_spine <- bus_lsoa21_2024_long %>%
    distinct({{ geog_code }},
             {{ geog_name }})
  # join on to this
  no_service_summary <- left_join(geog_spine, no_service_summary)
  # set any NAs (i.e. LAs with no zero service LSOAs) to 0
  no_service_summary[is.na(no_service_summary)] <- 0

  # finalise names
  no_service_summary <- finalise_t4_colnames(no_service_summary)

  return(no_service_summary)

}



# summarise car ownership by different quintiles --------------------------

#' for a given year of data (at LSOA21 level), this function takes a time periods of interest
#' and returns summary statistics on the best and worst quintiles, including adding in
#' data on the number/proportion of people who don't own cars. The results are aggregated
#' by rural urban classification
t5_best_worst_quintiles_cars <- function(lsoa_bus, time_period_name = "weekday_daytime") {

  lsoa_bus_qt <- make_tph_quintiles(lsoa_bus)

  lsoa_bus_qt_timeperiod <- lsoa_bus_qt %>%
    filter(time_period == time_period_name)

  # add car ownership by using common functions from another repository
  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")
  lsoa_bus_qt_timeperiod <- add_noncar_owners_to_lsoa21_data(lsoa_bus_qt_timeperiod)

  # aggregate summary data for all quintiles
  lsoa_qt_summary <- lsoa_bus_qt_timeperiod %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             tph_qt,
             tph_qt_id) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              car_ownership_no_cars = round(sum(car_ownership_no_cars), -2),
              car_ownership_no_cars_pct = round(sum(car_ownership_no_cars) / sum(census_households), 4)) %>%
    ungroup()

  # filter for top and bottom deciles and add label to these.
  lsoa_qt_summary <- lsoa_qt_summary %>%
    filter(tph_qt_id %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(tph_qt_id == "A" ~ "Best 20%",
                                      tph_qt_id == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Households without a car (No.)` = car_ownership_no_cars,
           `Households without a car (%)` = car_ownership_no_cars_pct)

  # restructure table to a nicer wider presentation format
  # gather
  lsoa_qt_summary <- lsoa_qt_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)
  # combine and then spread
  lsoa_qt_summary <- lsoa_qt_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

  # turn numerical cols back to numbers (otherwise they are characters)
  lsoa_qt_summary[3] <- as.numeric(lsoa_qt_summary[[3]])
  lsoa_qt_summary[4] <- as.numeric(lsoa_qt_summary[[4]])
  lsoa_qt_summary[6] <- as.numeric(lsoa_qt_summary[[6]])
  lsoa_qt_summary[7] <- as.numeric(lsoa_qt_summary[[7]])

  return(lsoa_qt_summary)

}


# summarises 2010 and 2024 quintiles --------------------------------------------------------------

#' take the full LSOA long data set with all time periods and including quintiles assigned
#' above, and summarise the stats, identifying the bandings or range in each quintile
#' for each time period in each rural/urban classification.
make_quintile_lookup <- function(lsoa_bustrips_long) {

  quintile_bands_lookup <- lsoa_bustrips_long %>%
    group_by(rurality,
             time_period,
             tph_qt,
             tph_qt_id) %>%
    summarise(tph_min = min(tph),
              tph_max = max(tph)) %>%
    ungroup() %>%
    filter(!is.na(tph_min)) %>%
    mutate(time_period = gsub("tph_", "", time_period))

}

#' the quintile lookup table is used to assign which 2007-10 baseline quintile bands
#' the most recent data (i.e. 2024) would be in. That is to day, the 2007-10 quintile
#' bands are applied to 2024 TPH data to identify the distribution of service frequency
#' today when compared with levels from 2007-10.
add_200710_quintiles_to_2024_data <- function(lsoa_bustrips_2024,
                                              lsoa_bustrips_2010,
                                              time_period_name = "weekday_daytime") {

  # make long df
  lsoa_bustrips_2010_qt <- make_tph_quintiles(lsoa_bustrips_2010)
  lsoa_bustrips_2024_qt <- make_tph_quintiles(lsoa_bustrips_2024)
  lsoa_bustrips_2024_qt_timeperiod <- lsoa_bustrips_2024_qt %>%
    filter(time_period == time_period_name)

  # identify the quintile bands from 200710 data
  quintile_lookup_2010 <- make_quintile_lookup(lsoa_bustrips_2010_qt)
  # filter this table for the time period of interest
  qt <- quintile_lookup_2010 %>%
    filter(time_period == time_period_name) %>%
    transmute(rurality,
              rurality_4x = case_when(rurality == "Urban: Conurbation" ~ "urban_conurb",
                                      rurality == "Urban: City and Town" ~ "urban_city",
                                      rurality == "Rural: Town and Fringe" ~ "rural_town",
                                      rurality == "Rural: Village/Hamlets/Isolated Dwellings" ~ "rural_village"),
              time_period,
              tph_qt,
              tph_qt_id,
              tph_min = round(tph_min, 1),
              tph_max = round(tph_max, 2))

  # identify the quintile banding from the 200710 data, to be applied for 2024 data:
  conurb_a <- qt[qt$rurality_4x == "urban_conurb" & qt$tph_qt_id == "A" , ]$tph_min
  conurb_b <- qt[qt$rurality_4x == "urban_conurb" & qt$tph_qt_id == "B" , ]$tph_min
  conurb_c <- qt[qt$rurality_4x == "urban_conurb" & qt$tph_qt_id == "C" , ]$tph_min
  conurb_d <- qt[qt$rurality_4x == "urban_conurb" & qt$tph_qt_id == "D" , ]$tph_min

  city_a <- qt[qt$rurality_4x == "urban_city" & qt$tph_qt_id == "A" , ]$tph_min
  city_b <- qt[qt$rurality_4x == "urban_city" & qt$tph_qt_id == "B" , ]$tph_min
  city_c <- qt[qt$rurality_4x == "urban_city" & qt$tph_qt_id == "C" , ]$tph_min
  city_d <- qt[qt$rurality_4x == "urban_city" & qt$tph_qt_id == "D" , ]$tph_min

  town_a <- qt[qt$rurality_4x == "rural_town" & qt$tph_qt_id == "A" , ]$tph_min
  town_b <- qt[qt$rurality_4x == "rural_town" & qt$tph_qt_id == "B" , ]$tph_min
  town_c <- qt[qt$rurality_4x == "rural_town" & qt$tph_qt_id == "C" , ]$tph_min
  town_d <- qt[qt$rurality_4x == "rural_town" & qt$tph_qt_id == "D" , ]$tph_min

  village_a <- qt[qt$rurality_4x == "rural_village" & qt$tph_qt_id == "A" , ]$tph_min
  village_b <- qt[qt$rurality_4x == "rural_village" & qt$tph_qt_id == "B" , ]$tph_min
  village_c <- qt[qt$rurality_4x == "rural_village" & qt$tph_qt_id == "C" , ]$tph_min
  village_d <- qt[qt$rurality_4x == "rural_village" & qt$tph_qt_id == "D" , ]$tph_min

  # apply banding to allocate LSOAs using 2010 quintiles on 2024 data
  lsoa_bustrips_2024_qt_timeperiod <- lsoa_bustrips_2024_qt_timeperiod %>%
    mutate(tph_qt_2010 = case_when(rurality == "Urban: Conurbation" & tph >= conurb_a ~ 1,
                                   rurality == "Urban: Conurbation" & between(tph, conurb_b, conurb_a) ~ 2,
                                   rurality == "Urban: Conurbation" & between(tph, conurb_c, conurb_b) ~ 3,
                                   rurality == "Urban: Conurbation" & between(tph, conurb_d, conurb_c) ~ 4,
                                   rurality == "Urban: Conurbation" & tph < conurb_d ~ 5,
                                   rurality == "Urban: City and Town" & tph >= city_a ~ 1,
                                   rurality == "Urban: City and Town" & between(tph, city_b, city_a) ~ 2,
                                   rurality == "Urban: City and Town" & between(tph, city_c, city_b) ~ 3,
                                   rurality == "Urban: City and Town" & between(tph, city_d, city_c) ~ 4,
                                   rurality == "Urban: City and Town" & tph < city_d ~ 5,
                                   rurality == "Rural: Town and Fringe" & tph >= town_a ~ 1,
                                   rurality == "Rural: Town and Fringe" & between(tph, town_b, town_a) ~ 2,
                                   rurality == "Rural: Town and Fringe" & between(tph, town_c, town_b) ~ 3,
                                   rurality == "Rural: Town and Fringe" & between(tph, town_d, town_c) ~ 4,
                                   rurality == "Rural: Town and Fringe" & tph < town_d ~ 5,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph >= village_a ~ 1,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, village_b, village_a) ~ 2,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, village_c, village_b) ~ 3,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, village_d, village_c) ~ 4,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph < village_d ~ 5))

  # make a letter variable for the new quintiles
  lsoa_bustrips_2024_qt_timeperiod$tph_qt_2010_id = LETTERS[lsoa_bustrips_2024_qt_timeperiod$tph_qt_2010]

  # add 2010 TPHs data to sit along side the 200710 quintiles, so both the quintiles
  # and TPH can be directly compared across each LSOA
  lsoa_bustrips_2010_tph <- lsoa_bustrips_2010_qt %>%
    filter(time_period == time_period_name) %>%
    transmute(lsoa21,
              tph_2010 = round(tph, 2))

  lsoa_bustrips_2024_qt_timeperiod <- left_join(lsoa_bustrips_2024_qt_timeperiod, lsoa_bustrips_2010_tph, by = "lsoa21")

  return(lsoa_bustrips_2024_qt_timeperiod)

}

graph1_summarise_2010_2024_quintile_counts <- function(lsoa_bustrips_2024,
                                                       lsoa_bustrips_2010,
                                                       time_period_name = "weekday_daytime") {

  lsoa_bus_2024_2010_qt <- add_200710_quintiles_to_2024_data(lsoa_bustrips_2024,
                                                             lsoa_bustrips_2010,
                                                             time_period_name)

  lsoa_bustrips_2010_qt <- make_tph_quintiles(lsoa_bustrips_2024)
  lsoa_bustrips_2010_qt <- lsoa_bustrips_2010_qt %>%
    filter(time_period == time_period_name)

  # bespoke function to summarise quintiles - need this twice, hence functionise
  summarise_quintiles_by_rurality <- function(lsoa_df, qt_var) {

    qt_sum <- lsoa_df %>%
      group_by(rurality,
               {{ qt_var }}) %>%
      summarise(n = n()) %>%
      ungroup()

    qt_sum <- qt_sum %>%
      group_by({{ qt_var }}) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      bind_rows(qt_sum, .)

    # complete rurality for 'all neighbourhoods'
    levels(qt_sum$rurality) <- c(levels(qt_sum$rurality), "All neighbourhoods")
    qt_sum$rurality[is.na(qt_sum$rurality)] <-  "All neighbourhoods"

    return(qt_sum)

  }

  qt_24 <- summarise_quintiles_by_rurality(lsoa_bus_2024_2010_qt, tph_qt_2010_id)
  qt_10 <- summarise_quintiles_by_rurality(lsoa_bustrips_2010_qt, tph_qt_id)

  qt_10 <- qt_10 %>%
    rename(n_2010 = n)

  qt_24 <- qt_24 %>%
    rename(tph_qt_id = tph_qt_2010_id,
           n_2024 = n)

  quintile_comp_2010_2024 <- inner_join(qt_10, qt_24, by = c("rurality", "tph_qt_id"))

  quintile_comp_2010_2024 <- quintile_comp_2010_2024 %>%
    transmute(`Rural/Urban Classification` = rurality,
              Quintile = tph_qt_id,
              `Qunitile label` = case_when(tph_qt_id == "A" ~ "A: Very best service",
                                           tph_qt_id == "B" ~ "B: Best service",
                                           tph_qt_id == "C" ~ "C: Middling service",
                                           tph_qt_id == "D" ~ "D: Poor service",
                                           tph_qt_id == "E" ~ "E: Very poor service"),
              `Number of neighbourhoods (2010)` = n_2010,
              `Number of neighbourhoods (2024)` = n_2024)

  levels(quintile_comp_2010_2024$`Qunitile label`) <- c("E: Very poor service",
                                                        "D: Poor service",
                                                        "C: Middling service",
                                                        "B: Best service",
                                                        "A: Very best service")

  return(quintile_comp_2010_2024)

}
