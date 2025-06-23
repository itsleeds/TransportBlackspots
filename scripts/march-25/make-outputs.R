# SUMMARISE DATA BY HIGHER GEOGRAPHIES ------------------------------------

# summary stats between rural and urban, london and not london
make_london_rurality_summary <- function(bus_lsoa,
                                         time_period_name,
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

  london_2010 <- make_london_rurality_summary(bus_lsoa_2010,
                                              time_period_name = time_period_filter,
                                              year = `2010`)
  london_2024 <- make_london_rurality_summary(bus_lsoa_2024,
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

  # These outputs not used at the moment, so commenting out
  # t3a_2010_daytime <<- summarise_tph_quintiles(lsoa21_buses_2010_qt,
  #                                              filter_by = "weekday_daytime",
  #                                              stats = "mean",
  #                                              round_fig = 1)
  #
  # t3b_2010_daytime_range <<- summarise_tph_quintiles(lsoa21_buses_2010_qt,
  #                                                    filter_by = "weekday_daytime",
  #                                                    stats = "all",
  #                                                    round_fig = 1)

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


#  map outputs ------------------------------------------------------------

#' Four layers for map outputs:
#'  - Transport authority
#'  - Local authority
#'  - Constituency
#'  - LSOA

#' Fields in each of these files (Saved as csvs) are:
#'  - geog_code
#'  - geog_name
#'  - tph_2010 [Bus services frequency (trips per hour) 2010]
#'  - tph_2024 [Bus services frequency (trips per hour) 2023]
#'  - tph1024_chg [Not used in pop ups]
#'  - tph1024_chg_pct [Change in service frequency 2010 - 2023]

calculate_2010_2024_change <- function(bus_2010_2024) {

  bus_2010_2024 <- bus_2010_2024 %>%
    mutate(tph1024_chg = round(tph_2024 - tph_2010, 2)) %>%
    mutate(tph1024_chg_pct = round(tph1024_chg / tph_2010, 4))

  bus_2010_2024 <- bus_2010_2024 %>%
    mutate(tph1024_chg_pct = ifelse(tph1024_chg_pct == "Inf", 1, tph1024_chg_pct)) %>%
    mutate(tph1024_chg_pct = ifelse(tph1024_chg_pct == "NaN", NA, tph1024_chg_pct))

}

# LSOA
# comparison function for 2010 and 2024 using LSOA data
calculate_lsoa_bus_2010_2024_trends <- function(lsoa_bus_2010,
                                               lsoa_bus_2024,
                                               time_period_name,
                                               set_nas_zero = FALSE,
                                               new_time_periods = TRUE) {

  # make both data sets long
  lsoa_bus_2010 <- add_daytime_evening_timeperiod(lsoa_bus_2010, new_periods = new_time_periods)
  lsoa_bus_2024 <- add_daytime_evening_timeperiod(lsoa_bus_2024, new_periods = new_time_periods)

  lsoa_bus_2010_long <- make_bustrips_long(lsoa_bus_2010)
  lsoa_bus_2024_long <- make_bustrips_long(lsoa_bus_2024)

  # select tph values only and filter for an individual time periods
  # 2010
  lsoa_bus_2010_long <- lsoa_bus_2010_long %>%
    filter(time_period == time_period_name) %>%
    select(lsoa21,
           tph_2010 = tph)
  # 2024
  lsoa_bus_2024_long <- lsoa_bus_2024_long %>%
    filter(time_period == time_period_name) %>%
    select(lsoa21,
           tph_2024 = tph)

  # join data together
  lsoa_bus_2010_2024 <- inner_join(lsoa_bus_2010_long, lsoa_bus_2024_long, by = "lsoa21")

  # Question/TODO: Should NA TPH values be ignored or set to 0? I think 0 as no services
  if(set_nas_zero) {
    lsoa_bus_2010_2024 <- lsoa_bus_2010_2024 %>%
      mutate(tph_2010 = is_na_zero(tph_2010),
             tph_2024 = is_na_zero(tph_2024))
  }


  # then calculate the changes/trends as tph and %
  lsoa_bus_2010_2024 <- calculate_2010_2024_change(lsoa_bus_2010_2024)

  # add geography for further aggregations
  lsoa_bus_2010_2024 <- add_geography21(lsoa_bus_2010_2024)

  return(lsoa_bus_2010_2024)

}

# generic geog summariser
summarise_bustrends_geog <- function(lsoa_bus_trends, geog_code, geog_name) {

  geog_bus_trends <- lsoa_bus_trends %>%
    group_by({{ geog_code }},
             {{ geog_name }}) %>%
    summarise(tph_2010 = round(mean(tph_2010, na.rm = TRUE), 2),
              tph_2024 = round(mean(tph_2024, na.rm = TRUE), 2)) %>%
    ungroup()

  geog_bus_trends <- calculate_2010_2024_change(geog_bus_trends)


}

# Transport authority
make_and_save_geog_trends_csv <- function(lsoa21_buses_2010_new,
                                          lsoa21_buses_2024_new,
                                          time_period_label = "weekday_daytime") {

  lsoa_bus_trends <- calculate_lsoa_bus_2010_2024_trends(lsoa21_buses_2010_new,
                                                         lsoa21_buses_2024_new,
                                                         time_period_name = time_period_label)

  ta_lup <- make_la_to_transport_authority_lup(remove_la_name = TRUE)
  lsoa_bus_trends <- left_join(lsoa_bus_trends, ta_lup, by = "oslaua")

  # Transport Authority
  TA_bus_trends <- summarise_bustrends_geog(lsoa_bus_trends, trans_auth_code, trans_auth_name)

  # Local authority
  LA_bus_trends <- summarise_bustrends_geog(lsoa_bus_trends, oslaua, local_authority)

  # Constituency
  PCON24_bus_trends <- summarise_bustrends_geog(lsoa_bus_trends, pcon24, pcon24_name)

  # finalise lsoa trends (remove unnecessary geog columns)
  lsoa_bus_trends <- lsoa_bus_trends %>%
    select(lsoa21,
           msoa21_name,
           tph_2010,
           tph_2024,
           tph1024_chg,
           tph1024_chg_pct)

  # then add quintiles too for an additional layer
  lsoa_qts <- make_bus_quintiles_csv(lsoa21_buses_2010_new,
                                     lsoa21_buses_2024_new,
                                     time_period_label)


  lsoa_bus_trends_qts <- inner_join(lsoa_bus_trends, lsoa_qts, by = "lsoa21")

  time_period_filename <- gsub("_", "-", time_period_label)

  save_map_csvs(lsoa_bus_trends_qts, paste0("lsoa-bus-trends-and-quintiles-", time_period_filename, ".csv"))
  save_map_csvs(PCON24_bus_trends, paste0("PCON24-bus-trends-", time_period_filename, ".csv"))
  save_map_csvs(LA_bus_trends, paste0("LA-bus-trends-", time_period_filename, ".csv"))
  save_map_csvs(TA_bus_trends, paste0("TA-bus-trends-", time_period_filename, ".csv"))


}

save_map_csvs <- function(bus_data_for_csv, file_name) {

  if(!dir.exists("outputs/march-25/map-csvs")) {
    dir.create("outputs/march-25/map-csvs")
  }

  full_file_name <- paste0("outputs/march-25/map-csvs/", file_name)

  write.csv(bus_data_for_csv,
            full_file_name,
            row.names = FALSE,
            na = "")


}

# quintiles map output

make_bus_quintiles_csv <- function(lsoa21_buses_2010, lsoa21_buses_2024, time_period_label = "weekday_daytime") {

  # identify both sets of quintiles in 2010 and 2024
  lsoa_qts <- add_200710_quintiles_to_2024_data(lsoa21_buses_2010,
                                                lsoa21_buses_2024,
                                                time_period_name = time_period_label)


  # add car ownership by using common functions from another repository
  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")
  lsoa_qts <- add_noncar_owners_to_lsoa21_data(lsoa_qts)

  # finalise data set
  lsoa_qts <- lsoa_qts %>%
    select(lsoa21,
           #year,
           rurality,
           urban_rural_cat,
           #time_period,
           # tph,
           # tph_2010,
           #tph_qt,
           tph_qt_id,
           #tph_qt_2010,
           tph_qt_2010_id,
           #census_households,
           #car_ownership_no_cars,
           car_ownership_no_cars_pct,
           #car_ownership_no_cars_decile
           )

  # relabel quintiles
  lsoa_qts <- lsoa_qts %>%
    mutate(tph_qt_id = case_when(tph_qt_id == "A" ~ "Quintile 1: Best 20%",
                                 tph_qt_id == "B" ~ "Quintile 2",
                                 tph_qt_id == "C" ~ "Quintile 3",
                                 tph_qt_id == "D" ~ "Quintile 4",
                                 tph_qt_id == "E" ~ "Quintile 5: Worst 20%")) %>%
    mutate(tph_qt_2010_id = case_when(tph_qt_2010_id == "A" ~ "Quintile 1: Best 20%",
                                 tph_qt_2010_id == "B" ~ "Quintile 2",
                                 tph_qt_2010_id == "C" ~ "Quintile 3",
                                 tph_qt_2010_id == "D" ~ "Quintile 4",
                                 tph_qt_2010_id == "E" ~ "Quintile 5: Worst 20%")) %>%
    rename(bus_quintile_2024 = tph_qt_id,
           bus_quintile_2010 = tph_qt_2010_id)

  table(lsoa_qts$bus_quintile_2024, useNA = "ifany")
  table(lsoa_qts$bus_quintile_2010, useNA = "ifany")

  return(lsoa_qts)

}

# MAPS - TEST DATA --------------------------------------------------------


# MAPS --------------------------------------------------------------------
#
make_map_of_bus_service_lsoa21 <- function(lsoa_bustrips, map_var, type, convert_to_percent, popup_var_list = c("lsoa21")) {

  if(convert_to_percent) {
    pct_cols <- names(lsoa_bustrips)[grepl("pct|[%]", names(lsoa_bustrips))]
      lsoa_bustrips <- lsoa_bustrips %>%
        mutate(across(all_of(pct_cols), \(x) round(x * 100, 1))) %>%
        mutate(across(all_of(pct_cols), \(x) ifelse(x > 50, 50, x)))
  }

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2021_EW_BSC_V4/LSOA_2021_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa21 = LSOA21CD,
           geometry)

  lsoa_bustrips <- left_join(lsoas, lsoa_bustrips, by = "lsoa21")

  if(type == "scale") {
    foe_scale <- tm_scale_intervals(values = "rd_yl_gn",
                                    n = 6,
                                    midpoint = 0)
  }

  if(type == "quintile") {
    foe_scale <- tm_scale_categorical(values = "-rd_yl_gn",
                                      n = 5)
  }

  t_lsoa <- tm_shape(lsoa_bustrips) +
      tm_polygons(fill = map_var,
                  fill.scale = foe_scale,
                  col_alpha = 0,
                  popup.vars = popup_var_list,
                  fill.legend = tm_legend(position = tm_pos_in())) +
      tm_layout(
        frame = FALSE,
        legend.frame = FALSE,
        legend.title.fontface = "bold",
        legend.title.color = "#1e234d",
        legend.title.size = 0.8,
        legend.text.fontface = "bold",
        legend.text.color = "#1e234d",
        legend.text.size = 0.7
      )

  t_lsoa

}

# make_map_of_bus_service_lsoa21(lsoa_bus_trends_qts, "bus_quintile_2024", type = "quintile")
# make_map_of_bus_service_lsoa21(lsoa_bus_trends_qts, "bus_quintile_2010", type = "quintile")
# make_map_of_bus_service_lsoa21(lsoa_bus_trends_qts, "tph1024_chg_pct", type = "scale")

make_map_of_bus_service_oslaua <- function(lsoa_bustrips, tph, tph_service) {

  la_bustrips <- LA_bus_trends
  la_bustrips <- la_bustrips %>%
    transmute(oslaua,
              local_authority,
              tph_2010,
              tph_2024,
              tph1024_chg,
              tph1024_chg_pct = tph1024_chg_pct * 100) %>%
    mutate(tph1024_chg_pct = ifelse(tph1024_chg_pct > 50, 50, tph1024_chg_pct))

  las <- st_read("../gis-data/boundaries/local-authorities/LAD_MAY_2024_UK_BUC/LAD_DEC_24_UK_BUC.shp",
                   quiet = TRUE)
  las <- las %>%
    select(oslaua = LAD24CD,
           geometry)

  la_bustrips <- left_join(las, la_bustrips, by = "oslaua")

  foe_scale <- tm_scale_intervals(values = "rd_yl_gn",
                                  n = 6,
                                  midpoint = 0)

  tmap_mode("view")

  t_la <- tm_shape(la_bustrips) +
    tm_polygons(fill = "tph1024_chg_pct",
                fill.scale = foe_scale,
                col_alpha = 0,
                popup.vars = c("local_authority",
                               "tph_2010",
                               "tph_2024",
                               "tph1024_chg",
                               "tph1024_chg_pct"),
                fill.legend = tm_legend(position = tm_pos_in())) +
    tm_layout(
      frame = FALSE,
      legend.frame = FALSE,
      legend.title.fontface = "bold",
      legend.title.color = "#1e234d",
      legend.title.size = 0.8,
      legend.text.fontface = "bold",
      legend.text.color = "#1e234d",
      legend.text.size = 0.7
    )

  t_la

}


make_map_of_bus_service_oslaua <- function(lsoa_bustrips, tph, tph_service) {

  pcon_bustrips <- PCON24_bus_trends
  pcon_bustrips <- pcon_bustrips %>%
    transmute(pcon24,
              pcon24_name,
              tph_2010,
              tph_2024,
              tph1024_chg,
              tph1024_chg_pct = tph1024_chg_pct * 100) %>%
    mutate(tph1024_chg_pct = ifelse(tph1024_chg_pct > 50, 50, tph1024_chg_pct))

  pcons <- st_read("../gis-data/boundaries/parliamentary-constituencies/Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BSC/PCON_JULY_2024_UK_BSC.shp",
                 quiet = TRUE)
  pcons <- pcons %>%
    select(pcon24 = PCON24CD,
           geometry)

  pcon_bustrips <- left_join(pcons, pcon_bustrips, by = "pcon24")

  foe_scale <- tm_scale_intervals(values = "rd_yl_gn",
                                  n = 6,
                                  midpoint = 0)

  tmap_mode("view")

  t_pcon <- tm_shape(pcon_bustrips) +
    tm_polygons(fill = "tph1024_chg_pct",
                fill.scale = foe_scale,
                col_alpha = 0,
                popup.vars = c("pcon24_name",
                               "tph_2010",
                               "tph_2024",
                               "tph1024_chg",
                               "tph1024_chg_pct"),
                fill.legend = tm_legend(position = tm_pos_in())) +
    tm_layout(
      frame = FALSE,
      legend.frame = FALSE,
      legend.title.fontface = "bold",
      legend.title.color = "#1e234d",
      legend.title.size = 0.8,
      legend.text.fontface = "bold",
      legend.text.color = "#1e234d",
      legend.text.size = 0.7
    )

  t_pcon

}
