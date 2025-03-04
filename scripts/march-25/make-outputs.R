# SUMMARISE DATA BY HIGHER GEOGRAPHIES ------------------------------------

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
              `Weekday daytime 2010-2024 (% change)` = weekday_daytime_change_2010_2024 / weekday_daytime_2010,
              `Weekday evening 2010-2024 (% change)` = weekday_evening_change_2010_2024 / weekday_Evening_2010,
              `Saturday daytime 2010-2024 (% change)` = sat_daytime_change_2010_2024 / Sat_daytime_2010,
              `Saturday evening 2010-2024 (% change)` = sat_evening_change_2010_2024 / Sat_Evening_2010,
              `Sunday daytime 2010-2024 (% change)` = Sun_daytime_change_2010_2024 / Sun_daytime_2010,
              `Sunday evening 2010-2024 (% change)` = Sun_evening_change_2010_2024 / Sun_Evening_2010)

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


  }


# QUINTILES OUTPUTS -------------------------------------------------------

# Table 3: Bus service frequencies at neighbourhood (LSOA) in 2024 for weekday daytime
#   (Note: this is the frequency of all buses within a neighbourhood or within 500m of the centre of the neighbourhood)


make_tph_quintiles <- function(bus_lsoa) {

  bus_lsoa_long <- make_bustrips_long(bus_lsoa)

  bus_lsoa_long <- bus_lsoa_long %>%
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
    mutate(tph_stats = paste0(tph_mean, " (", tph_min, "-", tph_max, ")")) %>%
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

  return(no_service_summary)

}

t4_test_las <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = 0.5, oslaua, local_authority)
t4_test_pcon24 <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = 0.5, pcon24, pcon24_name)
t4_test_region <- t4_zero_tph_geog_summary(lsoa21_buses_2024_new, threshold = 0.5, rgn, region_name)
