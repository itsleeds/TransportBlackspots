
# summary tables ----------------------------------------------------------

summarise_service_by_period_and_geog <- function(lsoa_bustrips_2023, geog_name = region_name, group_var = rurality, service, measure = "") {

  service_by_geog_rurality <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by({{ geog_name }},
             {{ group_var }},
             {{ service }}) %>%
    summarise(n = n()) %>%
    group_by({{ geog_name }},
             {{ group_var }}) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    rename(`(no.)` = n,
           `(%)` = pct) %>%
    gather(key = indicator,
           value = val,
           -{{ geog_name }},
           -{{ group_var }},
           -{{ service }}) %>%
    unite(full_indicator, {{ service }}, indicator, sep = " ") %>%
    spread(key = full_indicator,
           value = val,
           fill = 0)

  if(measure == "service") {
    service_by_geog_rurality <- service_by_geog_rurality %>%
      mutate(Neighbourhoods = `Good (no.)` + `Okay (no.)` + `Poor (no.)`) %>%
      select({{ geog_name }},
             {{ group_var }},
             `Good (%)`,
             `Okay (%)`,
             `Poor (%)`,
             `Good (no.)`,
             `Okay (no.)`,
             `Poor (no.)`,
             Neighbourhoods)
  }

  if(measure == "quintiles") {
    service_by_geog_rurality <- service_by_geog_rurality %>%
      mutate(Neighbourhoods = `Very good (no.)` + `Good (no.)` + `Average (no.)` + `Poor (no.)` + `Very poor (no.)`) %>%
      select({{ geog_name }},
             {{ group_var }},
             `Very good (%)`,
             `Good (%)`,
             `Average (%)`,
             `Poor (%)`,
             `Very poor (%)`,
             `Very good (no.)`,
             `Good (no.)`,
             `Average (no.)`,
             `Poor (no.)`,
             `Very poor (no.)`,
             Neighbourhoods)
  }

  return(service_by_geog_rurality)

}

make_ethnicity_summary <- function(lsoa_bustrips_2023, geog_name = region_name, service) {

  ethnicity_service_by_geog <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by({{ geog_name }},
             {{ service }}) %>%
    summarise(all_ethnicities = sum(all_ethnicities, na.rm = TRUE),
              ethnicity_white = sum(ethnicity_white, na.rm = TRUE),
              ethnicity_minority = sum(ethnicity_bame, na.rm = TRUE)) %>%
    group_by({{ geog_name }}) %>%
    mutate(ethnicity_minority_row_pct = ethnicity_minority / all_ethnicities,
           ethnicity_minority_allpct = ethnicity_minority / sum(ethnicity_minority)) %>%
    ungroup()

  totals <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by({{ service }}) %>%
    summarise(all_ethnicities = sum(all_ethnicities, na.rm = TRUE),
              ethnicity_white = sum(ethnicity_white, na.rm = TRUE),
              ethnicity_minority = sum(ethnicity_bame, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ethnicity_minority_row_pct = ethnicity_minority / all_ethnicities,
           ethnicity_minority_allpct = ethnicity_minority / sum(ethnicity_minority)) %>%
    mutate("{{ geog_name }}" := "England and Wales")

  ethnicity_service_by_geog <- bind_rows(ethnicity_service_by_geog,
                                         totals)

  return(ethnicity_service_by_geog)

}

make_carownership_summary <- function(lsoa_bustrips_2023, geog_name = region_name, service) {

  ethnicity_service_by_geog <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by({{ geog_name }},
             {{ service }}) %>%
    summarise(no_cars_n = sum(car_ownership_no_cars, na.rm = TRUE),
              census_households = sum(census.households, na.rm = TRUE)) %>%
    group_by({{ geog_name }}) %>%
    mutate(no_cars_rowpct = no_cars_n / census_households,
           no_cars_all_pct = no_cars_n / sum(no_cars_n)) %>%
    ungroup()

  totals <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by({{ service }}) %>%
    summarise(no_cars_n = sum(car_ownership_no_cars, na.rm = TRUE),
              census_households = sum(census.households, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(no_cars_rowpct = no_cars_n / census_households,
           no_cars_all_pct = no_cars_n / sum(no_cars_n)) %>%
    mutate("{{ geog_name }}" := "England and Wales")


  ethnicity_service_by_geog <- bind_rows(ethnicity_service_by_geog,
                                         totals)

  return(ethnicity_service_by_geog)

}

make_imd_summary <- function(lsoa_bustrips_2023, service) {

  imd_by_service <- lsoa_bustrips_2023_quintiles %>%
    filter(!is.na(tph_daytime_avg_service)) %>%
    mutate(imd_3 = ifelse(imd_income_decile_bottom_three == 1, "lowest_imd_income_deciles", "not_lowest_imd_income_deciles")) %>%
    group_by(imd_3,
             tph_daytime_avg_service) %>%
    summarise(lsoas = n()) %>%
    ungroup() %>%
    gather(key = indicator,
           value = val,
           -tph_daytime_avg_service,
           -imd_3) %>%
    spread(key = imd_3,
           value = val) %>%
    select(-indicator) %>%
    mutate(lowest_imd_income_deciles_row_pct = lowest_imd_income_deciles / not_lowest_imd_income_deciles,
           lowest_imd_income_deciles_pct = lowest_imd_income_deciles / sum(lowest_imd_income_deciles),
           not_lowest_imd_income_deciles_pct = not_lowest_imd_income_deciles / sum(not_lowest_imd_income_deciles))

    # %>%
    # mutate(Neighbourhoods = `Good (no.)` + `Okay (no.)` + `Poor (no.)`) %>%
    # select(imd_3,
    #        `Good (%)`,
    #        `Okay (%)`,
    #        `Poor (%)`,
    #        `Good (no.)`,
    #        `Okay (no.)`,
    #        `Poor (no.)`,
    #        Neighbourhoods)

  return(imd_by_service)

}

make_imd_summary_by_region <- function(lsoa_bustrips_2023, service, measure = "quintiles") {

  imd_by_service <- lsoa_bustrips_2023 %>%
    filter(!is.na({{ service }})) %>%
    group_by(rurality,
             imd_income_domain_decile,
             {{ service }}) %>%
    summarise(n = n()) %>%
    group_by(rurality,
             {{ service }}) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    rename(`(no.)` = n,
           `(%)` = pct) %>%
    gather(key = indicator,
           value = val,
           -imd_income_domain_decile,
           -rurality,
           -{{ service }}) %>%
    unite(full_indicator, {{ service }}, indicator, sep = " ") %>%
    spread(key = full_indicator,
           value = val,
           fill = 0)

  if(measure == "service") {
    imd_by_service <- imd_by_service %>%
      mutate(Neighbourhoods = `Good (no.)` + `Okay (no.)` + `Poor (no.)`) %>%
      select(rurality,
             imd_income_domain_decile,
             `Good (%)`,
             `Okay (%)`,
             `Poor (%)`,
             `Good (no.)`,
             `Okay (no.)`,
             `Poor (no.)`,
             Neighbourhoods)
  }

  if(measure == "quintiles") {
    imd_by_service <- imd_by_service %>%
      mutate(Neighbourhoods = `Very good (no.)` + `Good (no.)` + `Average (no.)` + `Poor (no.)` + `Very poor (no.)`) %>%
      select(rurality,
             imd_income_domain_decile,
             `Very good (%)`,
             `Good (%)`,
             `Average (%)`,
             `Poor (%)`,
             `Very poor (%)`,
             `Very good (no.)`,
             `Good (no.)`,
             `Average (no.)`,
             `Poor (no.)`,
             `Very poor (no.)`,
             Neighbourhoods)
  }

  return(imd_by_service)

}

# MAPS --------------------------------------------------------------------

make_map_of_bus_service <- function(lsoa_bustrips, tph, tph_service) {

  tph_col <- gsub("tph_", "", deparse(substitute(tph)))
  tph_col <- gsub("_", " ", tph_col)
  tph_col <- paste0(toupper(substring(tph_col, 1, 1)), tolower(substring(tph_col, 2)), " (tph)")
  tph_col <- gsub("\"", "", tph_col)

  tph_freq <- gsub("tph_", "", deparse(substitute(tph_service)))
  tph_freq <- gsub("_service", "", deparse(substitute(tph_freq)))
  tph_freq <- gsub("_", " ", tph_freq)
  tph_freq <- paste0(toupper(substring(tph_freq, 1, 1)), tolower(substring(tph_freq, 2)), " (service freq)")
  tph_freq <- gsub("\"", "", tph_freq)


  lsoa_bustrips <- lsoa_bustrips %>%
    transmute(lsoa11,
              rurality,
              tph = round({{ tph }}, 1),
              tph_service = {{ tph_service }})

  colnames(lsoa_bustrips)[3] <- tph_col
  colnames(lsoa_bustrips)[4] <- tph_freq

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips <- left_join(lsoas, lsoa_bustrips, by = "lsoa11")

  t <- tm_shape(lsoa_bustrips) +
    tm_polygons(fill = tph_freq,
                fill.scale = tm_scale_categorical(values = "-rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               tph_col,
                               tph_freq))


  file_name <- gsub("[(]|[)]", "", tph_freq)
  file_name <- gsub("[ ]+", "-", file_name)
  full_file_name <- paste0("outputs/november-24/plots/", file_name,".png")

  tmap_save(t,
            full_file_name)

  t

}

make_map_of_bus_service_am_peak <- function(lsoa_bustrips) {



  lsoa_bustrips_daytime <- lsoa_bustrips %>%
    transmute(lsoa11,
              rurality,
              `Weekday morning peak (tph)` = round(tph_weekday_Morning_Peak, 1),
              `Weekday morning peak (service freq)` = tph_weekday_Morning_Peak_service
    )

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_daytime <- left_join(lsoas, lsoa_bustrips_daytime, by = "lsoa11")
  #lsoa_bustrips_daytime <- lsoa_bustrips_daytime %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  #tmap_mode("view")

  t <- tm_shape(lsoa_bustrips_daytime) +
    tm_polygons(fill = "Weekday morning peak (service freq)",
                fill.scale = tm_scale_categorical(values = "-rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Weekday morning peak (tph)",
                               "Weekday morning peak (service freq)"))

  tmap_save(t,
            "outputs/november-24/plots/weekday-morning-service-rag.png")

  t

}


make_map_of_bus_service_wkday_eve <- function(lsoa_bustrips) {

  lsoa_bustrips_sun <- lsoa_bustrips %>%
    transmute(lsoa11,
              rurality,
              `Weekday evening (tph)` = round(tph_weekday_Evening, 1),
              `Weekday evening (service freq)` = tph_weekday_Evening_service
    )

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_sun <- left_join(lsoas, lsoa_bustrips_sun, by = "lsoa11")

  t <- tm_shape(lsoa_bustrips_sun) +
    tm_polygons(fill = "Weekday evening (service freq)",
                fill.scale = tm_scale_categorical(values = "-rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Weekday evening (tph)",
                               "Weekday evening (service freq)"))

  tmap_save(t,
            "outputs/november-24/plots/weekday-evening-service.png")

  t

}


make_map_of_bus_service_sun_pm <- function(lsoa_bustrips) {

  lsoa_bustrips_sun <- lsoa_bustrips %>%
    transmute(lsoa11,
              rurality,
              `Sunday afternoon peak (tph)` = round(tph_Sun_Afternoon_Peak, 1),
              `Sunday afternoon peak (service freq)` = tph_Sun_Afternoon_Peak_service
    )

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_sun <- left_join(lsoas, lsoa_bustrips_sun, by = "lsoa11")

  t <- tm_shape(lsoa_bustrips_sun) +
    tm_polygons(fill = "Sunday afternoon peak (service freq)",
                fill.scale = tm_scale_categorical(values = "-rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Sunday afternoon peak (tph)",
                               "Sunday afternoon peak (service freq)"))

  tmap_save(t,
            "outputs/november-24/plots/sunday-afternoon-service-rag.png")

  t

}





make_map_of_bus_service_score <- function(lsoa_bustrips) {

  lsoa_bustrips_score <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           `Weekday service score` = weekday_service_score,
           `Saturday service score` = saturday_service_score,
           `Sunday service score` = sunday_service_score,
           `Overall service score` = overall_service_score)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_score <- left_join(lsoas, lsoa_bustrips_score, by = "lsoa11")
  #lsoa_bustrips_score <- lsoa_bustrips_score %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  #tmap_mode("view")

  t <- tm_shape(lsoa_bustrips_score) +
    tm_polygons(fill = "Overall service score",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Weekday service score",
                               "Saturday service score",
                               "Sunday service score",
                               "Overall service score"))

  tmap_save(t,
            "outputs/november-24/plots/overall-bus-service-score.png")

  t

}

make_map_of_bus_good_service_duration <- function(lsoa_bustrips) {

  lsoa_bustrips_plot <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           "Weekday good duration" = weekday_good_duration,
           "Saturday good duration" = saturday_good_duration,
           "Sunday good duration" = sunday_good_duration,
           "Weeklong good duration" = weeklong_good_duration)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_plot <- left_join(lsoas, lsoa_bustrips_plot, by = "lsoa11")
  #lsoa_bustrips_plot <- lsoa_bustrips_plot %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  #tmap_mode("view")

  t <- tm_shape(lsoa_bustrips_plot) +
    tm_polygons(fill = "Weeklong good duration",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Weekday good duration",
                               "Saturday good duration",
                               "Sunday good duration",
                               "Weeklong good duration"))

  tmap_save(t,
            "outputs/november-24/plots/good-bus-service-duration.png")

  t

}


make_map_of_bus_all_service_duration <- function(lsoa_bustrips) {

  lsoa_bustrips_duration <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           "Weekday duration" = weekday_duration,
           "Saturday duration" = saturday_duration,
           "Sunday duration" = sunday_duration,
           "Weeklong duration" = weeklong_duration)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_duration <- left_join(lsoas, lsoa_bustrips_duration, by = "lsoa11")
  #lsoa_bustrips_duration <- lsoa_bustrips_duration %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  #tmap_mode("view")

  t <- tm_shape(lsoa_bustrips_duration) +
    tm_polygons(fill = "Weeklong duration",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "Weekday duration",
                               "Saturday duration",
                               "Sunday duration",
                               "Weeklong duration"))

  tmap_save(t,
            "outputs/november-24/plots/bus-service-duration.png")

  t
}
