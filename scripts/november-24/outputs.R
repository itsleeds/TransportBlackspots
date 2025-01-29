
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

make_map_of_bus_service <- function(lsoa_bustrips, tph, tph_service, type = "quintile", pct = FALSE) {

  tph_col <- gsub("tph_", "", deparse(substitute(tph)))
  tph_col <- gsub("_", " ", tph_col)
  tph_col <- paste0(toupper(substring(tph_col, 1, 1)), tolower(substring(tph_col, 2)), " (tph)")
  tph_col <- gsub("\"", "", tph_col)

  tph_freq <- gsub("tph_", "", deparse(substitute(tph_service)))
  tph_freq <- gsub("_service", "", tph_freq)
  tph_freq <- gsub("_score", " frequency score", tph_freq)
  tph_freq <- gsub("_", " ", tph_freq)
  tph_freq <- gsub(" pct", " (%)", tph_freq)
  if(type == "quintile") {
    tph_freq <- paste0(toupper(substring(tph_freq, 1, 1)), tolower(substring(tph_freq, 2)), " (service freq)")
  }
  if(type == "decile") {
    tph_freq <- paste0(toupper(substring(tph_freq, 1, 1)), tolower(substring(tph_freq, 2)), "\n(1 = most buses)")
  }
  if(type == "score") {
    tph_freq <- paste0(toupper(substring(tph_freq, 1, 1)), tolower(substring(tph_freq, 2)))
  }
  tph_freq <- gsub("\"", "", tph_freq)

  lsoa_bustrips <- lsoa_bustrips %>%
    transmute(lsoa11,
              rurality,
              tph = round({{ tph }}, 1),
              tph_service = {{ tph_service }})

  if(pct) {
    lsoa_bustrips <- lsoa_bustrips %>%
      mutate(tph_service = tph_service * 100)
  }

  colnames(lsoa_bustrips)[3] <- tph_col
  colnames(lsoa_bustrips)[4] <- tph_freq

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips <- left_join(lsoas, lsoa_bustrips, by = "lsoa11")

  if(type %in% c("quintile", "decile")) {
    foe_scale <- tm_scale_categorical(values = "-rd_yl_gn")
  }
  if(type == "score") {
    foe_scale <- tm_scale_continuous(values = "rd_yl_gn")
  }

  t <- tm_shape(lsoa_bustrips) +
    tm_polygons(fill = tph_freq,
                fill.scale = foe_scale,
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               tph_col,
                               tph_freq),
                fill.legend = tm_legend(position = tm_pos_in())) +
    tm_layout(
      #title = "Bus service frequency by rural/urban class (Deciles)",
      #title.position = c("center", "top"),
      #title.size = 1,
      #title.color = "#1e234d",
      #bg.color = "#f4f6fd",
      #inner.margins = c(0.1, 0.1, 0.1, 0.1),
      frame = FALSE,
      #frame.lwd = 1,
      #legend.position = "left",
      legend.frame = FALSE,
      #legend.bg.color = "#f4f6fd",
      legend.title.fontface = "bold",
      legend.title.color = "#1e234d",
      legend.title.size = 0.8,
      legend.text.fontface = "bold",
      legend.text.color = "#1e234d",
      legend.text.size = 0.7
    )


  file_name <- gsub("[(]|[)]", "", tph_freq)
  file_name <- gsub("[ ]+", "-", file_name)
  file_name <- gsub("[(]|[)]", "", file_name)
  file_name <- gsub("[%]", "pct", file_name)
  file_name <- gsub("\n", "", file_name)
  file_name <- gsub("1-=-most-buses", "", file_name)
  full_file_name <- paste0("outputs/november-24/plots/", file_name,".png")
  message(full_file_name)

  tmap_save(t,
            full_file_name)

  return(t)

}


#  OLD FUNCTIONS ----------------------------------------------------------



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
