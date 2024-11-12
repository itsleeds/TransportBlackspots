
service_quality <- function(lsoa_bustrips, tph) {

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate("{{ tph }}_service" := case_when(rurality == "Urban: Conurbation" & round({{ tph }}) >= 60 ~ "Good",
                                         rurality == "Urban: Conurbation" & round({{ tph }}) < 60 & round({{ tph }}) >= 30 ~ "Okay",
                                         rurality == "Urban: Conurbation" & round({{ tph }}) < 30 ~ "Poor",
                                         rurality == "Urban: City and Town" & round({{ tph }}) >= 30 ~ "Good",
                                         rurality == "Urban: City and Town" & round({{ tph }}) < 30 & round({{ tph }}) >= 15 ~ "Okay",
                                         rurality == "Urban: City and Town" & round({{ tph }}) < 15 ~ "Poor",
                                         rurality == "Rural: Town and Fringe" & round({{ tph }}) >= 12 ~ "Good",
                                         rurality == "Rural: Town and Fringe" & round({{ tph }}) < 12 & round({{ tph }}) >= 6 ~ "Okay",
                                         rurality == "Rural: Town and Fringe" & round({{ tph }}) < 6 ~ "Poor",
                                         rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= 9 ~ "Good",
                                         rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < 9 & round({{ tph }}) >= 4 ~ "Okay",
                                         rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < 4 ~ "Poor")) %>%
    mutate("{{ tph }}_score" := case_when(rurality == "Urban: Conurbation" & round({{ tph }}) >= 60 ~ 1,
                                            rurality == "Urban: Conurbation" & round({{ tph }}) < 60 & round({{ tph }}) >= 30 ~ 0.5,
                                            rurality == "Urban: Conurbation" & round({{ tph }}) < 30 ~ 0,
                                            rurality == "Urban: City and Town" & round({{ tph }}) >= 30 ~ 1,
                                            rurality == "Urban: City and Town" & round({{ tph }}) < 30 & round({{ tph }}) >= 15 ~ 0.5,
                                            rurality == "Urban: City and Town" & round({{ tph }}) < 15 ~ 0,
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) >= 12 ~ 1,
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) < 12 & round({{ tph }}) >= 6 ~ 0.5,
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) < 6 ~ 0,
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= 9 ~ 1,
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < 9 & round({{ tph }}) >= 4 ~ 0.5,
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < 4 ~ 0))
}

classify_service_quality <- function(lsoa_bustrips) {

  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_weekday_Morning_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_weekday_Midday)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_weekday_Afternoon_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_weekday_Evening)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sat_Morning_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sat_Midday)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sat_Afternoon_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sat_Evening)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sun_Morning_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sun_Midday)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sun_Afternoon_Peak)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_Sun_Evening)
  lsoa_bustrips <- service_quality(lsoa_bustrips, tph_daytime_avg)

  # calculate service score
  lsoa_bustrips <- lsoa_bustrips %>%
    mutate(weekday_service_score = tph_weekday_Morning_Peak_score + tph_weekday_Midday_score + tph_weekday_Afternoon_Peak_score + tph_weekday_Evening_score,
           saturday_service_score = tph_Sat_Morning_Peak_score + tph_Sat_Midday_score + tph_Sat_Afternoon_Peak_score + tph_Sat_Evening_score,
           sunday_service_score = tph_Sun_Morning_Peak_score + tph_Sun_Midday_score + tph_Sun_Afternoon_Peak_score + tph_Sun_Evening_score) %>%
    mutate(overall_service_score = weekday_service_score + saturday_service_score + sunday_service_score)

  # calculate good service duration?
  lsoa_bustrips <- calculate_good_service_duration(lsoa_bustrips)

  #' remove intermediate values? [TODO]

}

calculate_good_service_duration <- function(lsoa_bustrips) {

  if_one_else_none <- function(x, hours) ifelse(x == 1, 1, 0) * hours

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate(weekday_good_duration = if_one_else_none(tph_weekday_Morning_Peak_score, 4) + if_one_else_none(tph_weekday_Midday_score, 5) + if_one_else_none(tph_weekday_Afternoon_Peak_score, 3) + if_one_else_none(tph_weekday_Evening_score, 4),
           saturday_good_duration = if_one_else_none(tph_Sat_Morning_Peak_score, 4) + if_one_else_none(tph_Sat_Midday_score, 5) + if_one_else_none(tph_Sat_Afternoon_Peak_score, 3) + if_one_else_none(tph_Sat_Evening_score, 4),
           sunday_good_duration = if_one_else_none(tph_Sun_Morning_Peak_score, 4) + if_one_else_none(tph_Sun_Midday_score, 5) + if_one_else_none(tph_Sun_Afternoon_Peak_score, 3) + if_one_else_none(tph_Sun_Evening_score, 4)) %>%
    mutate(weeklong_good_duration = weekday_good_duration + saturday_good_duration + sunday_good_duration)


}



make_map_of_bus_service <- function(lsoa_bustrips) {

  lsoa_bustrips_daytime <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           tph_weekday_Morning_Peak = round(tph_weekday_Morning_Peak, 1),
           tph_weekday_Morning_Peak_service)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_daytime <- left_join(lsoas, lsoa_bustrips_daytime, by = "lsoa11")
  #lsoa_bustrips_daytime <- lsoa_bustrips_daytime %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  tmap_mode("view")

  tm_shape(lsoa_bustrips_daytime) +
    tm_polygons(fill = "tph_weekday_Morning_Peak_service",
                fill.scale = tm_scale_categorical(values = "-rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "tph_weekday_Morning_Peak",
                               "tph_weekday_Morning_Peak_service"))

}


make_map_of_bus_service_score <- function(lsoa_bustrips) {

  lsoa_bustrips_daytime <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           weekday_service_score,
           saturday_service_score,
           sunday_service_score,
           overall_service_score)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_daytime <- left_join(lsoas, lsoa_bustrips_daytime, by = "lsoa11")
  #lsoa_bustrips_daytime <- lsoa_bustrips_daytime %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  tmap_mode("view")

  tm_shape(lsoa_bustrips_daytime) +
    tm_polygons(fill = "overall_service_score",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "weekday_service_score",
                               "saturday_service_score",
                               "sunday_service_score",
                               "overall_service_score"))

}

make_map_of_bus_good_service_duration <- function(lsoa_bustrips) {

  lsoa_bustrips_daytime <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           weekday_good_duration,
           saturday_good_duration,
           sunday_good_duration,
           weeklong_good_duration)

  lsoas <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp",
                   quiet = TRUE)
  lsoas <- lsoas %>%
    select(lsoa11 = LSOA11CD,
           geometry)

  lsoa_bustrips_daytime <- left_join(lsoas, lsoa_bustrips_daytime, by = "lsoa11")
  #lsoa_bustrips_daytime <- lsoa_bustrips_daytime %>%
  #  mutate(tph_weekday_Morning_Peak_service = ifelse(is.na(tph_weekday_Morning_Peak_service), "No buses", tph_weekday_Morning_Peak_service))

  tmap_mode("view")

  tm_shape(lsoa_bustrips_daytime) +
    tm_polygons(fill = "weeklong_good_duration",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "weekday_good_duration",
                               "saturday_good_duration",
                               "sunday_good_duration",
                               "weeklong_good_duration"))

}
