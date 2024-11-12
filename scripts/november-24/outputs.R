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


make_map_of_bus_all_service_duration <- function(lsoa_bustrips) {

  lsoa_bustrips_daytime <- lsoa_bustrips %>%
    select(lsoa11,
           rurality,
           weekday_duration,
           saturday_duration,
           sunday_duration,
           weeklong_duration)

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
    tm_polygons(fill = "weeklong_duration",
                fill.scale = tm_scale_continuous(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("lsoa11",
                               "rurality",
                               "weekday_duration",
                               "saturday_duration",
                               "sunday_duration",
                               "weeklong_duration"))

}
