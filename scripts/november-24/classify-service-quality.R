
service_quality <- function(lsoa_bustrips, tph) {

  urban_good <- 60
  urban_poor <- 30
  city_good <- 30
  city_poor <- 15
  town_good <- 12
  town_poor <- 6
  village_good <- 9
  village_poor <- 4

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate("{{ tph }}_service" := case_when(rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_good ~ "Good",
                                            rurality == "Urban: Conurbation" & round({{ tph }}) < urban_good & round({{ tph }}) >= urban_poor ~ "Okay",
                                            rurality == "Urban: Conurbation" & round({{ tph }}) < urban_poor ~ "Poor",
                                            rurality == "Urban: City and Town" & round({{ tph }}) >= city_good ~ "Good",
                                            rurality == "Urban: City and Town" & round({{ tph }}) < city_good & round({{ tph }}) >= city_poor ~ "Okay",
                                            rurality == "Urban: City and Town" & round({{ tph }}) < city_poor ~ "Poor",
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_good ~ "Good",
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) < town_good & round({{ tph }}) >= town_poor ~ "Okay",
                                            rurality == "Rural: Town and Fringe" & round({{ tph }}) < town_poor ~ "Poor",
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_good ~ "Good",
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < village_good & round({{ tph }}) >= village_poor ~ "Okay",
                                            rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < village_poor ~ "Poor")) %>%
    mutate("{{ tph }}_score" := case_when(rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_good ~ 1,
                                          rurality == "Urban: Conurbation" & round({{ tph }}) < urban_good & round({{ tph }}) >= urban_poor ~ 0.5,
                                          rurality == "Urban: Conurbation" & round({{ tph }}) < urban_poor ~ 0,
                                          rurality == "Urban: City and Town" & round({{ tph }}) >= city_good ~ 1,
                                          rurality == "Urban: City and Town" & round({{ tph }}) < city_good & round({{ tph }}) >= city_poor ~ 0.5,
                                          rurality == "Urban: City and Town" & round({{ tph }}) < city_poor ~ 0,
                                          rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_good ~ 1,
                                          rurality == "Rural: Town and Fringe" & round({{ tph }}) < town_good & round({{ tph }}) >= town_poor ~ 0.5,
                                          rurality == "Rural: Town and Fringe" & round({{ tph }}) < town_poor ~ 0,
                                          rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_good ~ 1,
                                          rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < village_good & round({{ tph }}) >= village_poor ~ 0.5,
                                          rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < village_poor ~ 0))
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

  # calculate all service duration
  lsoa_bustrips <- calculate_all_service_duration(lsoa_bustrips)

  #' remove intermediate values?
  lsoa_bustrips <- tidy_bustrips_data(lsoa_bustrips)

}

# this calculates the duration of good services across the day/evening/week. Maximum duration of good weekly service is 48 hours*
# Or should that be weekday*5 + sat + sun = 16*5 + 16 + 16 = 112?

calculate_good_service_duration <- function(lsoa_bustrips, weekday_multiplier = 1) {

  if_one_else_none <- function(x, hours) ifelse(x == 1, 1, 0) * hours

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate(weekday_good_duration = if_one_else_none(tph_weekday_Morning_Peak_score, 4) + if_one_else_none(tph_weekday_Midday_score, 5) + if_one_else_none(tph_weekday_Afternoon_Peak_score, 3) + if_one_else_none(tph_weekday_Evening_score, 4),
           saturday_good_duration = if_one_else_none(tph_Sat_Morning_Peak_score, 4) + if_one_else_none(tph_Sat_Midday_score, 5) + if_one_else_none(tph_Sat_Afternoon_Peak_score, 3) + if_one_else_none(tph_Sat_Evening_score, 4),
           sunday_good_duration = if_one_else_none(tph_Sun_Morning_Peak_score, 4) + if_one_else_none(tph_Sun_Midday_score, 5) + if_one_else_none(tph_Sun_Afternoon_Peak_score, 3) + if_one_else_none(tph_Sun_Evening_score, 4)) %>%
    mutate(weeklong_good_duration = (weekday_good_duration * weekday_multiplier) + saturday_good_duration + sunday_good_duration)


}

# this calculates duration of service regardless of frequency.
calculate_all_service_duration <- function(lsoa_bustrips, weekday_multiplier = 1) {

  # if above zero then count and multiple by hours of period, otherwise 0.
  if_some_else_none <- function(x, hours) if_else(round(x) > 0, 1, 0) * hours

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate(weekday_duration = if_some_else_none(tph_weekday_Morning_Peak, 4) + if_some_else_none(tph_weekday_Midday, 5) + if_some_else_none(tph_weekday_Afternoon_Peak, 3) + if_some_else_none(tph_weekday_Evening, 4),
           saturday_duration = if_some_else_none(tph_Sat_Morning_Peak, 4) + if_some_else_none(tph_Sat_Midday, 5) + if_some_else_none(tph_Sat_Afternoon_Peak, 3) + if_some_else_none(tph_Sat_Evening, 4),
           sunday_duration = if_some_else_none(tph_Sun_Morning_Peak, 4) + if_some_else_none(tph_Sun_Midday, 5) + if_some_else_none(tph_Sun_Afternoon_Peak, 3) + if_some_else_none(tph_Sun_Evening, 4)) %>%
    mutate(weeklong_duration = (weekday_duration * weekday_multiplier) + saturday_duration + sunday_duration)

}

tidy_bustrips_data <- function(lsoa_bustrips) {

  lsoa_bustrips <- lsoa_bustrips %>%
    select(lsoa11,
           #route_type,
           year,
           london_underground,
           #ru11ind,
           urban_rural_cat,
           rurality,
           max_number_routes,
           tph_weekday_Morning_Peak,
           tph_weekday_Midday,
           tph_weekday_Afternoon_Peak,
           tph_weekday_Evening,
           tph_weekday_Night,
           tph_Sat_Morning_Peak,
           tph_Sat_Midday,
           tph_Sat_Afternoon_Peak,
           tph_Sat_Evening,
           tph_Sat_Night,
           tph_Sun_Morning_Peak,
           tph_Sun_Midday,
           tph_Sun_Afternoon_Peak,
           tph_Sun_Evening,
           tph_Sun_Night,
           tph_daytime_avg,
           tph_weekday_Morning_Peak_service,
           # tph_weekday_Morning_Peak_score,
           tph_weekday_Midday_service,
           # tph_weekday_Midday_score,
           tph_weekday_Afternoon_Peak_service,
           # tph_weekday_Afternoon_Peak_score,
           tph_weekday_Evening_service,
           # tph_weekday_Evening_score,
           tph_Sat_Morning_Peak_service,
           # tph_Sat_Morning_Peak_score,
           tph_Sat_Midday_service,
           # tph_Sat_Midday_score,
           tph_Sat_Afternoon_Peak_service,
           # tph_Sat_Afternoon_Peak_score,
           tph_Sat_Evening_service,
           # tph_Sat_Evening_score,
           tph_Sun_Morning_Peak_service,
           # tph_Sun_Morning_Peak_score,
           tph_Sun_Midday_service,
           # tph_Sun_Midday_score,
           tph_Sun_Afternoon_Peak_service,
           # tph_Sun_Afternoon_Peak_score,
           tph_Sun_Evening_service,
           # tph_Sun_Evening_score,
           tph_daytime_avg_service,
           # tph_daytime_avg_score,
           weekday_service_score,
           saturday_service_score,
           sunday_service_score,
           overall_service_score,
           weekday_good_duration,
           saturday_good_duration,
           sunday_good_duration,
           weeklong_good_duration,
           weekday_duration,
           saturday_duration,
           sunday_duration,
           weeklong_duration)

}
