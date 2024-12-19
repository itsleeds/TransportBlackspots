
service_quality_quintiles <- function(lsoa_bustrips, tph, tph_quintile) {

  urban_1 = 90
  urban_2 = 55
  urban_3 = 35
  urban_4 = 20

  city_1  =  45
  city_2  =  25
  city_3  =  15
  city_4  =  10

  town_1 = 15
  town_2 = 10
  town_3 = 6
  town_4 = 4

  village_1  = 13
  village_2  = 7
  village_3  = 5
  village_4  = 2

  lsoa_bustrips <- lsoa_bustrips %>%
    mutate("{{ tph }}_quintile" := case_when(rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_1 ~ 5,
                                             rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_2 ~ 4,
                                             rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_3 ~ 3,
                                             rurality == "Urban: Conurbation" & round({{ tph }}) >= urban_4 ~ 2,
                                             rurality == "Urban: Conurbation" & round({{ tph }}) < urban_4 ~ 1,

                                             rurality == "Urban: City and Town" & round({{ tph }}) >= city_1 ~ 5,
                                             rurality == "Urban: City and Town" & round({{ tph }}) >= city_2 ~ 4,
                                             rurality == "Urban: City and Town" & round({{ tph }}) >= city_3 ~ 3,
                                             rurality == "Urban: City and Town" & round({{ tph }}) >= city_4 ~ 2,
                                             rurality == "Urban: City and Town" & round({{ tph }}) < city_4 ~ 1,

                                             rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_1 ~ 5,
                                             rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_2 ~ 4,
                                             rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_3 ~ 3,
                                             rurality == "Rural: Town and Fringe" & round({{ tph }}) >= town_4 ~ 2,
                                             rurality == "Rural: Town and Fringe" & round({{ tph }}) < town_4 ~ 1,

                                             rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_1 ~ 5,
                                             rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_2 ~ 4,
                                             rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_3 ~ 3,
                                             rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) >= village_4 ~ 2,
                                             rurality == "Rural: Village/Hamlets/Isolated Dwellings" & round({{ tph }}) < village_4 ~ 1)) %>%

    mutate("{{ tph }}_service" := case_when({{ tph_quintile }} == 5 ~ "Very good",
                                            {{ tph_quintile }} == 4 ~ "Good",
                                            {{ tph_quintile }} == 3 ~ "Average",
                                            {{ tph_quintile }} == 2 ~ "Poor",
                                            {{ tph_quintile }} == 1 ~ "Very poor")) %>%

    mutate("{{ tph }}_score" := case_when({{ tph_quintile }} == 5 ~ 2,
                                          {{ tph_quintile }} == 4 ~ 1.5,
                                          {{ tph_quintile }} == 3 ~ 1,
                                          {{ tph_quintile }} == 2 ~ 0.5,
                                          {{ tph_quintile }} == 1 ~ 0))

}

classify_service_quality_in_quintiles <- function(lsoa_bustrips) {

  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_weekday_Morning_Peak, tph_weekday_Morning_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_weekday_Midday, tph_weekday_Midday_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_weekday_Afternoon_Peak, tph_weekday_Afternoon_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_weekday_Evening, tph_weekday_Evening_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sat_Morning_Peak, tph_Sat_Morning_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sat_Midday, tph_Sat_Midday_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sat_Afternoon_Peak, tph_Sat_Afternoon_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sat_Evening, tph_Sat_Evening_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sun_Morning_Peak, tph_Sun_Morning_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sun_Midday, tph_Sun_Midday_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sun_Afternoon_Peak, tph_Sun_Afternoon_Peak_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_Sun_Evening, tph_Sun_Evening_quintile)
  lsoa_bustrips <- service_quality_quintiles(lsoa_bustrips, tph_daytime_avg, tph_daytime_avg_quintile)

  test <- lsoa_bustrips %>%
    group_by(rurality,
             tph_weekday_Morning_Peak_quintile,
             tph_weekday_Morning_Peak_score,
             tph_weekday_Morning_Peak_service) %>%
    summarise(min(tph_weekday_Morning_Peak),
              mean(tph_weekday_Morning_Peak, na.rm = TRUE),
              max(tph_weekday_Morning_Peak),
              n()) %>%
    ungroup()

  table(lsoa_bustrips$rurality,
        is.na(lsoa_bustrips$tph_Sat_Morning_Peak))

  # calculate service score
  lsoa_bustrips <- lsoa_bustrips %>%
    mutate(weekday_service_score = tph_weekday_Morning_Peak_score + tph_weekday_Midday_score + tph_weekday_Afternoon_Peak_score + tph_weekday_Evening_score,
           saturday_service_score = tph_Sat_Morning_Peak_score + tph_Sat_Midday_score + tph_Sat_Afternoon_Peak_score + tph_Sat_Evening_score,
           sunday_service_score = tph_Sun_Morning_Peak_score + tph_Sun_Midday_score + tph_Sun_Afternoon_Peak_score + tph_Sun_Evening_score) %>%
    mutate(overall_service_score = weekday_service_score + saturday_service_score + sunday_service_score)

  #
  hist(lsoa_bustrips$overall_service_score)
  summary(lsoa_bustrips$overall_service_score)

  # calculate good service duration?
  lsoa_bustrips <- calculate_verygood_service_duration(lsoa_bustrips, weekday_multiplier = 5)

  # calculate all service duration
  lsoa_bustrips <- calculate_all_service_duration(lsoa_bustrips, weekday_multiplier = 5)

  #' remove intermediate values?
  lsoa_bustrips <- tidy_bustrips_data_quintiles(lsoa_bustrips)

  # turn quintile service scores into factor with levels
  lsoa_bustrips$tph_weekday_Morning_Peak_service = make_service_factor(lsoa_bustrips$tph_weekday_Morning_Peak_service)
  lsoa_bustrips$tph_weekday_Midday_service = make_service_factor(lsoa_bustrips$tph_weekday_Midday_service)
  lsoa_bustrips$tph_weekday_Afternoon_Peak_service = make_service_factor(lsoa_bustrips$tph_weekday_Afternoon_Peak_service)
  lsoa_bustrips$tph_weekday_Evening_service = make_service_factor(lsoa_bustrips$tph_weekday_Evening_service)
  lsoa_bustrips$tph_Sat_Morning_Peak_service = make_service_factor(lsoa_bustrips$tph_Sat_Morning_Peak_service)
  lsoa_bustrips$tph_Sat_Midday_service = make_service_factor(lsoa_bustrips$tph_Sat_Midday_service)
  lsoa_bustrips$tph_Sat_Afternoon_Peak_service = make_service_factor(lsoa_bustrips$tph_Sat_Afternoon_Peak_service)
  lsoa_bustrips$tph_Sat_Evening_service = make_service_factor(lsoa_bustrips$tph_Sat_Evening_service)
  lsoa_bustrips$tph_Sun_Morning_Peak_service = make_service_factor(lsoa_bustrips$tph_Sun_Morning_Peak_service)
  lsoa_bustrips$tph_Sun_Midday_service = make_service_factor(lsoa_bustrips$tph_Sun_Midday_service)
  lsoa_bustrips$tph_Sun_Afternoon_Peak_service = make_service_factor(lsoa_bustrips$tph_Sun_Afternoon_Peak_service)
  lsoa_bustrips$tph_Sun_Evening_service = make_service_factor(lsoa_bustrips$tph_Sun_Evening_service)
  lsoa_bustrips$tph_daytime_avg_service = make_service_factor(lsoa_bustrips$tph_daytime_avg_service)

  return(lsoa_bustrips)

}

# this calculates the duration of good services across the day/evening/week. Maximum duration of good weekly service is 48 hours*
# Or should that be weekday*5 + sat + sun = 16*5 + 16 + 16 = 112?

calculate_verygood_service_duration <- function(lsoa_bustrips, weekday_multiplier = 1) {

  if_one_else_none <- function(x, hours) ifelse(x == 2, 1, 0) * hours

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

make_service_factor <- function(x) {
  x <- factor(x,
              levels = c("Very good",
                         "Good",
                         "Average",
                         "Poor",
                         "Very poor"))
}


tidy_bustrips_data_quintiles <- function(lsoa_bustrips) {

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
           tph_weekday_Morning_Peak_quintile,
           tph_weekday_Midday_quintile,
           tph_weekday_Afternoon_Peak_quintile,
           tph_weekday_Evening_quintile,
           tph_Sat_Morning_Peak_quintile,
           tph_Sat_Midday_quintile,
           tph_Sat_Afternoon_Peak_quintile,
           tph_Sat_Evening_quintile,
           tph_Sun_Morning_Peak_quintile,
           tph_Sun_Midday_quintile,
           tph_Sun_Afternoon_Peak_quintile,
           tph_Sun_Evening_quintile,
           tph_daytime_avg_quintile,
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
