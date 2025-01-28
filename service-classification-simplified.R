#' Reduce slots into 4 time periods
#'  - Weekday day 6am-6pm
#'  - Weekday eve 6pm-10pm
#'  - Weekend day 6am-6pm
#'  - Weekend eve 6pm-10pm
#'  [CHECK THIS MAKES SENSE FOR SUNDAY, if not split Sunday and Saturday]
#'
#' Calculate current quintiles
#' Compare with 2007-10 quintiles
#' Just use 1-5 with 1 being the best service. Could convert to letters? A-E?



# main function to save xlsx outputs --------------------------------------

save_bustrip_quintiles_fairness_report <- function() {

  lsoa_bustrips_2023 <- make_simplified_time_periods(onspd, year_list = 2023)
  lsoa_bustrips_200710 <- make_simplified_time_periods(onspd, year_list = c(2007, 2008, 2009, 2010), year_name = "2007-10")

  lsoa_bustrips_200710_long <- calculate_bus_service_quintiles(lsoa_bustrips_200710)
  lsoa_bustrips_2023_long <- make_lsoa_bustrips_long(lsoa_bustrips_2023)

  lsoa_bustrips_2023_daytime <- add_200710_quintiles_to_2023_data(lsoa_bustrips_2023, lsoa_bustrips_200710_long, time_period_name = "tph_weekday_daytime")

  lsoa_bus_200710_weekday_summary <- summarise_best_worst_quintiles(lsoa_bustrips_200710_long, time_period_name = "tph_weekday_daytime")
  lsoa_bus_2023_weekday_summary <- summarise_best_worst_quintiles(lsoa_bustrips_2023_daytime, time_period_name = "tph_weekday_daytime")

  save_2023_bustrip_summary(lsoa_bus_2023_weekday_summary,
                            filename = "bus-services-2023-best-worst-cars-jan25-test.xlsx")

  save_200710_2023_quintile_comparison(lsoa_bustrips_2023_daytime,
                                       lsoa_bustrips_2023_long,
                                       time_period_name = "tph_weekday_daytime",
                                       filename = "bus-service-comparisons-jan25-test.xlsx")

}

# detailed functions defined ----------------------------------------------

make_simplified_time_periods <- function(onspd, year_list, year_name = "2007-10") {

  lsoa_bustrips_year <- load_lsoa_bustrips(onspd, year_list)

  lsoa_bustrips_year <- lsoa_bustrips_year %>%
    transmute(lsoa11,
              year,
              rurality,
              urban_rural_cat,
              tph_weekday_daytime = ((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12,
              tph_weekday_Evening,
              tph_Sat_daytime = ((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12,
              tph_Sat_Evening,
              tph_Sun_daytime = ((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12,
              tph_Sun_Evening)

  if(length(year_list) > 1) {
    lsoa_bustrips_year <- lsoa_bustrips_year %>%
      group_by(lsoa11,
               year = year_name,
               rurality,
               urban_rural_cat) %>%
      summarise(tph_weekday_daytime = mean(tph_weekday_daytime, na.rm = TRUE),
                tph_weekday_Evening = mean(tph_weekday_Evening, na.rm = TRUE),
                tph_Sat_daytime = mean(tph_Sat_daytime, na.rm = TRUE),
                tph_Sat_Evening = mean(tph_Sat_Evening, na.rm = TRUE),
                tph_Sun_daytime = mean(tph_Sun_daytime, na.rm = TRUE),
                tph_Sun_Evening = mean(tph_Sun_Evening, na.rm = TRUE)) %>%
      ungroup()
  }

  lsoa_bustrips_year$rurality <- factor(lsoa_bustrips_year$rurality,
                                        levels = c("Urban: Conurbation",
                                                   "Urban: City and Town",
                                                   "Rural: Town and Fringe",
                                                   "Rural: Village/Hamlets/Isolated Dwellings"))

  return(lsoa_bustrips_year)

}

make_lsoa_bustrips_long <- function(lsoa_bustrips) {

  # gather, removing not indicator data
  lsoa_bustrips_long <- lsoa_bustrips %>%
    gather(key = time_period,
           value = tph,
           -lsoa11,
           -year,
           -rurality,
           -urban_rural_cat)

}

calculate_bus_service_quintiles <- function(lsoa_bustrips) {

  lsoa_bustrips_quintiles <- lsoa_bustrips %>%
    group_by(rurality) %>%
    mutate(across(where(is.numeric), \(x) ntile(desc(x), n = 5), .names = "{col}_quintile")) %>%
    ungroup()

  #table(lsoa_bustrips_200710$rurality,
  #      lsoa_bustrips_200710$tph_weekday_daytime_quintile)

  # find quintile bands
  lsoa_bustrips_long <- make_lsoa_bustrips_long(lsoa_bustrips_quintiles)

  lsoa_bustrips_long <- lsoa_bustrips_long %>%
    mutate(quintile = ifelse(grepl("_quintile", time_period), TRUE, FALSE))

  quintile_data <- lsoa_bustrips_long %>%
    filter(quintile) %>%
    transmute(lsoa11,
              time_period = gsub("_quintile", "", time_period),
              quintile = tph)

  lsoa_bustrips_long <- lsoa_bustrips_long %>%
    filter(!quintile) %>%
    select(-quintile)

  lsoa_bustrips_long <- left_join(lsoa_bustrips_long, quintile_data, by = c("lsoa11", "time_period"))
  lsoa_bustrips_long$quintile_letter = LETTERS[lsoa_bustrips_long$quintile]

  return(lsoa_bustrips_long)

}


make_quintile_lookup <- function(lsoa_bustrips_long) {

  quintile_bands_lookup <- lsoa_bustrips_long %>%
    group_by(rurality,
             time_period,
             quintile,
             quintile_letter) %>%
    summarise(tph_min = min(tph),
              tph_max = max(tph)) %>%
    ungroup() %>%
    filter(!is.na(tph_min))

}

add_200710_quintiles_to_2023_data <- function(lsoa_bustrips_2023,
                                              lsoa_bustrips_200710_long,
                                              time_period_name = "tph_weekday_daytime") {

  # apply quintile bands to 2023 data
  lsoa_bustrips_2023_long <- lsoa_bustrips_2023 %>%
    gather(key = time_period,
           value = tph,
           -lsoa11,
           -year,
           -rurality,
           -urban_rural_cat)

  lsoa_bustrips_2023_timeperiod <- lsoa_bustrips_2023_long %>%
    filter(time_period == time_period_name) %>%
    group_by(rurality) %>%
    mutate(quintile = ntile(desc(tph), n = 5)) %>%
    ungroup()

  lsoa_bustrips_2023_timeperiod$quintile_letter = LETTERS[lsoa_bustrips_2023_timeperiod$quintile]

  # lsoa_bustrips_2023_timeperiod_summary <- lsoa_bustrips_2023_timeperiod %>%
  #   filter(!is.na(tph)) %>%
  #   group_by(rurality,
  #            quintile,
  #            quintile_letter) %>%
  #   summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
  #             tph_mean = round(mean(tph, na.rm = TRUE), 1),
  #             tph_max = round(max(tph, na.rm = TRUE), 1)) %>%
  #   ungroup()

  quintile_lookup_200710 <- make_quintile_lookup(lsoa_bustrips_200710_long)

  qt <- quintile_lookup_200710 %>%
    filter(time_period == time_period_name) %>%
    transmute(rurality,
              rurality_4x = case_when(rurality == "Urban: Conurbation" ~ "urban_conurb",
                                      rurality == "Urban: City and Town" ~ "urban_city",
                                      rurality == "Rural: Town and Fringe" ~ "rural_town",
                                      rurality == "Rural: Village/Hamlets/Isolated Dwellings" ~ "rural_village"),
              time_period,
              quintile,
              quintile_letter,
              tph_min = round(tph_min, 1),
              tph_max = round(tph_max, 2))

  # select banding to be used for 2023 data:
  conurb_a <- qt[qt$rurality_4x == "urban_conurb" & qt$quintile_letter == "A" , ]$tph_min
  conurb_b <- qt[qt$rurality_4x == "urban_conurb" & qt$quintile_letter == "B" , ]$tph_min
  conurb_c <- qt[qt$rurality_4x == "urban_conurb" & qt$quintile_letter == "C" , ]$tph_min
  conurb_d <- qt[qt$rurality_4x == "urban_conurb" & qt$quintile_letter == "D" , ]$tph_min

  city_a <- qt[qt$rurality_4x == "urban_city" & qt$quintile_letter == "A" , ]$tph_min
  city_b <- qt[qt$rurality_4x == "urban_city" & qt$quintile_letter == "B" , ]$tph_min
  city_c <- qt[qt$rurality_4x == "urban_city" & qt$quintile_letter == "C" , ]$tph_min
  city_d <- qt[qt$rurality_4x == "urban_city" & qt$quintile_letter == "D" , ]$tph_min

  town_a <- qt[qt$rurality_4x == "rural_town" & qt$quintile_letter == "A" , ]$tph_min
  town_b <- qt[qt$rurality_4x == "rural_town" & qt$quintile_letter == "B" , ]$tph_min
  town_c <- qt[qt$rurality_4x == "rural_town" & qt$quintile_letter == "C" , ]$tph_min
  town_d <- qt[qt$rurality_4x == "rural_town" & qt$quintile_letter == "D" , ]$tph_min

  village_a <- qt[qt$rurality_4x == "rural_village" & qt$quintile_letter == "A" , ]$tph_min
  village_b <- qt[qt$rurality_4x == "rural_village" & qt$quintile_letter == "B" , ]$tph_min
  village_c <- qt[qt$rurality_4x == "rural_village" & qt$quintile_letter == "C" , ]$tph_min
  village_d <- qt[qt$rurality_4x == "rural_village" & qt$quintile_letter == "D" , ]$tph_min

  # apply banding to allocate LSOAs using 2010 quintiles on 2023 data
  lsoa_bustrips_2023_timeperiod <- lsoa_bustrips_2023_timeperiod %>%
    mutate(quintile_2010 = case_when(rurality == "Urban: Conurbation" & tph >= conurb_a ~ 1,
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
  lsoa_bustrips_2023_timeperiod$quintile_2010_letter = LETTERS[lsoa_bustrips_2023_timeperiod$quintile_2010]

  # add 2010 data
  lsoa_bus_200710_timeperiod <- lsoa_bustrips_200710_long %>%
    filter(time_period == time_period_name) %>%
    select(lsoa11,
           tph_200710 = tph)

  lsoa_bustrips_2023_timeperiod <- left_join(lsoa_bustrips_2023_timeperiod, lsoa_bus_200710_timeperiod, by = "lsoa11")

  return(lsoa_bustrips_2023_timeperiod)

}


#' finalise data for reports
#' We need the following:
#'  - best areas in 2023 by rurality, including TPH, and car ownership stats
#'  - worst areas in 2023 by rurality, including TPH, and car ownership stats
#'
#'  - best and worst areas in 2010, TPH
#'  - best and worst areas in 2023, TPH

summarise_best_worst_quintiles <- function(lsoa_bus, time_period_name = "tph_weekday_daytime") {

  lsoa_bus_timeperiod <- lsoa_bus %>%
    filter(time_period == time_period_name)

  # add car ownership
  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")
  lsoa_bus_timeperiod <- add.noncar.owners.to.lsoa.dataset(lsoa_bus_timeperiod, rename_lsoa_code = TRUE)

  lsoa_bus_summary <- lsoa_bus_timeperiod %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             quintile,
             quintile_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              car_ownership_no_cars = round(sum(car_ownership_no_cars), -2),
              car_ownership_no_cars_pct = sum(car_ownership_no_cars) / sum(census.households)) %>%
    ungroup()

  lsoa_bus_summary <- lsoa_bus_summary %>%
    filter(quintile_letter %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(quintile_letter == "A" ~ "Best 20%",
                                      quintile_letter == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Households without a car (No.)` = car_ownership_no_cars,
           `Households without a car (%)` = car_ownership_no_cars_pct)

  lsoa_bus_summary <- lsoa_bus_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)

  lsoa_bus_summary <- lsoa_bus_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

}


# 2023: Best and Worst LSOAS and car ownership
save_2023_bustrip_summary <- function(lsoa_bus_2023_timeperiod_summary,
                                      filename = "bus-services-2023-best-worst-cars-jan25.xlsx") {

  source("../environmental-data-for-change/scripts/save-foe-workbook.R")

  full_filename_cdrive <- paste0("outputs/", filename)
  full_filename_onedrive <- paste0("../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/November 2024/", filename)

  message(paste0("Saving: ", full_filename_cdrive))
  message(paste0("Saving: ", full_filename_onedrive))

  save_as_spreadsheet_multiformat(number_of_tabs = 1,
                                  tab1_data = lsoa_bus_2023_timeperiod_summary,
                                  tab1_name = "2023",
                                  xlsx_path = full_filename_cdrive,
                                  #alternative_xlsx_path = full_filename_onedrive,
                                  number_cols_1 = c(4,7),
                                  percent_cols_1 = c(3,6),
                                  number_decimal = FALSE,
                                  percent_decimal = FALSE)

}




summarise_top_bottom_neighbourhoods <- function(lsoa_bustrips_long,
                                                time_period_name = "",
                                                qt,
                                                qt_let) {

  if(time_period_name != "") {
    lsoa_bustrips_long <- lsoa_bustrips_long %>%
      filter(time_period == time_period_name)
  }

  lsoa_bustrips_2023_timeperiod_summary <- lsoa_bustrips_long %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             {{ qt }},
             {{ qt_let }}) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_2023_timeperiod_summary <- lsoa_bustrips_2023_timeperiod_summary %>%
    filter({{ qt_let }} %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when({{ qt_let }} == "A" ~ "Best 20%",
                                      {{ qt_let }} == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_2023_timeperiod_summary <- lsoa_bustrips_2023_timeperiod_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)

  lsoa_bustrips_2023_timeperiod_summary <- lsoa_bustrips_2023_timeperiod_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

  # make totals
  lsoa_bustrips_2023_timeperiod_totals <- lsoa_bustrips_long %>%
    filter(!is.na(tph)) %>%
    group_by({{ qt }},
             {{ qt_let }}) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_2023_timeperiod_totals <- lsoa_bustrips_2023_timeperiod_totals %>%
    filter({{ qt_let }} %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when({{ qt_let }} == "A" ~ "Best 20%",
                                      {{ qt_let }} == "E" ~ "Worst 20%")) %>%
    select(neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_2023_timeperiod_totals <- lsoa_bustrips_2023_timeperiod_totals %>%
    gather(key = indicator,
           value = val,
           -neighbourhoods)

  lsoa_bustrips_2023_timeperiod_totals <- lsoa_bustrips_2023_timeperiod_totals %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val) %>%
    mutate(rurality = "All neighbourhoods") %>%
    select(rurality,
           everything())

  # bring rurality and totals together in final table
  lsoa_bustrips_2023_timeperiod_summary <- bind_rows(lsoa_bustrips_2023_timeperiod_summary,
                                                     lsoa_bustrips_2023_timeperiod_totals)

  return(lsoa_bustrips_2023_timeperiod_summary)

}



## Use best quintile bands for 2010-2023 comparison
save_200710_2023_quintile_comparison <- function(lsoa_bustrips_2023_timeperiod,
                                                 lsoa_bustrips_2023_long,
                                                 time_period_name,
                                                 filename = "bus-service-comparisons-jan25.xlsx") {

  lsoa_bustrips_2023_summary <- summarise_top_bottom_neighbourhoods(lsoa_bustrips_long = lsoa_bustrips_2023_timeperiod,
                                                                    qt = quintile_2010,
                                                                    qt_let = quintile_2010_letter)

  lsoa_bustrips_200710_summary <- summarise_top_bottom_neighbourhoods(lsoa_bustrips_long = lsoa_bustrips_200710_long,
                                                                      time_period_name = time_period_name,
                                                                      qt = quintile,
                                                                      qt_let = quintile_letter)

  source("../environmental-data-for-change/scripts/save-foe-workbook.R")

  full_filename_cdrive <- paste0("outputs/", filename)
  full_filename_onedrive <- paste0("../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/", filename)

  message(paste0("Saving: ", full_filename_cdrive))
  message(paste0("Saving: ", full_filename_onedrive))

  save_as_spreadsheet_multiformat(number_of_tabs = 2,
                                  tab1_data = lsoa_bustrips_200710_summary,
                                  tab1_name = "2007-10",
                                  tab2_data = lsoa_bustrips_2023_summary,
                                  tab2_name = "2023",
                                  xlsx_path = full_filename_cdrive,
                                  #alternative_xlsx_path = full_filename_onedrive,
                                  number_cols_1 = c(3,5),
                                  percent_cols_1 = 0,
                                  number_cols_2 = c(3,5),
                                  percent_cols_2 = 0)

}
