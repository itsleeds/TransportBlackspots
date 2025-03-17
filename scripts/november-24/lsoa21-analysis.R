#' this function takes four years worth of historical data, combines into one file,
#' simplifies the 15 time periods into 9, and then calculates the average trips per hour
#' over the four years (2007-2010) to provide a baseline average
make_simplified_bustrips_lsoa21_200710 <- function() {

  # get individual year data
  lsoa21_bustrips_07 <- load_lsoa21_bustrips(year = 2007)
  lsoa21_bustrips_08 <- load_lsoa21_bustrips(year = 2008)
  lsoa21_bustrips_09 <- load_lsoa21_bustrips(year = 2009)
  lsoa21_bustrips_10 <- load_lsoa21_bustrips(year = 2010)

  # combine
  lsoa21_bustrips_0710 <- bind_rows(lsoa21_bustrips_07,
                                    lsoa21_bustrips_08,
                                    lsoa21_bustrips_09,
                                    lsoa21_bustrips_10)

  # calculate simplified time periods, combining the three daytime periods between 6am and 6pm into one
  lsoa21_bustrips_simplified_0710 <- lsoa21_bustrips_0710 %>%
    transmute(lsoa21,
              year = data_year,
              rurality,
              urban_rural_cat,
              tph_weekday_daytime = round(((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12, 2),
              tph_weekday_Evening,
              tph_weekday_Night,
              tph_Sat_daytime = round(((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12, 2),
              tph_Sat_Evening,
              tph_Sat_Night,
              tph_Sun_daytime = round(((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12, 2),
              tph_Sun_Evening,
              tph_Sun_Night)


  # aggregate and calculate average across 2007-10 to get a baseline tph in each LSOA
  # generate list of cols to aggregate
  tph_cols <- c("tph_weekday_daytime",
                "tph_weekday_Evening",
                "tph_weekday_Night",
                "tph_Sat_daytime",
                "tph_Sat_Evening",
                "tph_Sat_Night",
                "tph_Sun_daytime",
                "tph_Sun_Evening",
                "tph_Sun_Night")
  # aggregation process
  lsoa21_bustrips_simplified_0710 <- lsoa21_bustrips_simplified_0710 %>%
    group_by(lsoa21,
             year = "2007-10",
             rurality,
             urban_rural_cat) %>%
    summarise(across(all_of(tph_cols), \(x) mean(x, na.rm = TRUE))) %>%
    ungroup()

  # turn rurality into a factor so that more urban areas comes first, most rural last (i.e. not alphabetically)
  lsoa21_bustrips_simplified_0710$rurality <- factor(lsoa21_bustrips_simplified_0710$rurality,
                                                     levels = c("Urban: Conurbation",
                                                                "Urban: City and Town",
                                                                "Rural: Town and Fringe",
                                                                "Rural: Village/Hamlets/Isolated Dwellings"))

  return(lsoa21_bustrips_simplified_0710)

}

#' this function loads and simplifies one year of data from one file for 2024, but results in a
#' a dataframe with the same structure as the 2007-10 data frame produced from the
#' 'make_simplified_bustrips_lsoa21_200710()' function above.
make_simplified_bustrips_lsoa21_2024 <- function() {

  # get data for 2024
  lsoa21_bustrips_24 <- load_lsoa21_bustrips(year = 2024)

  # calculate simplified time periods, combining the three daytime periods between 6am and 6pm into one
  lsoa21_bustrips_24 <- lsoa21_bustrips_24 %>%
    transmute(lsoa21,
              year = 2024,
              rurality,
              urban_rural_cat,
              tph_weekday_daytime = round(((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12, 2),
              tph_weekday_Evening,
              tph_weekday_Night,
              tph_Sat_daytime = round(((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12, 2),
              tph_Sat_Evening,
              tph_Sat_Night,
              tph_Sun_daytime = round(((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12, 2),
              tph_Sun_Evening,
              tph_Sun_Night)

  # turn rurality into a factor so that more urban comes first
  lsoa21_bustrips_24$rurality <- factor(lsoa21_bustrips_24$rurality,
                                        levels = c("Urban: Conurbation",
                                                   "Urban: City and Town",
                                                   "Rural: Town and Fringe",
                                                   "Rural: Village/Hamlets/Isolated Dwellings"))

  return(lsoa21_bustrips_24)

}

#' this function turns a wide data frame in which the trips per hours for different time
#' periods are individual columns, into a long table with a column identifying the time periods
#' and a second with the TPH values in this.
#' The resulting table can be filtered to pull out individual time period data
make_lsoa_bustrips_long_lsoa21 <- function(lsoa_bustrips) {

  # gather, removing not indicator data
  lsoa_bustrips_long <- lsoa_bustrips %>%
    gather(key = time_period,
           value = tph,
           -lsoa21,
           -year,
           -rurality,
           -urban_rural_cat) %>%
    mutate(time_period = gsub("tph_", "", time_period))

}

#' This calculates quintiles for the TPH values. It take the wide bus trips data as
#' an input but results in a long table in which each LSOA has a quintile band assigned
#' for each time periods. Assignment of quintiles is split across 4x different rural/urban
#' classifications
calculate_bus_service_quintiles_lsoa21 <- function(lsoa_bustrips) {

  # calculate the quintile for each time period by rurality (wide table)
  lsoa_bustrips_quintiles <- lsoa_bustrips %>%
    group_by(rurality) %>%
    mutate(across(where(is.numeric), \(x) ntile(desc(x), n = 5), .names = "{col}_quintile")) %>%
    ungroup()

  # convert into a long table
  lsoa_bustrips_long <- make_lsoa_bustrips_long_lsoa21(lsoa_bustrips_quintiles)
  # identify the quintile data points
  lsoa_bustrips_long <- lsoa_bustrips_long %>%
    mutate(quintile = ifelse(grepl("_quintile", time_period), TRUE, FALSE))
  # and filter on these to pull them out of the data
  quintile_data <- lsoa_bustrips_long %>%
    filter(quintile) %>%
    transmute(lsoa21,
              time_period = gsub("_quintile", "", time_period),
              quintile = tph)

  # then pull out the other columns (i.e. the TPH data from which the quintiles were made)
  lsoa_bustrips_long <- lsoa_bustrips_long %>%
    filter(!quintile) %>%
    select(-quintile)

  # join the quintile data back on to the main spine, so that TPH and quintiles have their own separate columns
  lsoa_bustrips_long <- left_join(lsoa_bustrips_long, quintile_data, by = c("lsoa21", "time_period"))
  # assign a quintile letter to the quintiles in addition to the quintile number.
  lsoa_bustrips_long$quintile_letter = LETTERS[lsoa_bustrips_long$quintile]

  return(lsoa_bustrips_long)

}

#' take the full LSOA long data set with all time periods and including quintiles assigned
#' above, and summarise the stats, identifying the bandings or range in each quintile
#' for each time period in each rural/urban classification.
make_quintile_lookup_lsoa21 <- function(lsoa_bustrips_long) {

  quintile_bands_lookup <- lsoa_bustrips_long %>%
    group_by(rurality,
             time_period,
             quintile,
             quintile_letter) %>%
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
                                              lsoa_bustrips_200710_long,
                                              time_period_name = "weekday_daytime") {

  # make long df from 2024 data
  lsoa_bustrips_2024_long <- make_lsoa_bustrips_long_lsoa21(lsoa_bustrips_2024)
  # filter for the time period of interest and make a quintile column based on current data
  lsoa_bustrips_2024_timeperiod <- lsoa_bustrips_2024_long %>%
    filter(time_period == time_period_name) %>%
    group_by(rurality) %>%
    mutate(quintile = ntile(desc(tph), n = 5)) %>%
    ungroup()
  # add a quintile letter column
  lsoa_bustrips_2024_timeperiod$quintile_letter = LETTERS[lsoa_bustrips_2024_timeperiod$quintile]

  # identify the quintile bands from 200710 data
  quintile_lookup_200710 <- make_quintile_lookup_lsoa21(lsoa_bustrips_200710_long)
  # filter this table for the time period of interest
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

  # identify the quintile banding from the 200710 data, to be applied for 2024 data:
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

  # apply banding to allocate LSOAs using 2010 quintiles on 2024 data
  lsoa_bustrips_2024_timeperiod <- lsoa_bustrips_2024_timeperiod %>%
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
  lsoa_bustrips_2024_timeperiod$quintile_2010_letter = LETTERS[lsoa_bustrips_2024_timeperiod$quintile_2010]

  # add 2010 TPHs data to sit along side the 200710 quintiles, so both the quintiles
  # and TPH can be directly compared across each LSOA
  lsoa_bus_200710_timeperiod <- lsoa_bustrips_200710_long %>%
    mutate(time_period = gsub("tph_", "", time_period)) %>%
    filter(time_period == time_period_name) %>%
    transmute(lsoa21,
              tph_200710 = round(tph, 2))

  lsoa_bustrips_2024_timeperiod <- left_join(lsoa_bustrips_2024_timeperiod, lsoa_bus_200710_timeperiod, by = "lsoa21")

  return(lsoa_bustrips_2024_timeperiod)

}

#' for a given year of data (at LSOA21 level), this function takes a time periods of interest
#' and returns summary statistics on the best and worst quintiles, including adding in
#' data on the number/proportion of people who don't own cars. The results are aggregated
#' by rural urban classification
summarise_best_worst_quintiles_lsoa21 <- function(lsoa_bus, time_period_name = "weekday_daytime") {

  lsoa_bus_timeperiod <- lsoa_bus %>%
    filter(time_period == time_period_name)

  # add car ownership by using common functions from another repository
  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")
  lsoa_bus_timeperiod <- add_noncar_owners_to_lsoa21_data(lsoa_bus_timeperiod)

  # aggregate summary data for all quintiles
  lsoa_bus_summary <- lsoa_bus_timeperiod %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             quintile,
             quintile_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              car_ownership_no_cars = round(sum(car_ownership_no_cars), -2),
              car_ownership_no_cars_pct = round(sum(car_ownership_no_cars) / sum(census_households), 4)) %>%
    ungroup()

  # filter for top and bottom deciles and add label to these.
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

  # restructure table to a nicer wider presentation format
  # gather
  lsoa_bus_summary <- lsoa_bus_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)
  # combine and then spread
  lsoa_bus_summary <- lsoa_bus_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

  # turn numerical cols back to numbers (otherwise they are characters)
  lsoa_bus_summary[3] <- as.numeric(lsoa_bus_summary[[3]])
  lsoa_bus_summary[4] <- as.numeric(lsoa_bus_summary[[4]])
  lsoa_bus_summary[6] <- as.numeric(lsoa_bus_summary[[6]])
  lsoa_bus_summary[7] <- as.numeric(lsoa_bus_summary[[7]])

  return(lsoa_bus_summary)

}

#' Another function to identify statistics in the top and bottom deciles, this one
#' just summarises the number of quintiles and the distribution of TPH in the top
#' and bottom deciles. However, it is typically used as a comparison on 2010 quintiles
#' across different years/data sets. These have different column headings in more
#' recent data, hence the need to take the quintile and quintile letter as function
#' parameters.
summarise_top_bottom_neighbourhoods_lsoa21 <- function(lsoa_bustrips_long,
                                                time_period_name = "",
                                                qt,
                                                qt_let) {

  if(time_period_name != "") {
    lsoa_bustrips_long <- lsoa_bustrips_long %>%
      filter(time_period == time_period_name)
  }

  lsoa_bustrips_timeperiod_summary <- lsoa_bustrips_long %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             {{ qt }},
             {{ qt_let }}) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_timeperiod_summary <- lsoa_bustrips_timeperiod_summary %>%
    filter({{ qt_let }} %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when({{ qt_let }} == "A" ~ "Best 20%",
                                      {{ qt_let }} == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_timeperiod_summary <- lsoa_bustrips_timeperiod_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)

  lsoa_bustrips_timeperiod_summary <- lsoa_bustrips_timeperiod_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

  # make totals
  lsoa_bustrips_timeperiod_totals <- lsoa_bustrips_long %>%
    filter(!is.na(tph)) %>%
    group_by({{ qt }},
             {{ qt_let }}) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_timeperiod_totals <- lsoa_bustrips_timeperiod_totals %>%
    filter({{ qt_let }} %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when({{ qt_let }} == "A" ~ "Best 20%",
                                      {{ qt_let }} == "E" ~ "Worst 20%")) %>%
    select(neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_timeperiod_totals <- lsoa_bustrips_timeperiod_totals %>%
    gather(key = indicator,
           value = val,
           -neighbourhoods)

  lsoa_bustrips_timeperiod_totals <- lsoa_bustrips_timeperiod_totals %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val) %>%
    mutate(rurality = "All neighbourhoods") %>%
    select(rurality,
           everything())

  # bring rurality and totals together in final table
  lsoa_bustrips_timeperiod_summary <- bind_rows(lsoa_bustrips_timeperiod_summary,
                                                lsoa_bustrips_timeperiod_totals)


  lsoa_bustrips_timeperiod_summary[3] <- as.numeric(lsoa_bustrips_timeperiod_summary[[3]])
  lsoa_bustrips_timeperiod_summary[5] <- as.numeric(lsoa_bustrips_timeperiod_summary[[5]])


  return(lsoa_bustrips_timeperiod_summary)

}

# Save Excel outputs]
save_quintile_outputs_lsoa21 <- function(quintile_summary_2024,
                                         quintile_summary_200710,
                                         top_bottom_qunitiles_2024,
                                         top_bottom_qunitiles_200710,
                                         filename) {


  source("../environmental-data-for-change/scripts/save-foe-workbook.R")

  full_filename_cdrive <- paste0("outputs/", filename)
  full_filename_onedrive <- paste0("../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/November 2024/", filename)

  message(paste0("Saving: ", full_filename_cdrive))
  message(paste0("Saving: ", full_filename_onedrive))

  data.frame(id = 1:ncol(top_bottom_qunitiles_200710), sapply(top_bottom_qunitiles_200710, class))

  save_as_spreadsheet_multiformat(number_of_tabs = 4,
                                  tab1_data = quintile_summary_200710,
                                  tab2_data = quintile_summary_2024,
                                  tab3_data = top_bottom_qunitiles_200710,
                                  tab4_data = top_bottom_qunitiles_2024,
                                  tab1_name = "1a) A-E stats 2007-10",
                                  tab2_name = "1b) A-E stats 2023",
                                  tab3_name = "2a) 2010 quintiles",
                                  tab4_name = "2b) 2010 quintiles on 2024",
                                  xlsx_path = full_filename_cdrive,
                                  alternative_xlsx_path = full_filename_onedrive,
                                  number_cols_1 = c(4,7),
                                  percent_cols_1 = c(3,6),
                                  number_cols_2 = c(4,7),
                                  percent_cols_2 = c(3,6),
                                  number_cols_3 = c(3,5),
                                  percent_cols_3 = 0,
                                  number_cols_4 = c(3,5),
                                  percent_cols_4 = 0)

}


run_analysis_bustrips_2024_lsoa21 <- function() {

  lsoa21_bustrips_200710 <- make_simplified_bustrips_lsoa21_200710()
  lsoa21_bustrips_2024 <- make_simplified_bustrips_lsoa21_2024()

  lsoa21_bustrips_2024_quintiles <- calculate_bus_service_quintiles_lsoa21(lsoa21_bustrips_2024)
  lsoa21_bustrips_200710_quintiles <- calculate_bus_service_quintiles_lsoa21(lsoa21_bustrips_200710)

  lsoa21_bustrips_2024_long <- make_lsoa_bustrips_long_lsoa21(lsoa21_bustrips_2024)
  lsoa21_bustrips_2024_quintiles_daytime <- add_200710_quintiles_to_2024_data(lsoa_bustrips_2024 = lsoa21_bustrips_2024, lsoa_bustrips_200710_long = lsoa21_bustrips_200710_quintiles, time_period_name = "weekday_daytime")

  # MAKE OUTPUTS
  # top and bottom quintiles with no car ownership stats
  quintile_summary_2024_daytime <- summarise_best_worst_quintiles_lsoa21(lsoa_bus = lsoa21_bustrips_2024_quintiles_daytime, time_period_name = "weekday_daytime")
  quintile_summary_200710_daytime <- summarise_best_worst_quintiles_lsoa21(lsoa_bus = lsoa21_bustrips_200710_quintiles, time_period_name = "weekday_daytime")

  #' Top and bottom 2010 quintile stats - allowing comparison of 2010 and 2024 bus service frequencies
  top_bottom_qunitiles_2024 <- summarise_top_bottom_neighbourhoods_lsoa21(lsoa21_bustrips_2024_quintiles_daytime,
                                                                          time_period_name = "weekday_daytime",
                                                                          qt = quintile_2010,
                                                                          qt_let = quintile_2010_letter)

  top_bottom_qunitiles_200710 <- summarise_top_bottom_neighbourhoods_lsoa21(lsoa21_bustrips_200710_quintiles,
                                                                            time_period_name = "weekday_daytime",
                                                                            qt = quintile,
                                                                            qt_let = quintile_letter)

  # SAVE OUTPUTS
  # outputs are:
  #  - quintile_summary_2024_daytime
  #  - quintile_summary_200710_daytime
  #  - top_bottom_qunitiles_2024
  #  - top_bottom_qunitiles_200710

  save_quintile_outputs_lsoa21(quintile_summary_2024_daytime,
                               quintile_summary_200710_daytime,
                               top_bottom_qunitiles_2024,
                               top_bottom_qunitiles_200710,
                               "bus-services-2010-2024-quintile-comparison.xlsx")

}

run_lsoa21_bus_anlaysis_for_fairness_report <- function() {

  lsoa21_bustrips_200710 <- make_simplified_bustrips_lsoa21_200710()
  lsoa21_bustrips_2024 <- make_simplified_bustrips_lsoa21_2024()

  lsoa21_bustrips_2024_quintiles <- calculate_bus_service_quintiles_lsoa21(lsoa21_bustrips_2024)
  lsoa21_bustrips_200710_quintiles <- calculate_bus_service_quintiles_lsoa21(lsoa21_bustrips_200710)

  lsoa21_bustrips_2024_long <- make_lsoa_bustrips_long_lsoa21(lsoa21_bustrips_2024)
  lsoa21_bustrips_2024_quintiles_daytime <- add_200710_quintiles_to_2024_data(lsoa_bustrips_2024 = lsoa21_bustrips_2024, lsoa_bustrips_200710_long = lsoa21_bustrips_200710_quintiles, time_period_name = "weekday_daytime")

  # TODO: Save these outputs
  quintile_summary_2024_daytime <- summarise_best_worst_quintiles_lsoa21(lsoa_bus = lsoa21_bustrips_2024_daytime, time_period_name = "weekday_daytime")
  quintile_summary_200710_daytime <- summarise_best_worst_quintiles_lsoa21(lsoa_bus = lsoa21_bustrips_200710_quintiles, time_period_name = "weekday_daytime")

  # TODO: Rework this so that outputs are saved in a different function. Maybe with the above outputs together?
  make_and_save_200710_2023_quintile_comparisons_lsoa21(lsoa21_bustrips_2024_quintiles_daytime,
                                                        lsoa21_bustrips_200710_quintiles,
                                                        "bus-services-2010-2024-quintile-comparison.xlsx")

}

#  end of section ---------------------------------------------------------



# assess 2024 data quintiles ----------------------------------------------









lsoa21_bustrips_0710 <- lsoa21_bustrips_0710 %>%
  group_by(lsoa21,
           year,
           rurality,
           urban_rural_cat) %>%
  summarise(tph_weekday_daytime = round(mean(tph_weekday_daytime, na.rm = TRUE), 2),
            tph_weekday_Evening = round(mean(tph_weekday_Evening, na.rm = TRUE), 2),
            tph_Sat_daytime = round(mean(tph_Sat_daytime, na.rm = TRUE), 2),
            tph_Sat_Evening = round(mean(tph_Sat_Evening, na.rm = TRUE), 2),
            tph_Sun_daytime = round(mean(tph_Sun_daytime, na.rm = TRUE), 2),
            tph_Sun_Evening = round(mean(tph_Sun_Evening, na.rm = TRUE), 2)) %>%
  ungroup()

lsoa21_bustrips_0710$rurality <- factor(lsoa21_bustrips_0710$rurality,
                                        levels = c("Urban: Conurbation",
                                                   "Urban: City and Town",
                                                   "Rural: Town and Fringe",
                                                   "Rural: Village/Hamlets/Isolated Dwellings"))

# Identify the 2007-10 quintile boundaries
# 1. need to make a long table of

lsoa_bustrips_20070_10_quintiles <- lsoa_bustrips_20070_10_long %>%
  filter(!is.na(rurality)) %>%
  group_by(rurality,
           time_period) %>%
  summarise(tph_20th_ptile = round(quantile(tph, probs = 0.2, na.rm = TRUE), 1),
            tph_40th_ptile = round(quantile(tph, probs = 0.4, na.rm = TRUE), 1),
            tph_60th_ptile = round(quantile(tph, probs = 0.6, na.rm = TRUE), 1),
            tph_80th_ptile = round(quantile(tph, probs = 0.8, na.rm = TRUE), 1)) %>%
  ungroup()
