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


lsoa_bustrips_2023 <- load_lsoa_bustrips(onspd, year_list = 2023)

lsoa_bustrips_2023 <- lsoa_bustrips_2023 %>%
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



lsoa_bustrips_200710 <- load_lsoa_bustrips(onspd, year_list = c(2007, 2008, 2009, 2010))

lsoa_bustrips_200710 <- lsoa_bustrips_200710 %>%
  mutate(tph_weekday_daytime = ((tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3)) / 12,
         tph_weekday_Evening,
         tph_Sat_daytime = ((tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3)) / 12,
         tph_Sat_Evening,
         tph_Sun_daytime = ((tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3)) / 12,
         tph_Sun_Evening)

lsoa_bustrips_200710 <- lsoa_bustrips_200710 %>%
  group_by(lsoa11,
           year = "2007-10",
           rurality,
           urban_rural_cat) %>%
  summarise(tph_weekday_daytime = mean(tph_weekday_daytime, na.rm = TRUE),
            tph_weekday_Evening = mean(tph_weekday_Evening, na.rm = TRUE),
            tph_Sat_daytime = mean(tph_Sat_daytime, na.rm = TRUE),
            tph_Sat_Evening = mean(tph_Sat_Evening, na.rm = TRUE),
            tph_Sun_daytime = mean(tph_Sun_daytime, na.rm = TRUE),
            tph_Sun_Evening = mean(tph_Sun_Evening, na.rm = TRUE)) %>%
  ungroup()

lsoa_bustrips_200710 <- lsoa_bustrips_200710 %>%
  group_by(rurality) %>%
  mutate(across(where(is.numeric), \(x) ntile(desc(x), n = 5), .names = "{col}_quintile")) %>%
  ungroup()

table(lsoa_bustrips_200710$rurality,
      lsoa_bustrips_200710$tph_weekday_daytime_quintile)

# find quintile bands
lsoa_bustrips_200710_long <- lsoa_bustrips_200710 %>%
  gather(key = time_period,
         value = tph,
         -lsoa11,
         -year,
         -rurality,
         -urban_rural_cat) %>%
  mutate(quintile = ifelse(grepl("_quintile", time_period), TRUE, FALSE))

quintile_data <- lsoa_bustrips_200710_long %>%
  filter(quintile) %>%
  transmute(lsoa11,
            time_period = gsub("_quintile", "", time_period),
            quintile = tph)

lsoa_bustrips_200710_long <- lsoa_bustrips_200710_long %>%
  filter(!quintile) %>%
  select(-quintile)

lsoa_bustrips_200710_long <- left_join(lsoa_bustrips_200710_long, quintile_data, by = c("lsoa11", "time_period"))
lsoa_bustrips_200710_long$quintile_letter = LETTERS[lsoa_bustrips_200710_long$quintile]

quintile_bands_lookup_200710 <- lsoa_bustrips_200710_long %>%
  group_by(rurality,
           time_period,
           quintile,
           quintile_letter) %>%
  summarise(tph_min = min(tph),
            tph_max = max(tph)) %>%
  ungroup() %>%
  filter(!is.na(tph_min))

# apply quintile bands to 2023 data
lsoa_bustrips_2023_long <- lsoa_bustrips_2023 %>%
  gather(key = time_period,
         value = tph,
         -lsoa11,
         -year,
         -rurality,
         -urban_rural_cat)

lsoa_bustrips_2023_weekday <- lsoa_bustrips_2023_long %>%
  filter(time_period == "tph_weekday_daytime") %>%
  group_by(rurality) %>%
  mutate(quintile = ntile(desc(tph), n = 5)) %>%
  ungroup()

lsoa_bustrips_2023_weekday$quintile_letter = LETTERS[lsoa_bustrips_2023_weekday$quintile]

lsoa_bustrips_2023_weekday_summary <- lsoa_bustrips_2023_weekday %>%
  filter(!is.na(tph)) %>%
  group_by(rurality,
           quintile,
           quintile_letter) %>%
  summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
            tph_mean = round(mean(tph, na.rm = TRUE), 1),
            tph_max = round(max(tph, na.rm = TRUE), 1)) %>%
  ungroup()

quintile_bands_200710_weekday <- quintile_bands_lookup_200710 %>%
  filter(time_period == "tph_weekday_daytime") %>%
  transmute(rurality,
            time_period,
            quintile,
            quintile_letter,
            tph_min = round(tph_min, 1),
            tph_max = round(tph_max, 2))

quintile_bands_200710_weekday

lsoa_bustrips_2023_weekday <- lsoa_bustrips_2023_weekday %>%
  mutate(quintile_2010 = case_when(rurality == "Urban: Conurbation" & tph >= 91.5 ~ 1,
                                   rurality == "Urban: Conurbation" & between(tph, 54.7, 91.5) ~ 2,
                                   rurality == "Urban: Conurbation" & between(tph, 35.2, 54.7) ~ 3,
                                   rurality == "Urban: Conurbation" & between(tph, 20.8, 35.2) ~ 4,
                                   rurality == "Urban: Conurbation" & tph < 20.8 ~ 5,
                                   rurality == "Urban: City and Town" & tph >= 45.8 ~ 1,
                                   rurality == "Urban: City and Town" & between(tph, 27.6, 45.8) ~ 2,
                                   rurality == "Urban: City and Town" & between(tph, 17.2, 27.6) ~ 3,
                                   rurality == "Urban: City and Town" & between(tph, 10, 17.2) ~ 4,
                                   rurality == "Urban: City and Town" & tph < 10 ~ 5,
                                   rurality == "Rural: Town and Fringe" & tph >= 16.1 ~ 1,
                                   rurality == "Rural: Town and Fringe" & between(tph, 10, 16.1) ~ 2,
                                   rurality == "Rural: Town and Fringe" & between(tph, 6.7, 10) ~ 3,
                                   rurality == "Rural: Town and Fringe" & between(tph, 4.1, 6.7) ~ 4,
                                   rurality == "Rural: Town and Fringe" & tph < 4.1 ~ 5,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph >= 13.5 ~ 1,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 7.7, 13.5) ~ 2,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 4.7, 7.7) ~ 3,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 2.7, 4.7) ~ 4,
                                   rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph < 2.7 ~ 5))


lsoa_bustrips_2023_weekday$quintile_2010_letter = LETTERS[lsoa_bustrips_2023_weekday$quintile_2010]

table(lsoa_bustrips_2023_weekday$quintile,
      lsoa_bustrips_2023_weekday$quintile_letter)

table(lsoa_bustrips_2023_weekday$quintile_2010,
      lsoa_bustrips_2023_weekday$quintile_2010_letter)

table(lsoa_bustrips_2023_weekday$rurality,
      lsoa_bustrips_2023_weekday$quintile_2010_letter)



table(lsoa_bustrips_2023_weekday$rurality,
      lsoa_bustrips_2023_weekday$quintile_2010_letter)

#' finalise data for reports
#' We need the following:
#'  - best areas in 2023 by rurality, including TPH, and car ownership stats
#'  - worst areas in 2023 by rurality, including TPH, and car ownership stats
#'
#'  - best and worst areas in 2010, TPH
#'  - best and worst areas in 2023, TPH

lsoa_bus_200710_weekday <- quintile_bands_20070_10 %>%
  filter(time_period == "tph_weekday_daytime")

lsoa_bus_200710_weekday$rurality <- factor(lsoa_bus_200710_weekday$rurality,
                                                   levels = c("Urban: Conurbation",
                                                              "Urban: City and Town",
                                                              "Rural: Town and Fringe",
                                                              "Rural: Village/Hamlets/Isolated Dwellings"))


# add car ownership
lsoa_bus_200710_weekday <- add.noncar.owners.to.lsoa.dataset(lsoa_bus_200710_weekday, rename_lsoa_code = TRUE)

summarise_best_worst_quintiles <- function(lsoa_bus) {

  lsoa_bus_summary <- lsoa_bus %>%
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

lsoa_bus_200710_weekday_summary <- summarise_best_worst_quintiles(lsoa_bus_200710_weekday)


# 2023: Best and Worst LSOAS and car ownership
make_2023_bustrip_summary <- function(lsoa_bustrips_2023,
                                      filename = "bus-services-2023-best-worst-cars-jan25.xlsx") {

  lsoa_bustrips_2023_long <- lsoa_bustrips_2023 %>%
    gather(key = time_period,
           value = tph,
           -lsoa11,
           -year,
           -rurality,
           -urban_rural_cat)

  lsoa_bustrips_2023_weekday <- lsoa_bustrips_2023_long %>%
    filter(time_period == "tph_weekday_daytime") %>%
    group_by(rurality) %>%
    mutate(quintile = ntile(desc(tph), n = 5)) %>%
    ungroup()

  lsoa_bustrips_2023_weekday$quintile_letter = LETTERS[lsoa_bustrips_2023_weekday$quintile]

  # make rurality factor so it is ordered by most urban to most rural
  lsoa_bustrips_2023_weekday$rurality <- factor(lsoa_bustrips_2023_weekday$rurality,
                                                levels = c("Urban: Conurbation",
                                                           "Urban: City and Town",
                                                           "Rural: Town and Fringe",
                                                           "Rural: Village/Hamlets/Isolated Dwellings"))

  # add car ownership
  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")
  lsoa_bustrips_2023_weekday <- add.noncar.owners.to.lsoa.dataset(lsoa_bustrips_2023_weekday, rename_lsoa_code = TRUE)
  lsoa_bus_2023_weekday_summary <- summarise_best_worst_quintiles(lsoa_bustrips_2023_weekday)

  source("../environmental-data-for-change/scripts/save-foe-workbook.R")

  full_filename_cdrive <- paste0("outputs/", filename)
  full_filename_onedrive <- paste0("../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/November 2024/", filename)

  message(paste0("Saving: ", full_filename_cdrive))
  message(paste0("Saving: ", full_filename_onedrive))

  save_as_spreadsheet_multiformat(number_of_tabs = 1,
                                  tab1_data = lsoa_bus_2023_weekday_summary,
                                  tab1_name = "2023",
                                  xlsx_path = full_filename_cdrive,
                                  alternative_xlsx_path = full_filename_onedrive,
                                  number_cols_1 = c(4,7),
                                  percent_cols_1 = c(3,6),
                                  number_decimal = FALSE,
                                  percent_decimal = FALSE)

}




## Use best quintile bands for 2010-2023 comparison
make_2010_2023_daytime_comparison <- function(lsoa_bustrips_200710_long,
                                              lsoa_bustrips_2023_long,
                                              filename = "bus-service-comparisons-jan25.xlsx") {

  lsoa_bustrips_200710_weekday_summary <- make_2010_daytime_comparison(lsoa_bustrips_200710_long)
  lsoa_bustrips_2023_weekday_summary <- make_2023_daytime_comparison(lsoa_bustrips_2023_long)

  source("../environmental-data-for-change/scripts/save-foe-workbook.R")

  full_filename_cdrive <- paste0("outputs/", filename)
  full_filename_onedrive <- paste0("../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/", filename)

  message(paste0("Saving: ", full_filename_cdrive))
  message(paste0("Saving: ", full_filename_onedrive))

  save_as_spreadsheet_multiformat(number_of_tabs = 2,
                                  tab1_data = lsoa_bustrips_200710_weekday_summary,
                                  tab1_name = "2007-10",
                                  tab2_data = lsoa_bustrips_2023_weekday_summary,
                                  tab2_name = "2023",
                                  xlsx_path = full_filename_cdrive,
                                  alternative_xlsx_path = full_filename_onedrive,
                                  number_cols_1 = c(3,5),
                                  percent_cols_1 = 0,
                                  number_cols_2 = c(3,5),
                                  percent_cols_2 = 0)

  }

make_2010_daytime_comparison <- function(lsoa_bustrips_200710_long) {

  lsoa_bustrips_200710_weekday <- lsoa_bustrips_200710_long %>%
    filter(time_period == "tph_weekday_daytime")

  lsoa_bustrips_200710_weekday$rurality <- factor(lsoa_bustrips_200710_weekday$rurality,
                                                  levels = c("Urban: Conurbation",
                                                             "Urban: City and Town",
                                                             "Rural: Town and Fringe",
                                                             "Rural: Village/Hamlets/Isolated Dwellings"))

  lsoa_bustrips_200710_weekday_summary <- lsoa_bustrips_200710_weekday %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             quintile,
             quintile_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_200710_weekday_summary <- lsoa_bustrips_200710_weekday_summary %>%
    filter(quintile_letter %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(quintile_letter == "A" ~ "Best 20%",
                                      quintile_letter == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_200710_weekday_summary <- lsoa_bustrips_200710_weekday_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)

  lsoa_bustrips_200710_weekday_summary <- lsoa_bustrips_200710_weekday_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)


  # 2007-10 totals
  # make totals
  lsoa_bustrips_200710_weekday_totals <- lsoa_bustrips_200710_weekday %>%
    filter(!is.na(tph)) %>%
    group_by(quintile,
             quintile_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_200710_weekday_totals <- lsoa_bustrips_200710_weekday_totals %>%
    filter(quintile_letter %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(quintile_letter == "A" ~ "Best 20%",
                                      quintile_letter == "E" ~ "Worst 20%")) %>%
    select(neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_200710_weekday_totals <- lsoa_bustrips_200710_weekday_totals %>%
    gather(key = indicator,
           value = val,
           -neighbourhoods)

  lsoa_bustrips_200710_weekday_totals <- lsoa_bustrips_200710_weekday_totals %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val) %>%
    mutate(rurality = "All neighbourhoods") %>%
    select(rurality,
           everything())

  # bring rurality and totals together in final table
  lsoa_bustrips_200710_weekday_summary <- bind_rows(lsoa_bustrips_200710_weekday_summary,
                                                    lsoa_bustrips_200710_weekday_totals)
}




# 2023
make_2023_daytime_comparison <- function(lsoa_bustrips_2023_long) {

  lsoa_bustrips_2023_weekday <- lsoa_bustrips_2023_long %>%
    filter(time_period == "tph_weekday_daytime")

  lsoa_bustrips_2023_weekday$rurality <- factor(lsoa_bustrips_2023_weekday$rurality,
                                                levels = c("Urban: Conurbation",
                                                           "Urban: City and Town",
                                                           "Rural: Town and Fringe",
                                                           "Rural: Village/Hamlets/Isolated Dwellings"))

  lsoa_bustrips_2023_weekday <- lsoa_bustrips_2023_weekday %>%
    mutate(quintile_2010 = case_when(rurality == "Urban: Conurbation" & tph >= 91.5 ~ 1,
                                     rurality == "Urban: Conurbation" & between(tph, 54.7, 91.5) ~ 2,
                                     rurality == "Urban: Conurbation" & between(tph, 35.2, 54.7) ~ 3,
                                     rurality == "Urban: Conurbation" & between(tph, 20.8, 35.2) ~ 4,
                                     rurality == "Urban: Conurbation" & tph < 20.8 ~ 5,
                                     rurality == "Urban: City and Town" & tph >= 45.8 ~ 1,
                                     rurality == "Urban: City and Town" & between(tph, 27.6, 45.8) ~ 2,
                                     rurality == "Urban: City and Town" & between(tph, 17.2, 27.6) ~ 3,
                                     rurality == "Urban: City and Town" & between(tph, 10, 17.2) ~ 4,
                                     rurality == "Urban: City and Town" & tph < 10 ~ 5,
                                     rurality == "Rural: Town and Fringe" & tph >= 16.1 ~ 1,
                                     rurality == "Rural: Town and Fringe" & between(tph, 10, 16.1) ~ 2,
                                     rurality == "Rural: Town and Fringe" & between(tph, 6.7, 10) ~ 3,
                                     rurality == "Rural: Town and Fringe" & between(tph, 4.1, 6.7) ~ 4,
                                     rurality == "Rural: Town and Fringe" & tph < 4.1 ~ 5,
                                     rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph >= 13.5 ~ 1,
                                     rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 7.7, 13.5) ~ 2,
                                     rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 4.7, 7.7) ~ 3,
                                     rurality == "Rural: Village/Hamlets/Isolated Dwellings" & between(tph, 2.7, 4.7) ~ 4,
                                     rurality == "Rural: Village/Hamlets/Isolated Dwellings" & tph < 2.7 ~ 5))


  lsoa_bustrips_2023_weekday$quintile_2010_letter = LETTERS[lsoa_bustrips_2023_weekday$quintile_2010]

  lsoa_bustrips_2023_weekday_summary <- lsoa_bustrips_2023_weekday %>%
    filter(!is.na(tph)) %>%
    group_by(rurality,
             quintile_2010,
             quintile_2010_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_2023_weekday_summary <- lsoa_bustrips_2023_weekday_summary %>%
    filter(quintile_2010_letter %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(quintile_2010_letter == "A" ~ "Best 20%",
                                      quintile_2010_letter == "E" ~ "Worst 20%")) %>%
    select(rurality,
           neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_2023_weekday_summary <- lsoa_bustrips_2023_weekday_summary %>%
    gather(key = indicator,
           value = val,
           -rurality,
           -neighbourhoods)

  lsoa_bustrips_2023_weekday_summary <- lsoa_bustrips_2023_weekday_summary %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val)

  # make totals
  lsoa_bustrips_2023_weekday_totals <- lsoa_bustrips_2023_weekday %>%
    filter(!is.na(tph)) %>%
    group_by(quintile_2010,
             quintile_2010_letter) %>%
    summarise(tph_min = round(min(tph, na.rm = TRUE), 1),
              tph_mean = round(mean(tph, na.rm = TRUE), 1),
              tph_max = round(max(tph, na.rm = TRUE), 1),
              neighbourhood_n = n()) %>%
    ungroup()

  lsoa_bustrips_2023_weekday_totals <- lsoa_bustrips_2023_weekday_totals %>%
    filter(quintile_2010_letter %in% c("A", "E")) %>%
    mutate(`Buses per hour` = paste0(round(tph_mean), " (", round(tph_min), "-", round(tph_max), ")")) %>%
    mutate(neighbourhoods = case_when(quintile_2010_letter == "A" ~ "Best 20%",
                                      quintile_2010_letter == "E" ~ "Worst 20%")) %>%
    select(neighbourhoods,
           `Buses per hour: average (min-max)` = `Buses per hour`,
           `Number of neighbourhoods` = neighbourhood_n)

  lsoa_bustrips_2023_weekday_totals <- lsoa_bustrips_2023_weekday_totals %>%
    gather(key = indicator,
           value = val,
           -neighbourhoods)

  lsoa_bustrips_2023_weekday_totals <- lsoa_bustrips_2023_weekday_totals %>%
    unite(full_indicator, neighbourhoods, indicator, sep = ": ") %>%
    spread(key = full_indicator,
           value = val) %>%
    mutate(rurality = "All neighbourhoods") %>%
    select(rurality,
           everything())

  # bring rurality and totals together in final table
  lsoa_bustrips_2023_weekday_summary <- bind_rows(lsoa_bustrips_2023_weekday_summary,
                                                  lsoa_bustrips_2023_weekday_totals)

  }


