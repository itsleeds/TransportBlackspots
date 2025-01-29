# set up ------------------------------------------------------------------

clear_all = TRUE
source("scripts/november-24/set-up.R")
source("../environmental-data-for-change/scripts/save-foe-workbook.R")

onspd <- load_onspd(keep_only_current = FALSE)

# run functions -----------------------------------------------------------

lsoa_bustrips_2023 <- load_lsoa_bustrips(onspd, year_list = 2023)

lsoa_bustrips_2023_totals <- calculate_total_bus_services(lsoa_bustrips_2023)
lsoa_bustrips_2023_ranks <- rank_total_bus_services(lsoa_bustrips_2023_totals)
lsoa_bustrips_2023_scores <- convert_ranks_to_scores_and_deciles(lsoa_bustrips_2023_ranks)

bus_deciles_by_rurality_and_total_buses <- summarise_bus_deciles_by_rurality_and_total_buses(lsoa_bustrips_2023_scores)
bus_deciles_by_region <- summarise_bus_deciles_by_region(lsoa_bustrips_2023_scores)

make_map_of_bus_service(lsoa_bustrips_2023_scores, tph = total_no_buses_score, tph_service = total_no_buses_decile, type = "decile")

save_as_spreadsheet_multiformat(number_of_tabs = 2,
                                tab1_data = bus_deciles_by_rurality_and_total_buses,
                                tab1_name = "1. rurality",
                                tab2_data = bus_deciles_by_region,
                                tab2_name = "2. region",
                                xlsx_path = "outputs/november-24/bus-numbers-deciles-jan25.xlsx",
                                #alternative_xlsx_path = ,
                                number_cols_1 = c(2:5),
                                percent_cols_1 = 0,
                                number_cols_2 = c(2:11),
                                percent_cols_2 = ,
                                colwidths = 20)


#  define functions -------------------------------------------------------


calculate_total_bus_services <- function(lsoa_bus) {

  lsoa_bus_totals <- lsoa_bus %>%
    transmute(lsoa11,
              year,
              rurality,
              urban_rural_cat,
              buses_weekday_daytime = (tph_weekday_Morning_Peak * 4) + (tph_weekday_Midday * 5) + (tph_weekday_Afternoon_Peak * 3),
              buses_weekday_evening = tph_weekday_Evening * 4,
              buses_sat_daytime = (tph_Sat_Morning_Peak * 4) + (tph_Sat_Midday * 5) + (tph_Sat_Afternoon_Peak * 3),
              buses_sat_evening = tph_Sat_Evening * 4,
              buses_sun_daytime = (tph_Sun_Morning_Peak * 4) + (tph_Sun_Midday * 5) + (tph_Sun_Afternoon_Peak * 3),
              buses_sun_evening = tph_Sun_Evening * 4) %>%
    mutate(buses_total = buses_weekday_daytime + buses_weekday_evening + buses_sat_daytime + buses_sat_evening + buses_sun_daytime + buses_sun_evening)

  lsoa_bus_totals$rurality <- factor(lsoa_bus_totals$rurality,
                                     levels = c("Urban: Conurbation",
                                                "Urban: City and Town",
                                                "Rural: Town and Fringe",
                                                "Rural: Village/Hamlets/Isolated Dwellings"))

  return(lsoa_bus_totals)

}

rank_total_bus_services <- function(lsoa_bus_totals) {

  lsoa_bus_totals <- lsoa_bus_totals %>%
    group_by(rurality) %>%
    mutate(buses_weekday_daytime_rank = rank(desc(buses_weekday_daytime)),
           buses_total_rank = rank(desc(buses_total)))

}


convert_ranks_to_scores_and_deciles <- function(lsoa_bus_totals) {

  lsoa_bus_scores <- lsoa_bus_totals %>%
    group_by(rurality) %>%
    mutate(no_of_buses_weekday_daytime_score = buses_weekday_daytime_rank / max(buses_weekday_daytime_rank),
           total_no_buses_score = buses_total_rank / max(buses_total_rank))

  lsoa_bus_scores <- lsoa_bus_scores %>%
    mutate(no_of_buses_weekday_daytime_decile = ntile(no_of_buses_weekday_daytime_score, n = 10),
           total_no_buses_decile = ntile(total_no_buses_score, n = 10))

}

summarise_bus_deciles_by_rurality_and_total_buses <- function(lsoa_bus_scores) {

  lsoa_bus_scores_summary <- lsoa_bus_scores %>%
    group_by(rurality,
             `Total number of weekly buses decile (1 = most buses)` = total_no_buses_decile) %>%
    summarise(average_no_buses_weekly = round(mean(buses_total, na.rm = TRUE))) %>%
    ungroup()

  # make the rurality column headings more informativce of contents
  lsoa_bus_scores_summary <- lsoa_bus_scores_summary %>%
    mutate(rurality_colheading = gsub("[/]Hamlets[/]Isolated Dwellings", "", rurality)) %>%
    mutate(rurality_colheading = gsub(": ", " - ", rurality_colheading)) %>%
    mutate(rurality_colheading = paste0("Total number of buses (weekly):\n", rurality_colheading)) %>%
    select(-rurality)

  # set as factor so most urban comes first
  lsoa_bus_scores_summary$rurality_colheading <- factor(lsoa_bus_scores_summary$rurality_colheading,
                                                        levels = c("Total number of buses (weekly):\nUrban - Conurbation",
                                                                   "Total number of buses (weekly):\nUrban - City and Town",
                                                                   "Total number of buses (weekly):\nRural - Town and Fringe",
                                                                   "Total number of buses (weekly):\nRural - Village"))

  # make into a more user friendly structure
  lsoa_bus_scores_summary <- lsoa_bus_scores_summary %>%
    spread(key = rurality_colheading,
           value = average_no_buses_weekly)

}

summarise_bus_deciles_by_region <- function(lsoa_bus_scores) {

  lsoa_bus_scores <- add_geography(lsoa_bus_scores)

  lsoa_bus_scores_summary <- lsoa_bus_scores %>%
    group_by(region_name,
             `Total number of weekly buses decile (1 = most buses)` = total_no_buses_decile) %>%
    summarise(neighbourhoods = n()) %>%
    ungroup()

  lsoa_bus_scores_summary <- lsoa_bus_scores_summary %>%
    mutate(region_name = paste0(region_name, "\n(number of\nneighbourhoods)"))

  lsoa_bus_scores_summary <- lsoa_bus_scores_summary %>%
    spread(key = region_name,
           value = neighbourhoods)

}

