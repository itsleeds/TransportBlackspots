# clear environment
rm(list = ls())

# load packages
load_packages <- function() {
  library(UK2GTFS)
  library(tidyverse)
  library(lubridate)
  library(tmap)
  library(future.apply)
  library(sf)
  library(tictoc)
  library(pracma)
  library(openxlsx)
  library(readxl)
}
load_packages()

# load data
lsoa_trips_2004_2022 <- readRDS("data/trips_per_lsoa_2004_2022.Rds")

# check column names
# names(lsoa_trips_2004_2022)

#' plan:
#' identify consistent years in data
#' make a selection on what areas to include for what years

#' add metropolitan areas

#' Make LA to Combined Authority lookup
join_metro_by_lup <- function(trips_data) {

  # read in data set
  la_to_metro_lup <- read.xlsx("data/lsoa/LAD21_CAUTH21_EN_LU.xlsx")

  la_to_metro_lup <- la_to_metro_lup %>%
    rename(oslaua = LAD21CD,
           local_authority_name = LAD21NM,
           metro_area_code = CAUTH21CD,
           metro_area_name = CAUTH21NM)

  trips_data <- left_join(trips_data, la_to_metro_lup, by = c("LAD17NM" = "local_authority_name"))

  trips_data <- trips_data %>%
    mutate(metro_area_name = ifelse(RGN11NM == "London", "Greater London", metro_area_name))

}

summarise_trips_by_geog <- function(trips_data,
                                    runs_field = runs_weekday_Morning_Peak,
                                    post_pandemic = "clean",
                                    geog_field) {

  trips_2004_2022 <- trips_data %>%
    group_by({{ geog_field }},
             year) %>%
    summarise(runs = sum({{ runs_field }}, na.rm = TRUE)) %>%
    group_by({{ geog_field }}) %>%
    mutate(runs_pct_max = runs / max(runs),
           runs_zscore = (runs - mean(runs)) / sd(runs)) %>%
    ungroup()

  trips_2004_2022_wide <- trips_2004_2022 %>%
    gather(key = indicator,
           value = val,
           -{{ geog_field }},
           -year) %>%
    unite(indicator_year, indicator, year, sep = "_") %>%
    spread(key = indicator_year,
           value = val,
           fill = 0)

  trips_2004_2022_status <- trips_2004_2022 %>%
    mutate(status = case_when(year == 2020 ~ "Pandemic",
                              runs_zscore <= -1 ~ "Missing data", # more than one standard deviation below zscore
                              runs_pct_max <= 0.25 ~ "Missing data", # below 25% of peak
                              runs_pct_max <= 0.4 & runs_zscore <= -0.7 ~ "Missing data", # capture some anomalies
                              year < 2008 & runs_pct_max < 0.6 ~ "Missing data",
                              between(runs_pct_max, 0.25, 0.5) ~ "Possibly missing data",
                              TRUE ~ ""))

  # do not clean post pandemic data and assume is complete, if post_pandemic == "keep"
  if(post_pandemic == "keep") {
    trips_2004_2022_status <- trips_2004_2022_status %>%
      mutate(status = ifelse(year > 2020, "", status))
  }

  return(trips_2004_2022_status)

}

# remove missing and interpolate
clean_interp <- function(trips_data,
                         geog_var,
                         missing_type = "Missing data") {



  # set missing year data to NAs
  if(missing_type == "Possibly missing data") {
    trips_data_cleaned <- trips_data %>%
      mutate(runs_pct_max = ifelse(grepl("missing data", status, ignore.case = TRUE), NA, runs_pct_max)) %>%
      filter(!is.na({{ geog_var }}))
  } else if(missing_type == "Missing data") {
    trips_data_cleaned <- trips_data %>%
      mutate(runs_pct_max = ifelse(status == "Missing data", NA, runs_pct_max)) %>%
      filter(!is.na({{ geog_var }}))
  }

  # add 2012 and 2013 as missing years for all
  other_missing_years <- trips_data_cleaned %>%
    filter(!is.na({{ geog_var }})) %>%
    distinct({{ geog_var }})
  other_missing_years <- expand_grid(other_missing_years, year = c(2012, 2013))
  other_missing_years <- other_missing_years %>%
    mutate(status = "No data")

  # join no data years to main cleaned data set
  trips_data_cleaned <- bind_rows(trips_data_cleaned,
                                  other_missing_years)


  # interpolate for missing data by region and year
  trips_data_cleaned <- trips_data_cleaned %>%
    group_by({{ geog_var }}) %>%
    mutate(runs_pct_max_int = interp1(x = year, y = runs_pct_max, xi = year)) %>%
    arrange({{ geog_var }}, year)
}

# make a summary showing the data status for each area by year
make_wide_data_check_table <- function(clean_trips_data, geog_field) {

  clean_trips_data <- clean_trips_data %>%
    select({{ geog_field }},
           year,
           status) %>%
    spread(key = year,
           value = status)

}


plot_geog_grid <- function(trips_clean,
                           geog_var,
                           ncols,
                           ylab_name = "Percent of max (Weekday am peak)") {

  trips_clean <- trips_clean %>%
    mutate(region := {{ geog_var }})

  ggplot(data = trips_clean, aes(x = year, col = region)) +
    geom_line(aes(y = runs_pct_max), linewidth = 1.5, show.legend = FALSE) +
    geom_line(aes(y = runs_pct_max_int), linetype = "dotted", linewidth = 1.5, show.legend = FALSE) +
    xlab("Year") +
    ylab(ylab_name) +
    theme_bw() +
    facet_wrap(. ~ region, ncol = ncols)
}


#  REGION -----------------------------------------------------------------
region_trips_analysis <- function(lsoa_trips_2004_2022) {

  trips_by_region_status <- summarise_trips_by_geog(trips_data = lsoa_trips_2004_2022,
                                                    geog_field = RGN11NM,
                                                    runs_field = runs_weekday_Morning_Peak,
                                                    post_pandemic = "keep")

  trips_by_region_clean <- clean_interp(trips_by_region_status,
                                        geog_var = RGN11NM)

  region_trips_by_year <- plot_geog_grid(trips_by_region_clean,
                                         geog_var = RGN11NM,
                                         ncols = 3,
                                         ylab_name = "Percent of max (Weekday am peak)")

  #plot(region_trips_by_year)
  ggsave(filename = "plots/region-trips-by-year.png",
         plot = region_trips_by_year,
         device = "png",
         width = 25,
         height = 30,
         units = "cm")

  return(trips_by_region_clean)

}



# METROPOLITAN AREA -------------------------------------------------------

lsoa_trips_2004_2022 <- join_metro_by_lup(lsoa_trips_2004_2022)

trips_by_metro_status <- summarise_trips_by_geog(trips_data = lsoa_trips_2004_2022,
                                                  geog_field = metro_area_name,
                                                  runs_field = runs_weekday_Morning_Peak,
                                                  post_pandemic = "keep")

trips_by_metro_clean <- clean_interp(trips_by_metro_status,
                                      geog_var = metro_area_name)

metro_trips_by_year <- plot_geog_grid(trips_by_metro_clean,
                                       geog_var = metro_area_name,
                                       ncols = 3,
                                       ylab_name = "Percent of max (Weekday am peak)")


lsoa_trips_metro_2004_2022 <- lsoa_trips_2004_2022 %>%
  filter(!is.na(metro_area_name)) %>%
  filter(metro_area_name %in% c("West of England", "Greater Manchester"))

lsoa_trips_metro_2004_2022 <- lsoa_trips_metro_2004_2022 %>%
  mutate(year_band = case_when(between(year, 2006, 2007) ~ "2006-2007",
                               between(year, 2021, 2022) ~ "2021-2022")) %>%
  mutate(year_band = ifelse(is.na(year_band), year, year_band))

#str(lsoa_trips_metro_2004_2022)
trip_cols <- colnames(lsoa_trips_metro_2004_2022)[5:19]

lsoa_trips_max <- lsoa_trips_metro_2004_2022 %>%
  group_by(lsoa11 = zone_id) %>%
  summarise(across(all_of(trip_cols), max, na.rm = TRUE, .names = "{.col}_max")) %>%
  ungroup()

lsoa_trips_trend <- lsoa_trips_metro_2004_2022 %>%
  group_by(lsoa11 = zone_id,
           year_band,
           metro_area_name) %>%
  summarise(across(all_of(trip_cols), mean, na.rm = TRUE)) %>%
  ungroup()

# check NA values for 2006/07
# check the ratio of 2006/07 to max value (for weekday morning?)
lsoa_trips_trend <- lsoa_trips_trend %>%
  filter(year_band %in% c("2005",
                          "2006-2007",
                          "2008",
                          "2021-2022")) %>%
  select(lsoa11,
         year_band,
         runs_weekday_Morning_Peak) %>%
  spread(key = year_band,
         value = runs_weekday_Morning_Peak)

# add max value
lsoa_trips_max <- lsoa_trips_max %>%
  select(lsoa11,
         runs_weekday_Morning_Peak_max)

# join and check
lsoa_trips_trend <- lsoa_trips_trend %>%
  mutate(change_2006_2022 = `2021-2022` - `2006-2007`)

lsoa_boundaries <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/",
                           quiet = TRUE)
lsoa_boundaries <- lsoa_boundaries %>%
  select(OBJECTID,
         lsoa11 = LSOA11CD,
         lsoa_name = LSOA11NM,
         geometry)

lsoa_trips_trend <- inner_join(lsoa_boundaries, lsoa_trips_trend, by = "lsoa11")

tmap_mode("view")
qtm(lsoa_trips_trend,
    fill = "change_2006_2022",
    borders = NULL)

# LA analysis -------------------------------------------------------------

la_trips_analysis <- function() {

  trips_by_la_status <- summarise_trips_by_geog(lsoa_trips_2004_2022,
                                                LAD17NM)

  trips_by_la_clean <- clean_interp(trips_by_la_status,
                                    geog_var = LAD17NM)

  la_trips_by_year <- plot_geog_grid(trips_by_la_clean,
                                     geog_var = LAD17NM,
                                     ncols = 5)

  ggsave(filename = "plots/la-trips-by-year.png",
         plot = la_trips_by_year,
         device = "png",
         width = 25,
         height = 250,
         units = "cm",
         limitsize = FALSE)

  return(trips_by_la_clean)

}


#  make wide table for mapping --------------------------------------------
make_wide_lsoa_for_mapping <- function(lsoa_trips_2004_2022) {

  lsoa_trips_2004_2022_wide <- lsoa_trips_2004_2022 %>%
    gather(key = indicator,
           value = val,
           -zone_id,
           -LAD17NM,
           -RGN11NM,
           -year)

  lsoa_trips_2004_2022_wide <- lsoa_trips_2004_2022_wide %>%
    unite(full_indicator, indicator, year, sep = "_") %>%
    spread(key = full_indicator,
           value = val)

  View(head(lsoa_trips_2004_2022_wide, 100))
  names(lsoa_trips_2004_2022_wide)

}

