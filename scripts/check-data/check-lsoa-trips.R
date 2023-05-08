# clear environment
rm(list = ls())

# load packages
library(UK2GTFS)
library(tidyverse)
library(lubridate)
library(tmap)
library(future.apply)
library(sf)
library(tictoc)
library(pracma)

# load data
lsoa_trips_2004_2022 <- readRDS("data/trips_per_lsoa_2004_2022.Rds")
# check column names
names(lsoa_trips_2004_2022)

#' plan:
#' identify consistent years in data
#' make a selection on what areas to include for what years

summarise_trips_by_geog <- function(trips_data,
                                    geog_field) {

trips_2004_2022 <- trips_data %>%
  group_by({{ geog_field }},
           year) %>%
  summarise(runs = sum(runs_weekday_Morning_Peak)) %>%
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
                            runs_zscore <= -1 ~ "Missing", # more than one standard deviation below zscore
                            runs_pct_max <= 0.25 ~ "Missing", # below 25% of peak
                            runs_pct_max <= 0.4 & runs_zscore <= -0.7 ~ "Missing", # capture some anomalies
                            between(runs_pct_max, 0.25, 0.5) ~ "Possibly missing",
                            year < 2008 & runs_pct_max < 0.6 ~ "Missing",
                            TRUE ~ ""))

#trips_2004_2022_status <- trips_2004_2022_status %>%
#  select({{ geog_field }},
#         year,
#         status) %>%
#  spread(key = year,
#         value = status)

}

trips_by_region_status <- summarise_trips_by_geog(lsoa_trips_2004_2022,
                                                  RGN11NM)

trips_by_la_status <- summarise_trips_by_geog(lsoa_trips_2004_2022,
                                              LAD17NM)

# remove missing and interpolate
clean_interp <- function(trips_data,
                         geog_var,
                         missing_type = "Missing") {

  # set missing year data to NAs
  if(missing_type == "Possibly missing") {
    trips_data_cleaned <- trips_data %>%
      mutate(runs_pct_max = ifelse(grepl("missing", status, ignore.case = TRUE), NA, runs_pct_max)) %>%
      filter(!is.na({{ geog_var }}))
  } else if(missing_type == "Missing") {
    trips_data_cleaned <- trips_data %>%
      mutate(runs_pct_max = ifelse(status == "Missing", NA, runs_pct_max)) %>%
      filter(!is.na({{ geog_var }}))
  }

  # interpolate for missing data by region and year
  trips_data_cleaned <- trips_data_cleaned %>%
    group_by({{ geog_var }}) %>%
    mutate(runs_pct_max_int = interp1(x = year, y = runs_pct_max, xi = year))
}

plot_geog_grid <- function(trips_clean, geog_var, ncols) {

  trips_clean <- trips_clean %>%
    mutate(region := {{ geog_var }})

  ggplot(data = trips_clean, aes(x = year, col = region)) +
    geom_line(aes(y = runs_pct_max), linewidth = 1.5, show.legend = FALSE) +
    geom_line(aes(y = runs_pct_max_int), linetype = "dotted", linewidth = 1.5, show.legend = FALSE) +
    xlab("Year") +
    ylab("Percent of max (Weekday am peak)") +
    theme_bw() +
    facet_wrap(. ~ region, ncol = ncols)
}

trips_by_region_clean <- clean_interp(trips_by_region_status,
                                      geog_var = RGN11NM)

trips_by_la_clean <- clean_interp(trips_by_la_status,
                                      geog_var = LAD17NM)

la_trips_by_year <- plot_geog_grid(trips_by_la_clean,
                                   geog_var = LAD17NM,
                                   ncols = 5)

region_trips_by_year <- plot_geog_grid(trips_by_region_clean,
                                   geog_var = RGN11NM,
                                   ncols = 3)

ggsave(filename = "plots/la-trips-by-year.png",
       plot = la_trips_by_year,
       device = "png",
       width = 25,
       height = 250,
       units = "cm",
       limitsize = FALSE)

ggsave(filename = "plots/region-trips-by-year.png",
       plot = region_trips_by_year,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")
