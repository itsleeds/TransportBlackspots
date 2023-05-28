rm(list = ls())
source("scripts/check-data/lsoa-trips-analysis-functions.R")
load_packages()

trips_mode_lsoa_2004_2023 <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")
#table(trips_mode_lsoa_2004_2023$route_type)
#table(trips_mode_lsoa_2004_2023$RGN11NM, useNA = "ifany")

bus_trips_lsoa_2004_2023 <- trips_mode_lsoa_2004_2023 %>%
  filter(route_type == 3)

la_bus_trips <- bus_trips_lsoa_2004_2023 %>%
  filter(!is.na(LAD17NM)) %>%
  group_by(LAD17NM,
           year) %>%
  summarise(runs = sum(runs_weekday_Morning_Peak, na.rm = TRUE)) %>%
  group_by(LAD17NM) %>%
  mutate(runs_pct_max = runs / max(runs),
         runs_zscore = (runs - mean(runs)) / sd(runs)) %>%
  ungroup()

la_bus_trips <- la_bus_trips %>%
  mutate(status = case_when(year == 2020 ~ "Pandemic",
                            year < 2020 & runs_zscore <= -1 ~ "Incomplete data", # more than one standard deviation below zscore
                            runs_pct_max <= 0.25 ~ "Incomplete data", # below 25% of peak
                            runs_pct_max <= 0.4 & runs_zscore <= -0.7 ~ "Incomplete data", # capture some anomalies
                            year < 2008 & runs_pct_max < 0.6 ~ "Incomplete data",
                            #between(runs_pct_max, 0.25, 0.5) ~ "Possibly incomplete data",
                            TRUE ~ ""))

# remove data for years with incomplete data
la_bus_trips <- la_bus_trips %>%
  mutate(runs = ifelse(status == "Incomplete data", NA, runs))

la_bus_trips <- la_bus_trips %>%
  select(LAD17NM,
         year,
         runs) %>%
  spread(key = year,
         value = runs)

la_bus_trips <- la_bus_trips %>%
  mutate(change_n = `2022` - `2008`) %>%
  mutate(change_pct = change_n / `2008`)

la_boundaries <- st_read("../gis-data/boundaries/local-authority/Local_Authority_Districts_(December_2021)_UK_BFC/",
                         quiet = TRUE)

la_boundaries <- la_boundaries %>%
  select(OBJECTID,
         LAD21CD,
         LAD21NM,
         geometry)

la_bus_trips <- left_join(la_boundaries, la_bus_trips, by = c("LAD21NM" = "LAD17NM"))

tmap_mode("view")
qtm(la_bus_trips,
    fill = "change_pct",
    border = NULL)

# clean
trips_mode_lsoa_2004_2023 <- trips_mode_lsoa_2004_2023 %>%
  filter(!is.na(RGN11NM)) %>%
  filter(route_type != 1100)

mode_by_year_by_region <- trips_mode_lsoa_2004_2023 %>%
  group_by(RGN11NM,
           route_type,
           year) %>%
  summarise(runs = sum(runs_weekday_Morning_Peak, na.rm = TRUE)) %>%
  group_by(RGN11NM,
           route_type) %>%
  mutate(runs_pct_max = runs / max(runs),
         runs_zscore = (runs - mean(runs)) / sd(runs)) %>%
  ungroup() %>%
  mutate(mode = case_when(route_type == 0 ~ "Tram",
                          route_type == 1 ~ "Metro",
                          route_type == 2 ~ "Rail",
                          route_type == 3 ~ "Bus",
                          route_type == 4 ~ "Ferry")) %>%
  mutate(mode = as.factor(mode)) # for graph


mode_by_year_by_region <- mode_by_year_by_region %>%
  mutate(status = case_when(year == 2020 ~ "Pandemic",
                            year < 2020 & runs_zscore <= -1 ~ "Incomplete data", # more than one standard deviation below zscore
                            runs_pct_max <= 0.25 ~ "Incomplete data", # below 25% of peak
                            runs_pct_max <= 0.4 & runs_zscore <= -0.7 ~ "Incomplete data", # capture some anomalies
                            year < 2008 & runs_pct_max < 0.6 ~ "Incomplete data",
                            #between(runs_pct_max, 0.25, 0.5) ~ "Possibly incomplete data",
                            RGN11NM == "North East" & route_type == 3 & year == 2008 ~ "Incomplete data",
                            TRUE ~ ""))

# remove data for years with incomplete data
mode_by_year_by_region <- mode_by_year_by_region %>%
  mutate(runs = ifelse(status == "Incomplete data", NA, runs))

other_missing_years <- mode_by_year_by_region %>%
  distinct(RGN11NM)
other_missing_years <- expand_grid(other_missing_years, year = c(2012, 2013), route_type = c(0,1,2,3,4))
other_missing_years <- other_missing_years %>%
  mutate(status = "No data") %>%
  mutate(mode = case_when(route_type == 0 ~ "Tram",
                          route_type == 1 ~ "Metro",
                          route_type == 2 ~ "Rail",
                          route_type == 3 ~ "Bus",
                          route_type == 4 ~ "Ferry")) %>%
  mutate(mode = as.factor(mode))

# join no data years to main cleaned data set
mode_by_year_by_region <- bind_rows(mode_by_year_by_region,
                                    other_missing_years)

# interpolate for missing data by region and year
mode_by_year_by_region <- mode_by_year_by_region %>%
  group_by(RGN11NM,
           route_type) %>%
  mutate(runs_int = ifelse(route_type %in% c(2,3),
                           interp1(x = year, y = runs, xi = year),
                           runs)) %>%
  arrange(RGN11NM,
          route_type,
          year)

# clean incomplete data for graphs

plot_mode_by_year_by_region <- ggplot(data = mode_by_year_by_region, aes(x = year, col = mode)) +
  geom_line(aes(y = runs), linewidth = 1) +
  geom_line(aes(y = runs_int), linetype = "dotted", linewidth = 1) +
  facet_wrap(RGN11NM ~ ., scales = "free_y", ncol = 3) +
  theme_bw()

plot(plot_mode_by_year_by_region)

ggsave(filename = "plots/region-trips-by-year-and-mode.png",
       plot = plot_mode_by_year_by_region,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")

mode_by_year_by_region_wide <- mode_by_year_by_region %>%
  select(RGN11NM,
         year,
         route_type,
         runs_pct_max) %>%
  spread(key = year,
         value = runs_pct_max)

write.csv(mode_by_year_by_region_wide,
          "plots/toby-initial-lsoa-tests/mode-by-region-by-year.csv",
          row.names = FALSE,
          na="")


