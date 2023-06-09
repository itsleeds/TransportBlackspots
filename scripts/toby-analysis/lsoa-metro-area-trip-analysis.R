# clear environment
rm(list = ls())

# load functions
source("scripts/check-data/lsoa-trips-analysis-functions.R")

# load packages
load_packages()

# load data
lsoa_trips_2004_2023 <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")
lsoa_bustrips_2004_2023 <- lsoa_trips_2004_2023 %>%
  filter(route_type == 3)
names(lsoa_bustrips_2004_2023)



lsoa_trips_2004_2022 <- join_metro_by_lup(lsoa_trips_2004_2022)

metro_trips_data_summary <- metro_trips_analysis(lsoa_trips_2004_2022)
region_trips_data_summary <- region_trips_analysis(lsoa_trips_2004_2022)

# METROPOLITAN AREA -------------------------------------------------------

metro_areas_to_include <- c("Cambridgeshire and Peterborough",
                            "Greater London",
                            "Greater Manchester",
                            "Liverpool City Region",
                            "North East",
                            "North of Tyne",
                            "Sheffield City Region",
                            "Tees Valley",
                            "West Midlands",
                            "West of England",
                            "West Yorkshire")

lsoa_trips_metro_2004_2022 <- lsoa_trips_2004_2022 %>%
  filter(!is.na(metro_area_name)) %>%
  filter(metro_area_name %in% metro_areas_to_include)

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
lsoa_trips_trend <- left_join(lsoa_trips_trend, lsoa_trips_max, by = "lsoa11")

# find how close 2006-2007 is to max. If way off and 2005 or 2008 is closer then use these values.
lsoa_trips_trend <- lsoa_trips_trend %>%
  rename(mean_trips_2005 = `2005`,
         mean_trips_200607 = `2006-2007`,
         mean_trips_2008 = `2008`,
         mean_trips_202122 = `2021-2022`) %>%
  mutate(ratio_2005_max = mean_trips_2005 / runs_weekday_Morning_Peak_max,
         ratio_200607_max = mean_trips_200607 / runs_weekday_Morning_Peak_max,
         ratio_2008_max = mean_trips_2008 / runs_weekday_Morning_Peak_max) %>%
  mutate(mean_trips_200607 = ifelse(ratio_200607_max < 0.75 & ratio_2008_max > 0.75, mean_trips_2008, mean_trips_200607))

# find trends over time period
lsoa_trips_trend <- lsoa_trips_trend %>%
  mutate(change_200607_2022 = mean_trips_202122 - mean_trips_200607) %>%
  mutate(change_200607_2022_pct = change_200607_2022 / mean_trips_200607)

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
    fill = "change_200607_2022_pct",
    borders = NULL)


save_gpkg <- function(gpkg_name = "busstops.gpkg",
                      layer,
                      layer_name) {

  full_gpkg_name <- file.path("../gis-data/transport", gpkg_name)

  # output results
  st_write(dsn = full_gpkg_name,
           obj = layer,
           layer = layer_name,
           delete_dsn = FALSE,
           delete_layer = TRUE)

}

save_gpkg("busstops.gpkg",
          layer = lsoa_trips_trend,
          layer_name = "lsoa-trips-trends-metro-areas")
