# set up

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

# quiet dplyr
options(dplyr.summarise.inform = FALSE)

# set tmap mode to interactive view
tmap_mode("view")

# load scripts to create functions
source("scripts/gtfs-functions.R")
source("scripts/lsoa-analysis.R")
source("R/stops_per_week_functions.R")

# get gtfs data
gtfs_wales = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_wales_gtfs.zip")
#gtfs_london = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_london_gtfs.zip")
#gtfs_southwest = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_south_west_gtfs.zip")
#gtfs_midlands = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_west_midlands_gtfs.zip")
gtfs_england = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_england_gtfs.zip")

# set dates
startdate = lubridate::ymd("2023-03-01")
enddate = lubridate::ymd("2023-03-31")

# run functions
wales_stops_lsoa <- make_lsoa_stop_summary(gtfs_wales,
                                           startdate,
                                           enddate)

england_stops_lsoa <- make_lsoa_stop_summary(gtfs_england,
                                             startdate,
                                             enddate)

# output results
st_write(dsn = "../gis-data/transport/busstops.gpkg",
         obj = england_stops_lsoa,
         layer = "england-busstops-lsoa",
         delete_dsn = FALSE,
         delete_layer = TRUE)

# provisional workings and tests ------------------------------------------

tm_shape(stops_runs) +
  tm_dots(col = "rushhour_runs_per_weekday", style = "quantile")

tm_shape(stops_runs) +
  tm_dots(col = "runs_per_saturday", style = "quantile")


# look at stops timetable and weekly/weekend frequency of service....
sw_stops <- count_stops(gtfs = sw_gtfs,
                        startdate = lubridate::ymd("2023-02-28"),
                        enddate = lubridate::ymd("2023-03-31"))

# summarise all services and stop frequency.
sw_stops_summary <- summarise_stops(sw_stops,
                                    gtfs = sw_gtfs)

sw_stops_summary <- sf::st_as_sf(sw_stops_summary, coords = c("stop_lon","stop_lat"), crs = 4326)

# Options for LSOA assessment:
# LSOA area is less than 500m buffer then do 500m buffer. If bigger then look inside the LSOA area. Binary logic.
#   - see about edge cases.
# 5 min walking isochrome.(Malcolm - 1 day per week).

# Options:
# Run the code for one day of the week. Create a function for each day of the week, run separately
# then aggregate for all days.

# Remember: functional unit in GTFS is a trip. it has stop times.
# What happens if that trip starts before and carries on through rush hour?

# get stop times - left join trip table, by trip id. Show stop times for each trip.
# then left join calendar by schedule id. then trip, with stops and days of the week.
# ideally process this with less than an hour runtime.


tm_shape(sw_stops_summary) +
  tm_dots(col = "stops_per_weekday", style = "quantile")
tm_shape(sw_stops_summary) +
  tm_dots(col = "stops_per_weekend", style = "quantile") +
  tm_shape(lsoa_centroids)


#' TODO:
#' [x] summarise weekend services v week day services
#' [ ] summarise rush hour (or any given period...?) - needs time and day...


#  geometry of routes -----------------------------------------------------

sw_trip_geoms <- make_trip_geoms(sw_gtfs)

test <- sw_trip_geoms %>%
  filter(trip_id %in% c("VJ6cd7fd0b42a34f82532eefa24abd4c10bf159365",
                        "VJa89e5c262d836b8561c5a4c39d8ff80970e3eceb",
                        "VJfbc03f8c31a2a88e78d405048d18811d397e8f6b",
                        "VJ971647ebe2dfe3ccc93cffc856c9a4244274864f",
                        "VJd94f1e23e35b2f7f11fb43455577405a8acb55bd",
                        "VJf6772408e689b664ea9b8b05702456140fac784e"
                        ))
qtm(test)

