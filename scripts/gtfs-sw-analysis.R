# set up

# clear environment
rm(list = ls())

# load packages
library(UK2GTFS)
library(tidyverse)
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


# get gtfs data
tic("GTFS file loaded")
gtfs = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_south_west_gtfs.zip")
toc()

# set dates
startdate = lubridate::ymd("2023-03-01")
enddate = lubridate::ymd("2023-03-31")

# run functions...
# TODO: account for extra and cancelled services
stops_calendar <- stop_timetables(gtfs,
                                  startdate,
                                  enddate)

stops_runs <- summarise_all_stop_data(stops_calendar,
                                      gtfs,
                                      startdate,
                                      enddate)

# change crs system to BNG to align with LSOA boundary CRS
stops_runs <- st_transform(stops_runs, crs = 27700)

# get boundaries (which decides whether to use LSOA boundaries or radius from centroid - whichever is bigger)
lsoa_transit_boundary <- make_lsoa_boundary_file(radius = 500)

# intersect LSOA boundaries
stops_lsoa <- st_intersection(lsoa_transit_boundary, stops_runs)

# TODO: how to summarise stop data by lsoa?
stops_lsoa_summary <- stops_lsoa %>%
  st_drop_geometry() %>%
  group_by(lsoa11cd) %>%
  summarise(stops_weekdays = sum(stops_weekdays, na.rm = TRUE),
            stops_saturday = sum(stops_saturday, na.rm = TRUE),
            stops_sunday = sum(stops_sunday, na.rm = TRUE),
            stops_per_week = sum(stops_per_week, na.rm = TRUE),
            stops_per_weekday = sum(stops_per_weekday, na.rm = TRUE),
            stops_per_saturday = sum(stops_per_saturday, na.rm = TRUE),
            stops_per_sunday = sum(stops_per_sunday, na.rm = TRUE),
            rushhour_stops_weekdays = sum(rushhour_stops_weekdays, na.rm = TRUE),
            rushhour_stops_per_weekday = sum(rushhour_stops_per_weekday, na.rm = TRUE))

# get actual lsoa boundaries
lsoa_boundary <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Full_Clipped_(BFC)_EW_V3.shp",
                         quiet = TRUE)

stops_lsoa_summary <- inner_join(lsoa_boundary, stops_lsoa_summary, by = c("LSOA11CD" = "lsoa11cd"))

hist(stops_lsoa_summary$rushhour_stops_weekdays)

tm_shape(stops_lsoa_summary) +
  tm_fill(col = "rushhour_stops_weekdays", style = "quantile")

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

