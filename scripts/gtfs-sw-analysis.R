# set up

rm(list = ls()) # clear everything

library(UK2GTFS)
library(dplyr)
library(tmap)
library(future.apply)
library(lubridate)

tmap_mode("view")

#understanding unfamiliar operators
4 %*% 4   # multiply a by b
18 %/% 4  # integer division: find how many times a goes into b, wholly.
18 %% 4   # find the remainder after dividing a by b

# load scripts to create functions
source("scripts/gtfs-functions.R")

# get gtfs data
sw_gtfs = gtfs_read("C:/Users/toby.bridgeman/foe-work-on-cdrive/environmental-data-for-change/data/transport/itm_south_west_gtfs.zip")
gtfs <- sw_gtfs
# calendar = sw_gtfs$calendar
# summary(sw_gtfs$start_date)
# summary(sw_gtfs$end_date)

# run functions...
sw_stops <- count_stops(gtfs = sw_gtfs,
                        startdate = lubridate::ymd("2023-02-28"),
                        enddate = lubridate::ymd("2023-03-31"))
str(sw_stops)

sw_stops <- sw_stops %>%
  mutate(arrival_seconds = period_to_seconds(arrival_time),
         departure_seconds = period_to_seconds(departure_time))

# identify time slot
hours_to_secs <- function(x) x * 60 * 60

sw_stops <- sw_stops %>%
  mutate(rush_hour = between(arrival_seconds, hours_to_secs(6), hours_to_secs(9)) | between(arrival_seconds, hours_to_secs(16), hours_to_secs(19))) %>%
  mutate(evening = (arrival_seconds >= hours_to_secs(19)) | (arrival_seconds <= hours_to_secs(1)))

# summarise all services and stop frequency.
sw_stops_summary <- summarise_stops(sw_stops,
                                    gtfs = sw_gtfs)
# TODO:
# summarise weekend services v week day services
# summarise rush hour (or any given period...?)


#  geometry of routes -----------------------------------------------------



sw_trip_geoms <- make_trip_geoms(sw_gtfs)
qtm(sw_trip_geoms[1:1000,])

