#' Plan
#' For London, aggregate average number of trips per hour by london underground lsoas and non-underground lsoas
#' from LSOA level data.
#' Outside london use LA level data

#' this needs to be done on LA and LSOA trend data. So run these and then save as RDS objects.
#'
#' 2. Bivariate map
#' Calculate the number of routes in an LSOA.
#' then calculate



# -------------------------------------------------------------------------


# set up
rm(list = ls())
# load scripts with relevant functions
source("scripts/toby-analysis/bus-trips-analysis-functions.R")
#source("scripts/toby-analysis/onspd.R")
#source("scripts/toby-analysis/la-amalgamation-2023.R")

# load packages
load_packages()

tmap_mode("view")

# quiten group summarise
options(dplyr.summarise.inform = FALSE)

# get onspd
#onspd <- get_onspd(keep.only.current = TRUE)
bustrips_lsoa <- load_trips_data(geog = "lsoa",
                                 mode_no = 3)

london_metro_lsoas <- bustrips_lsoa %>%
  filter(year == max(year) & london_underground)

lsoa_boundaries <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/")

london_metro_lsoas  <- inner_join(lsoa_boundaries, london_metro_lsoas, by = c("LSOA11CD" = "zone_id"))

london_stations <- st_read("data/london-underground/London stations/London stations.shp")
london_stations <- st_transform(london_stations, crs = 27700)

london_lines <- st_read("data/london-underground/London Train Lines/London Train Lines.shp")
london_lines <- st_transform(london_lines, crs = 27700)
london_lines <- london_lines %>%
  separate(Name, into = c("line", "section"), sep = " - ")

underground <- c("Bakerloo",
                 #"C2C",
                 "Central",
                 #"Chiltern Railways",
                 "Circle",
                 "District",
                 #"DLR",
                 #"Elizabeth",
                 #"Great Northern",
                 #"Great Western",
                 #"Greater Anglia",
                 "Hammersmith and City",
                 #"Heathrow Connect",
                 #"Heathrow Express",
                 "Jubilee",
                 #"London Midland",
                 "Metropolitan",
                 "Northern",
                 #"Overground",
                 "Piccadilly",
                 #"South Western",
                 #"Southeastern",
                 #"Southern",
                 #"Thameslink",
                 #"Tramlink",
                 "Victoria",
                 "Waterloo and City")

london_underground_lines <- london_lines %>%
  filter(line %in% underground)

london_underground_stations <- st_intersection(london_underground_lines, london_stations)

tm_shape(london_metro_lsoas) +
  tm_polygons(col = "blue", alpha = 0.5) +
  tm_shape(london_underground_lines) +
  tm_lines(col = "line", lwd = 4) +
  tm_shape(london_underground_stations) +
  tm_dots(col = "black")

