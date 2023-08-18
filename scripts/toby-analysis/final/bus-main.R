# set up, includes loading packages and sourcing scripts
clear_all = FALSE
source("scripts/toby-analysis/final/set-up.R")

# run functions
get_onspd = FALSE
if(get_onspd) {
  onspd <- load_onspd()
}

# run LA, Region and C. Auth functions
la_bustrips <- make_la_bustrips() # this cleans and interpolates for all tph cols, and makes a long table
la_bustrips <- readRDS("data/la_bustrip_trends_2008_2023.rds") # reads in from previously created

# make_la_region_cauth_summary_tables_and_plots(la_bustrips)

# creates a small version of selected tph cols wide table
la_bustrip_trends <- make_trend_summary(la_bustrips,
                                         geog = "LA",
                                         geog_code = LAD23CD,
                                         geog_name = LAD23NM)

# which can then be used to make maps at LA level
make_la_maps(la_bustrip_trends)
