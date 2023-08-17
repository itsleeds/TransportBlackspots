# set up, includes loading packages and sourcing scripts
clear_all = FALSE
source("scripts/toby-analysis/final/set-up.R")

# run functions
get_onspd = FALSE
if(get_onspd) {
  onspd <- load_onspd()
}

# run LA, Region and C. Auth functions
la_bustrips <- make_la_bustrips()
make_la_region_cauth_summary_tables_and_plots(la_bustrips)

# run lsoa analysis
#lsoa_bustrips <- readRDS("data/bustrips_lsoa_2004_2008_cleaned.rds")


