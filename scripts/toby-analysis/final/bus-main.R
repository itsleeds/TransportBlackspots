# set up, includes loading packages and sourcing scripts
clear_all = FALSE
source("scripts/toby-analysis/final/set-up.R")

# run functions
get_onspd = TRUE
if(get_onspd) {
  onspd <- load_onspd()
}

la_bustrips_cleaned <- make_clean_la_bustrips_data()
la_bustrips_cleaned <- add_la_code_2(la_bustrips_cleaned)
la_bustrips_cleaned <- add_region_by_la_code(la_bustrips_cleaned)
la_bustrips_cleaned <- join_metro_by_la_lup(la_bustrips_cleaned)
la_bustrips_cleaned <- la_bustrips_cleaned %>%
  select(LAD23CD,
         LAD23NM,
         RGN20CD,
         RGN20NM,
         CAUTH23CD,
         CAUTH23NM,
         everything())
