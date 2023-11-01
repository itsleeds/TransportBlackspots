reload = FALSE
# set up, includes loading packages and sourcing scripts
clear_all = reload
source("scripts/toby-analysis/final/set-up.R")

# run functions
get_onspd = reload
if(get_onspd) {
  onspd <- load_onspd()
}


# run LA, Region and C. Auth functions
#la_bustrips <- make_la_bustrips() # this cleans and interpolates for all tph cols, and makes a long table
la_bustrips <- readRDS("data/la_bustrips_2005_23_cleaned.rds") # reads in from previously created

make_la_region_cauth_summary_tables_and_plots(la_bustrips, make_graphs = FALSE)

## LSOA based analysis...
#lsoa_bustrips_cleaned <- make_clean_lsoa_bustrips_data()
lsoa_bustrips_cleaned <- readRDS("data/bustrips_lsoa_2004_2023_cleaned.rds")

make_londontube_summary_maps()
national_trends <- make_national_trend_summaries()

# creates a small version of selected tph cols wide table
la_bustrip_trends <- make_trend_summary(la_bustrips,
                                         geog = "LA",
                                         geog_code = LAD23CD,
                                         geog_name = LAD23NM)

# which can then be used to make maps at LA level
make_la_maps(la_bustrip_trends)

la_bustrip_trends <- la_bustrip_trends %>%
  mutate(tph_daytime_avg_tph_2006_2023_change = tph_daytime_avg_tph_2023 - tph_daytime_avg_tph_avg_2006_08)


# ---- make lsoa bivariate maps ---------------------------------------------------------

lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate_analysis(threshold_3 = 0.15,
                                                            threshold_2 = -0.15,
                                                            label_1 = "Worse",
                                                            label_2 = "Same",
                                                            label_3 = "Better")

make_lsoa_bivariate_maps(lsoa_bustrips_bivariate)

