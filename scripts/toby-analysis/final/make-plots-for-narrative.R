#  REGION -----------------------------------------------------------------
region_trips_analysis <- function(lsoa_trips_2004_2022) {

  trips_by_region_status <- summarise_trips_by_geog(trips_data = lsoa_trips_2004_2022,
                                                    geog_field = RGN11NM,
                                                    runs_field = runs_weekday_Morning_Peak,
                                                    post_pandemic = "keep")

  trips_by_region_clean <- clean_interp(trips_by_region_status,
                                        geog_var = RGN11NM)

  region_trips_by_year <- plot_geog_grid(trips_by_region_clean,
                                         geog_var = RGN11NM,
                                         ncols = 3,
                                         ylab_name = "Percent of max (Weekday am peak)")

  #plot(region_trips_by_year)
  ggsave(filename = "plots/region-trips-by-year.png",
         plot = region_trips_by_year,
         device = "png",
         width = 25,
         height = 30,
         units = "cm")

  # make wide summary
  trips_by_region_wide <- make_wide_data_check_table(trips_by_region_clean)

  return(trips_by_region_wide)

}

# METROPOLITAN AREA ANALYSIS ----------------------------------------------

metro_trips_analysis <- function(lsoa_trips_2004_2022) {

  trips_by_metro_status <- summarise_trips_by_geog(trips_data = lsoa_trips_2004_2022,
                                                   geog_field = metro_area_name,
                                                   runs_field = runs_weekday_Morning_Peak,
                                                   post_pandemic = "keep")

  trips_by_metro_clean <- clean_interp(trips_by_metro_status,
                                       geog_var = metro_area_name)

  metro_trips_by_year <- plot_geog_grid(trips_by_metro_clean,
                                        geog_var = metro_area_name,
                                        ncols = 3,
                                        ylab_name = "Percent of max (Weekday am peak)")

  ggsave(filename = "plots/metropolitan-area-trips-by-year.png",
         plot = metro_trips_by_year,
         device = "png",
         width = 25,
         height = 30,
         units = "cm")

  # make wide summary
  trips_by_metro_wide <- make_wide_data_check_table(trips_by_metro_clean,
                                                    geog_field = metro_area_name)

  return(trips_by_metro_wide)

}

