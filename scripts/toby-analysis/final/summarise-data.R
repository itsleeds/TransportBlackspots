# For regional and combined authorities, we want to show the following:
# tables of reductions (in %) for each time of the week (then pull out the most significant to show)
# graphs of all times for all areas.

# function to summarise the data by different geographies and times of the day
make_trend_summary <- function(la_bustrips_cleaned,
                               geog,
                               geog_code,
                               geog_name) {

  cleaned_bustrips_wide_years <- la_bustrips_cleaned %>%
    filter(!is.na({{ geog_name }})) %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             period_name,
             year) %>%
    summarise(runs_cleaned = mean(runs_cleaned, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(key = year,
           value = runs_cleaned)

  cleaned_bustrips_wide_years <- cleaned_bustrips_wide_years %>%
    transmute({{ geog_code }},
              {{ geog_name }},
              period_name,
              tph_avg_2006_08 = (`2006` + `2007` + `2008`) / 3,
              tph_2010 = `2010`,
              tph_2019 = `2019`,
              tph_2022 = `2022`,
              tph_2023 = `2023`) %>%
    mutate(tph_2006_2023_change = tph_2023 - tph_avg_2006_08) %>%
    mutate(tph_2006_2023_change_pct = tph_2006_2023_change / tph_avg_2006_08)

  # focus on key time periods
  period_cols <- c("tph_weekday_Morning_Peak",
                   #"tph_weekday_Midday",
                   #"tph_weekday_Afternoon_Peak",
                   #"tph_weekday_Evening",
                   #"tph_weekday_Night",
                   #"tph_Sat_Morning_Peak",
                   "tph_Sat_Midday",
                   #"tph_Sat_Afternoon_Peak",
                   #"tph_Sat_Evening",
                   "tph_Sat_Night",
                   #"tph_Sun_Morning_Peak",
                   "tph_Sun_Midday"
                   #",tph_Sun_Afternoon_Peak",
                   #"tph_Sun_Evening",
                   #"tph_Sun_Night"
  )

  cleaned_bustrips_wide_years <- cleaned_bustrips_wide_years %>%
    filter(period_name %in% period_cols)

  bustrips_summary <- cleaned_bustrips_wide_years %>%
    rename(geogcd = {{ geog_code }},
           geognm = {{ geog_name }}) %>%
    select(geogcd, #{{ geog_code }},
           geognm, #{{ geog_name }},
           period_name,
           tph_avg_2006_08,
           tph_2023,
           tph_2006_2023_change_pct) %>%
    gather(key = indicator,
           value = val,
           -geogcd,
           -geognm,
           -period_name) %>%
    unite(full_indicator, period_name, indicator, sep = "_") %>%
    spread(key = full_indicator,
           value = val)

  bustrips_summary <- bustrips_summary %>%
    rename({{ geog_code }} := geogcd,
           {{ geog_name }} := geognm)

  # save outputs
  if(!dir.exists("plots/aug-23")) {
    dir.create("plots/aug-23")
  }
  summary_filename <- paste0("plots/aug-23/", geog, "-summary-of-bustrip-trends-2007-2023.csv")
  message(paste0("Saving: ", summary_filename))
  write.csv(bustrips_summary,
            summary_filename,
            row.names = FALSE,
            na = "")

  return(bustrips_summary)
}


make_graph_of_trends <- function(la_bustrips_cleaned,
                                 geog = "region",
                                 geog_code,
                                 geog_name) {

  bustrips_summary <- la_bustrips_cleaned %>%
    filter(!is.na({{ geog_name }})) %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             period_name,
             year) %>%
    summarise(runs_cleaned = mean(runs_cleaned, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(runs_cleaned = ifelse(is.nan(runs_cleaned), NA, runs_cleaned)) %>%
    group_by({{ geog_code }},
             {{ geog_name }},
             period_name) %>%
    mutate(tph_pct_max = runs_cleaned / max(runs_cleaned)) %>%
    ungroup()

  # focus on key time periods
  period_cols <- c("tph_weekday_Morning_Peak",
                   "tph_weekday_Midday",
                   "tph_weekday_Afternoon_Peak",
                   "tph_weekday_Evening",
                   "tph_weekday_Night",
                   "tph_Sat_Morning_Peak",
                   "tph_Sat_Midday",
                   "tph_Sat_Afternoon_Peak",
                   "tph_Sat_Evening",
                   "tph_Sat_Night",
                   "tph_Sun_Morning_Peak",
                   "tph_Sun_Midday",
                   "tph_Sun_Afternoon_Peak",
                   "tph_Sun_Evening",
                   "tph_Sun_Night"
  )

  period_graph_names <- tolower(period_cols)
  period_graph_names <- gsub("tph", "Trips per hour:", period_graph_names)
  period_graph_names <- gsub("_", " ", period_graph_names)
  period_graph_names <- gsub("sun", "Sunday", period_graph_names)
  period_graph_names <- gsub("sat", "Saturday", period_graph_names)
  period_graph_names <- gsub("weekday", "Weekday", period_graph_names)

  period_graph_names_max <- gsub("Trips per hour:", "Pct of max trips per hour:", period_graph_names)


  for(p in 1:length(period_cols)) {

    bustrips_summary_test <- bustrips_summary %>%
      filter(period_name == period_cols[p]) %>%
      mutate(geography_name := {{ geog_name }})

    g1 <- ggplot(data = bustrips_summary_test, aes(x = year, y = runs_cleaned, col = geography_name)) +
      geom_line(show.legend = FALSE, linewidth = 1) +
      geom_point(show.legend = FALSE) +
      facet_wrap(. ~ geography_name, nrow = 4) +
      ylab(period_graph_names[p]) +
      xlab("Year") +
      theme_bw()

    g2 <- ggplot(data = bustrips_summary_test, aes(x = year, y = tph_pct_max, col = geography_name)) +
      geom_line(show.legend = FALSE, linewidth = 1) +
      geom_point(show.legend = FALSE) +
      ylim(c(0,1)) +
      facet_wrap(. ~ geography_name, nrow = 4) +
      ylab(period_graph_names_max[p]) +
      xlab("Year") +
      theme_bw()

    if(!dir.exists("plots/aug-23")) {
      dir.create("plots/aug-23")
    }

    if(!dir.exists(paste0("plots/aug-23/", geog))) {
      dir.create(paste0("plots/aug-23/", geog))
    }

    gg_filename <- paste0("plots/aug-23/", geog,"/", period_cols[p], ".png")
    gg_filename_max <- paste0("plots/aug-23/", geog,"/", period_cols[p], "_pct_max.png")

    message(paste0("saving: ", gg_filename, "..."))
    ggsave(filename = gg_filename,
           plot = g1,
           height = 9,
           width = 7.3)

    message(paste0("saving: ", gg_filename_max, "..."))
    ggsave(filename = gg_filename_max,
           plot = g2,
           height = 9,
           width = 7.3)
  }


}

# run functions
make_la_region_cauth_summary_tables_and_plots <- function(la_bustrips_cleaned) {

  la_bustrip_trends <<- make_trend_summary(la_bustrips_cleaned,
                                          geog = "LA",
                                          geog_code = LAD23CD,
                                          geog_name = LAD23NM)

  region_bustrip_trends <<- make_trend_summary(la_bustrips_cleaned,
                                              geog = "region",
                                              geog_code = RGN20CD,
                                              geog_name = RGN20NM)

  cauth_bustrip_trends <<- make_trend_summary(la_bustrips_cleaned,
                                             geog = "cauth",
                                             geog_code = CAUTH23CD,
                                             geog_name = CAUTH23NM)

  make_graph_of_trends(la_bustrips_cleaned,
                       "region",
                       geog_code = RGN20CD,
                       geog_name = RGN20NM)

  make_graph_of_trends(la_bustrips_cleaned,
                       "cauth",
                       geog_code = CAUTH23CD,
                       geog_name = CAUTH23NM)

}

## bivariate analysis of LSOA data
lsoa_bustrips_bivariate_analysis <- function(threshold_3 = 0.5,
                                             threshold_2 = -0.40,
                                             label_1 = "Worse",
                                             label_2 = "Same",
                                             label_3 = "Better") {

  lsoa_bustrips_wide <- readRDS("data/bustrips_lsoa_2004_2008_cleaned_wide_years.rds")
  lsoa_trends_2008_23 <- simplify_tph_trends(lsoa_bustrips_wide)

  # bivariate classification
  # LEVEL OF SERVICE 2006-08
  # find thresholds outside london
  non_london_data <- lsoa_trends_2008_23 %>%
    filter(region_name != "London")

  thresholds <- non_london_data %>%
    summarise(tph_2008_first_third = quantile(tph_2006_08, 0.33),
              tph_2008_last_third = quantile(tph_2006_08, 0.67))

  threshold_b <- round(thresholds$tph_2008_first_third)
  threshold_c <- round(thresholds$tph_2008_last_third)

  lsoa_trends_2008_23 <- lsoa_trends_2008_23 %>%
    mutate(service_frequency_2008 = case_when(tph_2006_08 > threshold_c ~ "C",
                                              between(tph_2006_08, threshold_b, threshold_c) ~ "B",
                                              tph_2006_08 < threshold_b ~ "A"),
           service_frequency_2008_label = case_when(tph_2006_08 > threshold_c ~ "Good",
                                                    between(tph_2006_08, threshold_b, threshold_c) ~ "OK",
                                                    tph_2006_08 < threshold_b ~ "Poor"))

  # check: table(lsoa_trends_2008_23$service_frequency_2008_label)

  # REDUCTION IN SERVICE
  lsoa_trends_2008_23 <- lsoa_trends_2008_23 %>%
    mutate(service_reduction_2008_23 = case_when(tph_2006_2023_change_pct >= threshold_3 ~ "3",
                                                 between(tph_2006_2023_change_pct, threshold_2, threshold_3) ~ "2",
                                                 tph_2006_2023_change_pct < threshold_2 ~ "1"),
           service_reduction_2008_23_label = case_when(tph_2006_2023_change_pct >= threshold_3 ~ label_3,
                                                       between(tph_2006_2023_change_pct, threshold_2, threshold_3) ~ label_2,
                                                       tph_2006_2023_change_pct < threshold_2 ~ label_1))

  # check: table(lsoa_trends_2008_23$service_reduction_2008_23)
  table(lsoa_trends_2008_23$service_frequency_2008,
        lsoa_trends_2008_23$service_reduction_2008_23)

  lsoa_trips_bivariate <- lsoa_trends_2008_23 %>%
    filter(!is.na(service_reduction_2008_23)) %>%
    filter(!is.na(service_frequency_2008)) %>%
    unite(trips_bi_variate, service_frequency_2008, service_reduction_2008_23, sep = "", remove = FALSE) %>%
    unite(trips_bi_variate_label, service_frequency_2008_label, service_reduction_2008_23_label, sep = ": ", remove = FALSE) %>%
    mutate(trips_bi_variate = as.factor(trips_bi_variate),
           trips_bi_variate_label = as.factor(trips_bi_variate_label)) %>%
    transmute(lsoa11,
              london_tube,
              region_name,
              period_name,
              trips_bi_variate,
              trips_bi_variate_label,
              service_frequency_2008,
              service_frequency_2008_label,
              service_reduction_2008_23,
              service_reduction_2008_23_label,
              tph_2006_08,
              tph_2023,
              tph_2006_2023_change,
              tph_2006_2023_change_pct = round(tph_2006_2023_change_pct * 100, 1))

  # Sort levels out for bivariate_label field (so graph plots with right colour scheme)
  lsoa_trips_bivariate$trips_bi_variate_label <- factor(lsoa_trips_bivariate$trips_bi_variate_label,
                                                        levels =  c(
                                                          paste("Poor", label_1, sep = ": "),
                                                          paste("OK", label_1, sep = ": "),
                                                          paste("Good", label_1, sep = ": "),
                                                          paste("Poor", label_2, sep = ": "),
                                                          paste("OK", label_2, sep = ": "),
                                                          paste("Good", label_2, sep = ": "),
                                                          paste("Poor", label_3, sep = ": "),
                                                          paste("OK", label_3, sep = ": "),
                                                          paste("Good", label_3, sep = ": ")
                                                        ))
 print(table(lsoa_trips_bivariate$trips_bi_variate_label))

 return(lsoa_trips_bivariate)

}


make_lsoa_bivariate_maps <- function() {

  # run lsoa analysis
  #lsoa_bustrips <- readRDS("data/bustrips_lsoa_2004_2008_cleaned.rds")
  lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate_analysis(threshold_3 = 0.05,
                                                              threshold_2 = -0.33,
                                                              label_1 = "Worse",
                                                              label_2 = "Same",
                                                              label_3 = "Better")

  # focus on key time periods
  periods_for_maps <- c("tph_weekday_Morning_Peak",
                        #"tph_weekday_Midday",
                        #"tph_weekday_Afternoon_Peak",
                        #"tph_weekday_Evening",
                        #"tph_weekday_Night",
                        #"tph_Sat_Morning_Peak",
                        "tph_Sat_Midday",
                        #"tph_Sat_Afternoon_Peak",
                        #"tph_Sat_Evening",
                        "tph_Sat_Night",
                        #"tph_Sun_Morning_Peak",
                        "tph_Sun_Midday"
                        #",tph_Sun_Afternoon_Peak",
                        #"tph_Sun_Evening",
                        #"tph_Sun_Night"
  )

  period_map_names <- tolower(periods_for_maps)
  period_map_names <- gsub("tph", "Bus services 2008-2023:", period_map_names)
  period_map_names <- gsub("_", " ", period_map_names)
  period_map_names <- gsub("sun", "Sunday", period_map_names)
  period_map_names <- gsub("sat", "Saturday", period_map_names)
  period_map_names <- gsub("weekday", "Weekday", period_map_names)

  tmap_mode("plot")
  myPalette2 <- c("#f3f3f3",#A1 - "Poor: Worse"
                  "#eac5dd", #B1 - "OK: Worse"
                  "#e6a3d0", #C1 - "Good: Worse"
                  "#c2f1ce", #A2 - "Poor: Same"
                  "#9ec6d3", #B2 - "OK: Same"
                  "#bc9fce", #C2 - "Good: Same"
                  "#8be2af", #A3 - "Poor: Better"
                  "#7fc6b1", #B3 - "OK: Better"
                  "#7b8eaf") #C3 - "Good: Better"

  for(p in 1:length(periods_for_maps)) {

    lsoa_bustrip_bi_map <- lsoa_bustrips_bivariate %>%
      filter(period_name == periods_for_maps[p])

    lsoa_boundaries <- readRDS("../gis-data/boundaries/lsoa/GB_LSOA_2011_super_generalised.Rds")
    lsoa_boundaries <- rename(lsoa_boundaries, lsoa11 = code)
    lsoa_bustrip_bi_map <- left_join(lsoa_boundaries, lsoa_bustrip_bi_map, by = "lsoa11")

    # removing missing values
    lsoa_bustrip_bi_map <- lsoa_bustrip_bi_map %>%
      filter(!is.na(trips_bi_variate))

    bi_map <- tm_shape(lsoa_bustrip_bi_map) +
      tm_polygons(col = "trips_bi_variate_label",
                  alpha = 1,
                  border.col = NA,
                  border.alpha = 0,
                  palette = myPalette2,
                  title = "Public tranport (2008 - 2023)") +
      tm_layout(legend.show = FALSE,
                frame = FALSE,
                title = period_map_names[p])

    if(!dir.exists("plots/aug-23/bivariate")) {
      dir.create("plots/aug-23/bivariate")
    }

    p_filename <- tolower(periods_for_maps[p])
    p_filename <- gsub("_", "-", p_filename)
    map_filename <- paste0("plots/aug-23/bivariate/", p_filename, "-bivariate.png")
    tmap_save(tm = bi_map,
              filename = map_filename,
              dpi = 400)
  }


}

make_la_maps <- function(la_bustrip_trends) {

  # start timer
  tic("Local authority change in TPH maps saved.")

  # list all tph cols
  tph_cols <- names(la_bustrip_trends)[grepl("tph", names(la_bustrip_trends))]

  # convert pct into a percentage value
  convert_to_percentage <- function(x, n = 0) round(x * 100, n)
  la_bustrip_trends <- la_bustrip_trends %>%
    mutate(across(all_of(tph_cols), convert_to_percentage))

  # get LA boundaries for mapping
  la_boundaries <- st_read("../gis-data/boundaries/local-authority/Local_Authority_Districts_May_2023_UK_BGC_V2/LAD_MAY_2023_UK_BGC_V2.shp")
  la_boundaries <- la_boundaries %>%
    select(LAD23CD,
           geometry)

  # create sf object from bustrips data
  la_bustrip_trends <- left_join(la_boundaries, la_bustrip_trends, by = "LAD23CD")
  la_bustrip_trends <- la_bustrip_trends %>%
    filter(substring(LAD23CD, 1, 1) != "N") # remove NI (no data)

  # make nice legend titles
  legend_titles <- tolower(tph_cols)
  legend_titles <- gsub("tph_", "", legend_titles)
  legend_titles <- gsub("_", " ", legend_titles)
  legend_titles <- gsub("sun", "Sunday", legend_titles)
  legend_titles <- gsub("sat", "Saturday", legend_titles)
  legend_titles <- gsub("weekday", "Weekday", legend_titles)
  legend_titles <- paste0(legend_titles, "\n(% change)")

  #tmap_mode("view")
  tmap_mode("plot")

  #run loop that makes and saves a map for each of the tph cols in the data set
  for(t in 1:length(tph_cols)) {

    #make maps
    la_map <- tm_shape(la_bustrip_trends) +
      tm_polygons(col = tph_cols[t],
                  title = legend_titles[t],
                  palette = hcl.colors(10, palette = "RdYlBu"),
                  border.alpha = 0.1,
                  border.col = "white",
                  breaks = c(-100,-75,-50,-25,0,25,50,Inf),
                  midpoint = NA) +
      tm_layout(frame = FALSE,
                #legend.frame = "black",
                legend.position = c(0.63,0.68),
                title = "Change in bus service trips per hour, 2008-23")

    if(!dir.exists("plots/aug-23/la-maps")) {
      dir.create("plots/aug-23/la-maps")
    }

    # save maps
    t_filename <- tolower(tph_cols[t])
    t_filename <- gsub("tph_", "", t_filename)
    t_filename <- gsub("_", "-", t_filename)
    t_map_filename <- paste0("plots/aug-23/la-maps/", t_filename, "-change-la-map.png")
    tmap_save(tm = la_map,
              filename = t_map_filename,
              dpi = 400)
  }

  # stop timer
  toc()

}


# OLD ---------------------------------------------------------------------

old_maps <- function() {

  tmap_mode("view")
  tm_shape(lsoa_bustrip_bi_map) +
    tm_polygons(col = "trips_bi_variate_label",
                alpha = 1,
                border.col = NA,
                border.alpha = 0,
                palette = myPalette2,
                title = "Public tranport (2008 - 2023)",
                popup.vars = c("Frequency (2008)" = "service_frequency_2008_label",
                               "Change in service (2008-2023)" = "service_reduction_2008_23_label",
                               "Trips per hour (2008)" = "tph_2006_08",
                               "Trips per hour (2023)" = "tph_2023",
                               "Change in trips per hour (%)" = "tph_2006_2023_change_pct"))


}

# end ---------------------------------------------------------------------
