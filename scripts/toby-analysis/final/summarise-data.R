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
              tph_avg_2005_07 = (`2005` + `2006` + `2007`) / 3,
              tph_2010 = `2010`,
              tph_2019 = `2019`,
              tph_2022 = `2022`,
              tph_2023 = `2023`) %>%
    mutate(tph_2006_2023_change = tph_2023 - tph_avg_2005_07) %>%
    mutate(tph_2006_2023_change_pct = tph_2006_2023_change / tph_avg_2005_07)

  bustrips_summary <- cleaned_bustrips_wide_years %>%
    select({{ geog_code }},
           {{ geog_name }},
           period_name,
           tph_2006_2023_change_pct) %>%
    spread(key = period_name,
           value = tph_2006_2023_change_pct)

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

  bustrips_summary <- bustrips_summary %>%
    select({{ geog_code }},
           {{ geog_name }},
           all_of(period_cols))

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
 table(lsoa_trips_bivariate$trips_bi_variate_label)

}


