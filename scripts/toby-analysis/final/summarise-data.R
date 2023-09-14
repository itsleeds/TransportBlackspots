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
                   "tph_weekday_Evening",
                   #"tph_weekday_Night",
                   #"tph_Sat_Morning_Peak",
                   "tph_Sat_Midday",
                   #"tph_Sat_Afternoon_Peak",
                   #"tph_Sat_Evening",
                   #"tph_Sat_Night",
                   #"tph_Sun_Morning_Peak",
                   #"tph_Sun_Midday"
                   #",tph_Sun_Afternoon_Peak",
                   #"tph_Sun_Evening",
                   "tph_Sun_Night",
                   "tph_daytime_avg")

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
                                 geog,
                                 geog_code,
                                 geog_name,
                                 useFacets = TRUE,
                                 useLegend = FALSE,
                                 plotHeight = 9,
                                 plotWidth = 7.3) {

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
                   "tph_Sun_Night",
                   "tph_daytime_avg"
  )

  period_graph_names <- tolower(period_cols)
  period_graph_names <- gsub("tph", "Trips per hour:", period_graph_names)
  period_graph_names <- gsub("_", " ", period_graph_names)
  period_graph_names <- gsub("sun", "Sunday", period_graph_names)
  period_graph_names <- gsub("sat", "Saturday", period_graph_names)
  period_graph_names <- gsub("weekday", "Weekday", period_graph_names)
  period_graph_names <- gsub("daytime avg", "Weekday daytime average", period_graph_names)

  period_graph_names_max <- gsub("Trips per hour:", "Pct of max trips per hour:", period_graph_names)


  for(p in 1:length(period_cols)) {

    bustrips_summary_test <- bustrips_summary %>%
      filter(period_name == period_cols[p]) %>%
      mutate(Location := {{ geog_name }})

    # make the facets order by descending initial value of TPHs
    order_of_facets <- bustrips_summary_test %>%
      filter(year == min(year)) %>%
      arrange(desc(runs_cleaned))

    bustrips_summary_test$Location = factor(bustrips_summary_test$Location,
                                            levels = order_of_facets$Location)

    if(useFacets) {
      g1 <- ggplot(data = bustrips_summary_test, aes(x = year, y = runs_cleaned, col = Location)) +
        geom_line(show.legend = useLegend, linewidth = 1) +
        geom_point(show.legend = useLegend) +
        ylim(c(0,NA)) +
        facet_wrap(. ~ Location, nrow = 4) +
        ylab(period_graph_names[p]) +
        xlab("Year") +
        theme_bw()

      g2 <- ggplot(data = bustrips_summary_test, aes(x = year, y = tph_pct_max, col = Location)) +
        geom_line(show.legend = useLegend, linewidth = 1) +
        geom_point(show.legend = useLegend) +
        ylim(c(0,1)) +
        facet_wrap(. ~ Location, nrow = 4) +
        ylab(period_graph_names_max[p]) +
        xlab("Year") +
        theme_bw()
    } else if(!useFacets) {
      g1 <- ggplot(data = bustrips_summary_test, aes(x = year, y = runs_cleaned, col = Location)) +
        geom_line(show.legend = useLegend, linewidth = 1) +
        geom_point(show.legend = useLegend)  +
        ylim(c(0,NA)) +
        #facet_wrap(. ~ Location, nrow = 4) +
        ylab(period_graph_names[p]) +
        xlab("Year") +
        theme_bw()

      g2 <- ggplot(data = bustrips_summary_test, aes(x = year, y = tph_pct_max, col = Location)) +
        geom_line(show.legend = useLegend, linewidth = 1) +
        geom_point(show.legend = useLegend) +
        ylim(c(0,1)) +
        #facet_wrap(. ~ Location, nrow = 4) +
        ylab(period_graph_names_max[p]) +
        xlab("Year") +
        theme_bw()
    }

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
           height = plotHeight,
           width = plotWidth)

    message(paste0("saving: ", gg_filename_max, "..."))
    ggsave(filename = gg_filename_max,
           plot = g2,
           height = plotHeight,
           width = plotWidth)
  }

}

make_national_trend_summaries <- function() {

  # get processed cleaned wide data LSOA and simplify
  lsoa_bustrip_trends <- simplify_tph_trends(cleaned_bustrips_lsoa_wide_years = readRDS("data/bustrips_lsoa_2004_2023_cleaned_wide_years.rds"))

  lsoa_bustrip_trends <- add_rurality_lsoa(lsoa_bustrip_trends)

  lsoa_bustrip_trends <- lsoa_bustrip_trends %>%
    mutate(london_tube_rurality = case_when(london_tube == "Outside London" & rural_urban == "Urban" ~ "Outside London: Urban",
                                            london_tube == "Outside London" & rural_urban == "Rural" ~ "Outside London: Rural",
                                            london_tube == "Outside London" ~ "Scotland",
                                            TRUE ~ london_tube))

  # summarise by location and period.
  overall_summary_of_trends <- lsoa_bustrip_trends %>%
    group_by(london_tube_rurality,
             period_name) %>%
    summarise(tph_2006_08 = round(mean(tph_2006_08), 1),
              tph_2023 = round(mean(tph_2023), 1)) %>%
    ungroup() %>%
    mutate(tph_2006_2023_change = tph_2023 - tph_2006_08) %>%
    mutate(tph_2006_2023_change_pct = round(tph_2006_2023_change / tph_2006_08, 3)) %>%
    arrange(period_name,
            london_tube_rurality)

  # save
  write.csv(overall_summary_of_trends,
            "plots/aug-23/national-trend-summary-tph.csv",
            row.names = FALSE)

}

# run functions
make_la_region_cauth_summary_tables_and_plots <- function(la_bustrips_cleaned,
                                                          make_graphs = FALSE) {

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

  if(make_graphs) {
    make_graph_of_trends(la_bustrips_cleaned,
                         "region",
                         geog_code = RGN20CD,
                         geog_name = RGN20NM)

    make_graph_of_trends(la_bustrips_cleaned,
                         "cauth",
                         geog_code = CAUTH23CD,
                         geog_name = CAUTH23NM)
  }


}

make_londontube_summary_maps <- function(remove_scotland = TRUE) {

  # get lsoa level data (previously prepared from cleaning process)
  lsoa_bustrips_cleaned <- readRDS("data/bustrips_lsoa_2004_2023_cleaned.rds")

  lsoa_bustrips_cleaned <- add_rurality_lsoa(lsoa_bustrips_cleaned)

  # find which lsoas are on the london tube network (from raw lsoa data)
  lsoa_bustrips <- load_lsoa_bustrips()
  lsoa_bustrips <- lsoa_bustrips %>%
    distinct(lsoa11 = zone_id,
             region_name = RGN11NM,
             london_underground)

  # join this to make data set
  lsoa_bustrips_cleaned <- left_join(lsoa_bustrips_cleaned, lsoa_bustrips, by = "lsoa11")

  lsoa_bustrips_cleaned <- lsoa_bustrips_cleaned %>%
    mutate(london_tube_code = case_when(region_name == "London" & london_underground ~ "LT01",
                                        region_name == "London" & !london_underground ~ "LT02",
                                        region_name != "London" & rural_urban == "Urban" ~ "OLU01",
                                        region_name != "London" & rural_urban == "Rural" ~ "OLR02",
                                        region_name == "Scotland" ~ "SCOT01",
                                        TRUE ~ "OTHER01")) %>%
    mutate(london_tube_name = case_when(region_name == "London" & london_underground ~ "London: On tube",
                                        region_name == "London" & !london_underground ~ "London: Off tube",
                                        region_name != "London" & rural_urban == "Urban" ~ "Outside London: Urban",
                                        region_name != "London" & rural_urban == "Rural" ~ "Outside London: Rural",
                                        region_name == "Scotland" ~ "Scotland",
                                        TRUE ~ "Unknown location"))

  # remove scotland
  if(remove_scotland) {
    lsoa_bustrips_cleaned <- lsoa_bustrips_cleaned %>%
      filter(!region_name == "Scotland")
  }

  # use graph function to plot maps
  make_graph_of_trends(lsoa_bustrips_cleaned,
                       geog = "londonTube",
                       geog_code = london_tube_code,
                       geog_name = london_tube_name,
                       useFacets = FALSE,
                       useLegend = TRUE,
                       plotHeight = 6,
                       plotWidth = 7)

}


## bivariate analysis of LSOA data
lsoa_bustrips_bivariate_analysis <- function(threshold_3 = 0.15,
                                             threshold_2 = -0.15,
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


# Region and devolved nation boundary data
make_regions_sf <- function(detolerate = TRUE, detolerate_value = 500) {

  regions <- st_read("../gis-data/boundaries/regions/Regions_(December_2020)_EN_BFC/Regions_(December_2020)_EN_BFC.shp")
  countries <- st_read("../gis-data/boundaries/countries/Countries_December_2022_GB_BFC/CTRY_DEC_2022_GB_BFC.shp")
  countries <- st_make_valid(countries)
  scot_wales <- countries %>%
    filter(CTRY22NM %in% c("Scotland", "Wales")) %>%
    transmute(RGN20CD = CTRY22CD,
              RGN20NM = CTRY22NM,
              Shape__Are = as.numeric(st_area(geometry)),
              geometry)

  regions <- bind_rows(regions,
                       scot_wales)

  if(any(st_is_valid(regions)) == FALSE) {
    message("some regions geometry is invalid")
  }

  if(detolerate) {
    regions <- st_simplify(regions,
                          preserveTopology = FALSE,
                          dTolerance = detolerate_value)
  }

  # qtm(countries,
  #     fill = "CTRY22NM")
  # qtm(regions,
  #     fill = "RGN20NM")



  return(regions)

}

make_lsoa_bivariate_maps <- function() {

  # start timer
  tic("bivariate maps on tph changes 2006-08 to 2023 made")

  # run lsoa analysis
  #lsoa_bustrips <- readRDS("data/bustrips_lsoa_2004_2008_cleaned.rds")
  lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate_analysis(threshold_3 = 0.15,
                                                              threshold_2 = -0.15,
                                                              label_1 = "Worse",
                                                              label_2 = "Same",
                                                              label_3 = "Better")

  # focus on key time periods
  periods_for_maps <- c("tph_weekday_Morning_Peak",
                        #"tph_weekday_Midday",
                        #"tph_weekday_Afternoon_Peak",
                        "tph_weekday_Evening",
                        #"tph_weekday_Night",
                        #"tph_Sat_Morning_Peak",
                        "tph_Sat_Midday",
                        #"tph_Sat_Afternoon_Peak",
                        #"tph_Sat_Evening",
                        #"tph_Sat_Night",
                        #"tph_Sun_Morning_Peak",
                        #"tph_Sun_Midday"
                        #",tph_Sun_Afternoon_Peak",
                        #"tph_Sun_Evening",
                        "tph_Sun_Night",
                        "tph_daytime_avg"
  )

  period_map_names <- tolower(periods_for_maps)
  period_map_names <- gsub("tph", "Bus services 2008-2023:", period_map_names)
  period_map_names <- gsub("_", " ", period_map_names)
  period_map_names <- gsub("sun", "Sunday", period_map_names)
  period_map_names <- gsub("sat", "Saturday", period_map_names)
  period_map_names <- gsub("weekday", "Weekday", period_map_names)
  period_map_names <- gsub("daytime avg", "Week daytime average", period_map_names)

  tmap_mode("plot")
  myPalette2 <- c("#e6a3d0",#A1 - "Poor: Worse"
                  "#bc9fce", #B1 - "OK: Worse"
                  "#7b8eaf", #C1 - "Good: Worse"
                  "#eac5dd", #A2 - "Poor: Same"
                  "#9ec6d3", #B2 - "OK: Same"
                  "#7fc6b1", #C2 - "Good: Same"
                  "#f3f3f3", #A3 - "Poor: Better"
                  "#c2f1ce", #B3 - "OK: Better"
                  "#8be2af") #C3 - "Good: Better"

  lsoa_boundaries <- readRDS("../gis-data/boundaries/lsoa/GB_LSOA_2011_super_generalised.Rds")
  lsoa_boundaries <- rename(lsoa_boundaries, lsoa11 = code)

  la_boundaries <- st_read("../gis-data/boundaries/local-authority/Local_Authority_Districts_May_2023_UK_BGC_V2/LAD_MAY_2023_UK_BGC_V2.shp")
  la_boundaries <- la_boundaries %>%
    filter(substring(LAD23CD, 1, 1) != "N")

  region_boundaries <- make_regions_sf(detolerate = TRUE,
                                       detolerate_value = 200)

  la_boundaries <- st_simplify(la_boundaries,
                               preserveTopology = FALSE,
                               dTolerance = 200)

  for(p in 1:length(periods_for_maps)) {

    lsoa_bustrip_bi_map <- lsoa_bustrips_bivariate %>%
      filter(period_name == periods_for_maps[p])

    # removing missing values
    lsoa_bustrip_bi_map <- lsoa_bustrip_bi_map %>%
      filter(!is.na(trips_bi_variate))

    # make into a spatial file
    lsoa_bustrip_bi_map <- left_join(lsoa_boundaries, lsoa_bustrip_bi_map, by = "lsoa11")

    bi_map <- tm_shape(lsoa_bustrip_bi_map) +
      tm_polygons(col = "trips_bi_variate_label",
                  alpha = 1,
                  border.col = NA,
                  border.alpha = 0,
                  palette = myPalette2,
                  title = "Public tranport (2008 - 2023)") +
      tm_shape(la_boundaries) +
      tm_borders(col = "black",
                 lwd = 0.3) +
      tm_shape(region_boundaries) +
      tm_borders(col = "black",
                 lwd = 0.6) +
      tm_layout(legend.show = FALSE,
                frame = FALSE
                #title = period_map_names[p]
                )

    if(!dir.exists("plots/aug-23/bivariate")) {
      dir.create("plots/aug-23/bivariate")
    }

    p_filename <- tolower(periods_for_maps[p])
    p_filename <- gsub("_", "-", p_filename)
    map_filename <- paste0("plots/aug-23/bivariate/", p_filename, "-bivariate.png")
    tmap_save(tm = bi_map,
              filename = map_filename,
              dpi = 600)
  }

  # stop timer
  toc()

}


# rurality
add_rurality_lsoa <- function(df) {

  rurality_lsoa <- read.csv("../constituency-environmental-reports/data/general/Rural_Urban_Classification_(2011)_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv",
                            stringsAsFactors = FALSE)
  rurality_lsoa <- rurality_lsoa %>%
    transmute(lsoa11 = LSOA11CD,
              rural_urban = case_when(grepl("Urban", RUC11) ~ "Urban",
                                      grepl("Rural", RUC11) ~ "Rural"))

  # table(rurality_lsoa$ruc11,
  #       rurality_lsoa$rural_urban,
  #       useNA = "ifany")
  df <- left_join(df, rurality_lsoa, by = "lsoa11")

}

make_bivariate_summary <- function() {

  # run lsoa analysis
  #lsoa_bustrips <- readRDS("data/bustrips_lsoa_2004_2008_cleaned.rds")
  lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate_analysis(threshold_3 = 0.15,
                                                              threshold_2 = -0.15,
                                                              label_1 = "Worse",
                                                              label_2 = "Same",
                                                              label_3 = "Better")

  # focus on key time periods
  periods_for_maps <- c("tph_weekday_Morning_Peak",
                        #"tph_weekday_Midday",
                        #"tph_weekday_Afternoon_Peak",
                        "tph_weekday_Evening",
                        #"tph_weekday_Night",
                        #"tph_Sat_Morning_Peak",
                        "tph_Sat_Midday",
                        #"tph_Sat_Afternoon_Peak",
                        #"tph_Sat_Evening",
                        #"tph_Sat_Night",
                        #"tph_Sun_Morning_Peak",
                        #"tph_Sun_Midday"
                        #",tph_Sun_Afternoon_Peak",
                        #"tph_Sun_Evening",
                        "tph_Sun_Night",
                        "tph_daytime_avg"
  )

  period_map_names <- tolower(periods_for_maps)
  period_map_names <- gsub("tph", "Bus services 2008-2023:", period_map_names)
  period_map_names <- gsub("_", " ", period_map_names)
  period_map_names <- gsub("sun", "Sunday", period_map_names)
  period_map_names <- gsub("sat", "Saturday", period_map_names)
  period_map_names <- gsub("weekday", "Weekday", period_map_names)
  period_map_names <- gsub("daytime avg", "Week daytime average", period_map_names)

  bivariate_summary_name <- gsub("Bus services 2008-2023: ", "", period_map_names)

  # add rurality
  lsoa_bustrips_bivariate <- add_rurality_lsoa(lsoa_bustrips_bivariate)
  lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate %>%
    mutate(rural_urban = ifelse(region_name == "London", "Urban", rural_urban))

  bi_summary <- list()

  for(p in 1:length(periods_for_maps)) {

    lsoa_bustrip_bi_map <- lsoa_bustrips_bivariate %>%
      filter(period_name == periods_for_maps[p]) %>%
      # remove scotland
      filter(region_name != "Scotland")

    # removing missing values
    lsoa_bustrip_bi_map <- lsoa_bustrip_bi_map %>%
      filter(!is.na(trips_bi_variate))

    bivariate_summary_new <- lsoa_bustrip_bi_map %>%
      group_by(region_name,
               rural_urban,
               period = bivariate_summary_name[p],
               trips_bi_variate_label) %>%
      summarise(number_lsoas = n()) %>%
      ungroup() %>%
      spread(key = trips_bi_variate_label,
             value = number_lsoas,
             fill = 0)

    bi_summary[[p]] <- bivariate_summary_new

  }

  bi_summary <- bind_rows(bi_summary)

  bi_summary <- bi_summary %>%
    mutate(london = ifelse(region_name == "London", "London", "Rest of country (outside London)")) %>%
    group_by(london,
             rural_urban,
             period) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(london != "London") %>%
    mutate(region_name = london) %>%
    select(-london) %>%
    bind_rows(bi_summary, .)

  bi_summary <- bi_summary %>%
    filter(period == "Week daytime average") %>%
    filter(region_name %in% c("London", "Rest of country (outside London)"))

  write.csv(bi_summary,
            "plots/aug-23/bivariate/bivariate-engwales-summary.csv",
            row.names = FALSE)

  }

make_la_maps <- function(la_bustrip_trends) {

  # start timer
  tic("Local authority change in TPH maps saved.")

  # list all tph cols
  tph_cols <- names(la_bustrip_trends)[grepl("tph.+change_pct", names(la_bustrip_trends))]

  # convert pct into a percentage value
  convert_to_percentage <- function(x, n = 0) round(x * 100, n)
  la_bustrip_trends <- la_bustrip_trends %>%
    mutate(across(all_of(tph_cols), convert_to_percentage))

  # get LA boundaries for mapping
  la_boundaries <- st_read("../gis-data/boundaries/local-authority/Local_Authority_Districts_May_2023_UK_BGC_V2/LAD_MAY_2023_UK_BGC_V2.shp")
  la_boundaries <- la_boundaries %>%
    select(LAD23CD,
           geometry)

  # get region boundaries
  region_boundaries <- make_regions_sf(detolerate = TRUE,
                                       detolerate_value = 200)

  # create sf object from bustrips data
  la_bustrip_trends <- left_join(la_boundaries, la_bustrip_trends, by = "LAD23CD")
  la_bustrip_trends <- la_bustrip_trends %>%
    filter(substring(LAD23CD, 1, 1) != "N") # remove NI (no data)

  # make nice legend titles
  legend_titles <- tolower(tph_cols)
  legend_titles <- gsub("tph_", "", legend_titles)
  legend_titles <- gsub("_", " ", legend_titles)
  legend_titles <- gsub("sun", "Sunday", legend_titles) # replace with full spelling
  legend_titles <- gsub("sat", "Saturday", legend_titles) # replace with full spelling
  legend_titles <- gsub("weekday", "Weekday", legend_titles) # replace with full spelling
  legend_titles <- gsub("daytime", "Daytime", legend_titles) # replace with full spelling
  legend_titles <- gsub("avg", "average", legend_titles) # replace with full spelling
  legend_titles <- gsub("2006 2023", "", legend_titles) # remove this (it is in title)
  legend_titles <- gsub("change pct", "", legend_titles) # remove this (added below in better format on new line)
  legend_titles <- gsub(" {2,}", " ", legend_titles) # remove more than one consequetive space
  legend_titles <- gsub(" $", "", legend_titles) # remove space on the end of the line
  message(c("These are the titles for the graphs that will be made: ", legend_titles))
  legend_titles <- paste0(legend_titles, "\n(% change)") # and a break/new line and explainer of data/unit etc.

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
                  breaks = c(-100,-75,-50,-25,0,11,Inf),
                  midpoint = NA,
                  colorNA = NULL,
                  textNA = "") +
      tm_shape(region_boundaries) +
      tm_borders(col = "black",
                 lwd = 0.6) +
      tm_layout(frame = FALSE,
                #legend.frame = "black",
                legend.position = c(0.63,0.68),
                #title = "Change in bus service trips per hour, 2008-23",
                )

    if(!dir.exists("plots/aug-23/la-maps")) {
      dir.create("plots/aug-23/la-maps")
    }

    # save maps
    t_filename <- tolower(tph_cols[t])
    t_filename <- gsub("tph_", "", t_filename)
    t_filename <- gsub("_", "-", t_filename)
    t_filename <- gsub("-2006-2023-change-pct", "", t_filename)
    t_map_filename <- paste0("plots/aug-23/la-maps/", t_filename, "-change-la-map.png")
    tmap_save(tm = la_map,
              filename = t_map_filename,
              dpi = 600)
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
