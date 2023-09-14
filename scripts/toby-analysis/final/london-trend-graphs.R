
#we want to make a graph that adds a empty facet in position 4.
#And make london graph twice the height...

la_bustrips <- readRDS("data/la_bustrips_2005_23_cleaned.rds") # reads in from previously created

# in la bustrips we need to add another region.
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
                 "tph_daytime_avg")

blank_region <- data.frame(expand.grid(year = seq(2005,2023),
                                       period_name = period_cols,
                                       RGN20CD = c("LB1", "LB2")))

blank_region <- blank_region %>%
  mutate(RGN20NM = case_when(RGN20CD == "LB1" ~ "Greater London",
                             RGN20CD == "LB2" ~ "London Blank"),
         runs = 0,
         runs_cleaned = 0)

la_bustrips <- bind_rows(blank_region,
                         la_bustrips)

bustrips_summary <- la_bustrips %>%
  group_by(RGN20CD,
           RGN20NM,
           period_name,
           year) %>%
  summarise(runs_cleaned = mean(runs_cleaned, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(runs_cleaned = ifelse(is.nan(runs_cleaned), NA, runs_cleaned)) %>%
  group_by(RGN20CD,
           RGN20NM,
           period_name) %>%
  mutate(tph_pct_max = runs_cleaned / max(runs_cleaned)) %>%
  mutate(tph_pct_max = ifelse(is.nan(tph_pct_max), 0, tph_pct_max)) %>%
  ungroup()

period_graph_names <- tolower(period_cols)
period_graph_names <- gsub("tph", "Trips per hour:", period_graph_names)
period_graph_names <- gsub("_", " ", period_graph_names)
period_graph_names <- gsub("sun", "Sunday", period_graph_names)
period_graph_names <- gsub("sat", "Saturday", period_graph_names)
period_graph_names <- gsub("weekday", "Weekday", period_graph_names)
period_graph_names <- gsub("daytime avg", "Weekday daytime average", period_graph_names)

period_graph_names_max <- gsub("Trips per hour:", "Pct of max trips per hour:", period_graph_names)

p = 16

  bustrips_summary <- bustrips_summary %>%
    filter(period_name == period_cols[p]) %>%
    mutate(Location = RGN20NM)

  bustrips_summary_nolondon <- bustrips_summary %>%
    filter(Location != "London")

  bustrips_summary_london <- bustrips_summary %>%
    filter(Location == "London")

  order_of_facets <- c("North East",
                       "Yorkshire and The Humber",
                       "Greater London",
                       "Scotland",
                       "North West",
                       "London Blank",
                       "West Midlands",
                       "East Midlands",
                       "South West",
                       "Wales",
                       "East of England",
                       "South East")


  bustrips_summary_nolondon$Location = factor(bustrips_summary_nolondon$Location,
                                          levels = order_of_facets)

  useLegend = FALSE

   g1 <- ggplot(data = bustrips_summary_nolondon, aes(x = year, y = runs_cleaned, col = Location)) +
      geom_line(show.legend = useLegend, linewidth = 1) +
      geom_point(show.legend = useLegend) +
      ylim(c(0,NA)) +
      facet_wrap(. ~ Location, nrow = 4) +
      ylab(period_graph_names[p]) +
      xlab("Year") +
      theme_bw()

    g2 <- ggplot(data = bustrips_summary_nolondon, aes(x = year, y = tph_pct_max, col = Location)) +
      geom_line(show.legend = useLegend, linewidth = 1) +
      geom_point(show.legend = useLegend) +
      ylim(c(0,1)) +
      facet_wrap(. ~ Location, nrow = 4) +
      ylab(period_graph_names_max[p]) +
      xlab("Year") +
      theme_bw()

    g1_london <- ggplot(data = bustrips_summary_london, aes(x = year, y = runs_cleaned, col = Location)) +
      geom_line(show.legend = useLegend, linewidth = 1) +
      geom_point(show.legend = useLegend) +
      ylim(c(0,600)) +
      ylab("") +
      xlab("Year") +
      theme_bw()

    g2_london <- ggplot(data = bustrips_summary_london, aes(x = year, y = tph_pct_max, col = Location)) +
      geom_line(show.legend = useLegend, linewidth = 1) +
      geom_point(show.legend = useLegend) +
      ylim(c(0,1)) +
      ylab(period_graph_names_max[p]) +
      xlab("Year") +
      theme_bw()

    plot(g1)
    plot(g2)
    plot(g1_london)
    plot(g2_london)



    geog = "region"

    plotHeight = 9
    plotWidth = 7.3
    plotHeightLondon = 4.4
    plotWidthLondon = 2.5

    gg_filename <- paste0("plots/aug-23/outside-london/", period_cols[p], ".png")
    gg_filename_max <- paste0("plots/aug-23/outside-london/", period_cols[p], "_pct_max.png")

    gg_filename_london <- paste0("plots/aug-23/london/", period_cols[p], ".png")
    gg_filename_max_london <- paste0("plots/aug-23/london/", period_cols[p], "_pct_max.png")

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

    message(paste0("saving: ", gg_filename_london, "..."))
    ggsave(filename = gg_filename_london,
           plot = g1_london,
           height = plotHeightLondon,
           width = plotWidthLondon)

    message(paste0("saving: ", gg_filename_max_london, "..."))
    ggsave(filename = gg_filename_max_london,
           plot = g2_london,
           height = plotHeightLondon,
           width = plotWidthLondon)
