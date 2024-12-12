lsoa_bustrips <- classify_service_quality(lsoa_bustrips)




# find palettes here:
cols4all::c4a_gui()

table(lsoa_bustrips$rurality,
      lsoa_bustrips$tph_daytime_avg_service)




# use earlier time table data to look at what was good --------------------
# derive metric from previously good year.
# find a year. check gaps (i.e. count LSOAs with missing data, as well as total routes)

# #lsoa_bustrips_2006 <- load_lsoa_bustrips(onspd, year_list = 2006)
# lsoa_bustrips_2007 <- load_lsoa_bustrips(onspd, year_list = 2007)
# lsoa_bustrips_2008 <- load_lsoa_bustrips(onspd, year_list = 2008)
# lsoa_bustrips_2009 <- load_lsoa_bustrips(onspd, year_list = 2009)
# lsoa_bustrips_2010 <- load_lsoa_bustrips(onspd, year_list = 2010)
# #lsoa_bustrips_2011 <- load_lsoa_bustrips(onspd, year_list = 2011)
#
# #table(is.na(lsoa_bustrips_2006$tph_weekday_Morning_Peak))
# table(is.na(lsoa_bustrips_2007$tph_weekday_Morning_Peak))
# table(is.na(lsoa_bustrips_2008$tph_weekday_Morning_Peak))
# table(is.na(lsoa_bustrips_2009$tph_weekday_Morning_Peak))
# table(is.na(lsoa_bustrips_2010$tph_weekday_Morning_Peak))
# #table(is.na(lsoa_bustrips_2011$tph_weekday_Morning_Peak))
#
# average_trips <- function(lsoa_bustrips, tph_var) {
#   lsoa_bustrips %>%
#     summarise(mean({{ tph_var }}, na.rm = TRUE))
# }
#
# #average_trips(lsoa_bustrips_2006, tph_daytime_avg)
# average_trips(lsoa_bustrips_2007, tph_daytime_avg)
# average_trips(lsoa_bustrips_2008, tph_daytime_avg)
# average_trips(lsoa_bustrips_2009, tph_daytime_avg)
# average_trips(lsoa_bustrips_2010, tph_daytime_avg)
# #average_trips(lsoa_bustrips_2011, tph_daytime_avg)

# pick 2007-2010 and average, based on only 20-30 missing LSOAS and similar TPH values for morning peak and daytime average

lsoa_bustrips_20070_10 <- load_lsoa_bustrips(onspd, year_list = c(2007, 2008, 2009, 2010))
lsoa_bustrips_20070_10_long <- lsoa_bustrips_20070_10 %>%
  select(-route_type,
         -year,
         -year,
         -ru11ind,
         -urban_rural_cat,
         -max_number_routes) %>%
  gather(key = time_period,
         value = tph,
         -lsoa11,
         -rurality) %>%
  mutate(time_period = gsub("tph_", "", time_period)) %>%
  mutate(time_period = gsub("_", " ", time_period)) %>%
  mutate(time_period = paste0(toupper(substring(time_period, 1, 1)), tolower(substring(time_period, 2))))

table(lsoa_bustrips_20070_10_long$time_period)

lsoa_bustrips_20070_10_long$time_period <- factor(lsoa_bustrips_20070_10_long$time_period,
                                                  levels = c("Weekday morning peak",
                                                             "Weekday midday",
                                                             "Weekday afternoon peak",
                                                             "Weekday evening",
                                                             "Weekday night",
                                                             "Sat morning peak",
                                                             "Sat midday",
                                                             "Sat afternoon peak",
                                                             "Sat evening",
                                                             "Sat night",
                                                             "Sun morning peak",
                                                             "Sun midday",
                                                             "Sun afternoon peak",
                                                             "Sun evening",
                                                             "Sun night",
                                                             "Daytime avg"))

lsoa_bustrips_20070_10_long_spilt <- split(lsoa_bustrips_20070_10_long, lsoa_bustrips_20070_10_long$rurality)

lsoa_bustrips_20070_10_village <- lsoa_bustrips_20070_10_long_spilt$`Rural: Village/Hamlets/Isolated Dwellings`
lsoa_bustrips_20070_10_town <- lsoa_bustrips_20070_10_long_spilt$`Rural: Town and Fringe`
lsoa_bustrips_20070_10_city <- lsoa_bustrips_20070_10_long_spilt$`Urban: City and Town`
lsoa_bustrips_20070_10_conurbation <- lsoa_bustrips_20070_10_long_spilt$`Urban: Conurbation`

ggplot(data = lsoa_bustrips_20070_10_village, aes(x = tph, fill = time_period)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(. ~ time_period, scales = "free", nrow = 4, ncol = 5)

ggplot(data = lsoa_bustrips_20070_10_town, aes(x = tph, fill = time_period)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(. ~ time_period, scales = "free", nrow = 4, ncol = 5)

ggplot(data = lsoa_bustrips_20070_10_city, aes(x = tph, fill = time_period)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(. ~ time_period, scales = "free", nrow = 4, ncol = 5)

ggplot(data = lsoa_bustrips_20070_10_conurbation, aes(x = tph, fill = time_period)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(. ~ time_period, scales = "free", nrow = 4, ncol = 5)


lsoa_bustrips_20070_10_stats <- lsoa_bustrips_20070_10_long %>%
  filter(!is.na(rurality)) %>%
  group_by(rurality,
           time_period) %>%
  summarise(tph_5th_ptile = round(quantile(tph, probs = 0.05, na.rm = TRUE), 1),
            tph_25th_ptile = round(quantile(tph, probs = 0.25, na.rm = TRUE), 1),
            tph_33rd_ptile = round(quantile(tph, probs = 0.33, na.rm = TRUE), 1),
            tph_50th_ptile = round(quantile(tph, probs = 0.5, na.rm = TRUE), 1),
            tph_67th_ptile = round(quantile(tph, probs = 0.67, na.rm = TRUE), 1),
            tph_75th_ptile = round(quantile(tph, probs = 0.75, na.rm = TRUE), 1),
            tph_95th_ptile = round(quantile(tph, probs = 0.95, na.rm = TRUE), 1)) %>%
  ungroup()

lsoa_bustrips_20070_10_quintiles <- lsoa_bustrips_20070_10_long %>%
  filter(!is.na(rurality)) %>%
  group_by(rurality,
           time_period) %>%
  summarise(tph_20th_ptile = round(quantile(tph, probs = 0.2, na.rm = TRUE), 1),
            tph_40th_ptile = round(quantile(tph, probs = 0.4, na.rm = TRUE), 1),
            tph_60th_ptile = round(quantile(tph, probs = 0.6, na.rm = TRUE), 1),
            tph_80th_ptile = round(quantile(tph, probs = 0.8, na.rm = TRUE), 1)) %>%
  ungroup()


dir.create("outputs")
dir.create("outputs/november-24/")

write.csv(lsoa_bustrips_20070_10_stats,
          "outputs/november-24/bustrips-rural-urban-tph-summary-stats.csv",
          row.names = FALSE,
          na = "")

write.csv(lsoa_bustrips_20070_10_quintiles,
          "outputs/november-24/bustrips-rural-urban-tph-quintile-stats.csv",
          row.names = FALSE,
          na = "")

# old <- function() {
#
#   tph_cols <- c("tph_weekday_Morning_Peak",
#                 "tph_weekday_Midday",
#                 "tph_weekday_Afternoon_Peak",
#                 "tph_weekday_Evening",
#                 "tph_weekday_Night",
#                 "tph_Sat_Morning_Peak",
#                 "tph_Sat_Midday",
#                 "tph_Sat_Afternoon_Peak",
#                 "tph_Sat_Evening",
#                 "tph_Sat_Night",
#                 "tph_Sun_Morning_Peak",
#                 "tph_Sun_Midday",
#                 "tph_Sun_Afternoon_Peak",
#                 "tph_Sun_Evening",
#                 "tph_Sun_Night",
#                 "tph_daytime_avg")
#
#   lsoa_bustrips_20070_10 <- lsoa_bustrips_20070_10 %>%
#     mutate(year_band = "2007-10") %>%
#     group_by(lsoa11,
#              rurality) %>%
#     summarise(across(all_of(tph_cols), \(x) mean(x, na.rm = TRUE))) %>%
#     ungroup()
#
#   # find average, median and 33rd/66th percentiles for each one.
#
#   lsoa_bustrips_20070_10_mean <- lsoa_bustrips_20070_10 %>%
#     group_by(rurality) %>%
#     summarise(across(all_of(tph_cols), \(x) round(mean(x, na.rm = TRUE), 1))) %>%
#     ungroup() %>%
#     mutate(stat = "Mean")
#
#   lsoa_bustrips_20070_10_median <- lsoa_bustrips_20070_10 %>%
#     group_by(rurality) %>%
#     summarise(across(all_of(tph_cols), \(x) round(quantile(x, probs = 0.5, na.rm = TRUE), 1))) %>%
#     ungroup() %>%
#     mutate(stat = "Median")
#
#   lsoa_bustrips_20070_10_low_perc <- lsoa_bustrips_20070_10 %>%
#     group_by(rurality) %>%
#     summarise(across(all_of(tph_cols), \(x) round(quantile(x, probs = 0.25, na.rm = TRUE), 1))) %>%
#     ungroup() %>%
#     mutate(stat = "Lower percentile")
#
#   lsoa_bustrips_20070_10_high_perc <- lsoa_bustrips_20070_10 %>%
#     group_by(rurality) %>%
#     summarise(across(all_of(tph_cols), \(x) round(quantile(x, probs = 0.75, na.rm = TRUE), 1))) %>%
#     ungroup() %>%
#     mutate(stat = "Upper percentile")
#
#
#   lsoa_bustrips_20070_10_stats <- bind_rows(lsoa_bustrips_20070_10_low_perc,
#                                             #lsoa_bustrips_20070_10_mean,
#                                             lsoa_bustrips_20070_10_median,
#                                             lsoa_bustrips_20070_10_high_perc)
#
#   lsoa_bustrips_20070_10_stats <- lsoa_bustrips_20070_10_stats %>%
#     select(rurality,
#            stat,
#            everything()) %>%
#     arrange(rurality,stat) %>%
#     filter(!is.na(rurality))
#
# }
