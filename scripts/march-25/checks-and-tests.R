lsoa_bus_trends_23_24 <- calculate_lsoa_bus_2010_2024_trends(lsoa21_buses_2023,
                                                             lsoa21_buses_2024,
                                                             time_period_name = "weekday_dayevening")

lsoa_bus_trends_22_23 <- calculate_lsoa_bus_2010_2024_trends(lsoa21_buses_2022,
                                                             lsoa21_buses_2023,
                                                             time_period_name = "weekday_dayevening")


lsoa_bus_trends_10_23 <- calculate_lsoa_bus_2010_2024_trends(lsoa21_buses_2010,
                                                             lsoa21_buses_2023,
                                                             time_period_name = "weekday_daytime")


lsoa_bus_trends_23_24 <- lsoa_bus_trends_23_24 %>%
  rename(tph_2023 = tph_2010,
         tph_2024 = tph_2024,
         `Change in TPH (2023-24)` = tph1024_chg,
         `Change in TPH (2023-24) %` = tph1024_chg_pct)

lsoa_bus_trends_22_23 <- lsoa_bus_trends_22_23 %>%
  rename(tph_2022 = tph_2010,
         tph_2023 = tph_2024,
         `Change in TPH (2022-23)` = tph1024_chg,
         `Change in TPH (2022-23) %` = tph1024_chg_pct)

lsoa_bus_trends_10_23 <- lsoa_bus_trends_10_23 %>%
  rename(tph_2023 = tph_2010,
         tph2324_chg = tph1024_chg,
         tph2324_chg_pct = tph1024_chg_pct)

tmap_mode("view")
tmap_mode("plot")
LSOA_bus_trends_22_23 <- make_map_of_bus_service_lsoa21(lsoa_bus_trends_22_23, "Change in TPH (2022-23) %", type = "scale", convert_to_percent = TRUE, popup_var_list = c("lsoa21",
                                                                                                                                                                          "tph_2022",
                                                                                                                                                                          "tph_2023",
                                                                                                                                                                          "Change in TPH (2022-23)",
                                                                                                                                                                          "Change in TPH (2022-23) %"))

LSOA_bus_trends_23_24 <- make_map_of_bus_service_lsoa21(lsoa_bus_trends_23_24, "Change in TPH (2023-24) %", type = "scale", convert_to_percent = TRUE, popup_var_list = c("lsoa21",
                                                                                                                                                                          "tph_2023",
                                                                                                                                                                          "tph_2024",
                                                                                                                                                                          "Change in TPH (2023-24)",
                                                                                                                                                                          "Change in TPH (2023-24) %"))

tmap_save(LSOA_bus_trends_22_23, filename = "outputs/march-25/plots/LSOA-bus0-trends-2022-2023.png")
tmap_save(LSOA_bus_trends_23_24, filename = "outputs/march-25/plots/LSOA-bus0-trends-2023-2024.png")



#  explore each day of the weekday to see if anything stands out ----------

filter_for_weekday_timeperiod_test <- function(lsoa21_buses, period_name = "Morning_Peak") {

  lsoa21_buses_long <- make_bustrips_long(lsoa21_buses)

  lsoa21_buses_weekday <- lsoa21_buses_long %>%
    filter(grepl("Mon|Tue|Wed|Thu|Fri", time_period) & grepl(period_name, time_period))

}

filter_for_timeperiod_test <- function(lsoa21_buses, period_name = "Morning_Peak") {

  lsoa21_buses_long <- make_bustrips_long(lsoa21_buses)

  lsoa21_buses_weekday <- lsoa21_buses_long %>%
    filter(grepl("Mon|Tue|Wed|Thu|Fri|Sat|Sun", time_period) & grepl(period_name, time_period))

}

calculate_change <- function(lsoa21_buses_1, lsoa21_buses_2, tph_1, tph_2, change_years) {

  # select tph values only and filter for an individual time periods
  # 2010
  lsoa21_buses_1 <- lsoa21_buses_1 %>%
    transmute(lsoa21,
              time_period,
              {{ tph_1 }} := as.numeric(tph))
  # 2024
  lsoa21_buses_2 <- lsoa21_buses_2 %>%
    transmute(lsoa21,
              time_period,
              {{ tph_2 }} := as.numeric(tph))

  # join data together
  lsoa21_buses_both <- inner_join(lsoa21_buses_1, lsoa21_buses_2, by = c("lsoa21", "time_period"))

  # then calculate the changes/trends as tph and %
  lsoa21_buses_both <- lsoa21_buses_both %>%
    mutate(tph_chg := round({{ tph_2 }} - {{ tph_1 }}, 2)) %>%
    mutate(tph_chg_pct := round(tph_chg / {{ tph_1 }}, 4))

  lsoa21_buses_both <- lsoa21_buses_both %>%
    mutate(tph_chg_pct = ifelse(tph_chg_pct == "Inf", 1, tph_chg_pct)) %>%
    mutate(tph_chg_pct = ifelse(tph_chg_pct == "NaN", NA, tph_chg_pct))

  lsoa21_buses_both <- lsoa21_buses_both %>%
    rename("tph_{{ change_years }}_chg" := tph_chg,
           "tph_{{ change_years }}_chg_pct" := tph_chg_pct)



}

lsoa21_buses_2010_all <- load_lsoa21_bustrips_alldays(data_year = 2010)
lsoa21_buses_2022_all <- load_lsoa21_bustrips_alldays(data_year = 2022)
lsoa21_buses_2023_all <- load_lsoa21_bustrips_alldays(data_year = 2023)
lsoa21_buses_2024_all <- load_lsoa21_bustrips_alldays(data_year = 2024)

lsoa21_buses_2010_weekday <- filter_for_weekday_timeperiod_test(lsoa21_buses_2010_all, period_name = "Peak|Midday")
lsoa21_buses_2022_weekday <- filter_for_weekday_timeperiod_test(lsoa21_buses_2022_all, period_name = "Peak|Midday")
lsoa21_buses_2023_weekday <- filter_for_weekday_timeperiod_test(lsoa21_buses_2023_all, period_name = "Peak|Midday")
lsoa21_buses_2024_weekday <- filter_for_weekday_timeperiod_test(lsoa21_buses_2024_all, period_name = "Peak|Midday")

lsoa21_buses_2010_alldays <- filter_for_timeperiod_test(lsoa21_buses_2010_all, period_name = "Peak|Midday|Evening")
lsoa21_buses_2022_alldays <- filter_for_timeperiod_test(lsoa21_buses_2022_all, period_name = "Peak|Midday|Evening")
lsoa21_buses_2023_alldays <- filter_for_timeperiod_test(lsoa21_buses_2023_all, period_name = "Peak|Midday|Evening")
lsoa21_buses_2024_alldays <- filter_for_timeperiod_test(lsoa21_buses_2024_all, period_name = "Peak|Midday|Evening")


lsoa21_buses_2023_24_change <- calculate_change(lsoa21_buses_2023_alldays,
                                                lsoa21_buses_2024_alldays,
                                                tph_2023,
                                                tph_2024,
                                                change_years = `23_24`)

lsoa21_buses_2022_23_change <- calculate_change(lsoa21_buses_2022_alldays,
                                                lsoa21_buses_2023_alldays,
                                                tph_2022,
                                                tph_2023,
                                                change_years = `22_23`)

lsoa21_buses_2010_23_change <- calculate_change(lsoa21_buses_2010_weekday,
                                                lsoa21_buses_2023_weekday,
                                                tph_2010,
                                                tph_2023,
                                                change_years = `10_23`)

lsoa21_buses_2010_24_change <- calculate_change(lsoa21_buses_2010_weekday,
                                                lsoa21_buses_2024_weekday,
                                                tph_2010,
                                                tph_2024,
                                                change_years = `10_24`)

lsoa21_buses_2023_24_change <- add_geography21(lsoa21_buses_2023_24_change)
lsoa21_buses_2022_23_change <- add_geography21(lsoa21_buses_2022_23_change)

lsoa21_buses_2023_24_change_summary <- lsoa21_buses_2023_24_change %>%
  group_by(region_name,
           time_period) %>%
  summarise(tph_2023 = mean(tph_2023, na.rm = TRUE),
            tph_2024 = mean(tph_2024, na.rm = TRUE),
            tph_23_24_chg = mean(tph_23_24_chg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(tph_23_24_chg_pct = round(tph_23_24_chg / tph_2023, 4))


lsoa21_buses_2022_23_change_summary <- lsoa21_buses_2022_23_change %>%
  group_by(region_name,
           time_period) %>%
  summarise(tph_2022 = mean(tph_2022, na.rm = TRUE),
            tph_2023 = mean(tph_2023, na.rm = TRUE),
            tph_22_23_chg = mean(tph_22_23_chg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(tph_22_23_chg_pct = round(tph_22_23_chg / tph_2022, 4))

hist(lsoa)


lsoa21_buses_2010 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2010, new = FALSE)
lsoa21_buses_2022 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2022, new = FALSE)
lsoa21_buses_2023 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2023, new = FALSE)
lsoa21_buses_2024 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024, new = FALSE)


headlines_22_23 <- make_rurality_comparison_2010_2024(bus_lsoa_2010 = lsoa21_buses_2022,
                                                      bus_lsoa_2024 = lsoa21_buses_2023,
                                                      time_period_filter = FALSE)

headlines_23_24 <- make_rurality_comparison_2010_2024(bus_lsoa_2010 = lsoa21_buses_2023,
                                                      bus_lsoa_2024 = lsoa21_buses_2024,
                                                      time_period_filter = FALSE)

t1_22_23 <- t1_regional_all_trends(lsoa21_buses_2022, lsoa21_buses_2023)
t1_23_24 <- t1_regional_all_trends(lsoa21_buses_2023, lsoa21_buses_2024)

rename_year_cols <- function(data, new_2010, new_2024) {

  old_names <- names(data)
  new_names <- gsub("2010", new_2010, old_names)
  new_names <- gsub("2024", new_2024, new_names)
  colnames(data) <- new_names

  return(data)

}

headlines_22_23 <- rename_year_cols(headlines_22_23, new_2010 = "2022", new_2024 = "2023")
headlines_23_24 <- rename_year_cols(headlines_23_24, new_2010 = "2023", new_2024 = "2024")

t1_22_23 <- rename_year_cols(t1_22_23, new_2010 = "2022", new_2024 = "2023")
t1_23_24 <- rename_year_cols(t1_23_24, new_2010 = "2023", new_2024 = "2024")

# send malcolm changes between 23-24 and 22-23
 # maps of 22-23 and 22-24 changes %
 # regional changes by weekday daytime
 # regional changes by weekday periods.
 # average changes by region by all time periods

source("../environmental-data-for-change/scripts/save-foe-workbook.R")

list_col_types <- function(data) {
  data.frame(col = 1:ncol(data), sapply(data, class))
}

list_col_types(headlines_22_23)
list_col_types(headlines_23_24)
list_col_types(t1_22_23)
list_col_types(t1_23_24)

save_as_spreadsheet_multiformat(number_of_tabs = 4,
                                tab1_data = headlines_22_23,
                                tab2_data = headlines_23_24,
                                tab3_data = t1_22_23,
                                tab4_data = t1_23_24,
                                tab1_name = "London-2022-23",
                                tab2_name = "London-2023-24",
                                tab3_name = "Regions-2022-23",
                                tab4_name = "Regions-2023-24",
                                percent_cols_1 = 8,
                                number_cols_1 = 5:7,
                                percent_cols_2 = 8,
                                number_cols_2 = 5:7,
                                percent_cols_3 = 3:8,
                                number_cols_3 = 0,
                                percent_cols_4 = 3:8,
                                number_cols_4 = 0,
                                xlsx_path = "outputs/march-25/Bus-data-comparisons-2022-2023-2024.xlsx",
                                percent_decimal = TRUE,
                                number_decimal = TRUE)


#  COMPARE BODS GTFS DATA IN 2023 WITH MALCOLM GTFS 2023 DATA -------------

lsoa21_buses_2023_bods <- make_simplified_bustrips_lsoa21_single_year(data_year = "BODS2023", new = TRUE)
lsoa21_buses_2023 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2023, new = FALSE)

lsoa21_buses_2023_long <- make_bustrips_long(lsoa21_buses_2023)
lsoa21_buses_2023_bods_long <- make_bustrips_long(lsoa21_buses_2023_bods)

lsoa21_buses_2023_long <- lsoa21_buses_2023_long %>%
  rename(tph_2023 = tph)
lsoa21_buses_2023_bods_long <- lsoa21_buses_2023_bods_long %>%
  mutate(tph_BODS2023 = tph) %>%
  select(lsoa21,
         time_period,
         tph_BODS2023)

lsoa21_2023_comparison <- inner_join(lsoa21_buses_2023_long, lsoa21_buses_2023_bods_long, by = c("lsoa21", "time_period"))

lsoa21_2023_comparison <- lsoa21_2023_comparison %>%
  mutate(diff = tph_BODS2023 - tph_2023) %>%
  mutate(diff_pct = diff / tph_2023) %>%
  mutate(diff_pct = ifelse(diff_pct == "Inf", NA, diff_pct))

lsoa_to_oslaua_lup <- get_lsoa21_to_oslaua_lup()
oslaua_to_rgn_lup <- make_oslaua_to_rgn_lup()

lsoa21_2023_comparison <- left_join(lsoa21_2023_comparison, lsoa_to_oslaua_lup, by = "lsoa21")
lsoa21_2023_comparison <- left_join(lsoa21_2023_comparison, oslaua_to_rgn_lup, by = "oslaua")

test <- lsoa21_2023_comparison %>%
  #filter(grepl("weekday", time_period) & !grepl("Night", time_period)) %>%
  filter(time_period == "weekday_daytime") %>%
  group_by(region_name,
           time_period) %>%
  summarise(mean_tph_2023 = round(mean(tph_2023, na.rm = TRUE), 2),
            mean_tph_bods_2023 = round(mean(tph_BODS2023, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(mean_diff = mean_tph_bods_2023 - mean_tph_2023) %>%
  mutate(mean_diff_pct = round(mean_diff / mean_tph_2023, 4))


region_test <- lsoa21_2023_comparison %>%
  group_by(region_name,
           time_period) %>%
  summarise(mean_tph_2023 = round(mean(tph_2023, na.rm = TRUE), 2),
            mean_tph_bods_2023 = round(mean(tph_BODS2023, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(mean_diff = mean_tph_bods_2023 - mean_tph_2023) %>%
  mutate(mean_diff_pct = round(mean_diff / mean_tph_2023, 4))

save_as_spreadsheet_multiformat(number_of_tabs = 1,
                                tab1_data = test,
                                tab2_data = region_test,
                                tab1_name = "BODS-comparison-weekday",
                                tab2_name = "BODS-comparison-all",
                                number_cols_1 = 3:5,
                                percent_cols_1 = 6,
                                number_cols_2 = 3:5,
                                percent_cols_2 = 6,
                                xlsx_path = "outputs/march-25/BODS-GTFS-comparison-summary.xlsx",
                                number_decimal = TRUE,
                                percent_decimal = TRUE)


table(is.na(lsoa21_2023_comparison_weekday_daytime$tph_BODS2023))
table(is.na(lsoa21_2023_comparison_weekday_daytime$tph_2023))
lsoa21_2023_comparison_weekday_daytime <- lsoa21_2023_comparison %>%
  filter(time_period == "weekday_daytime")

summary(lsoa21_2023_comparison_weekday_daytime$diff_pct)
summary(lsoa21_2023_comparison_weekday_daytime$diff)

BODS_map <- make_map_of_bus_service_lsoa21(lsoa21_2023_comparison_weekday_daytime, map_var = "diff_pct", type = "scale", convert_to_percent = TRUE)

tmap_save(BODS_map,
          filename = "outputs/march-25/map-csvs/BODS-GTFS-comparison-2023-data.png")

gtfs_bods_comp <- ggstatsplot::ggscatterstats(data = lsoa21_2023_comparison_weekday_daytime,
                                               x = tph_2023,
                                               y = tph_BODS2023,
                                               #ggtheme = theme_fo(),
                                               marginal = TRUE,
                                               results.subtitle = TRUE,
                                               bf.message = FALSE,
                                               point.args = list(size = 1.5, alpha = 0.2, stroke = 0, colour = "#1e234d"),
                                               smooth.line.args = list(linewidth = 1.5, color = "#ed6132", method = "lm", formula = y ~ x),
                                               xsidehistogram.args = list(fill = "#61bdaa", color = "#1e234d", na.rm = TRUE),
                                               ysidehistogram.args = list(fill = "#faef63", color = "#1e234d", na.rm = TRUE),
                                               xlab = "Original TPH 2023",
                                               ylab = "BODS TPH 2023")

corr_graph <- ggplot(lsoa21_2023_comparison_weekday_daytime, aes(x=tph_2023, y=tph_BODS2023)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

ggsave(plot = corr_graph,
       filename = "outputs/march-25/plots/BODS-correlation.png")

summary(lm(lsoa21_2023_comparison_weekday_daytime$tph_BODS2023 ~ lsoa21_2023_comparison_weekday_daytime$tph_2023))$coefficients

line(lsoa21_2023_comparison_weekday_daytime$tph_2023,
     lsoa21_2023_comparison_weekday_daytime$tph_BODS2023)

plot(lsoa21_2023_comparison_weekday_daytime$)


hist(lsoa21_2023_comparison$diff, breaks = 100)
