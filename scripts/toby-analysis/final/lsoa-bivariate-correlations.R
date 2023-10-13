# compare bivariate data with other socio-economic data
# use environ data for change scripts for this

source("../environmental-data-for-change/scripts/useful-functions.R")
source("../environmental-data-for-change/scripts/ethnicity/ethnicity-analysis.R")
source("../environmental-data-for-change/scripts/health/health.R")
source("../environmental-data-for-change/scripts/imd/imd-analysis.R")
source("../environmental-data-for-change/scripts/transport/car-ownership.R")
source("../environmental-data-for-change/scripts/general/lsoa-rurality.R")

lsoa_bustrips_bivariate <- lsoa_bustrips_bivariate_analysis(threshold_3 = 0.15,
                                                            threshold_2 = -0.15,
                                                            label_1 = "Worse",
                                                            label_2 = "Same",
                                                            label_3 = "Better")

periods <- c("tph_daytime_avg",
             "tph_Sat_Afternoon_Peak",
             "tph_Sat_Evening",
             "tph_Sat_Midday",
             "tph_Sat_Morning_Peak",
             "tph_Sat_Night",
             "tph_Sun_Afternoon_Peak",
             "tph_Sun_Evening",
             "tph_Sun_Midday",
             "tph_Sun_Morning_Peak",
             "tph_Sun_Night",
             "tph_weekday_Afternoon_Peak",
             "tph_weekday_Evening",
             "tph_weekday_Midday",
             "tph_weekday_Morning_Peak",
             "tph_weekday_Night")

lsoa_bustrips_bivariate_avg <- lsoa_bustrips_bivariate %>%
  filter.for.england.wales(lsoa11) %>%
  filter(period_name == "tph_weekday_Morning_Peak") %>%
  mutate(london = ifelse(region_name == "London", "london", "not_london"))

lsoa_bustrips_bivariate_avg <- add.disability.data.to.lsoa(lsoa_bustrips_bivariate_avg)
lsoa_bustrips_bivariate_avg <- add.ethnicity.to.lsoa.dataset(lsoa_bustrips_bivariate_avg, rename_lsoa_code = TRUE)
lsoa_bustrips_bivariate_avg <- add.imd.income.domain.only(lsoa_bustrips_bivariate_avg)
lsoa_bustrips_bivariate_avg <- add.noncar.owners.to.lsoa.dataset(lsoa_bustrips_bivariate_avg, rename_lsoa_code = TRUE)
lsoa_bustrips_bivariate_avg <- add.rurality.to.lsoa.dataset(lsoa_bustrips_bivariate_avg, rename_lsoa_code = TRUE)

# simplify rurality
#table(lsoa_bustrips_bivariate_avg$RUC11)
lsoa_bustrips_bivariate_avg <- lsoa_bustrips_bivariate_avg %>%
  mutate(urban_rural = case_when(grepl("Urban", RUC11, ignore.case = TRUE) ~ "Urban",
                                 grepl("Rural", RUC11, ignore.case = TRUE) ~ "Rural"))

table(lsoa_bustrips_bivariate_avg$RUC11,
      lsoa_bustrips_bivariate_avg$urban_rural,
      useNA = "ifany")

# remove london
split_bivariate <- split(lsoa_bustrips_bivariate_avg, lsoa_bustrips_bivariate_avg$london)
nonlondon_bivariate <- split_bivariate$not_london
london_bivariate <- split_bivariate$london

table(`2008` = nonlondon_bivariate$service_frequency_2008_label,
      `2023` = nonlondon_bivariate$service_frequency_2023_label)

table(nonlondon_bivariate$region_name,
      nonlondon_bivariate$london_tube)

correlation_agg <- function(.data, ...) {
  aggregation <- .data %>%
    group_by(...) %>%
    summarise(lsoa_count = n(),
              average_tph_2006_08 = mean(tph_2006_08),
              average_tph_2023 = mean(tph_2023)) %>%
    ungroup() %>%
    mutate(tph_change = average_tph_2023 - average_tph_2006_08) %>%
    mutate(tph_change_pct = tph_change / average_tph_2006_08)
}

test <- correlation_agg(nonlondon_bivariate,
                        urban_rural,
                        imd_income_domain_decile)

# group by level of service in 2023
imd_1a <- correlation_agg(nonlondon_bivariate,
                         service_frequency_2023,
                         service_frequency_2023_label,
                         imd_income_domain_decile)

imd_1b <- correlation_agg(nonlondon_bivariate,
                          urban_rural,
                          service_frequency_2023,
                          service_frequency_2023_label,
                          imd_income_domain_decile)

imd_2a <- correlation_agg(nonlondon_bivariate,
                          service_reduction_2008_23,
                          service_reduction_2008_23_label,
                          imd_income_domain_decile)

imd_2a <- imd_2a %>%
  group_by(imd_income_domain_decile) %>%
  mutate(lsoa_pct = lsoa_count / sum(lsoa_count)) %>%
  ungroup()

imd_2b <- correlation_agg(nonlondon_bivariate,
                          urban_rural,
                          service_reduction_2008_23,
                          service_reduction_2008_23_label,
                          imd_income_domain_decile)

imd_2b <- imd_2b %>%
  group_by(imd_income_domain_decile) %>%
  mutate(lsoa_pct = lsoa_count / sum(lsoa_count)) %>%
  ungroup()

imd_3 <- correlation_agg(nonlondon_bivariate,
                         trips_bi_variate,
                         trips_bi_variate_label,
                         imd_income_domain_decile)

imd_3 <- imd_3 %>%
  group_by(imd_income_domain_decile) %>%
  mutate(lsoa_pct = lsoa_count / sum(lsoa_count)) %>%
  ungroup()


disability_agg <- function(.data,
                           ...) {

  disability_1 <- .data %>%
    group_by(...) %>%
    summarise(population = sum(census_population),
              disability_n = sum(disability_n),
              disability_pct = sum(disability_n) / sum(census_population),
              average_tph_2006_08 = mean(tph_2006_08),
              average_tph_2023 = mean(tph_2023)) %>%
    ungroup() %>%
    mutate(tph_change = average_tph_2023 - average_tph_2006_08) %>%
    mutate(tph_change_pct = tph_change / average_tph_2006_08)

}


ethnicity_agg <- function(.data,
                           ...) {

  disability_1 <- .data %>%
    group_by(...) %>%
    summarise(population = sum(census_population),
              ethnicity_bame_n = sum(ethnicity_bame),
              ethnicity_bame_pct = sum(ethnicity_bame) / sum(census_population),
              average_tph_2006_08 = mean(tph_2006_08),
              average_tph_2023 = mean(tph_2023)) %>%
    ungroup() %>%
    mutate(tph_change = average_tph_2023 - average_tph_2006_08) %>%
    mutate(tph_change_pct = tph_change / average_tph_2006_08)

}

carownership_agg <- function(.data,
                           ...) {

  carownership_1 <- .data %>%
    group_by(...) %>%
    summarise(population = sum(census_population),
              car_ownership_no_cars_n = sum(car_ownership_no_cars),
              car_ownership_no_cars_pct = sum(car_ownership_no_cars_n) / sum(census.households),
              average_tph_2006_08 = mean(tph_2006_08),
              average_tph_2023 = mean(tph_2023)) %>%
    ungroup() %>%
    mutate(tph_change = average_tph_2023 - average_tph_2006_08) %>%
    mutate(tph_change_pct = tph_change / average_tph_2006_08)

}

disability_1 <- disability_agg(nonlondon_bivariate,
                               service_frequency_2023,
                               service_frequency_2023_label)

disability_2 <- disability_agg(nonlondon_bivariate,
                               urban_rural,
                               service_reduction_2008_23,
                               service_reduction_2008_23_label)

ethnicity_1 <- ethnicity_agg(nonlondon_bivariate,
                             service_frequency_2023,
                             service_frequency_2023_label)

ethnicity_2 <- ethnicity_agg(nonlondon_bivariate,
                             urban_rural,
                             service_reduction_2008_23,
                             service_reduction_2008_23_label)

carownership_1 <- carownership_agg(nonlondon_bivariate,
                                   service_frequency_2023,
                                   service_frequency_2023_label)

carownership_2 <- carownership_agg(nonlondon_bivariate,
                                   urban_rural,
                                   service_reduction_2008_23,
                                   service_reduction_2008_23_label)

# London
disability_1_london <- disability_agg(london_bivariate,
                               service_frequency_2023,
                               service_frequency_2023_label)

disability_2_london <- disability_agg(london_bivariate,
                               service_reduction_2008_23,
                               service_reduction_2008_23_label)

ethnicity_1_london <- ethnicity_agg(london_bivariate,
                             service_frequency_2023,
                             service_frequency_2023_label)

ethnicity_2_london <- ethnicity_agg(london_bivariate,
                             service_reduction_2008_23,
                             service_reduction_2008_23_label)


# change in service good to poor details and counts -----------------------

service_change_summary <- lsoa_bustrips_bivariate_avg %>%
  group_by(london,
           service_frequency_2008_label,
           service_frequency_2023_label) %>%
  summarise(`_2023_lsoa_count` = n()) %>%
  group_by(london) %>%
  mutate(`_2023_lsoa_pct` = `_2023_lsoa_count` / sum(`_2023_lsoa_count`)) %>%
  ungroup()

service_change_summary <- service_change_summary %>%
  gather(key = indicator,
         value = val,
         -london,
         -service_frequency_2008_label,
         -service_frequency_2023_label) %>%
  unite(indicator_2023, service_frequency_2023_label, indicator, sep = "") %>%
  spread(key = indicator_2023,
         value = val,
         fill = 0)

write.csv(service_change_summary,
          file = "plots/aug-23/bivariate/service-levels-2008-2023.csv",
          row.names = FALSE,
          na="")
