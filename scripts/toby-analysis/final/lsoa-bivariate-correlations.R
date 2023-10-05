# compare bivariate data with other socio-economic data
# use environ data for change scripts for this

source("../environmental-data-for-change/scripts/useful-functions.R")
source("../environmental-data-for-change/scripts/ethnicity/ethnicity-analysis.R")
source("../environmental-data-for-change/scripts/health/health.R")
source("../environmental-data-for-change/scripts/imd/imd-analysis.R")
source("../environmental-data-for-change/scripts/transport/car-ownership.R")

lsoa_bustrips_bivariate_avg <- lsoa_bustrips_bivariate %>%
  filter.for.england.wales(lsoa11) %>%
  filter(period_name == "tph_daytime_avg") %>%
  mutate(london = ifelse(region_name == "London", "london", "not_london"))

lsoa_bustrips_bivariate_avg <- add.disability.data.to.lsoa(lsoa_bustrips_bivariate_avg)
lsoa_bustrips_bivariate_avg <- add.ethnicity.to.lsoa.dataset(lsoa_bustrips_bivariate_avg, rename_lsoa_code = TRUE)
lsoa_bustrips_bivariate_avg <- add.imd.income.domain.only(lsoa_bustrips_bivariate_avg)
lsoa_bustrips_bivariate_avg <- add.noncar.owners.to.lsoa.dataset(lsoa_bustrips_bivariate_avg, rename_lsoa_code = TRUE)

# remove london
split_bivariate <- split(lsoa_bustrips_bivariate_avg, lsoa_bustrips_bivariate_avg$london)
nonlondon_bivariate <- split_bivariate$not_london

# group by level of service in 2023
imd_1 <- nonlondon_bivariate %>%
  group_by(service_frequency_2023,
           service_frequency_2023_label,
           imd_decile) %>%
  summarise(lsoa_count = n())

imd_2 <- nonlondon_bivariate %>%
  group_by(service_reduction_2008_23,
           service_reduction_2008_23_label,
           imd_decile) %>%
  summarise(lsoa_count = n())

disability_1 <- nonlondon_bivariate %>%
  group_by(service_frequency_2023,
           service_frequency_2023_label) %>%
  summarise(disability_n = sum(disability_n),
            disability_pct = sum(disability_n) / sum(census_population)) %>%
  ungroup()

disability_2 <- nonlondon_bivariate %>%
  group_by(service_reduction_2008_23,
           service_reduction_2008_23_label) %>%
  summarise(avg_change = mean(tph_2006_2023_change_pct),
            disability_n = sum(disability_n),
            disability_pct = sum(disability_n) / sum(census_population)) %>%
  ungroup()
