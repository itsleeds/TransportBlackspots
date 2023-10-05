la_bustrips <- readRDS("data/la_bustrips_2005_23_cleaned.rds") # reads in from previously created
la_bustrips_regions <- la_bustrips %>%
  filter(period_name == "tph_daytime_avg") %>%
  filter(year %in% c(2006, 2007, 2008, 2023)) %>%
  mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "2006_2008",
                               year == 2023 ~ "2023")) %>%
  group_by(LAD23CD,
           LAD23NM,
           RGN20CD,
           RGN20NM,
           year_band) %>%
  summarise(runs = mean(runs, na.rm = TRUE),
            runs_cleaned = mean(runs_cleaned, na.rm = TRUE)) %>%
  ungroup()


# get la population
pop_la <- read.xlsx("data/la/updatedabpe27062023.xlsx",
                    sheet = "Population")

pop_la <- pop_la %>%
  filter(time == max(time)) %>%
  rename(oslaua = ladcode21,
         local_authority_name = laname_21) %>%
  la_consolidation_april_2023() %>%
  group_by(oslaua,
           local_authority_name) %>%
  summarise(population = round(sum(mean))) %>%
  ungroup()

la_bustrips_regions <- left_join(la_bustrips_regions, pop_la, by = c("LAD23CD" = "oslaua"))

bustrips_regions <- la_bustrips_regions %>%
  group_by(RGN20CD,
           RGN20NM,
           year_band) %>%
  summarise(cleaned_wtdmean_tph = weighted.mean(runs_cleaned, population, na.rm = TRUE),
            cleaned_mean_tph = mean(runs_cleaned, na.rm = TRUE),
            wtdmean_tph = weighted.mean(runs, population, na.rm = TRUE),
            mean_tph = mean(runs, na.rm = TRUE)) %>%
  ungroup()
