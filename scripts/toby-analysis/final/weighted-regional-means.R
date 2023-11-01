la_bustrips <- readRDS("data/la_bustrips_2005_23_cleaned.rds") # reads in from previously created
la_bustrips_regions <- la_bustrips %>%
  filter(period_name == "tph_daytime_avg") %>%
  filter(year %in% c(2006, 2007, 2008, 2010, 2023)) %>%
  mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "2006_2008",
                               year == 2010 ~ "2010",
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
            mean_tph = mean(runs, na.rm = TRUE),
            ) %>%
  ungroup()


#================================================================================

lsoa_bustrips_cleaned <- readRDS("data/bustrips_lsoa_2004_2023_cleaned.rds")

lsoa_bustrips <- lsoa_bustrips_cleaned %>%
  filter(period_name == "tph_daytime_avg") %>%
  filter(year %in% c(2006, 2007, 2008, 2010, 2023)) %>%
  mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "tph_2006_2008",
                               year == 2010 ~ "tph_2010",
                               year == 2023 ~ "tph_2023")) %>%
  group_by(lsoa11,
           year_band) %>%
  summarise(runs = mean(runs, na.rm = TRUE),
            runs_cleaned = mean(runs_cleaned, na.rm = TRUE)) %>%
  ungroup()




# get lsoa to pcon lookup
# lsoa_to_pcon <- make_lsoa_to_pcon_lup()
#
# test <- lsoa_to_pcon %>%
#   group_by(lsoa11) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   filter(n != 1)




# LSOA TO NEW PCON LOOKUP -------------------------------------------------



pcon_bustrips_23 <- Reduce(function(x, y) left_join(x, y, by = "lsoa11"),
                           list(lsoa_bustrips,
                                pop_lsoa,
                                lsoa_to_pcon23_lup))

pcon_bustrips_23 <- pcon_bustrips_23 %>%
  filter(!grepl("^S", lsoa11)) %>% # remove scotland data...
  group_by(parliamentary_constituency_2023,
           year_band) %>%
  summarise(wtdmean = weighted.mean(runs_cleaned, population, na.rm = TRUE),
            mean = mean(runs_cleaned, na.rm = TRUE)
            #wtdmean_tph = weighted.mean(runs, population, na.rm = TRUE),
            #mean_tph = mean(runs, na.rm = TRUE),
  ) %>%
  ungroup()

pcon_bustrips_23 <- pcon_bustrips_23 %>%
  gather(key = indicator,
         value = val,
         -parliamentary_constituency_2023,
         -year_band) %>%
  unite(full_indicator, year_band, indicator, sep = "_") %>%
  spread(key = full_indicator,
         value = val,
         fill = 0)


pcon_bustrips_23 <- pcon_bustrips_23 %>%
  mutate(tph0823_wtd_change = round(tph_2023_wtdmean - tph_2006_2008_wtdmean, 2),
         tph1023_wtd_change = round(tph_2023_wtdmean - tph_2010_wtdmean, 2)) %>%
  mutate(tph0823_wtd_change_pct = round(tph0823_wtd_change / tph_2006_2008_wtdmean, 4),
         tph1023_wtd_change_pct = round(tph1023_wtd_change / tph_2010_wtdmean, 4)) %>%
  mutate(tph_2006_2008_mean = round(tph_2006_2008_mean, 2),
         tph_2006_2008_wtdmean = round(tph_2006_2008_wtdmean, 2),
         tph_2010_mean = round(tph_2010_mean, 2),
         tph_2010_wtdmean = round(tph_2010_wtdmean, 2),
         tph_2023_mean = round(tph_2023_mean, 2),
         tph_2023_wtdmean = round(tph_2023_wtdmean, 2)) %>%
  mutate(tph0823_wtd_change_percent = tph0823_wtd_change_pct * 100)


source("../constituency-environmental-reports/scripts/geography-lookups.R")
pcon23_boundaries <- get_pcon_boundaries_2023()

pcon23_boundaries <- pcon23_boundaries %>%
  select(parliamentary_constituency_2023 = Constituen,
         Region,
         Sub.region)

pcon_bustrips_23 <- left_join(pcon23_boundaries, pcon_bustrips_23, by = "parliamentary_constituency_2023")

# get region boundaries
region_boundaries <- make_regions_sf(detolerate = TRUE,
                                     detolerate_value = 200)

pcon_graph <- tm_shape(pcon_bustrips_23) +
  tm_polygons(col = "tph0823_wtd_change_percent",
              title = "Weekday daytime average\nChange (%)",
              palette = hcl.colors(10, palette = "RdYlBu"),
              border.alpha = 0.1,
              border.col = "white",
              breaks = c(-100,-75,-50,-25,0,10,Inf),
              midpoint = NA,
              colorNA = NULL,
              textNA = "") +
  tm_shape(region_boundaries) +
  tm_borders(col = "black",
             lwd = 0.6) +
  tm_layout(frame = FALSE,
            #legend.frame = "black",
            legend.position = c(0.65,0.8))

if(!dir.exists("plots/aug-23/pcon")) {
  dir.create("plots/aug-23/pcon")
}

t_map_filename = "plots/aug-23/pcon/weekday-daytime-avg-change-pct.png"

tmap_save(tm = pcon_graph,
          filename = t_map_filename,
          dpi = 600)

# length(unique(lsoa_to_pcon23_lup$parliamentary_constituency_2023))
# length(unique(pcon_bustrips_23$parliamentary_constituency_2023))
