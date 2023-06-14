# clear environment
rm(list = ls())

# load functions
source("scripts/toby-analysis/lsoa-trips-analysis-functions.R")
source("scripts/toby-analysis/onspd.R")

# load packages
load_packages()

# get onspd
onspd <- get_onspd(keep.only.current = TRUE)
# make lsoa to LA lookup
#lsoa_to_oslaua <- make_lsoa_to_oslaua_lup()
oslaua_to_rgn <- make_oslaua_to_rgn_lup()


#  Prepare LA level data --------------------------------------------------

# get ONS area classification
#excel_sheets("../near-you-data-ingest/data/reference/clustermembershipv2.xls")
ons_class_la <- read_excel("../near-you-data-ingest/data/reference/clustermembershipv2.xls",
                           sheet = "Clusters by Local Authority",
                           skip = 9)

# remove NA rows
ons_class_la <- na.omit(ons_class_la)

ons_class_la <- ons_class_la %>%
  select(oslaua = Code,
         local_authority_name = Name,
         #supergroup_code = `Supergroup Code`,
         supergroup_name = `Supergroup Name`,
         #group_code = `Group Code`,
         group_name = `Group Name`,
         #subgroup_code = `Subgroup Code`,
         subgroup_name = `Subgroup Name`)

la_pop <- read_excel("../near-you-data-ingest/data/reference/ukmidyearestimates20182018ladcodes.xls",
                     sheet = "MYE3",
                     skip = 4)
la_pop <- la_pop %>%
  select(oslaua = Code,
         #local_authority_name = Name,
         pop = `Estimated Population  mid-2017`)

ons_class_la <- left_join(ons_class_la, la_pop, by = "oslaua")
ons_class_la <- la_consolidation_april_2023(ons_class_la)

ons_class_la_supergroup <- ons_class_la %>%
  group_by(oslaua,
           local_authority_name,
           supergroup_name) %>%
  summarise(pop = sum(pop)) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  select(-pop)

ons_class_la_group <- ons_class_la %>%
  group_by(oslaua,
           local_authority_name,
           group_name) %>%
  summarise(pop = sum(pop)) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  select(-pop)

ons_class_la_subgroup <- ons_class_la %>%
  group_by(oslaua,
           local_authority_name,
           subgroup_name) %>%
  summarise(pop = sum(pop)) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  select(-pop)

ons_class_la_2023 <- Reduce(function(x, y) left_join(x, y, by = c("oslaua", "local_authority_name")),
                            list(ons_class_la_supergroup,
                                 ons_class_la_group,
                                 ons_class_la_subgroup))

# load data
la_bustrips_2004_2023 <- readRDS("data/trips_per_la_by_mode_2004_2023.Rds") %>%
  filter(route_type == 3) %>%
  filter(!is.na(zone_id))

tph_cols <- names(la_bustrips_2004_2023)[grepl("tph", names(la_bustrips_2004_2023))]

la_bustrips_2004_2023 <- la_bustrips_2004_2023 %>%
  select(local_authority_name = zone_id,
         route_type,
         year,
         all_of(tph_cols))

la_bustrips_2004_2023 <- left_join(la_bustrips_2004_2023, ons_class_la, by = "local_authority_name")

# complete Westminster/city of london entries...
la_bustrips_2004_2023 <- la_bustrips_2004_2023 %>%
  mutate(supergroup_name = ifelse(local_authority_name %in% c("City of London", "Westminster"), "London Cosmopolitan", supergroup_name)) %>%
  mutate(group_name = ifelse(local_authority_name %in% c("City of London", "Westminster"), "London Cosmopolitan", group_name)) %>%
  mutate(subgroup_name = ifelse(local_authority_name %in% c("City of London", "Westminster"), "London Cosmopolitan", subgroup_name))

# Add region
oslaua_to_rgn <- make_oslaua_to_rgn_lup()
la_bustrips_2004_2023 <- left_join(la_bustrips_2004_2023, oslaua_to_rgn, by = "oslaua")

la_bustrips_2004_2023 <- la_bustrips_2004_2023 %>%
  mutate(region_name = case_when(local_authority_name == "City of London" ~ "London",
                                 local_authority_name == "Somerset" ~ "South West",
                                 local_authority_name == "Westminster" ~ "London",
                                 local_authority_name == "Westmorland and Furness" ~ "North West",
                                 local_authority_name == "Cumberland" ~ "North West",
                                 local_authority_name == "North Yorkshire" ~ "Yorkshire and The Humber",
                                 local_authority_name == "Fife" ~ "Scotland",
                                 local_authority_name == "Perth and Kinross" ~ "Scotland",
                                 local_authority_name == "Glasgow City" ~ "Scotland",
                                 local_authority_name == "North Lanarkshire" ~ "Scotland",
                                 TRUE ~ region_name))

# la_bustrips_2004_2023 %>%
#   filter(is.na(region_name)) %>%
#   distinct(local_authority_name)

# table(la_bustrips_2004_2023$region_name, useNA = "ifany")


median_with_round <- function(x) round(median(x, na.rm = TRUE), 2)

la_bustrips_by_ons <- la_bustrips_2004_2023 %>%
  group_by(group_name,
           year) %>%
  summarise(across(all_of(tph_cols), median_with_round)) %>%
  ungroup() %>%
  mutate(group_name = factor(group_name,
                             levels = c("Rural-Urban Fringe",
                                        "Thriving Rural",
                                        "Larger Towns and Cities",
                                        "University Towns and Cities",
                                        "English and Welsh Countryside",
                                        "Remoter Coastal Living",
                                        "Scottish Countryside",
                                        "Ethnically Diverse Metropolitan  Living",
                                        "London Cosmopolitan",
                                        "Services, Manufacturing and Mining Legacy",
                                        "Scottish Industrial Heritage",
                                        "Country Living",
                                        #"Northern Ireland Countryside",
                                        "Town Living",
                                        "Manufacturing Traits",
                                        "Suburban Traits")))


# remove London and Ethnically diverse metropolitan living for 2014-2017
la_bustrips_by_ons <- la_bustrips_by_ons %>%
  filter(!(group_name == "London Cosmopolitan" & between(year, 2014, 2017))) %>%
  filter(!(group_name == "Ethnically Diverse Metropolitan  Living" & between(year, 2014, 2017)))


cols_groups_la <- c("Rural-Urban Fringe" = "#fafa9d",
                    "Thriving Rural" = "#fcfade",
                    "Larger Towns and Cities" = "#6b349b",
                    "University Towns and Cities" = "#a288be",
                    "English and Welsh Countryside" = "#7dc27f",
                    "Remoter Coastal Living" = "#c9dec6",
                    "Scottish Countryside" = "#deeadd",
                    "Ethnically Diverse Metropolitan  Living" = "#f3a4ed",
                    "London Cosmopolitan" = "#e61a1c",
                    "Services, Manufacturing and Mining Legacy" = "#b0b0b0",
                    "Scottish Industrial Heritage" = "#c9c9c9",
                    "Country Living" = "#3f8abc",
                    #"Northern Ireland Countryside" = "#3f8abc",
                    "Town Living" = "#c2d0e5",
                    "Manufacturing Traits" = "#fd8000",
                    "Suburban Traits" = "#facfa3")

ons_la_class_graph <- ggplot(la_bustrips_by_ons, aes(x = year, y = tph_weekday_Morning_Peak, col = group_name)) +
  geom_line(linewidth=1) +
  ylab("Median bus trips per hour") +
  xlab("Year") +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols_groups_la) +
  #facet_wrap(group_name ~ ., ncol = 3, scales = "free_y") +
  theme_bw()

ons_la_class_graph_facet <- ggplot(la_bustrips_by_ons, aes(x = year, y = tph_weekday_Morning_Peak, col = group_name)) +
  geom_line(linewidth=1) +
  ylab("Median bus trips per hour") +
  xlab("Year") +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols_groups_la) +
  facet_wrap(group_name ~ ., ncol = 3, scales = "free_y") +
  theme_bw()


# LSOA ANALYSIS -----------------------------------------------------------

lsoa_bustrips_2004_2023 <- readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds") %>%
  filter(route_type == 3) %>%
  filter(!is.na(zone_id))

# remove london in 2014-2017
lsoa_bustrips_2004_2023 <- lsoa_bustrips_2004_2023 %>%
  filter(!(RGN11NM == "London" & between(year, 2014, 2017)))

tph_cols <- names(lsoa_bustrips_2004_2023)[grepl("tph", names(lsoa_bustrips_2004_2023))]

lsoa_bustrips_2004_2023 <- lsoa_bustrips_2004_2023 %>%
  select(lsoa11 = zone_id,
         route_type,
         year,
         all_of(tph_cols))

ons_class_lsoa <- read_csv("data/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
ons_class_lsoa <- ons_class_lsoa %>%
  distinct(lsoa11 = LSOA11CD,
           SOAC11CD,
           SOAC11NM) %>%
  mutate(SOAC11NM = factor(SOAC11NM,
                           levels = c("Cosmopolitan student neighbourhoods",
                                      "Ageing rural neighbourhoods",
                                      "Prospering countryside life",
                                      "Remoter communities",
                                      "Rural traits",
                                      "Achieving neighbourhoods",
                                      "Asian traits",
                                      "Highly qualified professionals",
                                      "Households in terraces and flats",
                                      "Challenged white communities",
                                      "Constrained renters",
                                      "Hampered neighbourhoods",
                                      "Hard-pressed flat dwellers",
                                      "Ageing urban communities",
                                      "Aspiring urban households",
                                      "Comfortable neighbourhoods",
                                      "Endeavouring social renters",
                                      "Primary sector workers",
                                      "Inner city cosmopolitan",
                                      "Urban cultural mix",
                                      "Young ethnic communities",
                                      "Affluent communities",
                                      "Ageing suburbanites",
                                      "Comfortable suburbia")))

lsoa_bustrips_2004_2023 <- left_join(lsoa_bustrips_2004_2023, ons_class_lsoa, by = "lsoa11")

ons_lsoa_bustrips_2004_2023 <- lsoa_bustrips_2004_2023 %>%
  group_by(SOAC11CD,
           SOAC11NM,
           year) %>%
  summarise(across(all_of(tph_cols), median_with_round))

cols_groups_lsoa = c("Cosmopolitan student neighbourhoods" ='#955123',
                     "Ageing rural neighbourhoods" ='#007f42',
                     "Prospering countryside life" ='#3ea456',
                     "Remoter communities" ='#8aca8e',
                     "Rural traits" ='#cfe8d1',
                     "Achieving neighbourhoods" ='#00498d',
                     "Asian traits" ='#2967ad',
                     "Highly qualified professionals" ='#7b99c7',
                     "Households in terraces and flats" ='#b9c8e1',
                     "Challenged white communities" ='#e3ac20',
                     "Constrained renters" ='#edca1a',
                     "Hampered neighbourhoods" ='#f6e896',
                     "Hard-pressed flat dwellers" ='#fcf5d8',
                     "Ageing urban communities" ='#e64c2b',
                     "Aspiring urban households" ='#ec773c',
                     "Comfortable neighbourhoods" ='#faa460',
                     "Endeavouring social renters" ='#fcc9a0',
                     "Primary sector workers" ='#fee4ce',
                     "Inner city cosmopolitan" = '#f79ff0',
                     "Urban cultural mix" ='#6a339a',
                     "Young ethnic communities" ='#9f84bd',
                     "Affluent communities" ='#576362',
                     "Ageing suburbanites" ='#a1a2a1',
                     "Comfortable suburbia" ='#e5e4e3')

ons_lsoa_class_graph <- ggplot(ons_lsoa_bustrips_2004_2023, aes(x = year, y = tph_weekday_Morning_Peak, col = SOAC11NM)) +
  geom_line(linewidth=1) +
  ylab("Median bus trips per hour") +
  xlab("Year") +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols_groups_lsoa) +
  #facet_wrap(. ~ SOAC11NM, ncol = 3, scales = "free_y") +
  theme_bw()

ons_lsoa_class_graph_facet <- ggplot(ons_lsoa_bustrips_2004_2023, aes(x = year, y = tph_weekday_Morning_Peak, col = SOAC11NM)) +
  geom_line(linewidth=1) +
  ylab("Median bus trips per hour") +
  xlab("Year") +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols_groups_lsoa) +
  facet_wrap(. ~ SOAC11NM, ncol = 3, scales = "free_y") +
  theme_bw()


# Region analysis ---------------------------------------------------------

median_with_round <- function(x) round(median(x, na.rm = TRUE), 2)

rgn_bustrips_by_ons <- la_bustrips_2004_2023 %>%
  group_by(region_name,
           year) %>%
  summarise(across(all_of(tph_cols), median_with_round)) %>%
  ungroup()

rgn_graph <- ggplot(data = rgn_bustrips_by_ons, aes(x = year, col = region_name)) +
  geom_line(aes(y = tph_weekday_Morning_Peak), linewidth = 1, show.legend = FALSE) +
  #geom_line(aes(y = runs_pct_max_int), linetype = "dotted", linewidth = 1.5, show.legend = FALSE) +
  xlab("Year") +
  ylab("median no. trips") +
  theme_bw() +
  facet_wrap(. ~ region_name, ncol = 3, scales = "free_y")


# METROPOLITAN AREA -------------------------------------------------------
#  metro area analysis ----------------------------------------------------

la_bustrips_2004_2023 <- join_metro_by_lup(la_bustrips_2004_2023)

metro_areas_to_include <- c("Cambridgeshire and Peterborough",
                            "Greater London",
                            "Greater Manchester",
                            "Liverpool City Region",
                            "North East",
                            "North of Tyne",
                            "Sheffield City Region",
                            "Tees Valley",
                            "West Midlands",
                            "West of England",
                            "West Yorkshire")

metro_bustrips_2004_2023 <- la_bustrips_2004_2023 %>%
  filter(!is.na(metro_area_name)) %>%
  filter(metro_area_name %in% metro_areas_to_include)

metro_bustrips_summary <- metro_bustrips_2004_2023 %>%
  group_by(metro_area_code,
           metro_area_name,
           year) %>%
  summarise(across(all_of(tph_cols), median_with_round)) %>%
  ungroup()

metro_graph <- ggplot(data = metro_bustrips_summary, aes(x = year, col = metro_area_name)) +
  geom_line(aes(y = tph_weekday_Morning_Peak), linewidth = 1.5, show.legend = FALSE) +
  #geom_line(aes(y = runs_pct_max_int), linetype = "dotted", linewidth = 1.5, show.legend = FALSE) +
  xlab("Year") +
  ylab("median no. trips") +
  theme_bw() +
  facet_wrap(. ~ metro_area_name, ncol = 3)


ggsave(filename = "plots/june-2023/region_bustrips_trends.png",
       plot = rgn_graph,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")

ggsave(filename = "plots/june-2023/metroregion_bustrips_trends.png",
       plot = metro_graph,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")



ggsave(filename = "plots/june-2023/ONS-LA-class.png",
       plot = ons_la_class_graph,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")

ggsave(filename = "plots/june-2023/ONS-LA-class-facet.png",
       plot = ons_la_class_graph_facet,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")

ggsave(filename = "plots/june-2023/ONS-LSOA-class.png",
       plot = ons_lsoa_class_graph,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")

ggsave(filename = "plots/june-2023/ONS-LSOA-class-facet.png",
       plot = ons_lsoa_class_graph_facet,
       device = "png",
       width = 25,
       height = 30,
       units = "cm")
