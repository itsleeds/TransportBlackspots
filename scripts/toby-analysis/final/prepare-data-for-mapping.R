
#  common functions for making map data -----------------------------------

calculate_bustrip_yearband_chgs <- function(bustrips_data) {

  # create final variables and tidy up (e.g. decimal places...)
  bustrips_data <- bustrips_data %>%
    mutate(tph0823_chg = round(tph_2023 - tph_2006_2008, 2),
           tph1023_chg = round(tph_2023 - tph_2010, 2)) %>%
    mutate(tph0823_chg_pct = round(tph0823_chg / tph_2006_2008, 4),
           tph1023_chg_pct = round(tph1023_chg / tph_2010, 4)) %>%
    mutate(tph_2006_2008 = round(tph_2006_2008, 2),
           tph_2010 = round(tph_2010, 2),
           tph_2023 = round(tph_2023, 2)) %>%
    mutate(tph0823_chg_pct_rank = rank(tph0823_chg_pct),
           tph1023_chg_pct_rank = rank(tph1023_chg_pct))
}



make_map_data_simple <- function(geog, ...) {

  if(!geog %in% c("lsoa", "la", "region")) {
    stop("geog is not one of 'lsoa', 'la' or 'region'")
  }

  if(geog == "lsoa") {
    bustrip_data <- readRDS("data/bustrips_lsoa_2004_2023_cleaned.rds")
  }
  if(geog == "la" | geog == "region") {
    bustrip_data <- readRDS("data/la_bustrips_2005_23_cleaned.rds")
  }

  bustrip_data <- bustrip_data %>%
    filter(period_name == "tph_daytime_avg") %>%
    filter(year %in% c(2006, 2007, 2008, 2010, 2023)) %>%
    mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "tph_2006_2008",
                                 year == 2010 ~ "tph_2010",
                                 year == 2023 ~ "tph_2023"))

  bustrip_data <- bustrip_data %>%
    group_by(...,
             year_band) %>%
    summarise(tph = mean(runs_cleaned, na.rm = TRUE)) %>%
    ungroup()

  # spread data wide
  bustrips_for_map <- bustrip_data %>%
    spread(key = year_band,
           value = tph,
           fill = 0)


  #finalise data fields
  bustrips_for_map <- calculate_bustrip_yearband_chgs(bustrips_for_map)

}

# get lsoa population data for PCON pop-weighted averages
get_pop_lsoa <- function() {

  # get lsoa population statistics
  pop_lsoa <- read.xlsx("../environmental-data-for-change/data/population/sape23dt13mid2020lsoabroadagesestimatesunformatted.xlsx",
                        sheet = "Mid-2020 Persons",
                        startRow = 5)

  # select and rename key variables
  pop_lsoa <- pop_lsoa %>%
    transmute(lsoa11 = LSOA.Code,
              population = as.numeric(All.Ages))

}

# get la population
get_pop_la <- function() {

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
}



## pcon data specific function...
make_lsoa11_pcon23_lup <- function() {

  # read in from another repository
  lsoa_to_pcon23_lup <- read.csv("../constituency-environmental-reports/data/lookup-tables/postcode-based/LSOA11-to-PC23-by-postcode.csv")

  # make one to one lookup
  lsoa_to_pcon23_lup <- lsoa_to_pcon23_lup %>%
    group_by(lsoa11 = LSOA11CD) %>%
    slice_max(order_by = postcode_proportion_in_pcon,
              n = 1) %>%
    ungroup() %>%
    select(lsoa11,
           parliamentary_constituency_2023 = Constituen)
}

# see this script for details processing polical data:
# source("scripts/toby-analysis/final/politics.R")
make_map_data_pcon23 <- function() {

  # get lsoa bustrip data, lsoa pop data and lsoa to pcon lookup
  lsoa_bustrips <- readRDS("data/bustrips_lsoa_2004_2023_cleaned.rds")
  pop_lsoa <- get_pop_lsoa()
  lsoa_to_pcon23_lup <- make_lsoa11_pcon23_lup()

  # join all by lsoa11
  pcon_bustrips_23 <- Reduce(function(x, y) left_join(x, y, by = "lsoa11"),
                             list(lsoa_bustrips,
                                  pop_lsoa,
                                  lsoa_to_pcon23_lup))

  # remove scotland data...
  pcon_bustrips_23 <- pcon_bustrips_23 %>%
    filter(!grepl("^S", lsoa11))

  # summarise data by year bands for a given period
  pcon_bustrips_23 <- pcon_bustrips_23 %>%
    filter(period_name == "tph_daytime_avg") %>%
    filter(year %in% c(2006, 2007, 2008, 2010, 2023)) %>%
    mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "tph_2006_2008",
                                 year == 2010 ~ "tph_2010",
                                 year == 2023 ~ "tph_2023"))

  pcon_bustrips_23 <- pcon_bustrips_23 %>%
    group_by(parliamentary_constituency_2023,
             year_band) %>%
    summarise(tph = mean(runs_cleaned, na.rm = TRUE)) %>%
    ungroup()

  # spread data wide
  pcon_bustrips_23 <- pcon_bustrips_23 %>%
    spread(key = year_band,
           value = tph)

  pcon_bustrips_23 <-  calculate_bustrip_yearband_chgs(pcon_bustrips_23)

  # add implied pcon results
  pcon23_implied_results <- read.csv("data/pcon23-implied-results.csv")
  pcon23_implied_results <- pcon23_implied_results %>%
    rename(parliamentary_constituency_2023 = constituency23_name)

  pcon_bustrips_23 <- left_join(pcon_bustrips_23, pcon23_implied_results, by = "parliamentary_constituency_2023")

  pcon_bustrips_23 <- pcon_bustrips_23 %>%
    select(parliamentary_constituency_2023,
           implied_mp_name,
           implied_first_party,
           implied_majority_2019,
           implied_swing_needed,
           everything())

}
#  Transport Authorities --------------------------------------------------

# Transport authorities are Combined Authorities, County Councils, Greater London. Plus some other LAs (e.g. N. Somerset)
# https://atip.uk/

#' Make LA to Combined Authority lookup
la_to_transport_authority_lup <- function() {

  # read in data set
  la_to_metro_lup <- read.csv("data/lsoa/Local_Authority_District_to_Combined_Authority_(April_2023)_Lookup_in_England.csv")
  la_to_metro_lup <- la_to_metro_lup %>%
    transmute(oslaua = LAD23CD,
              local_authority_name = LAD23NM,
              trans_auth_code = CAUTH23CD,
              trans_auth_name = CAUTH23NM,
              auth_type = "combined-authority")

  oslaua_to_cty_lup <- make_oslaua_to_cty_lup()
  oslaua_to_cty_lup <- oslaua_to_cty_lup %>%
    rename(trans_auth_code = oscty,
           trans_auth_name = county_name) %>%
    mutate(auth_type = "county-council")

  # cambridge is listed as a county council and a combined authority (Cambridge and Peterborough).
  # remove from county list as the combined authority is the transport authority
  oslaua_to_cty_lup <- oslaua_to_cty_lup %>%
    filter(!trans_auth_name == "Cambridgeshire")

  # transport remit for york and north yorkshire is separate, so remove north yorkshire from cty
  oslaua_to_cty_lup <- oslaua_to_cty_lup %>%
    filter(!trans_auth_name == "North Yorkshire")

  london_authorities <- make_oslaua_to_london_lup()
  london_authorities <- london_authorities %>%
    rename(trans_auth_code = CAUTH23CD,
           trans_auth_name = CAUTH23NM) %>%
    mutate(auth_type = "greater-london-authority")

  oslaua_to_TA_lup <- bind_rows(la_to_metro_lup,
                                oslaua_to_cty_lup,
                                london_authorities)

  # find all local authorities and join to transport authorities
  la_list <- get_la_names()
  oslaua_to_TA_lup <- left_join(la_list, oslaua_to_TA_lup, by = c("oslaua", "local_authority_name"))

  # having manually checked, all unitary authorities that aren't county councils or metro areas or greater london are
  # their own transport authority. This covers 68 unitary authorities across England and Wales

  oslaua_to_TA_lup <- oslaua_to_TA_lup %>%
    mutate(trans_auth_code = ifelse(is.na(trans_auth_code), oslaua, trans_auth_code),
           trans_auth_name = ifelse(is.na(trans_auth_name), local_authority_name, trans_auth_name)) %>%
    filter(grepl("^E|W", oslaua)) %>%
    mutate(auth_type = ifelse(local_authority_name == trans_auth_name, "unitary-authority", auth_type))

}


get_la_names <- function() {
  # read in local authority names and rename fields
  la_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "LA_UA names.+csv", full.names = TRUE)
  la_names <- read.csv(la_names_location,
                       stringsAsFactors = FALSE)
  la_names <- la_names[1:2]
  colnames(la_names) <- c("oslaua", "local_authority_name")
  return(la_names)
}


#' Make LA to County lookup
make_oslaua_to_cty_lup <- function() {

  # select distinct fields and entries from onspd
  la_to_cty_lup <- onspd %>%
    filter(substring(oslaua, 1, 1) %in% c("E", "W")) %>%
    distinct(oslaua,
             oscty)

  # doesn't include new LA to cty codes
  la_to_cty_lup <- la_to_cty_lup %>%
    mutate(oscty = case_when(oslaua %in% c("E06000014", "E06000065") ~ "E10000023",  # York and N. Yorks
                             oslaua %in% c("E06000063", "E06000064") ~ "E10000006", # Cumbria
                             oslaua == "E06000066" ~ "E10000027", # Somerset
                             TRUE ~ oscty))

  # read in local authority names and rename fields
  la_names <- get_la_names()

  # read in county names and rename fields
  cty_names_location <-  list.files("../ons-geog-data/onspd/Documents/", pattern = "County names.+csv", full.names = TRUE)
  cty_names <- read.csv(cty_names_location,
                        stringsAsFactors = FALSE)
  cty_names <- cty_names[1:2]
  colnames(cty_names) <- c("oscty", "county_name")

  # join names to data
  la_to_cty_lup <- left_join(la_to_cty_lup, cty_names, by = "oscty")
  la_to_cty_lup <- left_join(la_to_cty_lup, la_names, by = "oslaua")

  # In April 2023, Cumbria county council ceased to exist, replace by westmorland and cumberland,
  # And Somerset became a unitary authority, not a county council
  la_to_cty_lup <- la_to_cty_lup %>%
    filter(!county_name == "Cumbria") %>%
    filter(!county_name == "Somerset")

  # filter for only counties (not unitary or metropolitan areas)
  la_to_cty_lup <- la_to_cty_lup %>%
    filter(oscty != "E99999999" & oscty != "W99999999")


}


make_oslaua_to_london_lup <- function() {

  la_to_london_lup <- onspd %>%
    filter(substring(oslaua, 1, 1) %in% c("E", "W")) %>%
    distinct(oslaua,
             rgn)

  rgn_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "Region names.+csv", full.names = TRUE)
  rgn_names <- read.csv(rgn_names_location,
                        stringsAsFactors = FALSE)

  rgn_names <- rgn_names[c(1,3)]
  colnames(rgn_names) <- c("rgn", "region_name")

  # la names
  la_names <- get_la_names()

  la_to_london_lup <- left_join(la_to_london_lup, rgn_names, by = "rgn")
  la_to_london_lup <- left_join(la_to_london_lup, la_names, by = "oslaua")

  la_to_london_lup <- la_to_london_lup %>%
    filter(region_name == "London") %>%
    select(oslaua,
           local_authority_name,
           CAUTH23CD = rgn,
           CAUTH23NM = region_name)

}



# test function -----------------------------------------------------------

make_map_data_transauth <- function() {

  # get lsoa bustrip data, lsoa pop data and lsoa to pcon lookup
  la_bustrips <- readRDS("data/la_bustrips_2005_23_cleaned.rds")
  la_bustrips <- la_bustrips %>%
    rename(oslaua = LAD23CD,
           local_authority_name = LAD23NM)
  pop_la <- get_pop_la()
  la_to_transauth_lup <- la_to_transport_authority_lup()

  # join all by lsoa11
  transauth_bustrips_23 <- Reduce(function(x, y) left_join(x, y, by = c("oslaua", "local_authority_name")),
                                  list(la_bustrips,
                                       pop_la,
                                       la_to_transauth_lup))

  # remove scotland data...
  transauth_bustrips_23 <- transauth_bustrips_23 %>%
    filter(!grepl("^S", oslaua))

  # summarise data by year bands for a given period
  transauth_bustrips_23 <- transauth_bustrips_23 %>%
    filter(period_name == "tph_daytime_avg") %>%
    filter(year %in% c(2006, 2007, 2008, 2010, 2023)) %>%
    mutate(year_band = case_when(year %in% c(2006, 2007, 2008) ~ "tph_2006_2008",
                                 year == 2010 ~ "tph_2010",
                                 year == 2023 ~ "tph_2023"))

  transauth_bustrips_23 <- transauth_bustrips_23 %>%
    group_by(trans_auth_code,
             trans_auth_name,
             auth_type,
             year_band) %>%
    summarise(tph = weighted.mean(runs_cleaned, population, na.rm = TRUE)) %>%
    ungroup()

  # spread data wide
  transauth_bustrips_23 <- transauth_bustrips_23 %>%
    spread(key = year_band,
           value = tph)

  transauth_bustrips_23 <- calculate_bustrip_yearband_chgs(transauth_bustrips_23)

}

# local authority data ----------------------------------------------------
la_map_data <- make_map_data_simple(geog = "la",
                                    LAD23CD,
                                    LAD23NM)

# region
region_map_data <- make_map_data_simple(geog = "region",
                                        RGN20CD,
                                        RGN20NM)

# lsoa data ---------------------------------------------------------------
lsoa_map_data <- make_map_data_simple(geog = "lsoa",
                                      lsoa11)

#  2023 westminster constituency data -------------------------------------
pcon23_map_data <- make_map_data_pcon23()

#  Transport authority data -------------------------------------
transauth_map_data <- make_map_data_transauth()

# make transport authority gis layer from combined authority, local authority, county council and region gis boundaries
make_trans_auth_gis_layer <- function() {

  simplify_gis_boundaries <- function(boundaries, col_numbers = c(1:2), filter = FALSE) {

    boundaries <- boundaries[col_numbers]
    colnames(boundaries) <- c("trans_auth_code", "trans_auth_name", "geometry")
    return(boundaries)

  }

  la_boundaries <- simplify_gis_boundaries(st_read("../gis-data/boundaries/local-authority/Local_Authority_Districts_May_2023_UK_BFC_V2/LAD_MAY_2023_UK_BFC_V2.shp"),)
  cty_boundaries <- simplify_gis_boundaries(st_read("../gis-data/boundaries/counties/Counties_May_2023_Boundaries_EN_BFC/CTY_MAY_2023_EN_BFC.shp"))
  cauth_boundaries <- simplify_gis_boundaries(st_read("../gis-data/boundaries/combined-authorities/Combined_Authorities_December_2022_EN_BFC/CAUTH_DEC_2022_EN_BFC.shp"))
  regions <- simplify_gis_boundaries(st_read("../gis-data/boundaries/regions/Regions_(December_2020)_EN_BFC/Regions_(December_2020)_EN_BFC.shp"),
                                     col_numbers = c(2:3))

  trans_auth_boundaries <- bind_rows(la_boundaries,
                                     cty_boundaries,
                                     cauth_boundaries,
                                     regions)


  trans_auth_boundaries <- inner_join(trans_auth_boundaries, transauth_map_data, by = c("trans_auth_code", "trans_auth_name"))

  trans_auth_boundaries <- trans_auth_boundaries %>%
    select(TA_code = trans_auth_code,
           TA_name = trans_auth_name,
           geometry)

  st_write(trans_auth_boundaries,
           dsn = "../gis-data/boundaries/transport-authorities/Transport_authorities_April23/Transport_authorities_April23.shp",
           delete_dsn = TRUE)

}



save_map_layer_data <- function(df,
                                filepath) {

  if(!dir.exists("foe-outputs/map-data")) {
    dir.create("foe-outputs/map-data")
  }

  fullfilepath = paste0("foe-outputs/map-data/", filepath)

  write.csv(df,
            fullfilepath,
            row.names = FALSE,
            na = "")
}

save_map_layer_data(la_map_data, "la-bustrip-trends.csv")
save_map_layer_data(region_map_data, "region-bustrip-trends.csv")
save_map_layer_data(transauth_map_data, "transport-authority-bustrip-trends.csv")
save_map_layer_data(pcon23_map_data, "pcon23-bustrip-trends.csv")
save_map_layer_data(lsoa_map_data, "lsoa-bustrip-trends.csv")

rename_bustrip_fields <- function(bustrip_data, geog) {

  if(geog == "pcon") {
    bustrip_data <- bustrip_data %>%
      rename(`Constituency (2023 boundary)` = parliamentary_constituency_2023,
             `MP (2019-based estimate)` = implied_mp_name,
             `Party (2019-based estimate)` = implied_first_party,
             `Majority (2019-based estimate)` = implied_majority_2019,
             `Swing needed (2019-based estimate)` = implied_swing_needed)
  }

  if(geog == "region") {
    bustrip_data <- bustrip_data %>%
      rename(region_code = RGN20CD,
             `Region` = RGN20NM)
  }

  if(geog == "la") {
    bustrip_data <- bustrip_data %>%
      rename(la_code = LAD23CD,
             `Local authority` = LAD23NM)
  }

  if(geog == "transauth") {
    bustrip_data <- bustrip_data %>%
      rename(ons_code = trans_auth_code,
             `Transport authority` = trans_auth_name)
  }

  bustrip_data <- bustrip_data %>%
    rename(`Trips per hour (2006/08)` = tph_2006_2008,
           `Trips per hour (2010)` = tph_2010,
           `Trips per hour (2023)` = tph_2023,
           `Trips per hour change: 2006/08-2023` = tph0823_chg,
           `Trips per hour change: 2010-2023` = tph1023_chg,
           `Trips per hour % change: 2006/08-2023` = tph0823_chg_pct,
           `Trips per hour % change: 2010-2023` = tph1023_chg_pct,
           `Trips per hour change rank (2006/08-2023)` = tph0823_chg_pct_rank,
           `Trips per hour change rank (2010-2023)` = tph1023_chg_pct_rank)


}

la_map_data <- rename_bustrip_fields(la_map_data, "la")
region_map_data <- rename_bustrip_fields(region_map_data, "region")
transauth_map_data <- rename_bustrip_fields(transauth_map_data, "transauth")
pcon23_map_data <- rename_bustrip_fields(pcon23_map_data, "pcon")
lsoa_map_data <- rename_bustrip_fields(lsoa_map_data, "lsoa")


#  political analysis -----------------------------------------------------

source("../environmental-data-for-change/scripts/save-workbook.R")
save.spreadsheet.multiple.tab(tab1.data = region_map_data,
                              tab1.name = "region",
                              tab2.data = transauth_map_data,
                              tab2.name = "transport-authority",
                              tab3.data = la_map_data,
                              tab3.name = "local-authority",
                              number.of.tabs = 3,
                              xlsx.path = "foe-outputs/map-data/bustrip-trends-authority.xlsx",
                              alternative.xlsx.path = "C:/Users/toby.bridgeman/OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/bustrip-trends-by-authority.xlsx",
                              wb.name = "buses",
                              number.cols = c(3:7,10:11),
                              percent.cols = 8:9,
                              currency.cols = 0,
                              #auto.col.widths = ,
                              manual.colwidths = 15,
                              make.cols.nice = FALSE)

save.spreadsheet.1tab(tab1.data = pcon23_map_data,
                      tab1.name = "pcon23",
                      xlsx.path = "foe-outputs/map-data/bustrip-trends-pcon23.xlsx",
                      alternative.xlsx.path = "C:/Users/toby.bridgeman/OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/bustrip-trends-pcon23.xlsx",
                      wb.name = "buses",
                      number.cols = c(4,6:10,13:14),
                      percent.cols = c(5,11:12),
                      currency.cols = 0,
                      #auto.col.widths = ,
                      manual.colwidths = 15,
                      make.cols.nice = FALSE)

save.spreadsheet.1tab(tab1.data = lsoa_map_data,
                      tab1.name = "lsoa11",
                      xlsx.path = "foe-outputs/map-data/bustrip-trends-lsoa.xlsx",
                      alternative.xlsx.path = "C:/Users/toby.bridgeman/OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/bustrip-trends-lsoa.xlsx",
                      wb.name = "buses",
                      number.cols = c(2:6,9:10),
                      percent.cols = c(7:8),
                      currency.cols = 0,
                      #auto.col.widths = ,
                      manual.colwidths = 15,
                      make.cols.nice = FALSE)
