#' Make LA to Region lookup
make_oslaua_to_rgn_lup <- function(add_la_names = FALSE) {

  # select distinct fields and entries from onspd
  la_to_rgn_lup <- onspd %>%
    #filter(grepl("^[E|W]", lsoa11)) %>%
    group_by(oslaua,
             rgn) %>%
    summarise(no_pcs = n())

  # AUGUST ONSPD has one postcode for Wiltshire in the South East. Slice max to keep only the main region of each LA
  la_to_rgn_lup <- la_to_rgn_lup %>%
    group_by(oslaua) %>%
    slice_max(order_by = no_pcs,
              n = 1) %>%
    select(-no_pcs) %>%
    ungroup()

  if(add_la_names) {
    ## read in la names
    la_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "LA_UA.+csv", full.names = TRUE)
    la_names <- read.csv(la_names_location,
                         stringsAsFactors = FALSE)
    la_names <- la_names[c(1:2)]
    colnames(la_names) <- c("oslaua", "local_authority_name")

    ## join names to data
    la_to_rgn_lup <- left_join(la_to_rgn_lup, la_names, by = "oslaua")
  }

  # read in region names and rename fields
  rgn_names_location <-  list.files("../ons-geog-data/onspd/Documents/", pattern = "Region.+csv", full.names = TRUE)
  rgn_names <- read.csv(rgn_names_location,
                        stringsAsFactors = FALSE)
  rgn_names <- rgn_names[c(1,3)]
  colnames(rgn_names) <- c("rgn", "region_name")

  # join names to data
  la_to_rgn_lup <- left_join(la_to_rgn_lup, rgn_names, by = "rgn")

  # remove '(pseudo)' from Wales region name
  la_to_rgn_lup <- la_to_rgn_lup %>%
    mutate(region_name = gsub("[(]pseudo[)] ", "", region_name))

}


# geography look ups ------------------------------------------------------

source("../geography-lookups/scripts/lookups.R")
source("../geography-lookups/scripts/filter-geographies.R")

add_geography21 <- function(lsoa_df) {

  lsoa21_to_msoa21_lup <- make_lsoa21_to_msoa21_lup()
  lsoa21_to_pcon24_lup <- make_lsoa21_to_pcon24_one_to_one(lookup_type = "pc")
  lsoa21_to_oslaua_lup <- get_lsoa21_to_oslaua_lup()
  oslaua_to_rgn_lup <- make_oslaua_to_rgn_lup()

  lsoa_df <- left_join(lsoa_df, lsoa21_to_msoa21_lup, by = "lsoa21")
  lsoa_df <- left_join(lsoa_df, lsoa21_to_pcon24_lup, by = "lsoa21")
  lsoa_df <- left_join(lsoa_df, lsoa21_to_oslaua_lup, by = "lsoa21")
  lsoa_df <- left_join(lsoa_df, oslaua_to_rgn_lup, by = "oslaua")

  lsoa_df <- lsoa_df %>%
    select(lsoa21,
           msoa21,
           msoa21_name,
           pcon24,
           pcon24_name,
           oslaua,
           local_authority,
           rgn,
           region_name,
           everything())

}


# TRANSPORT AUTHORITY -----------------------------------------------------

make_la_to_transport_authority_lup <- function(remove_la_name = TRUE) {

  # read in data set
  la_to_metro_lup <- read.csv("data/lookups/Local_Authority_District_to_Combined_Authority_(April_2023)_Lookup_in_England.csv")
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

  if(remove_la_name) {
    oslaua_to_TA_lup$local_authority_name <- NULL
  }

  return(oslaua_to_TA_lup)

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


get_la_names <- function() {
  # read in local authority names and rename fields
  la_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "LA_UA names.+csv", full.names = TRUE)
  la_names <- read.csv(la_names_location,
                       stringsAsFactors = FALSE)
  la_names <- la_names[1:2]
  colnames(la_names) <- c("oslaua", "local_authority_name")
  return(la_names)
}
