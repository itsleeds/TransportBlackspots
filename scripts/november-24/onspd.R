load_onspd <- function(keep_only_current = TRUE) {

  # set timer
  print("reading in ONSPD...")
  tic("ONSPD loaded")
  #set current filepath of ONSPD (to allow updates to ONSPD in data without having to update script)
  onspd.location <- list.files("../ons-geog-data/onspd/Data",
                               pattern = "ONSPD.+csv",
                               full.names = TRUE)
  # read in full onspd
  onspd <- read.csv(onspd.location,
                    stringsAsFactors = FALSE)

  # keep only current postcodes
  if(keep_only_current) {
    onspd <- onspd %>%
      filter(is.na(doterm))
  }

  # select only geographical fields of interest
  onspd <- onspd %>%
    select(pcds,
           dointr,
           doterm,
           ru11ind,
           oa11,
           lsoa11,
           msoa11,
           osward,
           pcon,
           oslaua,
           oscty,
           rgn,
           lat,
           long)

  # end timer
  toc()

  # return ONSPD
  return(onspd)
}




#' make lsoa to LA lookup
make_lsoa_to_oslaua_lup <- function() {

  # this is annoying because some LSOAs straddle two local authorities
  # need to use the one that the LSOA is predominantly in based on highest proportion of postcodes
  lsoa_to_oslaua_lup <- onspd %>%
    filter(substring(lsoa11, 1, 1) %in% c("E", "W")) %>%
    group_by(lsoa11,
             oslaua) %>%
    summarise(n = n())

  # in all these cases, there is just one postcode in a different LSOA, so this is straightforward:
  # test <- summarise_by_vars(lsoa_to_oslaua_lup, lsoa11) %>%
  #   filter(n > 1) %>%
  #   distinct()
  #
  # lsoa_to_oslaua_lup <- lsoa_to_oslaua_lup %>%
  #   filter(lsoa11 %in% test$lsoa11)

  lsoa_to_oslaua_lup <- lsoa_to_oslaua_lup %>%
    top_n(n = 1,
          wt = n) %>%
    select(-n)

}



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
add_geography <- function(lsoa_df) {

  lsoa_to_msoa_lup <- make_lsoa_to_msoa_lup()
  lsoa_to_pcon_lup <- make_lsoa11_to_pcon24_one_to_one(lookup_type = "pc")
  lsoa_to_oslaua_lup <- get_lsoa_to_oslaua_lup()
  oslaua_to_rgn_lup <- make_oslaua_to_rgn_lup()

  lsoa_df <- left_join(lsoa_df, lsoa_to_msoa_lup, by = "lsoa11")
  lsoa_df <- left_join(lsoa_df, lsoa_to_pcon_lup, by = "lsoa11")
  lsoa_df <- left_join(lsoa_df, lsoa_to_oslaua_lup, by = "lsoa11")
  lsoa_df <- left_join(lsoa_df, oslaua_to_rgn_lup, by = "oslaua")

  lsoa_df <- lsoa_df %>%
    select(lsoa11,
           msoa11,
           msoa11_name,
           pcon24,
           pcon24_name,
           oslaua,
           local_authority,
           rgn,
           region_name,
           everything())

}
