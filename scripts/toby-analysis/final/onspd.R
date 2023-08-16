load_onspd <- function(keep.only.current = TRUE) {

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
  if(keep.only.current) {
    onspd <- onspd %>%
      filter(is.na(doterm))
  }

  # select only geographical fields of interest
  onspd <- onspd %>%
    select(pcds,
           dointr,
           doterm,
           oa11,
           lsoa11,
           msoa11,
           osward,
           pcon,
           oslaua,
           oscty,
           rgn)

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

  # use official names
  la_to_rgn_lup <- la_to_rgn_lup %>%
    rename(LAD23CD = oslaua,
           RGN20CD = rgn)

  if(add_la_names) {
    ## read in la names
    la_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "LA_UA.+csv", full.names = TRUE)
    la_names <- read.csv(la_names_location,
                         stringsAsFactors = FALSE)
    la_names <- la_names[c(1:2)]

    ## join names to data
    la_to_rgn_lup <- left_join(la_to_rgn_lup, la_names, by = "LAD23CD")
  }

  # read in region names and rename fields
  rgn_names_location <-  list.files("../ons-geog-data/onspd/Documents/", pattern = "Region.+csv", full.names = TRUE)
  rgn_names <- read.csv(rgn_names_location,
                        stringsAsFactors = FALSE)
  rgn_names <- rgn_names[c(1,3)]

  # join names to data
  la_to_rgn_lup <- left_join(la_to_rgn_lup, rgn_names, by = "RGN20CD")

  # remove '(pseudo)' from Wales region name
  la_to_rgn_lup <- la_to_rgn_lup %>%
    mutate(RGN20NM = gsub("[(]pseudo[)] ", "", RGN20NM))

  la_to_rgn_lup <- la_to_rgn_lup %>%
    filter(substring(LAD23CD, 1, 1) != "N")

}

add_region_by_la_code <- function(trips_data) {

  # get lookup up
  oslaua_to_rgn <- make_oslaua_to_rgn_lup(add_la_names = FALSE)

  # join to data set
  trips_data <- left_join(trips_data, oslaua_to_rgn, by = "LAD23CD")

}


#' Make LA to Combined Authority lookup
join_metro_by_la_lup <- function(trips_data) {

  # read in data set
  la_to_metro_lup <- read.csv("data/lsoa/Local_Authority_District_to_Combined_Authority_(April_2023)_Lookup_in_England.csv")

  la_to_metro_lup <- la_to_metro_lup %>%
    select(LAD23CD,
           LAD23NM,
           CAUTH23CD,
           CAUTH23NM)

  trips_data <- left_join(trips_data, la_to_metro_lup, by = c("LAD23CD", "LAD23NM"))

  trips_data <- trips_data %>%
    mutate(CAUTH23NM = ifelse(RGN20NM == "London", "Greater London", CAUTH23NM))

}
