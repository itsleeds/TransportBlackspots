load_onspd <- function(keep_only_current = TRUE) {

  # set timer
  print("reading in ONSPD...")
  tic("ONSPD loaded")
  #set current filepath of ONSPD (to allow updates to ONSPD in data without having to update script)
  onspd.location <- list.files("../ons-geog-data/onspd-24/Data",
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
           oa21,
           lsoa11,
           lsoa21,
           msoa11,
           msoa21,
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
