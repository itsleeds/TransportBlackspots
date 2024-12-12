# add ethnicity, income and car ownership ---------------------------------

add_ethnicity_imd_carownership_lsoa <- function(lsoa11_df) {

  source("../environmental-data-for-change/scripts/useful-functions.R")
  source("../environmental-data-for-change/scripts/imd/imd-analysis.R")
  source("../environmental-data-for-change/scripts/ethnicity/ethnicity-analysis.R")
  source("../environmental-data-for-change/scripts/transport/car-ownership.R")

  lsoa11_df <- add.ethnicity.to.lsoa.dataset(lsoa11_df, rename_lsoa_code = TRUE)
  lsoa11_df <- add.imd.data.to.lsoa.data.set(lsoa11_df, rename_lsoa_code = TRUE)
  lsoa11_df <- add.noncar.owners.to.lsoa.dataset(lsoa11_df, rename_lsoa_code = TRUE)

}
