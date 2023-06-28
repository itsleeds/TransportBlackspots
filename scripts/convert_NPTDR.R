remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR"

for(j in 2004:2011){

  year = paste0("October-",j,".zip")
  message(year)

  gtfs <- nptdr2gtfs(file.path(base_path, year), silent = FALSE)
  gtfs <- gtfs_clean(gtfs)

  # Fix known errors
  naptan_replace <- UK2GTFS::naptan_replace
  naptan_replace <- naptan_replace[naptan_replace$stop_id %in% gtfs$stops$stop_id,]
  message("Replacing ",nrow(naptan_replace)," stop locations with values from UK2GTFS")
  gtfs$stops <- gtfs$stops[!gtfs$stops$stop_id %in% naptan_replace$stop_id,]
  gtfs$stops <- rbind(gtfs$stops, naptan_replace)

  gtfs <- gtfs_interpolate_times(gtfs, ncores = 30)
  gtfs_validate_internal(gtfs)

  gtfs_write(gtfs, folder = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/GTFS/", name = paste0("NPTDR_", j))



}
