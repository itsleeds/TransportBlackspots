remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR"

for(j in 2004:2011){

  year = paste0("October-",j,".zip")
  message(year)

  gtfs <- nptdr2gtfs(file.path(base_path, year), silent = FALSE)
  gtfs <- gtfs_clean(gtfs)
  gtfs_validate_internal(gtfs)
  gtfs_write(gtfs, folder = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/GTFS/", name = paste0("NPTDR_", j))

}
