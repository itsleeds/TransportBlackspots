remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)


for(i in 2004:2009){
  message(i)
  gtfs <- nptdr2gtfs(path = paste0("D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-",i,".zip"))
  gtfs <- gtfs_clean(gtfs)
  gtfs_validate_internal(gtfs)
  gtfs_write(gtfs, folder = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/GTFS/", name = paste0("NPTDR_",i))
}

