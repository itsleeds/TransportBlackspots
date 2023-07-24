remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/ATOC"
skip_done = TRUE

if(!dir.exists(file.path(base_path,"GTFS"))){dir.create(file.path(base_path,"GTFS"))}

# dates
#dates = c("2018-10-16","2019-08-31","2020-11-26","2021-10-09","2022-11-02")
dates = c("2023-05-03")

for(i in 1){
  message(dates[i])

  zps <- list.files(file.path(base_path,"timetable",dates[i]), full.names = TRUE, pattern = ".zip", recursive = T)
  zps = zps[!grepl("xml",zps)]
  dir.create(file.path(base_path,"GTFS",paste0("GTFS_",dates[i])))


    if(file.exists(file.path(base_path,"GTFS",paste0(dates[i],".zip")))){
      message("skipping ",nm)
      next
    } else {
      message(zps)
    }


    gtfs <- atoc2gtfs(zps,
                      silent = TRUE,
                      ncores = 30)
    gtfs <- gtfs_clean(gtfs)


    gtfs_write(gtfs,
               file.path(base_path,"GTFS"),
               name = dates[i])


  rm(gtfs)


}
