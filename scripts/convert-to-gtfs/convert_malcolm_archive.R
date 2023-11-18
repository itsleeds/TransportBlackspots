#remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)
#load_data("naptan_replace")

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange"
naptan = get_naptan()
cal = get_bank_holidays()
skip_done = TRUE

if(!dir.exists(file.path(base_path,"GTFS"))){dir.create(file.path(base_path,"GTFS"))}

# dates
#dates = c("20180515","20191008","20200701","20211012","20221102", "20230503")
#dates = dates[length(dates):1]
dates = "20231101"

for(i in seq_along(dates)){
  message(dates[i])

  zps <- list.files(file.path(base_path, paste0("data_",dates[i])), full.names = TRUE, pattern = ".zip")
  dir.create(file.path(base_path,"GTFS",paste0("GTFS_",dates[i])))

  for(j in seq_along(zps)){


    nm = strsplit(zps[j], "/", fixed = TRUE)[[1]]
    nm = nm[length(nm)]
    nm = gsub(".zip","",nm)

    if(file.exists(file.path(base_path,"GTFS",paste0("GTFS_",dates[i]),paste0(nm,".zip"))) & skip_done){
      message("skipping ",nm)
      next
    } else {
      message(zps[j])
    }


    gtfs <- transxchange2gtfs(zps[j],
                              silent = TRUE,
                              cal = cal,
                              naptan = naptan,
                              ncores = 30,
                              try_mode = FALSE,
                              force_merge = TRUE)
    gtfs <- gtfs_clean(gtfs)

    # Fix known errors
    load_data("naptan_replace")
    naptan_replace <- naptan_replace[naptan_replace$stop_id %in% gtfs$stops$stop_id,]
    message("Replacing ",nrow(naptan_replace)," stop locations with values from UK2GTFS")
    gtfs$stops <- gtfs$stops[!gtfs$stops$stop_id %in% naptan_replace$stop_id,]
    gtfs$stops <- rbind(gtfs$stops, naptan_replace)

    gtfs <- gtfs_interpolate_times(gtfs, ncores = 30)
    gtfs_validate_internal(gtfs)


    gtfs_write(gtfs,
               file.path(base_path,"GTFS",paste0("GTFS_",dates[i])),
               name = nm)
    rm(gtfs)

  }


}
