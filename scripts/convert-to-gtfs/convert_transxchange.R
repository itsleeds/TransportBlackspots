#remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive"
naptan = get_naptan()
cal = get_bank_holidays()
skip_done = TRUE

# 2023-07-01 10:14:53 NE 20151006.zip
# All files converted
# Replacing 0 stop locations with values from UK2GTFS
# Crash during time interpolation fail at 17%
# 2 nodes produced errors; first error: conversion of times failed for tripID: 16110

for(j in 2014:2017){
  gc()
  year = paste0(j," Oct")

  if(!dir.exists(file.path(base_path,"GTFS",year))){
    dir.create(file.path(base_path,"GTFS",year))
  }

  if(j < 2017){
    zips = list.files(file.path(base_path, year), pattern = ".zip", full.names = TRUE)

    for(i in 1:length(zips)){ # Skipping bug for now
      scot = grepl("S 2",zips[i])
      nm = strsplit(zips[i], "/")[[1]]
      nm = nm[length(nm)]

      if(skip_done){
        if(file.exists(file.path(base_path,"GTFS",year,nm))){
          message("Skipping ",nm)
          next
        }
      }

      message(Sys.time()," ",nm)
      gtfs = transxchange2gtfs(zips[i],
                               naptan = naptan,
                               ncores = 20,
                               silent = TRUE,
                               cal = cal,
                               scotland = ifelse(scot,"yes","no"),
                               force_merge = TRUE,
                               try_mode = FALSE)
      gtfs = gtfs_clean(gtfs)

      # Fix known errors
      load_data("naptan_replace")
      naptan_replace <- naptan_replace[naptan_replace$stop_id %in% gtfs$stops$stop_id,]
      message("Replacing ",nrow(naptan_replace)," stop locations with values from UK2GTFS")
      gtfs$stops <- gtfs$stops[!gtfs$stops$stop_id %in% naptan_replace$stop_id,]
      gtfs$stops <- rbind(gtfs$stops, naptan_replace)

      gtfs <- gtfs_interpolate_times(gtfs, ncores = 30)
      gtfs_validate_internal(gtfs)



      gtfs_write(gtfs,
                 folder = file.path(base_path,"GTFS",year),
                 name = gsub(".zip","",nm))

    }


  } else {
    zips = list.files(file.path(base_path, year), pattern = ".zip", full.names = TRUE, recursive = TRUE)

    for(i in 1:length(zips)){
      scot = grepl("S.zip",zips[i])
      nm = strsplit(zips[i], "/")[[1]]
      nm = nm[c(length(nm) - 1,length(nm))]
      nm = paste(nm[2:1], collapse = " ")
      nm = gsub(".zip","",nm)

      if(skip_done){
        if(file.exists(file.path(base_path,"GTFS",year,paste0(nm,".zip")))){
          message("Skipping ",nm)
          next
        }
      }

      message(Sys.time()," ",nm)
      gtfs = transxchange2gtfs(zips[i],
                               naptan = naptan,
                               ncores = 30,
                               silent = TRUE,
                               cal = cal,
                               scotland = ifelse(scot,"yes","no"),
                               force_merge = TRUE,
                               try_mode = FALSE)

      gtfs = gtfs_clean(gtfs)

      # Fix known errors
      load_data("naptan_replace")
      naptan_replace <- naptan_replace[naptan_replace$stop_id %in% gtfs$stops$stop_id,]
      message("Replacing ",nrow(naptan_replace)," stop locations with values from UK2GTFS")
      gtfs$stops <- gtfs$stops[!gtfs$stops$stop_id %in% naptan_replace$stop_id,]
      gtfs$stops <- rbind(gtfs$stops, naptan_replace)

      gtfs <- gtfs_interpolate_times(gtfs, ncores = 30)
      gtfs_validate_internal(gtfs)


      gtfs_write(gtfs,
                 folder = file.path(base_path,"GTFS",year),
                 name = gsub(".zip","",nm))

    }

  }


}

# Errors
# NE 20141021.zip
#  C:\Users\earmmor\AppData\Local\Temp\Rtmp8YK1Sw/txc/NE_130_PB2518_702.xml with error:
# Error in as.data.frame.default(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors) :

