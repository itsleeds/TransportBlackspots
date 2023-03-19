remotes::install_github("ITSleeds/UK2GTFS", upgrade = "never")
library(UK2GTFS)

base_path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/Bus Archive"
naptan = get_naptan()
cal = get_bank_holidays()
skip_done = TRUE

for(j in 2014:2019){

  year = paste0(j," Oct")

  if(!dir.exists(file.path(base_path,"GTFS",year))){
    dir.create(file.path(base_path,"GTFS",year))
  }

  zips = list.files(file.path(base_path, year), pattern = ".zip", full.names = TRUE)


  for(i in 1:length(zips)){
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
                             ncores = 30,
                             silent = TRUE,
                             cal = cal,
                             scotland = ifelse(scot,"yes","no"),
                             force_merge = TRUE,
                             try_mode = FALSE)
    gtfs = gtfs_clean(gtfs)
    gtfs_write(gtfs,
               folder = file.path(base_path,"GTFS",year),
               name = gsub(".zip","",nm))

  }
}

# Errors
# NE 20141021.zip
#  C:\Users\earmmor\AppData\Local\Temp\Rtmp8YK1Sw/txc/NE_130_PB2518_702.xml with error:
# Error in as.data.frame.default(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors) :

