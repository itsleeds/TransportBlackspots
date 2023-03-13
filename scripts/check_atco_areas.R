# Check which ATCO area are in each years data.
library(sf)
library(dplyr)
library(tmap)

check_atco <- function(path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-2004.zip"){

  checkmate::assert_file_exists(path, extension = "zip")
  dir.create(file.path(tempdir(),"nptdr_temp"))

  utils::unzip(path, exdir = file.path(tempdir(),"nptdr_temp"))

  # List the files
  fls <- list.files(file.path(tempdir(),"nptdr_temp"), recursive = TRUE, full.names = TRUE)
  fls <- fls[!grepl("MACOSX",fls)]

  fls_admin <- fls[grepl("Admin_Area",fls)]

  if(!length(fls_admin) > 1){
    stop(length(fls_admin)," Admin Area files found")
  }

  # Unzip the Admin Area files
  dir.create(file.path(tempdir(),"nptdr_temp","areas"))

  for(i in seq_len(length(fls_admin))){
    utils::unzip(fls_admin[i], exdir = file.path(tempdir(),"nptdr_temp","areas"))
  }

  fls_cif <- list.files(file.path(tempdir(),"nptdr_temp","areas"),
                        recursive = TRUE, full.names = FALSE,
                        pattern = ".CIF")

  if(!length(fls_cif) >= 1){
    stop(length(fls_cif)," No CIF files found")
  } else {
    message(length(fls_cif)," CIF files found")
  }

  unlink(file.path(tempdir(),"nptdr_temp"), recursive = TRUE)

  fls_nm <- strsplit(fls_cif,"/")
  fls_nm <- sapply(fls_nm, function(x){x[length(x)]})

  fls_nm <- gsub(".CIF","",fls_nm, fixed = TRUE)
  fls_nm <- gsub("ATCO_","",fls_nm, fixed = TRUE)
  fls_nm <- strsplit(fls_nm,"_")

  fls_area <- sapply(fls_nm, `[[`, 1)
  fls_mode <- sapply(fls_nm, `[[`, 2)

  fls_summary = data.frame(area = fls_area, mode = fls_mode)
  fls_summary$inc = TRUE
  fls_summary = tidyr::pivot_wider(fls_summary, names_from = "mode", values_from = "inc", values_fill = FALSE)
  fls_summary$year = substr(path, nchar(path) - 7, nchar(path) - 4)

  return(fls_summary)

}

path_base = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/"

sum_04 = check_atco(file.path(path_base,"October-2004.zip"))
sum_05 = check_atco(file.path(path_base,"October-2005.zip"))
sum_06 = check_atco(file.path(path_base,"October-2006.zip"))
sum_07 = check_atco(file.path(path_base,"October-2007.zip"))
sum_08 = check_atco(file.path(path_base,"October-2008.zip"))
sum_09 = check_atco(file.path(path_base,"October-2009.zip"))
sum_10 = check_atco(file.path(path_base,"October-2010.zip"))
sum_11 = check_atco(file.path(path_base,"October-2011.zip"))

sum_all = list(sum_04, sum_05, sum_06, sum_07, sum_08, sum_09, sum_10, sum_11)
sum_all = bind_rows(sum_all)

sum_bus = sum_all[,c("area","year","BUS")]
sum_bus2 = tidyr::pivot_wider(sum_bus,
                              names_from = "year",
                              values_from = "BUS",
                              values_fill = FALSE)

year_summary = list()
for(i in 1:nrow(sum_bus2)){
  sub = sum_bus2[i,]
  sub = unlist(sub[,2:9])
  year_summary[[i]] = paste(names(sub)[sub], collapse = ", ")
}

sum_bus2$year_summary <- unlist(year_summary)
sum_bus2 = sum_bus2[,c("area","year_summary")]
sum_bus2$year_summary[sum_bus2$year_summary == "2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011"] = "2004 to 2011"
table(sum_bus2$year_summary)

atco_areas = UK2GTFS::atco_areas
atco_areas = st_simplify(atco_areas, dTolerance = 100)

summary(atco_areas$atco_code %in% sum_bus2$area)
summary(sum_bus2$area %in% atco_areas$atco_code)

atco_areas = left_join(atco_areas, sum_bus2, by = c("atco_code" = "area"))
atco_areas = atco_areas[!is.na(atco_areas$year_summary),]

m1 = tm_shape(atco_areas, bbox = bbox) +
  tm_fill(col = "year_summary", breaks = breaks)
tmap_save(m1, "plots/checks/atco_coverage.png")
