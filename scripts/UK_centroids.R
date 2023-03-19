library(tmap)
library(sf)
tmap_mode("view")

lsoa_eng <- read_sf("../../creds2/CarbonCalculator/data-prepared/lsoa_centroids.geojson")

dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/EW_LSOA_2011_Centroids.zip",
      exdir = "tmp")
lsoa_wales <- read_sf("tmp/Lower_Layer_Super_Output_Areas__December_2011__Population_Weighted_Centroids.shp")
unlink("tmp", recursive = TRUE)
lsoa_wales <- lsoa_wales[substr(lsoa_wales$lsoa11cd,1,1) == "W",]
lsoa_wales <- lsoa_wales[,c("lsoa11cd")]
lsoa_wales <- st_transform(lsoa_wales, 4326)
names(lsoa_wales)[1] <- "code"

lsoa <- rbind(lsoa_eng, lsoa_wales)
st_write(lsoa,"data/EngWales_centroids_modified.geojson")
