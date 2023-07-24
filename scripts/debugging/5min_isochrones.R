remotes::install_github("ropensci/opentripplanner")

library(opentripplanner)
library(tmap)
library(sf)
tmap_mode("view")

#java -Xmx110000M -d64 -jar "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar" --router great-britain2 --graphs "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs" --server --port 8091 --securePort 8092 --analyst --pointSets "D:/OneDrive - University of Leeds/Data/opentripplanner/pointsets"

lsoa = st_read("data/EngWales_centroids_modified.geojson")

otpcon <- otp_connect(router = "great-britain2", port = 8091)
iso_walk <- otp_isochrone(otpcon,
                          fromPlace = lsoa,
                          fromID = lsoa$code,
                          mode = "WALK",
                          cutoffSec = 6*60,
                          ncores = 30)



saveRDS(iso_walk, "data/isochones_walk.Rds")
