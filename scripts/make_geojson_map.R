library(tidyr)
library(dplyr)
library(sf)

all = readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")
all = all[,!grepl("runs_",names(all))]
all = all[all$route_type < 5,]

wide = pivot_wider(all,
                   names_from = c("year","route_type"),
                   values_from = tph_weekday_Night:tph_Sun_Evening,
                   id_cols = c("zone_id","LAD17NM","RGN11NM")
                   )

lsoa = readRDS("data/GB_LSOA_2011_super_generalised.Rds")
wide[4:1218] <- lapply(wide[4:1218], round, digits = 1)

lsoa = left_join(lsoa, wide, by = c("code" = "zone_id"))

st_precision(lsoa) <- 100000

st_write(lsoa, "data/lsoa_wide.geojson", delete_dsn = TRUE)





## Bash
cd /mnt/d/GitHub/ITSLeeds/TransportBlackspots/data

tippecanoe -o lsoa.pmtiles \
--name=lsoa \
--layer=lsoa \
--attribution=UniverstyofLeeds \
-zg \
--minimum-zoom=5 \
--coalesce-smallest-as-needed \
--detect-shared-borders \
--extend-zooms-if-still-dropping \
--maximum-tile-bytes=5000000 \
--simplification=10 \
--buffer=5 \
--force  lsoa_wide.geojson
