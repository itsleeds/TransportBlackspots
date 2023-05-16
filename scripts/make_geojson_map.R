library(tidyr)
library(dplyr)
library(sf)

all = readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")

wide = pivot_wider(all,
                   names_from = c("year","route_type"),
                   values_from = runs_weekday_Night:runs_Sun_Evening,
                   id_cols = c("zone_id","LAD17NM","RGN11NM")
                   )

lsoa = readRDS("data/GB_LSOA_2011_super_generalised.Rds")


lsoa = left_join(lsoa, wide, by = c("code" = "zone_id"))

st_write(lsoa, "data/lsoa_wide.geojson")





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
