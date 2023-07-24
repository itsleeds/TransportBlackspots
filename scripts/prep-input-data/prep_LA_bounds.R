la = geojsonsf::geojson_sf("data/Local_Authority_Districts_(May_2023)_UK_BFC.geojson")
la = la[,"LAD23NM"]
saveRDS(la,"data/LA_bounds_2023.Rds")
