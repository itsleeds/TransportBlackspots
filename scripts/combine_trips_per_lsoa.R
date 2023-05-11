library(tmap)
library(ggplot2)
library(dplyr)

zone = readRDS("data/GB_LSOA_2011_super_generalised.Rds")
zone_service <- list()

for(i in c(2004:2011,2014:2023)){
  sub = readRDS(paste0("data/trips_per_lsoa_by_mode_",i,".Rds"))
  sub$year = i
  zone_service[[i]] <- sub
}

zone_service = dplyr::bind_rows(zone_service)

# COmbine Weekdays
zone_service$runs_weekday_Night <- zone_service$runs_Mon_Night +
  zone_service$runs_Tue_Night +
  zone_service$runs_Wed_Night +
  zone_service$runs_Thu_Night +
  zone_service$runs_Fri_Night / 5

zone_service$runs_weekday_Morning_Peak <- zone_service$`runs_Mon_Morning Peak` +
  zone_service$`runs_Tue_Morning Peak` +
  zone_service$`runs_Wed_Morning Peak` +
  zone_service$`runs_Thu_Morning Peak` +
  zone_service$`runs_Fri_Morning Peak` / 5

zone_service$runs_weekday_Afternoon_Peak <- zone_service$`runs_Mon_Afternoon Peak` +
  zone_service$`runs_Tue_Afternoon Peak` +
  zone_service$`runs_Wed_Afternoon Peak` +
  zone_service$`runs_Thu_Afternoon Peak` +
  zone_service$`runs_Fri_Afternoon Peak` / 5

zone_service$runs_weekday_Midday <- zone_service$runs_Mon_Midday +
  zone_service$runs_Tue_Midday +
  zone_service$runs_Wed_Midday +
  zone_service$runs_Thu_Midday +
  zone_service$runs_Fri_Midday / 5

zone_service$runs_weekday_Evening <- zone_service$runs_Mon_Evening +
  zone_service$runs_Tue_Evening +
  zone_service$runs_Wed_Evening +
  zone_service$runs_Thu_Evening +
  zone_service$runs_Fri_Evening / 5

zone_service$runs_weekday_Night <- zone_service$runs_Mon_Night +
  zone_service$runs_Tue_Night +
  zone_service$runs_Wed_Night +
  zone_service$runs_Thu_Night +
  zone_service$runs_Fri_Night / 5

zone_service <- zone_service[,c("zone_id",
                                "route_type",
                                "runs_weekday_Night",
                                "runs_weekday_Morning_Peak",
                                "runs_weekday_Afternoon_Peak",
                                "runs_weekday_Midday",
                                "runs_weekday_Evening",
                                "runs_Sat_Night",
                                "runs_Sat_Morning Peak",
                                "runs_Sat_Midday",
                                "runs_Sat_Afternoon Peak",
                                "runs_Sat_Evening",
                                "runs_Sun_Night",
                                "runs_Sun_Morning Peak",
                                "runs_Sun_Midday",
                                "runs_Sun_Afternoon Peak",
                                "runs_Sun_Evening",
                                "year")]
names(zone_service) <- gsub(" ","_",names(zone_service))

# Output
la = read.csv("data/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
la = la[,c("LSOA11CD","LAD17NM","RGN11NM")]
la = la[!duplicated(la$LSOA11CD),]

zone_service_out <- left_join(zone_service, la, by = c("zone_id" = "LSOA11CD"))
zone_service_out <- zone_service_out[,c(1:2,18:20,3:17)]

zone_service_out <- zone_service_out[order(zone_service_out$zone_id, zone_service_out$route_type, zone_service_out$year),]
saveRDS(zone_service_out, "data/trips_per_lsoa_by_mode_2004_2023.Rds")



zone_service_wide = tidyr::pivot_wider(zone_service,
                                       names_from = "year",
                                       values_from = names(zone_service)[3:17],
                                       values_fill = 0,
                                       id_cols = c("zone_id","route_type"))
zone_service_wide$change_2007_2019_weekday_AM = round((zone_service_wide$`runs_weekday_Morning_Peak_2019` -
  zone_service_wide$`runs_weekday_Morning_Peak_2007`) / zone_service_wide$`runs_weekday_Morning_Peak_2007` * 100, 2)



zone2 = dplyr::left_join(zone, zone_service_wide[zone_service_wide$route_type == 2, ], by = c("code" =  "zone_id"))
zone3 = dplyr::left_join(zone, zone_service_wide[zone_service_wide$route_type == 3, ], by = c("code" =  "zone_id"))

m2 = tm_shape(zone2) +
  tm_fill("runs_weekday_Morning_Peak_2007",
          style = "quantile",
          n = 10)

m3 = tm_shape(zone3) +
  tm_fill("runs_weekday_Morning_Peak_2007",
          style = "quantile",
          n = 10)





tmap_mode("view")
# m1 = tm_shape(zone2[,"change_2007_2011_Mon_AM"]) +
#   tm_fill("change_2007_2011_Mon_AM",
#           style = "fixed",
#           breaks = c(-100,-50,-20,-10,-5,0,5,10,20,50,100,200,500,Inf),
#           palette = tmaptools::get_brewer_pal("RdBu", n = 13))
# m1


m2 = tm_shape(zone2) +
  tm_fill("change_2007_2022_weekday_AM",
          style = "fixed",
          breaks = c(-100,-50,-20,-10,-5,0,5,10,20,50,100,200,500,Inf),
          palette = tmaptools::get_brewer_pal("RdBu", n = 13),
          popup.vars = c("change_2007_2022_weekday_AM",
                         "runs_weekday_Morning_Peak_2004","runs_weekday_Morning_Peak_2007","runs_weekday_Morning_Peak_2011","runs_weekday_Morning_Peak_2015","runs_weekday_Morning_Peak_2018","runs_weekday_Morning_Peak_2022",
                         "runs_weekday_Midday_2004","runs_weekday_Midday_2007","runs_weekday_Midday_2011","runs_weekday_Midday_2015","runs_weekday_Midday_2018","runs_weekday_Midday_2022"
                         ))

# LA trends

zone_service_la = zone_service_out
#zone_service_la = dplyr::left_join(zone_service_la, la, by = c("zone_id" = "LSOA11CD"))
zone_service_la = group_by(zone_service_la, RGN11NM, year, route_type) %>%
  summarise(tot_weekday_Morning_Peak = sum(runs_weekday_Morning_Peak, na.rm = T))
zone_service_max = group_by(zone_service_la, RGN11NM, route_type) %>%
  summarise(max_weekday_Morning_Peak = max(tot_weekday_Morning_Peak, na.rm = T))

zone_service_la <- left_join(zone_service_la, zone_service_max, by = c("RGN11NM","route_type"))
zone_service_la$percent <- zone_service_la$tot_weekday_Morning_Peak / zone_service_la$max_weekday_Morning_Peak

ggplot(zone_service_la[zone_service_la$route_type == 3, ], aes(x = year, y = percent, color = RGN11NM)) +
  geom_line(lwd = 1) +
  scale_color_brewer(palette="Set3")
