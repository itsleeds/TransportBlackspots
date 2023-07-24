library(dplyr)
library(sf)
library(tidyr)
library(tmap)
tmap_mode("view")

yrs = c(2004:2011, 2014:2022)

stops_all = list()

for(i in seq_along(yrs)){
  sub = readRDS(paste0("data/stops_per_week_",yrs[i],".Rds"))
  class(sub) = "data.frame"
  sub$year = yrs[i]
  sub = st_as_sf(sub)
  stops_all[[i]] = sub
}

stops_all = bind_rows(stops_all)

stops_geom = stops_all[,c("stop_id","stop_code","stop_name","geometry")]
stops_geom = unique(stops_geom) # Loads of duplicates!
# summary(duplicated(stops_geom$stop_id))
# foo = stops_geom$stop_id[duplicated(stops_geom$stop_id)]
# foo = stops_geom[stops_geom$stop_id %in% foo,]
# foo = foo[order(foo$stop_id),]
stops_geom = stops_geom[!duplicated(stops_geom$stop_id),]

stops_data = st_drop_geometry(stops_all)
stops_data = stops_data[,c("stop_id","stops_per_week", "year")]
names(stops_data)[2] = "stops_pw"

stops_year = pivot_wider(stops_data, names_from = "year", values_from = "stops_pw", names_sep = "_", names_prefix = "stops_pw_")

stops_year = left_join(stops_geom, stops_year, by = "stop_id")
#qtm(stops_year[1:100,])


stops_summary = stops_data %>%
  group_by(year) %>%
  summarise(mean = mean(stops_pw, na.rm = T),
            median = median(stops_pw, na.rm = T),
            max = max(stops_pw, na.rm = T),
            min = min(stops_pw, na.rm = T),
            q25 = quantile(stops_pw, na.rm = T, probs = 0.25),
            q75 = quantile(stops_pw, na.rm = T, probs = 0.75)
            )

plot(stops_summary$year,
     stops_summary$median,
     type = "b",
     ylim = c(0, 700),
     xlab = "Year",
     ylab = "Stops per week")
lines(stops_summary$year,
      stops_summary$q25,
      type = "b",
      col = "red")
lines(stops_summary$year,
      stops_summary$q75,
      type = "b",
      col = "red")

stops_year$ratio = stops_year$stops_pw_2014 / stops_year$stops_pw_2011
#qtm(stops_year, dots.col = "ratio")

