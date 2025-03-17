lsoa21_buses_2010 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2010)
lsoa21_buses_2010_new <- make_simplified_bustrips_lsoa21_single_year(data_year = 2010, new = TRUE)

lsoa21_buses_2024 <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024)
lsoa21_buses_2024_new <- make_simplified_bustrips_lsoa21_single_year(data_year = 2024, new = TRUE)

plot(lsoa21_buses_2024$tph_weekday_daytime,
     lsoa21_buses_2024_new$tph_weekday_daytime)
plot(lsoa21_buses_2024$tph_weekday_Evening,
     lsoa21_buses_2024_new$tph_weekday_Evening)

plot(lsoa21_buses_2024$tph_Sat_daytime,
     lsoa21_buses_2024_new$tph_Sat_daytime)
plot(lsoa21_buses_2024$tph_Sat_Evening,
     lsoa21_buses_2024_new$tph_Sat_Evening)

plot(lsoa21_buses_2024$tph_Sun_daytime,
     lsoa21_buses_2024_new$tph_Sun_daytime)
plot(lsoa21_buses_2024$tph_Sun_Evening,
     lsoa21_buses_2024_new$tph_Sun_Evening)

plot(lsoa21_buses_2010$tph_weekday_daytime,
     lsoa21_buses_2010_new$tph_weekday_daytime)
plot(lsoa21_buses_2010$tph_weekday_Evening,
     lsoa21_buses_2010_new$tph_weekday_Evening)

plot(lsoa21_buses_2010$tph_Sat_daytime,
     lsoa21_buses_2010_new$tph_Sat_daytime)
plot(lsoa21_buses_2010$tph_Sat_Evening,
     lsoa21_buses_2010_new$tph_Sat_Evening)

plot(lsoa21_buses_2010$tph_Sun_daytime,
     lsoa21_buses_2010_new$tph_Sun_daytime)
plot(lsoa21_buses_2010$tph_Sun_Evening,
     lsoa21_buses_2010_new$tph_Sun_Evening)

cor(lsoa21_buses_2010$tph_weekday_daytime,
    lsoa21_buses_2010_new$tph_weekday_daytime,
    use = "complete.obs")
cor(lsoa21_buses_2010$tph_weekday_Evening,
     lsoa21_buses_2010_new$tph_weekday_Evening,
    use = "complete.obs")
cor(lsoa21_buses_2010$tph_weekday_Night,
    lsoa21_buses_2010_new$tph_weekday_Night,
    use = "complete.obs")

cor(lsoa21_buses_2010$tph_Sat_daytime,
     lsoa21_buses_2010_new$tph_Sat_daytime,
     use = "complete.obs")
cor(lsoa21_buses_2010$tph_Sat_Evening,
     lsoa21_buses_2010_new$tph_Sat_Evening,
     use = "complete.obs")

cor(lsoa21_buses_2010$tph_Sun_daytime,
     lsoa21_buses_2010_new$tph_Sun_daytime,
     use = "complete.obs")
cor(lsoa21_buses_2010$tph_Sun_Evening,
     lsoa21_buses_2010_new$tph_Sun_Evening,
     use = "complete.obs")
cor(lsoa21_buses_2010$tph_Sun_Night,
    lsoa21_buses_2010_new$tph_Sun_Night,
    use = "complete.obs")
