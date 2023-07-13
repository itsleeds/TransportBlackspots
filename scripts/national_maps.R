#National Map


gj = geojsonsf::geojson_sf("data/lsoa_wide.geojson")
sub = gj[,c("tph_weekday_Morning_Peak_2008_3",
           "tph_Sun_Evening_2008_3",
           "tph_weekday_Morning_Peak_2022_3"
           )]

sf::sf_use_s2(FALSE)
sub = sf::st_make_valid(sub)

library(tmap)
tmap_options(check.and.fix = TRUE)
m1 = tm_shape(sub) +
  tm_fill("tph_weekday_Morning_Peak_2008_3",
          style = "fixed",
          title = "",
          breaks = c(0,1,2,5,10,20,40,60,100,2000),
                   palette = c("#a50026",
                     #"#d73027",
                     "#f46d43",
                     "#fdae61",
                     "#fee090",
                     "#e0f3f8",
                     "#abd9e9",
                     "#74add1",
                     "#4575b4",
                     "#313695")) +
  tm_layout(legend.position = c(0,0.2),
            frame = FALSE)
tmap_save(m1, "plots/map_bus_AM_peak_2008.png")

m2 = tm_shape(sub) +
  tm_fill("tph_Sun_Evening_2008_3",
          style = "fixed",
          title = "",
          breaks = c(0,1,2,5,10,20,40,60,100,2000),
          palette = c("#a50026",
                      #"#d73027",
                      "#f46d43",
                      "#fdae61",
                      "#fee090",
                      "#e0f3f8",
                      "#abd9e9",
                      "#74add1",
                      "#4575b4",
                      "#313695")) +
  tm_layout(legend.position = c(0,0.2),
            frame = FALSE)
tmap_save(m2, "plots/map_bus_sun_evening_2008.png")

sub$change <- (sub$tph_weekday_Morning_Peak_2022_3  - sub$tph_weekday_Morning_Peak_2008_3) / sub$tph_weekday_Morning_Peak_2008_3 * 100

m3 = tm_shape(sub) +
  tm_fill("change",
          title = "",
          style = "fixed",
          breaks = c(-100,-70,-50,-30,-5,5,30,50,70,100),
          palette = tmaptools::get_brewer_pal("PRGn", n = 9)) +
  tm_layout(legend.position = c(0,0.2),
            frame = FALSE)
tmap_save(m3, "plots/map_bus_change_AM_peak_2008_2022.png")


#palette = tmaptools::get_brewer_pal("BrBG", n = 9))
