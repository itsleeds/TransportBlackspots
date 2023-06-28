# Get UNCCUT data
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)


car = readRDS("../../mem48/UNCCUT/data/LSOA_cars_per_person_2002_2018_long.Rds")
pt = readRDS("data/trips_per_lsoa_by_mode_2004_2023.Rds")

oac <- read.csv("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
oac <- oac[,c("LSOA11CD","SOAC11NM")]
oac <- oac[!duplicated(oac$LSOA11CD),]

pt = pt[,c("zone_id","route_type","year","tph_weekday_Morning_Peak")]
pt = pt[pt$year %in% c(2008,2018),]

pt = pivot_wider(pt, id_cols = c("zone_id","route_type"),
                 names_from = "year",
                 values_from = "tph_weekday_Morning_Peak")

car = car[car$year %in% c(2008,2018),]
car = car[,c("code","year","cars_per_person"),]
car = pivot_wider(car, id_cols = c("code"),
                 names_from = "year",
                 values_from = "cars_per_person")

pt = pt[pt$zone_id %in% car$code,]
oac = oac[oac$LSOA11CD %in% car$code,]

names(car) = c("code","carpp_2008","carpp_2018")
names(pt) = c("zone_id","route_type","tph_2008","tph_2018")

pt_bus = pt[pt$route_type == 3,]



all_bus = left_join(oac, car, by = c("LSOA11CD" = "code"))
all_bus = left_join(all_bus, pt_bus, by = c("LSOA11CD" = "zone_id"))

cols = c("Cosmopolitan student neighbourhoods" ='#955123',
         "Ageing rural neighbourhoods" ='#007f42',
         "Prospering countryside life" ='#3ea456',
         "Remoter communities" ='#8aca8e',
         "Rural traits" ='#cfe8d1',
         "Achieving neighbourhoods" ='#00498d',
         "Asian traits" ='#2967ad',
         "Highly qualified professionals" ='#7b99c7',
         "Households in terraces and flats" ='#b9c8e1',
         "Challenged white communities" ='#e3ac20',
         "Constrained renters" ='#edca1a',
         "Hampered neighbourhoods" ='#f6e896',
         "Hard-pressed flat dwellers" ='#fcf5d8',
         "Ageing urban communities" ='#e64c2b',
         "Aspiring urban households" ='#ec773c',
         "Comfortable neighbourhoods" ='#faa460',
         "Endeavouring social renters" ='#fcc9a0',
         "Primary sector workers" ='#fee4ce',
         "Inner city cosmopolitan" = '#f79ff0',
         "Urban cultural mix" ='#6a339a',
         "Young ethnic communities" ='#9f84bd',
         "Affluent communities" ='#576362',
         "Ageing suburbanites" ='#a1a2a1',
         "Comfortable suburbia" ='#e5e4e3')


ggplot(all_bus, aes(x = tph_2018, y = carpp_2018, color = SOAC11NM)) +
  geom_point() +
  ylab("Cars per person 2018") +
  xlab("PT trips per hour AM peak 2018") +
  ylim(0,1.5) +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols)

all_bus$change_carpp = (all_bus$carpp_2018 - all_bus$carpp_2008) / all_bus$carpp_2008 * 100
all_bus$change_tph = (all_bus$tph_2018 - all_bus$tph_2008) / all_bus$tph_2008 * 100

quantile(all_bus$change_carpp, probs = seq(0,1,0.1))
quantile(all_bus$change_tph, probs = seq(0,1,0.1), na.rm = TRUE)

ggplot(all_bus[all_bus$change_carpp < 1000,], aes(change_tph, change_carpp)) +
  geom_hex() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_fill_gradientn(colours = c("lightblue","yellow","red")) +


  #geom_point() +
  #geom_smooth() +
  ylab("Change in cars per person") +
  xlab("Change in PT trips per hour") +
  ylim(-100,100) +
  xlim(-100,300)
  #guides(color=guide_legend(title="Area classification", ncol =1)) +
  #scale_color_manual(values=cols)

lsoa = readRDS("data/GB_LSOA_2011_super_generalised.Rds")
lsoa = lsoa[lsoa$code %in% all_bus$LSOA11CD,]
lsoa = left_join(lsoa, all_bus, by = c("code" = "LSOA11CD"))

library(tmap)
tm_shape(lsoa) +
  tm_fill("change_carpp", style = "fixed", breaks = c(-9000,-100,-10,-5,-1,0,1,5,10,100,9000))
