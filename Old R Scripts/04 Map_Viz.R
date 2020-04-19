library(sf)
library(raster)
library(tidyverse)
library(spData)
library(spDataLarge)
library(maps)
library(mapdata)
library(ggmap)
library(rgdal)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications

library(tidyverse)

load("wb_time_series_clean.rdata")

# Countries in Africa from tmap

current.mode <- tmap_mode("plot")
data(World, metro, rivers)

af<-World %>% filter(continent=="Africa")



# Single Country (https://geocompr.robinlovelace.net/adv-map.html#prerequisites-6)

tm_shape(nz) + tm_fill()
tm_shape(nz) + tm_borders()
# 'nz' here is an sf object from


# New Zealand
map_nz <- tm_shape(nz) + tm_polygons()
class(map_nz)
map_nz1 <- map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)

nz_water <- st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")
map_nz2 <- map_nz1 +
  tm_shape(nz_water) + tm_lines()

map_nz3 <- map_nz2 +
  tm_shape(nz_height) + tm_dots()


tmap_arrange(map_nz1, map_nz2, map_nz3)



# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://pvanb.wordpress.com/2010/02/27/a-map-of-deforestation-in-africa-using-r-2/
  
setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 
load("wb_time_series_clean.rdata")
load("NN_dataset.rdata")

glimpse(lp_full)
data("World","metro")

nn_names<-unique(lp_full$Country1) # 44 in total
world_names<-World$name %>% unique() # 177 in total

# are all 44 represented in the 177
table(nn_names%in%world_names) # 8 are not!

nn_names[!nn_names%in%world_names]
# Cabo Verde, Central African Republic, Congo, Dem. Rep., Congo, Rep.
# Egypt, Arab Rep., Equitorial Guinea, Gambia, The, Mauritius

# Cabo Verde is not included
# Central African Republic, is not included
# Congo, Dem. Rep. is Dem. Rep. Congo
# Congo, Rep. is Congo
# Egypt, Arab. Rep., is Egypt
# Equitorial Guinea is Eq. Guinea
# Gambia, The is Gambia,
# Mauritius is not included

lp_full_viz <- lp_full
lp_full_viz$Country1 <- as.character(lp_full_viz$Country1) 
lp_full_viz$Country2 <- as.character(lp_full_viz$Country2) 

lp_full_viz$Country1 <- ifelse(lp_full_viz$Country1=="Congo, Dem. Rep.","Dem. Rep. Congo",
                        ifelse(lp_full_viz$Country1=="Congo, Rep.","Congo",
                        ifelse(lp_full_viz$Country1=="Egypt, Arab Rep.","Egypt",
                        ifelse(lp_full_viz$Country1=="Equatorial Guinea","Eq. Guinea",
                        ifelse(lp_full_viz$Country1=="Gambia, The","Gambia",lp_full_viz$Country1)))))

lp_full_viz$Country2 <- ifelse(lp_full_viz$Country2=="Congo, Dem. Rep.","Dem. Rep. Congo",
                        ifelse(lp_full_viz$Country2=="Congo, Rep.","Congo",
                        ifelse(lp_full_viz$Country2=="Egypt, Arab Rep.","Egypt",
                        ifelse(lp_full_viz$Country2=="Equatorial Guinea","Eq. Guinea",
                        ifelse(lp_full_viz$Country2=="Gambia, The","Gambia",lp_full_viz$Country2)))))

lp_full_viz2 <-lp_full_viz %>% filter(!Country1%in%c("Cabo Verde","Central African Republic","Mauritius")) %>% 
  filter(!Country2%in%c("Cabo Verde","Central African Republic","Mauritius"))

# Check to see if these new names map
lp_full_viz2_names <- unique(lp_full_viz2$Country1)

lp_full_viz2_names[!lp_full_viz2_names%in%world_names] # none!


### Filtering down world into just african countries from above with some others that have no stats

africa <- World %>% filter(name%in%c(lp_full_viz2_names,
          "Libya","Mauritania","Cameroon","Central African Rep.",
          "S. Sudan","Zimbabwe","Somalia","Eritrea","Djibouti","W. Sahara"))

tmap::qtm(africa, borders = "#636363", fill = "#bcbddc") + tm_layout(outer.margins = F, frame = F)


###############
nn_
africa_geo <- africa %>% select(iso_a3,name,geometry)

tmp1995_bn <- lp_full_viz2 %>% filter(Year==1995) %>% filter(Country1=="Benin")


africa_tmp1995_bn <- left_join(africa,tmp1995_bn, by=c("name"="Country2"))

tmap::qtm(africa_tmp1995_bn, borders = "#636363") +
  tm_layout(outer.margins = F, frame = F)


tt <- africa_tmp1995_bn %>% 
  select(-c(sovereignt,continent,area,pop_est,pop_est_dens,economy,
            income_grp,gdp_cap_est,life_exp,well_being,footprint,inequality,HPI,iso_a3))

tm_shape(tt) +
  tm_fill("Distance",alpha = 0.8,style = "cont")


benin <- lp_full_viz2 %>% filter(Country1=="Benin")
benin_geo <- left_join(benin,africa_geo,by=c("Country2"="name"))
benin_geo<-st_as_sf(benin_geo)

benin_plots <- list()
yrs <- unique(benin_geo$Year)
for (i in 1:length(yrs)){
  tmp <- benin_geo %>% filter(Year==yrs[[i]])
  benin_plots[[i]] <- tm_shape(tmp) + tm_fill("Distance",alpha = 0.8,style = "cont")
}
