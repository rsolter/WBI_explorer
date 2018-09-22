library(geojsonio)
library(leaflet)
library(dplyr)
library(geojsonio)
library(sf)



setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App")

# reading in country codes
countryCodes<-read.csv("Country_codes_full.csv", stringsAsFactors = F, header=T)
countryCodes <- countryCodes %>% 
  filter(countryCodes$region=="Africa") %>%
  select(name,iso2,iso3)

# recoding Nambia
countryCodes$iso2 <- ifelse(is.na(countryCodes$iso2),"NA",countryCodes$iso2)



WorldCountry <-geojsonio::geojson_read("countries.geo.json", what = "sp")
WorldCountry<-sf::st_as_sf(WorldCountry)
names(WorldCountry) <- c("iso3","Country","geometry")


# filtering down to African continent
Af_geo <- WorldCountry[WorldCountry$iso3 %in% countryCodes$iso3, ]


# removing "French Southern and Antarctic Lands" 
Af_geo <- Af_geo %>% filter(!iso3=="ATF")

# chancing Af_geo to a character for joins later
Af_geo$Country <- as.character(Af_geo$Country)
Af_geo$iso3 <- as.character(Af_geo$iso3)

# need to add the iso2 to Af_geo "sf" "data.frame"

Af_geo_final<-left_join(Af_geo,countryCodes,by=c("iso3"="iso3"))


save(Af_geo_final,file="African_geometries.rdata")

#Map <- leaflet(Af_geo_final) %>% addTiles() %>% addPolygons()
#Map
