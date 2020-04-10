# 01 WDI - Import

## Returns two datasets:

#   africa_geo - a collection of shape files for all African nations
#   


library(WDI)
library(tidyverse)
library(tmap)
library(zoo)


## Shape File ----

# Gathering African country names for querying WDI 
tmp_World_names <- WDI(indicator=c("NY.GDP.PCAP.PP.CD"), start=1991, end=2015)
tmp_World_names <- tmp_World_names %>% select(iso2c,country) %>% unique()


# Subsetting African Countries from WBI data
africanCountries <- c("Algeria","Angola","Benin","Botswana",
                      "Burkina Faso","Burundi","Cabo Verde",
                      "Central African Rep","Cameroon","Chad","Congo, Dem. Rep.","Congo, Rep.",
                      "Cote d'Ivoire","Djibouti","Egypt, Arab Rep.","Equatorial Guinea",
                      "Eritrea","Ethiopia","Gabon","Gambia, The",
                      "Ghana","Guinea","Guinea-Bissau","Kenya",
                      "Lesotho","Liberia","Libya","Madagascar",
                      "Malawi","Mali","Mauritania","Morocco",
                      "Mozambique","Namibia","Niger","Nigeria",
                      "Rwanda","Senegal","Seychelles","Sierra Leone","Somalia",
                      "South Africa","South Sudan","Sudan","Swaziland","Tanzania",
                      "Togo","Tunisia","Uganda","Zambia","Zimbabwe") 

af_country_list <- tmp_World_names %>% filter(country%in%africanCountries)


# Getting geographic Shape file for same countries
library(tmap)
data("World")

World$name <- as.character(World$name)

# There are differences in some of the country names. Changing 'World' to match WBI data
World$name <- ifelse(World$name=="Dem. Rep. Congo","Congo, Dem. Rep.",
                     ifelse(World$name=="Congo","Congo, Rep.",
                            ifelse(World$name=="Egypt","Egypt, Arab Rep.",
                                   ifelse(World$name=="Eq. Guinea","Equatorial Guinea",
                                          ifelse(World$name=="Gambia","Gambia, The",World$name)))))

# subsetting down to necessary columns as well as african countries above, and those missing 
africa_geo <- World %>% 
  dplyr::select(iso_a3,name,geometry) %>% 
  filter(name%in%c(africanCountries,"Libya","Mauritania","Central African Rep.",
                   "S. Sudan","Zimbabwe","Somalia","Eritrea","Djibouti","W. Sahara"))

rm(World)
save(africa_geo,file="Datasets/africa_geo.rdata")



## WDI Test File ----


# Indicators can be searched for by keyword
WDIsearch('gdp')[1:10,]
WDI_dict <- WDIsearch()

# Selecting random subset of indicators
Indicators <- c("SI.DST.FRST.20","NY.GDP.PCAP.KD.ZG","EG.ELC.ACCS.ZS","NY.GDP.MKTP.CD")
# Income share held by lowest 20% - NY.GDP.PCAP.PP.CD
# GDP per capita growth (annual %) - NY.GDP.PCAP.KD.ZG
# Access to electricity (% of population) - EG.ELC.ACCS.ZS
# GDP (current US$) - NY.GDP.MKTP.CD


# Downloading by indicator for the last 30 years
dat = WDI(indicator=Indicators, country = af_country_list$iso2c, start=1989, end=2019)



# Using 'na.lofc' from zoo to replaces NAs with previous value recorded
ao <- dat %>% filter(iso2c=="AO")
ao2 <- ao[,4:ncol(ao)]
ao2_fill <- apply(ao2,2,na.locf)

desired_length <- nrow(ao2)

for(i in 1:length(ao2_fill)){
  vec_length<-(length(ao2_fill[[i]]))
  if (vec_length<desired_length){
    padding <- desired_length-vec_length
    ao2_fill[[i]] <- c(rep(NA,padding),ao2_fill[[i]])
  } else 
    ao2_fill[[i]] <- ao2_fill[[i]]
}

ao_f <- cbind(ao[,1:3],ao2_fill)
