# 01 WDI - Import

## Returns two datasets:

#   africa_geo.rdata - a collection of shape files for all African nations
#   processed_WDI.rdata - historical data for select world bank indiators. processed to replace missing values with most recently observed numbers


library(WDI)
library(tidyverse)
library(tmap)
library(zoo)
library(imputeTS)


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

rm(World,africanCountries)
save(africa_geo,file="Datasets/africa_geo.rdata")



## WDI Test File ----


# Indicators can be searched for by keyword
#WDIsearch('gdp')[1:10,]
WDI_dict <- WDIsearch()

# Selecting random subset of indicators
Indicators <- c("SP.DYN.LE00.IN","NY.GDP.PCAP.KD.ZG","NY.GDP.MKTP.CD")
  # Life exepctacy at birth - SP.DYN.LE00.IN
  # GDP per capita growth (annual %) - NY.GDP.PCAP.KD.ZG
  # GDP (current US$) - NY.GDP.MKTP.CD


# Downloading by indicator for the last 50 years
dat <- WDI(indicator=Indicators, country = af_country_list$iso2c, start=1960, end=2017)
dat <- dat %>% select(-iso2c)
rm(af_country_list)

# There is a lot of missing data - %32%
(sum(is.na(dat))/(dim(dat)[1]*dim(dat)[2]))*100



# Imputing missing data using ImputeTS and simple interpolation

WDI <- dat %>% pivot_longer(-c(country,year),names_to = "metric",values_to = "value")

nested_WDI <- WDI %>% group_by(country,metric) %>% nest()

for(k in nrow(nested_WDI)){

  tmp_series <- nested_WDI$data[[k]]
  tmp_series <- as.ts(tmp_series)

  imputed_tmp_series <- imputeTS::na_interpolation(tmp_series)

  tmp_series2 <- as.data.frame(imputed_tmp_series)

  nested_WDI$data[[k]] <- tmp_series2

}

processed_WDI <- nested_WDI %>% unnest(cols = c(data))

# Missing data is down to 12%
(sum(is.na(processed_WDI))/(dim(processed_WDI)[1]*dim(processed_WDI)[2]))*100


# NA count by year
yearlyNAs<- processed_WDI %>% group_by(year) %>% summarise(sumNA=sum(is.na(value)))
View(yearlyNAs) # Start to collect data for the majority of countries in the 90's

# NA count by country
countryNAs<- processed_WDI %>% group_by(country) %>% summarise(sumNA=sum(is.na(value)))
View(countryNAs)


# NA count by Metric
metricNAs<- processed_WDI %>% group_by(metric) %>% summarise(sumNA=sum(is.na(value)))
View(metricNAs)

# NA Count by metric over time
x<-processed_WDI %>% group_by(metric,year) %>% summarise(sumNA=sum(is.na(value)))
ggplot(x, aes(x=year,y=sumNA, group=metric)) + geom_line() + facet_wrap(facets = "metric")

# NA Count by country over time
y<-processed_WDI %>% group_by(country,year) %>% summarise(sumNA=sum(is.na(value)))
ggplot(y, aes(x=year,y=sumNA, group=country)) + geom_line() + facet_wrap(facets = "country")



# Filtering out data past 1990
processed_WDI <- processed_WDI %>% filter(year>1990)
processed_WDI %>% filter(is.na(value))

# Filtering out select countries
processed_WDI <- processed_WDI %>% filter(!country%in%c("Libya","Djibouti","Eritrea","Liberia","Somalia","South Sudan"))

save(processed_WDI,file="Datasets/processed_WDI.rdata")
