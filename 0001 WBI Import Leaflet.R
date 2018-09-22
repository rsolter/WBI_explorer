# 0001 WBI Import Leaflet


library(WDI)
library(dplyr)
library(tidyr)
library(data.table)
library(geojsonio)
library(leaflet)

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 


## Import from WBI ----


# Selecting Indicators :
Indicators <- c("NY.GDP.PCAP.PP.CD","SP.DYN.CBRT.IN","EG.ELC.ACCS.ZS","SL.UEM.TOTL.ZS")
# GDP per capita, PPP (current international $) - NY.GDP.PCAP.PP.CD
# Birth rate, crude (per 1,000 people) - SP.DYN.CBRT.IN
# Access to electricity (% of population) - EG.ELC.ACCS.ZS
# Unemployment, total (% of total labor force) (modeled ILO estimate) - SL.UEM.TOTL.ZS

# Country Names

load(file="African_geometries.rdata")
af_country_list<-Af_geo_final$iso2

# Downloading by indicator
dat2 = WDI::WDI(indicator=Indicators, country = af_country_list, start=1991, end=2016)

# Adding in missing values for 'EH' - "Eastern Western Sahara"
nrecords <- length(1991:2016)
EH<-data.frame(iso2c=rep("EH",nrecords),
             country=rep("Eastern Western Sahara",nrecords),
             year=c(1991:2016),
             NY.GDP.PCAP.PP.CD=NA,
             SP.DYN.CBRT.IN=NA,
             EG.ELC.ACCS.ZS=NA,
             SL.UEM.TOTL.ZS=NA)

dat_wide<-rbind(dat2,EH)
rm(dat2)
rm(EH)


# rounding 
dat_wide$NY.GDP.PCAP.PP.CD <- round(dat_wide$NY.GDP.PCAP.PP.CD,1)
dat_wide$SP.DYN.CBRT.IN <- round(dat_wide$SP.DYN.CBRT.IN,1)
dat_wide$EG.ELC.ACCS.ZS <- round(dat_wide$EG.ELC.ACCS.ZS,1)
dat_wide$SL.UEM.TOTL.ZS <- round(dat_wide$SL.UEM.TOTL.ZS,1)

save(dat_wide,file="dat_wide.rdata")
# load(file="dat_wide.rdata")

years<-unique(dat_wide$year)



### Code for creating nested data frame for dtwclust 'proc_data2'

# 'complete_countries' takes a raw, wide WDI dataset and converts into a list of matrixes for dtwclust
# countries without complete observations for the metrics chosen are dropped


complete_countries <- function(df){ 
  incompleteCountries <- df[!complete.cases(df), ] %>% dplyr::select(country) %>% unique()
  complete_countries_list<-df %>% 
    dplyr::filter(!country%in%incompleteCountries$country) %>%
    dplyr::arrange(country,year) %>%
    dplyr::select(-year) %>%
    dplyr::group_by(iso2c,country) %>% nest()
  
  df_tt <- as.list(complete_countries_list$data)
  names(df_tt) <- complete_countries_list$country
  
  df_tt <<- lapply(df_tt, as.matrix)
  
  # Number of dropeed countries
  orig_country_N <- length(unique(df$country))
  new_country_N <- length(df_tt)
  lost_country_N <- orig_country_N-new_country_N

  
  # Names of dropped countries 
  all_cc <- unique(df$country)
  new_cc <- names(df_tt)
  dropped_cc <<- all_cc[!all_cc%in%new_cc]

  print(paste("Dropped ",lost_country_N," of ",orig_country_N," countries for non-completeness :",sep=""))
  print(dropped_cc)
  
  }


proc_dat2 <- df_tt

save(proc_dat2,file="list_mv_wb.rdata")




