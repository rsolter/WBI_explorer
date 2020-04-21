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



## Importing WDI Data  ----

# downloading WDI data in bulk
bulkWDIdata<- WDI::WDIbulk()

  save(bulkWDIdata,file="Datasets/bulkWDI.rdata")
  load(file="Datasets/bulkWDI.rdata")
  # isolating raw data for african nations
  dat_bulk <- bulkWDIdata$Data
  dat <- dat_bulk %>% filter(Country.Code%in%africa_geo$iso_a3) %>% select(-Indicator.Name)
  
  # indicator series data
  series <- bulkWDIdata$Series
    # creating a metatopic variable
  series <- series %>%
    mutate(meta_topic=substr(series$Topic,1,str_locate(series$Topic,":"[1])))
  series <- series %>% select(Series.Code,meta_topic,Topic,Indicator.Name,Periodicity) 

  # adding meta data to observe missing
  dat_extra <- left_join(dat,series,by=c("Indicator.Code"="Series.Code"))
  
  
## Missing Data:Subset I ---- 
  # script below subsets down indictors to observations from 1990 onwards which are missing less than 6% of observations
    
  # From the original data, 62% is missing
  (sum(is.na(dat_extra$value))/length(dat_extra$value))*100
  
    
  # By year - data collection has improved, but still missing 45-50% of data in 2000's 
  na_year <- dat_extra %>% group_by(year) %>%
      summarise(missing_data=(sum(is.na(value))/n())*100)
  ggplot(na_year,aes(x=year,y=missing_data)) + geom_point() + theme_light()
  
  
  # By meta topic -- vast majority of topics in the Economic Policy + Debt, Health sections
  dat_extra %>% group_by(meta_topic) %>% tally()
  
  # missing by year/mta topic
  na_meta <- dat_extra %>% 
    #filter(year>1990) %>%
    group_by(year,meta_topic) %>%
    summarise(missing_data=(sum(is.na(value))/n())*100) %>%
    filter(!is.na(meta_topic))
  
  # poverty, gender are missing almost 100%
  ggplot(na_meta,aes(x=year,y=missing_data)) + 
    geom_point() + theme_minimal() +
    facet_wrap(facets = "meta_topic",nrow = 3)
  
  # By indicator, there are over 1,429 indicators
  # filtering by data from 1990 onwards
  na_ind <- dat_extra %>% 
    filter(year>1990) %>%
    group_by(meta_topic,Topic,Indicator.Name,Indicator.Code) %>%
    summarise(missing_data=(sum(is.na(value))/n())*100) 
  
  table(na_ind$missing_data<6) # 161 of 1429 indicators are missing less than %5
  
  # almost all of these 144 values are in teh health or Social Protection and Labor 
  na_ind %>% filter(missing_data<6) %>% group_by(meta_topic) %>% tally()
  
  ind_keep <- na_ind %>% filter(missing_data<6) %>% select(-Indicator.Name,-meta_topic)
  #ind_keep %>% View()


dat_extra_k <- dat_extra %>% filter(Indicator.Code%in%ind_keep$Indicator.Code) %>% filter(year>=1990)

# Countries missing the most data - South Sudan (2011), Eritrea (1993) gained independence much later
dat_extra_k %>% group_by(Country.Name) %>%
  summarise(missing_data=(sum(is.na(value))/n())*100) %>%
  arrange(-missing_data)


## Missing Data:Subset II ----

# subjectively removing indicators which are redundant and annual % growth

wdi_keep <- c("BX.KLT.DINV.CD.WD", #Foreign investment inflow
              "BM.KLT.DINV.CD.WD", #Foreign investment outflow
              "DT.ODA.ALLD.CD", #Net official development assistance + official aid
              "SP.RUR.TOTL.ZS", #% of population that is rural
              "SP.URB.TOTL.IN.ZS", #% of population that is urban
              "SP.DYN.CBRT.IN", #crude birth rate/1000 people
              "SH.HIV.INCD.ZS", #Incidence of HIV/1000 people 
              "SH.DTH.IMRT", #Number of infant deaths
              "SH.DTH.MORT", #Number of under-five deaths
              "SP.POP.0014.TO", #% of pop 0-14 
              "SP.POP.1564.TO", #% of pop 15-64
              "SP.POP.TOTL", #total population
              "IT.CEL.SETS.P2", #Mobile subscriptions/1000 people
              "SL.GDP.PCAP.EM.KD", #GDP per person employed
              "SL.UEM.TOTL.ZS", #Unemployment rate
              "SM.POP.REFG.OR") #Refugee population by country)


dat_final <- dat_extra_k %>% filter(Indicator.Code%in%wdi_keep)              

rm(af_country_list,africa_geo,bulkWDIdata,dat,dat_bulk,dat_extra,dat_extra_k,
   ind_keep,na_ind,na_meta,na_year,series,tmp_World_names,World,
   africanCountries,wdi_keep)

## Imputation ----
               
# Imputing missing data using ImputeTS and simple interpolation

dat_impute <- dat_final %>% select(-Country.Code,-meta_topic,-Topic,-Indicator.Name,-Periodicity)
table(is.na(dat_impute$value))

# (sum(is.na(dat_impute$value))/nrow(dat_impute))*100 # 4.14% missing

nested_WDI <- dat_impute %>% group_by(Country.Name,Indicator.Code) %>% nest() 

for(k in 1:nrow(nested_WDI)){

  tmp_series <- nested_WDI$data[[k]]
  tmp_series <- as.ts(tmp_series)

  #  imputed_tmp_series <- imputeTS::na_mean(imputed_tmp_series) 
  #  imputed_tmp_series <- imputeTS::na_locf(imputed_tmp_series) 
    imputed_tmp_series <- imputeTS::na_interpolation(tmp_series) 
  #  imputed_tmp_series <- imputeTS::na_locf(imputed_tmp_series,option = "nocb") 
  
  tmp_series2 <- as.data.frame(imputed_tmp_series)

  nested_WDI$data[[k]] <- tmp_series2
  
  imputeTS::plotNA.distribution(tmp_series2$value)

}


processed_WDI2 <- nested_WDI %>% unnest()
table(is.na(processed_WDI2$value)) # cleaned up


# Export ----
  
save(processed_WDI2,file="Datasets/processed_WDI.rdata")
rm(tmp_series,tmp_series2,k,imputed_tmp_series,dat_final,dat_impute)

