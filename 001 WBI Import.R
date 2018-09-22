# This script uses 'WBI' package to access World Bank API and shapes the resulting dataset

library(WDI)
library(dplyr)
library(tidyr)
library(data.table)

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 


# getting Country.Name/Country.Code Lookup
tmp_World_names <- WDI(indicator=c("NY.GDP.PCAP.PP.CD"), start=1991, end=2015)
worldNames <- tmp_World_names %>% select(iso2c,country) %>% unique()


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

af_country_list <- worldNames %>% filter(country%in%africanCountries)



### Geo Shape File

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
save(africa_geo,file="africa_geo.rdata")



# Selecting Indicators :
Indicators <- c("NY.GDP.PCAP.PP.CD","SP.DYN.CBRT.IN","EG.ELC.ACCS.ZS","SL.UEM.TOTL.ZS")
# GDP per capita, PPP (current international $) - NY.GDP.PCAP.PP.CD
# Birth rate, crude (per 1,000 people) - SP.DYN.CBRT.IN
# Access to electricity (% of population) - EG.ELC.ACCS.ZS
# Unemployment, total (% of total labor force) (modeled ILO estimate) - SL.UEM.TOTL.ZS


# Downloading by indicator
dat = WDI(indicator=Indicators, country = af_country_list$iso2c, start=1991, end=2016)


# Processing

# Checking for missing values 
# dat %>% Hmisc::describe()

# Counting countries with complete information
#length(unique(dat$year)) # 25

# Subsetting down to countries with complete information for all years/metrics in question
dat_comp <- dat[complete.cases(dat), ]
comp_countries<-dat_comp %>% group_by(country) %>% tally() %>% filter(n==25) %>% select(country)
dim(comp_countries) # 44



# Returning a dataset that only includes countries with full sets of data 
dat_f<-dat %>% filter(country%in%comp_countries$country)


# re-naming indicator columns
names(dat_f) <- c("iso2c","country","year","GDP_pC_PPP",
                  "birth_rate_per_K","access_to_electricity_perc","Unemployment_perc")

names(dat) <- c("iso2c","country","year","GDP_pC_PPP",
                  "birth_rate_per_K","access_to_electricity_perc","Unemployment_perc")

worldBank_raw_wide<-dat # includes countries with missing values!

## adding in Mauritania, CAR, W. Sahara, S. Sudan with missing data

numYrs <- length(unique(dat$year))
CRT_CAF<-data.frame(iso2c=c(rep("CA",numYrs),
                   rep("MT",numYrs),
                   rep("WS",numYrs),
                   rep("SS",numYrs)),
           country=c(rep("Central African Rep.",numYrs),
                     rep("Mauritania",numYrs),
                     rep("W. Sahara",numYrs),
                     rep("S. Sudan",numYrs)),
           year=rep(unique(dat$year),4),
           GDP_pC_PPP=NA,
           birth_rate_per_K=NA,
           access_to_electricity_perc=NA,
           Unemployment_perc=NA)


worldBank_raw_wide<-rbind(worldBank_raw_wide,CRT_CAF)


# outputting clean wide data for nearest-neighbor visualization
save(worldBank_raw_wide,file="worldBank_raw_wide.rdata")


rm(comp_countries)
rm(dat)




# creating a nested dataframe , working with data
nested_dat <- dat_f %>% dplyr::select(-iso2c,-year) %>% group_by(country) %>% nest()
rm(dat_f)
proc_dat <- as.list(nested_dat$data)
names(proc_dat) <- nested_dat$country


# 
WBI_dwclust_input <- lapply(proc_dat, as.matrix)

save(WBI_dwclust_input,file="WBI_dwclust_input.rdata")


rm(af_country_list)

rm(dat_comp)
rm(nested_dat)
rm(proc_dat)
