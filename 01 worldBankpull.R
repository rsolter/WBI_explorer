# World Bank Analysis.

  # The objective of this project is to use time-series clustering on World Bank indicators 
  # for countries on the African continent to explore trends in development over time.
  # The hope is that we can pick out different paths that countries have taken which 
  # development practitioners could use to validate their own beliefs about developmental policies.


  # inspired by Hans Rosling's TED talk



# factors to gather
# land policy 
# education levels 
# growth in government programs
# transportation and communication
# expansion of health system
# economic development (per capita)
# GDP
# Inequality




# Steps to take / Scripts to create  
  # 1: Data Gathering - use WBI package to grab data from World Bank API
  # 2: Exploration - Explore the data using visual and descriptive techniques 
  # 3: Modeling. Use different time-series clustering techniques to find clusters
  # 4: Build a shiny app that allows for:
  #    - Selection of an individual country
  #    - Animation over time
  #    - Map that shows countries most and least similar 
  #    - Check boxes for different metric groups to include


# Using the WDI package to access World Bank API

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 

library(WDI)
library(dplyr)
library(tidyr)
library(Hmisc)
library(data.table)


cc<-read.csv("country_codes.csv",header = T,stringsAsFactors = TRUE)

# Will focus on African Countries, and select indicators

africanCountries <- c("Algeria","Angola","Benin","Botswana",
                      "Burkina Faso","Burundi","Cabo Verde",
                      "Central African Republic","Chad","Congo, Dem. Rep.","Congo, Rep.",
                      "Cote d'Ivoire","Djibouti","Egypt, Arab Rep.","Equatorial Guinea",
                      "Eritrea","Ethiopia","Gabon","Gambia, The",
                      "Ghana","Guinea","Guinea-Bissau","Kenya",
                      "Lesotho","Liberia","Libya","Madagascar",
                      "Malawi","Mali","Mauritius","Morocco",
                      "Mozambique","Namibia","Niger","Nigeria",
                      "Rwanda","Senegal","Seychelles","Sierra Leone","Somalia",
                      "South Africa","South Sudan","Sudan","Swaziland","Tanzania",
                      "Togo","Tunisia","Uganda","Zambia","Zimbabwe") 

af_country_list <- cc %>% filter(Country.Name%in%africanCountries)

## Only 50 countries here 

# Indicators :
  # Literacy rate, adult total (% of people ages 15 and above) - SE.ADT.LITR.ZS --- Lost of missing data
  # GDP per capita, PPP (current international $) - NY.GDP.PCAP.PP.CD
  # Birth rate, crude (per 1,000 people) - SP.DYN.CBRT.IN
        # Births attended by skilled health staff (% of total) - SH.STA.BRTC.ZS --- LOTS of missing data
  # Access to electricity (% of population) - EG.ELC.ACCS.ZS
  # Unemployment, total (% of total labor force) (modeled ILO estimate) - SL.UEM.TOTL.ZS
 

Indicators <- c("NY.GDP.PCAP.PP.CD","SP.DYN.CBRT.IN",
                "EG.ELC.ACCS.ZS","SL.UEM.TOTL.ZS")

# Download by indicator
#   Will download all countries from 2015 on by default
dat = WDI(indicator=Indicators, country = af_country_list$Country.Code, start=1991, end=2015)





# Processing

# Checking for missing values and filtering down to countries with complete information
#dat %>% Hmisc::describe()

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
worldBank_raw_wide<-dat_f

# outputting clean wide data for nearest-neighbor visualization
save(worldBank_raw_wide,file="worldBank_raw_wide.rdata")



## Code for creating a wide dataframe using data.table
wide_dat_f<-dcast(setDT(dat_f), country ~ year, value.var = c("GDP_pC_PPP",
                                                              "birth_rate_per_K",
                                                              "access_to_electricity_perc",
                                                              "Unemployment_perc"))

nested_dat <- dat_f %>% dplyr::select(-iso2c,-year) %>% group_by(country) %>% nest()

proc_dat <- as.list(nested_dat$data)
names(proc_dat) <- nested_dat$country

proc_dat2 <- lapply(proc_dat, as.matrix)

save(proc_dat2,file="list_mv_wb.rdata")
