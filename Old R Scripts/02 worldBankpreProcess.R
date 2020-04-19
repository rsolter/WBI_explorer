setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 
library(tidyr)
library(data.table)

load("wb_time_series_clean.rdata")


# Need to process for time series clustering


# Need a list of matrices for multivariate analysis (https://rdrr.io/cran/dtwclust/man/tsclust.html) 


## Code for creating a wide dataframe using data.table
wide_dat_f<-dcast(setDT(worldBank_raw_wide), country ~ year, value.var = c("GDP_pC_PPP",
                                                  "birth_rate_per_K",
                                                  "access_to_electricity_perc",
                                                  "Unemployment_perc"))

nested_dat <- dat_f %>% dplyr::select(-iso2c,-year) %>% group_by(country) %>% nest()

proc_dat <- as.list(nested_dat$data)
names(proc_dat) <- nested_dat$country

proc_dat2 <- lapply(proc_dat, as.matrix)

save(proc_dat2,file="list_mv_wb.rdata")



