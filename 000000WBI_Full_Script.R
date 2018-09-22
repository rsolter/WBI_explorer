#000000WBI_Full_Script

## libraries ----
library(WDI)
library(dplyr)
library(tidyr)
library(data.table)
library(geojsonio)
library(leaflet)
library(dtwclust)
library(tm)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)
library(tibble)
library(purrr)
library(stringr)
library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(sf) # needed to code dataframes into 'sf' format (st_as_st)


## Import WDI dictionary, African Geo file, ----
setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\WDI R Data\\") 
load(file="WDI_dict.rdata") 
load(file="African_geometries.rdata")


## Pulling wide and raw dataset from WDI API ----

# Setting variables for import
orig_Indicators <- c("NY.GDP.PCAP.PP.CD","SP.DYN.CBRT.IN","EG.ELC.ACCS.ZS","SL.UEM.TOTL.ZS")

WDI_dict %>% group_by(Subject) %>% tally()

poverty_indicators <- WDI_dict %>% dplyr::filter(Subject=="Poverty") %>% dplyr::select(WBI) %>% as.vector()
poverty_indicators_names <- left_join(poverty_indicators,WDI_dict) %>% dplyr::select(WBI_name)




af_country_list<-Af_geo_final$iso2
Years<-2011:2016
# number of clusters
NC<-as.integer(5)

dat2 <- WDI::WDI(indicator=poverty_indicators$WBI, 
                country = af_country_list, 
                start=Years[1], 
                end=Years[length(Years)])


# Adding Empty Eastern Sahara  data
# Adding in missing values for 'EH' - "Eastern Western Sahara"

nrecords <- length(Years)
names(dat2)
EH_metric_col_names<-names(dat2)[4:ncol(dat2)]
EH_metric_col_df<-setnames(data.frame(matrix(ncol=length(EH_metric_col_names),
                           nrow=length(Years))),
         EH_metric_col_names)

EH<-data.frame(iso2c=rep("EH",nrecords),
               country=rep("Eastern Western Sahara",nrecords),
               year=Years,
               EH_metric_col_df)


# Joining 'EH' to 'dat2'
dat_wide<-rbind(dat2,EH)
rm(dat2)
rm(EH)


#save(dat_wide,file="dat_wide.rdata")



## Creating NN Dataset for Map Visualization ----

### Code for creating Nearest Neighbors datasets from dat_wide, af_geo_final
# calculating distance between all countries 
dist_Long_func <- function(df){
  # scaling
  df <- data.frame(df[1:3], apply(df[,4:7], 2, base::scale))
  
  # calculating distances in distance matrix
  df_1 <- fields::rdist(df[ ,4:7])
  
  # adding country names
  row.names(df_1) <- df$country
  colnames(df_1) <- df$country
  
  # transforming into long format
  long_dist <<- reshape::melt(df_1)[melt(upper.tri(df_1))$value, ]
  
  #long_dist <- reshape::melt(df_1)[melt(df_1)$value, ]
  names(long_dist) <- c("Country_Focus","Country_Comp","Distance")
  long_dist
}



# -- apply nn_viz to (dat_wide)
nn_viz <- function(df){
  
  # splitting apart by year into a list
  df <- df %>% dplyr::arrange(year, country)
  df_long <- split(df, df$year)
  
  # applying to each year, combining in a list
  dist_list <- lapply(df_long, dist_Long_func)
  
  # combining all elements in list into one long dataframe
  dist_df <- do.call("rbind",dist_list)
  
  
  # Pulling Row name into df as a year
  dist_df <- tibble::rownames_to_column(dist_df,"Year")
  dist_df$Year <- substr(dist_df$Year,start = 1,stop = 4)
  
  
  # renaming and exporting into nearest-neighbors dataset
  nn_distances_long <- dist_df
  
  # switching Country_Focus, Country_Comp variable types to character 
  nn_distances_long$Country_Focus <- as.character(nn_distances_long$Country_Focus)
  nn_distances_long$Country_Comp <- as.character(nn_distances_long$Country_Comp)
  
  # duplicating records and switching focus, comp columns
  nn_distances_long_dupe <- nn_distances_long
  names(nn_distances_long_dupe) <- c("Year","Country_Comp","Country_Focus","Distance")
  nn_distances_long_dupe <- nn_distances_long_dupe[ ,c(1,3,2,4)]
  
  # binding the two
  nn_distances_long <- rbind(nn_distances_long, nn_distances_long_dupe)
  
  # rejoining iso2 data 
  codes<-dat_wide %>% dplyr::select(country,iso2c) %>% unique()
  nn_distances_long <- dplyr::left_join(nn_distances_long,codes,by=c("Country_Comp"="country"))
  names(nn_distances_long) <- c("Year","Country_Focus","Country_Comp","Distance","Country_Comp_Iso2c")
  
  nn_dist_viz <- dplyr::left_join(nn_distances_long,Af_geo_final,by=c("Country_Comp_Iso2c"="iso2"))
  
  nn_dist_viz <- nn_dist_viz[,c(1:4,9)] 
  
  
  nn_dist_viz <- sf::st_as_sf(nn_dist_viz)
  nn_dist_viz$Year <- as.numeric(nn_dist_viz$Year)
  
  nn_dist_viz <<- nn_dist_viz
  
  # want to rejoin raw data by country_COMP for tool tip
  nn_dist_viz<<-left_join(nn_dist_viz,dat_wide,by=c("Country_Comp"="country","Year"="year"))
  
}


nn_viz(dat_wide)



## Creating List of TimeSeries Data for dtwclust Analysis ----

### Code for creating nested data frame for dtwclust 'proc_data2' from dat_wide

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

complete_countries(dat_wide)

proc_dat2 <- df_tt

#save(proc_dat2,file="list_mv_wb.rdata")




#### Clustering  ----

# NC is the number of cluster var that needs to be passed as an integer
# NC set at top of script

# based off of 
clust_ts_func <- function(l){
  mvc <<- dtwclust::tsclust(l,
                            k=NC,
                            distance= "gak",
                            seed=390)
  
  # returning some information about the clusters
  mvc@clusinfo
  
  # exporting cluster assignments
  cluster_export <<- as.data.frame(cbind(names(mvc@datalist),mvc@cluster))
  names(cluster_export) <<- c("country","Cluster")
  
}

clust_ts_func(proc_dat2)
mvc@clusinfo

## Calculating values to visualize


WDI_trend_calc <- function(matrix){
  Years_list <- Years
  matrix2 <- as.data.frame(cbind(Year=Years_list,matrix))
  matrix2 <- gather(matrix2,metric,value,-Year)
  matrix2 <- matrix2 %>% dplyr::group_by(metric) %>% dplyr::mutate(value_lag=lag(value),
                                                     first_value=first(value))
  matrix2 <- matrix2 %>% dplyr::mutate(RAW=value,
                                YOY=(value-value_lag)/value*100,
                                CUM=(value/first_value)*100)
  matrix2 <- matrix2 %>% dplyr::select(Year,metric,YOY,CUM,RAW)
  matrix2_long <- matrix2 %>% tidyr::gather(measure,value,-Year,-metric)
  return(matrix2_long)
}


# mvc can be used to visualize
mvc_viz <- mvc@datalist %>% 
  purrr::set_names(cluster_export$country) %>%
  tibble::enframe("country") %>%
  mutate(cluster_num=cluster_export$Cluster) %>%
  # something is breaking in the map step below
  mutate(value=lapply(value, WDI_trend_calc)) %>% 
  unnest()


##############################################


# Clustering Viz ----


# Plotting each clusters time series for each metric (average in black) 
mvc_viz_clsut_list <- split(mvc_viz, mvc_viz$cluster_num) 

map(mvc_viz_clsut_list, function(df) {
    cluster_name <- unique(df$cluster_num)
    ggplot(df, aes(x=Year,y=value, colour=country)) + 
      geom_line(data=df[ which(df$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
      stat_summary(data=df[ which(df$measure=="RAW"), ], 
                   fun.y="mean", geom="line", colour="black", size=1.2) +
      facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10") +
      ggtitle(sprintf("Cluster Number %s",cluster_name))
  })

# RAW PLOT
ggplot(mvc_viz_clsut_list[[1]], aes(x=Year,y=value, colour=country)) + 
  geom_line(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="RAW"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10") 

# YOY PLOT
ggplot(mvc_viz_clsut_list[[1]], aes(x=Year,y=value, colour=country)) + 
  geom_line(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="YOY"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="YOY"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal()

# CUM PLOT
ggplot(mvc_viz_clsut_list[[1]], aes(x=Year,y=value, colour=country)) + 
  geom_line(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="CUM"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="CUM"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal() 


library(plotly)


# Plotly version
cmp<-ggplot(mvc_viz_clsut_list[[1]], aes(x=Year,y=value, colour=country)) + 
  geom_line(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="CUM"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=mvc_viz_clsut_list[[1]][ which(mvc_viz_clsut_list[[1]]$measure=="CUM"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(metric~.) + theme_minimal() + ggtitle("Cumulative Change from 1991 Values")

ggplotly(cmp)
