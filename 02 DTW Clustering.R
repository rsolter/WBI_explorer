# 02 WDI - DTW Clustering

library(tidyverse)
library(plotly)
load("Datasets/processed_WDI.rdata")

# https://stats.stackexchange.com/questions/131281/dynamic-time-warping-clustering

# http://ofdataandscience.blogspot.com/2013/03/capital-bikeshare-time-series-clustering.html

# http://ofdataandscience.blogspot.com/2013/03/capital-bikeshare-time-series-clustering.html
  WDI_long <-processed_WDI %>% gather(metric, value, 4:7) %>% select(-iso2c)
  
  # Test plot of Access to electricity (% of population) - EG.ELC.ACCS.ZS
  tmp<- WDI_long %>% filter(metric=="EG.ELC.ACCS.ZS")
  ggplot(tmp,aes(x=year,y=value)) + geom_line(aes(color=country))
  ggplot(tmp,aes(x=year,y=value, group=country)) + geom_line(color="grey",alpha=0.6) + theme_minimal()
  
  
  # normalized data by series
  normal_tmp <- tmp %>% group_by(country,metric) %>% mutate(norm_value=value/max(value,na.rm=T)) %>% select(-value)
  ggplot(normal_tmp,aes(x=year,y=norm_value)) + geom_line(aes(color=country))
  ggplot(normal_tmp,aes(x=year,y=norm_value, group=country)) + geom_line(color="grey",alpha=0.6) + theme_minimal()


  # Counting NAs by year
  yearlyNAs<- normal_tmp %>% group_by(year) %>% summarise(sumNA=sum(is.na(norm_value)))
  View(yearlyNAs) # Start to collect data for the majority of countries in the 90's
  
  # Spreading to wide format 
  wide_normal_tmp <- normal_tmp %>% 
    as.data.frame() %>% 
    filter(year>1995) %>% 
    select(-metric) %>% 
    spread(year,norm_value) 
  
  # removing countries with missing rows
  wide_normal_tmp <- wide_normal_tmp[ complete.cases(wide_normal_tmp), ]

  
  wss <- map_dbl(1:5, ~{kmeans(select(wide_normal_tmp,-country), ., nstart=50,iter.max = 15 )$tot.withinss})
    
  n_clust <- 1:5
  
  elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
  
  ggplot(elbow_df) +
    geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
    theme_minimal() # will use 3
  
  clusters <- kmeans(select(wide_normal_tmp, -country), centers = 3)
  
  
  (centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster"))
  
  
  final_out <- wide_normal_tmp %>% 
    mutate(cluster = clusters$cluster)
  
  
  
  