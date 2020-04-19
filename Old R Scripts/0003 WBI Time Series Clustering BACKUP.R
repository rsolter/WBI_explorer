# 0003 WBI Time Series Clustering 

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 
load("dat_wide.rdata")
load("list_mv_wb.rdata")

library(dtwclust)
library(tm)
library(wordcloud)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# ====================================================================================
# Multivariate time series Clustering ----
# ====================================================================================


# Using GAK distance, and four clusters

NC<-as.integer(3)
mvc <- tsclust(proc_dat2, k = NC, distance = "gak", seed = 390)


# cluster info
mvc@clusinfo

mvc@datalist
mvc@datalist$Benin

#exporting country and cluster
names(mvc@datalist)
mvc@cluster

# Exporing Clusters ----

# Exporting clusterrs
cluster_export <- mvc@cluster
names(cluster_export) <- names(mvc@datalist)

cluster_export <- as.data.frame(cbind(names(mvc@datalist),mvc@cluster))
names(cluster_export) <- c("country","Cluster")







####################################################################################
# Describing/Visualizaing Clusters ----
####################################################################################

# End state (?)
# Avg % changes from beginning to end
# trend line of changes
# Click on for more info


# note -- years is coming from a variable defined in 0001 WBI import leaflet (derived from dat_wide)
WDI_trend_calc <- function(matrix){
  matrix2 <- as.data.frame(cbind(Year=years,matrix))
  matrix2 <- gather(matrix2,metric,value,-Year)
  matrix2 <- matrix2 %>% group_by(metric) %>% mutate(value_lag=lag(value),
                                                     first_value=first(value))
  matrix2 <- matrix2 %>% mutate(RAW=value,
                                YOY=(value-value_lag)/value*100,
                                CUM=(value/first_value)*100)
  matrix2 <- matrix2 %>% select(Year,metric,YOY,CUM,RAW)
  matrix2_long <- matrix2 %>% gather(measure,value,-Year,-metric)
  return(matrix2_long)
}


x <- mvc@datalist %>% 
  purrr::set_names(cluster_export$country) %>%
  tibble::enframe("country") %>%
  mutate(cluster_num=cluster_export$Cluster) %>%
  mutate(value=map(value, WDI_trend_calc)) %>% 
  unnest()

# Plotting each clusters time series for each metric (average in black) 
split(x, x$cluster_num) %>%
  map(., function(df) {
    cluster_name <- unique(df$cluster_num)
    ggplot(df, aes(x=Year,y=value, colour=country)) + 
      geom_line(data=df[ which(df$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
      stat_summary(data=df[ which(df$measure=="RAW"), ], 
                   fun.y="mean", geom="line", colour="black", size=1.2) +
      facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10") +
      ggtitle(sprintf("Cluster Number %s",cluster_name))
  })


# Just looking at the cluster means without individual country trends
cluster_mean_viz <- x %>% group_by(cluster_num,Year,metric,measure) %>%
  summarise(value_mean=mean(value,na.rm=T))

ggplot(cluster_mean_viz, aes(x=Year,y=value_mean, colour=cluster_num)) + 
  geom_line(data=cluster_mean_viz[ which(cluster_mean_viz$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=cluster_mean_viz[ which(cluster_mean_viz$measure=="RAW"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10")












# Algeria Example
alg_test<-mvc@datalist[[1]]

ppp_alg <- alg_test[ ,1]

ppp_alg
lag(ppp_alg)
# YoY % change
(ppp_alg-lag(ppp_alg))/ppp_alg*100

# cum % percent 
ppp_alg
ppp_alg/ppp_alg[1]


# Plotting YoY % Change, Cumulative Change
yoy_alg_ppp <-(ppp_alg-lag(ppp_alg))/ppp_alg*100
cum_alg_ppp <-ppp_alg/ppp_alg[1]*100

alg_dev_ppp <- data.frame(Year=1991:2016, YOY=yoy_alg_ppp,CUM=cum_alg_ppp) 
alg_dev_ppp_viz <- alg_dev_ppp %>% gather(metric,value,-Year)

# combined
library(ggplot2)
ggplot(alg_dev_ppp_viz, aes(x=Year,y=value)) + 
  geom_bar(data=alg_dev_ppp_viz[ which(alg_dev_ppp_viz$metric=="YOY"), ], inherit.aes = T, stat="identity") +
  geom_line(data=alg_dev_ppp_viz[ which(alg_dev_ppp_viz$metric=="CUM"), ], inherit.aes = T)

# Just bar chart -- YoY Change
ggplot(alg_dev_ppp_viz, aes(x=Year,y=value)) + 
  geom_bar(data=alg_dev_ppp_viz[ which(alg_dev_ppp_viz$metric=="YOY"), ], inherit.aes = T, stat="identity") +
  labs(x="Year",y="% YoY Difference") + theme_minimal()

# Just line chart -- Cumulative Change
ggplot(alg_dev_ppp_viz, aes(x=Year,y=value)) + 
  geom_line(data=alg_dev_ppp_viz[ which(alg_dev_ppp_viz$metric=="CUM"), ], inherit.aes = T, size=1.5) + 
  labs(x="Year",y="% Difference from 1990 level") + theme_minimal()




####################################################################################
# Calculating for all of Alg's columns  ----
####################################################################################

alg_test2 <- as.data.frame(cbind(Year=1991:2016,alg_test))
alg_test2 <- gather(alg_test2,metric,value,-Year)
alg_test2 <- alg_test2 %>% group_by(metric) %>% mutate(value_lag=lag(value),
                                                       first_value=first(value))
alg_test2 <- alg_test2 %>% mutate(YOY=(value-value_lag)/value*100,
                                  CUM=(value/first_value)*100)
alg_test2 <- alg_test2 %>% select(Year,metric,YOY,CUM)
alg_test_long <- alg_test2 %>% gather(measure,value,-Year,-metric)

alg_test_long$metric <- ifelse(alg_test_long$metric=="NY.GDP.PCAP.PP.CD","GPD PPP",
                        ifelse(alg_test_long$metric=="SP.DYN.CBRT.IN","Birth Rate",
                        ifelse(alg_test_long$metric=="EG.ELC.ACCS.ZS","Access to Electricity",
                        ifelse(alg_test_long$metric=="SL.UEM.TOTL.ZS","Unemployment Rate",NA))))



# faceted plot
ggplot(alg_test_long, aes(x=Year,y=value)) + 
  geom_bar(data=alg_test_long[ which(alg_test_long$measure=="YOY"), ], inherit.aes = T, stat="identity") +
  geom_line(data=alg_test_long[ which(alg_test_long$measure=="CUM"), ], inherit.aes = T, size=1.2) +
  geom_hline(yintercept = 100, linetype=2, colour="green") +
  facet_grid(.~metric) + theme_minimal() + ggtitle("Algeria - 1991 to 2016")



####################################################################################
# Generalizing ----
####################################################################################

# Want to do this for all cumulative group stats, all four metrics

# using cluster_export
cluster_export %>% head()
table(cluster_export$Cluster)

cluster1 <- cluster_export%>%filter(Cluster=="1")

# filtering down to cluster '1' produces a list of length 7
clust_list1 <- mvc@datalist[names(mvc@datalist)%in%cluster1$country]



# WDI_trend_calc(clust_list1[[2]])

clust1_list_trend <- list()
clust1_list_trend <- lapply(clust_list1,WDI_trend_calc)


# Examining Cluster 1

df <- clust1_list_trend[[1]]
country_name <- names(clust1_list_trend[1])
  
ggplot(df, aes(x=Year,y=value)) + 
  geom_bar(data=df[ which(df$measure=="YOY"), ], inherit.aes = T, stat="identity") +
  geom_line(data=df[ which(df$measure=="CUM"), ], inherit.aes = T, size=1.2) +
  geom_hline(yintercept = 100, linetype=2, colour="green") +
  facet_grid(.~metric) + theme_minimal() + ggtitle(paste(country_name," - 1991 to 2016",sep=""))




# Maybe just with the line
ggplot(df, aes(x=Year,y=value)) + 
  geom_line(data=df[ which(df$measure=="CUM"), ], inherit.aes = T, size=1.2) +
  geom_hline(yintercept = 100, linetype=2, colour="green") +
  facet_grid(.~metric) + theme_minimal() + ggtitle(paste(country_name," - 1991 to 2016",sep=""))




# Plot of all countries with mean highlighted
cluster1 <- bind_rows(clust1_list_trend,.id = "country")

ggplot(cluster1, aes(x=Year,y=value, colour=country)) + 
  geom_line(data=cluster1[ which(cluster1$measure=="CUM"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=cluster1[ which(cluster1$measure=="CUM"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  geom_hline(yintercept = 100, linetype=2, colour="green") +
  facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10")


# plotting raw time series
ggplot(cluster1, aes(x=Year,y=value, colour=country)) + 
  geom_line(data=cluster1[ which(cluster1$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=cluster1[ which(cluster1$measure=="RAW"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10")



###########################################################################


cluster_export

x <- mvc@datalist %>% 
  purrr::set_names(cluster_export$country) %>%
  tibble::enframe("country") %>%
  mutate(cluster_num=cluster_export$Cluster) %>%
  mutate(value=map(value, WDI_trend_calc)) %>% 
  unnest()

split(x, x$cluster_num) %>%
  map(., function(df) {
    cluster_name <- unique(df$cluster_num)
    ggplot(df, aes(x=Year,y=value, colour=country)) + 
      geom_line(data=df[ which(df$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
      stat_summary(data=df[ which(df$measure=="RAW"), ], 
                   fun.y="mean", geom="line", colour="black", size=1.2) +
      facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10") +
      ggtitle(sprintf("Cluster Number %s",cluster_name))
  })



cluster_mean_viz <- x %>% group_by(cluster_num,Year,metric,measure) %>%
  summarise(value_mean=mean(value,na.rm=T))

ggplot(cluster_mean_viz, aes(x=Year,y=value_mean, colour=cluster_num)) + 
  geom_line(data=cluster_mean_viz[ which(cluster_mean_viz$measure=="RAW"), ], inherit.aes = T, size=1, alpha=0.5) +
  stat_summary(data=cluster_mean_viz[ which(cluster_mean_viz$measure=="RAW"), ], 
               fun.y="mean", geom="line", colour="black", size=1.2) +
  facet_grid(.~metric) + theme_minimal() + scale_y_continuous(trans = "log10")










