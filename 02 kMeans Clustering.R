library(tidyverse)
library(cluster)

load(file="Datasets/processed_WDI.rdata")


# https://www.brodrigues.co/blog/2019-10-12-cluster_ts/
# http://ofdataandscience.blogspot.com/2013/03/capital-bikeshare-time-series-clustering.html
# https://stats.stackexchange.com/questions/131281/dynamic-time-warping-clustering
# https://github.com/SteffenMoritz/imputeTS

# Steps

# Break out into dfs for each Indicator.Code
# run kmeans
# test 1-5 clusters
# elbow test/silhouette test
# finalize kmeans

nested_Indicator.Codes <- processed_WDI2 %>% group_by(Indicator.Code) %>% nest() %>% mutate(plot=NA,clusterData=NA)

clusterData <- list()
plots <- list()

for (u in 1:nrow(nested_Indicator.Codes)){
  
  tmp_Indicator.Code_data <- nested_Indicator.Codes$data[[u]] %>% as.data.frame()
  tmp_Indicator.Code_name <- nested_Indicator.Codes$Indicator.Code[[u]]
  
  tmp_Indicator.Code_wide <- tmp_Indicator.Code_data %>% spread(year,value)
  
  
  #Silhouette analysis for determining the number of clusters
  asw <- numeric(20)
  for (k in 2:20)
    asw[[k]] <- cluster::pam(tmp_Indicator.Code_wide, k) $ silinfo $ avg.width
  k.best <- which.max(asw)
  
  
  ## Code for running elbow method to determine ideal number of clusters -- MANUAL
  #  wss <- map_dbl(1:5, ~{kmeans(select(tmp_Indicator.Code_wide,-Country.Name), ., nstart=50,iter.max = 15 )$tot.withinss})
  #  n_clust <- 1:5
  #  elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
  #  ggplot(elbow_df) + geom_line(aes(y = wss, x = n_clust), colour = "#82518c") + theme_minimal() # will use 3
 


  # Running code with 3 clusters for now
  clusters <- kmeans(select(tmp_Indicator.Code_wide, -Country.Name), centers = k.best)
  
  # Average time serie for clusters
  centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster")
 
  # Adding cluster id to original wide data 
  tmp_Indicator.Code_wide <- tmp_Indicator.Code_wide %>% 
    mutate(cluster = clusters$cluster)
   
  # Cluster countries 
  clust_countries <- tmp_Indicator.Code_wide %>% select(cluster, Country.Name) %>% unique() %>% group_by(cluster) %>% nest() %>% mutate(text=NA) %>% arrange(cluster)
  
  for(l in 1:nrow(clust_countries)){
    clust_id <- clust_countries$cluster[[l]]
    clust_Country.Name_names <- clust_countries$data[[l]] %>% as.list()
    clust_Country.Name_names <- clust_Country.Name_names[[1]]
    
    clust_Country.Name_names <- paste(clust_Country.Name_names,collapse = ', ')
    
    out <- paste("Cluster ",clust_id,": ",clust_Country.Name_names,sep="")
    
    clust_countries$text[[l]] <- out
  }
  
  # Cluster info will form the caption for the chart
  cluster_info <- paste(clust_countries$text,collapse = '\n')[[1]]
  
  
  cluster_data <- data.frame(cluster_id=1:(clusters$size %>% length()),
                            cluster_size=clusters$size,
                            cluster_countries=clust_countries$text)
  
  clusterData[[u]] <- cluster_data
  
  # Visualization
  
  # long form
  Indicator.Code_long <- tmp_Indicator.Code_wide %>%
    pivot_longer(cols=c(-Country.Name, -cluster), names_to = "year", values_to = tmp_Indicator.Code_name) %>%
    mutate(year = lubridate::ymd(paste0(year, "-01-01"))) %>% as.data.frame()
  
  centers_long <- centers %>%
    pivot_longer(cols = -cluster, names_to = "year", values_to = tmp_Indicator.Code_name) %>%  
    mutate(year = lubridate::ymd(paste0(year, "-01-01"))) %>% as.data.frame()
  
  
  Indicator.Code_col_name <- names(Indicator.Code_long[4])

  # outputting plots
  tmp_plot <- 
    ggplot() +
    geom_line(data = Indicator.Code_long, aes_string(y = Indicator.Code_col_name, x = "year", group = "Country.Name"), colour = "#82518c") +
    facet_wrap(~cluster, nrow = 1) + 
    geom_line(data = centers_long, aes_string(y = Indicator.Code_col_name, x = "year", group = "cluster"), col = "#b58900", size = 2) +
    theme_minimal() +
    labs(title = tmp_Indicator.Code_name, caption = cluster_info[1]) +
    theme(plot.caption = element_text(colour = "white"))
  
  
  plots[[u]] <- tmp_plot

}


nested_Indicator.Codes$plot <- plots
nested_Indicator.Codes$clusterData <- clusterData

rm(plots,tmp_Indicator.Code_data,tmp_Indicator.Code_wide,tmp_plot,centers,clust_countries,
   clusters,Indicator.Code_long,clust_id,cluster_info,l,Indicator.Code_col_name,
   u,tmp_Indicator.Code_name,out,clust_Country.Name_names,centers_long,cluster_data,clusterData)
