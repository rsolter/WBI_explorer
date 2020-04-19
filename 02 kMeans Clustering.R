load(file="Datasets/processed_WDI.rdata")


# Steps

# Break out into dfs for each metric
# run kmeans
# test 1-5 clusters
# elbow test
# finalize kmeans

nested_metrics <- processed_WDI %>% group_by(metric) %>% nest() %>% mutate(plot=NA,clusterData=NA)

clusterData <- list()
plots <- list()

for (u in 1:nrow(nested_metrics)){
  
  tmp_metric_data <- nested_metrics$data[[u]] %>% as.data.frame()
  tmp_metric_name <- nested_metrics$metric[[u]]
  
  tmp_metric_wide <- tmp_metric_data %>% spread(year,value)
  
  
  
  ## Code for running elbow method to determine ideal number of clusters
  
#  wss <- map_dbl(1:5, ~{kmeans(select(tmp_metric_wide,-country), ., nstart=50,iter.max = 15 )$tot.withinss})
#  n_clust <- 1:5
#  elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
#  ggplot(elbow_df) + geom_line(aes(y = wss, x = n_clust), colour = "#82518c") + theme_minimal() # will use 3
 
  
  
  
  # Running code with 3 clusters for now
  clusters <- kmeans(select(tmp_metric_wide, -country), centers = 3)
  
  # Average time serie for clusters
  centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster")
 
  # Adding cluster id to original wide data 
  tmp_metric_wide <- tmp_metric_wide %>% 
    mutate(cluster = clusters$cluster)
   
  # Cluster countries 
  clust_countries <- tmp_metric_wide %>% select(cluster, country) %>% unique() %>% group_by(cluster) %>% nest() %>% mutate(text=NA) %>% arrange(cluster)
  
  for(l in 1:nrow(clust_countries)){
    clust_id <- clust_countries$cluster[[l]]
    clust_country_names <- clust_countries$data[[l]] %>% as.list()
    clust_country_names <- clust_country_names[[1]]
    
    clust_country_names <- paste(clust_country_names,collapse = ', ')
    
    out <- paste("Cluster ",clust_id,": ",clust_country_names,sep="")
    
    clust_countries$text[[l]] <- out
  }
  
  # Cluster info will form the caption for the chart
  cluster_info <- paste(clust_countries$text[[1]],"\n",
                        clust_countries$text[[2]],"\n",
                        clust_countries$text[[3]],sep="")
  
  
  cluster_data <- data.frame(cluster_id=1:(clusters$size %>% length()),
                            cluster_size=clusters$size,
                            cluster_countries=clust_countries$text)
  
  clusterData[[u]] <- cluster_data
  
  # Visualization
  
  # long form
  metric_long <- tmp_metric_wide %>%
    pivot_longer(cols=c(-country, -cluster), names_to = "year", values_to = tmp_metric_name) %>%
    mutate(year = lubridate::ymd(paste0(year, "-01-01"))) %>% as.data.frame()
  
  centers_long <- centers %>%
    pivot_longer(cols = -cluster, names_to = "year", values_to = tmp_metric_name) %>%  
    mutate(year = lubridate::ymd(paste0(year, "-01-01"))) %>% as.data.frame()
  
  
  metric_col_name <- names(metric_long[4])

  # outputting plots
  tmp_plot <- 
    ggplot() +
    geom_line(data = metric_long, aes_string(y = metric_col_name, x = "year", group = "country"), colour = "#82518c") +
    facet_wrap(~cluster, nrow = 1) + 
    geom_line(data = centers_long, aes_string(y = metric_col_name, x = "year", group = "cluster"), col = "#b58900", size = 2) +
    theme_minimal() +
    labs(title = tmp_metric_name, caption = cluster_info[1]) +
    theme(plot.caption = element_text(colour = "white"))
  
  
  plots[[u]] <- tmp_plot

}


nested_metrics$plot <- plots
nested_metrics$clusterData <- clusterData

rm(plots,tmp_metric_data,tmp_metric_wide,tmp_plot,centers,clust_countries,
   clusters,metric_long,clust_id,cluster_info,l,metric_col_name,
   u,tmp_metric_name,out,clust_country_names,centers_long,cluster_data,clusterData)