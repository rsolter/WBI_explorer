
# Creating Nearest-neighbors dataset to be used in visualization
setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 


library(dplyr) 
library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(sf) # needed to code dataframes into 'sf' format (st_as_st)
library(leaflet)

# reading in data
load("dat_wide.rdata")
load(file="African_geometries.rdata")



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
  
  nn_dist_viz
}


### Exporting NN_dataset.rdata ----
head()
save(nn_dist_viz,file = "NN_dataset.rdata")




