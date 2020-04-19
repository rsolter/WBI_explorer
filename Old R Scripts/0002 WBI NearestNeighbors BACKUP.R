
# Creating Nearest-neighbors dataset to be used in visualization
setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 


library(dplyr) 
library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(sf) # needed to code dataframes into 'sf' format (st_as_st)
library(leaflet)

# ordering
load("dat_wide.rdata")



dat_wide <- dat_wide %>% dplyr::arrange(year,country)

# splitting apart by year -- 
worldBank_raw_long <- split(dat_wide,dat_wide$year)

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
  long_dist <- reshape::melt(df_1)[melt(upper.tri(df_1))$value, ]
  #long_dist <- reshape::melt(df_1)[melt(df_1)$value, ]
  names(long_dist) <- c("Country_Focus","Country_Comp","Distance")
  long_dist
}


nn_viz <- function(df){
  
  # splitting apart by year into a list
  df <- df %>% dplyr::arrange(year, country)
  df_long <- split(df, df$year)
  
  # calculating distance with dist_Long_func and putting back into a list 
  dist_list <- lapply(df_long, dist)
}



# applying to each year, combining in a list
dist_list<-lapply(worldBank_raw_long, dist_Long_func)

# combining all elements in list into one long dataframe
dist_df <- do.call("rbind", dist_list)

# Pulling Row name into df as a year
dist_df <- tibble::rownames_to_column(dist_df,"Year")
dist_df$Year <- substr(dist_df$Year,start = 1,stop = 4)

rm(dist_list)


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

rm(nn_distances_long_dupe)
rm(dist_df)

# rejoining iso2 data 

codes<-dat_wide %>% select(country,iso2c) %>% unique()
nn_distances_long <- left_join(nn_distances_long,codes,by=c("Country_Comp"="country"))
names(nn_distances_long) <- c("Year","Country_Focus","Country_Comp","Distance","Country_Comp_Iso2c")

# Attaching geo graphic shape file

# loading
load(file="African_geometries.rdata")



# the problem with this join, is that not all Countries are used in metric comparison,
# notbaly not Libya, Central Africa Republic, the Horn, etc

nn_dist_viz <- left_join(nn_distances_long,Af_geo_final,by=c("Country_Comp_Iso2c"="iso2"))

nn_dist_viz <- nn_dist_viz[,c(1:4,9)] 


nn_dist_viz <- st_as_sf(nn_dist_viz)
nn_dist_viz$Year <- as.numeric(nn_dist_viz$Year)


### Exporting NN_dataset.rdata ----

save(nn_dist_viz,file = "NN_dataset.rdata")



### Putting in long format for timeseries 




## Vizualization test with Leaflet  ----

# basic example
leaflet(data=Af_geo_final) %>%
  addTiles() %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

# subset with one just Algeria
temp <- nn_dist_viz %>% filter(Year=="2012",Country_Focus=="Benin")

temp_Focus <- nn_dist_viz %>% filter(Year=="2012",Country_Comp=="Benin")


# creating bins for distance color scale
bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = temp$Distance, bins = bins)


# Working leaflet with legend, 
leaflet() %>%
  addTiles() %>% 
  addPolygons(data=temp, fillColor = ~pal(Distance), stroke = FALSE) %>%
  addPolygons(data=temp_Focus, 
              fillColor = "#a6bddb",  
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = .1) %>%
  addLegend(pal=pal, values=temp$Distnace, opacity = 0.7, title = "Similarity",
            position = "bottomright")


####################################################################


# try to add in tool tip with raw scores
load("C:/Users/rsolt008/Documents/personal git/WorldBank Shiny App/dat_wide.rdata")

dat_wide_12<-dat_wide %>% filter(year=="2012")
temp_raw <- left_join(temp,dat_wide_12)
temp_Focus_raw <- left_join(temp_Focus,dat_wide_12)





# creating bins for distance color scale
bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = temp_raw$Distance, bins = bins)


# Working leaflet with legend, popups
leaflet() %>%
  addTiles() %>%
  addPolygons(data=temp_raw,  
              fillColor = ~pal(Distance), 
              stroke = FALSE,
              popup=paste("<b>","Country","</b>", temp_raw$Country_Comp, "<br>",
                          "GDP PPP:", temp_raw$GDP_pC_PPP, "<br>",
                          "Birth Rate:", temp_raw$birth_rate_per_K, "<br>",
                          "Electricity:", temp_raw$access_to_electricity_perc, "<br>",
                          "Unemployed:", temp_raw$Unemployment_perc),
              popupOptions = popupOptions(maxWidth ="100%", 
                                          closeOnClick = TRUE)) %>%
  addPolygons(data=temp_Focus_raw, 
              fillColor = "#a6bddb",  
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = .1) %>%
  addLegend(pal=pal, values=temp_raw$Distnace, opacity = 0.7, title = "Similarity",
            position = "bottomright") 


####################################################################


# setting dimensions of map in Shiny
# https://www.rdocumentation.org/packages/leaflet/versions/2.0.1/topics/leafletOutput

