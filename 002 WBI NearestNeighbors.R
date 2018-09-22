
# Creating Nearest-neighbors dataset to be used in visualization
setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 


library(dplyr) 
library(tmap) # for mapping
library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(sf) # needed to code dataframes into 'sf' format (st_as_st)


# ordering
load("worldBank_raw_wide.rdata")
worldBank_raw_wide <- worldBank_raw_wide %>% arrange(year,country)


# splitting apart by year -- 
worldBank_raw_long <- split(worldBank_raw_wide,worldBank_raw_wide$year)

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

# Attaching geo graphic shape file

# loading
load(file="africa_geo.rdata")

load(file="African_geometries.rdata")

# the problem with this join, is that not all Countries are used in metric comparison,
# notbaly not Libya, Central Africa Republic, the Horn, etc

nn_dist_viz <- left_join(nn_distances_long,Af_geo,by=c("Country_Comp"="name"))


unique(Af_geo$name)


nn_dist_viz <- st_as_sf(nn_dist_viz)


save(nn_dist_viz,file = "NN_dataset.rdata")

# Done!





############### Vizualization test

# Let's make a gif for Benin


# outline
# https://mran.revolutionanalytics.com/snapshot/2016-03-22/web/packages/tmap/vignettes/tmap-nutshell.html

benin_geo<-nn_dist_viz %>% filter(Country_Focus=="Benin") %>% arrange(Year)
benin_geo<-st_as_sf(benin_geo)

plots <- list()
yrs <- unique(benin_geo$Year)

for (i in 1:length(yrs)){
  tmp <- benin_geo %>% filter(Year==yrs[[i]])
  plots[[i]] <- tm_shape(tmp) + tm_borders(col = "#d9d9d9") +
    tm_fill("Distance",alpha = 0.8,style = "cont", palette = c("#253494","#2c7fb8","#41b6c4","#7fcdbb","#c7e9b4")) +
    tm_layout(outer.margins = F, frame = F) +
    tm_credits(paste("Distance Map for",tmp$Country_Focus[1],"in",tmp$Year[1],sep=" "), 
               position=c("left","bottom"))
}

names(plots)<-yrs

plots[[1]]




################################################################################################

# Exporting Plots into a gif

#png(file="benin%02d.png")
for (i in 1:length(plots)){
  tmap_save(plots[[i]], filename = paste(sprintf("benin_%03d",i),".png",sep=""))
}
dev.off()

# need to call 'magick' instead of 'convert' for new version of imagemagick (change from tutorial linked above)
# -delay is working to set speed of animation
# -loop sets a finite number of loops (or 0 for infinite)
system("magick -delay 45 -loop 3 *.png Benin_dist_overtime.gif")

# to not leave the directory with the single .png files, I remove them
file.remove(list.files(pattern=".png"))


################################################################################################

# Integrating Leaflet!
# https://rdrr.io/cran/tmap/man/tmap_leaflet.html

library(leaflet)

m <- plots[[4]]
lf <- tmap_leaflet(m)


lf %>% addPolygons(
  label = str_c(
  )
)







addPolygons(
  label=~stringr::str_c(
    NAME, ' ',
    formatC(POPDENSITY, big.mark = ',', format='d')),
  labelOptions= labelOptions(direction = 'auto'),
  weight=1,color='#333333', opacity=1,
  fillColor = ~qpal(POPDENSITY), fillOpacity = 1,
  highlightOptions = highlightOptions(
    color='#000000', weight = 2,
    bringToFront = TRUE, sendToBack = TRUE)
)



