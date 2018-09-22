# 04 Map Viz Production Code


setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 
library(tmap)

glimpse(nn_distances_long)

# Reading in shape files for mapping
data("World")

# formatting down to 
nn_distances_viz <- nn_distances_long
nn_distances_viz$Country1 <- as.character(nn_distances_viz$Country1) 
nn_distances_viz$Country2 <- as.character(nn_distances_viz$Country2) 

nn_distances_viz$Country1 <- ifelse(nn_distances_viz$Country1=="Congo, Dem. Rep.","Dem. Rep. Congo",
                               ifelse(nn_distances_viz$Country1=="Congo, Rep.","Congo",
                                      ifelse(nn_distances_viz$Country1=="Egypt, Arab Rep.","Egypt",
                                             ifelse(nn_distances_viz$Country1=="Equatorial Guinea","Eq. Guinea",
                                                    ifelse(nn_distances_viz$Country1=="Gambia, The","Gambia",nn_distances_viz$Country1)))))

nn_distances_viz$Country2 <- ifelse(nn_distances_viz$Country2=="Congo, Dem. Rep.","Dem. Rep. Congo",
                               ifelse(nn_distances_viz$Country2=="Congo, Rep.","Congo",
                                      ifelse(nn_distances_viz$Country2=="Egypt, Arab Rep.","Egypt",
                                             ifelse(nn_distances_viz$Country2=="Equatorial Guinea","Eq. Guinea",
                                                    ifelse(nn_distances_viz$Country2=="Gambia, The","Gambia",nn_distances_viz$Country2)))))

nn_distances_viz2 <-nn_distances_viz %>% filter(!Country1%in%c("Cabo Verde","Central African Republic","Mauritius")) %>% 
  filter(!Country2%in%c("Cabo Verde","Central African Republic","Mauritius"))
