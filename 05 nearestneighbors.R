
# Nearest-neighbors should be broken apart by year
# For each year, we should have a distance matrix
# Ultimately, there should be a long df with four columns:
  # Country, Comparison Country, Year, Distance


library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(purrr)

load("wb_time_series_clean.rdata")
dat_f <- dat_f %>% arrange(year,country)



# test on one year
test <- dat_f %>% filter(year=="1991")

# standardizing data and re-attaching to original df
test2 <- data.frame(test[1:3], apply(test[,4:7], 2, base::scale))


# Calculate euclidian distance

test_dist <- rdist(test2[,4:7]) # full matrix
row.names(test_dist)<-test2$country # giving it names
colnames(test_dist)<-test2$country

# transforming to long
long_dist <- melt(test_dist)[melt(upper.tri(test_dist))$value,]
names(long_dist) <- c("Country1","Country2","Distance")


# viz -- the lower the number, the darker the color

ggplot(long_dist, aes(x=Country1,y=Country2)) + 
  geom_raster(aes(x=Country1,y=Country2, fill=Distance)) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))



# Doing this for the entire df -- all years

country_NN <- function(metrics=c("GDP_pC_PPP","birth_rate_per_K",
                                 "access_to_electricity_perc","Unemployment_perc"),
                       data=DF){
  
}
  

# splitting apart b yyear
dat_f_L <- split(dat_f,dat_f$year)

dist_Long <- function(df){
  df_1 <- fields::rdist(df[ ,4:7])
  row.names(df_1) <- df$country
  colnames(df_1) <- df$country
  
  long_dist <- reshape::melt(df_1)[melt(upper.tri(df_1))$value, ]
  names(long_dist) <- c("Country1","Country2","Distance")
  long_dist
}

lp<-lapply(dat_f_L, dist_Long)

# re-forming into one long dataframe
lp_full <- do.call("rbind", lp)

lp_full <- tibble::rownames_to_column(lp_full,"Year")
lp_full$Year <- substr(lp_full$Year,start = 1,stop = 4)


nn_distances_long <-lp_full

save(nn_distances_long,file = "NN_dataset.rdata")
# Done!

