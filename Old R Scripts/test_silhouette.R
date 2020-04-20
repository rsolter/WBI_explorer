#Dataset for Clustering
n = 150
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
mydata<-d
#Plot 3X2 plots
attach(mtcars)
par(mfrow=c(3,2))

#Plot the original dataset
plot(mydata$x,mydata$y,main="Original Dataset")

#Scree plot to deterine the number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}   
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

#Silhouette analysis for determining the number of clusters
library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- cluster::pam(mydata, k) $ silinfo $ avg.width
k.best <- which.max(asw)

cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(cluster::pam(d, k.best))

# K-Means Cluster Analysis
fit <- kmeans(mydata,k.best)
mydata 
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, clusterid=fit$cluster)
plot(mydata$x,mydata$y, col = fit$cluster, main="K-means Clustering results")