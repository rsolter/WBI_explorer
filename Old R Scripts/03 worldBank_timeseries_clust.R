setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\") 
load("wb_time_series_clean.rdata")
load("list_mv_wb.rdata")


# Considerations in time-series clustering:
  
# 1) Pre-processing of data, possibly changing the decision space
#
# 2) Type of clustering (partitional,hierarchical,etc)
#   -   
#
# 3) Number of desired or expected clusters
#
# 4) Choice of distance measure, along with its parameterization
#
#   - Vector Norms (l1, l2) - will probably go with l2

library(InspectChangepoint)

v <- c(5,7)
# vector norms (magnitudes) can be calculated l1 (manhattan), l2 (euclidian) - sum or sqrt of sum of squares
InspectChangepoint::vector.norm(v,1) # 12
InspectChangepoint::vector.norm(v,2) # 8.6025 


#
#   - Possible distance measures include: DTW, GAK, Soft-DTW
#     - DTW - Dynamic Time Warping: Programming algorithm taht compares two sereis and tries 
#       to find the optimum warping path between them under certain constrains such as monotonicity
#
# 5) Choice of centroid function and its parameterization (may depend on distance. eg. using DTW distance for DTW-based prototypes)
#
# 6) Evaluation of results
#
# 7) Computational cost 

library(dtwclust)
data("uciCT")

# Using CharTraj which is a list of univariate series. 5 for each letter. 
str(CharTraj)
CharTraj[[1]] %>% class() # "numeric"



# ====================================================================================
# Simple partitional clustering with Euclidean distance and PAM centroids
# ====================================================================================

# Reinterpolate to same length
series <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))

# Subset for speed
series <- series[1:20]
labels <- CharTrajLabels[1:20]

# Making many repetitions - 
#     partitional clustering 
#     vector norm of l2
#     run ten times
pc.l2 <- tsclust(series, k = 4L,
                 distance = "L2", centroid = "pam",
                 seed = 3247, trace = TRUE,
                 control = partitional_control(nrep = 10L))

# Cluster validity indices
sapply(pc.l2, cvi, b = labels)




# ====================================================================================
# Hierarchical clustering with Euclidean distance
# ====================================================================================

# Re-use the distance matrix from the previous example (all matrices are the same)
# Use all available linkage methods for function hclust
hc.l2 <- tsclust(series, type = "hierarchical",
                 k = 4L, trace = TRUE,
                 control = hierarchical_control(method = "all",
                                                distmat = pc.l2[[1L]]@distmat))

# Plot the best dendrogram according to variation of information
plot(hc.l2[[which.min(sapply(hc.l2, cvi, b = labels, type = "VI"))]])





# ====================================================================================
# Multivariate time series
# ====================================================================================


# Using GAK distance
mvc <- tsclust(proc_dat2, k = 4L, distance = "gak", seed = 390)

# Note how the variables of each series are appended one after the other in the plot
plot(mvc)
plot(mvc, type="series", clust = 1L)
plot(mvc, type="centroids", clust = 1L)
