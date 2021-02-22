##################
### Distance 2 ###
##################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)   # Data Manipulation
# library(cluster)     # Clustering Alogrithms
# library(factoextra)  # Clustering Algorithms and Visualization
# library(mclust)      # Model Based Clustering
# library(dbscan)      # Density Based Clustering
# library(reshape2)

### Loading Data
sumNorm <- read_csv("normalized.csv") %>% select(-X1)

### Specifying Variables
num.clusters <- 12


### Other variables
n.row <- nrow(sumNorm)
n.col <- num.clusters
dist.mat <- matrix(numeric(n.row * n.col), nrow = n.row, ncol = n.col)

### clustering
set.seed(02182021)
k <- kmeans(sumNorm, centers = num.clusters, iter.max = 30)
sumNorm$cluster <- k$cluster

# kcenters <- k$centers
# 
# save(kcenters, file = "Mah2Centers.RData")

pb <- progress::progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = num.clusters, clear = FALSE, width= 100)
### Calculating Mahalanobis distance
for(i in 1:num.clusters) {
  pb$tick()
  Sys.sleep(1 / num.clusters)
  
  cov_matrix <- cov(sumNorm[sumNorm$cluster == i,1:5])
  dist.mat[,i] <- apply(sumNorm[,1:5], 1, function(x) sqrt(mahalanobis(x,k$centers[i,],cov = cov_matrix)))
}
i <- 3
cov_matrix <- cov(sumNorm[sumNorm$cluster == i,1:5])

sqrt(mahalanobis(sumNorm[1,1:5],k$centers[i,],cov = cov_matrix))

# save(dist.mat, file = "mah_all.RData")
all_dist <- cbind(sumNorm, dist.mat)
colnames(all_dist)[7:18] <- paste0("C",1:12)
# save(all_dist, file = "all_dist_df.RData")

# jpeg("plots/C12.jpeg", height = 5, width = 7, units = "in", res = 300)
# ggplot(all_dist, mapping = aes(x = C12, fill = as.factor(cluster))) + geom_histogram() + 
#   facet_wrap(vars(cluster), nrow = 3) + labs(title = "Distance from Cluster 12") + 
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "None")
# dev.off()

# # # The Question is how do we develop a metric from 
dist_scores <- 2 * pnorm(-dist.mat)
