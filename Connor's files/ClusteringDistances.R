#### Libraries I Need ####
library(tidyverse)

#### Calculate Distance Between Point and Centroids ####

for (i in nrow(dat_new)) {
  
  for (j in nrow(clust$centers)) {
  
    # kmdist <- function(data,km) {
    #   sqrt(clust$centers[j,] - fitted(km)[i,]) ^ 2)
    # }
    
    kmdist <- function(data,km) {
      sqrt(rowSums(dat_new[colnames(clust$centers)] - fitted(clust)) ^ 2)
    }
    
    dist_temp <- clust$centers[j,] - fitted(km)[i,])
    dat_new[i,ncol(dat_new)+j] <- dist_temp
  }
}



big5dist <- kmdist(dat_new[,1:5], clust)
which(big5dist <= min(big5dist))  
big5dist[87903]
dat_new$clusterID[87903]

