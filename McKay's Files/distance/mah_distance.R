################
### Distance ###
################

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
input <- read_csv("normalized.csv") %>% select(-X1)
grid <- as.data.frame(expand.grid(level1_num_clusters = c(2,4,6,8), level2_num_clusters = c(4,8,16), 
                    num_sd = seq(1.5,3, by = .125))) %>% filter(level2_num_clusters > level1_num_clusters)
perc.vec1 <- perc.vec2 <- c()
nrep <- nrow(grid)
pb <- progress::progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = nrep, clear = FALSE, width= 100)
for(n in 1:nrow(grid)) {
  pb$tick()
  Sys.sleep(1 / nrep)
  
  sumNorm <- input
  ### Setting Parameters
  level1_num_clusters <- grid[n,1]
  num_sd <- grid[n,3]
  
  level2_num_clusters <- grid[n,2]
  
  ### clustering
  set.seed(02052021)
  k <- kmeans(sumNorm, centers = level1_num_clusters)
  sumNorm$cluster <- k$cluster
  
  ### Calculating Mahalanobis distance
  sumNorm$dist <- 0
  for(i in 1:level1_num_clusters) {
    df <- sumNorm[sumNorm$cluster == i,1:5]
    cov_matrix <- cov(df)
    sumNorm[sumNorm$cluster == i,"dist"] <- apply(df, 1, function(x) sqrt(mahalanobis(x,k$centers[i,],cov = cov_matrix)))
  }
  # beepr::beep(sound = 1)
  # summary(sumNorm$dist)
  # ggplot(sumNorm, mapping = aes(x = dist, fill = as.factor(cluster))) + geom_histogram(color = "black") + facet_wrap(vars(cluster))
  
  
  sumNorm <- sumNorm %>% mutate(level1 = ifelse(dist<= num_sd,TRUE,FALSE))
  perc.vec1 <- append(perc.vec1,sum(sumNorm$level1) / nrow(sumNorm))
  
  set.seed(020520211)
  k2 <- kmeans(sumNorm %>% filter(level1 == FALSE), centers = level2_num_clusters)
  sumNorm$cluster2 <- 0
  sumNorm$cluster2[sumNorm$level1 == FALSE] <- k2$cluster
  
  sumNorm$dist2 <- 0
  for(i in 1:level2_num_clusters) {
    df <- sumNorm[sumNorm$cluster2 == i,1:5]
    cov_matrix <- cov(df)
    sumNorm[sumNorm$cluster2 == i,"dist2"] <- apply(df, 1, function(x) sqrt(mahalanobis(x,k2$centers[i,],cov = cov_matrix)))
  }
  # sumNorm %>% filter(dist2!= 0) %>% ggplot(mapping = aes(x = dist2, fill = as.factor(cluster2))) + geom_histogram(color = "black") + 
  #   facet_wrap(vars(cluster2))
  
  sumNorm <- sumNorm %>% mutate(level2 = ifelse(dist2<= num_sd,TRUE, FALSE))
  # sumNorm %>% filter(level1 == FALSE & level2 == FALSE) %>% nrow
  # sumNorm %>% filter(level1 == FALSE) %>% nrow
  
  # % within groups with parameters specified
  perc.vec2 <- append(perc.vec2,sum(sumNorm$level2) / nrow(sumNorm))
  print(sum(sumNorm$level2) / nrow(sumNorm))
}
results2 <- cbind(grid,"Percent Contained Level 1" = perc.vec1, "Percent Contained Level 2" = perc.vec2)
save(results2, file = "~/workspace/Results2.RData")
write.csv(results2, file = "~/workspace/Results2.csv", row.names = FALSE)
# save(results, file = "Results.RData")
# write.csv(results, file = "Results.csv", row.names = FALSE)
