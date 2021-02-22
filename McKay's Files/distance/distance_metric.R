###################################
### Developing Distance Measure ###
###################################

### Loading Packages
library(tidyverse)   # Data Manipulation
# library(cluster)     # Clustering Alogrithms
# library(factoextra)  # Clustering Algorithms and Visualization
# library(mclust)      # Model Based Clustering
# library(dbscan)      # Density Based Clustering
# library(reshape2)

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Developing Distances Metric
sumNorm <- read_csv("normalized.csv")
sumNorm <- sumNorm %>% select(-X1)

# finding good starting centers
set.seed(02082021)
ktest <- kmeans(sumNorm, centers = 12, iter.max = 50)
# sum(duplicated(ktest$cluster) == FALSE)
which(duplicated(ktest$cluster) == FALSE) #c(1,2,4,5,6,8,15,16,18,21,26,29)
paste(which(duplicated(ktest$cluster) == FALSE), collapse = ",")
initial_index <- c(1,2,4,5,6,8,15,16,18,21,26,29)

set.seed(02082021)
k <- kmeans(sumNorm, centers = sumNorm[initial_index,], iter.max = 50)
sumNorm$Cluster <- k$cluster
sumNorm %>% head
temp1 <- k$cluster
temp2 <- k$cluster

sum(temp1 == temp2)

for(j in 1:7) {
  temp <- c()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = 100000, clear = FALSE, width= 100)
  start <- ((j-1) * 100000) + 1
  end <- j * 100000
  for(i in start:end) {
    pb$tick()
    Sys.sleep(1 / 100000)
    clust_dist <- (matrix(as.numeric(sumNorm[i,-6]),byrow = TRUE, ncol = 5, nrow = 12) - (matrix(as.numeric(k$centers), ncol = 5, nrow = 12)))^2 %>% rowSums()
    temp <- rbind(temp,matrix(clust_dist,nrow = 1)) # 1 - matrix(clust_dist / max(clust_dist),nrow = 1)
  }
  assign(paste0("store",j),temp)
}
store <- rbind(store1, store2, store3, store4, store5, store6, store7[!is.na(store7[,1]),] )

save(store, file = "distances.RData")
load("distances_noseed.RData")

### Average distance - 7.969148
dist_avg <- mean(store)
dist_sd <- sd(store)
ks.test(sqrt(store), "pnorm")
### Average distance to each cluster center - c(9.628, 7.473, 6.778, 9.842, 6.972, 8.628, 8.004, 6.587, 8.222, 9.037, 7.263, 7.198)
paste(round(apply(store, 2, mean),3), collapse = ", ")
### Standard deviation of each cluster center - c(6.625, 4.921, 4.473, 5.904, 4.431, 4.614, 4.647, 4.116, 5.224, 5.143, 4.736, 4.989)
paste(round(apply(store,2, sd),3), collapse = ", ")
sumNorm$Cluster <- apply(store, 1, which.min)

store_df <- as.data.frame(store) 
colnames(store_df) <- paste0("Cluster ",1:12)
store_df <- store_df %>% gather(cluster,dist)
store_df %>% ggplot(mapping = aes(x = dist, fill = cluster)) + geom_histogram(color = "black") + 
  facet_wrap(vars(cluster)) + labs(x = "Distance", y = "Frequency", title = "Histograms of Distance by Cluster") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None")

store_df %>% ggplot(mapping = aes(x = dist)) + geom_histogram(fill = "slateblue", color = "black") + 
  labs(x = "Distance", y = "Frequency", title = "Histogram of Distance") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None")



test <- matrix(c(1,2,3), byrow = TRUE, nrow = 3, ncol = 3)
apply((test),c(1,2),function(x) (x - mean(test))/sd(test))


store_sd <- apply(store[1:2,], c(1,2), function(x) (x - mean(store))/sd(store))
save(store_sd, file = "distances_sd.RData")

store_sd <- (store - mean(store))/sd(store)


  weight <- 1/ store
weightNorm <- apply(as.data.frame(weight),1,function(x) x/sum(x))
# Distances From Each Cluster
store[1,]

# Originally Proposed Metric
1 - store[1,]/sum(store[1,])

# 1 - Distance/Max(Distance) of point
1 - store[1,]/max(store[1,])

# Standardized "Fit" to each cluster
- (store[1,] - mean(store[1,])) / sd(store[1,])



apply(rank[1:626,7:18],1,min) %>% which.max()
apply(rank[1:626,7:18],1,max) %>% which.max()

min(rank[1:626,7:18])


# fviz_cluster(k, data = sumNorm)

k$centers



# distance <- dist(sumNorm)
set.seed(536)
index <- sample(1:nrow(sumNorm), nrow(sumNorm) * .1, replace = FALSE)
distance <- get_dist(sumNorm[index,])
hc.c <- hclust(distance)




# 
# apply(res, 2, function(x) sum(x == "NULL"))
# apply(res, 2, function(x) sum(x == "0"))
# sum(apply(res, 2, function(x) sum(x == "NULL"|x == "0")))
# for(i in 1:50) res[,i][res[,i] == "NULL"] <- 99
# 
# as.matrix(res) %>% table
# 
# 
# res2 <- apply(res,2, as.numeric)
# 
# res2[,10] %>% table
# 
# ### Removing NULL Values
# res2 <- res2 %>% as.data.frame %>% dplyr::filter(EXT1 != 99)
# 
# ### Normalizing
# norm <- apply(res2, 2, function(x) (x - mean(x))/(sd(x)))
# distance <- dist(norm) # To calculate Euclidean Distance
# 
# hc.c <- hclust(distance)
# #plot(hc.c) #probably don't want to do this with so many observations
# 
# hc.a <- hclust(distance, method = "average")
# 
# # Cluster Membership
# member.c <- cutree(hc.c, 12)
# member.a <- cutree(hc.a, 12)
# table(member.c, member.a)
# 
# # Cluster Means
# aggregate(norm, list(member.c),mean)
# 
# ?duplicated
# sum(duplicated(res2))
# 
# ## Try this
# try_this <- do.call(rbind,trials12[[i]][2])
# clust_nums <- t(sapply(trials12, function(x) do.call(rbind,x[2])))
# clust_averages <- lapply(trials12, function(x) x[1])
# 
# clust_averages[[2]][1]
# 
# clust_averages1 <- list()
# clust_averagesdf <- c()
# for(i in 1:10) {
#   clust_averages1[[i]] <- clust_averages[[i]][[1]] %>% dplyr::mutate(iteration_num = i)
#   clust_averagesdf <- rbind(clust_averagesdf,clust_averages[[i]][[1]] %>% dplyr::mutate(iteration_num = i))
# }

x <- as.data.frame(cbind(rnorm(10, 0, 12), rnorm(10,1,3)))
cov(x)
