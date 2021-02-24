# !diagnostics off

#######################
### Layered K-Means ###
#######################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)   # Data Manipulation
library(cluster)     # Clustering Alogrithms
library(factoextra)  # Clustering Algorithms and Visualization
# library(mclust)      # Model Based Clustering
# library(dbscan)      # Density Based Clustering
# library(reshape2)

### Loading Data
sumNorm <- read_csv("../data/normalized.csv") %>% select(-X1)

### First Layer Clustering
num.clusters.1 <- 4

num_sd <- 2

set.seed(02222021)
k <- kmeans(sumNorm, centers = num.clusters.1, iter.max = 1000, algorithm = "Lloyd")
k$centers
cen <- as.data.frame(k$centers) %>% mutate(Cluster = 1:num.clusters.1) %>% gather(Construct, Score, EXT:OPN)
cen %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)

sumNorm$cluster <- k$cluster
### Calculating distance
sumNorm$dist <- 0
for(i in 1:num.clusters.1) {
  df <- sumNorm[sumNorm$cluster == i,1:5]
  cov_matrix <- cov(df)
  sumNorm[sumNorm$cluster == i,"dist"] <- apply(df, 1, function(x) sqrt(mahalanobis(x,k$centers[i,],cov = cov_matrix)))
}


### Including all of group in second clustering:
recluster <- sumNorm 
recluster$cluster2 <- 0
num.clusters.2 <- 3
for (k in 1:num.clusters.1) {
  df <- recluster %>% filter(cluster == k) %>% select(EXT:OPN)
  set.seed(02222021)
  ktemp <- kmeans(df, centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
  recluster$cluster2[recluster$cluster == k] <- ktemp$cluster
}
recluster %<>% mutate(cluster_id = paste(cluster, cluster2, sep = "_"))
reclusterSum <- recluster %>% group_by(cluster, cluster2) %>% summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE)
reclusterGathered <- reclusterSum %>% gather(Construct, Value, EXT:OPN) %>% mutate(label = paste(cluster, cluster2, sep = "_"))

# save(recluster, file = "recluster.RData")
# save(reclusterGathered, file = "reclusterGathered.RData")
### BY GROUP
selection1 <- c()
selection2 <- c()
ggplot() +   geom_point(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
                        mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
  geom_line(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
            mapping = aes(x = as.factor(Construct), y = Value, 
                          group = as.factor(label)), size = 2) + 
  geom_point(reclusterGathered, 
                       mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
  labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))

### BY LABEL
label.vec <- c("1_2", "4_3", "1_1")
ggplot() +   geom_point(reclusterGathered %>% filter(label %in% label.vec), 
                        mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
  geom_line(reclusterGathered %>% filter(label %in% label.vec), 
            mapping = aes(x = as.factor(Construct), y = Value, 
                          group = as.factor(label)), size = 2) + 
  geom_point(reclusterGathered, 
             mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
  labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))




### Miscellany, testing
test1 <- sumNorm %>% filter(cluster == 1)
test2 <- sumNorm %>% filter(cluster == 2)
test3 <- sumNorm %>% filter(cluster == 3)
test4 <- sumNorm %>% filter(cluster == 4)

mean(test4$dist < num_sd)

colMeans(test1)

wss.vec <- c()
for(j in 1:20) {
  set.seed(02222021)
  ktemp <- kmeans(test1[,1:5], centers = j, iter.max = 1000, algorithm = "Lloyd")
  wss.vec <- append(wss.vec, ktemp$tot.withinss)
}
data.frame(k = 1:20, wss = wss.vec) %>% ggplot(mapping = aes(x = k, y = wss)) + geom_line() + geom_point(size = 2)

num.clusters.2 <- 3
set.seed(02222021)
k1 <- kmeans(test1[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
cen1 <- as.data.frame(k1$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
cen1 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)

centers1 <- c()
nrep <- 100
set.seed(02222021)
for(i in 1:nrep) {
  k1 <- kmeans(test1[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
  # cen2 <- as.data.frame(k2$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
  # cen2 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
  centers1 <- rbind(centers1, k1$centers)
}
hc <- hclust(dist(centers1))
hc.clusters <- cutree(hc, k = 3)
table(hc.clusters)
plot(hc)
rect.hclust.label(hc, k = 6, border = 2:6, cluster = c(1,6))

hc.clusters2 <- cutree(hc, k = 6)
table(hc.clusters2)

cen.plot <- data.frame(centers1,"Cluster" =  hc.clusters, "Cluster2" = hc.clusters2) %>% gather(Construct, Value, EXT:OPN) %>% 
  group_by(Cluster2) %>% 
  mutate(num = n()/5)
cen.plot %>% filter(Cluster2 %in% c(1,5)) %>% group_by(Cluster2, Construct) %>% summarize(cen.values = mean(Value))

ggplot() + #geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster)),size = 4) + 
  geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster2), size = num))


set.seed(02222021)
k2 <- kmeans(test2[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
cen2 <- as.data.frame(k2$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
cen2 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)

centers2 <- c()
nrep <- 100
for(i in 1:nrep) {
  k2 <- kmeans(test2[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
  # cen2 <- as.data.frame(k2$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
  # cen2 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
  centers2 <- rbind(centers2, k2$centers)
}
hc <- hclust(dist(centers2))
hc.clusters <- cutree(hc, k = 3)
table(hc.clusters)
plot(hc)
rect.hclust(hc, k = 6, border = 2:6)

hc.clusters2 <- cutree(hc, k = 6)
table(hc.clusters2)




cen.plot <- data.frame(centers2,"Cluster" =  hc.clusters, "Cluster2" = hc.clusters2) %>% gather(Construct, Value, EXT:OPN) %>% 
  group_by(Cluster2) %>% 
  mutate(num = n()/5)
ggplot() + #geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster)),size = 4) + 
  geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster2), size = num))





kexam2 <- kmeans(centers2, centers = 6, iter.max = 1000, algorithm = "Lloyd")
kexam2$size
kexam$withinss
kexam2$withinss
kexam$tot.withinss
kexam2$tot.withinss

kexam2$size
set.seed(02222021)
k3 <- kmeans(test3[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
cen3 <- as.data.frame(k3$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
cen3 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)

centers3 <- c()
nrep <- 100
set.seed(02222021)
for(i in 1:nrep) {
  k4 <- kmeans(test3[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
  # cen2 <- as.data.frame(k2$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
  # cen2 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
  centers3 <- rbind(centers3, k3$centers)
}
hc <- hclust(dist(centers3))
hc.clusters <- cutree(hc, k = 3)
table(hc.clusters)
plot(hc)
rect.hclust(hc, k = 6, border = 2:6)

hc.clusters2 <- cutree(hc, k = 6)
table(hc.clusters2)

cen.plot <- data.frame(centers3,"Cluster" =  hc.clusters, "Cluster2" = hc.clusters2) %>% gather(Construct, Value, EXT:OPN) %>% 
  group_by(Cluster2) %>% 
  mutate(num = n()/5)
ggplot() + #geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster)),size = 4) + 
  geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster2), size = num))

set.seed(02222021)
k4 <- kmeans(test4[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
cen4 <- as.data.frame(k4$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
cen4 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)

centers4 <- c()
nrep <- 100
set.seed(02222021)
for(i in 1:nrep) {
  k4 <- kmeans(test4[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
  # cen2 <- as.data.frame(k2$centers) %>% mutate(Cluster = 1:num.clusters.2) %>% gather(Construct, Score, EXT:OPN)
  # cen2 %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
  centers4 <- rbind(centers4, k4$centers)
}
hc <- hclust(dist(centers4))
hc.clusters <- cutree(hc, k = 3)
table(hc.clusters)
plot(hc)
rect.hclust(hc, k = 6, border = 2:6)

hc.clusters2 <- cutree(hc, k = 6)
table(hc.clusters2)

cen.plot <- data.frame(centers4,"Cluster" =  hc.clusters, "Cluster2" = hc.clusters2) %>% gather(Construct, Value, EXT:OPN) %>% 
  group_by(Cluster2) %>% 
  mutate(num = n()/5)
ggplot() + #geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster)),size = 4) + 
  geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster2), size = num))


### Having one big group and sub groups
bigcluster <- sumNorm

# # # Adjusted Rand index

# sumNorm <- sumNorm %>% mutate(level1 = ifelse(dist<= num_sd,TRUE,FALSE))
# perc.vec1 <- append(perc.vec1,sum(sumNorm$level1) / nrow(sumNorm))
# 
# set.seed(02222021)
# k2 <- kmeans(sumNorm %>% filter(level1 == FALSE), centers = level2_num_clusters)
# sumNorm$cluster2 <- 0
# sumNorm$cluster2[sumNorm$level1 == FALSE] <- k2$cluster
# 
# sumNorm$dist2 <- 0
# for(i in 1:level2_num_clusters) {
#   df <- sumNorm[sumNorm$cluster2 == i,1:5]
#   cov_matrix <- cov(df)
#   sumNorm[sumNorm$cluster2 == i,"dist2"] <- apply(df, 1, function(x) sqrt(mahalanobis(x,k2$centers[i,],cov = cov_matrix)))
# }



nrep <- 100
centers_first <- c()
set.seed(02222021)
for(i in 1:nrep) {
  k <- kmeans(sumNorm, centers = num.clusters.1, iter.max = 1000, algorithm = "Lloyd")
  centers_first <- rbind(centers_first, k$centers)
  # cen <- as.data.frame(k$centers) %>% mutate(Cluster = 1:num.clusters.1) %>% gather(Construct, Score, EXT:OPN)
  # cen %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
}
hc <- hclust(dist(centers_first))
hc.clusters <- cutree(hc, k = 4)
table(hc.clusters)
test <- list()
test[["plot"]] 
plot(hc)
rect.hclust(plot(hc), k = 10, border = 2:6)

hc.clusters2 <- cutree(hc, k = 10)
table(hc.clusters2)

cen.plot <- data.frame(centers_first,"Cluster" =  hc.clusters, "Cluster2" = hc.clusters2) %>% gather(Construct, Value, EXT:OPN) %>% 
  group_by(Cluster2) %>% 
  mutate(num = n()/5)
ggplot() + #geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster)),size = 4) + 
  geom_point(cen.plot, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(Cluster2), size = num))
