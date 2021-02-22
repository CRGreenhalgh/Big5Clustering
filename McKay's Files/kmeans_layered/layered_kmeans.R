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
num.cluster <- 4

set.seed(02222021)
k <- kmeans(sumNorm, centers = num.clusters, iter.max = 1000, algorithm = "Lloyd")

cen <- as.data.frame(k$centers) %>% mutate(Cluster = 1:num.clusters) %>% gather(Construct, Score, EXT:OPN)
cen %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)


### Calculating distance
