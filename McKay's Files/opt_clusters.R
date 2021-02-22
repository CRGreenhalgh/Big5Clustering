##################################
### Optimal Number of Clusters ###
##################################

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
sumNorm <- read_csv("normalized.csv") %>% select(-X1)

df <- setNames(as.data.frame(cbind(1:2, matrix(rnorm(10),nrow = 2))),nm =  c("Cluster", "EXT", "AGR", "OPN", "CSN", "EST"))

wide.df <- df %>% gather(Construct, Score, EXT:EST) 

wide.df %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_point()

### Initiating Variables
iterations <- list()
num.clusters <- 4
set.seed(02192021)
k <- kmeans(sumNorm, centers = num.clusters, iter.max = 1000, algorithm = "Lloyd")
# k$centers
# sum(k$withinss) # variance within each cluster
# k$tot.withinss # total within cluster sum of square error
# k$totss

cen <- as.data.frame(k$centers) %>% mutate(Cluster = 1:num.clusters) %>% gather(Construct, Score, EXT:OPN)
cen %>% ggplot(mapping = aes(x = as.factor(Construct), y = Score, color = as.factor(Cluster))) + geom_line() + geom_point(size = 4)
temp3 <- k$centers

### Elbow Method
  
fviz_nbclust(sumNorm, kmeans, method = "wss")

wss.vec <- c()
sil.vec <- c()
set.seed(536)
for(num.clusters in 1:15) {
  k.temp <- kmeans(sumNorm, centers = num.clusters, iter.max = 1000, algorithm = "Lloyd")
  wss.vec <- append(wss.vec, k.temp$tot.withinss)
  sil.vec <- append(sil.vec, silhouette())
}
elbow.df <- data.frame("k" = 1:15, "wss" = wss.vec)
elbow.df[2:12,] %>% ggplot(mapping = aes(x = k, y = wss)) + geom_point(size = 2) + geom_line() + 
  labs(x = "K \n Number of Clusters", y = "Total Within Cluster Sum of Squared Error", title = "Elbow Plot") + 
  theme(plot.title = element_text(hjust = 0.5))

# # # Based on the elbow plot, I think that clusters of 4, 6, or 8 make sense



### Gap Method

gap_stat <- clusGap(sumNorm, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50)


test <- sumNorm
test$Cluster <- k$cluster
cluster.aov <- aov(OPN ~ -1 + as.factor(Cluster), data = test, x = TRUE)
  
# summary(cluster.aov)
  
x <- cluster.aov$x

# cmat <- matrix(c(0,1,-1,0,
#                0,1,0,-1), byrow = TRUE, nrow = 2)
cmat <- cbind(rep(1,num.clusters - 1), -1 * diag(num.clusters - 1))
cmat <- matrix(c(1,-1,0,0,
                 1,0,-1,0,
                 1,0,0,-1), byrow = TRUE, nrow = 3)   ## Testing to see if all of the groups are equal to each other
# cmat <- diag(4)                                       ## Testing if all of the group means are equal to zero
betahat <- solve(t(x) %*% x) %*% t(x) %*% test$OPN
s2 <- (1/(nrow(x) - ncol(x))) * as.numeric(t(test$OPN - x %*% betahat) %*% (test$OPN - x %*% betahat))
f_stat <- as.numeric(t(cmat %*% betahat) %*% solve(cmat %*% solve(t(x) %*% x) %*% t(cmat)) %*% (cmat %*% betahat))/(nrow(cmat) * s2)

pvalue <- 1 - pf(f_stat, nrow(cmat), nrow(x) - ncol(x))
pvalue
