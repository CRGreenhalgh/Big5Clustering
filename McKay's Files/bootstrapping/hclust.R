##############################
### Hierarichal Clustering ###
##############################

### Questions
# what does a response of 0 mean? My understanding is that you can only answer between 1 and 5
# it appears that there are 1783 NULL responses for each column. What are these from? How do they differ from 0?

### Observations
# So technically, there are over 5^50 possible combinations (which comes out to about 8.8817E34...) 
# There are 6554 duplicated rows (close to 2000 of which are NA's)

### Notes
# Visual of which clusters are closest to another
# Aggregate to 5 groups, explore how much of the population fits into each of the categories
# Principle component analysis - princomp(data)$loadings (each piece is indpendent of each other), $score nx50[,5] defines new scores
# Goals - priority - ti fighter 

### Loading Packages
library(tidyverse)   # Data Manipulation
# library(cluster)     # Clustering Alogrithms
library(factoextra)  # Clustering Algorithms and Visualization
# library(mclust)      # Model Based Clustering
# library(dbscan)      # Density Based Clustering
# library(reshape2)

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Reading in data
dat <- read.csv("normalized.csv")

set.seed(536)
index <- sample(1:nrow(dat), floor(nrow(dat) * .0001), replace = TRUE)

dist <- factoextra::get_dist(dat[index,-1])
trial <- hclust(dist)
div4 <- cutree(trial, k = 4)
factoextra::fviz_nbclust(dat[index,-1], factoextra::hcut, "silhouette") # wss [within sum of squares]

gap_stat <- cluster::clusGap(dat[index,-1], FUN = factoextra::hcut, nstart = 25, K.max = 10, B = 50)
factoextra::fviz_gap_stat(gap_stat)

trial$merge

plot(trial)

