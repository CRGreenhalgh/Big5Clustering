#### Libraries I need
library(tidyverse)

#### Read in data
dat <- read_csv("big5clean_all.csv") %>% select(-X1)

#### Bootstrap the data
n_bstraps <- 5000
boot_index <- sample(1:nrow(dat), n_bstraps, replace = TRUE)
boot_dat <- dat[boot_index,]

#### Scale data
s_dat <- scale(boot_dat)

#### Set seed
set.seed(185)

#### Build distance matrix
dist_mat <- dist(s_dat, method = 'euclidean')

#### Hierarchical cluster
h_clust <- hclust(dist_mat, method = 'complete')
plot(h_clust)

#### Cut tree to k clusters
k <- 4
cut_hclust <- cutree(h_clust, k=k)

#### Visualize
dend <- as.dendrogram(h_clust)
col_dend <- dendextend::color_branches(dend,k=k,h=k)
plot(col_dend)

####
boot_clust <- mutate(boot_dat, cluster = cut_hclust)
count(boot_clust,cluster)

table((boot_clust %>% filter(cluster==4))$EXT1)
