#### Libraries I need ####
library(tidyverse)
library(mclust)

#### Read in Data ####
dat <- read_csv("big5clean_construct.csv")
sc_dat <- read_csv("normalized.csv") 
#load("./Connor's files/SubClusters.RData")

#### Run Mixed Model with 4 clusters ####
k <- 4
mc <- Mclust(sc_dat[,2:6],k)

# Get the probs for each cluster
pr <- mc$z %>% as.data.frame() 

# Get the max for each obs highest prob
pr$Max <- apply(pr, 1, max)

# Create variable for each obs best cluster
pr$ClusterID <- mc$classification

# Loop to get the second highest as well
for (i in 1:nrow(pr)) {
  pr$Max2[i] <- sort(pr[i,1:k], TRUE)[2] %>% as.numeric()
  pr$SecondClusterID[i] <- match(pr$Max2[i], pr[i,1:k]) %>% as.numeric()
}

# Create variable for the two highest probs added together
pr$toptwo <- pr$Max + pr$Max2

# Lastly merge data with the new dataframe
dat_new <- cbind(dat, pr)
sc_dat_new <- cbind(sc_dat, pr)

# Centers for the clusters
centers <- mc$parameters$mean %>% 
  as.data.frame()

#### Split into 4 separate dfs ####
for (i in 1:k) {
  name <- paste0("dat",i,"best")
  temp <- filter(sc_dat_new, ClusterID == i)
  assign(name, temp)
}
best_clusters <- list(dat1best,dat2best,dat3best,dat4best)

# Repeat for second best match clusters
for (i in 1:k) {
  name <- paste0("dat",i,"second")
  temp <- filter(sc_dat_new, SecondClusterID == i)
  assign(name, temp)
}
second_clusters <- list(dat1second,dat2second,dat3second,dat4second)

#### Split each cluster into 2 or 4 df ####
dat_final<- data.frame()
for (i in 1:k) {
  #i <- 1
  mc <- Mclust(best_clusters[i] %>% 
                 as.data.frame() %>% 
                 select(2:6), 
               2)
  pr <- mc$z %>% as.data.frame()
  names(pr)[1:2] <- c("Sub1Best", "Sub2Best")
  pr$SubMaxBest <- apply(pr, 1, max)
  pr$SubMax2Best <- apply(pr, 1, min)
  pr$SubBestClusterID <- mc$classification
  temp <- best_clusters[i] %>% 
    as.data.frame() %>% 
    cbind(pr)
  dat_final <- rbind(dat_final, temp)
}

# Repeat for second best match cluster
dat_temp <- data.frame()
for (i in 1:k) {
  #i <- 1
  mc <- Mclust(second_clusters[i] %>% 
                 as.data.frame() %>% 
                 select(2:6), 
               2)
  pr <- mc$z %>% as.data.frame()
  names(pr)[1:2] <- c("Sub1Second", "Sub2Second")
  pr$SubMaxSecond <- apply(pr, 1, max)
  pr$SubMax2Second <- apply(pr, 1, min)
  pr$SubSecondClusterID <- mc$classification
  temp <- second_clusters[i] %>% 
    as.data.frame() %>% 
    select(X1) %>% 
    cbind(pr)
  dat_temp <- rbind(dat_temp, temp)
}

#### Merge two dfs together ####
dat_final <- merge(dat_final, dat_temp, by = "X1")

#### Conditional Probs ####
dat_final$BB <- dat_final$Max * dat_final$SubMaxBest
dat_final$BS <- dat_final$Max * dat_final$SubMax2Best
dat_final$SB <- dat_final$Max2 * dat_final$SubMaxSecond
dat_final$SS <- dat_final$Max2 * dat_final$SubMax2Second

#### Reorder Columns ####
dat_final <- dat_final %>% select(X1,EXT,EST,AGR,CSN,OPN,ClusterID,SubBestClusterID,SecondClusterID,
                                  SubSecondClusterID,BB,BS,SB,SS,Max,SubMaxBest,SubMax2Best,Max2,
                                  SubMaxSecond,SubMax2Second,V1,V2,V3,V4)
dat$temp <- rowSums(dat_final[11:14])

#### Save data ####
save(best_clusters, centers, dat_final, second_clusters,
     file = "./Connor's files/SubClusters.RData")
