#### Libraries I need
library(tidyverse)

#### Read in data
dat <- read_csv("big5clean.csv") %>% select(-X1)

#### Scale data
#s_dat <- scale(dat)

#### Kmeans
k <- 8
set.seed(185)
tictoc::tic()
clust <- kmeans(dat, k, algorithm = "Lloyd"
                , iter.max = 5000, nstart = 100
                )
tictoc::toc()
beepr::beep()

#### Look at which responses go into each cluster. Add cluster ID variable
clust$cluster
dat$clusterID <- clust$cluster
s_dat$clusterID <- clust$cluster

#### Find percentage of responses into each cluster
clust$size
(clust_pct <- (clust$size / nrow(dat)) * 100)

#### Loop to get the mahalobis function of each datapoint
  #Split the data set by clusterID
dat_split <- split(dat, dat$clusterID)
  #Create empty dataframe
dat_new <- data.frame(NULL)
for (i in 1:k) {
  #take one of the splits from dat_split
  temp <- dat_split[i] %>% as.data.frame()
  names(temp) <- substring(names(temp),4)
  temp <- temp %>% select(-clusterID)
  temp$md <- mahalanobis(temp, colMeans(temp), var(temp))
  ##Uncomment next two lines to get each cluster's df
  #name <- paste0("k",i)
  #assign(name,temp)
  dat_new <- rbind(dat_new, temp)
}

mean(dat_new$md <= 1.5)
