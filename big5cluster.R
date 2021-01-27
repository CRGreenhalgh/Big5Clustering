#############################
#### Clustering Anaylsis ####
#############################

#### Libraries I Need ####
library(tidyverse)
library(DataExplorer)
library(factoextra)

#### Read and Clean Data ####
dat <- read_tsv("data-final.csv")
ques <- read.delim("codebook.txt")

#### Filter out IPC and remove non-question variables
dat <- dat %>% filter(IPC == 1) %>% 
  select(1:50)

#### Look at missing data. All missing values are given a 0
plot_missing(dat)
table(dat$EXT1)

#### Turn all 0s into NAs
dat[dat == 0] <- NA

#### Count number of NAs in a row and remove any row with at least one NA
dat$na_count <- apply(dat, 1, function(x) sum(is.na(x)))
dat <- dat %>% filter(na_count == 0)

#### Drop NA_Count variable
dat <- dat %>% dplyr::select(-na_count)

####
neg_questions <- c(2,4,6,8,10,12,14,21,23,25,27,32,34,36,38,42,44,46)
dat[,neg_questions] <- (dat[,neg_questions] * -1) + 6
dat <- dat - 3

#### Create sum variables
dat_new <- dat %>%
  mutate(EXT_SUM = select(., EXT1:EXT10) %>% 
           rowSums(), 
         EST_SUM = select(., EST1:EST10) %>% 
           rowSums(),
         AGR_SUM = select(., AGR1:AGR10) %>% 
           rowSums(),
         CSN_SUM = select(., CSN1:CSN10) %>% 
           rowSums(),
         OPN_SUM = select(., OPN1:OPN10) %>% 
           rowSums()) %>% 
  select(EXT_SUM, EST_SUM, AGR_SUM, CSN_SUM, OPN_SUM)

#### Cluster Analysis ####
#### Scale data
s_dat <- scale(dat_new)

# #### Random Sample
# randrows <- sample(1:nrow(dat), 20000)
# new_dat <- dat[randrows,]
# s_new_dat <- scale(new_dat)

#### Kmeans
set.seed(185)
tictoc::tic()
clust <- kmeans(s_dat, 12, algorithm = "Lloyd", iter.max = 5000, nstart = 100)
tictoc::toc()
beepr::beep()

#### Look at which responses go into each cluster. Add cluster ID variable
clust$cluster
dat_new$clusterID <- clust$cluster

#### Find percentage of responses into each cluster
(clust_pct <- (clust$size / nrow(dat_new)) * 100)

#### Calculate mean and sd of each category
  ## Condesnse 
dat_temp <- dat_new %>% filter(clusterID == 11)
mean(dat_temp$EXT_SUM)
dat_plot <- dat_new %>% 
  group_by(clusterID) %>% 
  mutate(EXT_MEAN = mean(EXT_SUM)) %>% 
  mutate(EXT_SD = sd(EXT_SUM)) %>% 
  mutate(EST_MEAN = mean(EST_SUM)) %>% 
  mutate(EST_SD = sd(EST_SUM)) %>% 
  mutate(AGR_MEAN = mean(AGR_SUM)) %>% 
  mutate(AGR_SD = sd(AGR_SUM)) %>% 
  mutate(CSN_MEAN = mean(CSN_SUM)) %>% 
  mutate(CSN_SD = sd(CSN_SUM)) %>% 
  mutate(OPN_MEAN = mean(OPN_SUM)) %>% 
  mutate(OPN_SD = sd(OPN_SUM)) %>% 
  ungroup() %>% 
  select(-c(EXT_SUM,EST_SUM,AGR_SUM,CSN_SUM,OPN_SUM)) %>% 
  distinct()

EXTplot <- ggplot(data = dat_new, mapping = aes(group = clusterID, y = EXT_SUM)) + 
  geom_boxplot()
ESTplot <- ggplot(data = dat_new, mapping = aes(group = clusterID, y = EST_SUM)) + 
  geom_boxplot()
AGRplot <- ggplot(data = dat_new, mapping = aes(group = clusterID, y = AGR_SUM)) + 
  geom_boxplot()
CSNplot <- ggplot(data = dat_new, mapping = aes(group = clusterID, y = CSN_SUM)) + 
  geom_boxplot()
OPNplot <- ggplot(data = dat_new, mapping = aes(group = clusterID, y = OPN_SUM)) + 
  geom_boxplot()

#### visualize
factoextra::fviz_cluster(clust, dat_new, 
             palette = rainbow(12), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
