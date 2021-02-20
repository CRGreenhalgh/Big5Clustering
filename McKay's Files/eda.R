#############################################
### Cluster Analysis for Mental Guru Data ###
#############################################

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
library(cluster)     # Clustering Alogrithms
library(factoextra)  # Clustering Algorithms and Visualization
library(mclust)      # Model Based Clustering
library(dbscan)      # Density Based Clustering
library(reshape2)

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Reading in data
responses <- read.csv("../../data/responses.csv", sep = "\t")
# load("data/trials.RData")

#####################
### Data Cleaning ###
#####################

### Reducing to only responses
res <- responses[,c(1:50,107)]

### filtering by IPC == 1
res <- res %>% filter(IPC == 1) %>% select(-IPC)

### Getting rid of responses with 0's and null responses
res$nullCount <- apply(res,1,function(x) sum(x == "NULL")) #1,141 have all NULL responses, rest are complete
res$zeroCount <- apply(res,1,function(x) sum(x == "0"))    #92,382 are missing at least one response. Of those most are missing 1,2,3,4, and 50 responses

res <- res %>% filter(nullCount == 0, zeroCount == 0) %>% select(-c(nullCount,zeroCount)) #reduced to 603,322 observations

### Changing to numeric format
res <- res %>% apply(2, as.numeric) %>% as.data.frame()

### Replacing Values for negative questions
neg.quest <- c('EXT2','EXT4','EXT6','EXT8','EXT10', # 5
               'EST2','EST4',                       # 2
               'AGR1','AGR3','AGR5','AGR7',         # 4
               'CSN2','CSN4','CSN6','CSN8',         # 4
               'OPN2','OPN4','OPN6')                # 3

pos.quest <- c('EXT1','EXT3','EXT5','EXT7','EXT9',                       # 5
               'EST1','EST3','EST5','EST6','EST7','EST8','EST9','EST10', # 8
               'AGR2','AGR4','AGR6','AGR8','AGR9','AGR10',               # 6
               'CSN1','CSN3','CSN5','CSN7','CSN9','CSN10',               # 6
               'OPN1','OPN3','OPN5','OPN7','OPN8','OPN9','OPN10')        # 7

# res[,"EXT2"] %>% table #1 115440, 2 140936, 3 146777, 4 120122, 5 80047; checking values
res[,neg.quest] <- apply(res[,neg.quest],2, function(x) (-1 * x) + 6)


### Summing values across each group
sumRes <- res %>% mutate(EXT = rowSums(.[1:10]),
                         EST = rowSums(.[11:20]),
                         AGR = rowSums(.[21:30]),
                         CSN = rowSums(.[31:40]),
                         OPN = rowSums(.[41:50])) %>% select(EXT:OPN)

### Normalizing 
sumNorm <- as.data.frame(apply(sumRes,2,function(x) (x - mean(x)) / sd(x)))

### Saving Dataset
write.csv(file = "normalized.csv",sumNorm, row.names = FALSE)

##################
### Clustering ###
##################

sumNorm <- read_csv("normalized.csv")
sumNorm <- sumNorm %>% select(-X1)
### K means
k <- kmeans(sumNorm, centers = 12)
sumNorm$Cluster <- k$cluster

k$centers

summary1 <- sumNorm %>% group_by(Cluster) %>%
  summarize_all(list(mean))
summary2 <- sumNorm %>% group_by(Cluster) %>%
  summarize_all(list(sd))
summary3 <- sumNorm %>% group_by(Cluster) %>%
  summarize_all(list(length))
# summary4 <- sumNorm %>% group_by(Cluster) %>%
#   summarize_all(list(fun = min,lun = max))

s1 <- melt(summary1, id.vars='Cluster', variable.name = "Group", value.name = "Mean")
s2 <- melt(summary2, id.vars='Cluster', value.name = "SD")
s3 <- melt(summary3, id.vars='Cluster', value.name = "Number")

summary <- cbind(s1,"SD" = s2[,3],"Number" = s3[,3])

summary %>% ggplot(mapping = aes(x = as.factor(Cluster), y = Mean, color = as.factor(Group))) + 
  geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD), position = position_dodge(.7), size = .1) + 
  geom_point(aes(y = Mean), position = position_dodge(.7)) + 
  labs(x = "Cluster", title = "Group Averages by Cluster", color = "Group") + 
  theme(plot.title = element_text(hjust = 0.5))


### Finding percent match for each group
sumNorm[1,] %>% mutate(cluster1 = )


clust_dist <- (matrix(as.numeric(sumNorm[1,-6]),byrow = TRUE, ncol = 5, nrow = 12) - (matrix(as.numeric(k$centers), ncol = 5, nrow = 12)))^2 %>% rowSums()
1 - clust_dist / sum(clust_dist)


rank <- sumNorm
rank[,paste0("rank",1:12)] <- 0
store1 <- c()


pb <- progress::progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 100000, clear = FALSE, width= 100)
for(i in 1:100000) {
  pb$tick()
  Sys.sleep(1 / 100000)
  clust_dist <- (matrix(as.numeric(sumNorm[i,-6]),byrow = TRUE, ncol = 5, nrow = 12) - (matrix(as.numeric(k$centers), ncol = 5, nrow = 12)))^2 %>% rowSums()
  temp <- rbind(temp,matrix(clust_dist,nrow = 1)) # 1 - matrix(clust_dist / max(clust_dist),nrow = 1)
}

for(j in 2:7) {
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
load("distances.RData")
sumNorm$Cluster <- apply(store, 1, which.min)

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


