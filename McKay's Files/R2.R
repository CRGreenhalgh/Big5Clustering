#############################
### Cluster Analysis - R2 ###
#############################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)   # Data Manipulation
library(cluster)     # Clustering Alogrithms
library(factoextra)  # Clustering Algorithms and Visualization
library(mclust)      # Model Based Clustering
library(dbscan)      # Density Based Clustering
library(reshape2)

### Loading Data
sumNorm <- read_csv("normalized.csv") %>% select(-X1)
load("r2list.RData")


r2_list <- list()
set.seed(212021)
pb <- progress::progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 15, clear = FALSE, width= 100)
for(k in 2:16) {
  pb$tick()
  Sys.sleep(1 / 15)
  clusters <- kmeans(sumNorm, centers = k)
  r2_list[[k]] <- numeric()
  for(col in 1:5) {
    temp_mod <- lm(unlist(sumNorm[,col]) ~ as.factor(clusters$cluster))
    r2_list[[k]] <- append(r2_list[[k]],summary(temp_mod)$r.squared )
  }
}

# save(file = "r2list.RData", r2_list)


r2_df <- data.frame(do.call("rbind", r2_list))
colnames(r2_df) <- colnames(sumNorm)
r2_df$num_cluster <- 2:16
r2_df_long <- gather(r2_df, group, r2, EXT:OPN)

r2_df_long %>% ggplot(mapping = aes(x = num_cluster, y = r2, color = group)) + geom_line() + 
  scale_x_continuous(breaks = 2:16) +
  labs(x = "Number of Clusters", title = "R-Squared by Number of Clusters by Group", color = "Group") + 
  theme(plot.title = element_text(hjust = 0.5))

r2_df_long %>% ggplot(mapping = aes(x = num_cluster, y = r2, color = group)) + geom_line() + facet_wrap(vars(group), nrow = 2)+ 
  scale_x_continuous(breaks = 2:16) +
  labs(x = "Number of Clusters", title = "R-Squared by Number of Clusters by Group", color = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None")

nrep <- 10
set.seed(222021)
r2sims <- replicate(nrep, {
  r2_list <- list()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = 15, clear = FALSE, width= 100)
  for(k in 2:16) {
    pb$tick()
    Sys.sleep(1 / 15)
    clusters <- kmeans(sumNorm, centers = k)
    r2_list[[k]] <- numeric()
    for(col in 1:5) {
      temp_mod <- lm(unlist(sumNorm[,col]) ~ as.factor(clusters$cluster))
      r2_list[[k]] <- append(r2_list[[k]],summary(temp_mod)$r.squared )
    }
  }
  r2_df <- data.frame(do.call("rbind", r2_list))
  colnames(r2_df) <- colnames(sumNorm)
  r2_df$num_cluster <- 2:16
  r2_df_long <- gather(r2_df, group, r2, EXT:OPN)
  r2_df_long$r2
}, simplify = "matrix")

r2_sims_df <- cbind(r2_df_long[,1:2],r2sims, rowMeans(r2sims))
colnames(r2_sims_df)[13] <- "Average"
r2_sims_df_long <- r2_sims_df %>% gather(iteration, r2, `1`:`10`)
save(r2_sims_df, file = "r2_sims_df.RData")
save(r2_sims_df_long, file = "r2_sims_df_long.RData")
ggplot() + geom_point(r2_sims_df_long, mapping = aes(x = num_cluster, y = r2, color = group)) + 
  geom_line(r2_sims_df, mapping = aes(x = num_cluster, y = Average, color = group)) +
  scale_x_continuous(breaks = 2:16) +
  labs(x = "Number of Clusters", title = "R-Squared by Number of Clusters by Group", color = "Group") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot() + geom_point(r2_sims_df_long, mapping = aes(x = num_cluster, y = r2, color = group)) + 
  geom_line(r2_sims_df, mapping = aes(x = num_cluster, y = Average, color = group)) +
  scale_x_continuous(breaks = 2:16) + facet_wrap(vars(group), nrow = 2) +
  labs(x = "Number of Clusters", title = "R-Squared by Number of Clusters by Group", color = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None")
