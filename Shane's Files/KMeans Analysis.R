# K Means Analysis

library(tidyverse)
library(reshape2)

setwd("C:/Users/shane/Google Drive/BYU/Mental Gurus/Big Five Kaggle")

kdata_r <- read_csv("IPIP-FFM-data-8Nov2018/data-clean-means.csv")

kdata_py <- read_csv("IPIP-FFM-data-8Nov2018/kmeans_py.csv")

k <- kmeans(kdata_r, 12)

k$cluster
kdata_r$cluster <- k$cluster

# Plot tie-fighters
summary1 <- kdata_r %>% group_by(cluster) %>%
  summarize_all(list(.=mean))
summary2 <- kdata_r %>% group_by(cluster) %>%
  summarize_all(list(.=sd))
summary3 <- kdata_r %>% group_by(cluster) %>%
  summarize_all(list(.=length))

s1 <- melt(summary1, id.vars='cluster')
s2 <- melt(summary2, id.vars='cluster')
s3 <- melt(summary3, id.vars='cluster')

summary <- data.frame('cluster'=s1[,1], 'category'=s1[,2], 'mean'=s1[,3], 'sd'=s2[,3], 'n'=s3[,3])

summary$category <- substr(summary$category,1,nchar(as.character(summary$category))-2)
summary$category <- as.factor(summary$category)
summary$cluster <- as.factor(summary$cluster)

# Everything Plot
ggplot(data=summary, aes(x=cluster, y=mean, color=category)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=position_dodge(.4)) +
  geom_point(position=position_dodge(.4)) +
  labs(title='Mean and SD by Category', x='Cluster', y='Mean Response', color='Question\nType')

# Individual Q Cat Plots
for (i in levels(summary$category)){
  dir.create(paste0("./Plots/"), showWarnings = F)

  jpeg(file=paste0("./Plots/",i,".jpeg"), width=450, height=375, res=100)
  
  dat <- summary %>% filter(category == i)
  
  print(
  ggplot(data=dat, aes(x=cluster, y=mean)) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_point() +
    labs(title=paste0('Mean and SD for ',i), x='Cluster', y='Mean Response')
  )
  dev.off()
}
