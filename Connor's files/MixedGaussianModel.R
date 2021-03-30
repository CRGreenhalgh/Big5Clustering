#### Libraries I need ####
library(tidyverse)
library(mclust)

#### Read in data
dat <- read_csv("big5clean_construct.csv")
sc_dat <- read_csv("normalized.csv") 
#load("Connor's files/Clusters.RData")

#### Gaussian Mixed Model
#k <- 6

bic_clust <- NULL
mean_maxes <- NULL
mean_sec_max <- NULL
mean_toptwo <- NULL
tictoc::tic()
for (k in 4:12) {
  # Create cluster object
  mc <- Mclust(sc_dat, G = k)
  
  # Assign each cluster
  name <- paste0("mc",k)
  assign(name,mc)
  
  # Get the probs for each cluster
  pr <- mc$z %>% as.data.frame() 
  
  # Get the max for each obs highest prob
  pr$Max <- apply(pr, 1, max)
  
  # Loop to get the second highest as well
  for (i in 1:nrow(pr)) {
    pr$Max2[i] <- sort(pr[i,1:k], TRUE)[2] %>% as.numeric()
  }
  
  # Create variable for the two highest probs added together
  pr$toptwo <- pr$Max + pr$Max2
  
  # Create variable for each obs best cluster
  pr$ClusterID <- mc$classification
  
  # Create objects for each iteration's bic, mean max, mean second max, and mean top two added
  bic_clust <- append(bic_clust, mc$bic)
  mean_maxes <- append(mean_maxes, mean(pr$Max))
  mean_sec_max <- append(mean_sec_max, mean(pr$Max2))
  mean_toptwo <- append(mean_toptwo, mean(pr$toptwo))
  
  # Lastly merge data with the new dataframe
  name2 <- paste0("dat",k)
  dat_new <- cbind(dat, pr)
  assign(name2, dat_new)
}
tictoc::toc()

list_dfs <- list(dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12)
list_mc <- list(mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc12)
centers <- list(as.matrix(mc4$parameters$mean), as.matrix(mc5$parameters$mean), 
                as.matrix(mc6$parameters$mean), as.matrix(mc7$parameters$mean),
                as.matrix(mc8$parameters$mean), as.matrix(mc9$parameters$mean),
                as.matrix(mc10$parameters$mean), as.matrix(mc11$parameters$mean),
                as.matrix(mc12$parameters$mean))

plot_df <- data.frame(num_clusters = 4:12, 
                      max_prob = as.numeric(mean_maxes), 
                      sec_max_prob = mean_sec_max, 
                      top_two_probs = mean_toptwo,
                      bic = as.numeric(bic_clust))

plot1 <- ggplot(data = plot_df, mapping = aes(x = num_clusters)) + 
  geom_line(aes(y = max_prob), color = "#0033A0") +
  geom_line(aes(y = sec_max_prob), color = "#fb4f14") +
  geom_line(aes(y = top_two_probs), color = "#c6011f")

plot2 <- ggplot(data = plot_df, mapping = aes(x = num_clusters)) +
  geom_line(aes(y = bic))

save(dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc12,
     mean_maxes,mean_sec_max,mean_toptwo,bic_clust,centers,plot_df,plot1,plot2, 
     file = "./Connor's files/Clusters.RData")

