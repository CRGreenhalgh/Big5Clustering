######################
### Mclust Centers ###
######################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)
library(mvtnorm)
library(data.table)
library(Matrix)



### Loading Data
sumNorm <- read_csv("../data/normalized.csv") %>% select(-X1)
load("all4.RData")

### Custom Functions
sample1 <- function(x) sample(1:16, 1, prob = x)
f <- function(df, nx, ny)  {
  ## Tally up the frequencies
  dt <- data.table(df, key=c("x", "y"))
  xyN <- dt[, .N, by=key(dt)]
  ## Place counts in matrix in their respective i/j x/y row/column
  as.matrix(with(xyN, sparseMatrix(i=x,j=y,x=N,dims=c(nx,ny))))
}

### Cluster Centers
mus <- rbind(all4$second$Clust1$Subclust4$k$centers,
             all4$second$Clust2$Subclust4$k$centers,
             all4$second$Clust3$Subclust4$k$centers,
             all4$second$Clust4$Subclust4$k$centers)
rownames(mus) <- 1:16

### Starting Covariance Structures
num <- 1
cov.list <- list()
assign.list <- list()
for(i in 1:4) for(j in 1:4) {
  assign.list[[num]] <- sumNorm[all4$first$k$cluster == i, ][eval(parse(text = paste0("all4$second$Clust",i,"$Subclust4$k$cluster"))) == j,]
  cov.list[[num]] <- cov(sumNorm[all4$first$k$cluster == i, ][eval(parse(text = paste0("all4$second$Clust",i,"$Subclust4$k$cluster"))) == j,])
  num <- num + 1
}
###############################
### One iteration - TRY ONE ###
###############################

# temp <- matrix(rep(as.numeric(sumNorm[1,]),16), byrow = TRUE, nrow = 16)
temp <- sumNorm[1:100,]
tal <- matrix(0, nrow = nrow(temp), ncol = 16)
update.cov <- cov.list
n.tick <- nrow(temp)

### Within Each Iteration
draws <- c()

pb <- progress::progress_bar$new(
  format = "  Overall [:bar] :percent eta: :eta",
  total = n.tick, clear = FALSE, width= 100)
for(num in 1:n.tick) {
  pb$tick()
  Sys.sleep(1 / n.tick)
  
  ### Calculating Probabilities and Making Draw
  probs <- c()
  for(i in 1:16) {
    probs <- append(probs, dmvnorm(temp[num,], mus[i,], update.cov[[i]]))
  }
  probs.scaled <- probs / sum(probs)
  draw <- sample(1:16, 1, prob = probs.scaled)
  
  ### Tallying Draws
  tal[num, draw] <- tal[num, draw] + 1
  draws <- append(draws, draw)
}

### Updating Variance
for(j in 1:16) {
  update.cov[[j]] <- cov(temp[draws == j, ])
}

############################
### Constant Probability ###
############################

temp <- sumNorm#[1:100,]
tal <- matrix(0, nrow = nrow(temp), ncol = 16)
update.cov <- cov.list
n.tick <- nrow(temp)
nrep <- 100

set.seed(536)
pb <- progress::progress_bar$new(
  format = "  Overall [:bar] :percent eta: :eta",
  total = nrep, clear = FALSE, width= 100)
for(i in 1:nrep) {
  pb$tick()
  Sys.sleep(1 / nrep)
  probs <- c()
  for(i in 1:16) {
    probs <- cbind(probs, dmvnorm(temp, mus[i,], update.cov[[i]]))
  }
  probs.scaled <- t(scale(t(probs), center = FALSE, 
                          scale = colSums(t(probs))))
  draws <- apply(probs.scaled, 1, sample1)
  temp_tal <- f(data.frame(x = 1:n.tick, y = draws), n.tick, 16)
  tal <- tal + temp_tal
  
  
  ### Updating Variance
  for(j in 1:16) {
    update.cov[[j]] <- cov(temp[draws == j, ])
  }
}

scale.tal <- tal/nrep
sum(tal > 900)/n.tick
sum(tal > 800)/n.tick
sum(tal > 700)/n.tick
sum(tal > 600)/n.tick
sum(tal > 500)/n.tick


graph.recluster(all4$results$plotdf2, selection1 = 1:4, first.only = TRUE)

load("tal1000.RData")
tal %>% head

tal[2,] %>% prop.table() %>% sort.int(decreasing = TRUE) %>% cumsum 
tal.vec.9 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .9)[1])
tal.vec.9.plot <- ggplot(mapping = aes(x = tal.vec.9)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 90% Coverage")
tal.vec.8 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .8)[1])
tal.vec.8.plot <- ggplot(mapping = aes(x = tal.vec.8)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 80% Coverage")
tal.vec.7 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .7)[1])
tal.vec.7.plot <- ggplot(mapping = aes(x = tal.vec.7)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 70% Coverage")
tal.vec.6 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .6)[1])
tal.vec.6.plot <- ggplot(mapping = aes(x = tal.vec.6)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 60% Coverage")
tal.vec.5 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .5)[1])
tal.vec.5.plot <- ggplot(mapping = aes(x = tal.vec.5)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 50% Coverage")
tal.vec.4 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .4)[1])
tal.vec.4.plot <- ggplot(mapping = aes(x = tal.vec.4)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 40% Coverage")


jpeg("numClustersConstantProb.jpeg", height = 1500, width = 1500)
gridExtra::grid.arrange(tal.vec.9.plot, tal.vec.8.plot, tal.vec.7.plot,
                        tal.vec.6.plot, tal.vec.5.plot, tal.vec.4.plot, nrow = 2, ncol = 3)
dev.off()

### Making Plots
# Main Personalities
for(i in 1:4) {
  assign(paste0("p",i),
         graph.recluster(all4$results$plotdf2, selection1 = i, first.only = TRUE) + 
           labs(title = paste0("Main Personality Cluster: ",i)) + 
           theme(legend.position = "None"))
}
jpeg("MainClusters.jpeg", height = 1500, width = 1500)
gridExtra::grid.arrange(p1, 
                        p2, 
                        p3, 
                        p4, nrow = 2, ncol = 2) 
dev.off()

graph.df <- mus %>% as.data.frame() %>% mutate(cluster = rep(1:4, each = 4), cluster2 = rep(1:4, 4), id = 1:16, label = paste0(cluster, "_", cluster2)) %>% gather(Construct, Value, EXT:OPN)
for(i in 1:4) for(j in 1:4) {
assign(paste0("p_",i,j),
       graph.recluster(graph.df,
                selection1 = i, selection2 = j) + 
         labs(title = paste0("Main Personality Cluster: ",i, "\n Sub Personality Cluster: ",j)) + theme(legend.position = "None"))
}
jpeg("SubClusters2.jpeg", height = 1500, width = 1500)
gridExtra::grid.arrange(p_11, p_12, p_13, p_14,
                        p_21, p_22, p_23, p_24,
                        p_31, p_32, p_33, p_34,
                        p_41, p_42, p_43, p_44, nrow = 4, ncol = 4) 
dev.off()


############################
### Updating Probability ###
############################
weights <- data.frame("Main" = all4$first$k$cluster, "Sub" = 0, "Id" = 0)
for(i in 1:4) {
  weights[weights$Main == i,2] <- eval(parse(text = paste0("all4$second$Clust",i,"$Subclust4$k$cluster")))
}
for(i in 1:4) for(j in 1:4) {
  weights[(weights$Main == i & weights$Sub == j), 3] <- ((i -1) * 4) + j
}
prop.weights <- table(weights$V3) %>% prop.table

temp <- sumNorm#[1:100,]
temp.weight <- prop.weights
tal <- matrix(0, nrow = nrow(temp), ncol = 16)
update.cov <- cov.list
n.tick <- nrow(temp)
nrep <- 10

set.seed(536)
pb <- progress::progress_bar$new(
  format = "  Overall [:bar] :percent eta: :eta",
  total = nrep, clear = FALSE, width= 100)
for(n in 1:nrep) {
  pb$tick()
  Sys.sleep(1 / nrep)
  
  probs <- c()
  for(i in 1:16) {
    probs <- cbind(probs, dmvnorm(temp, mus[i,], update.cov[[i]]))
  }
  # probs.scaled <- t(scale(t(probs), center = FALSE, 
                          # scale = colSums(t(probs))))
  probs.scaled <- t(apply(probs, 1,  
                        function(x) x * temp.weight / sum(x * temp.weight)))
  
  draws <- apply(probs.scaled, 1, sample1)
  temp_tal <- f(data.frame(x = 1:n.tick, y = draws), n.tick, 16)
  tal <- tal + temp_tal
  
  temp.weight <- prop.table(table(draws))
  
  ### Updating Variance
  for(j in 1:16) {
    update.cov[[j]] <- cov(temp[draws == j, ])
  }
}

(probs[1,] * prop.weights / sum(probs[1,] * prop.weights))

probs.scaled <- apply(probs,1,function(x) x * prop.weights / sum(x * prop.weights))

probs.scaled[1,] %>% sum

sum(tal > 900)/n.tick
sum(tal > 800)/n.tick
sum(tal > 700)/n.tick
sum(tal > 600)/n.tick
sum(tal > 500)/n.tick


graph.recluster(all4$results$plotdf2, selection1 = 1:4, first.only = TRUE)

load("tal1000UpdateProb.RData")
# load("tal1000.RData")
tal %>% head

# tal[2,] %>% prop.table() %>% sort.int(decreasing = TRUE) %>% cumsum 
tal.vec.9 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .9)[1])
tal.vec.9.plot <- ggplot(mapping = aes(x = tal.vec.9)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 90% Coverage")
tal.vec.8 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .8)[1])
tal.vec.8.plot <- ggplot(mapping = aes(x = tal.vec.8)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 80% Coverage")
tal.vec.7 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .7)[1])
tal.vec.7.plot <- ggplot(mapping = aes(x = tal.vec.7)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 70% Coverage")
tal.vec.6 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .6)[1])
tal.vec.6.plot <- ggplot(mapping = aes(x = tal.vec.6)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 60% Coverage")
tal.vec.5 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .5)[1])
tal.vec.5.plot <- ggplot(mapping = aes(x = tal.vec.5)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 50% Coverage")
tal.vec.4 <- apply(tal, 1, function(x) which(cumsum(sort.int(prop.table(x),decreasing = TRUE)) > .4)[1])
tal.vec.4.plot <- ggplot(mapping = aes(x = tal.vec.4)) + geom_histogram() + 
  labs(x = "Number of Clusters to Reach 40% Coverage")


jpeg("numClustersUpdatedProb.jpeg", height = 1500, width = 1500)
gridExtra::grid.arrange(tal.vec.9.plot, tal.vec.8.plot, tal.vec.7.plot,
                        tal.vec.6.plot, tal.vec.5.plot, tal.vec.4.plot, nrow = 2, ncol = 3)
dev.off()
