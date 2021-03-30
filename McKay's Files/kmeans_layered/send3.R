######################
### Mclust Centers ###
######################

### Setting Working Directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)
library(mvtnorm)
library(data.table)
library(Matrix)



### Loading Data
sumNorm <- read_csv("normalized.csv") %>% select(-X1)
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
prop.weights <- prop.table(table(weights$Id))

temp <- sumNorm#[1:100,]
temp.weight <- prop.weights
tal <- matrix(0, nrow = nrow(temp), ncol = 16)
update.cov <- cov.list
n.tick <- nrow(temp)
nrep <- 1000

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

save(tal, file = "tal1000UpdateProb.RData")
