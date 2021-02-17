data(iris)
bc1 <- bclust(iris[,1:4], 3, base.centers=5)
plot(bc1)

table(clusters.bclust(bc1, 3))
centers.bclust(bc1, 3)
# }

iris %>% head

set.seed(5555)
index <- sample(1:nrow(dat), floor(.1*nrow(dat)), replace = FALSE)
bc2 <- bclust(dat[index,-1], iter.base = 5, base.centers = 5, iter.max = 30)
# bc2$hclust
plot(bc2$hclust)
length(index)
bc2$allcluster %>% table


iris
try <- hclust(factoextra::get_dist(iris[,1:4]))
plot(try)

nrep <- 100
full <- data.frame(index = 1:150)
iterations <- numeric(150)
set.seed(1234)
for(i in 1:nrep) {
  index <- sample(1:nrow(iris), nrow(iris), replace = TRUE)
  temp.clust <- hclust(factoextra::get_dist(iris[index,1:4]))
  temp.tree <- cutree(temp.clust, k = 3)
  temp.df <- as.data.frame(cbind(index, model.matrix(~-1 + as.factor(temp.tree)))[order(index),])
  colnames(temp.df) <- c("index", paste("iteration", i, 1:3, sep = "_"))
  iterations <- cbind(iterations, plyr::join(full, temp.df, "index", type = "left", match = "first")[,-1])
}

table.list <- list()
for(j in 1:nrow(iterations)) {
  cols <- which(iterations[j,] == 1)
  table.list[[j]] <- numeric(0)
  for(k in cols) {
    table.list[[j]] <- append(table.list[[j]],which(iterations[,k]==1))
  }
  table.list[[j]] <- table.list[[j]][table.list[[j]] != j]
}


lapply(table.list, table)[[1]]
