setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(plot.matrix)
data(iris)
dat <- iris[,1:4]
k <- 3
nrep <- 1000
num <- nrow(dat)
nsamp <- num
full <- data.frame(index = 1:num)
iterations <- numeric(num)
set.seed(1234)
for(i in 1:nrep) {
  index <- sample(1:nrow(dat), nsamp, replace = TRUE)
  temp.clust <- hclust(factoextra::get_dist(dat[index,])) #will need to adjust
  temp.tree <- cutree(temp.clust, k = k)
  temp.df <- as.data.frame(cbind(index, model.matrix(~-1 + as.factor(temp.tree)))[order(index),])
  colnames(temp.df) <- c("index", paste("iteration", i, 1:k, sep = "_"))
  iterations <- cbind(iterations, plyr::join(full, temp.df, "index", type = "left", match = "first")[,-1])
}

table.list <- list()
col.vec <- c()
for(j in 1:nrow(iterations)) {
  cols <- which(iterations[j,] == 1)
  table.list[[j]] <- numeric(0)
  for(k in cols) {
    table.list[[j]] <- append(table.list[[j]],which(iterations[,k]==1))
  }
  table.list[[j]] <- table.list[[j]][table.list[[j]] != j]
  col.vec <- append(col.vec, length(cols))
}

tally_list <- lapply(table.list, table)

zeros <- lapply(table.list, function(x) which(!(1:150 %in% as.numeric(rownames(table(x))))))
zeros.post <- lapply(zeros, function(x) setNames(rep(0,length(x)),x))

test.me <- mapply(c, zeros.post, tally_list, SIMPLIFY = FALSE)
final <- as.data.frame(do.call(dplyr::bind_rows, test.me))
colnames(final) <- as.numeric(colnames(final))
heatmap(t(as.matrix(final[,paste(1:150)])))

plot(as.matrix(final[,paste(1:150)]), border = NA)

# 
# bc1 <- bclust(iris[,1:4], 3, base.centers=5)
# plot(bc1)
# 
# table(clusters.bclust(bc1, 3))
# centers.bclust(bc1, 3)
# # }
# 
# iris %>% head
# 
# set.seed(5555)
# index <- sample(1:nrow(dat), floor(.1*nrow(dat)), replace = FALSE)
# bc2 <- bclust(dat[index,-1], iter.base = 5, base.centers = 5, iter.max = 30)
# # bc2$hclust
# plot(bc2$hclust)
# length(index)
# bc2$allcluster %>% table
# 
# 
# iris
# try <- hclust(factoextra::get_dist(iris[,1:4]))
# plot(try)


# tally_list[[1]]
# how <- sapply(tally_list,length)
# jpeg("try.jpeg")
# nr <- 1000
# plot(matrix(c(1,8,3,4,0,3), nrow = nr, ncol = nr))
# dev.off()
# ?heatmap
# # rownames(final) <- as.numeric(rownames(final))
# 
# colnames(final) %>% class
# test.me[[1]]
# zeros.post[[2]]
