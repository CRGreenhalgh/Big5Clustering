library(tidyverse)

data <- read_csv("data-clean-means.csv")

#data_matrix <- as.matrix(data)

res.km <- kmeans(data_matrix, centers = 8, algorithm  = "Forgy")
k <- as.integer(res.km$cluster)
new_data <- cbind(data, k)
new_matrix <- matrix(ncol = 5)
for (i in 1:8){
  name <- paste("new_matrix", i, sep = "")
  num <- as.matrix(new_data[which(i == new_data["k"]),1:5])
  assign(name, num)
  
  
}
mean(new_matrix1)
mean(new_matrix2)
mean(new_matrix3)
mean(new_matrix4)
mean(new_matrix5)
mean(new_matrix6)
mean(new_matrix7)
mean(new_matrix8)
mean(data_matrix)

new_data[which(1 == new_data["k"]),1:5]

data_matrix <- as.matrix(data)
means_matrix <- as.matrix(colMeans(data))
var_matrix <- as.matrix(var(data))
mean(mahalanobis(data_matrix, means_matrix, var_matrix))
#confint(mahalanobis(data, zzz, var_matrix))
