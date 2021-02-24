#################
### Functions ###
#################


recluster <- function(input.df, num.clusters.1 = 4, num.clusters.2 = 3, nrep.center.check = 100, set = TRUE, seed = 2232021) {
  dat <- input.df
  
  ### First Layer
  layer1 <- list()
  centers.first <- c()
  if(set) set.seed(seed)
  pb <- progress::progress_bar$new(
    format = "  First Layer [:bar] :percent eta: :eta",
    total = nrep.center.check, clear = FALSE, width= 100)
  for(i in 1:nrep.center.check) {
    pb$tick()
    Sys.sleep(1 / nrep.center.check)
    
    k <- kmeans(dat, centers = num.clusters.1, iter.max = 1000, algorithm = "Lloyd")
    centers.first <- rbind(centers.first, k$centers)
  }
  hc.first <- hclust(dist(centers.first))
  hc.first.1 <- cutree(hc.first, k = num.clusters.1)
  hc.first.2 <- cutree(hc.first, k = num.clusters.1 * 2)
  layer1[["hc.first"]] <- hc.first
  tal <- table(hc.first.2)
  keep <- as.numeric(names(tal)[order(tal, decreasing = T)[1:num.clusters.1]])
  layer1[["tal"]] <- tal
  layer1[["keep"]] <- keep
  centers.first.final <- data.frame(centers.first,"Cluster" =  hc.first.1, "Cluster2" = hc.first.2) %>% 
    dplyr::filter(Cluster2 %in% keep) %>% group_by(Cluster2) %>% 
    summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE) %>% ungroup()
  layer1[["centers"]] <- centers.first.final
  cen.matrix <- centers.first.final %>% select(EXT:OPN) %>% as.matrix
  k <- kmeans(dat, centers = cen.matrix, iter.max = 1000, algorithm = "Lloyd")
  layer1[["k"]] <- k
  cluster1.vec <- k$cluster
  
  ### Second Layer
  layer2 <- list()
  cluster2.vec <- numeric(length(cluster1.vec))
  for(j in 1:num.clusters.1) {
    temp.df <- dat[cluster1.vec == j,]
    temp.list <- c()
    centers2 <- c()
    # if(set) set.seed(seed)
    pb <- progress::progress_bar$new(
      format = paste("  Cluster ", j, " [:bar] :percent eta: :eta"),
      total = nrep.center.check, clear = FALSE, width= 100)
    for(i in 1:nrep.center.check) {
      pb$tick()
      Sys.sleep(1 / nrep.center.check)
      
      k2 <- kmeans(temp.df[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
      centers2 <- rbind(centers2, k2$centers)
    }
    hc.second <- hclust(dist(centers2))
    hc.second.1 <- cutree(hc.second, k = num.clusters.2)
    hc.second.2 <- cutree(hc.second, k = num.clusters.2 * 2)
    temp.list[["hc.second"]] <- hc.second
    tal2 <- table(hc.second.2)
    keep2 <- as.numeric(names(tal2)[order(tal2, decreasing = T)][1:num.clusters.2])
    temp.list[["tal"]] <- tal2
    temp.list[["keep"]] <- keep2
    centers.second.final <- data.frame(centers2,"Cluster" =  hc.second.1, "Cluster2" = hc.second.2) %>% 
      dplyr::filter(Cluster2 %in% keep2) %>% group_by(Cluster2) %>% 
      summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE) %>% ungroup()
    temp.list[["centers"]] <- centers.second.final
    cen.matrix <- centers.second.final %>% select(EXT:OPN) %>% as.matrix
    k2 <- kmeans(temp.df, centers = cen.matrix, iter.max = 1000, algorithm = "Lloyd")
    temp.list[["k"]] <- k2
    cluster2.vec[cluster1.vec == j] <- k2$cluster
    layer2[[paste0("Clust",j)]] <- temp.list
  }
  
  ### Overall
  layer3 <- list()
  recluster <- data.frame(dat, "cluster" = cluster1.vec, "cluster2" = cluster2.vec) %>% mutate(cluster_id = paste(cluster, cluster2, sep = "_"))
  reclusterSum <- recluster %>% group_by(cluster, cluster2) %>% summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE)
  reclusterGathered <- reclusterSum %>% gather(Construct, Value, EXT:OPN) %>% mutate(label = paste(cluster, cluster2, sep = "_"))
  layer3[["cluster1.vec"]] <- cluster1.vec
  layer3[["cluster2.vec"]] <- cluster2.vec
  layer3[["plotdf"]] <- reclusterGathered
  
  ### Saving Results
  results <- list("first" = layer1,"second" = layer2,"results" = layer3)
  return(results)
}

graph.recluster <- function(input.df, label.vec = NULL, selection1 = NULL, selection2 = NULL) {
  reclusterGathered <- input.df
  
  if (length(selection1) > 0) {
    ### BY GROUP
    # selection1 <- c()
    # selection2 <- c()
    ggplot() +   geom_point(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
                            mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
      geom_line(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
                mapping = aes(x = as.factor(Construct), y = Value, 
                              group = as.factor(label)), size = 2) + 
      geom_point(reclusterGathered, 
                 mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
      labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
  } else
    if (length(label.vec) > 0) {
      ### BY LABEL
      # label.vec <- c("1_2", "4_3", "1_1")
      ggplot() +   geom_point(reclusterGathered %>% filter(label %in% label.vec), 
                              mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
        geom_line(reclusterGathered %>% filter(label %in% label.vec), 
                  mapping = aes(x = as.factor(Construct), y = Value, 
                                group = as.factor(label)), size = 2) + 
        geom_point(reclusterGathered, 
                   mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
        labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
    }
  else print("Provide Valid Input")
}
