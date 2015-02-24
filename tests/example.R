# Examples

# TuneGrid
# Distance metrics
# Number of clusters
# Model tuning parameters

data(iris)
bc1 <- bclust(iris[,1:4], 3, base.centers=5)

library(cluster)
PAM1 <- pam(iris[, 1:4], k = 4, diss = FALSE)


# Self-organizing maps (two-dimensional)
somCluster <- vector(mode = "list", length = length(colList))

for(i in 1:length(colList)){
  library(kohonen)
  modDF <- train[, names(train) %in% colList[[i]]]
  idVector <- train[, IDs]
  mod <- som(as.matrix(modDF),  grid = somgrid(x=2, y =2), alpha = c(0.05, 0.01))
  clustDF <- na.omit(mod$data)
  clustMatrix <- daisy(as.matrix(clustDF))
  somCluster[[i]] <- silhouette(mod$unit.classif, clustMatrix)
}


#c means clustering
library(e1071)
out <- cmeans(x = train[, 3:33], centers = 4, m = 6)

#Bclust
# bclust
out <- bclust(x = train[, 3:33], centers = 4, minsize = 40)

# cshell
out <- cshell(x = train[, 3:33], centers = 4)
fclustIndex(out, train[, 3:33], index = "partition.coefficient")
#fclust?
# fclustIndex(y, x, index = "all")






# MDS
tmpMatrix <- daisy(as.matrix(train[, 3:33]))
out <- isoMDS(tmpMatrix, k = 4)


#SPCA
# require(elasticnet)
# no cluster produced?
# SPCA1 <- spca(train[, 3:33], K = 3, para=c(0.2, 0.5, 0.2))

# Fast ICA
