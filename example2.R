# #example 2
#
# library(circul)
# # data(iris)
# myData <- rbind(iris[, 1:4], iris[, 1:4])
# myData <- rbind(myData, myData)
# myCont <- list(method = "cv", number = 5, p = 0.75)
#
#
# zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
#               modelType = 'Classification',
#               circulControl = myCont)
# zed2 <- circul(x = myData, method = "cmeans", tuneLength = 3,
#                modelType = 'Classification', metric = "Trace_W",
#                circulControl = myCont)
# zed3 <- circul(x = myData, method = "cmeans", tuneLength = 3,
#                modelType = 'Classification', metric = "GDI13",
#                circulControl = myCont)

library(clusterCrit)
d <- iris
mat <- as.matrix(d[, 1:4])
clus <- as.integer(d[, 5])

intCriteria(mat[0, ], clus, crit = "all")
intCriteria(mat, clus[0], crit = "all")
intCriteria(mat[0, ], clus[0], crit = "all")


