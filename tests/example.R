library(circul)
# data(iris)
myData <- rbind(iris[, 1:4], iris[, 1:4])
myData <- rbind(myData, myData)

zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "pe", modelType = 'Fuzzy')
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "pc", modelType = 'Fuzzy')
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "fs", modelType = 'Fuzzy')
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "propexp", modelType = 'Fuzzy')

zed <- cmeans(x = myData, centers = 3)

zed$x <- myData
class(zed) <- c(class(zed), "clres")

criteriaSummary(zed)
sumFunc <- criteriaSummary
metric <- "Trace_W"
sumFunc(zed, method = metric)
sumFunc(zed, method = "Tau")


intCriteria(as.matrix(myData), zed$cluster, metric)


zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
               modelType = 'Classification')
zed2 <- circul(x = myData, method = "cmeans", tuneLength = 3,
              modelType = 'Classification', metric = "Trace_W")



