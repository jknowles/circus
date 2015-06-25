library(circul)

data(iris)

zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "pe", modelType = 'Fuzzy')
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "pc", modelType = 'Fuzzy')
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "fs", modelType = 'Fuzzy')
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "propexp", modelType = 'Fuzzy')


myData <- rbind(iris[, 1:4], iris[, 1:4])

zed <- cmeans(x = myData, centers = 3)

zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              modelType = 'Classification')



