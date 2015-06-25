library(circul)

data(iris)

zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "pe")
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "pc")
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "fs")
zed <- circul(x = iris[, 1:4], method = "cmeans", tuneLength = 4,
              metric = "propexp")
