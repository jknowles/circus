library(circul)
# data(iris)
myData <- rbind(iris[, 1:4], iris[, 1:4])
myData <- rbind(myData, myData)

myCont <- list(method = "cv", number = 10, p = 0.75)
myCont <- list(method = "boot", number = 25, p = 0.75)

zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "pe", modelType = 'Fuzzy',
              circulControl = myCont)
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "pc", modelType = 'Fuzzy',
              circulControl = myCont)
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "fs", modelType = 'Fuzzy',
              circulControl = myCont)
zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
              metric = "propexp", modelType = 'Fuzzy',
              circulControl = myCont)

zed <- cmeans(x = myData, centers = 5, dist = "euclidean")

zed$x <- myData
class(zed) <- c(class(zed), "clres")

criteriaSummary(zed)
sumFunc <- criteriaSummary
metric <- "Det_Ratio"
sumFunc(zed, metric = metric)
sumFunc(zed, metric = "Tau")
sumFunc(zed, metric = "Gamma")

intCriteria(as.matrix(myData)[0,], zed$cluster[0], crit = "a")

data(EPIA)
mod <- EstCRMitem(data = EPIA, max.item = apply(EPIA, 2, max),
           min.item = apply(EPIA, 2, min))
mod2 <- EstCRMperson(data = EPIA, ipar = mod$param, min.item = mod$descriptive$Min,
                     max.item = mod$descriptive$Max)

temp <- as.data.frame(mod2[1])
temp$class <- cut(temp$thetas.Theta.Est.,
                  breaks = quantile(temp$thetas.Theta.Est.,
                                    seq(0, 1, 1/5)),
                  label = FALSE, include.lowest = TRUE)



zed <- circul(x = myData, method = "cmeans", tuneLength = 3,
               modelType = 'Classification',
              circulControl = myCont)
zed2 <- circul(x = myData, method = "cmeans", tuneLength = 3,
              modelType = 'Classification', metric = "Trace_W",
              circulControl = myCont)
zed3 <- circul(x = myData, method = "cmeans", tuneLength = 3,
               modelType = 'Classification', metric = "GDI13",
               circulControl = myCont)


library(boot)

critFun <- function(data, indices){
  library(clusterCrit)
  data <- data[indices, ]
   zed <- try(cmeans(x = data, centers = 4, dist = "euclidean"))
   if(class(zed) != "fclust"){
     out <- NA
   } else{
     data <- as.matrix(data)
     clus <- as.integer(zed$cluster)
     out <- intCriteria(traj = data,
                        part = clus, crit = "Tau")
   }
   as.numeric(out)
}


b1 <- boot(myData, critFun, R = 500)

