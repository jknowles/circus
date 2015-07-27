modelInfo <- list(label = "Fuzzy C-Means Clustering",
                  "library" = c("e1071"),
                  type = c('Classification', 'Fuzzy'),
                  parameters = data.frame(parameter = c('centers', 'dist', 'm'),
                                          class = c('integer', 'character','numeric'),
                                          label = c('Number of clusters, k',
                                                    'Distance method',
                                                    'Degree of fuzzification')),
                  grid = function(x, len = NULL) {
                    p <- ncol(x)
                    n <- nrow(x)
                    distMethods <- c("euclidean")
                    kmax <- max(c(3, 2 + len))
                    # minsizeMax <- max(c(10, n/(kmax^2)))
                    k <- ceiling(seq(2, kmax, length.out = len))
                    mSeq <- seq(1.01, 3, by = (3-1.01)/len)
                    # minsize <- ceiling(seq(0, minsizeMax, length.out = len))
                    if(len == 1){
                      expand.grid(centers = 3, m = 2,
                                  dist = "euclidean")
                    } else{
                      expand.grid(centers = k, m = mSeq, dist = distMethods)
                    }
                  },
                  fit = function(x, wts, lev, param, last, classProbs, ...) {
                    theDots <- list(...)
                    mod <- cmeans(x = x, centers = param$centers,
                           dist = param$dist,
                           m = param$m, method = 'cmeans', ...)
                    mod$x <- x
                    class(mod) <- c(class(mod), "clres")
                    return(mod)
                  },
                  predict = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    # L <- nrow(newdata)
                    # newdata <- rbind(newdata, modelFit$x)
                    pred <- cmeans(x = newdata,
                                  centers = theDots$param$centers,
                                  dist = theDots$param$dist,
                                  m = theDots$param$m, method = 'cmeans')
                    pred$x <- newdata
                    class(pred) <- c(class(pred), "clres")
                    return(pred)
                  },
                  prob = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    if(!missing(newdata)){
                      newdata <- rbind(newdata, modelFit$x)
                    } else {
                      newdata <- modelFit$x
                    }
                    L <- nrow(newdata)
                    pred <- cmeans(x = newdata,
                                  centers = theDots$param$centers,
                                  dist = theDots$param$dist,
                                  m = theDots$param$m, method = 'cmeans')
                    return(list(centers = pred$centers, withinerror = pred$withinerror,
                                membership = pred$membership[1:L, ],
                                cluster = pred$cluster[1:L]))
                  },
                  varImp = function(object, ...) {
                    stop("Not written")
                  },
                  tags = c("Fuzzy"),
                  sort = function(x) x[order(x$centers),])
