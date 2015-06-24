modelInfo <- list(label = "Bagged Clustering",
                  "library" = c("e1071"),
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('centers', 'dist.method' ),
                                          class = c('integer', 'character'),
                                          label = c('#Number of clusters, k',
                                                    'Distance method')),
                  grid = function(x, len = NULL) {
                    p <- ncol(x)
                    n <- nrow(x)
                    distMethods <- c("euclidian", "maximum", "manhattan", "canberra")
                    kmax <- max(c(3, 2 + len))
                    iter.base <- c(10)
                    minsizeMax <- max(c(10, n/(kmax^2)))
                    k <- ceiling(seq(2, kmax, length.out = len))
                    minsize <- ceiling(seq(0, minsizeMax, length.out = len))
                    if(len == 1){
                      expand.grid(centers = 2, dist.method = "euclidian")
                    } else{
                      expand.grid(centers = k, dist.method = "euclidian")
                    }
                  },
                  fit = function(x, wts, lev, param, last, classProbs, ...) {
                    theDots <- list(...)
                    bclust(x = x, centers = param$centers,
                           dist.method = param$dist.method, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    bclust(x = newdata,
                           centers = length(unique(modelFit$cluster)),
                           dist.method = modelFit$dist.method, ...)$cluster
                  },
                  prob = function(modelFit, newdata, submodels = NULL)
                    stop("Not written"),
                  varImp = function(object, ...) {
                    stop("Not written")
                  },
                  tags = c("Hierarchical"),
                  sort = function(x) x[order(x$centers),])
