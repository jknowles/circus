modelInfo <- list(label = "Samejima's Continuous Response Model",
                  "library" = c("EstCRM"),
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('clusters'),
                                          class = c('integer'),
                                          label = c('Number of clusters')),
                  grid = function(x, len = NULL) {
                    n <- nrow(x)
                    if(len < 2){
                      expand.grid(clusters = 3)
                    } else{
                      expand.grid(clusters = max(c(3, 2 + len)))
                    }
                  },
                  fit = function(x, wts, lev, param, last, classProbs, ...) {
                    theDots <- list(...)
                    max.item <- apply(x, 2, max)
                    min.item <- apply(x, 2, min)
                    predFun <- EstCRMitem(data = x, max.item = max.item,
                                      min.item = min.item, ...)
                    mod <- EstCRMperson(data = x, ipar = predFun$param,
                                        max.item = max.item, min.item=min.item)
                    mod$x <- x
                    mod$predFun <- predFun
                    temp <- as.data.frame(mod[1])
                    temp$class <- cut(temp$thetas.Theta.Est.,
                                      breaks = quantile(temp$thetas.Theta.Est.,
                                                       seq(0, 1, 1/param$clusters)),
                                      label = FALSE, include.lowest = TRUE)
                    mod$cluster <- temp$class
                    mod$cut <- param$clusters
                    class(mod) <- c(class(mod), "clres")
                    return(mod)
                  },
                  predict = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    pred <- EstCRMperson(data = newdata,
                                         ipar = modelFit$predFun$param,
                                         min.item = modelFit$predFun$descriptive$Min,
                                         max.item = modelFit$predFun$descriptive$Max)
                    temp <- as.data.frame(pred[1])
                    temp$class <- cut(temp$thetas.Theta.Est.,
                                 breaks = quantile(temp$thetas.Theta.Est., seq(0, 1, 1/modelFit$cut)),
                                 label = FALSE, include.lowest = TRUE)
                    temp <- temp[, c(1, 4)]
                    names(temp) <- c("ID", "clusters")
                    pred$x <- newdata
                    pred$cluster <- temp$clusters
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
                    pred <- EstCRMperson(data = newdata,
                                         ipar = modelFit$predFun$param,
                                         min.item = modelFit$predFun$descriptive$Min,
                                         max.item = modelFit$predFun$descriptive$Max)
                    temp <- as.data.frame(pred[1])
                    temp <- temp[, c(1:3)]
                    names(temp) <- c("ID", "Est", "EstSE")
                    pred$x <- newdata
                    pred$class <- temp
                    class(pred) <- c(class(pred), "clres")
                    return(pred)
                  },
                  varImp = function(object, ...) {
                    stop("Not written")
                  },
                  tags = c("Classification"),
                  sort = function(x) x[order(x$clusters),])
