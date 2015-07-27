modelInfo <- list(label = "Graded Response Model - Polytomous IRT",
                  "library" = c("ltm"),
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
                    predict.grm <- function(model, newdata = NULL){
                      L <- ncol(model$X)
                      if(missing(newdata)){
                        newdata <- as.data.frame(data.matrix(model$X), rownames.force=TRUE)
                        newdata$ID <- row.names(newdata)
                      } else{
                        newdata <- as.data.frame(data.matrix(newdata))
                        newdata <- newdata[, c(names(model$X),
                                               names(newdata)[!names(newdata) %in% names(model$X)])]
                      }
                      thetas <- factor.scores(model,
                                              resp.patterns = newdata[, 1:L, drop = FALSE],
                                              method = c("EB"), B = 5, robust.se = FALSE,
                                              prior = TRUE, return.MIvalues = FALSE)
                      thetas <- thetas$score.dat[!duplicated(thetas$score.dat),]
                      thetas[, intersect(names(thetas), names(newdata))] <-
                        apply(thetas[, intersect(names(thetas), names(newdata))], 2, as.integer)
                      out <- merge(newdata, thetas)
                      return(out)
                    }
                    mod <- grm(data = x, ...)
                    theta <- predict.grm(mod)
                    mod$x <- x
                    mod$predFun <- predict.grm
                    theta$class <- cut(theta$z1,
                                      breaks = quantile(theta$z1,
                                                        seq(0, 1, 1/param$clusters)),
                                      label = FALSE, include.lowest = TRUE)
                    mod$cluster <- theta$class
                    mod$cut <- param$clusters
                    class(mod) <- c(class(mod), "clres")
                    return(mod)
                  },
                  predict = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    L <- ncol(model$X)
                    if(missing(newdata)){
                      newdata <- as.data.frame(data.matrix(model$X), rownames.force=TRUE)
                      newdata$ID <- row.names(newdata)
                    } else{
                      newdata <- as.data.frame(data.matrix(newdata))
                      newdata <- newdata[, c(names(model$X),
                                             names(newdata)[!names(newdata) %in% names(model$X)])]
                    }
                    pred <- predict.grm(modelFit, newdata = newdata[, 1:L])
                    pred$class <- cut(pred$z1,
                                       breaks = quantile(pred$z1,
                                                         seq(0, 1, 1/modelFit$clusters)),
                                       label = FALSE, include.lowest = TRUE)
                    modelFit$x <- newdata
                    modelFit$cluster <- pred$class; pred$class <- NULL
                    class(modelFit) <- c(class(modelFit), "clres")
                    return(modelFit)
                  },
                  prob = function(modelFit, newdata, submodels = NULL, ...){
                    theDots <- list(...)
                    L <- ncol(model$X)
                    if(missing(newdata)){
                      newdata <- as.data.frame(data.matrix(model$X), rownames.force=TRUE)
                      newdata$ID <- row.names(newdata)
                    } else{
                      newdata <- as.data.frame(data.matrix(newdata))
                      newdata <- newdata[, c(names(model$X),
                                             names(newdata)[!names(newdata) %in% names(model$X)])]
                    }
                    pred <- predict.grm(modelFit, newdata = newdata[, 1:L])
                    modelFit$x <- newdata
                    modelFit$prob <- pred[, c("z1", "se.z1")]
                    class(modelFit) <- c(class(modelFit), "clres")
                    return(modelFit)
                  },
                  varImp = function(object, ...) {
                    stop("Not written")
                  },
                  tags = c("Classification"),
                  sort = function(x) x[order(x$clusters),])
