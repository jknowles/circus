# From train
checkInstall <- function(pkg){
  good <- rep(TRUE, length(pkg))
  for(i in seq(along = pkg)){
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if(class(tested)[1] == "try-error") good[i] <- FALSE
  }
  if(any(!good)){
    pkList <- paste(pkg[!good], collapse = ", ")
    msg <- paste(sum(!good),
                 ifelse(sum(!good) > 1, " packages are", " package is"),
                 " needed for this model and",
                 ifelse(sum(!good) > 1, " are", " is"),
                 " not installed. (",
                 pkList,
                 "). Would you like to try to install",
                 ifelse(sum(!good) > 1, " them", " it"),
                 " now?",
                 sep = "")
    cat(msg)
    if(interactive()) {
      bioc <- c("affy", "logicFS", "gpls", "vbmp")
      installChoice <- menu(c("yes", "no"))
      if(installChoice == 1){
        hasBioc <- any(pkg[!good] %in% bioc)
        if(!hasBioc) {
          install.packages(pkg[!good])
        } else {
          inst <- pkg[!good]
          instC <- inst[!(inst %in% bioc)]
          instB <- inst[inst %in% bioc]
          if(length(instC) > 0) install.packages(instC)
          biocLite <- NULL
          source("http://bioconductor.org/biocLite.R")
          biocLite(instB)
        }
      } else stop()
    } else stop()
  }
}

"createModel" <-function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, classProbs, ...) {
  if(!is.null(pp$options))
  {
    pp$method <- pp$options
    pp$options <- NULL
    if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
    pp$ICAcomp <- NULL
    pp$x <- x
    ppObj <- do.call("preProcess", pp)
    ppObj$call <- "scrubed"
    x <- predict(ppObj, x)
    rm(pp)
  } else ppObj <- NULL
  modelFit <- method$fit(x = x,
                         wts = wts,
                         param  = tuneValue, lev = obsLevels,
                         last = last,
                         classProbs = classProbs, ...)
  ## for models using S4 classes, you can't easily append data, so
  ## exclude these and we'll use other methods to get this information
  if(is.null(method$label)) method$label <- ""
#   if(!isS4(modelFit) &
#      !(method$label %in% c("Ensemble Partial Least Squares Regression",
#                            "Ensemble Partial Least Squares Regression with Feature Selection")))
#   {
#     modelFit$xNames <- colnames(x)
#     modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
#     modelFit$tuneValue <- tuneValue
#     modelFit$obsLevels <- obsLevels
#   }

  list(fit = modelFit, preProc = ppObj)
}

predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
{
  if(!is.null(preProc)) newdata <- predict(preProc, newdata)
  out <- method$predict(modelFit = modelFit,
                        newdata = newdata,
                        submodels = param)
  ## TODO convert to character with classification
  out
}
