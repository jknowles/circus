#' Train an unsupervised learning model
#'
#' @param x a data.frame consisting of measures to be clustered
#' @param method a method in the circul model library, character
#' @param metric a character representing the performance metric to calculate
#' @param circulControl a list of control parameters
#' @param tuneGrid a tuning grid of paramater values
#' @param modelType the type of model
#' @param tuneLength the length of the tuneGrid
#'
#' @return a data.frame with performance metrics
#' @importFrom caret createDataPartition
#' @importFrom caret checkInstall
#' @importFrom caret createResample
#' @export
#'
circul <- function(x, method = 'cmeans',
                          metric = NULL,
                          circulControl = NULL,
                          tuneGrid = NULL,
                          modelType = 'Classification',
                          tuneLength = 3) {
  if(missing(circulControl)){
    stop("Must specify the control parmaters to circul.")
  }
  models <- getModelInfo(method, regex = FALSE)[[1]]
  if (length(models) == 0) {
    stop(paste("Model", method, "is not in caret's built-in library"))
  }
  caret::checkInstall(models$library)
  for(i in seq(along = models$library)) do.call("require",
                                                list(package = models$library[i]))
  if(circulControl$method == "cv"){
    resampIdx <- caret::createDataPartition(x[, 1],
                                            times = circulControl$number,
                                            p = circulControl$p)
  } else if(circulControl$method == "boot"){
    resampIdx <- caret::createResample(x[, 1], times = circulControl$number)
  }
  parmGrid <- models$grid(x, len = tuneLength)
  L <- ncol(parmGrid)
  if(modelType == "Classification"){
    sumFunc <- criteriaSummary
    if(missing(metric)){
      metric <- "Silhouette"
    }
  } else if(modelType == "Fuzzy"){
    sumFunc <- fuzzySummary
    if(missing(metric)){
      metric <- "pe"
    }
  }
  for(j in 1:nrow(parmGrid)){
    trainSum <- rep(NA, length(resampIdx))
    testSum <- rep(NA, length(resampIdx))
    for(i in seq(along = resampIdx)){
      tmpMod <- try(models$fit(x = x[resampIdx[[i]], ], param = parmGrid[j, 1:L]))
      if(any(class(tmpMod) %in% c("clres", "cluster"))){
        trainSum[i] <- sumFunc(clres = tmpMod, metric = metric)
      } else {
        message("Iteration ___ of training resample failed")
        trainSum[i] <- NA
      }
      tmpIdx <- row.names(x)[!row.names(x) %in% resampIdx[[i]]]
      out1 <- try(models$predict(tmpMod, newdata =  x[tmpIdx, ],
                                 param = parmGrid[j, 1:L]))
      if(any(class(out1) %in% c("clres", "cluster"))){
        testSum[i] <- try(sumFunc(clres = out1, metric = metric))
      } else {
        message("Iteration ___ of training resample failed")
        testSum[i] <- NA
      }
      rm(tmpMod, out1)
     }
    if(anyNA(testSum)){
      warning("NA values in performance metric on test data")
    }
    if(anyNA(trainSum)){
      warning("NA values in performance metric on train data")
    }
    parmGrid[j, "testSumMEAN"] <- mean(testSum)
    parmGrid[j, "trainSumMEAN"] <- mean(trainSum)
    parmGrid[j, "testSumSD"] <- sd(testSum)
    parmGrid[j, "trainSumSD"] <- sd(trainSum)
  }
  if(modelType == "Classification"){
    bestIdx <- clusterCrit::bestCriterion(parmGrid$testSumMEAN, crit = metric)
  } else {
    bestIdx <- row.names(parmGrid)[parmGrid$testSumMEAN == max(parmGrid$testSumMEAN, na.rm=TRUE)]
  }

  bestIdx <- ifelse(is.na(bestIdx), 1, bestIdx)
  bestIdx <- ifelse(length(bestIdx) > 1, bestIdx[1], bestIdx)

  bestMod <-  models$fit(x = x, param = parmGrid[bestIdx, 1:L])

  out <- list(method = method, modelType = modelType,
              results = parmGrid, bestTune = parmGrid[bestIdx, 1:L],
              call = match.call, metric = metric, finalModel = bestMod,
              traininData = x, perfNames = names(parmGrid[, L:ncol(parmGrid)]),
              maximize = TRUE)
  return(out)
}


# circus.default <- function(x, y, method = 'bclust',
#                            preProcess = NULL, ...,
#                            weights = NULL,
#                            metric = "separation.index",
#                            trControl = trainControl(),
#                            tuneGrid = NULL, modelType = "Classification",
#                            tuneLength = 3){
#   startTime <- proc.time()
#
#   if(is.list(method)) {
#     minNames <- c("library", "type", "parameters", "grid",
#                   "fit", "predict", "prob")
#     nameCheck <- minNames %in% names(method)
#     if(!all(nameCheck)) stop(paste("some required components are missing:",
#                                    paste(minNames[!nameCheck], collapse = ", ")))
#     models <- method
#     method <- "custom"
#   } else {
#     stop("Model library not built yet")
#   }
#   checkInstall(models$library)
#   for(i in seq(along = models$library)) do.call("require", list(package = models$library[i]))
#
#   paramNames <- as.character(models$parameters$parameter)
#   funcCall <- match.call(expand.dots = TRUE)
#   modelType <- modelType
#   if(!is.null(modelType) %% !modelType %in% c("Classification", "Regression"))
#     stop(paste('modelType must be either "Classification" or "Regression"'))
#   if(!(modelType %in% models$type)) stop(paste("wrong model type for", tolower(modelType)))
#
#
#   if(any(class(x) == "data.table")) x <- as.data.frame(x)
#   stopifnot(nrow(x) > 1)
#
#   if(!is.null(preProcess) && !(all(preProcess %in% ppMethods)))
#     stop(paste('pre-processing methods are limited to:', paste(ppMethods, collapse = ", ")))
#
#   if(is.null(trControl$index)) {
#     trControl$index <- switch(tolower(trControl$method),
#                               oob = NULL, # for now, first column is the sampling column
#                               none = list(seq(along = x[,1])),
#                               alt_cv =, cv = createFolds(x[,1], trControl$number, returnTrain = TRUE),
#                               repeatedcv =, adaptive_cv = createMultiFolds(x[,1], trControl$number, trControl$repeats),
#                               loocv = createFolds(x[,1], length(x[,1]), returnTrain = TRUE),
#                               boot =, boot632 =,  adaptive_boot = createResample(x[,1], trControl$number),
#                               test = createDataPartition(x[,1], 1, trControl$p),
#                               adaptive_lgocv =, lgocv = createDataPartition(x[,1], trControl$number, trControl$p),
#                               timeslice = createTimeSlices(seq(along = x[,1]),
#                                                            initialWindow = trControl$initialWindow,
#                                                            horizon = trControl$horizon,
#                                                            fixedWindow = trControl$fixedWindow)$train,
#                               subsemble = subsemble_index(x[,1], V = trControl$number, J = trControl$repeats))
#   }
#
#   if(trControl$method == "subsemble") {
#     if(!trControl$savePredictions) trControl$savePredictions <- TRUE
#     trControl$indexOut <- trControl$index$holdout
#     trControl$index <- trControl$index$model
#   }
#
#   ## Create hold--out indicies
#   if(is.null(trControl$indexOut) & trControl$method != "oob"){
#     if(tolower(trControl$method) != "timeslice") {
#       trControl$indexOut <- lapply(trControl$index,
#                                    function(training, allSamples) allSamples[-unique(training)],
#                                    allSamples = seq(along = x[,1]))
#       names(trControl$indexOut) <- prettySeq(trControl$indexOut)
#     } else {
#       trControl$indexOut <- createTimeSlices(seq(along = x[,1]),
#                                              initialWindow = trControl$initialWindow,
#                                              horizon = trControl$horizon,
#                                              fixedWindow = trControl$fixedWindow)$test
#     }
#   }
#
#   if(trControl$method != "oob" & is.null(trControl$index)) names(trControl$index) <- prettySeq(trControl$index)
#   if(trControl$method != "oob" & is.null(names(trControl$index)))    names(trControl$index)    <- prettySeq(trControl$index)
#   if(trControl$method != "oob" & is.null(names(trControl$indexOut))) names(trControl$indexOut) <- prettySeq(trControl$indexOut)
#
#   ## Gather all the pre-processing info. We will need it to pass into the grid creation
#   ## code so that there is a concorance between the data used for modeling and grid creation
#   if(!is.null(preProcess)) {
#     ppOpt <- list(options = preProcess)
#     if(length(trControl$preProcOptions) > 0) ppOpt <- c(ppOpt,trControl$preProcOptions)
#   } else ppOpt <- NULL
#
#   ## If no default training grid is specified, get one. We have to pass in the formula
#   ## and data for some models (rpart, pam, etc - see manual for more details)
#   if(is.null(tuneGrid)) {
#     if(!is.null(ppOpt) && length(models$parameters$parameter) > 1 && as.character(models$parameters$parameter) != "parameter") {
#       pp <- list(method = ppOpt$options)
#       if("ica" %in% pp$method) pp$n.comp <- ppOpt$ICAcomp
#       if("pca" %in% pp$method) pp$thresh <- ppOpt$thresh
#       if("knnImpute" %in% pp$method) pp$k <- ppOpt$k
#       pp$x <- x
#       ppObj <- do.call("preProcess", pp)
#       tuneGrid <- models$grid(predict(ppObj, x), tuneLength)
#       rm(ppObj, pp)
#     } else tuneGrid <- models$grid(x, tuneLength)
#   }
#   dotNames <- function (grid, info)   {
#     mnames <- sort(as.character(info$parameters$parameter))
#     mnames2 <- paste(".", mnames, sep = "")
#     gnames <- sort(colnames(grid))
#     out <- all.equal(mnames2, gnames)
#     if (class(out)[1] != "logical")
#       out <- FALSE
#     out
#   }
#
#   dotNames <- hasDots(tuneGrid, models)
#   if(dotNames) colnames(tuneGrid) <- gsub("^\\.", "", colnames(tuneGrid))
#   ## Check tuning parameter names
#   tuneNames <- as.character(models$parameters$parameter)
#   goodNames <- all.equal(sort(tuneNames), sort(names(tuneGrid)))
#
#   if(!is.logical(goodNames) || !goodNames) {
#     stop(paste("The tuning parameter grid should have columns",
#                paste(tuneNames, collapse = ", ", sep = "")))
#   }
#
#   if(trControl$method == "none" && nrow(tuneGrid) != 1)
#     stop("Only one model should be specified in tuneGrid with no resampling")
#
#   if(is.null(trControl$seeds)) {
#     seeds <- vector(mode = "list", length = length(trControl$index))
#     seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = nrow(trainInfo$loop)))
#     seeds[[length(trControl$index) + 1]] <- sample.int(n = 1000000, size = 1)
#     trControl$seeds <- seeds
#   } else {
#     if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) {
#       ## check versus number of tasks
#       numSeeds <- unlist(lapply(trControl$seeds, length))
#       badSeed <- (length(trControl$seeds) < length(trControl$index) + 1) ||
#         (any(numSeeds[-length(numSeeds)] < nrow(trainInfo$loop)))
#       if(badSeed) stop(paste("Bad seeds: the seed object should be a list of length",
#                              length(trControl$index) + 1, "with",
#                              length(trControl$index), "integer vectors of size",
#                              nrow(trainInfo$loop), "and the last list element having a",
#                              "single integer"))
#     }
#   }
#   if(trControl$method == "oob"){
#     tmp <- oobTrainWorkflow(x = x, y = y, wts = weights,
#                             info = trainInfo, method = models,
#                             ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
#     performance <- tmp
#   } else {
#     if(trControl$method == "LOOCV"){
#       tmp <- looTrainWorkflow(x = x, y = y, wts = weights,
#                               info = trainInfo, method = models,
#                               ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
#       performance <- tmp$performance
#     } else {
#       if(!grepl("adapt", trControl$method)){
#         tmp <- nominalTrainWorkflow(x = x, y = y, wts = weights,
#                                     info = trainInfo, method = models,
#                                     ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
#         performance <- tmp$performance
#         resampleResults <- tmp$resample
#       } else {
#         tmp <- adaptiveWorkflow(x = x, y = y, wts = weights,
#                                 info = trainInfo, method = models,
#                                 ppOpts = preProcess,
#                                 ctrl = trControl,
#                                 lev = classLevels,
#                                 metric = metric,
#                                 maximize = maximize,
#                                 ...)
#         performance <- tmp$performance
#         resampleResults <- tmp$resample
#       }
#     }
#   }
#
#
#
# }
#
