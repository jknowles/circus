# Examples

library(cluster)
library(e1071)
library("caret")
# TuneGrid
# Distance metrics
# Number of clusters
# Model tuning parameters

data(iris)

# bagged clustering
## centers = k
## base.centers = bc


# getModelInfo
## Model info contains: library, type, parameters, grid, fit, predict, prob
## Model info is a stored list built into package
##
# getInstall(models$library)

# builds an object

circus.default <- function(x, y, method = 'bclust',
                           preProcess = NULL, ...,
                           weights = NULL,
                           metric = "separation.index",
                           trControl = trainControl(),
                           tuneGrid = NULL, modelType = "Classification",
                           tuneLength = 3){
  startTime <- proc.time()

  if(is.list(method)) {
    minNames <- c("library", "type", "parameters", "grid",
                  "fit", "predict", "prob")
    nameCheck <- minNames %in% names(method)
    if(!all(nameCheck)) stop(paste("some required components are missing:",
                                   paste(minNames[!nameCheck], collapse = ", ")))
    models <- method
    method <- "custom"
  } else {
    stop("Model library not built yet")
  }
  checkInstall(models$library)
  for(i in seq(along = models$library)) do.call("require", list(package = models$library[i]))

  paramNames <- as.character(models$parameters$parameter)
  funcCall <- match.call(expand.dots = TRUE)
  modelType <- modelType
  if(!is.null(modelType) %% !modelType %in% c("Classification", "Regression"))
    stop(paste('modelType must be either "Classification" or "Regression"'))
  if(!(modelType %in% models$type)) stop(paste("wrong model type for", tolower(modelType)))


  if(any(class(x) == "data.table")) x <- as.data.frame(x)
  stopifnot(nrow(x) > 1)

  if(!is.null(preProcess) && !(all(preProcess %in% ppMethods)))
    stop(paste('pre-processing methods are limited to:', paste(ppMethods, collapse = ", ")))

  if(is.null(trControl$index)) {
    trControl$index <- switch(tolower(trControl$method),
                              oob = NULL, # for now, first column is the sampling column
                              none = list(seq(along = x[,1])),
                              alt_cv =, cv = createFolds(x[,1], trControl$number, returnTrain = TRUE),
                              repeatedcv =, adaptive_cv = createMultiFolds(x[,1], trControl$number, trControl$repeats),
                              loocv = createFolds(x[,1], length(x[,1]), returnTrain = TRUE),
                              boot =, boot632 =,  adaptive_boot = createResample(x[,1], trControl$number),
                              test = createDataPartition(x[,1], 1, trControl$p),
                              adaptive_lgocv =, lgocv = createDataPartition(x[,1], trControl$number, trControl$p),
                              timeslice = createTimeSlices(seq(along = x[,1]),
                                                           initialWindow = trControl$initialWindow,
                                                           horizon = trControl$horizon,
                                                           fixedWindow = trControl$fixedWindow)$train,
                              subsemble = subsemble_index(x[,1], V = trControl$number, J = trControl$repeats))
  }

  if(trControl$method == "subsemble") {
    if(!trControl$savePredictions) trControl$savePredictions <- TRUE
    trControl$indexOut <- trControl$index$holdout
    trControl$index <- trControl$index$model
  }

  ## Create hold--out indicies
  if(is.null(trControl$indexOut) & trControl$method != "oob"){
    if(tolower(trControl$method) != "timeslice") {
      trControl$indexOut <- lapply(trControl$index,
                                   function(training, allSamples) allSamples[-unique(training)],
                                   allSamples = seq(along = x[,1]))
      names(trControl$indexOut) <- prettySeq(trControl$indexOut)
    } else {
      trControl$indexOut <- createTimeSlices(seq(along = x[,1]),
                                             initialWindow = trControl$initialWindow,
                                             horizon = trControl$horizon,
                                             fixedWindow = trControl$fixedWindow)$test
    }
  }

  if(trControl$method != "oob" & is.null(trControl$index)) names(trControl$index) <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$index)))    names(trControl$index)    <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$indexOut))) names(trControl$indexOut) <- prettySeq(trControl$indexOut)

  ## Gather all the pre-processing info. We will need it to pass into the grid creation
  ## code so that there is a concorance between the data used for modeling and grid creation
  if(!is.null(preProcess)) {
    ppOpt <- list(options = preProcess)
    if(length(trControl$preProcOptions) > 0) ppOpt <- c(ppOpt,trControl$preProcOptions)
  } else ppOpt <- NULL

  ## If no default training grid is specified, get one. We have to pass in the formula
  ## and data for some models (rpart, pam, etc - see manual for more details)
  if(is.null(tuneGrid)) {
    if(!is.null(ppOpt) && length(models$parameters$parameter) > 1 && as.character(models$parameters$parameter) != "parameter") {
      pp <- list(method = ppOpt$options)
      if("ica" %in% pp$method) pp$n.comp <- ppOpt$ICAcomp
      if("pca" %in% pp$method) pp$thresh <- ppOpt$thresh
      if("knnImpute" %in% pp$method) pp$k <- ppOpt$k
      pp$x <- x
      ppObj <- do.call("preProcess", pp)
      tuneGrid <- models$grid(predict(ppObj, x), tuneLength)
      rm(ppObj, pp)
    } else tuneGrid <- models$grid(x, tuneLength)
  }
  dotNames <- function (grid, info)   {
    mnames <- sort(as.character(info$parameters$parameter))
    mnames2 <- paste(".", mnames, sep = "")
    gnames <- sort(colnames(grid))
    out <- all.equal(mnames2, gnames)
    if (class(out)[1] != "logical")
      out <- FALSE
    out
  }

  dotNames <- hasDots(tuneGrid, models)
  if(dotNames) colnames(tuneGrid) <- gsub("^\\.", "", colnames(tuneGrid))
  ## Check tuning parameter names
  tuneNames <- as.character(models$parameters$parameter)
  goodNames <- all.equal(sort(tuneNames), sort(names(tuneGrid)))

  if(!is.logical(goodNames) || !goodNames) {
    stop(paste("The tuning parameter grid should have columns",
               paste(tuneNames, collapse = ", ", sep = "")))
  }

  if(trControl$method == "none" && nrow(tuneGrid) != 1)
    stop("Only one model should be specified in tuneGrid with no resampling")

  if(is.null(trControl$seeds)) {
    seeds <- vector(mode = "list", length = length(trControl$index))
    seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = nrow(trainInfo$loop)))
    seeds[[length(trControl$index) + 1]] <- sample.int(n = 1000000, size = 1)
    trControl$seeds <- seeds
  } else {
    if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) {
      ## check versus number of tasks
      numSeeds <- unlist(lapply(trControl$seeds, length))
      badSeed <- (length(trControl$seeds) < length(trControl$index) + 1) ||
        (any(numSeeds[-length(numSeeds)] < nrow(trainInfo$loop)))
      if(badSeed) stop(paste("Bad seeds: the seed object should be a list of length",
                             length(trControl$index) + 1, "with",
                             length(trControl$index), "integer vectors of size",
                             nrow(trainInfo$loop), "and the last list element having a",
                             "single integer"))
    }
  }
  if(trControl$method == "oob"){
    tmp <- oobTrainWorkflow(x = x, y = y, wts = weights,
                            info = trainInfo, method = models,
                            ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
    performance <- tmp
  } else {
    if(trControl$method == "LOOCV"){
      tmp <- looTrainWorkflow(x = x, y = y, wts = weights,
                              info = trainInfo, method = models,
                              ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
      performance <- tmp$performance
    } else {
      if(!grepl("adapt", trControl$method)){
        tmp <- nominalTrainWorkflow(x = x, y = y, wts = weights,
                                    info = trainInfo, method = models,
                                    ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
        performance <- tmp$performance
        resampleResults <- tmp$resample
      } else {
        tmp <- adaptiveWorkflow(x = x, y = y, wts = weights,
                                info = trainInfo, method = models,
                                ppOpts = preProcess,
                                ctrl = trControl,
                                lev = classLevels,
                                metric = metric,
                                maximize = maximize,
                                ...)
        performance <- tmp$performance
        resampleResults <- tmp$resample
      }
    }
  }









}


resampleIdx <- 1:10
tuneGrid <- expand.grid(.k = c(3, 5, 7, 9),
                        .bc = c(3, 5, 10, 20),
                        rowIdx = resampleIdx)

tuneGrid$metric <- NA

for(i in 1:nrow(tuneGrid)){
  parm <- tuneGrid[i, ]
  tmpDat <- iris[sample(row.names(iris), nrow(iris), replace = TRUE), ]
  tmpMod <- try(bclust(tmpDat[, 1:4], centers = parm$.k, base.centers = parm$.bc))
  tuneGrid[i, "metric"] <- try(fclustIndex(tmpMod, tmpDat[, 1:4], index = "partition.entropy"))
}


bc1 <- bclust(iris[,1:4], 3, base.centers=5)

library(cluster)
PAM1 <- pam(iris[, 1:4], k = 4, diss = FALSE)


#optpart
# betfit(x, cluster)

# Self-organizing maps (two-dimensional)
somCluster <- vector(mode = "list", length = length(colList))

for(i in 1:length(colList)){
  library(kohonen)
  modDF <- train[, names(train) %in% colList[[i]]]
  idVector <- train[, IDs]
  mod <- som(as.matrix(modDF),  grid = somgrid(x=2, y =2), alpha = c(0.05, 0.01))
  clustDF <- na.omit(mod$data)
  clustMatrix <- daisy(as.matrix(clustDF))
  somCluster[[i]] <- silhouette(mod$unit.classif, clustMatrix)
}


#c means clustering
library(e1071)
out <- cmeans(x = train[, 3:33], centers = 4, m = 6)

#Bclust
# bclust
out <- bclust(x = train[, 3:33], centers = 4, minsize = 40)

# cshell
out <- cshell(x = train[, 3:33], centers = 4)
fclustIndex(out, train[, 3:33], index = "partition.coefficient")
#fclust?
# fclustIndex(y, x, index = "all")






# MDS
tmpMatrix <- daisy(as.matrix(train[, 3:33]))
out <- isoMDS(tmpMatrix, k = 4)


#SPCA
# require(elasticnet)
# no cluster produced?
# SPCA1 <- spca(train[, 3:33], K = 3, para=c(0.2, 0.5, 0.2))

# Fast ICA
