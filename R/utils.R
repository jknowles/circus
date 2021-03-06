#' Get model info from model library
#'
#' @param model a character vector of the model name
#' @param regex logical, should regular expression matching be used?
#' @param ... additional arguments for the regex
#'
#' @return a list or list of lists of model objects
#' @details Based on the getModelInfo function in the \code{caret} package
#' @export
getModelInfo <- function(model = NULL, regex = TRUE, ...) {
  load(system.file("models/models.RData", package = "circul"))
  if(!is.null(model)){
    keepers <- if(regex) grepl(model, names(models), ...) else which(model == names(models))[1]
    models <- models[keepers]
  }
  if(length(models) == 0) stop("That model is not in caret's built-in library")
  models
}

# "createModel" <-function(x, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, classProbs, ...) {
#   if(!is.null(pp$options))
#   {
#     pp$method <- pp$options
#     pp$options <- NULL
#     if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
#     pp$ICAcomp <- NULL
#     pp$x <- x
#     ppObj <- do.call("preProcess", pp)
#     ppObj$call <- "scrubed"
#     x <- predict(ppObj, x)
#     rm(pp)
#   } else ppObj <- NULL
#   modelFit <- method$fit(x = x,
#                          wts = wts,
#                          param  = tuneValue, lev = obsLevels,
#                          last = last,
#                          classProbs = classProbs, ...)
#   ## for models using S4 classes, you can't easily append data, so
#   ## exclude these and we'll use other methods to get this information
#   if(is.null(method$label)) method$label <- ""
# #   if(!isS4(modelFit) &
# #      !(method$label %in% c("Ensemble Partial Least Squares Regression",
# #                            "Ensemble Partial Least Squares Regression with Feature Selection")))
# #   {
# #     modelFit$xNames <- colnames(x)
# #     modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
# #     modelFit$tuneValue <- tuneValue
# #     modelFit$obsLevels <- obsLevels
# #   }
#
#   list(fit = modelFit, preProc = ppObj)
# }
#
# predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
# {
#   if(!is.null(preProc)) newdata <- predict(preProc, newdata)
#   out <- method$predict(modelFit = modelFit,
#                         newdata = newdata,
#                         submodels = param)
#   ## TODO convert to character with classification
#   out
# }
