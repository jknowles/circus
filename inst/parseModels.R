# setwd("inst/modelLibrary/")
# modelFiles <- list.files(pattern = "\\.R$")
# #
# models <- vector(mode = "list", length = length(modelFiles))
# names(models) <- gsub("\\.R$", "", modelFiles)
#
# for(i in seq(along = modelFiles)) {
#   source(modelFiles[i])
#   models[[i]] <- modelInfo
#   rm(modelInfo)
# }
# #
# save(models, file = "../models/models.RData")
#
# Model objects have:

# http://topepo.github.io/caret/custom_models.html
# library is a character vector of package names that will be needed to fit the model or calculate predictions. NULL can also be used.
# type is a simple character vector with values "Classification", "Regression" or both.
# parameters is a data frame with three simple attributes for each tuning parameter (if any): the argument name (e.g. mtry), the type of data in the parameter grid and textual labels for the parameter.
# grid is a function that is used to create the tuning grid (unless the user gives the exact values of the parameters via tuneGrid)
# fit is a function that fits the model
# predict is the function that creates predictions
# prob is a function that can be used to create class probabilities (if applicable)
# sort is a function that sorts the parameter from most complex to least
# loop is an optional function for advanced users for models that can create multiple submodel predictions from the same object.
# levels is an optional function, primarily for classification models using S4 methods to return the factor levels of the outcome.
# tags is an optional character vector that has subjects associated with the model, such as Tree-Based Model or Embedded Feature Selection. This string is used by the package to create additional documentation pages on the package website.
# label is an optional character string that names the model (e.g. "Linear Discriminant Analysis").
# predictors is an optional function that returns a character vector that contains the names of the predictors that we used in the prediction equation.
# varImp is an optional function that calculates variable importance metrics for the model (if any).
