# From the caret release process

"listString" <-
  function(x, period = FALSE, verbose = FALSE)
  {
    if(verbose) cat("\n      entering listString\n")
    flush.console()

    if(!is.character(x))  x <- as.character(x)

    numElements <- length(x)
    out <- if(length(x) > 0)
    {
      switch(
        min(numElements, 3),
        x,
        paste(x, collapse = " and "),
        {
          x <- paste(
            x,
            c(
              rep(",", numElements - 2),
              " and",
              ""),
            sep = "")
          paste(x, collapse = " ")
        })
    } else ""

    if(period) out <- paste(out, ".", sep = "")
    if(verbose) cat("      leaving  listString\n\n")
    flush.console()
    out
  }

print_models <- function(x) {

  cat("\\strong{", x$label, "} (\\code{method = '", x$code, "'})\n\n", sep = "")
  cat("For", paste(as.character(tolower(sort(x$type))), collapse = " and "))
  if(length(x$library) > 0) {
    pkgs <- paste("\\pkg{", as.character(x$library), "}", sep = "")
    pkgs <- listString(pkgs)
    if(length(x$library) > 1)
      cat(" using packages", pkgs)  else
        cat(" using package", pkgs)
  }
  if(all(x$parameters$parameter == "parameter")) {
    cat(" with no tuning parameters\n\n\n")
  } else {
    params <- paste("\\item ", as.character(x$parameters$label), " (\\code{",
                    as.character(x$parameters$parameter), "}, ",
                    as.character(x$parameters$class), ")",
                    sep = "")
    params <- gsub("#", "Number of ", params, fixed = TRUE)
    cat(" with tuning parameters:\n\\itemize{\n")
    cat(paste(params, collapse = "\n", sep = ""))
    cat("\n}\n\n")
  }
  cat("\n")
}


mods <- getModelInfo()

labs <- unlist(lapply(mods, function(x) x$label))
mods <- mods[order(labs)]
for(i in seq(along = mods)) mods[[i]]$code <- names(mods)[i]

sink("models.Rd")
cat("\\name{train_model_list}\n",
    "\\alias{train_model_list}\n",
    "\\alias{models}\n",
    "\\title{A List of Available Models in circul}\n",
    "\\description{",
    "These models are included in the package via wrappers for ",
    "\\code{\\link{circul}}. Custom models can also be created. See the URL below.\n\n",
    sep = "")
tt <- lapply(mods, print_models)
cat("}\n")

cat("\\references{``Using your own model in \\code{\\link{circul}}''}\n")
cat("\\keyword{models}\n")
sink()
