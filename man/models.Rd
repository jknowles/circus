\name{train_model_list}
\alias{train_model_list}
\alias{models}
\title{A List of Available Models in circul}
\description{These models are included in the package via wrappers for \code{\link{circul}}. Custom models can also be created. See the URL below.

\strong{Bagged Clustering} (\code{method = 'bclust'})

For classification using package \pkg{e1071} with tuning parameters:
\itemize{
\item Number of Number of clusters, k (\code{centers}, integer)
\item Distance method (\code{dist.method}, character)
}


\strong{Fuzzy C-Means Clustering} (\code{method = 'cmeans'})

For classification and fuzzy using package \pkg{e1071} with tuning parameters:
\itemize{
\item Number of clusters, k (\code{centers}, integer)
\item Distance method (\code{dist}, character)
\item Degree of fuzzification (\code{m}, numeric)
}


\strong{Graded Response Model - Polytomous IRT} (\code{method = 'grm'})

For classification using package \pkg{ltm} with tuning parameters:
\itemize{
\item Number of clusters (\code{clusters}, integer)
}


\strong{Samejima's Continuous Response Model} (\code{method = 'estcrm'})

For classification using package \pkg{EstCRM} with tuning parameters:
\itemize{
\item Number of clusters (\code{clusters}, integer)
}


}
\references{``Using your own model in \code{\link{circul}}''}
\keyword{models}
