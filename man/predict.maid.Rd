\name{predict.maid}
\alias{predict.maid}
\title{Predictions of a MAID tree}
\description{Node, or mean of the response in the node, for each case.}
\usage{
\method{predict}{maid}(object, newdata = NULL, type = c("node", "mean"), ...)
}
\arguments{
  \item{object}{an object returned by \code{\link{maid}}.}
  \item{newdata}{a data frame with the splitting variables. If omitted, the data of the estimation.}
  \item{type}{\code{"node"} (number of the terminal node) or \code{"mean"} (mean of the response in the node).}
  \item{...}{not used.}
}
\value{
A vector with one element for each case; \code{NA} for the cases that are not in the estimation sample or that
have a value of a splitting variable not seen in the estimation.
}
\examples{
set.seed(1)
fit <- maid(mpg ~ cyl + gear + carb, data = mtcars, unordered = "carb", minnode = 5, minsplit = 10)
predict(fit, mtcars[1:4, ], type = "mean")
}
