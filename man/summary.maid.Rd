\name{summary.maid}
\alias{summary.maid}
\alias{print.summary.maid}
\alias{print.maid}
\title{Summary of a MAID tree}
\description{
\code{print} gives the mean and the standard deviation of the response in each terminal node, with its
definition, and the risk estimate. \code{summary} gives the table of the response by node (number of cases,
mean, standard deviation, minimum, median and maximum), the risk estimate and the F test and the R-squared
of the differences between the nodes. Because the nodes were chosen to differ as much as possible, the tests
describe the strength of the association but are not valid for inference.
}
\usage{
\method{print}{maid}(x, ...)
\method{summary}{maid}(object, branches = FALSE, definitions = FALSE, ...)
\method{print}{summary.maid}(x, digits = 2, ...)
}
\arguments{
  \item{x, object}{an object returned by \code{\link{maid}} (or by \code{summary}).}
  \item{branches}{logical: show the table with the branches of the tree.}
  \item{definitions}{logical: show the list of the nodes with their sizes and definitions.}
  \item{digits}{decimals of the table.}
  \item{...}{not used.}
}
\value{
\code{summary} returns an object of class \code{"summary.maid"}: a list with \code{nodes}, \code{N},
\code{mean}, \code{sd}, \code{sst}, \code{sse}, \code{ssb}, \code{r2}, \code{eta}, \code{F}, \code{p},
\code{df1}, \code{df2}, \code{risk} (variance of the response around the mean of its node) and \code{risk0}
(the same around the overall mean: the risk of the root node).
}
\examples{
set.seed(1)
fit <- maid(mpg ~ cyl + gear + carb, data = mtcars, unordered = "carb", minnode = 5, minsplit = 10)
summary(fit, definitions = TRUE)
}
