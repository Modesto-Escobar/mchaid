\name{summary.mchaid}
\alias{summary.mchaid}
\alias{print.summary.mchaid}
\alias{print.mchaid}
\alias{mchaid_node_labels}
\title{Summary of a CHAID tree}
\description{
\code{print} gives the definition of the terminal nodes and the risk estimate. \code{summary} gives the
table of the response by node (row percentages, with the frequencies in the column Total), the risk
estimate and the tests of association between the response and the nodes, as shown by the Stata
command. Because the nodes were chosen to be as associated with the response as possible, the tests
describe the strength of the association but are not valid for inference.
\code{mchaid_node_labels} returns the definition of every node.
}
\usage{
\method{print}{mchaid}(x, ...)
\method{summary}{mchaid}(object, branches = FALSE, definitions = FALSE, ...)
\method{print}{summary.mchaid}(x, digits = 1, ...)
mchaid_node_labels(x, numbers = TRUE)
}
\arguments{
  \item{x, object}{an object returned by \code{\link{mchaid}} (or by \code{summary}).}
  \item{branches}{logical: show the table with the branches of the tree.}
  \item{definitions}{logical: show the list of the nodes with their sizes and definitions.}
  \item{digits}{decimals of the percentages.}
  \item{numbers}{logical: start each label with the number of the node.}
  \item{...}{not used.}
}
\value{
\code{summary} returns an object of class \code{"summary.mchaid"}: a list with \code{freq},
\code{rowpct}, \code{colpct}, \code{chi2}, \code{p}, \code{chi2_lr}, \code{p_lr}, \code{CramersV},
\code{df}, \code{risk}, \code{risk_root} (the risk estimate is the proportion of cases that would be
misclassified if each one were assigned the modal category of its node; \code{risk_root} is that of the
root node, without the tree) and \code{N}. \code{mchaid_node_labels} returns a character vector, for
example \code{"2: Sex = Male & Class in {2nd, 3rd}"}; in an ordered variable, three or more adjacent
categories are written first-last (first~last when the labels are not numbers).
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
summary(fit, branches = TRUE)
mchaid_node_labels(fit)
}
