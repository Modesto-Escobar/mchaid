\name{predict.mchaid}
\alias{predict.mchaid}
\title{Predictions of a CHAID tree}
\description{
Node, modal category or distribution of the response for each case, by following the paths of the tree.
}
\usage{
\method{predict}{mchaid}(object, newdata = NULL, type = c("node", "class", "prob"), ...)
}
\arguments{
  \item{object}{an object returned by \code{\link{mchaid}}.}
  \item{newdata}{a data frame with the splitting variables. If omitted, the data of the estimation.}
  \item{type}{\code{"node"} (number of the terminal node), \code{"class"} (modal category of the node,
    a factor; the first one in case of ties) or \code{"prob"} (distribution of the response in the
    node, one column for each category).}
  \item{...}{not used.}
}
\value{
A vector, factor or matrix with one element (row) for each case; \code{NA} for the cases that are not
in the estimation sample or that have a value of a splitting variable not seen in the estimation
(a missing value is only accepted if the tree was grown with \code{missing = TRUE} and the variable had
missing values).
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
nd <- data.frame(Class = c("1st", "3rd"), Sex = c("Female", "Male"), Age = "Adult")
predict(fit, nd)
predict(fit, nd, type = "prob")
}
