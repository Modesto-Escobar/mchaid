\name{importance}
\alias{importance}
\title{Permutation importance of the splitting variables}
\description{
If the values of a splitting variable are randomly permuted there should be a decrease in fit when that
variable predicts the response. The fit is Cramer's V between the response and the nodes of the tree,
with the cases assigned to the nodes by the paths of the tree. For a tree of \code{\link{maid}} the fit is the
R-squared of the response on the nodes.
}
\usage{
importance(x, nrep = 10L)
}
\arguments{
  \item{x}{an object returned by \code{\link{mchaid}} or \code{\link{maid}}.}
  \item{nrep}{number of permutations of each variable; the mean decrease is used. The Stata command
    uses a single permutation.}
}
\details{
The decreases are normalised to add up to 1 (row \code{raw}). Because of the random permutation the raw
values are not very meaningful (they can even be negative), and the ranking (row \code{rank}) is what
should be interpreted. Use \code{set.seed()} for reproducible results. With frequency weights, the
cases are expanded (the weights must be integers) before they are permuted.
}
\value{
A matrix with the rows \code{raw} and \code{rank} and one column for each splitting variable.
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
set.seed(1)
importance(fit)
}
