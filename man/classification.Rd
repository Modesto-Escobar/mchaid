\name{classification}
\alias{classification}
\alias{print.mchaid_classification}
\title{Classification table of a CHAID tree}
\description{
Every case is classified in the modal category of its node and compared with its real category. Gives the
table, the percentage correctly classified (one minus the risk estimate) and, for each category versus
the rest, sensitivity (recall), precision (positive predictive value), F1, specificity and negative
predictive value, in per cent. Nodes with ties are classified in the first of the tied categories.
}
\usage{
classification(x, positive = NULL)
\method{print}{mchaid_classification}(x, ...)
}
\arguments{
  \item{x}{an object returned by \code{\link{mchaid}} (or by \code{classification}).}
  \item{positive}{optional: a category of the response (its label or its position) for which all the
    statistics of a 2 x 2 classification table (that category versus the rest) are given.}
  \item{...}{not used.}
}
\value{
An object of class \code{"mchaid_classification"}: a list with \code{class} (real category in the rows,
classified category in the columns), \code{N}, \code{N_correct}, \code{pct_correct}, \code{risk},
\code{stats} (a row for each category; columns \code{sensitivity}, \code{specificity}, \code{ppv},
\code{npv}, \code{fpos_true}, \code{fneg_true}, \code{fpos_class}, \code{fneg_class} and \code{f1}, in
per cent; a statistic is \code{NA} when its denominator is zero), \code{f1_macro} and
\code{f1_weighted}.
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
classification(fit, positive = "Yes")
}
