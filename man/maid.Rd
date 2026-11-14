\name{maid}
\alias{maid}
\title{AID tree for a quantitative response (CHAID with F tests)}
\description{
Grows a tree like \code{\link{mchaid}} for a quantitative response variable: the categories of every
splitting variable are merged when the means of the response are not significantly different, the variable
with the smallest Bonferroni-adjusted p-value splits the node, and the process continues in every new node.
The tests are the F tests of an analysis of variance (the merging tests compare two groups; the splitting test
compares all the groups, or, for an ordered variable, enters the code of the group 1, 2, 3... as one linear
term). This is a port of the Stata command \code{maid}.
}
\usage{
maid(formula, data, ordered = NULL, unordered = NULL, xtile = NULL,
     weights = NULL, subset = NULL, minnode = 100, minsplit = 200,
     maxbranch = 3, missing = FALSE, mergalpha = 0.95, spltalpha = 0.05,
     adj = TRUE, exhaust = FALSE, anova = FALSE, renumber = TRUE,
     steps = FALSE, maxlevels = 20L, bonferroni = c("exact", "mchaid"), forcevars = NULL)
}
\arguments{
  \item{formula}{a formula \code{response ~ x1 + x2 + ...}, with a numeric response. \code{response ~ .} uses
    all the other columns of \code{data} as splitting variables.}
  \item{data}{a data frame.}
  \item{ordered, unordered, xtile, weights, subset, minnode, minsplit, mergalpha, spltalpha, adj,
    exhaust, renumber, steps, maxlevels, bonferroni, forcevars}{as in \code{\link{mchaid}}.}
  \item{maxbranch}{maximum number of splits from the root to any node (default 3: a large tree takes long and
    cannot be drawn; a negative value means no limit).}
  \item{missing}{logical. If \code{TRUE}, missing values of the splitting variables are a category of their
    own (floating in the ordered variables). Cases with a missing response are always left out.}
  \item{anova}{logical. Test the ordered splitting variables with the F test of the analysis of variance
    (one degree of freedom less than the number of groups) instead of the linear term of their codes, which
    assumes that the categories are equally spaced.}
}
\details{
The tests are computed from the number, the sum and the sum of squares of the response in each category,
so they do not need the data of the cases. When several candidates have the same p-value the information
criterion (AICc of the normal linear model) decides, and, if it is also equal, the choice is random: use
\code{set.seed()}. A node is not split if the response does not vary in it or if it has no more than
\code{minsplit} cases.

Not ported from the Stata command: the options \code{svy}, \code{permute} and \code{respalpha}.
}
\value{
An object of class \code{"maid"}, a list with, among others: \code{node} (node of each row of \code{data};
\code{NA} outside the estimation sample), \code{paths}, \code{splitp}, \code{splitstat} (the F statistic of each split, in the layout of
\code{splitp}), \code{sizes}, \code{nodes} (matrix with
the number of cases, mean, standard deviation, minimum, quartiles and maximum of the response in each node),
\code{predictors}, \code{y}, \code{w}, \code{N} and \code{N_clusters}. Methods: \code{print},
\code{\link[=summary.maid]{summary}}, \code{\link[=plot.maid]{plot}}, \code{\link[=predict.maid]{predict}}; see
also \code{\link{importance}} and \code{\link{asnetCoin}}.
}
\examples{
set.seed(1)
fit <- maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb",
            xtile = c(disp = 3, hp = 3), minnode = 5, minsplit = 10)
fit
summary(fit)
plot(fit)
predict(fit, mtcars[1:3, ], type = "mean")
}
