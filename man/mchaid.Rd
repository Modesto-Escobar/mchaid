\name{mchaid}
\alias{mchaid}
\title{CHAID decision tree}
\description{
Grows a CHAID tree (Chi-square Automatic Interaction Detection; Kass, 1980) for a categorical
response variable. The categories of every splitting variable are merged when they are not
significantly different, the variable with the smallest Bonferroni-adjusted p-value splits the
node, and the process continues in every new node. This is a port of the Stata command
\code{mchaid} (Escobar, based on \code{chaid} by Luchman) and gives the same trees.
}
\usage{
mchaid(formula, data, ordered = NULL, unordered = NULL, xtile = NULL,
       weights = NULL, subset = NULL, minnode = 100, minsplit = 200,
       maxbranch = -1, missing = FALSE, mergalpha = 0.95, spltalpha = 0.05,
       dvordered = NULL, adj = TRUE, exhaust = FALSE, ordtable = FALSE,
       renumber = TRUE, steps = FALSE, maxlevels = 20L,
       bonferroni = c("exact", "mchaid"), forcevars = NULL)
}
\arguments{
  \item{formula}{a formula \code{response ~ x1 + x2 + ...}. \code{response ~ .} uses all the other
    columns of \code{data} as splitting variables.}
  \item{data}{a data frame.}
  \item{ordered, unordered}{character vectors with the names of the splitting variables treated as
    ordered (only adjacent categories are merged) or unordered (any categories can be merged). A
    variable named in neither is ordered if it is numeric or an ordered factor, and unordered if it
    is a factor, character or logical vector. The variables may have at most \code{maxlevels}
    distinct values.}
  \item{xtile}{named numeric vector (or list) with the number of quantile groups in which a
    continuous splitting variable is cut, for example \code{c(age = 5)}. The resulting groups are
    ordered categories labelled with their ranges.}
  \item{weights}{an optional vector of frequency weights (evaluated in \code{data}). Cases with a
    weight of zero are left out.}
  \item{subset}{an optional logical or numeric vector (evaluated in \code{data}) that selects the
    cases.}
  \item{minnode}{minimum number of cases in a terminal node.}
  \item{minsplit}{minimum number of cases in a node to try to split it.}
  \item{maxbranch}{maximum number of splits from the root to any node; a negative value means no
    limit.}
  \item{missing}{logical. If \code{TRUE}, missing values of the splitting variables are a category of
    their own (a "floating" one in the ordered variables: it can be merged with any category), and
    missing values of the response are one more category of it (except with \code{dvordered}, where
    those cases are excluded).}
  \item{mergalpha}{two categories are merged if the p-value of their difference is greater than
    \code{1 - mergalpha} (default 0.95, that is, p > 0.05).}
  \item{spltalpha}{a node is split if the adjusted p-value is smaller than this.}
  \item{dvordered}{logical. If \code{TRUE} the response is ordinal and the tests are those of a
    proportional-odds (ordered logit) model instead of those of a multinomial logit. The default is
    \code{is.ordered(response)}.}
  \item{adj}{logical. Bonferroni adjustment of the p-values of the splitting test.}
  \item{exhaust}{logical. Exhaustive CHAID (Biggs, de Ville and Suen, 1991): the merging continues
    until only two categories remain.}
  \item{ordtable}{logical. For the final splitting test of an ordered variable use the
    likelihood-ratio test of the whole cross-tabulation, as for unordered variables, instead of a
    single linear term of the group code. Not available with \code{dvordered}.}
  \item{renumber}{logical. Number the nodes in the order of the tree (default). With \code{FALSE}
    they are numbered in the order in which the algorithm creates them, as in the original
    \code{chaid}.}
  \item{steps}{logical. Print every step of the algorithm (candidate merges, tests, decisions).}
  \item{maxlevels}{maximum number of distinct values of a splitting variable.}
  \item{bonferroni}{multiplier of the Bonferroni adjustment for unordered variables:
    \code{"exact"} (default) is the number of ways of partitioning the categories into groups
    (Stirling number of the second kind; Kass, 1980), as in the Stata command since its correction on
    2026-10-25; \code{"mchaid"} reproduces the formula of \code{chaid} 2.2 and of earlier versions of
    \code{mchaid}, which omitted one term of the sum and was larger by \eqn{(-1)^{g-1}/(g-1)!} for \eqn{g}
    groups.}
  \item{forcevars}{character vector with the names of splitting variables (all of them declared in the formula,
    \code{ordered}, \code{unordered} or \code{xtile}) that choose the first splits of the tree: the first level of the
    tree is split by the first variable, the second level by the second one, and so on; below them the tree grows as usual.
    In every node the categories of the forced variable are merged as in the algorithm, and the node is split
    whether or not the adjusted p-value is below \code{spltalpha}; if it is not, merging goes on until two groups
    remain (a binary split). \code{minnode} is respected: if the sample cannot be split by the first variable the
    function stops with an error, and in a deeper node where the forced split is not possible (the variable has fewer than
    two categories there or a node would be too small) the node grows freely, with a warning. \code{maxbranch} cannot be
    smaller than the number of forced variables. The variables are saved in the \code{forcevars} component and the nodes that
    grew freely in \code{force_fallback}; \code{steps = TRUE} shows these decisions.}
}
\details{
The merging and splitting tests are likelihood-ratio tests computed from cross-tabulations of the
response by the (merged) categories of the splitting variable, which is what a multinomial logit
with dummy variables gives. For an ordered splitting variable the final test uses the group code
(1, 2, 3, ...) as one linear term (fitted by Newton-Raphson), as \code{chaid} does; see
\code{ordtable}. With an ordered response, a proportional-odds model fitted to the same tables is
used. When two groups of categories are completely separated by an ordered response the likelihood
has no finite maximum: the test is then computed at the limit and gives a very small p-value (the
Stata command did take p = 1 and merged them before its correction on 2026-10-25).

When several candidates have the same p-value, the information criterion (AICc) decides and, if this
is also equal, the choice is random: use \code{set.seed()} for reproducible results.

Not ported from the Stata command: the options \code{svy}, \code{permute}, \code{respalpha}
(re-splitting merged categories, which in the Stata code never takes effect) and \code{nofast}.
}
\value{
An object of class \code{"mchaid"}, a list with, among others: \code{node} (node of each row of
\code{data}; \code{NA} outside the estimation sample), \code{paths} (the splits that define each node),
\code{splitp} (adjusted p-value of each split on the path of each node), \code{splitstat} (its likelihood-ratio
chi-square, in the same layout), \code{sizes}, \code{freq}
(response by node), \code{predictors}, \code{N} and \code{N_clusters}. Methods: \code{print},
\code{\link[=summary.mchaid]{summary}}, \code{\link[=plot.mchaid]{plot}},
\code{\link[=predict.mchaid]{predict}}; see also \code{\link{classification}},
\code{\link{importance}}, \code{\link{asnetCoin}} and, for a quantitative response, \code{\link{maid}}.
}
\references{
Kass, G. V. (1980). An exploratory technique for investigating large quantities of categorical data.
\emph{Applied Statistics}, 29, 119-127.

Biggs, D., de Ville, B. and Suen, E. (1991). A method of choosing multiway partitions for
classification and decision trees. \emph{Journal of Applied Statistics}, 18, 49-62.
}
\author{Modesto Escobar, based on the Stata command \code{chaid} by Joseph N. Luchman.}
\examples{
## frequency weights: the Titanic table
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq,
              minnode = 30, minsplit = 60)
fit
summary(fit, definitions = TRUE)

## the first two levels chosen by the user: Sex, then Age
mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq,
       minnode = 30, minsplit = 60, forcevars = c("Sex", "Age"))
plot(fit)
classification(fit)

## a continuous predictor cut in quantiles, and an unordered one
set.seed(1)
fit2 <- mchaid(factor(am) ~ mpg + cyl, data = mtcars, xtile = c(mpg = 3),
               unordered = "cyl", minnode = 4, minsplit = 10)
fit2
}
