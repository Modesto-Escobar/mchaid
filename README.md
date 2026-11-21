# mchaid

[![R-CMD-check](https://github.com/Modesto-Escobar/mchaid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modesto-Escobar/mchaid/actions/workflows/R-CMD-check.yaml)

CHAID and AID decision trees for R: Chi-square Automatic Interaction Detection (Kass, 1980) for a categorical or
ordinal response and, with F tests, for a quantitative one. `mchaid` is a port to R of the Stata commands
`mchaid` (version 2.3, categorical response) and `maid` (quantitative response), which are based on the Stata
command `chaid` by Joseph N. Luchman. It uses only packages distributed with R (`stats`, `graphics`,
`grDevices`, `utils`); `jsonlite` is needed only for the interactive page of `tree_html()`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Modesto-Escobar/mchaid")
library(mchaid)
```

It is plain R code: no compiler is needed.

## Usage

```r
tt  <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq, minnode = 30, minsplit = 60)

fit                                   # nodes, modal category, risk
summary(fit, definitions = TRUE)      # response by node, risk, chi-square and Cramer's V
plot(fit, language = "es")            # the tree (returns its natural size in inches, invisibly)
plot(fit, treestyle = "boxes")        # style of boxes: every node with its plot, the test of each split and "Pred.:"
classification(fit, positive = "Yes") # classification table, sensitivity, precision, F1...
predict(fit, newdata, type = "class") # "node", "class" or "prob"
importance(fit)                       # permutation importance
asnetCoin(fit)                        # the tree as a network of the netCoin package (nodes, links, coordinates)
tree_html(fit, dir = "my_tree")       # interactive tree as a self-contained HTML page

# quantitative response (maid): F tests
fm <- maid(wage ~ occupation + industry + grade, data, unordered = c("occupation", "industry"),
           ordered = "grade", xtile = c(tenure = 4), minnode = 30, minsplit = 60)
fm; summary(fm); plot(fm, quartiles = TRUE); predict(fm, newdata, type = "mean")
plot(fm, treestyle = "boxes", nodeplot = "both")   # box plot and bar of the mean in every node
asnetCoin(fm)                         # same, coloured by the mean of the node
```

Among the features: ordered and unordered predictors, exhaustive CHAID, a floating category for missing values,
frequency weights, Bonferroni-adjusted splitting tests, `forcevars` to choose the first splits, value labels
(as read by `haven`) in all the texts, texts of the graphs in 10 languages, and several ways to draw the tree
(classic, boxes, interactive, network).

## Equivalences with Stata

| Stata | R |
|---|---|
| `mchaid y, unordered(a b) ordered(c d)` | `mchaid(y ~ a + b + c + d, data, unordered = c("a","b"), ordered = c("c","d"))` |
| `[fweight = w]` | `weights = w` |
| `if` / `in` | `subset = ...` |
| `xtile(x, n(5))` | `xtile = c(x = 5)` (the groups are labelled with their range) |
| `minnode() minsplit() maxbranch() mergalpha() spltalpha()` | the same arguments |
| `missing`, `dvordered`, `noadj`, `exhaust`, `ordtable`, `norenumber`, `steps` | `missing`, `dvordered`, `adj = FALSE`, `exhaust`, `ordtable`, `renumber = FALSE`, `steps` |
| `branches`, `definitions` | `summary(fit, branches = TRUE, definitions = TRUE)` |
| `nopval`, `pcut()`, `language()`, `quartiles()`, `nopct` | `plot(fit, pval = FALSE, pcut =, language =, quartiles =, pct = FALSE)` |
| `treestyle(boxes)`, `nodeplot()`, `center()`, `whisker()`, `noshade` | `plot(fit, treestyle = "boxes", nodeplot =, center =, whisker =, shade = FALSE)` (`center` only in `maid`) |
| `predicted` and `_CHAID` | `predict(fit)` and `fit$node` |
| `estat classification` | `classification(fit)` |
| `importance` | `importance(fit)` |
| (new) tree as a network | `asnetCoin(fit)` |
| `e(sizes)`, `e(splitp)`, `e(splitchi)` / `e(splitF)`, `r(rowpct)`... | `fit$sizes`, `fit$splitp`, `fit$splitstat`, `summary(fit)$rowpct`... |

Not ported: `svy`, `permute`, `nofast` and `respalpha()`.

Differences in use: the response and the predictors can be factors, character, logical or numeric (they do not
have to be non-negative integers); by default a numeric variable or an ordered factor is ordered and the rest
are not; `dvordered` is `is.ordered(y)` by default; `importance()` makes 10 permutations per variable (Stata,
one). Ties are broken at random: use `set.seed()`.

## Validation against Stata

The same data and options as the corrected `mchaid.ado` (2026-10-25) were run with `3411r2.dta` (voting
intention, ideology, age, education, religion...), `auto` and the Titanic table. The partition into nodes and
their numbering are identical in the 13 configurations of `mchaid` tried: nominal response with 4 unordered and
2 ordered variables, `missing`, `exhaust`, `ordtable`, weights, `norenumber`, `maxbranch`, `noadj`, `xtile`, and
ordinal response (`dvordered`) with and without `missing`.

For `maid()`, 14 configurations were compared with Stata (`3411r2.dta` with `esc` as response, `nlsw88` and
`mtcars`: `anova`, `exhaust`, `missing`, weights, `xtile`, `noadj`, `maxbranch`, `norenumber`): the partition
is identical and the statistics of every node (n, mean, standard deviation, minimum, quartiles and maximum)
agree up to the rounding of the CSV file (1e-6).

The statistics of every split (`fit$splitstat`) agree with `e(splitchi)` (Titanic: 434.469, 27.443, 142.727,
22.058) and `e(splitF)` (mtcars: 79.561) of Stata, and the percentiles of the style of boxes with those of
`summarize, detail` (type 2 of `quantile()`, also with weights).

## Three things found in `mchaid.ado` while porting it

1. **Bonferroni multiplier of the unordered variables.** The loop added one term less than the Stirling number
   S(C, g) (for g = 2 it gave 2^(C-1) instead of 2^(C-1) - 1). Corrected on 2026-10-25 in `mchaid.ado` and
   `maid.ado` (not in `chaid.ado`). In R, `bonferroni = "exact"` (default) is the corrected computation and
   `"mchaid"` reproduces the old formula.
2. **`dvordered` with perfectly separated groups.** `capture version 10: ologit` failed with r(430) and the
   program took p = 1, merging those groups first. Corrected by removing `version 10:` and, when `ologit` still
   does not converge (r(430)), using the limit of the likelihood-ratio test (the one `ologit` leaves in
   `e(chi2)`) instead of p = 1.
3. **`respalpha()` never has an effect.** The program calls `mchaidsplit()` with `match_p = -1` and the
   condition `min(p) < match_p` cannot be met. It is still not fixed; that is why it is not in the R version.

## Credits and license

`mchaid` (Stata and R) is based on the Stata command `chaid` by Joseph N. Luchman, whose algorithm and syntax
it follows. The interactive page includes a subset of D3 v5.16.0 (BSD 3-Clause) and the font Roboto
(Apache License 2.0); see `inst/www/NOTICE.md`.

Released under the GNU General Public License, version 3 (see `LICENSE.md`).

## News

See [NEWS.md](NEWS.md).
