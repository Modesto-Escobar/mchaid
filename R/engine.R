# The tests of the algorithm as interchangeable "engines" (used by .grow()):
#   $node(idx)       statistics of the node (the cases idx): list(N = size, pure = TRUE if nothing to split)
#   $table(code, K)  table of the categories of a splitting variable (rows) for the current node
#   $count(tab)      weighted number of cases of each row of a table
#   $pair(a, b)      c(p-value, AICc, statistic) of the difference between two groups of categories (rows of a table)
#   $split(gt, type) c(p-value, AICc, statistic) of the association with a splitting variable already merged in groups
#   $msg             text of the reason why a node is not split (for 'steps')

# categorical response (mchaid): likelihood-ratio tests of the cross-tabulation with the response
.engine_chaid <- function(dv, J, w, dvordered, ordtable) {
  idx <- NULL
  list(
    node = function(i) {
      idx <<- i
      dvt <- .wtab1(dv[i], w[i], J)
      list(N = sum(dvt), pure = sum(dvt > 0) == 1L)
    },
    table = function(code, K) .wtab(code, dv[idx], w[idx], K, J),
    count = function(tab) rowSums(tab),
    pair = function(a, b) .lr_pair(a, b, dvordered),
    split = function(gt, type) .lr_split(gt, type, dvordered, ordtable),
    msg = "the node is pure or has no more than minsplit(%d) observations")
}

# quantitative response (maid): F tests of one-way analyses of variance computed from the sufficient
# statistics of each category (n, sum and sum of squares of the response centred at the mean of the node)
.engine_maid <- function(y, w, anova) {
  idx <- NULL
  yc <- NULL
  list(
    node = function(i) {
      idx <<- i
      ww <- w[i]
      N <- sum(ww)
      m <- sum(ww * y[i]) / N
      yc <<- y[i] - m
      list(N = N, pure = N <= 1 || max(y[i]) == min(y[i]))
    },
    table = function(code, K) {
      ww <- w[idx]
      m <- matrix(0, K, 3)
      for (j in 1:3) {
        v <- switch(j, ww, ww * yc, ww * yc^2)
        agg <- rowsum(v, code)
        m[as.integer(rownames(agg)), j] <- agg[, 1]
      }
      m
    },
    count = function(tab) tab[, 1],
    pair = function(a, b) .f_pair(a, b),
    split = function(gt, type) .f_split(gt, type, anova),
    msg = "the node has no variation or no more than minsplit(%d) observations")
}

# F test of the difference between the means of two groups (a, b: n, sum, sum of squares)
.f_pair <- function(a, b) {
  if (a[1] <= 0 || b[1] <= 0) return(c(1, NA, NA))
  n <- a[1] + b[1]
  s <- a[2] + b[2]
  q <- a[3] + b[3]
  ssw <- (a[3] - a[2]^2 / a[1]) + (b[3] - b[2]^2 / b[1])
  sst <- q - s^2 / n
  dfr <- n - 2
  if (dfr <= 0 || ssw <= 1e-12 * q) return(c(1, NA, NA))
  ssb <- max(sst - ssw, 0)
  F <- ssb / (ssw / dfr)
  c(stats::pf(F, 1, dfr, lower.tail = FALSE), .aicc(.ll_gauss(ssw, n), 1, n), F)
}

# F test of the association of the response with the groups of a splitting variable: as a factor
# (unordered variable or option anova) or as one linear term of the code of the group (ordered variable)
.f_split <- function(gt, type, anova) {
  score <- seq_len(nrow(gt))
  ok <- gt[, 1] > 0
  gt <- gt[ok, , drop = FALSE]
  score <- score[ok]
  g <- nrow(gt)
  if (g < 2L) return(c(1, NA, NA))
  n <- gt[, 1]
  s <- gt[, 2]
  q <- gt[, 3]
  N <- sum(n)
  S <- sum(s)
  Q <- sum(q)
  sst <- Q - S^2 / N
  ssw <- sum(q - s^2 / n)
  if (type == "u" || anova) {
    df1 <- g - 1
    df2 <- N - g
    rss <- ssw
    ssb <- sst - ssw
  } else {
    xbar <- sum(n * score) / N
    sxx <- sum(n * (score - xbar)^2)
    sxy <- sum((score - xbar) * s)
    df1 <- 1
    df2 <- N - 2
    ssb <- sxy^2 / sxx
    rss <- sst - ssb
  }
  if (df2 <= 0 || rss <= 1e-12 * Q) return(c(1, NA, NA))
  F <- max(ssb, 0) / df1 / (rss / df2)
  c(stats::pf(F, df1, df2, lower.tail = FALSE), .aicc(.ll_gauss(rss, N), df1, N), F)
}

# log-likelihood of a linear model with normal errors, as in the e(ll) of regress
.ll_gauss <- function(rss, N) -N / 2 * (1 + log(2 * pi) + log(rss / N))
