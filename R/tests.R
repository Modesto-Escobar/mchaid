# Likelihood-ratio tests computed from cross-tabulations (port of the Mata functions of mchaid.ado:
# mchaid_lr2, mchaid_lrk, mchaid_lrt) and their ordinal-response counterparts (Stata's ologit).
# All of them work on a small table of counts: groups of categories (rows) by categories of the
# response variable (columns). They return c(p-value, AICc, chi2); AICc and chi2 are NA when they cannot be computed.

.nlogn <- function(n, N) {
  n <- n[n > 0]
  sum(n * log(n / N))
}

# small-sample corrected AIC, as in mchaid_getic
.aicc <- function(ll, parm, obs) {
  -2 * ll + 2 * parm + 2 * parm * (parm + 1) / (obs - parm - 1)
}

.chi2tail <- function(df, x) stats::pchisq(x, df, lower.tail = FALSE)

# the response levels without cases are dropped, as are the groups without cases
.clean_table <- function(tab) {
  tab <- tab[rowSums(tab) > 0, , drop = FALSE]
  tab[, colSums(tab) > 0, drop = FALSE]
}

# test of the difference between two groups of categories (merging step)
.lr_pair <- function(n1, n2, dvordered) {
  keep <- (n1 + n2) > 0
  n1 <- n1[keep]
  n2 <- n2[keep]
  J <- length(n1)
  N1 <- sum(n1)
  N2 <- sum(n2)
  if (J < 2 || N1 == 0 || N2 == 0) return(c(1, NA, NA))
  if (dvordered) return(.ologit_test(rbind(n1, n2), matrix(c(0, 1), 2, 1)))
  ll <- .nlogn(n1, N1) + .nlogn(n2, N2)
  ll0 <- .nlogn(n1 + n2, N1 + N2)
  chi2 <- max(2 * (ll - ll0), 0)
  df <- J - 1
  c(.chi2tail(df, chi2), .aicc(ll, df, N1 + N2), chi2)
}

# test of the association of the response with a splitting variable already merged into the rows of gt
#   type "u": groups as a nominal factor; type "o": groups coded 1, 2, 3... as one linear term
#   (or the full cross-tabulation with ordtable = TRUE)
.lr_split <- function(gt, type, dvordered, ordtable) {
  score <- seq_len(nrow(gt))
  ok <- rowSums(gt) > 0
  gt <- gt[ok, , drop = FALSE]
  score <- score[ok]
  gt <- gt[, colSums(gt) > 0, drop = FALSE]
  m <- nrow(gt)
  J <- ncol(gt)
  if (m < 2 || J < 2) return(c(1, NA, NA))
  if (dvordered) {
    X <- if (type == "u") diag(m)[, -1, drop = FALSE] else matrix(score, m, 1)
    return(.ologit_test(gt, X))
  }
  if (type == "u" || ordtable) return(.lr_table(gt))
  .lr_linear(gt, score)
}

# likelihood-ratio test of the full cross-tabulation, df = (m - 1)(J - 1)
.lr_table <- function(n) {
  N <- sum(n)
  ll <- sum(apply(n, 1, function(r) .nlogn(r, sum(r))))
  ll0 <- .nlogn(colSums(n), N)
  chi2 <- max(2 * (ll - ll0), 0)
  df <- (nrow(n) - 1) * (ncol(n) - 1)
  c(.chi2tail(df, chi2), .aicc(ll, df, N), chi2)
}

# multinomial logit of the response on one numeric score (the code of the group): Newton-Raphson
.lr_linear <- function(n, s) {
  m <- nrow(n)
  J <- ncol(n)
  N <- sum(n)
  Ng <- rowSums(n)
  K <- J - 1
  ll0 <- .nlogn(colSums(n), N)
  X <- cbind(1, s)
  theta <- numeric(2 * K)
  for (j in seq_len(K)) theta[2 * j - 1] <- log(sum(n[, j + 1]) / sum(n[, 1]))
  ll <- NA_real_
  llfun <- function(th) {
    full <- cbind(0, X %*% matrix(th, 2, K))
    lse <- log(rowSums(exp(full)))
    list(ll = sum(n * (full - lse)), logp = full - lse)
  }
  for (iter in 1:100) {
    cur <- llfun(theta)
    if (is.na(ll)) ll <- cur$ll
    p <- exp(cur$logp)
    g <- numeric(2 * K)
    H <- matrix(0, 2 * K, 2 * K)
    for (j in seq_len(K)) {
      ij <- (2 * j - 1):(2 * j)
      g[ij] <- crossprod(X, n[, j + 1] - Ng * p[, j + 1])
      for (k in seq_len(K)) {
        ik <- (2 * k - 1):(2 * k)
        wjk <- Ng * ((j == k) * p[, j + 1] - p[, j + 1] * p[, k + 1])
        H[ij, ik] <- -crossprod(X, wjk * X)
      }
    }
    step <- tryCatch(drop(.pinv_sym(-H) %*% g), error = function(e) rep(0, 2 * K))
    halve <- 0
    repeat {
      theta2 <- theta + step
      ll2 <- llfun(theta2)$ll
      halve <- halve + 1
      if (is.finite(ll2) && ll2 >= ll - 1e-12) break
      if (halve > 30) break
      step <- step / 2
    }
    if (!is.finite(ll2) || ll2 < ll) break
    theta <- theta2
    if (abs(ll2 - ll) < 1e-10 * (1 + abs(ll))) {
      ll <- ll2
      break
    }
    ll <- ll2
  }
  chi2 <- max(2 * (ll - ll0), 0)
  c(.chi2tail(K, chi2), .aicc(ll, K, N), chi2)
}

# generalised inverse of a symmetric matrix (like Mata's invsym: collinear columns get a zero row and column)
.pinv_sym <- function(A) {
  e <- eigen((A + t(A)) / 2, symmetric = TRUE)
  tol <- max(abs(e$values)) * 1e-12
  inv <- ifelse(abs(e$values) > tol, 1 / e$values, 0)
  e$vectors %*% (inv * t(e$vectors))
}

# proportional-odds (ordered logit) model of the response on the columns of X, fitted to a table of
# counts (rows: groups; columns: ordered levels of the response); X has one row per group and no
# constant. Returns the likelihood-ratio test against the model without X.
.ologit_test <- function(n, X) {
  n <- as.matrix(n)
  keep <- colSums(n) > 0
  n <- n[, keep, drop = FALSE]
  rk <- rowSums(n) > 0
  n <- n[rk, , drop = FALSE]
  X <- X[rk, , drop = FALSE]
  m <- nrow(n)
  J <- ncol(n)
  p <- ncol(X)
  if (J < 2 || m < 2 || p < 1) return(c(1, NA, NA))
  N <- sum(n)
  ll0 <- .nlogn(colSums(n), N)
  fit <- .ologit_fit(n, X)
  ll <- fit$ll
  if (!is.finite(ll)) return(c(1, NA, NA))
  chi2 <- max(2 * (ll - ll0), 0)
  c(.chi2tail(p, chi2), .aicc(ll, p, N), chi2)
}

.ologit_fit <- function(n, X) {
  m <- nrow(n)
  J <- ncol(n)
  p <- ncol(X)
  N <- sum(n)
  cs <- cumsum(colSums(n))[-J] / N
  k0 <- stats::qlogis(pmin(pmax(cs, 1e-9), 1 - 1e-9))
  a0 <- if (J > 2) c(k0[1], log(pmax(diff(k0), 1e-6))) else k0[1]
  par0 <- c(a0, numeric(p))
  parts <- function(par) {
    a <- par[seq_len(J - 1)]
    b <- par[J - 1 + seq_len(p)]
    kap <- cumsum(c(a[1], exp(a[-1])))
    eta <- drop(X %*% b)
    L <- stats::plogis(outer(-eta, kap, "+"))
    P <- pmax(cbind(L, 1) - cbind(0, L), 1e-300)
    list(a = a, L = L, P = P)
  }
  nll <- function(par) {
    q <- parts(par)
    -sum(n * log(q$P))
  }
  gr <- function(par) {
    q <- parts(par)
    lam <- q$L * (1 - q$L)
    w <- n / q$P
    d <- lam * (w[, -J, drop = FALSE] - w[, -1, drop = FALSE])
    gk <- colSums(d)
    gb <- -drop(crossprod(X, rowSums(d)))
    ga <- numeric(J - 1)
    ga[1] <- sum(gk)
    if (J > 2) for (t in 2:(J - 1)) ga[t] <- exp(q$a[t]) * sum(gk[t:(J - 1)])
    -c(ga, gb)
  }
  res <- stats::optim(par0, nll, gr, method = "BFGS", control = list(maxit = 1000, reltol = 1e-14))
  list(ll = -res$value, par = res$par)
}

# Bonferroni multiplier of the splitting test: number of ways in which the C original categories of a
# splitting variable can be merged into g groups (type "u": any categories; "o": adjacent categories only).
# For unordered variables the Stata command mchaid sums the terms i = 0..g-2 of the formula of the Stirling
# number of the second kind S(C, g) (sum over i = 0..g-1), so its multiplier is S(C, g) - (-1)^(g-1)/(g-1)!;
# exact = TRUE gives S(C, g) (Kass, 1980).
.bonferroni <- function(g, C, type, exhaust, exact = FALSE) {
  if (exhaust) return(if (type == "u") g * (g - 1) / 2 else g * (g^2 - 1) / 2)
  if (type == "u") {
    i <- 0:(g - if (exact) 1 else 2)
    return(sum((-1)^i * (g - i)^C / (factorial(i) * factorial(g - i))))
  }
  choose(C - 1, g - 1)
}
