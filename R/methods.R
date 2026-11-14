# Results, print, summary, predict, classification and importance

mchaid_node_labels <- function(x, numbers = TRUE) {
  txt <- vapply(x$paths, function(p) .path_text(x$predictors, p), "")
  if (numbers) paste0(seq_along(txt), ": ", txt) else txt
}

# frequencies, percentages, tests of association between the response and the nodes, risk estimate
.results <- function(x) {
  F <- x$freq
  k <- nrow(F)
  J <- ncol(F)
  N <- sum(F)
  E <- outer(rowSums(F), colSums(F)) / N
  chi2 <- sum((F - E)^2 / E)
  pos <- F > 0
  lr <- 2 * sum(F[pos] * log(F[pos] / E[pos]))
  df <- (k - 1) * (J - 1)
  m <- min(k, J) - 1
  V <- if (m >= 1) sqrt(chi2 / (N * m)) else NA_real_
  list(freq = F, rowpct = 100 * F / rowSums(F), colpct = 100 * sweep(F, 2, colSums(F), "/"),
       N = N, df = df, chi2 = chi2, p = if (df > 0) .chi2tail(df, chi2) else NA_real_,
       chi2_lr = lr, p_lr = if (df > 0) .chi2tail(df, lr) else NA_real_, CramersV = V,
       risk = 1 - sum(apply(F, 1, max)) / N, risk_root = 1 - max(colSums(F)) / N)
}

print.mchaid <- function(x, ...) {
  r <- .results(x)
  cat("Chi-Square Automated Interaction Detection (CHAID)\n")
  cat("Call: ", paste(deparse(x$call, width.cutoff = 100L), collapse = "\n"), "\n", sep = "")
  cat("Response: ", x$depvar, if (x$dvordered) " (ordered)" else "", "; splitting variables: ",
      paste(names(x$predictors), collapse = ", "), "\n", sep = "")
  if (length(x$forcevars)) cat("Forced splits (levels 1 to ", length(x$forcevars), "): ", paste(x$forcevars, collapse = ", "), "\n", sep = "")
  cat("N = ", format(r$N, big.mark = ",", trim = TRUE), ", ", x$N_clusters, if (x$N_clusters == 1L) " node\n" else " nodes\n", sep = "")
  if (x$N_clusters == 1L) {
    cat("\nNo splits uncovered. Node #1 is null.\n")
    return(invisible(x))
  }
  modal <- colnames(x$freq)[apply(x$freq, 1, which.max)]
  tab <- data.frame(Node = seq_len(x$N_clusters), n = format(x$sizes, big.mark = ",", trim = TRUE),
                    Modal = modal, Pct = sprintf("%.1f", 100 * apply(x$freq, 1, max) / x$sizes),
                    Definition = mchaid_node_labels(x, FALSE), stringsAsFactors = FALSE)
  print(tab, row.names = FALSE, right = FALSE)
  cat("\nRisk estimate = ", formatC(r$risk, format = "f", digits = 3), " (root node: ",
      formatC(r$risk_root, format = "f", digits = 3), "); Cramer's V = ", formatC(r$CramersV, format = "f", digits = 3), "\n", sep = "")
  invisible(x)
}

summary.mchaid <- function(object, branches = FALSE, definitions = FALSE, ...) {
  r <- .results(object)
  r$object <- object
  r$branches <- branches
  r$definitions <- definitions
  class(r) <- "summary.mchaid"
  r
}

# table of the branches of the tree: one column for each node, one row for each level of the tree
.branch_matrix <- function(x) {
  k <- x$N_clusters
  D <- max(lengths(x$paths))
  m <- matrix("", D + 1L, k, dimnames = list(c(seq_len(D), "node"), seq_len(k)))
  for (i in seq_len(k)) {
    for (d in seq_along(x$paths[[i]])) {
      s <- x$paths[[i]][[d]]
      pr <- x$predictors[[s$v]]
      m[d, i] <- paste0(pr$name, "@", paste(pr$labels[s$cats], collapse = " "))
    }
    m[D + 1L, i] <- paste0("Node #", i)
  }
  m
}

print.summary.mchaid <- function(x, digits = 1, ...) {
  o <- x$object
  hl <- strrep("-", 78)
  k <- o$N_clusters
  fmtn <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
  if (x$branches && k > 1L) {
    cat("\nChi-Square Automated Interaction Detection (CHAID) Tree Branching Results\n", hl, "\n\n", sep = "")
    print(noquote(.branch_matrix(o)), right = FALSE)
  }
  if (k == 1L) {
    cat("\nNo splits uncovered. Node #1 is null.\n")
    return(invisible(x))
  }
  if (x$definitions) {
    cat("\nNode definitions\n", hl, "\n", sep = "")
    labs <- mchaid_node_labels(o, FALSE)
    for (i in seq_len(k)) cat("Node ", i, " (n = ", fmtn(o$sizes[i]), "): ", labs[i], "\n", sep = "")
  }
  cat("\n", o$depvar, " by CHAID node\n", hl, "\n", sep = "")
  cat("Row percentages; frequencies in parentheses in the column Total\n")
  pct <- matrix(sprintf(paste0("%.", digits, "f"), x$rowpct), nrow(x$rowpct), ncol(x$rowpct))
  tot <- paste0("(", fmtn(o$sizes), ")")
  tab <- cbind(pct, tot)
  tab <- rbind(tab, c(sprintf(paste0("%.", digits, "f"), 100 * colSums(x$freq) / x$N), paste0("(", fmtn(x$N), ")")))
  dimnames(tab) <- list(c(mchaid_node_labels(o), paste0("Total (N = ", fmtn(x$N), ")")), c(colnames(x$freq), "Total"))
  print(noquote(tab), right = TRUE)
  cat("\nRisk estimate = ", formatC(x$risk, format = "f", digits = 3), " (root node: ",
      formatC(x$risk_root, format = "f", digits = 3), ")\n", sep = "")
  pf <- function(p) if (is.na(p)) "." else if (p < 1e-4) "<0.0001" else formatC(p, format = "f", digits = 4)
  cat("\nTests of association between ", o$depvar, " and node (N = ", fmtn(x$N), ")\n", sep = "")
  cat("  Pearson chi2(", x$df, ") = ", formatC(x$chi2, format = "f", digits = 2, width = 9), "   Pr = ", pf(x$p), "\n", sep = "")
  cat("  Likelihood-ratio chi2(", x$df, ") = ", formatC(x$chi2_lr, format = "f", digits = 2, width = 9), "   Pr = ", pf(x$p_lr), "\n", sep = "")
  cat("  Cramer's V = ", formatC(x$CramersV, format = "f", digits = 3), "\n", sep = "")
  cat("  Note: the nodes were chosen to be associated with ", o$depvar,
      ", so these tests describe the strength of the association but are not valid for inference.\n", sep = "")
  invisible(x)
}

# codes of the splitting variables for new data
.code_newdata <- function(object, newdata) {
  lapply(object$predictors, function(pr) {
    if (!(pr$name %in% names(newdata))) stop("variable '", pr$name, "' not found in 'newdata'")
    x <- newdata[[pr$name]]
    codes <- switch(pr$kind,
      xtile = match(as.integer(cut(x, pr$values, include.lowest = TRUE)), pr$used),
      numeric = match(x, pr$values),
      match(as.character(x), pr$values))
    nl <- if (pr$float) pr$K - 1L else pr$K
    codes[codes > nl] <- NA
    if (pr$float) codes[is.na(x)] <- pr$K
    as.integer(codes)
  })
}

# node of each case from the codes of its splitting variables (NA when it does not follow any path)
.route <- function(paths, codes, n) {
  node <- rep(NA_integer_, n)
  for (i in seq_along(paths)) {
    ok <- rep(TRUE, n)
    for (s in paths[[i]]) ok <- ok & (codes[[s$v]] %in% s$cats)
    node[ok] <- i
  }
  node
}

predict.mchaid <- function(object, newdata = NULL, type = c("node", "class", "prob"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    node <- object$node
  } else {
    n <- nrow(newdata)
    node <- if (object$N_clusters == 1L) rep(1L, n) else .route(object$paths, .code_newdata(object, newdata), n)
  }
  if (type == "node") return(node)
  if (type == "class") {
    modal <- apply(object$freq, 1, which.max)
    return(factor(colnames(object$freq)[modal[node]], levels = colnames(object$freq), ordered = object$dvordered))
  }
  p <- object$freq / rowSums(object$freq)
  p[node, , drop = FALSE]
}

classification <- function(x, positive = NULL) {
  if (!inherits(x, "mchaid")) stop("'x' must be an object returned by mchaid()")
  F <- x$freq
  L <- ncol(F)
  C <- matrix(0, L, L, dimnames = list(colnames(F), colnames(F)))
  for (i in seq_len(nrow(F))) {
    p <- which.max(F[i, ])
    C[, p] <- C[, p] + F[i, ]
  }
  n <- sum(C)
  S <- t(vapply(seq_len(L), function(i) {
    tp <- C[i, i]
    fn <- sum(C[i, ]) - tp
    fp <- sum(C[, i]) - tp
    tn <- n - tp - fn - fp
    100 * c(tp / (tp + fn), tn / (tn + fp), tp / (tp + fp), tn / (tn + fn), fp / (fp + tn), fn / (tp + fn),
            fp / (tp + fp), fn / (tn + fn), 2 * tp / (2 * tp + fp + fn))
  }, numeric(9)))
  dimnames(S) <- list(colnames(F), c("sensitivity", "specificity", "ppv", "npv", "fpos_true", "fneg_true", "fpos_class", "fneg_class", "f1"))
  pos <- 0L
  if (!is.null(positive)) {
    pos <- match(tolower(as.character(positive)), tolower(colnames(F)), nomatch = 0L)
    if (pos == 0L && is.numeric(positive) && positive >= 1 && positive <= L && positive == round(positive)) pos <- as.integer(positive)
    if (pos == 0L) stop("positive: '", positive, "' is neither a category of the response variable nor a number between 1 and ", L)
  }
  K <- sum(diag(C))
  out <- list(class = C, N = n, N_correct = K, pct_correct = 100 * K / n, risk = 1 - K / n, stats = S,
              f1_macro = mean(S[, "f1"]), f1_weighted = sum(S[, "f1"] * rowSums(C)) / n,
              depvar = x$depvar, N_clusters = x$N_clusters, positive = pos)
  class(out) <- "mchaid_classification"
  out
}

print.mchaid_classification <- function(x, ...) {
  C <- x$class
  L <- nrow(C)
  fmtn <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
  cat("\nClassification table: ", x$depvar, "\n", sep = "")
  cat("Each case is classified in the modal category of its node (", x$N_clusters, " nodes)\n\n", sep = "")
  tab <- cbind(matrix(fmtn(C), L, L), Total = fmtn(rowSums(C)), `% correct` = sprintf("%.1f", 100 * diag(C) / rowSums(C)))
  tab <- rbind(tab, Total = c(fmtn(colSums(C)), fmtn(x$N), sprintf("%.1f", x$pct_correct)))
  dimnames(tab) <- list(c(rownames(C), "Total"), c(colnames(C), "Total", "% correct"))
  cat("Rows: observed; columns: classified as\n")
  print(noquote(tab), right = TRUE)
  cat("\nCorrectly classified: ", sprintf("%.1f", x$pct_correct), "% (", fmtn(x$N_correct), " of ", fmtn(x$N), ")\n", sep = "")
  cat("Risk estimate (proportion misclassified): ", sprintf("%.3f", x$risk), "\n", sep = "")
  cat("\nStatistics of each category versus the rest of them (%)\n\n")
  S <- x$stats[, c("sensitivity", "ppv", "f1", "specificity", "npv"), drop = FALSE]
  colnames(S) <- c("Sensitivity", "Precision", "F1", "Specificity", "NPV")
  Sf <- matrix(ifelse(is.na(S), ".", sprintf("%.1f", S)), nrow(S), ncol(S), dimnames = dimnames(S))
  print(noquote(Sf), right = TRUE)
  cat("Sensitivity = recall, Precision = positive predictive value (PPV), NPV = negative predictive value\n")
  cat("F1 = 2*Precision*Sensitivity/(Precision + Sensitivity)\n")
  cat("F1 averaged over the categories: macro ", sprintf("%.1f", x$f1_macro), "%, weighted by the real cases ",
      sprintf("%.1f", x$f1_weighted), "%\n", sep = "")
  if (x$positive > 0L) {
    pos <- x$positive
    lab <- rownames(C)[pos]
    s <- x$stats[pos, ]
    tp <- C[pos, pos]
    acc <- 100 * (x$N - (sum(C[pos, ]) - tp) - (sum(C[, pos]) - tp)) / x$N
    cat("\nClassified + if classified in the category ", lab, "\n", sep = "")
    cat("True D defined as ", x$depvar, " = ", lab, "\n", strrep("-", 50), "\n", sep = "")
    st <- c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
            "False + rate for true ~D", "False - rate for true D", "False + rate for classified +", "False - rate for classified -")
    pr <- c("Pr( +| D)", "Pr( -|~D)", "Pr( D| +)", "Pr(~D| -)", "Pr( +|~D)", "Pr( -| D)", "Pr(~D| +)", "Pr( D| -)")
    for (i in 1:8) {
      cat(formatC(st[i], width = -32), formatC(pr[i], width = -10), formatC(if (is.na(s[i])) "." else sprintf("%.2f%%", s[i]), width = 8), "\n", sep = "")
      if (i == 4L || i == 8L) cat(strrep("-", 50), "\n", sep = "")
    }
    cat(formatC("F1 score", width = -42), formatC(if (is.na(s[9])) "." else sprintf("%.2f%%", s[9]), width = 8), "\n", sep = "")
    cat(formatC("Correctly classified", width = -42), formatC(sprintf("%.2f%%", acc), width = 8), "\n", strrep("-", 50), "\n", sep = "")
  }
  invisible(x)
}

# Cramer's V of the response by node (absolute value; 0 when it cannot be computed)
.fit_v <- function(dvcode, node, w, J) {
  ok <- !is.na(node)
  if (!any(ok)) return(0)
  k <- max(node[ok])
  F <- matrix(0, k, J)
  agg <- rowsum(w[ok], (dvcode[ok] - 1L) * k + node[ok])
  F[as.integer(rownames(agg))] <- agg[, 1]
  F <- F[rowSums(F) > 0, colSums(F) > 0, drop = FALSE]
  m <- min(dim(F)) - 1
  if (m < 1) return(0)
  E <- outer(rowSums(F), colSums(F)) / sum(F)
  v <- sqrt(sum((F - E)^2 / E) / (sum(F) * m))
  if (is.finite(v)) abs(v) else 0
}

importance <- function(x, nrep = 10L) {
  maidx <- inherits(x, "maid")
  if (!maidx && !inherits(x, "mchaid")) stop("'x' must be an object returned by mchaid() or maid()")
  if (x$N_clusters == 1L) stop("No splits uncovered: there is nothing to permute")
  resp <- if (maidx) x$y else x$dvcode
  idx <- seq_along(resp)
  w <- x$w
  if (any(w != 1)) {                                         # the cases are permuted, not the rows of a weighted table
    if (any(abs(w - round(w)) > 1e-8)) stop("importance() needs integer frequency weights")
    idx <- rep(idx, round(w))
  }
  n <- length(idx)
  resp <- resp[idx]
  codes <- lapply(x$predictors, function(p) p$codes[idx])
  ones <- rep(1, n)
  J <- if (maidx) 0L else length(x$dvlevels)
  fitfun <- if (maidx) function(node) .fit_r2(resp, node, ones) else function(node) .fit_v(resp, node, ones, J)
  fit <- fitfun(.route(x$paths, codes, n))
  dec <- vapply(seq_along(codes), function(v) {
    mean(vapply(seq_len(nrep), function(r) {
      cp <- codes
      cp[[v]] <- cp[[v]][sample.int(n)]
      fit - fitfun(.route(x$paths, cp, n))
    }, 0))
  }, 0)
  raw <- dec / sum(dec)
  out <- rbind(raw = raw, rank = rank(-raw, ties.method = "min"))
  colnames(out) <- names(x$predictors)
  out
}

# R-squared of the response on the nodes (0 when it cannot be computed)
.fit_r2 <- function(y, node, w) {
  ok <- !is.na(node)
  if (!any(ok)) return(0)
  y <- y[ok]
  w <- w[ok]
  node <- node[ok]
  m <- sum(w * y) / sum(w)
  sst <- sum(w * (y - m)^2)
  if (sst <= 0) return(0)
  nk <- rowsum(w, node)[, 1]
  sk <- rowsum(w * y, node)[, 1]
  sum(nk * (sk / nk - m)^2) / sst
}
