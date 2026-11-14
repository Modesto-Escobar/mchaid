maid <- function(formula, data, ordered = NULL, unordered = NULL, xtile = NULL, weights = NULL,
                 subset = NULL, minnode = 100, minsplit = 200, maxbranch = 3, missing = FALSE,
                 mergalpha = 0.95, spltalpha = 0.05, adj = TRUE, exhaust = FALSE, anova = FALSE,
                 renumber = TRUE, steps = FALSE, maxlevels = 20L, bonferroni = c("exact", "mchaid"), forcevars = NULL) {
  cl <- match.call()
  bonferroni <- match.arg(bonferroni)
  if (minsplit < minnode) stop("'minsplit' cannot be smaller than 'minnode'")
  if (mergalpha <= 0 || mergalpha >= 1 || spltalpha <= 0 || spltalpha >= 1) stop("'mergalpha' and 'spltalpha' must be between 0 and 1 (not included)")
  if (minnode <= 0) stop("'minnode' must be a positive number")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  wts <- eval(substitute(weights), data, parent.frame())
  sub <- eval(substitute(subset), data, parent.frame())
  pp <- .prepare(formula, data, ordered, unordered, xtile, wts, sub, all.vars(substitute(weights)), missing, maxlevels, "num")
  forced <- .check_forcevars(forcevars, pp$preds, maxbranch)
  y <- as.numeric(pp$dv)
  w <- pp$w
  g <- .grow(.engine_maid(y, w, anova), pp$preds, minnode, minsplit, maxbranch, mergalpha, spltalpha,
             adj, exhaust, renumber, steps, bonferroni == "exact", forced)
  .force_warning(g$fallback)
  k <- length(g$paths)
  node <- rep(NA_integer_, pp$nd)
  node[pp$rows] <- g$node
  nodes <- t(vapply(seq_len(k), function(i) .wsummary(y[g$node == i], w[g$node == i]), numeric(8)))
  dimnames(nodes) <- list(seq_len(k), c("n", "mean", "sd", "min", "p25", "p50", "p75", "max"))
  out <- list(call = cl, formula = formula, depvar = pp$dvname, predictors = pp$preds, paths = g$paths,
              splitp = .splitp_matrix(g$splitp), splitstat = .splitp_matrix(g$splitstat), statname = "F", branches = lengths(g$paths), node = node, rows = pp$rows,
              N = sum(w), N_clusters = k, sizes = nodes[, "n"], nodes = nodes, y = y, w = w, nrow_data = pp$nd,
              missing = missing, renumber = renumber, anova = anova,
              options = list(minnode = minnode, minsplit = minsplit, maxbranch = maxbranch, mergalpha = mergalpha,
                             spltalpha = spltalpha, adj = adj, exhaust = exhaust),
              forcevars = if (length(forced)) names(pp$preds)[forced] else NULL, force_fallback = g$fallback)
  class(out) <- "maid"
  out
}

# n, mean, standard deviation, minimum, quartiles and maximum of a weighted sample (frequency weights); the
# percentiles are those of summarize, detail: the average of two values when the cumulative weight is exactly p
.wsummary <- function(y, w) {
  N <- sum(w)
  m <- sum(w * y) / N
  sd <- if (N > 1) sqrt(sum(w * (y - m)^2) / (N - 1)) else NA_real_
  o <- order(y)
  ys <- y[o]
  cw <- cumsum(w[o])
  pct <- function(p) {
    t <- N * p / 100
    if (abs(t - round(t)) < 1e-9) {
      i <- round(t)
      lo <- ys[which(cw >= i - 1e-9)[1L]]
      hi <- ys[which(cw >= i + 1 - 1e-9)[1L]]
      if (is.na(hi)) hi <- lo
      (lo + hi) / 2
    } else ys[which(cw >= ceiling(t) - 1e-9)[1L]]
  }
  c(N, m, sd, min(y), pct(25), pct(50), pct(75), max(y))
}

# sums of squares, F test and R-squared of the response by node
.results_maid <- function(x) {
  nd <- x$nodes
  k <- nrow(nd)
  N <- sum(x$w)
  m <- sum(x$w * x$y) / N
  sst <- sum(x$w * (x$y - m)^2)
  sse <- sum(ifelse(nd[, "n"] > 1, nd[, "sd"]^2 * (nd[, "n"] - 1), 0), na.rm = TRUE)
  ssb <- sst - sse
  df1 <- k - 1
  df2 <- N - k
  F <- if (df1 > 0 && df2 > 0 && sse > 0) (ssb / df1) / (sse / df2) else NA_real_
  list(nodes = nd, N = N, mean = m, sd = sqrt(sst / (N - 1)), sst = sst, sse = sse, ssb = ssb,
       r2 = if (sst > 0) ssb / sst else NA_real_, eta = if (sst > 0) sqrt(max(0, ssb / sst)) else NA_real_,
       F = F, p = if (is.na(F)) NA_real_ else stats::pf(F, df1, df2, lower.tail = FALSE), df1 = df1, df2 = df2,
       risk = sse / N, risk0 = sst / N)
}

print.maid <- function(x, ...) {
  r <- .results_maid(x)
  cat("Automatic Interaction Detection with F tests (MAID)\n")
  cat("Call: ", paste(deparse(x$call, width.cutoff = 100L), collapse = "\n"), "\n", sep = "")
  cat("Response: ", x$depvar, "; splitting variables: ", paste(names(x$predictors), collapse = ", "), "\n", sep = "")
  if (length(x$forcevars)) cat("Forced splits (levels 1 to ", length(x$forcevars), "): ", paste(x$forcevars, collapse = ", "), "\n", sep = "")
  cat("N = ", format(r$N, big.mark = ",", trim = TRUE), ", ", x$N_clusters, if (x$N_clusters == 1L) " node\n" else " nodes\n", sep = "")
  if (x$N_clusters == 1L) {
    cat("\nNo splits uncovered. Node #1 is null.\n")
    return(invisible(x))
  }
  fm <- function(v) format(round(v, 3), nsmall = 0, trim = TRUE)
  tab <- data.frame(Node = seq_len(x$N_clusters), n = format(x$sizes, big.mark = ",", trim = TRUE),
                    mean = fm(x$nodes[, "mean"]), sd = fm(x$nodes[, "sd"]), Definition = mchaid_node_labels(x, FALSE),
                    stringsAsFactors = FALSE)
  print(tab, row.names = FALSE, right = FALSE)
  cat("\nRisk estimate = ", format(r$risk, digits = 4), " (root node: ", format(r$risk0, digits = 4), "); R-squared = ",
      formatC(r$r2, format = "f", digits = 3), "\n", sep = "")
  invisible(x)
}

summary.maid <- function(object, branches = FALSE, definitions = FALSE, ...) {
  r <- .results_maid(object)
  r$object <- object
  r$branches <- branches
  r$definitions <- definitions
  class(r) <- "summary.maid"
  r
}

print.summary.maid <- function(x, digits = 2, ...) {
  o <- x$object
  hl <- strrep("-", 78)
  k <- o$N_clusters
  fmtn <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
  if (x$branches && k > 1L) {
    cat("\nAutomatic Interaction Detection with F tests (MAID) Tree Branching Results\n", hl, "\n\n", sep = "")
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
  cat("\n", o$depvar, " by MAID node\n", hl, "\n", sep = "")
  nd <- x$nodes
  allst <- .wsummary(o$y, o$w)
  tab <- rbind(nd[, c("n", "mean", "sd", "min", "p50", "max")], Total = allst[c(1, 2, 3, 4, 6, 8)])
  fm <- function(v) formatC(v, format = "f", digits = digits)
  jr <- function(v) format(v, justify = "right")
  out <- data.frame(Node = jr(c(seq_len(k), "Total")), n = jr(fmtn(tab[, "n"])), mean = jr(fm(tab[, "mean"])), sd = jr(fm(tab[, "sd"])),
                    min = jr(fm(tab[, "min"])), median = jr(fm(tab[, "p50"])), max = jr(fm(tab[, "max"])),
                    Definition = c(mchaid_node_labels(o, FALSE), paste0("(N = ", fmtn(x$N), ")")), stringsAsFactors = FALSE)
  print(out, row.names = FALSE, right = FALSE)
  cat("\nRisk estimate = ", format(x$risk, digits = 4), " (root node: ", format(x$risk0, digits = 4), ")\n", sep = "")
  cat("\nDifferences of ", o$depvar, " between nodes (N = ", fmtn(x$N), ")\n", sep = "")
  if (!is.na(x$F)) {
    pf <- if (x$p < 1e-4) "<0.0001" else formatC(x$p, format = "f", digits = 4)
    cat("  F(", x$df1, ", ", x$df2, ") = ", formatC(x$F, format = "f", digits = 2, width = 9), "   Pr = ", pf, "\n", sep = "")
  }
  cat("  R-squared = ", formatC(x$r2, format = "f", digits = 3), "   Eta = ", formatC(x$eta, format = "f", digits = 3), "\n", sep = "")
  cat("  Note: the nodes were chosen to differ as much as possible in ", o$depvar,
      ", so these tests describe the strength of the association but are not valid for inference.\n", sep = "")
  invisible(x)
}

predict.maid <- function(object, newdata = NULL, type = c("node", "mean"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    node <- object$node
  } else {
    n <- nrow(newdata)
    node <- if (object$N_clusters == 1L) rep(1L, n) else .route(object$paths, .code_newdata(object, newdata), n)
  }
  if (type == "node") return(node)
  unname(object$nodes[node, "mean"])
}
