mchaid <- function(formula, data, ordered = NULL, unordered = NULL, xtile = NULL, weights = NULL,
                   subset = NULL, minnode = 100, minsplit = 200, maxbranch = -1, missing = FALSE,
                   mergalpha = 0.95, spltalpha = 0.05, dvordered = NULL, adj = TRUE, exhaust = FALSE,
                   ordtable = FALSE, renumber = TRUE, steps = FALSE, maxlevels = 20L,
                   bonferroni = c("exact", "mchaid"), forcevars = NULL) {
  cl <- match.call()
  bonferroni <- match.arg(bonferroni)
  if (minsplit < minnode) stop("'minsplit' cannot be smaller than 'minnode'")
  if (mergalpha <= 0 || mergalpha >= 1 || spltalpha <= 0 || spltalpha >= 1) stop("'mergalpha' and 'spltalpha' must be between 0 and 1 (not included)")
  if (minnode <= 0) stop("'minnode' must be a positive number")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  wts <- eval(substitute(weights), data, parent.frame())
  sub <- eval(substitute(subset), data, parent.frame())
  if (is.null(dvordered) && inherits(formula, "formula") && length(formula) == 3L) {
    dvordered <- is.ordered(eval(formula[[2L]], data, environment(formula)))
  }
  if (dvordered && ordtable) stop("'ordtable' is not allowed with 'dvordered'")
  pp <- .prepare(formula, data, ordered, unordered, xtile, wts, sub, all.vars(substitute(weights)), missing, maxlevels, "cat", dvordered)
  preds <- pp$preds
  forced <- .check_forcevars(forcevars, preds, maxbranch)
  w <- pp$w
  rows <- pp$rows
  nd <- pp$nd

  # ---- response
  dvs <- pp$dv
  dvmiss <- is.na(dvs)
  if (is.factor(dvs)) dvlev <- levels(dvs)[levels(dvs) %in% as.character(dvs[!dvmiss])]
  else dvlev <- as.character(sort(unique(dvs[!dvmiss])))
  dvcode <- match(as.character(dvs), dvlev)
  if (any(dvmiss)) {
    dvlev <- c(dvlev, "missing")
    dvcode[dvmiss] <- length(dvlev)
  }
  J <- length(dvlev)
  if (J < 2L) stop("the response variable has a single category")

  g <- .grow(.engine_chaid(dvcode, J, w, dvordered, ordtable), preds, minnode, minsplit, maxbranch, mergalpha, spltalpha,
             adj, exhaust, renumber, steps, bonferroni == "exact", forced)
  .force_warning(g$fallback)
  k <- length(g$paths)
  node <- rep(NA_integer_, nd)
  node[rows] <- g$node
  freq <- matrix(0, k, J, dimnames = list(seq_len(k), dvlev))
  for (j in seq_len(J)) {
    a <- rowsum(w[dvcode == j], g$node[dvcode == j])
    freq[as.integer(rownames(a)), j] <- a[, 1]
  }
  out <- list(call = cl, formula = formula, depvar = pp$dvname, dvlevels = dvlev, dvordered = dvordered,
              predictors = preds, paths = g$paths, splitp = .splitp_matrix(g$splitp), splitstat = .splitp_matrix(g$splitstat), statname = "chi2", branches = lengths(g$paths),
              node = node, rows = rows, N = sum(w), N_clusters = k, sizes = rowSums(freq), freq = freq,
              dvcode = dvcode, w = w, nrow_data = nd, missing = missing, renumber = renumber,
              options = list(minnode = minnode, minsplit = minsplit, maxbranch = maxbranch, mergalpha = mergalpha,
                             spltalpha = spltalpha, adj = adj, exhaust = exhaust, ordtable = ordtable),
              forcevars = if (length(forced)) names(preds)[forced] else NULL, force_fallback = g$fallback)
  class(out) <- "mchaid"
  out
}
