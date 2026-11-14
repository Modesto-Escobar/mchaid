# Preparation of the data shared by mchaid() and maid(): response, weights, subset, splitting variables
# and estimation sample. 'resp' is "cat" (categorical response, mchaid) or "num" (quantitative, maid).

.prepare <- function(formula, data, ordered, unordered, xtile, wts, sub, wvars, missing, maxlevels, resp, dvordered = FALSE) {
  if (!inherits(formula, "formula") || length(formula) != 3L) stop("'formula' must be of the form response ~ predictors")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  env <- environment(formula)
  if (is.null(env)) env <- parent.frame()
  nd <- nrow(data)
  dvname <- paste(deparse(formula[[2L]], width.cutoff = 500L), collapse = "")
  dv0 <- eval(formula[[2L]], data, env)
  if (length(dv0) != nd) stop("the response must have one value for each row of 'data'")
  if (is.null(wts)) wts <- rep(1, nd)
  if (length(wts) != nd) stop("'weights' must have one value for each row of 'data'")
  if (!is.numeric(wts) || any(wts < 0, na.rm = TRUE)) stop("'weights' must be non-negative numbers")
  touse <- rep(TRUE, nd)
  if (!is.null(sub)) {
    if (is.logical(sub)) touse <- !is.na(sub) & sub
    else touse <- seq_len(nd) %in% sub
  }
  touse <- touse & !is.na(wts) & wts > 0
  if (resp == "num") {
    if (is.logical(dv0)) dv0 <- as.numeric(dv0)
    if (!is.numeric(dv0)) stop("the response of maid() must be numeric")
  }

  # ---- splitting variables
  yvars <- all.vars(formula[[2L]])
  rhs <- all.vars(formula[[3L]])
  if ("." %in% rhs) rhs <- c(setdiff(rhs, "."), setdiff(names(data), c(yvars, setdiff(rhs, "."), wvars)))
  xtn <- names(xtile)
  if (length(xtile) > 0L && is.null(xtn)) stop("'xtile' must be a named vector: the names are the variables")
  allv <- unique(c(rhs, ordered, unordered, xtn))
  allv <- setdiff(allv, yvars)
  if (length(allv) == 0L) stop("No independent variables entered")
  bad <- setdiff(allv, names(data))
  if (length(bad)) stop("variable(s) not found in 'data': ", paste(bad, collapse = ", "))
  dup <- intersect(ordered, unordered)
  if (length(dup)) stop("variable(s) in both 'ordered' and 'unordered': ", paste(dup, collapse = ", "))
  type <- vapply(allv, function(v) {
    if (v %in% xtn || v %in% ordered) return("o")
    if (v %in% unordered) return("u")
    x <- data[[v]]
    if (is.ordered(x) || is.numeric(x)) "o" else "u"
  }, "")
  # the unordered variables come first, then the ordered ones, as in the Stata commands
  allv <- c(allv[type == "u"], allv[type == "o"])
  type <- type[allv]

  # ---- estimation sample
  if (!missing) {
    touse <- touse & !is.na(dv0)
    for (v in allv) touse <- touse & !is.na(data[[v]])
  } else if (resp == "num" || dvordered) {
    touse <- touse & !is.na(dv0)
  }
  if (!any(touse)) stop("no observations")
  rows <- which(touse)
  w <- as.numeric(wts[rows])
  preds <- lapply(allv, function(v) .make_predictor(data[[v]][rows], v, type[[v]], missing, maxlevels, if (v %in% xtn) xtile[[v]] else NULL, w,
                                              attr(data[[v]], "labels", exact = TRUE)))
  names(preds) <- allv
  list(dvname = dvname, dv = dv0[rows], rows = rows, w = w, preds = preds, nd = nd)
}

# splitting variable: codes 1..K for the cases of the estimation sample, labels and the information
# needed to code new data (see predict.mchaid)
.make_predictor <- function(x, name, type, missing, maxlevels, nq, w, vl = NULL) {
  kind <- "numeric"
  values <- NULL
  breaks <- NULL
  if (!is.null(nq)) {
    if (!is.numeric(x)) stop("xtile: '", name, "' must be numeric")
    if (!is.numeric(nq) || length(nq) != 1L || nq < 2) stop("xtile: the number of groups of '", name, "' must be at least 2")
    kind <- "xtile"
    xs <- x[!is.na(x)]
    ws <- w[!is.na(x)]
    cuts <- unique(.wquantile(xs, ws, seq_len(nq - 1L) / nq))
    breaks <- c(-Inf, cuts, Inf)
    codes0 <- as.integer(cut(x, breaks, include.lowest = TRUE))
    lab0 <- .cut_labels(cuts, xs)
    used <- sort(unique(codes0[!is.na(codes0)]))
    codes <- match(codes0, used)
    labels <- lab0[used]
    isnum <- FALSE
    values <- breaks
  } else if (is.factor(x) || is.character(x) || is.logical(x)) {
    kind <- if (is.factor(x)) "factor" else if (is.logical(x)) "logical" else "character"
    f <- if (is.factor(x)) x else factor(x)
    lv <- levels(f)[levels(f) %in% as.character(f[!is.na(f)])]
    codes <- match(as.character(f), lv)
    labels <- lv
    values <- lv
    isnum <- FALSE
  } else {
    if (!is.numeric(x)) stop("variable '", name, "' must be numeric, a factor, character or logical")
    u <- sort(unique(x[!is.na(x)]))
    codes <- match(x, u)
    labels <- as.character(u)
    values <- u
    isnum <- TRUE
    # value labels of a labelled numeric variable (haven, e.g. a Stata .dta): the label is written instead of the code
    if (is.numeric(vl) && !is.null(names(vl))) {
      j <- match(as.numeric(u), as.numeric(vl))
      if (any(!is.na(j))) {
        labels[!is.na(j)] <- names(vl)[j[!is.na(j)]]
        isnum <- FALSE                                          # ranges of labels are written a~b, not a-b
      }
    }
  }
  used <- if (kind == "xtile") used else NULL
  K <- length(labels)
  if (K > maxlevels) {
    stop("The number of distinct values of '", name, "' (", K, ") is larger than allowed (", maxlevels,
         "). Consider collapsing similar categories, or use 'xtile' for ordered variables.", call. = FALSE)
  }
  float <- FALSE
  if (missing && any(is.na(codes))) {
    K <- K + 1L
    labels <- c(labels, "missing")
    codes[is.na(codes)] <- K
    float <- TRUE
  }
  list(name = name, type = type, K = K, codes = as.integer(codes), labels = labels, float = float,
       numeric = isnum, kind = kind, values = values, used = used)
}

# weighted quantiles: the smallest value whose cumulative proportion reaches p
.wquantile <- function(x, w, probs) {
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p - 1e-12)[1L]], 0)
}

.cut_labels <- function(cuts, x) {
  b <- c(-Inf, cuts, Inf)
  nb <- length(b) - 1L
  fm <- function(v) format(v, trim = TRUE, digits = 4)
  vapply(seq_len(nb), function(i) {
    if (i == 1L) paste0("<=", fm(b[2L]))
    else if (i == nb) paste0(">", fm(b[nb]))
    else paste0("(", fm(b[i]), ",", fm(b[i + 1L]), "]")
  }, "")
}
