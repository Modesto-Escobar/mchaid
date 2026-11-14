# Text of the categories of the splitting variables (port of mchaid_levlab, mchaid_grptext,
# mchaid_catlist, mchaid_path and mchaid_pathtext of mchaid.ado)
#
# A splitting variable ("predictor") is a list with, among others:
#   name    name of the variable
#   type    "o" ordered, "u" unordered
#   K       number of categories (the floating "missing" category, if any, is the last one)
#   labels  text of each category
#   float   TRUE if the last category is "missing"
#   numeric TRUE if the labels are plain numbers (adjacent categories are then written first-last)

.sep_range <- function(a, b, isnum) paste0(a, if (isnum) "-" else "~", b)

# categories of a group, in order, as text; runs of three or more adjacent categories of an ordered
# variable are written as first-last (first~last when the labels are not numbers)
.levlab <- function(pr, cats, miss = "missing") {
  n <- length(cats)
  txt <- pr$labels[cats]
  if (pr$float) txt[cats == pr$K] <- miss
  if (pr$type == "o" && n >= 3) {
    pos <- ifelse(pr$float & cats == pr$K, 0L, as.integer(cats))
    idx <- order(pos)
    idx <- idx[pos[idx] > 0]
    rest <- which(pos == 0)
    items <- character(0)
    m <- length(idx)
    k <- 1L
    while (k <= m) {
      en <- k
      while (en < m && pos[idx[en + 1L]] == pos[idx[en]] + 1L) en <- en + 1L
      if (en - k + 1L >= 3L) items <- c(items, .sep_range(txt[idx[k]], txt[idx[en]], pr$numeric))
      else items <- c(items, txt[idx[k:en]])
      k <- en + 1L
    }
    items <- c(items, txt[rest])
    return(paste(items, collapse = ", "))
  }
  paste(txt, collapse = ", ")
}

# "{a, b}" or "a"; with withvar: "var in {a, b}" or "var = a"
.grptext <- function(pr, cats, withvar = FALSE, miss = "missing") {
  txt <- .levlab(pr, cats, miss)
  many <- length(cats) > 1L
  if (many) txt <- paste0("{", txt, "}")
  if (withvar) txt <- paste0(pr$name, if (many) " in " else " = ", txt)
  txt
}

.catlist <- function(preds, v, groups, sep = ", ", miss = "missing") {
  paste(vapply(groups, function(g) .grptext(preds[[v]], g, FALSE, miss), ""), collapse = sep)
}

# definition of a node from its path
.path_text <- function(preds, path, miss = "missing") {
  if (length(path) == 0L) return("all observations")
  paste(vapply(path, function(s) .grptext(preds[[s$v]], s$cats, TRUE, miss), ""), collapse = " & ")
}

# the splitting variable and the categories of each split of a path, as in e(path#) of the Stata version
.path_string <- function(preds, path) {
  if (length(path) == 0L) return("")
  paste0(vapply(path, function(s) paste0(preds[[s$v]]$name, "@", paste(preds[[s$v]]$labels[s$cats], collapse = " ")), ""), ";", collapse = "")
}

# p-value for the steps
.pfmt <- function(p) {
  if (is.na(p)) return(".")
  if (p < 1e-4) return("<0.0001")
  formatC(p, format = "f", digits = 4)
}

# p-value of a split for the graph: three decimals, or scientific notation with one decimal below 0.001
.ptxt <- function(p, cut = NULL) {
  if (is.na(p)) return("")
  if (!is.null(cut) && p < cut) return(paste0("p<", format(cut)))
  if (p >= 0.001) return(paste0("p=", formatC(p, format = "f", digits = 3)))
  "p<0.001"
}

# test statistic of a split, written before its p-value in the classic graph: "chi2=2131; " (F in maid)
.cstat <- function(ss, m1, du, sname) {
  if (is.null(ss) || nrow(ss) < m1 || ncol(ss) < du) return("")
  v <- ss[m1, du]
  if (is.na(v)) return("")
  paste0(sname, "=", formatC(v, format = "f", digits = if (v >= 100) 0 else if (v >= 10) 1 else 2), "; ")
}
