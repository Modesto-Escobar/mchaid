# Graph of the tree in the style of boxes (port of mchaid_tree2() and maid_tree2() of the Stata commands): every node
# of the tree is a box with its number, its size and a plot of the distribution of the response variable. For
# mchaid() it is the stacked bar of the categories (nodeplot = "bar") or, with an ordered response, a box plot on the
# scale of the categories (nodeplot = "box"); for maid() it is the deviation of the mean from the overall one
# (nodeplot = "bar"), a box plot with whiskers ("box") or both.

# texts of the graph of the boxes in several languages (accents as escapes so that the source is ASCII)
.lang_boxes <- function(code = "en") {
  u <- function(...) paste0(...)
  L <- list(
    en = c(legend = "(% within each node)", pred = "Pred.:", overallmed = "Overall median", above = "Above", below = "Below",
           minmax = "Min-Max", iqr = "1.5 IQR", scalecap = "Scale: lowest whisker, overall median, highest whisker",
           dots = "Observations", dotsamp = "a sample of those between the whiskers", leaf = "Terminal node"),
    es = c(legend = "(% dentro de cada nodo)", pred = "Pred.:", overallmed = "Mediana global", above = "Por encima", below = "Por debajo",
           minmax = "M\u00edn-M\u00e1x", iqr = "1,5 RIC", scalecap = "Escala: bigote m\u00ednimo, mediana global, bigote m\u00e1ximo",
           dots = "Observaciones", dotsamp = "muestra de las que quedan entre los bigotes", leaf = "Nodo terminal"),
    fr = c(legend = "(% dans chaque n\u0153ud)", pred = "Pr\u00e9d. :", overallmed = "M\u00e9diane globale", above = "Au-dessus", below = "En dessous",
           minmax = "Min-Max", iqr = "1,5 EIQ", scalecap = "\u00c9chelle : moustache minimale, m\u00e9diane globale, moustache maximale",
           dots = "Observations", dotsamp = "\u00e9chantillon de celles entre les moustaches", leaf = "N\u0153ud terminal"),
    de = c(legend = "(% innerhalb jedes Knotens)", pred = "Vorh.:", overallmed = "Gesamtmedian", above = "\u00dcber", below = "Unter",
           minmax = "Min-Max", iqr = "1,5 IQR", scalecap = "Skala: unterster Whisker, Gesamtmedian, oberster Whisker",
           dots = "Beobachtungen", dotsamp = "Stichprobe der Werte innerhalb der Whisker", leaf = "Endknoten"),
    it = c(legend = "(% all'interno di ogni nodo)", pred = "Prev.:", overallmed = "Mediana complessiva", above = "Sopra", below = "Sotto",
           minmax = "Min-Max", iqr = "1,5 SIQ", scalecap = "Scala: baffo minimo, mediana complessiva, baffo massimo",
           dots = "Osservazioni", dotsamp = "campione di quelle entro i baffi", leaf = "Nodo terminale"),
    pt = c(legend = "(% dentro de cada n\u00f3)", pred = "Prev.:", overallmed = "Mediana global", above = "Acima", below = "Abaixo",
           minmax = "M\u00edn-M\u00e1x", iqr = "1,5 AIQ", scalecap = "Escala: bigode m\u00ednimo, mediana global, bigode m\u00e1ximo",
           dots = "Observa\u00e7\u00f5es", dotsamp = "amostra das que ficam entre os bigodes", leaf = "N\u00f3 terminal"),
    ca = c(legend = "(% dins de cada node)", pred = "Pred.:", overallmed = "Mediana global", above = "Per sobre", below = "Per sota",
           minmax = "M\u00edn-M\u00e0x", iqr = "1,5 RIQ", scalecap = "Escala: bigoti m\u00ednim, mediana global, bigoti m\u00e0xim",
           dots = "Observacions", dotsamp = "mostra de les que queden entre els bigotis", leaf = "Node terminal"),
    gl = c(legend = "(% dentro de cada nodo)", pred = "Pred.:", overallmed = "Mediana global", above = "Por riba", below = "Por baixo",
           minmax = "M\u00edn-M\u00e1x", iqr = "1,5 RIC", scalecap = "Escala: bigote m\u00ednimo, mediana global, bigote m\u00e1ximo",
           dots = "Observaci\u00f3ns", dotsamp = "mostra das que quedan entre os bigotes", leaf = "Nodo terminal"),
    nl = c(legend = "(% binnen elke knoop)", pred = "Voorsp.:", overallmed = "Totale mediaan", above = "Boven", below = "Onder",
           minmax = "Min-Max", iqr = "1.5 IQR", scalecap = "Schaal: laagste whisker, totale mediaan, hoogste whisker",
           dots = "Waarnemingen", dotsamp = "steekproef van die binnen de whiskers", leaf = "Eindknoop"),
    eu = c(legend = "(% nodo bakoitzaren barruan)", pred = "Aurrez.:", overallmed = "Mediana orokorra", above = "Gainetik", below = "Azpitik",
           minmax = "Min-Max", iqr = "1.5 IQR", scalecap = "Eskala: bibote txikiena, mediana orokorra, bibote handiena",
           dots = "Behaketak", dotsamp = "bibote artekoen lagina", leaf = "Amaierako nodoa"))
  as.list(L[[code]])
}

# number written with the decimal and thousands separators of the language (en: 1,234.5; fr: 1 234,5; the others: 1.234,5)
.loc <- function(s, lang) {
  if (lang == "en") return(s)
  th <- if (lang == "fr") " " else "."
  s <- gsub(",", "\001", s, fixed = TRUE)
  s <- gsub(".", ",", s, fixed = TRUE)
  gsub("\001", th, s, fixed = TRUE)
}

# a number of the graph: four significant digits (thousands separators from 10,000)
.fmtn <- function(v, lang) {
  if (is.na(v)) return("")
  if (abs(v) >= 10000) return(.loc(formatC(v, format = "f", digits = 0, big.mark = ","), lang))
  .loc(trimws(formatC(v, digits = 4, format = "g")), lang)
}

# a number of the response variable of maid (mean, median, SD, scale) with a fixed number of decimals (thousands separators from 1,000)
.fmtd <- function(v, digits = 1, lang = "en") {
  if (is.na(v)) return("")
  if (round(v, digits) == 0) v <- 0
  .loc(formatC(v, format = "f", digits = digits, big.mark = ","), lang)
}

# digits: a whole number between 0 and 6
.check_digits <- function(digits) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0 || digits > 6 || digits != round(digits))
    stop("'digits' must be a whole number between 0 and 6", call. = FALSE)
  as.integer(digits)
}

# heading of the root node: "total" (translated), "depvar" (name of the response variable) or any text
.root_head <- function(rootlabel, lang, depvar) {
  tot <- c(en = "Total", es = "Total", fr = "Total", de = "Gesamt", it = "Totale", pt = "Total", ca = "Total", gl = "Total", nl = "Totaal", eu = "Guztira")
  if (identical(rootlabel, "total")) { if (lang %in% names(tot)) tot[[lang]] else "Total" }
  else if (identical(rootlabel, "depvar")) as.character(depvar)[1L]
  else as.character(rootlabel)[1L]
}

# chi2 as text: the symbol if the device can write it (the pdf and postscript devices cannot with their default fonts)
.chi_name <- function() {
  if (grDevices::dev.cur() == 1L) grDevices::dev.new()
  if (names(grDevices::dev.cur()) %in% c("pdf", "postscript", "xfig", "pictex")) "chi2" else "\u03c7\u00b2"
}

# test statistic and p-value of a split for the graph of the boxes: "chi2 = 13,764; p < 0,001"
.stattxt <- function(stat, p, sname, showp, cut, lang) {
  out <- ""
  if (!is.na(stat)) {
    s <- if (stat >= 100) formatC(stat, format = "f", digits = 0, big.mark = ",") else if (stat >= 10) formatC(stat, format = "f", digits = 1)
         else formatC(stat, format = "f", digits = 2)
    out <- paste0(sname, " = ", .loc(s, lang))
  }
  if (showp && !is.na(p)) {
    ptx <- if (!is.null(cut) && p < cut) paste0("p < ", .loc(format(cut), lang))
           else if (p < 0.001) paste0("p < ", .loc("0.001", lang))
           else paste0("p = ", .loc(formatC(p, format = "f", digits = 3), lang))
    out <- paste0(out, if (nzchar(out)) "; " else "", ptx)
  }
  out
}

# weighted (frequency weights) statistics: n, mean, sd, min, p5, q1, median, q3, p95, max and the limits of the whiskers
# of Tukey (the most extreme values within 1.5 IQR of the quartiles); the percentiles are those of summarize, detail
.wstats <- function(y, w) {
  o <- order(y)
  ys <- y[o]
  cw <- cumsum(w[o])
  n <- length(ys)
  W <- cw[n]
  m <- sum(w * y) / W
  v <- if (W > 1) sum(w * (y - m)^2) / (W - 1) else 0
  q <- vapply(c(5, 25, 50, 75, 95), function(p) {
    P <- W * p / 100
    i <- min(sum(cw < P - 1e-9) + 1L, n)
    if (abs(cw[i] - P) < 1e-9 && i < n) (ys[i] + ys[i + 1L]) / 2 else ys[i]
  }, 0)
  iqr <- q[4L] - q[2L]
  c(W, m, sqrt(v), ys[1L], q, ys[n], ys[min(sum(ys < q[2L] - 1.5 * iqr) + 1L, n)], ys[max(sum(ys <= q[4L] + 1.5 * iqr), 1L)])
}

# statistics of the distribution of an ordered response in a node on the scale 0-K (category j occupies the band j-1 to j; the
# percentiles are interpolated inside the category where the cumulative percentage reaches them): lower edge of the first
# occupied category, p5, q1, median, q3, p95 and upper edge of the last occupied category
.pcat <- function(pc) {
  K <- length(pc)
  tot <- sum(pc)
  occ <- which(pc > 0)
  out <- vapply(c(5, 25, 50, 75, 95), function(p) {
    p <- tot * p / 100
    cum <- 0
    for (j in seq_len(K)) {
      if (pc[j] > 0 && cum + pc[j] >= p - 1e-9) return((j - 1) + min(1, max(0, (p - cum) / pc[j])))
      cum <- cum + pc[j]
    }
    NA_real_
  }, 0)
  c(min(occ) - 1, out, max(occ))
}

# points of the plot of a node with nodeplot = "dots" (maid): all the observations that fall outside the whiskers (the outliers and
# the extremes) and, of those between them, at most as many as keep the node at maxdots points (a systematic sample on the cumulative
# weight of the sorted values, so that it keeps the shape of the distribution and is always the same; never fewer than maxdots/10).
# The vertical position inside the band (-1 to 1) is a low-discrepancy sequence, not random numbers: nothing depends on the seed.
.dots_select <- function(v, w, lo, hi, maxdots = 200) {
  o <- order(v)
  v <- v[o]
  w <- w[o]
  out <- v < lo - 1e-9 | v > hi + 1e-9
  nout <- sum(out)
  inn <- which(!out)
  Win <- sum(w[inn])
  tot <- ceiling(Win - 1e-9)
  m <- if (length(inn)) min(tot, max(maxdots - nout, ceiling(maxdots / 10))) else 0
  vals <- v[out]
  if (m > 0) {
    cw <- cumsum(w[inn])
    pos <- (seq_len(m) - 0.5) * Win / m
    vals <- sort(c(vals, v[inn][vapply(pos, function(p) which(cw >= p - 1e-9)[1L], 1L)]))
  }
  list(v = vals, j = ((seq_along(vals) * 0.6180339887) %% 1) * 2 - 1, sampled = m < tot)
}

# points of every node (maid, nodeplot = "dots"): list with v, j and sampled
.dots_nodes <- function(x, Mem, wlo, whi, maxdots) {
  nodeobs <- x$node[x$rows]
  lapply(seq_len(ncol(Mem)), function(u) .dots_select(x$y[Mem[nodeobs, u]], x$w[Mem[nodeobs, u]], wlo[u], whi[u], maxdots))
}

# the maxdots argument: a whole number from 20 to 5000
.check_maxdots <- function(m) {
  if (!is.numeric(m) || length(m) != 1L || is.na(m) || m < 20 || m > 5000 || m != round(m)) stop("'maxdots' must be a whole number between 20 and 5000")
  m
}

# level (1-5) of the shade of the colour of a node: the more standard deviations from the reference the stronger
.shade_level <- function(z, shade) if (!shade || is.na(z)) 5L else 1L + as.integer(round(4 * min(abs(z), 1)))

# lines of a box plot (y1, x1, y2, x2, kind) centred in px at height yr on the scale vmin-vmax that takes the width bw
.boxlines <- function(lo, q1, med, q3, hi, px, yr, vmin, vmax, bw) {
  s <- if (vmax > vmin) bw / (vmax - vmin) else 0
  f <- function(v) px - bw / 2 + (v - vmin) * s
  rbind(c(yr, f(lo), yr, f(q1), 2), c(yr, f(q3), yr, f(hi), 2), c(yr - 0.06, f(lo), yr + 0.06, f(lo), 2),
        c(yr - 0.06, f(hi), yr + 0.06, f(hi), 2), c(yr - 0.12, f(med), yr + 0.12, f(med), 3))
}

# the device is opened once; the layout is computed (dry runs) until the scale of the layout on the device, which fixes the
# size at which the texts are measured, is stable, and then drawn
.plot_boxes <- function(x, kind, ...) {
  cex <- list(...)$cex
  if (!is.null(cex) && (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex < 0.5 || cex > 3)) stop("'cex' must be a number between 0.5 and 3")
  op0 <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit(graphics::par(op0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  pin <- graphics::par("pin")
  sc <- 0.8
  for (i in 1:3) {
    d <- .plot_boxes_do(x, kind, ..., sc = sc, dry = TRUE)
    sc <- min(pin[1L] / d[1L], pin[2L] / d[2L])
  }
  .plot_boxes_do(x, kind, ..., sc = sc, dry = FALSE)
}

# a value of an option of the graph checked against its choices; the message names the option (match.arg() says 'arg')
.match_opt <- function(x, choices, name) {
  if (is.null(x) || identical(x, choices)) return(choices[1L])
  i <- if (is.character(x) && length(x) == 1L && !is.na(x)) pmatch(x, choices) else NA_integer_
  if (is.na(i)) stop("'", name, "' must be one of ", paste0("\"", choices, "\"", collapse = ", "), call. = FALSE)
  choices[i]
}

.plot_boxes_do <- function(x, kind, main = NULL, note = NULL, pval = TRUE, pcut = NULL, language = "en", pct = TRUE, nodeplot = NULL,
                        center = NULL, whisker = "p5", shade = TRUE, col = NULL, palette = "default", cex = 1, nodehead = c("category", "number"),
                        rootlabel = "total", digits = 1, maxdots = 200, sc = 0.8, dry = FALSE) {
  ismaid <- identical(kind, "maid")
  if (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex < 0.5 || cex > 3) stop("'cex' must be a number between 0.5 and 3")
  cex_txt <- cex                                              # cex multiplies the texts as seen on the device; the layout is that of cex = 1
  cex <- 1
  k <- x$N_clusters
  lang <- tolower(trimws(language))
  if (lang == "po") lang <- "pt"
  tx <- c(if (ismaid) .lang_maid(lang) else .lang(lang), .lang_boxes(lang))
  if (is.null(nodeplot)) nodeplot <- if (ismaid) "box" else "bar"
  nodeplot <- if (is.null(nodeplot)) (if (ismaid) "box" else "bar") else .match_opt(nodeplot, if (ismaid) c("bar", "box", "both", "dots") else c("bar", "box"), "nodeplot")
  isdots <- nodeplot == "dots"                                # maid: box plot with the observations as points under it (it is laid out as a "box")
  if (isdots) {
    nodeplot <- "box"
    maxdots <- .check_maxdots(maxdots)
  }
  if (!ismaid && nodeplot == "box" && !isTRUE(x$dvordered)) stop("nodeplot = \"box\" needs a tree grown with an ordered response (dvordered)")
  center <- .match_opt(center, c("mean", "median"), "center")
  whisker <- .match_opt(whisker, c("p5", "minmax", "tukey"), "whisker")
  nodehead <- .match_opt(nodehead, c("category", "number"), "nodehead")
  digits <- .check_digits(digits)
  .fmtn <- function(v, lang) .fmtd(v, digits, lang)           # the numbers of the response variable (mean, median, SD, scale) take "digits" decimals
  headcat <- nodehead == "category"                           # heading of the boxes: the category of the splitting variable that forms the node
  isbar <- nodeplot == "bar"
  isbox <- nodeplot == "box"
  isboth <- nodeplot == "both"
  plotbox <- !isbar                                           # the box plot is drawn (box, both)
  cut <- pcut
  showp <- isTRUE(pval)
  sname <- if (ismaid) "F" else .chi_name()
  preds <- x$predictors
  paths <- x$paths
  sizes <- x$sizes
  sp <- x$splitp
  ss <- x$splitstat
  b <- lengths(paths)
  D <- max(b)
  # ---- nodes of the tree: the root (1) and every group of categories of every level
  sig <- function(s) paste0(s$v, ":", paste(s$cats, collapse = ","))
  Pk <- matrix("", k, D)
  for (i in seq_len(k)) {
    key <- ""
    for (d in seq_len(b[i])) {
      key <- paste0(key, "|", sig(paths[[i]][[d]]))
      Pk[i, d] <- key
    }
  }
  xl <- seq_len(k)
  if (!isTRUE(x$renumber)) xl[order(Pk[cbind(seq_len(k), b)], method = "radix")] <- seq_len(k)
  nodes <- sort(unique(Pk[Pk != ""]), method = "radix")
  nt <- length(nodes) + 1L
  key <- c("", nodes)
  dep <- c(0L, nchar(nodes) - nchar(gsub("|", "", nodes, fixed = TRUE)))
  Mem <- matrix(FALSE, k, nt)
  Mem[, 1L] <- TRUE
  isl <- logical(nt)
  cl <- rep(NA_integer_, nt)
  par <- integer(nt)
  for (u in 2:nt) {
    Mem[, u] <- rowSums(Pk == key[u]) > 0
    ll <- which(Mem[, u] & b == dep[u])
    if (length(ll)) {
      isl[u] <- TRUE
      cl[u] <- ll[1L]
    }
    par[u] <- if (dep[u] == 1L) 1L else match(Pk[which(Mem[, u])[1L], dep[u] - 1L], key)
  }
  xu <- rep(NA_real_, nt)
  for (d in D:0) for (u in which(dep == d)) {
    if (isl[u]) xu[u] <- xl[cl[u]]
    else {
      ch <- which(par == u)
      xu[u] <- (min(xu[ch]) + max(xu[ch])) / 2
    }
  }
  num <- integer(nt)
  c0 <- 0L
  for (d in 0:D) {
    idx <- which(dep == d)
    for (i in idx[order(xu[idx])]) {
      num[i] <- c0
      c0 <- c0 + 1L
    }
  }
  zsd <- 1
  # ---- distribution of the response in each node
  if (!ismaid) {
    rp <- .results(x)$rowpct
    nlev <- ncol(rp)
    dvl <- colnames(x$freq)
    if (isTRUE(x$missing) && !isTRUE(x$dvordered) && dvl[nlev] == "missing") dvl[nlev] <- tx$missing
    PC <- matrix(0, nt, nlev)
    nsz <- numeric(nt)
    for (u in seq_len(nt)) {
      mm <- Mem[, u]
      nsz[u] <- sum(sizes[mm])
      if (isl[u]) PC[u, ] <- rp[cl[u], ]
      else {
        cnt <- colSums(sizes[mm] * rp[mm, , drop = FALSE] / 100)
        if (sum(cnt) > 0) PC[u, ] <- 100 * cnt / sum(cnt)
      }
    }
    N <- sum(sizes)
    sdv <- rep(NA_real_, nt)
    if (isbox) {
      S2 <- t(apply(PC, 1L, .pcat))                           # lower edge, p5, q1, median, q3, p95, upper edge
      stc <- S2[, 4L]
      cnt <- PC[1L, ]
      mm <- sum(cnt * ((seq_len(nlev) - 0.5))) / sum(cnt)
      zsd <- max(0.1, sqrt(sum(cnt * ((seq_len(nlev) - 0.5) - mm)^2) / sum(cnt)))
      wlo <- switch(whisker, minmax = S2[, 1L], tukey = pmax(S2[, 1L], S2[, 3L] - 1.5 * (S2[, 5L] - S2[, 3L])), S2[, 2L])
      whi <- switch(whisker, minmax = S2[, 7L], tukey = pmin(S2[, 7L], S2[, 5L] + 1.5 * (S2[, 5L] - S2[, 3L])), S2[, 6L])
      q1 <- S2[, 3L]
      q3 <- S2[, 5L]
      med <- S2[, 4L]
      vmin <- 0
      vmax <- nlev
      refm <- stc[1L]
      ref <- refm
    }
  } else {
    nodeobs <- x$node[x$rows]
    S <- t(vapply(seq_len(nt), function(u) .wstats(x$y[Mem[nodeobs, u]], x$w[Mem[nodeobs, u]]), numeric(12)))
    N <- S[1L, 1L]
    nsz <- S[, 1L]
    stc <- if (center == "mean") S[, 2L] else S[, 7L]
    ref <- stc[1L]
    refm <- S[1L, 7L]
    zsd <- S[1L, 3L]
    wlo <- switch(whisker, minmax = S[, 4L], tukey = S[, 11L], S[, 5L])
    whi <- switch(whisker, minmax = S[, 10L], tukey = S[, 12L], S[, 9L])
    q1 <- S[, 6L]
    q3 <- S[, 8L]
    med <- S[, 7L]
    sdv <- S[, 3L]
    DT <- NULL
    if (isdots) DT <- .dots_nodes(x, Mem, wlo, whi, maxdots)
    lo_all <- if (isdots) S[, 4L] else wlo                      # the scale has to hold the points too
    hi_all <- if (isdots) S[, 10L] else whi
    pad <- 0.04 * (max(max(hi_all), ref, refm) - min(min(lo_all), ref, refm))
    if (pad == 0) pad <- 1
    vmin <- min(min(lo_all), ref, refm) - pad
    vmax <- max(max(hi_all), ref, refm) + pad
  }
  dev <- if (isbox && !ismaid) stc - stc[1L] else if (ismaid) stc - ref else rep(0, nt)
  zz <- dev / zsd
  dmax <- if (ismaid) max(c(abs(dev[-1L]), 0)) else 0
  if (dmax == 0) dmax <- 1
  # ---- texts of the nodes
  nline <- vapply(seq_len(nt), function(u) .loc(paste0(format(nsz[u], big.mark = ",", trim = TRUE, scientific = FALSE), " (",
                                                      formatC(100 * nsz[u] / N, format = "f", digits = 1), "%)"), lang), "")
  nname <- vapply(seq_len(nt), function(u) sub("@", num[u], tx$node, fixed = TRUE), "")
  l3 <- rep("", nt)
  l4 <- rep("", nt)
  if (ismaid) {
    cen1 <- if (center == "mean") tx$mean else tx$median
    cen1 <- paste0(toupper(substr(cen1, 1L, 1L)), substr(cen1, 2L, nchar(cen1)))
    for (u in seq_len(nt)) {
      l3[u] <- paste0(cen1, " ", .fmtn(stc[u], lang))
      if (u > 1L) l3[u] <- paste0(l3[u], " (", if (dev[u] >= 0) "+" else "-", .fmtn(abs(dev[u]), lang), ")")
      l4[u] <- paste0(toupper(tx$sd), " ", .fmtn(sdv[u], lang))
    }
  } else if (isbox) {
    for (u in seq_len(nt)) l3[u] <- paste0(tx$median, ": ", dvl[min(max(ceiling(stc[u]), 1L), nlev)])
  }
  lab <- vector("list", nt)
  svn <- rep("", nt)
  sct <- rep("", nt)
  headlab <- rep("", nt)                                      # heading of the box (nodehead = "category"): category or interval of the node
  headlab[1L] <- .root_head(rootlabel, lang, x$depvar)
  for (u in seq_len(nt)) {
    m1 <- which(Mem[, u])[1L]
    if (u > 1L) {
      st <- paths[[m1]][[dep[u]]]
      headlab[u] <- .levlab(preds[[st$v]], st$cats, tx$missing)
      ln <- .wrap(headlab[u], 18L)
      if (length(ln) > 3L) ln <- c(ln[1:2], paste0(ln[3L], " ..."))
      lab[[u]] <- ln
    }
    if (!isl[u]) {
      du <- dep[u] + 1L
      vn <- preds[[paths[[m1]][[du]]$v]]$name
      if (nchar(vn) > 26L) vn <- paste0(substr(vn, 1L, 24L), "...")
      svn[u] <- paste0(vn, .fmark(paths[[m1]][[du]]))
      p <- if (nrow(sp) >= m1 && ncol(sp) >= du) sp[m1, du] else NA_real_
      st <- if (!is.null(ss) && nrow(ss) >= m1 && ncol(ss) >= du) ss[m1, du] else NA_real_
      sct[u] <- .stattxt(st, p, sname, showp, cut, lang)
    }
  }
  # width in inches of the layout of a label of height h, measured with the real font at the size it will be drawn
  # (sc = inches of the device for an inch of the layout, found by .plot_boxes with a dry run)
  tw <- function(h, lab, bold = FALSE) {
    lab <- as.character(lab)
    if (!length(lab)) return(0)
    1.05 * graphics::strwidth(lab, units = "inches", cex = h * sc * 72 * 1.1 / graphics::par("ps"), font = if (bold) 2L else 1L) / sc
  }
  # ---- sizes (inches); cex scales the texts, the boxes adapt to them
  fnm <- 0.165 * cex; fnn <- 0.125 * cex; fsv <- 0.15 * cex; fch <- 0.115 * cex; fbl <- 0.12 * cex; fpc <- 0.105 * cex; fpr <- 0.14 * cex
  flg <- 0.15 * cex; flt <- 0.17 * cex; ft <- 0.26 * cex
  # heading that has to fit in mw inches: smaller letter (down to 0.125), then two lines (split where the longer is shortest), then cut
  fit_head <- function(s, mw) {
    for (z in seq(fnm, 0.125, by = -0.005)) if (tw(z, s, TRUE) <= mw) return(list(lines = s, h = z))
    words <- strsplit(s, " +")[[1L]]
    cutt <- function(t) {
      while (nchar(t) > 1L && tw(0.12, paste0(t, "..."), TRUE) > mw) t <- substr(t, 1L, nchar(t) - 1L)
      paste0(t, "...")
    }
    if (length(words) > 1L) {
      best <- NULL
      for (z in c(0.13, 0.125, 0.12)) {
        for (i in seq_len(length(words) - 1L)) {
          a <- paste(words[seq_len(i)], collapse = " ")
          b2 <- paste(words[(i + 1L):length(words)], collapse = " ")
          m <- max(tw(z, a, TRUE), tw(z, b2, TRUE))
          if (m <= mw && (is.null(best) || m < best$m)) best <- list(lines = c(a, b2), h = z, m = m)
        }
        if (!is.null(best)) return(best)
      }
      bi <- 1L
      bm <- Inf
      for (i in seq_len(length(words) - 1L)) {
        m <- max(tw(0.12, paste(words[seq_len(i)], collapse = " "), TRUE), tw(0.12, paste(words[(i + 1L):length(words)], collapse = " "), TRUE))
        if (m < bm) { bm <- m; bi <- i }
      }
      l1 <- paste(words[seq_len(bi)], collapse = " ")
      l2 <- paste(words[(bi + 1L):length(words)], collapse = " ")
      return(list(lines = c(if (tw(0.12, l1, TRUE) > mw) cutt(l1) else l1, if (tw(0.12, l2, TRUE) > mw) cutt(l2) else l2), h = 0.12))
    }
    list(lines = cutt(s), h = 0.12)
  }
  # a line (the prediction under the leaves) that has to fit in mw inches: smaller letter (down to 0.105), then cut
  fit_line <- function(s, h0, mw) {
    for (z in seq(h0, 0.105, by = -0.005)) if (tw(z, s, TRUE) <= mw) return(list(s = s, h = z))
    t <- s
    while (nchar(t) > 1L && tw(0.105, paste0(t, "..."), TRUE) > mw) t <- substr(t, 1L, nchar(t) - 1L)
    list(s = paste0(t, "..."), h = 0.105)
  }
  sh <- (cex - 1) * (if (ismaid) 0.71 else if (isbox) 0.57 else 0.39)          # extra height of the text block of a node
  hb <- (if (ismaid) (if (isbar) 1.4 else if (isbox) 1.3 else 1.72) else (if (isbox) 1.08 else 0.92)) + sh + (if (isdots) 0.34 else 0)
  barh <- 0.28
  zt <- 0.46 * cex
  wb <- 1.2                                                   # least width that leaves room for the bars and box plots
  for (u in seq_len(nt)) wb <- max(wb, tw(fnn, nline[u]) + 0.25, tw(fnn, l3[u]) + 0.25, tw(fnn, l4[u]) + 0.25,
                                  if (headcat) min(1.7, tw(0.14, headlab[u], TRUE) + 0.2) else tw(fnm, nname[u], TRUE) + 0.3)
  bw <- wb - (if (ismaid) 0.22 else 0.18)
  dx <- wb + 0.18
  if (!ismaid && !isbox) {
    legt <- paste(x$depvar, tx$legend)
  } else legt <- x$depvar
  ltw <- tw(flt, legt)
  whtxt <- switch(whisker, minmax = tx$minmax, tukey = tx$iqr, "P5-P95")
  boxtxt <- paste0("Q1-Q3, ", tx$median, ", ", whtxt)
  dotstxt <- if (isdots) paste0(tx$dots, if (any(vapply(DT, function(d) d$sampled, TRUE))) paste0(" (", tx$dotsamp, ")") else "") else ""
  showref2 <- ismaid && isboth && center == "mean"
  if (ismaid) {
    reftxt <- if (isbar) paste0(if (center == "mean") tx$overall else tx$overallmed, " = ", .fmtn(ref, lang)) else paste0(tx$overallmed, " = ", .fmtn(refm, lang))
    reftxt2 <- paste0(tx$overall, " = ", .fmtn(ref, lang))
  } else if (isbox) {
    reftxt <- paste0(tx$overallmed, " = ", dvl[min(max(ceiling(refm), 1L), nlev)])
    reftxt2 <- ""
  } else {
    reftxt <- ""
    reftxt2 <- ""
  }
  # legend: items (mark + text) flowed into rows no wider than the tree, 3 rows at most
  gp <- 0.2
  LI <- list()
  addLI <- function(w, put) LI[[length(LI) + 1L]] <<- list(w = w, put = put)
  if (!ismaid && !isbox) {
    for (j in seq_len(nlev)) local({
      jj <- j
      wj <- tw(flg, dvl[jj])
      addLI(0.16 + 0.1 + wj, function(xl, yl) {
        addR(xl, xl + 0.16, yl, 0.16, 10 + jj)
        addT(yl, xl + 0.16 + 0.1 + wj / 2, "t", dvl[jj], flg)
      })
    })
  } else {
    if (!isbar) addLI(0.5 + 0.14 + tw(flg, boxtxt), function(xl, yl) {
      addE(rbind(c(yl, xl, yl, xl + 0.5, 2), c(yl - 0.06, xl, yl + 0.06, xl, 2), c(yl - 0.06, xl + 0.5, yl + 0.06, xl + 0.5, 2),
                 c(yl - 0.12, xl + 0.25, yl + 0.12, xl + 0.25, 3)))
      addR(xl + 0.15, xl + 0.35, yl, 0.24, 900)
      addT(yl, xl + 0.5 + 0.14 + tw(flg, boxtxt) / 2, "t", boxtxt, flg)
    })
    if (isdots) addLI(0.36 + 0.1 + tw(flg, dotstxt), function(xl, yl) {
      addP(xl + c(0.04, 0.13, 0.22, 0.31), yl + c(0.03, -0.05, 0.04, -0.03))
      addT(yl, xl + 0.36 + 0.1 + tw(flg, dotstxt) / 2, "t", dotstxt, flg)
    })
    addLI(0.12 + 0.1 + tw(flg, reftxt), function(xl, yl) {
      addE(c(yl - 0.11, xl + 0.06, yl + 0.11, xl + 0.06, if (isbar) 5 else 4))
      addT(yl, xl + 0.12 + 0.1 + tw(flg, reftxt) / 2, "t", reftxt, flg)
    })
    if (showref2) addLI(0.2 + 0.1 + tw(flg, reftxt2), function(xl, yl) {
      addE(rbind(c(yl - 0.11, xl + 0.04, yl + 0.11, xl + 0.04, 6), c(yl - 0.11, xl + 0.14, yl + 0.11, xl + 0.14, 7)))
      addT(yl, xl + 0.2 + 0.1 + tw(flg, reftxt2) / 2, "t", reftxt2, flg)
    })
    addLI(0.16 + 0.1 + tw(flg, tx$above), function(xl, yl) {
      addR(xl, xl + 0.16, yl, 0.16, 240)
      addT(yl, xl + 0.16 + 0.1 + tw(flg, tx$above) / 2, "t", tx$above, flg)
    })
    addLI(0.16 + 0.1 + tw(flg, tx$below), function(xl, yl) {
      addR(xl, xl + 0.16, yl, 0.16, 241)
      addT(yl, xl + 0.16 + 0.1 + tw(flg, tx$below) / 2, "t", tx$below, flg)
    })
  }
  addLI(0.16 + 0.1 + tw(flg, tx$leaf), function(xl, yl) {                   # terminal node: the dark square of its number
    addR(xl, xl + 0.16, yl, 0.15, 7)
    addT(yl, xl + 0.16 + 0.1 + tw(flg, tx$leaf) / 2, "t", tx$leaf, flg)
  })
  legrows <- function(mw) {
    rows <- list()
    cur <- integer(0)
    cw <- 0
    for (i in seq_along(LI)) {
      wi <- LI[[i]]$w
      if (length(cur) && cw + gp + wi > mw) {
        rows[[length(rows) + 1L]] <- cur
        cur <- integer(0)
        cw <- 0
      }
      cw <- cw + (if (length(cur)) gp else 0) + wi
      cur <- c(cur, i)
    }
    c(rows, list(cur))
  }
  legmax <- max(k * dx, 8)                                    # the legend may be wider than the tree
  LR <- legrows(legmax)
  while (length(LR) > 3L && legmax < 100) {
    legmax <- legmax * 1.1
    LR <- legrows(legmax)
  }
  while (legmax > 0.5 && length(legrows(legmax - 0.05)) == length(LR)) legmax <- legmax - 0.05   # balance the rows
  LR <- legrows(legmax)
  nlr <- length(LR)
  rw <- vapply(LR, function(r) sum(vapply(LI[r], function(it) it$w, 0)) + gp * (length(r) - 1L), 0)
  legw <- max(rw)
  rh <- 0.26 * cex                                            # pitch of the legend rows
  ttl <- if (is.null(main)) "" else main
  ttlw <- if (nzchar(ttl)) tw(ft, ttl) else 0
  hasforced <- .has_forced(x)
  fnote <- if (hasforced) paste0("* ", .forced_note(lang)) else ""
  unote <- .user_note(note)                                   # note of the user: at the bottom left, under the legend and the note of the forced splits
  nun <- length(unote)
  enote <- .note_room(nun)
  xs <- max(k * dx + 0.6, legw + 0.6, ltw + 0.6, ttlw + 0.6, if (hasforced) tw(fch, fnote) + 0.6 else 0, if (nun) max(tw(fch, unote)) + 0.4 else 0)
  x0 <- (xs - k * dx) / 2
  px_ <- x0 + (xu - 0.5) * dx
  topm <- if (nzchar(ttl)) 0.35 + 0.25 * cex else 0.15
  if (headcat) topm <- topm + 0.12                               # the square with the number sticks out of the root
  yleg0 <- if (!ismaid && !isbox) 0.28 else if (isbar) 0.28 else if (ismaid) 0.82 else 0.62
  mg0 <- (if (ismaid) (if (isbar) 0.95 else 1.6) else (if (isbox) 1.4 else 1.05)) - (yleg0 + 0.33 + 0.094)
  ynote <- (if (hasforced) 0.2 * cex else 0) + enote                     # room for the line with the note of the forced variables
  yax <- (if (ismaid) 0.14 + 0.36 * cex else 0.13 + 0.17 * cex) + ynote          # axis of the scale of the box plots
  yleg <- (if ((!ismaid && !isbox) || isbar) 0.12 + 0.16 * cex else yax - ynote + 0.2 + 0.12 * cex) + ynote
  botm <- yleg + (nlr - 1L) * rh + (0.33 + 0.094 + mg0) * cex
  gap <- numeric(D + 1L)
  nlc <- vapply(seq_len(nt), function(u) length(lab[[u]]), 0L)
  for (d in seq_len(D) - 1L) {
    nl <- max(1L, nlc[dep == d + 1L])
    gap[d + 1L] <- if (headcat) zt + 0.30 else zt + 0.12 + (0.10 + 0.14 * cex * nl) + 0.20   # without labels on the branches the drop is short
  }
  ys <- topm + (D + 1L) * hb + sum(gap) + botm
  if (dry) return(c(xs, ys))
  yc <- numeric(D + 1L)
  yc[1L] <- ys - topm - hb / 2
  for (d in seq_len(D)) yc[d + 1L] <- yc[d] - hb - gap[d]
  aa <- 0.2 + 0.8 * (seq_len(5) - 1) / 4
  mc <- if (ismaid) .maid_col(col) else c("#0072B2", "#D55E00")      # above and below the reference
  cb <- .mix(mc[1L], aa)
  co <- .mix(mc[2L], aa)
  green <- grDevices::rgb(0, 158, 115, maxColorValue = 255)         # the median (Okabe-Ito green; red was confused with the orange of "below")
  if (!ismaid) col <- .resp_col(x, col, palette, "en")      # already resolved by plot.mchaid
  E <- matrix(numeric(0), 0, 5)
  Rc <- matrix(numeric(0), 0, 5)             # x1, x2, y, height, kind
  Tt <- list()
  Pt <- matrix(numeric(0), 0, 2)             # points of the nodes (dots)
  addP <- function(xx, yy) Pt <<- rbind(Pt, cbind(xx, yy))
  addE <- function(m) E <<- rbind(E, m)
  addR <- function(x1, x2, y, h, kd) Rc <<- rbind(Rc, c(x1, x2, y, h, kd))
  addT <- function(y, xx, kd, lab, h, bold = FALSE, colr = "black", adj = 0.5) Tt[[length(Tt) + 1L]] <<- list(y = y, x = xx, kind = kd, lab = lab, h = h, bold = bold, col = colr, adj = adj)
  for (u in seq_len(nt)) {
    d <- dep[u]
    px <- px_[u]
    ytop <- yc[d + 1L] + hb / 2
    addR(px - wb / 2, px + wb / 2, yc[d + 1L], hb, 1)
    if (headcat) {
      fh <- fit_head(headlab[u], wb - 0.16)
      nlh <- length(fh$lines)
      for (i in seq_len(nlh)) addT(ytop - (if (nlh == 1L) 0.17 else c(0.105, 0.25)[i]) * cex, px, "t", fh$lines[i], fh$h, TRUE)
      nb <- as.character(num[u])                              # number of the node: small square on top of the top left corner
      bwid <- max(0.16, 0.064 * nchar(nb) + 0.08)
      addR(px - wb / 2, px - wb / 2 + bwid, ytop + 0.075, 0.15, if (isl[u]) 7 else 5)   # a terminal node: dark square with the number in white
      addT(ytop + 0.075, px - wb / 2 + bwid / 2, "t", nb, 0.105, TRUE, if (isl[u]) "white" else "black")
    } else {
      if (isl[u]) addR(px - wb / 2, px + wb / 2, ytop - 0.165, 0.27, 8)                 # (nodehead = "number") a terminal node: dark heading with the text in white
      addT(ytop - 0.17 * cex, px, "t", nname[u], fnm, TRUE, if (isl[u]) "white" else "black")
    }
    addT(ytop - 0.39 * cex + (if (ismaid) 0.02 else 0), px, "t", nline[u], fnn)
    lvl <- .shade_level(zz[u], shade)
    sgn <- if (u == 1L) 0 else if (zz[u] >= 0) 1 else -1
    if (ismaid || isbox) addT(ytop - (if (ismaid) 0.55 else 0.57) * cex, px, "t", l3[u], fnn)
    if (ismaid) addT(ytop - 0.71 * cex, px, "t", l4[u], fnn)
    jm <- 0L
    if (!ismaid) {
      mx <- max(PC[u, ])
      jm <- which(PC[u, ] == mx)[1L]
    }
    if (!ismaid && !isbox) {                                # stacked bar of the categories
      ybar <- ytop - 0.70 - sh
      xa <- px - bw / 2
      cum <- 0
      for (j in seq_len(nlev)) {
        pc <- PC[u, j]
        if (pc > 0) {
          addR(xa + bw * cum / 100, xa + bw * (cum + pc) / 100, ybar, barh, 100 + j)
          if (pct && (pc >= 12 || pc == mx) && pc >= 1) {
            addT(ybar, xa + bw * (cum + pc / 2) / 100, "t", as.character(round(pc)), fpc, FALSE, .texton(col[j]))
          }
        }
        cum <- cum + pc
      }
    }
    if (ismaid && isbar) {                                   # deviation of the node from the reference, on the scale of the largest one
      yd <- ytop - 1.04 - sh
      sc <- (bw / 2) * 0.95 / dmax
      addE(c(yd - 0.25, px, yd + 0.25, px, 5))
      if (u > 1L) {
        if (dev[u] >= 0) addR(px, px + dev[u] * sc, yd, 0.36, 220 + lvl) else addR(px + dev[u] * sc, px, yd, 0.36, 230 + lvl)
      }
    }
    if (plotbox) {                                           # box plot on the scale common to all the nodes
      yr <- ytop - (if (!ismaid) 0.86 else if (isbox) 1.05 else 1.02) - sh
      f <- function(v) px - bw / 2 + (v - vmin) * bw / (vmax - vmin)
      addR(f(q1[u]), f(q3[u]), yr, 0.24, if (u == 1L) 900 else if (zz[u] >= 0) 200 + lvl else 210 + lvl)
      addE(.boxlines(wlo[u], q1[u], med[u], q3[u], whi[u], px, yr, vmin, vmax, bw))
      addE(c(yr - (if (isdots) 0.46 else 0.19), f(refm), yr + 0.19, f(refm), 4))
      if (isdots && length(DT[[u]]$v)) addP(f(DT[[u]]$v), yr - 0.31 + 0.11 * DT[[u]]$j)
      if (isboth && u > 1L) {                                # below it, the distance of the node to the overall mean on the same scale
        yd <- ytop - 1.42 - sh
        a0 <- f(ref)
        a1 <- f(stc[u])
        if (a1 >= a0) addR(a0, a1, yd, 0.26, 220 + lvl) else addR(a1, a0, yd, 0.26, 230 + lvl)
        addE(c(yd - 0.19, a0, yd + 0.19, a0, if (center == "mean") (if (dev[u] >= 0) 6 else 7) else 5))
      }
    }
    if (isl[u] && !ismaid && (!isbox || TRUE) && jm > 0L) {
      colj <- if (isbox) "grey35" else col[jm]
      if (isbox) {
        colj <- if (u == 1L) "grey35" else .darken(if (zz[u] >= 0) cb[lvl] else co[lvl])
      } else {
        colj <- .darken(col[jm])
      }
      fp <- fit_line(paste(tx$pred, dvl[jm]), fpr, dx - 0.06)    # it may not touch the prediction of the next leaf
      addT(yc[d + 1L] - hb / 2 - 0.17 * cex, px, "t", fp$s, fp$h, TRUE, colj)
    }
    if (isl[u]) next
    yb <- yc[d + 1L] - hb / 2
    yline <- yb - zt
    ww <- max(tw(fsv, svn[u], TRUE), tw(fch, sct[u])) + 0.1
    addR(px - ww / 2, px + ww / 2, yb - 0.23 * cex, 0.36 * cex, 2)
    addT(yb - 0.15 * cex, px, "t", svn[u], fsv, TRUE)
    if (nzchar(sct[u])) addT(yb - 0.32 * cex, px, "t", sct[u], fch)
    addE(c(yb, px, yline, px, 0))
    ch <- which(par == u)
    if (max(px_[ch]) > min(px_[ch])) addE(c(yline, min(px_[ch]), yline, max(px_[ch]), 0))
    for (v in ch) {
      ytopc <- yc[d + 2L] + hb / 2
      nlv <- nlc[v]
      labh <- 0.10 + 0.14 * cex * nlv
      labc <- ytopc + 0.20 + labh / 2
      if (headcat) {                                         # the category is in the heading of the box: the line goes straight to it
        addE(c(yline, px_[v], ytopc, px_[v], 0))
        next
      }
      addE(rbind(c(yline, px_[v], labc, px_[v], 0), c(labc, px_[v], ytopc, px_[v], 1)))
      wl <- max(tw(fbl, lab[[v]]))
      addR(px_[v] - wl / 2 - 0.08, px_[v] + wl / 2 + 0.08, labc, labh, 3)
      for (i in seq_len(nlv)) addT(labc + ((nlv + 1) / 2 - i) * 0.14 * cex, px_[v], "t", lab[[v]][i], fbl)
    }
  }
  # ---- legend
  for (r in seq_len(nlr)) {
    xl <- (xs - rw[r]) / 2
    for (i in LR[[r]]) {
      LI[[i]]$put(xl, yleg + (nlr - r) * rh)
      xl <- xl + LI[[i]]$w + gp
    }
  }
  addT(yleg + (nlr - 1L) * rh + 0.33 * cex, xs / 2, "t", legt, flt)
  if (ismaid || isbox) {
    if (plotbox) {                                           # scale of the box plots
      xg <- xs / 2 - bw / 2
      addE(c(yax, xg, yax, xg + bw, 2))
      if (ismaid) {
        a0 <- vmin + pad
        a1 <- vmax - pad
        for (j in 1:3) {
          v <- c(a0, refm, a1)[j]
          xr <- xg + (v - vmin) * bw / (vmax - vmin)
          addE(c(yax - 0.05, xr, yax + 0.05, xr, 2))
          if (j != 2L || min(refm - a0, a1 - refm) * bw / (vmax - vmin) > 0.4) addT(yax - 0.17 * cex, xr, "t", .fmtn(v, lang), fch)
        }
        addT(yax - 0.36 * cex, xs / 2, "t", tx$scalecap, fch)
      } else {
        for (j in 0:nlev) addE(c(yax - 0.05, xg + j * bw / nlev, yax + 0.05, xg + j * bw / nlev, 2))
        addT(yax - 0.17 * cex, xg + 0.5 * bw / nlev, "t", dvl[1L], fch)
        addT(yax - 0.17 * cex, xg + (nlev - 0.5) * bw / nlev, "t", dvl[nlev], fch)
      }
    }
  }
  if (nzchar(ttl)) addT(ys - 0.3, xs / 2, "t", ttl, ft)
  if (hasforced) addT(0.1 * cex + 0.02 + enote, xs / 2, "t", fnote, fch)
  for (i in seq_len(nun)) addT(0.12 + (nun - i) * 0.16, 0.15, "t", unote[i], fch, adj = 0)

  # ---- draw
  cv_ <- .tree_canvas(xs, ys, new = FALSE, cex = cex_txt)
  cexh <- cv_$cexh
  ln <- function(m, colr, lwd = 1, lty = 1) if (nrow(m)) graphics::segments(m[, 2], m[, 1], m[, 4], m[, 3], col = colr, lwd = cv_$lw(lwd), lty = lty)
  ln(E[E[, 5] == 0, , drop = FALSE], "grey40", 1)
  ln(E[E[, 5] == 1, , drop = FALSE], "grey40", 1, 2)
  rc <- function(m, fill, border = NA, lwd = 1) if (nrow(m)) graphics::rect(m[, 1], m[, 3] - m[, 4] / 2, m[, 2], m[, 3] + m[, 4] / 2, col = fill, border = border, lwd = cv_$lw(lwd))
  rc(Rc[Rc[, 5] == 2, , drop = FALSE], "white")
  rc(Rc[Rc[, 5] == 3, , drop = FALSE], "#F5F2EB")
  rc(Rc[Rc[, 5] == 1, , drop = FALSE], "white", "grey25", 2)
  rc(Rc[Rc[, 5] == 5, , drop = FALSE], "white", "grey25", 1.3)
  rc(Rc[Rc[, 5] == 7, , drop = FALSE], "grey25", "grey25", 1.3)
  rc(Rc[Rc[, 5] == 8, , drop = FALSE], "grey25", NA)
  rc(Rc[Rc[, 5] == 900, , drop = FALSE], "grey85", "grey35", 0.8)
  for (s in 1:5) {
    rc(Rc[Rc[, 5] == 200 + s, , drop = FALSE], cb[s], "grey35", 0.8)
    rc(Rc[Rc[, 5] == 210 + s, , drop = FALSE], co[s], "grey35", 0.8)
    rc(Rc[Rc[, 5] == 220 + s, , drop = FALSE], cb[s], "white", 0.5)
    rc(Rc[Rc[, 5] == 230 + s, , drop = FALSE], co[s], "white", 0.5)
  }
  if (!ismaid && !isbox) {
    for (j in seq_len(nlev)) {
      rc(Rc[Rc[, 5] == 100 + j, , drop = FALSE], col[j], "white", 0.5)
      rc(Rc[Rc[, 5] == 10 + j, , drop = FALSE], col[j], "white", 0.5)
    }
  }
  rc(Rc[Rc[, 5] == 240, , drop = FALSE], cb[5], "white", 0.5)
  rc(Rc[Rc[, 5] == 241, , drop = FALSE], co[5], "white", 0.5)
  ln(E[E[, 5] == 2, , drop = FALSE], "grey35", 1)
  ln(E[E[, 5] == 3, , drop = FALSE], "black", 2.4)
  ln(E[E[, 5] == 4, , drop = FALSE], green, 1.3)
  ln(E[E[, 5] == 5, , drop = FALSE], if (center == "mean") "#404040" else green, 1.3)
  ln(E[E[, 5] == 6, , drop = FALSE], cb[5], 1.3)
  ln(E[E[, 5] == 7, , drop = FALSE], co[5], 1.3)
  if (nrow(Pt)) graphics::points(Pt[, 1L], Pt[, 2L], pch = 16, cex = cexh(0.085), col = grDevices::adjustcolor("grey25", 0.6))
  for (t in Tt) graphics::text(t$x, t$y, t$lab, cex = cexh(t$h), font = if (t$bold) 2L else 1L, col = t$col, adj = c(t$adj, 0.5))
  invisible(c(width = xs, height = ys))
}
