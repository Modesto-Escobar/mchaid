# Data of the interactive tree (tree_html): for every node of the tree of an mchaid() or maid() fit, the same numbers and texts that the
# graph treestyle = "boxes" works out (see .plot_boxes_do in plot_boxes.R, of which this is an adaptation without the geometry: the drawing
# is left to inst/www/tree.js). The nodes and links are those of asnetCoin() (a node for the root, one for every group of categories of
# every split); here they are numbered as in the boxes graph (root 0, level by level from left to right) and carry, besides, the data of
# the plot of each node.

.tree_data <- function(x, nodeplot = NULL, center = NULL, whisker = "p5", shade = TRUE, pval = TRUE, pcut = NULL,
                      language = "en", col = NULL, palette = "default", ramp = FALSE, collapse = NULL, pct = TRUE, nodehead = "category", rootlabel = "total", digits = 1, maxdots = 200,
                      treestyle = "boxes", quartiles = NULL) {
  ismaid <- inherits(x, "maid")
  if (!ismaid && !inherits(x, "mchaid")) stop("'x' must be an object returned by mchaid() or maid()")
  k <- x$N_clusters
  if (k == 1L) stop("No splits uncovered: there is no tree")
  classic <- identical(treestyle, "classic")                  # treestyle = "classic": the layout of the classic graph of plot()
  if (isTRUE(quartiles)) quartiles <- "auto"
  if (isFALSE(quartiles)) quartiles <- NULL
  if (!is.null(quartiles) && !(quartiles %in% c("auto", "all"))) stop("'quartiles' must be \"auto\" or \"all\"")
  if (!classic) quartiles <- NULL
  if (classic) {                                              # the classic style has the bars (mchaid) or the deviation of the mean (maid)
    nodeplot <- "bar"
    center <- "mean"
    nodehead <- "category"
    shade <- FALSE
  }
  # heading of the boxes: "category" = the category (or interval) of the splitting variable that forms the node, with the number of the node
  # in a circle; "number" = the classic heading "Node N" with the labels of the branches
  nodehead <- match.arg(nodehead, c("category", "number"))
  digits <- .check_digits(digits)
  lang <- tolower(trimws(language))
  if (lang == "po") lang <- "pt"
  tx <- c(if (ismaid) .lang_maid(lang) else .lang(lang), .lang_boxes(lang))
  if (is.null(nodeplot)) nodeplot <- if (ismaid) "box" else "bar"
  nodeplot <- match.arg(nodeplot, if (ismaid) c("bar", "box", "both", "dots") else c("bar", "box"))
  isdots <- nodeplot == "dots"                                 # maid: box plot with the observations as points under it
  if (isdots) {
    nodeplot <- "box"
    maxdots <- .check_maxdots(maxdots)
  }
  if (!ismaid && nodeplot == "box" && !isTRUE(x$dvordered)) stop("nodeplot = \"box\" needs a tree grown with an ordered response (dvordered)")
  center <- match.arg(if (is.null(center)) "mean" else center, c("mean", "median"))
  whisker <- match.arg(whisker, c("p5", "minmax", "tukey"))
  isbar <- nodeplot == "bar"
  isbox <- nodeplot == "box"
  isboth <- nodeplot == "both"
  plotbox <- !isbar
  showp <- isTRUE(pval)
  sname <- if (ismaid) "F" else "\u03c7\u00b2"
  preds <- x$predictors
  paths <- x$paths
  sizes <- x$sizes
  sp <- x$splitp
  ss <- x$splitstat
  b <- lengths(paths)
  D <- max(b)
  # ---- nodes of the tree: the root and every group of categories of every level
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
  num <- integer(nt)                                         # number of the node, as in the boxes graph
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
  scale <- NULL
  if (!ismaid) {
    rp <- .results(x)$rowpct
    nlev <- ncol(rp)
    dvl <- colnames(x$freq)
    if (isTRUE(x$missing) && !isTRUE(x$dvordered) && dvl[nlev] == "missing") dvl[nlev] <- tx$missing
    PC <- matrix(0, nt, nlev)
    CT <- matrix(0, nt, nlev)                                  # counts
    nsz <- numeric(nt)
    for (u in seq_len(nt)) {
      mm <- Mem[, u]
      nsz[u] <- sum(sizes[mm])
      CT[u, ] <- colSums(x$freq[mm, , drop = FALSE])
      if (isl[u]) PC[u, ] <- rp[cl[u], ]
      else {
        cnt <- colSums(sizes[mm] * rp[mm, , drop = FALSE] / 100)
        if (sum(cnt) > 0) PC[u, ] <- 100 * cnt / sum(cnt)
      }
    }
    N <- sum(sizes)
    sdv <- rep(NA_real_, nt)
    if (isbox) {
      S2 <- t(apply(PC, 1L, .pcat))
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
  # ---- colours
  aa <- 0.2 + 0.8 * (seq_len(5) - 1) / 4
  mc <- if (ismaid) .maid_col(col) else c("#0072B2", "#D55E00")
  cb <- .mix(mc[1L], aa)
  co <- .mix(mc[2L], aa)
  if (!ismaid) colv <- .resp_col(x, col, palette, language, ramp)
  lvl <- vapply(zz, function(z) .shade_level(z, shade), 0L)
  nodecol <- ifelse(seq_len(nt) == 1L, "#D9D9D9", ifelse(zz >= 0, cb[lvl], co[lvl]))      # box plot / deviation bar
  # ---- texts of the nodes
  loc <- .loc
  fmtn <- function(v, lang) .fmtd(v, digits, lang)              # mean, median, SD, quantiles and scale of maid: "digits" decimals
  nline <- vapply(seq_len(nt), function(u) loc(paste0(format(nsz[u], big.mark = ",", trim = TRUE, scientific = FALSE), " (",
                                                    formatC(100 * nsz[u] / N, format = "f", digits = 1), "%)"), lang), "")
  nname <- vapply(seq_len(nt), function(u) sub("@", num[u], tx$node, fixed = TRUE), "")
  l3 <- rep("", nt)
  l4 <- rep("", nt)
  if (ismaid) {
    cen1 <- if (center == "mean") tx$mean else tx$median
    cen1 <- paste0(toupper(substr(cen1, 1L, 1L)), substr(cen1, 2L, nchar(cen1)))
    for (u in seq_len(nt)) {
      l3[u] <- paste0(cen1, " ", fmtn(stc[u], lang))
      if (u > 1L) l3[u] <- paste0(l3[u], " (", if (dev[u] >= 0) "+" else "-", fmtn(abs(dev[u]), lang), ")")
      l4[u] <- paste0(toupper(tx$sd), " ", fmtn(sdv[u], lang))
    }
  } else if (isbox) {
    for (u in seq_len(nt)) l3[u] <- paste0(tx$median, ": ", dvl[min(max(ceiling(stc[u]), 1L), nlev)])
  }
  lab <- vector("list", nt)
  svn <- rep("", nt)
  sct <- rep("", nt)
  spv <- rep(NA_real_, nt)
  stv <- rep(NA_real_, nt)
  defin <- character(nt)
  tid <- rep(NA_integer_, nt)
  headlab <- rep("", nt)                                       # heading of the box: the category of the splitting variable that forms the node
  headlab[1L] <- .root_head(rootlabel, lang, x$depvar)
  fmk <- if (classic && !ismaid) "\u2020" else "*"              # mark of the forced splits (the classic graph of mchaid uses a dagger: the asterisk marks the modal category)
  for (u in seq_len(nt)) {
    m1 <- which(Mem[, u])[1L]
    defin[u] <- .path_text(preds, paths[[m1]][seq_len(dep[u])], tx$missing)
    if (u > 1L) {
      st <- paths[[m1]][[dep[u]]]
      headlab[u] <- .levlab(preds[[st$v]], st$cats, tx$missing)
      ln <- .wrap(headlab[u], 18L)
      if (length(ln) > 3L) ln <- c(ln[1:2], paste0(ln[3L], " ..."))
      lab[[u]] <- ln
    }
    if (isl[u]) tid[u] <- cl[u]
    if (!isl[u]) {
      du <- dep[u] + 1L
      vn <- preds[[paths[[m1]][[du]]$v]]$name
      if (nchar(vn) > 26L) vn <- paste0(substr(vn, 1L, 24L), "...")
      svn[u] <- paste0(vn, .fmark(paths[[m1]][[du]], fmk))
      p <- if (nrow(sp) >= m1 && ncol(sp) >= du) sp[m1, du] else NA_real_
      st <- if (!is.null(ss) && nrow(ss) >= m1 && ncol(ss) >= du) ss[m1, du] else NA_real_
      spv[u] <- p
      stv[u] <- st
      sct[u] <- if (!classic) .stattxt(st, p, sname, showp, pcut, lang)
                else if (showp) paste0(.cstat(ss, m1, du, sname), .ptxt(p, pcut)) else ""      # classic: "chi2=29.0; p<0.001"
    }
  }
  # ---- modal category of the terminal nodes of mchaid (the "Pred." of the leaves) and its colour
  jm <- rep(0L, nt)
  predcol <- rep("#555555", nt)
  if (!ismaid) {
    for (u in seq_len(nt)) {
      jm[u] <- which(PC[u, ] == max(PC[u, ]))[1L]
      predcol[u] <- if (isbox) {
        if (u == 1L) "#595959" else .darken(if (zz[u] >= 0) cb[lvl[u]] else co[lvl[u]])
      } else .darken(colv[jm[u]])
    }
  }
  # ---- tooltips
  f1 <- function(v) loc(formatC(v, format = "f", digits = 1), lang)
  tip <- character(nt)
  for (u in seq_len(nt)) {
    h <- paste0("<b>", nname[u], "</b>", if (isl[u]) paste0(" &middot; ", sub("@", tid[u], if (lang == "es") "nodo final @" else "terminal node @", fixed = TRUE)) else "",
                "<br><span class='def'>", if (u == 1L) "all observations" else defin[u], "</span><br>n = ", nline[u])
    if (!ismaid) {
      h <- paste0(h, "<table class='dist'>", paste0("<tr><td><i style='background:", colv, "'></i>", dvl, "</td><td>", f1(PC[u, ]), "%</td><td>",
                  format(CT[u, ], big.mark = ",", trim = TRUE), "</td></tr>", collapse = ""), "</table>")
    } else {
      h <- paste0(h, "<br>", tx$mean, " = ", fmtn(S[u, 2L], lang), if (u > 1L) paste0(" (", if (S[u, 2L] - S[1L, 2L] >= 0) "+" else "-", fmtn(abs(S[u, 2L] - S[1L, 2L]), lang), ")") else "",
                  "<br>", tx$sd, " = ", fmtn(sdv[u], lang), "<br>min = ", fmtn(S[u, 4L], lang), " &middot; Q1 = ", fmtn(S[u, 6L], lang),
                  " &middot; ", tx$median, " = ", fmtn(S[u, 7L], lang), " &middot; Q3 = ", fmtn(S[u, 8L], lang), " &middot; max = ", fmtn(S[u, 10L], lang))
    }
    if (!isl[u] && nzchar(sct[u])) h <- paste0(h, "<br><span class='sp'>", svn[u], ": ", sct[u], "</span>")
    if (isl[u] && !ismaid) h <- paste0(h, "<br><b>", tx$pred, " ", dvl[jm[u]], "</b>")
    tip[u] <- h
  }
  # ---- the node records
  recs <- vector("list", nt)
  for (u in seq_len(nt)) {
    r <- list(id = num[u], parent = if (u == 1L) NULL else num[par[u]], depth = dep[u], leaf = isl[u], term = if (isl[u]) tid[u] else NULL,
              x = xu[u], name = nname[u], n = nsz[u], nline = nline[u], pctn = 100 * nsz[u] / N, l3 = l3[u], l4 = l4[u],
              branch = if (u > 1L) as.list(lab[[u]]) else list(), svar = if (!isl[u]) svn[u] else NULL, stat = if (!isl[u]) sct[u] else NULL,
              p = if (!isl[u]) spv[u] else NULL, def = defin[u], tip = tip[u], fill = nodecol[u], lvl = lvl[u], sign = if (u == 1L) 0L else if (zz[u] >= 0) 1L else -1L,
              headlab = headlab[u],
              nleaves = sum(Mem[, u]))                           # terminal nodes under this node (shown when the branch is collapsed)
    if (!ismaid) {
      r$pc <- as.list(PC[u, ])
      r$ct <- as.list(CT[u, ])
      r$jm <- jm[u]
      r$pred <- paste(tx$pred, dvl[jm[u]])
      r$predcol <- predcol[u]
    }
    if (plotbox) {
      r$q1 <- q1[u]; r$q3 <- q3[u]; r$med <- med[u]; r$lo <- wlo[u]; r$hi <- whi[u]
    }
    if (ismaid) {
      r$mean <- stc[u]; r$dev <- dev[u]; r$sd <- sdv[u]
    }
    if (isdots) {                                              # the points: values and position inside the band (-1 to 1)
      r$dv <- as.list(DT[[u]]$v)
      r$dj <- as.list(round(DT[[u]]$j, 3))
    }
    if (classic) {                                             # classic: "n = 1,234" in the box; under the terminal nodes of maid, its mean and SD
      r$ncl <- paste0("n = ", format(nsz[u], big.mark = ",", trim = TRUE, scientific = FALSE))
      if (ismaid) {
        r$mtxt <- paste0(tx$mean, " = ", fmtn(stc[u], lang))
        r$stxt <- paste0(tx$sd, " = ", fmtn(sdv[u], lang))
        if (!is.null(quartiles)) { r$q1 <- q1[u]; r$q3 <- q3[u]; r$med <- med[u] }
      }
    }
    recs[[u]] <- r
  }
  recs <- recs[order(num)]
  # ---- the options of the page
  whtxt <- switch(whisker, minmax = tx$minmax, tukey = tx$iqr, "P5-P95")
  opt <- list(kind = if (ismaid) "maid" else "mchaid", nodeplot = if (isdots) "dots" else nodeplot, depvar = x$depvar, language = lang, D = D, k = k, pct = isTRUE(pct),
              center = center, whisker = whisker, whiskertxt = whtxt, dvordered = !ismaid && isTRUE(x$dvordered),
              collapse = collapse, nodehead = nodehead)
  if (plotbox) {
    opt$scale <- list(vmin = vmin, vmax = vmax, ref = refm, ref2 = if (ismaid) ref else NULL)
    if (ismaid) {
      opt$scale$ticks <- list(list(v = vmin + pad, t = fmtn(vmin + pad, lang)), list(v = refm, t = fmtn(refm, lang)), list(v = vmax - pad, t = fmtn(vmax - pad, lang)))
      opt$scale$reftxt <- paste0(tx$overallmed, " = ", fmtn(refm, lang))
      opt$scale$reftxt2 <- paste0(tx$overall, " = ", fmtn(ref, lang))
      opt$scale$showref2 <- isboth && center == "mean"
    } else {
      opt$scale$cats <- as.list(dvl)
      opt$scale$reftxt <- paste0(tx$overallmed, " = ", dvl[min(max(ceiling(refm), 1L), nlev)])
    }
  }
  if (isdots) opt$dotstxt <- paste0(tx$dots, if (any(vapply(DT, function(d) d$sampled, TRUE))) paste0(" (", tx$dotsamp, ")") else "")
  if (ismaid && isbar) opt$scale <- list(reftxt = paste0(if (center == "mean") tx$overall else tx$overallmed, " = ", fmtn(ref, lang)))
  if (ismaid) opt$colors <- list(above = mc[1L], below = mc[2L], above5 = cb[5L], below5 = co[5L], shade = lapply(seq_len(5), function(s) list(a = cb[s], b = co[s])))
  else {
    opt$colors <- list(above = cb[5L], below = co[5L], above5 = cb[5L], below5 = co[5L])
    opt$levels <- lapply(seq_len(nlev), function(j) list(name = dvl[j], color = colv[j], text = .texton(colv[j])))
    opt$legend <- paste(x$depvar, tx$legend)
  }
  if (.has_forced(x)) opt$forcednote <- paste0(fmk, " ", .forced_note(lang))               # the page writes it with the note
  if (classic) {
    opt$treestyle <- "classic"
    opt$showp <- showp
    opt$quartiles <- quartiles
    opt$foot <- .classic_foot(x, tx)
    if (!ismaid) {
      opt$nquart <- if (isTRUE(x$dvordered) && !is.null(quartiles) && nlev >= 5L) 3L else 1L       # lines of the median and the quartiles on the bars
    } else {
      ndm <- x$nodes
      gm0 <- ref
      maxdev <- max(abs(ndm[, "mean"] - gm0))
      if (!is.null(quartiles)) maxdev <- max(maxdev, abs(ndm[, c("p25", "p50", "p75")] - gm0))
      if (maxdev == 0 || !is.finite(maxdev)) maxdev <- 1
      opt$maxdev <- maxdev
      opt$gm <- gm0
      opt$nquart <- 3L
    }
  }
  list(options = opt, text = tx, nodes = recs)
}

# the two lines at the foot of the classic graph: the risk estimate with Cramer's V (Eta squared in maid) and the note of the bars
.classic_foot <- function(x, tx) {
  if (inherits(x, "maid")) {
    r <- .results_maid(x)
    left <- paste0(tx$risk, " = ", format(r$risk, digits = 4), " (", tx$root, ": ", format(r$risk0, digits = 4), ")")
    cv <- r$r2
  } else {
    r <- .results(x)
    left <- paste0(tx$risk, " = ", formatC(r$risk, format = "f", digits = 3), " (", tx$root, ": ", formatC(r$risk_root, format = "f", digits = 3), ")")
    cv <- abs(r$CramersV)
  }
  if (is.finite(cv)) left <- paste0(left, "  \u00b7  ", tx$cramer, " = ", formatC(cv, format = "f", digits = 3))
  list(left = left, note = tx$note)
}
