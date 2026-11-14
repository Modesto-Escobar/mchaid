# Graph of a MAID tree (port of maid_graph, maid_tree and maid_lang of maid.ado): the same tree as in mchaid, with
# a bar in each terminal node that shows the deviation of its mean from the overall mean

.lang_maid <- function(code = "en") {
  code <- tolower(trimws(code))
  if (code == "po") code <- "pt"
  L <- list(
    en = c(title = "MAID tree: ", node = "Node @", risk = "Risk estimate", root = "root node", cramer = "Eta\u00b2",
           note = "Bars: mean of each node compared with the overall mean (dashed line)", mean = "mean", sd = "sd",
           overall = "Overall mean", missing = "missing", median = "Median"),
    es = c(title = "\u00c1rbol MAID: ", node = "Nodo @", risk = "Estimaci\u00f3n del riesgo", root = "nodo ra\u00edz", cramer = "Eta\u00b2",
           note = "Barras: media de cada nodo respecto a la media global (l\u00ednea discontinua)", mean = "media", sd = "dt",
           overall = "Media global", missing = "perdido", median = "Mediana"),
    fr = c(title = "Arbre MAID : ", node = "N\u0153ud @", risk = "Estimation du risque", root = "n\u0153ud racine", cramer = "Eta\u00b2",
           note = "Barres : moyenne de chaque n\u0153ud par rapport \u00e0 la moyenne globale (ligne en pointill\u00e9s)", mean = "moyenne", sd = "\u00e9.t.",
           overall = "Moyenne globale", missing = "manquant", median = "M\u00e9diane"),
    de = c(title = "MAID-Baum: ", node = "Knoten @", risk = "Risikosch\u00e4tzung", root = "Wurzelknoten", cramer = "Eta\u00b2",
           note = "Balken: Mittelwert jedes Knotens im Vergleich zum Gesamtmittelwert (gestrichelte Linie)", mean = "Mittelwert", sd = "SD",
           overall = "Gesamtmittelwert", missing = "fehlend", median = "Median"),
    it = c(title = "Albero MAID: ", node = "Nodo @", risk = "Stima del rischio", root = "nodo radice", cramer = "Eta\u00b2",
           note = "Barre: media di ogni nodo rispetto alla media complessiva (linea tratteggiata)", mean = "media", sd = "ds",
           overall = "Media complessiva", missing = "mancante", median = "Mediana"),
    pt = c(title = "\u00c1rvore MAID: ", node = "N\u00f3 @", risk = "Estimativa de risco", root = "n\u00f3 raiz", cramer = "Eta\u00b2",
           note = "Barras: m\u00e9dia de cada n\u00f3 em rela\u00e7\u00e3o \u00e0 m\u00e9dia global (linha tracejada)", mean = "m\u00e9dia", sd = "dp",
           overall = "M\u00e9dia global", missing = "ausente", median = "Mediana"),
    ca = c(title = "Arbre MAID: ", node = "Node @", risk = "Estimaci\u00f3 del risc", root = "node arrel", cramer = "Eta\u00b2",
           note = "Barres: mitjana de cada node respecte a la mitjana global (l\u00ednia discont\u00ednua)", mean = "mitjana", sd = "dt",
           overall = "Mitjana global", missing = "perdut", median = "Mediana"),
    gl = c(title = "\u00c1rbore MAID: ", node = "Nodo @", risk = "Estimaci\u00f3n do risco", root = "nodo ra\u00edz", cramer = "Eta\u00b2",
           note = "Barras: media de cada nodo respecto da media global (li\u00f1a discontinua)", mean = "media", sd = "dt",
           overall = "Media global", missing = "perdido", median = "Mediana"),
    nl = c(title = "MAID-boom: ", node = "Knoop @", risk = "Risicoschatting", root = "wortelknoop", cramer = "Eta\u00b2",
           note = "Balken: gemiddelde van elke knoop ten opzichte van het totaalgemiddelde (stippellijn)", mean = "gemiddelde", sd = "sd",
           overall = "Totaalgemiddelde", missing = "ontbrekend", median = "Mediaan"),
    eu = c(title = "MAID zuhaitza: ", node = "@. nodoa", risk = "Arriskuaren estimazioa", root = "erro-nodoa", cramer = "Eta\u00b2",
           note = "Barrak: nodo bakoitzaren batezbestekoa batezbesteko orokorrarekin alderatuta (marra etena)", mean = "batezbestekoa", sd = "ds",
           overall = "Batezbesteko orokorra", missing = "galdua", median = "Mediana"))
  if (!(code %in% names(L))) stop("language must be one of: ", paste(names(L), collapse = " "), " (or po)")
  as.list(L[[code]])
}

plot.maid <- function(x, main = NULL, pval = TRUE, pcut = NULL, language = "en", quartiles = NULL,
                      treestyle = c("classic", "boxes"), nodeplot = NULL, center = NULL, whisker = c("p5", "minmax", "tukey"),
                      shade = TRUE, col = NULL, cex = 1, nodehead = c("category", "number"), rootlabel = "total", digits = 1, maxdots = 200, ...) {
  treestyle <- .match_opt(treestyle, c("classic", "boxes"), "treestyle")
  digits <- .check_digits(digits)
  if (treestyle == "classic") {                               # arguments of the style of boxes that the classic style ignores
    ign <- c(if (!is.null(nodeplot)) "nodeplot", if (!is.null(center)) "center", if (!missing(nodehead)) "nodehead",
             if (!missing(whisker)) "whisker", if (!missing(maxdots)) "maxdots")
    if (length(ign)) warning(paste(sQuote(ign, FALSE), collapse = ", "), if (length(ign) > 1L) " are" else " is", " ignored with treestyle = \"classic\": use treestyle = \"boxes\"", call. = FALSE)
  }
  whisker <- .match_opt(whisker, c("p5", "minmax", "tukey"), "whisker")
  mc <- .maid_col(col)
  if (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex < 0.5 || cex > 3) stop("'cex' must be a number between 0.5 and 3")
  k <- x$N_clusters
  if (k == 1L) {
    message("No splits uncovered: there is no tree to draw.")
    return(invisible(NULL))
  }
  if (treestyle == "boxes") {
    if (!is.null(pcut) && (!is.numeric(pcut) || pcut <= 0 || pcut >= 1)) stop("'pcut' must be a number between 0 and 1")
    return(.plot_boxes(x, "maid", main = main, pval = pval, pcut = pcut, language = language, nodeplot = nodeplot, center = center,
                       whisker = whisker, shade = shade, col = mc, cex = cex, nodehead = nodehead, rootlabel = rootlabel, digits = digits, maxdots = maxdots))
  }
  if (!is.null(pcut) && (!is.numeric(pcut) || pcut <= 0 || pcut >= 1)) stop("'pcut' must be a number between 0 and 1")
  if (isTRUE(quartiles)) quartiles <- "auto"
  if (isFALSE(quartiles)) quartiles <- NULL
  if (!is.null(quartiles) && !(quartiles %in% c("auto", "all"))) stop("'quartiles' must be \"auto\" or \"all\"")
  TR <- .TREE
  tx <- .lang_maid(language)
  r <- .results_maid(x)
  nd <- x$nodes
  gm <- r$mean
  quar <- !is.null(quartiles)
  maxdev <- max(abs(nd[, "mean"] - gm))
  if (quar) maxdev <- max(maxdev, abs(nd[, c("p25", "p50", "p75")] - gm))
  if (maxdev == 0 || !is.finite(maxdev)) maxdev <- 1
  D <- max(lengths(x$paths))
  fl <- TR$fl
  xs <- max(6, k * TR$dx + 0.8)
  lefttxt <- paste0(tx$risk, " = ", format(r$risk, digits = 4), " (", tx$root, ": ", format(r$risk0, digits = 4), ")")
  if (is.finite(r$r2)) lefttxt <- paste0(lefttxt, "    ", tx$cramer, " = ", formatC(r$r2, format = "f", digits = 3))
  note <- tx$note
  if (.has_forced(x)) note <- paste0(note, "; * ", .forced_note(language))
  wl <- .tw(0.12, nchar(lefttxt))
  xs <- max(xs, wl + .tw(0.12, nchar(note)) + 0.9)
  gtxt <- paste0(tx$overall, " = ", .fmtd(gm, digits))
  wmed <- .tw(fl, nchar(tx$median))
  wq <- .tw(fl, 6)
  legw <- 0.3 + 0.07 + .tw(fl, nchar(gtxt))
  if (quar) legw <- legw + 2 * (0.3 + 0.3 + 0.07) + wmed + wq
  xs <- max(xs, legw + 0.6)
  labh <- 0.42
  ys <- TR$topm + TR$hbox + D * .tree_dy(pval) + TR$gapbar + TR$barh + labh + TR$botm
  lang <- tolower(trimws(language))
  if (lang == "po") lang <- "pt"
  g <- .tree_geometry(x, xs, ys, pval, pcut, tx$missing, tx$node, newfmt = TRUE, rootlab = .root_head(rootlabel, lang, x$depvar))
  texts <- g$texts
  barh <- TR$barh
  fp <- TR$fp
  blue <- mc[1L]
  orange <- mc[2L]
  green <- grDevices::rgb(0, 158, 115, maxColorValue = 255)
  B <- NULL
  M <- NULL
  ybase <- NA_real_
  for (lf in g$leaves) {
    cl <- lf$cl
    nx <- lf$nx
    ybb <- lf$ybt - barh
    ybase <- ybb + barh / 2
    dev <- (nd[cl, "mean"] - gm) / maxdev * barh / 2
    B <- rbind(B, c(nx, min(ybase, ybase + dev), max(ybase, ybase + dev), dev >= 0))
    texts[[length(texts) + 1L]] <- list(x = nx, y = ybb - 0.2, lab = c(paste0(tx$mean, " = ", .fmtd(nd[cl, "mean"], digits)),
                                                                        paste0(tx$sd, " = ", .fmtd(nd[cl, "sd"], digits))),
                                        h = fp, adj = 0.5, box = FALSE)
    if (quar) {
      yq <- ybase + (nd[cl, c("p50", "p25", "p75")] - gm) / maxdev * barh / 2
      nq <- 3L
      if (quartiles != "all" && !is.na(yq[1L])) if (any(abs(yq[2:3] - yq[1L]) < 0.07, na.rm = TRUE)) nq <- 1L
      yd <- numeric(0)
      for (jq in seq_len(nq)) {
        if (is.na(yq[jq]) || any(abs(yd - yq[jq]) < 1e-6)) next
        yd <- c(yd, yq[jq])
        M <- rbind(M, c(yq[jq], nx - 0.5, yq[jq], nx + 0.5, if (jq == 1L) 2 else 3))
      }
    }
  }
  M <- rbind(M, c(ybase, min(g$xl) - TR$dx / 2, ybase, max(g$xl) + TR$dx / 2, 1))      # the overall mean, across the bars
  # legend: the dashed line is the overall mean
  yleg <- 0.55
  xleg <- (xs - legw) / 2
  Lm <- rbind(c(xleg, yleg, xleg + 0.3, yleg, 1))
  texts[[length(texts) + 1L]] <- list(x = xleg + 0.3 + 0.07 + .tw(fl, nchar(gtxt)) / 2, y = yleg, lab = gtxt, h = fl, adj = 0.5, box = FALSE)
  xleg <- xleg + 0.3 + 0.07 + .tw(fl, nchar(gtxt)) + 0.3
  if (quar) {
    Lm <- rbind(Lm, c(xleg, yleg, xleg + 0.3, yleg, 2))
    texts[[length(texts) + 1L]] <- list(x = xleg + 0.3 + 0.07 + wmed / 2, y = yleg, lab = tx$median, h = fl, adj = 0.5, box = FALSE)
    xleg <- xleg + 0.3 + 0.07 + wmed + 0.3
    Lm <- rbind(Lm, c(xleg, yleg, xleg + 0.3, yleg, 3))
    texts[[length(texts) + 1L]] <- list(x = xleg + 0.3 + 0.07 + wq / 2, y = yleg, lab = "Q1, Q3", h = fl, adj = 0.5, box = FALSE)
  }
  if (is.null(main)) main <- paste0(tx$title, x$depvar)
  texts[[length(texts) + 1L]] <- list(x = xs / 2, y = ys - 0.33, lab = main, h = TR$ft, adj = 0.5, box = FALSE)
  texts[[length(texts) + 1L]] <- list(x = 0.15, y = 0.17, lab = lefttxt, h = TR$fz, adj = 0, box = FALSE)
  texts[[length(texts) + 1L]] <- list(x = (wl + 0.3 + xs) / 2, y = 0.17, lab = note, h = TR$fz, adj = 0.5, box = FALSE)

  # ---- draw
  cv_ <- .tree_canvas(xs, ys, cex = cex)
  on.exit(graphics::par(cv_$op))
  lw <- cv_$lw
  .tree_draw_base(g, lw)
  graphics::rect(B[, 1] - 0.46, B[, 2], B[, 1] + 0.46, B[, 3], col = ifelse(B[, 4] == 1, blue, orange), border = "white", lwd = lw(0.5))
  quartiles_all <- identical(quartiles, "all")
  for (i in seq_len(nrow(M))) {
    t <- M[i, 5]
    graphics::segments(M[i, 2], M[i, 1], M[i, 4], M[i, 3], col = if (t == 1) "grey30" else green,
                       lwd = lw(if (t == 1) 0.9 else if (t == 2) 2.2 else if (quartiles_all) 1 else 1.8),
                       lty = if (t == 2) 1 else if (t == 1) 2 else "44")
  }
  for (i in seq_len(nrow(Lm))) {
    t <- Lm[i, 5]
    graphics::segments(Lm[i, 1], Lm[i, 2], Lm[i, 3], Lm[i, 4], col = if (t == 1) "grey30" else green,
                       lwd = lw(if (t == 1) 0.9 else 2), lty = if (t == 2) 1 else if (t == 1) 2 else "44")
  }
  .tree_draw_texts(texts, cv_$cexh)
  invisible(c(width = xs, height = ys))
}
