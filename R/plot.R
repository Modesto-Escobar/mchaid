# Graph of the tree (port of mchaid_graph, mchaid_tree and mchaid_lang of mchaid.ado) with base graphics
#
# The layout is computed in inches, as in the Stata command: boxes for the splits, the categories on the
# branches, and the final nodes at the bottom with a stacked bar with the distribution of the response.

# texts of the graph in several languages (accents as escapes so that the source is ASCII)
.lang <- function(code = "en") {
  code <- tolower(trimws(code))
  if (code == "po") code <- "pt"
  L <- list(
    en = c(title = "CHAID tree: ", node = "Node @", risk = "Risk estimate", root = "root node", cramer = "Cram\u00e9r's V",
           note = "Bars: distribution (%) of the response variable within each node; * = modal category", missing = "missing", median = "Median"),
    es = c(title = "\u00c1rbol CHAID: ", node = "Nodo @", risk = "Estimaci\u00f3n del riesgo", root = "nodo ra\u00edz", cramer = "V de Cram\u00e9r",
           note = "Barras: distribuci\u00f3n (%) de la variable respuesta en cada nodo; * = categor\u00eda modal", missing = "perdido", median = "Mediana"),
    fr = c(title = "Arbre CHAID : ", node = "N\u0153ud @", risk = "Estimation du risque", root = "n\u0153ud racine", cramer = "V de Cram\u00e9r",
           note = "Barres : distribution (%) de la variable r\u00e9ponse dans chaque n\u0153ud ; * = cat\u00e9gorie modale", missing = "manquant", median = "M\u00e9diane"),
    de = c(title = "CHAID-Baum: ", node = "Knoten @", risk = "Risikosch\u00e4tzung", root = "Wurzelknoten", cramer = "Cram\u00e9rs V",
           note = "Balken: Verteilung (%) der Zielvariable in jedem Knoten; * = h\u00e4ufigste Kategorie", missing = "fehlend", median = "Median"),
    it = c(title = "Albero CHAID: ", node = "Nodo @", risk = "Stima del rischio", root = "nodo radice", cramer = "V di Cram\u00e9r",
           note = "Barre: distribuzione (%) della variabile risposta in ogni nodo; * = categoria modale", missing = "mancante", median = "Mediana"),
    pt = c(title = "\u00c1rvore CHAID: ", node = "N\u00f3 @", risk = "Estimativa de risco", root = "n\u00f3 raiz", cramer = "V de Cram\u00e9r",
           note = "Barras: distribui\u00e7\u00e3o (%) da vari\u00e1vel resposta em cada n\u00f3; * = categoria modal", missing = "ausente", median = "Mediana"),
    ca = c(title = "Arbre CHAID: ", node = "Node @", risk = "Estimaci\u00f3 del risc", root = "node arrel", cramer = "V de Cram\u00e9r",
           note = "Barres: distribuci\u00f3 (%) de la variable resposta a cada node; * = categoria modal", missing = "perdut", median = "Mediana"),
    gl = c(title = "\u00c1rbore CHAID: ", node = "Nodo @", risk = "Estimaci\u00f3n do risco", root = "nodo ra\u00edz", cramer = "V de Cram\u00e9r",
           note = "Barras: distribuci\u00f3n (%) da variable resposta en cada nodo; * = categor\u00eda modal", missing = "perdido", median = "Mediana"),
    nl = c(title = "CHAID-boom: ", node = "Knoop @", risk = "Risicoschatting", root = "wortelknoop", cramer = "Cram\u00e9rs V",
           note = "Balken: verdeling (%) van de responsvariabele in elke knoop; * = modale categorie", missing = "ontbrekend", median = "Mediaan"),
    eu = c(title = "CHAID zuhaitza: ", node = "@. nodoa", risk = "Arriskuaren estimazioa", root = "erro-nodoa", cramer = "Cram\u00e9r-en V",
           note = "Barrak: erantzun-aldagaiaren banaketa (%) nodo bakoitzean; * = kategoria modala", missing = "galdua", median = "Mediana"))
  if (!(code %in% names(L))) stop("language must be one of: ", paste(names(L), collapse = " "), " (or po)")
  as.list(L[[code]])
}

# text in lines of at most 'width' characters
.wrap <- function(txt, width) {
  words <- strsplit(gsub("\"", "'", txt), " +")[[1]]
  words <- words[nzchar(words)]
  out <- character(0)
  cur <- ""
  for (w in words) {
    if (!nzchar(cur)) cur <- w
    else if (nchar(cur) + 1L + nchar(w) > width) {
      out <- c(out, cur)
      cur <- w
    } else cur <- paste(cur, w)
  }
  if (nzchar(cur)) out <- c(out, cur)
  if (!length(out)) "" else out
}

.tw <- function(h, nchar) 0.52 * h * nchar

# lines of the 'note' of a graph (a text for the bottom left; "\n" starts a new line): character(0) if there is none
.user_note <- function(note) {
  if (is.null(note)) return(character(0))
  if (!is.character(note) || anyNA(note)) stop("'note' must be a character string", call. = FALSE)
  note <- paste(note, collapse = "\n")
  if (!nzchar(trimws(note))) character(0) else strsplit(note, "\n", fixed = TRUE)[[1L]]
}

# room (inches) that the classic graph needs under the legend for the n lines of the note
.note_room <- function(n) if (n) 0.04 + 0.16 * n else 0

# plot(x, dir = ...): once the graph is drawn, the interactive page of the same tree. The graph has already checked the arguments and
# given the warnings, so those of tree_html() would be the same ones again: they are muted. The path is written (and the page is
# opened in the browser in an interactive session) as print() of the page does; the path of the page goes in the attribute "html" of the result.
.plot_page <- function(size, x, dir, ...) {
  if (is.null(dir)) return(invisible(size))
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) stop("'dir' must be the name of a folder", call. = FALSE)
  page <- suppressWarnings(tree_html(x, ..., dir = dir))
  print(page)
  attr(size, "html") <- page$file
  invisible(size)
}

# dimensions of the layout of a tree, in inches (as in the Stata commands)
.TREE <- list(dx = 1.15, hbox = 0.5, barh = 1.1, gapbar = 0.12, topm = 0.65, botm = 0.85,
              fn = 0.17, fnn = 0.125, fe = 0.14, fp = 0.125, ft = 0.28, fz = 0.12, fl = 0.15)

.tree_dy <- function(showp) if (showp) 1.6 else 1.45

# colours of the categories of the response: by default sequential (light to dark) if it is ordered, colour-blind safe
# otherwise; 'palette' chooses another one ("okabe", "viridis", "grey"), all of them light to dark if they are sequential.
# The orange and the purple of the colour-blind safe palette are lighter than those of Okabe and Ito (same hue) so that
# black texts stand out on them.
.dv_palette <- function(nlev, ordered, palette = "default") {
  if (palette == "default") palette <- if (ordered) "blues" else "okabe"
  switch(palette,
    blues = vapply(seq_len(nlev), function(j) {
      t <- (j - 1) / max(nlev - 1, 1)
      grDevices::rgb(round(222 + (8 - 222) * t), round(235 + (48 - 235) * t), round(247 + (107 - 247) * t), maxColorValue = 255)
    }, ""),
    okabe = c("#0072B2", "#FFC033", "#009E73", "#DCA3C2", "#56B4E9", "#D55E00", "#F0E442", "#646464")[(seq_len(nlev) - 1L) %% 8L + 1L],
    viridis = grDevices::hcl.colors(nlev, "viridis"),
    grey = grDevices::gray(seq(0.9, 0.3, length.out = max(nlev, 2L))[seq_len(nlev)]),
    .other_palette(nlev, palette))
}

# any other palette: a name of grDevices::palette.pals() (discrete, e.g. "Set 2", "Okabe-Ito") or of grDevices::hcl.pals()
# (e.g. "Dark 3", "Blues 3", "Zissou 1"); case does not matter
.other_palette <- function(nlev, palette) {
  if (!is.character(palette) || length(palette) != 1L || is.na(palette)) stop("'palette' must be a single name")
  pp <- if (getRversion() >= "4.0.0") grDevices::palette.pals() else character(0)     # palette.colors() exists since R 4.0.0
  i <- match(tolower(palette), tolower(pp))
  if (!is.na(i)) {
    cols <- tryCatch(unname(grDevices::palette.colors(nlev, pp[i])), error = function(e) NULL, warning = function(w) NULL)
    if (is.null(cols) || anyNA(cols)) stop("palette \"", pp[i], "\" has fewer than ", nlev, " colours: choose another palette or give 'col'")
    return(cols)
  }
  hp <- grDevices::hcl.pals()
  i <- match(tolower(palette), tolower(hp))
  if (!is.na(i)) return(grDevices::hcl.colors(nlev, hp[i]))
  stop("unknown palette \"", palette, "\": use \"default\", \"okabe\", \"viridis\", \"grey\", \"blues\" or a name of grDevices::palette.pals() or grDevices::hcl.pals()")
}

# colours of the categories of an mchaid object: resolves 'col' / 'palette' (it is idempotent: a complete vector goes through)
.resp_col <- function(x, col, palette = "default", language = "en", ramp = FALSE) {
  lev <- colnames(x$freq)
  alt <- lev
  if (isTRUE(x$missing) && !isTRUE(x$dvordered) && lev[length(lev)] == "missing") alt[length(lev)] <- .lang(language)$missing
  .cat_col(col, lev, isTRUE(x$dvordered), palette, x$depvar, alt, ramp)
}

# colours of the categories of the response, in the way of netCoin (see setAttrByValueKey): 'col' is
#  - NULL: the palette;
#  - a named vector (category = colour), in any order; the categories without colour take that of the palette (with a warning)
#    and the names that are not categories are ignored (with a warning);
#  - a list with a single named vector, list(depvar = c(category = colour)), as the 'color' of netCoin;
#  - a vector without names: the colours of the categories in their order (at least as many colours as categories);
#  - with ramp = TRUE (or "reverse"), one single colour: its shades, one for each category, from light to dark (reverse: dark to light).
# 'lev' are the names of the categories as stored (colnames(x$freq)), 'alt' the same translated (the missing category)
.cat_col <- function(col, lev, ordered, palette = "default", depvar = NULL, alt = lev, ramp = FALSE) {
  nlev <- length(lev)
  pal <- function() .dv_palette(nlev, ordered, palette)
  if (!(isTRUE(ramp) || isFALSE(ramp) || identical(ramp, "reverse"))) stop("'ramp' must be TRUE, FALSE or \"reverse\"")
  if (is.null(col)) {
    if (!isFALSE(ramp)) stop("'ramp' needs one colour in 'col', as in col = \"forestgreen\", ramp = TRUE")
    return(pal())
  }
  if (is.list(col) && !is.data.frame(col)) {
    if (length(col) != 1L) stop("'col' as a list must have a single element: list(", if (is.null(depvar)) "variable" else depvar, " = c(category = colour, ...))")
    if (!is.null(depvar) && !is.null(names(col)) && names(col) != depvar)
      warning("'col': the name of the list (\"", names(col), "\") is not the response variable (\"", depvar, "\")", call. = FALSE)
    col <- col[[1L]]
  }
  if (!is.atomic(col) || length(col) == 0L || is.factor(col)) stop("'col' must be a vector of colours (optionally named by category)")
  bad <- !is.na(col) & vapply(as.character(col), function(z) inherits(try(grDevices::col2rgb(z), silent = TRUE), "try-error"), NA)
  if (any(bad)) stop("'col' has colours that R does not recognise: ", paste(dQuote(as.character(col)[bad], FALSE), collapse = ", "))
  nm <- names(col)
  if (!isFALSE(ramp)) {                                  # one shade of a single colour for each category: the colour mixed with white
    if (length(col) != 1L || !(is.null(nm) || all(nm == ""))) stop("'ramp' needs one single colour in 'col', as in col = \"forestgreen\", ramp = TRUE")
    aa <- if (nlev == 1L) 1 else 0.2 + 0.8 * (seq_len(nlev) - 1) / (nlev - 1)         # from 20% to 100% of the colour, light to dark
    if (identical(ramp, "reverse")) aa <- rev(aa)
    return(vapply(aa, function(a) .mix(as.character(col), a), ""))
  }
  if (is.null(nm) || all(nm == "")) {
    if (length(col) < nlev) stop("'col' has ", length(col), " colour", if (length(col) > 1L) "s", " for ", nlev, " categories (", paste(lev, collapse = ", "),
                                 "): give one for each, name them by category or use 'palette'")
    if (length(col) > nlev) warning("'col' has more colours (", length(col), ") than categories (", nlev, "): the last ones are ignored", call. = FALSE)
    out <- as.character(col)[seq_len(nlev)]
    if (anyNA(out)) stop("'col' has missing values")
    return(out)
  }
  if (anyNA(nm) || any(nm == "") || anyDuplicated(nm)) stop("'col': if it has names, all the colours must have a different one")
  out <- rep(NA_character_, nlev)
  for (i in seq_along(col)) {
    j <- match(nm[i], lev)
    if (is.na(j)) j <- match(nm[i], alt)
    if (is.na(j)) j <- match(tolower(nm[i]), tolower(lev))
    if (is.na(j)) next
    if (!is.na(out[j])) stop("'col': the category \"", lev[j], "\" has two colours")
    out[j] <- as.character(col)[i]
  }
  used <- vapply(nm, function(z) !is.na(match(z, lev)) || !is.na(match(z, alt)) || !is.na(match(tolower(z), tolower(lev))), NA)
  if (any(!used)) warning("'col': names that are not categories of the response are ignored: ", paste(dQuote(nm[!used], FALSE), collapse = ", "),
                          " (categories: ", paste(lev, collapse = ", "), ")", call. = FALSE)
  if (anyNA(out)) {
    warning("'col': no colour for the categor", if (sum(is.na(out)) > 1L) "ies " else "y ", paste(dQuote(lev[is.na(out)], FALSE), collapse = ", "),
            ": taken from the palette", call. = FALSE)
    out[is.na(out)] <- pal()[is.na(out)]
  }
  out
}

# relative luminance of a colour and contrast ratio between two colours (WCAG)
.rellum <- function(col) {
  m <- grDevices::col2rgb(col)[, 1L] / 255
  sum(c(0.2126, 0.7152, 0.0722) * ifelse(m <= 0.03928, m / 12.92, ((m + 0.055) / 1.055)^2.4))
}
.contrast <- function(a, b) {
  l <- sort(c(.rellum(a), .rellum(b)), decreasing = TRUE)
  (l[1L] + 0.05) / (l[2L] + 0.05)
}

# colour (black or white) of the text written on a colour: the one that contrasts more
.texton <- function(col) if (.contrast(col, "black") >= .contrast(col, "white")) "black" else "white"

# the colour darkened (same hue) until its contrast with white is at least 'min', to write a text with it on a white background
.darken <- function(col, min = 3) {
  m <- grDevices::col2rgb(col)[, 1L]
  mk <- function(f) grDevices::rgb(t(round(m * f)), maxColorValue = 255)
  f <- 1
  while (f > 0.2 && .contrast(mk(f), "white") < min) f <- f * 0.95
  mk(f)
}

# the colour mixed with white, with the intensities aa (0 to 1): the shades of the nodes of the box plots
.mix <- function(col, aa) {
  m <- grDevices::col2rgb(col)[, 1L]
  grDevices::rgb(round(255 * (1 - aa) + m[1L] * aa), round(255 * (1 - aa) + m[2L] * aa), round(255 * (1 - aa) + m[3L] * aa), maxColorValue = 255)
}

# colours (above, below the reference) of the trees of maid
.maid_col <- function(col) {
  if (is.null(col)) return(c("#0072B2", "#D55E00"))
  if (is.list(col) && length(col) == 1L) col <- col[[1L]]
  if (!is.atomic(col) || length(col) != 2L) stop("'col' must be two colours: for the nodes above and below the reference")
  if (!is.null(names(col)) && !all(names(col) == "")) {                  # c(above = ..., below = ...) in any order
    j <- match(tolower(names(col)), c("above", "below"))
    if (anyNA(j) || anyDuplicated(j)) stop("'col': if it has names, they must be \"above\" and \"below\"")
    col <- col[order(j)]
  }
  tryCatch(grDevices::col2rgb(col), error = function(e) stop("'col' must be two valid colours"))
  unname(as.character(col))
}

# splits whose variable was chosen by the user (forcevars): translated note of the mark and helpers
.forced_note <- function(code = "en") {
  code <- tolower(trimws(code))
  if (code == "po") code <- "pt"
  L <- c(en = "variable chosen by the user (forcevars)", es = "variable elegida por el usuario (forcevars)",
         fr = "variable choisie par l'utilisateur (forcevars)", de = "vom Benutzer gew\u00e4hlte Variable (forcevars)",
         it = "variabile scelta dall'utente (forcevars)", pt = "vari\u00e1vel escolhida pelo utilizador (forcevars)",
         ca = "variable triada per l'usuari (forcevars)", gl = "variable escollida polo usuario (forcevars)",
         nl = "door de gebruiker gekozen variabele (forcevars)", eu = "erabiltzaileak aukeratutako aldagaia (forcevars)")
  if (!(code %in% names(L))) code <- "en"
  unname(L[code])
}
.has_forced <- function(x) any(vapply(x$paths, function(p) any(vapply(p, function(s) isTRUE(s$forced), NA)), NA))
# mark written after the name of a forced variable (a dagger in the classic graph of mchaid, which already uses * for the modal category)
.fmark <- function(s, mark = "*") if (isTRUE(s$forced)) mark else ""

# Geometry of the tree: boxes of the root and of the splits, branches, categories on the branches, p-values,
# terminal nodes (boxes and their texts). xs, ys: size of the layout; showp: p-values on the graph.
# Returns the segments, the boxes, the texts and, for each terminal node, the number, the position of its box
# and the top of the space for its bar.
# newfmt = TRUE (plot.mchaid): the heading of every box is the category (or group of categories) of the branch that leads to
# the node ('rootlab' in the root) with its size, and the splitting variable of a node with its test, "chi2=29.0; p<0.001", go
# together on a white block over the branch, between the box of the node and the horizontal line from which its children hang.
# 'whites' are the rectangles of these blocks (drawn over the branches and under the texts).
.tree_geometry <- function(x, xs, ys, showp, pcut, miss, nodeword, newfmt = FALSE, rootlab = "Total") {
  TR <- .TREE
  k <- x$N_clusters
  sizes <- x$sizes
  sp <- x$splitp
  sname <- if (identical(x$statname, "F")) "F" else .chi_name()
  preds <- x$predictors
  paths <- x$paths
  b <- lengths(paths)
  D <- max(b)
  dy <- .tree_dy(showp)
  sig <- function(s) paste0(s$v, ":", paste(s$cats, collapse = ","))
  Pk <- matrix("", k, D)
  for (i in seq_len(k)) {
    key <- ""
    for (d in seq_len(b[i])) {
      key <- paste0(key, "|", sig(paths[[i]][[d]]))
      Pk[i, d] <- key
    }
  }
  xl <- TR$dx * 0 + as.numeric(seq_len(k))
  x0 <- (xs - k * TR$dx) / 2
  xl <- x0 + (xl - 0.5) * TR$dx
  nodes <- unique(as.vector(Pk[Pk != ""]))
  nn <- length(nodes)
  depth_of <- function(key) lengths(regmatches(key, gregexpr("|", key, fixed = TRUE)))
  info <- lapply(nodes, function(key) {
    du <- depth_of(key)
    m1 <- which(rowSums(Pk == key) > 0)[1L]
    st <- paths[[m1]][[du]]
    lab <- .levlab(preds[[st$v]], st$cats, miss)
    list(du = du, m1 = m1, lab = lab, lines = .wrap(lab, 20), par = if (du > 1L) Pk[m1, du - 1L] else "")
  })
  nlu <- vapply(info, function(z) min(length(z$lines), 3L), 0)
  paru <- vapply(info, function(z) z$par, "")
  texts <- list()
  addtext <- function(xx, yy, lab, h, adj = 0.5, box = FALSE) {
    texts[[length(texts) + 1L]] <<- list(x = xx, y = yy, lab = lab, h = h, adj = adj, box = box)
  }
  fmtn <- function(v) format(v, big.mark = ",", trim = TRUE)
  segs <- NULL
  boxes <- list()
  pdone <- character(0)
  leaves <- list()
  whites <- list()
  # root
  px <- (min(xl) + max(xl)) / 2
  py <- ys - TR$topm - TR$hbox / 2
  fmk <- if (inherits(x, "mchaid")) "\u2020" else "*"
  svname <- function(st) {                                      # name of the splitting variable (cut at 26 characters) and its mark
    vn <- preds[[st$v]]$name
    if (nchar(vn) > 26L) vn <- paste0(substr(vn, 1L, 24L), "...")
    paste0(vn, .fmark(st, fmk))
  }
  svar <- paste0(preds[[paths[[1]][[1]]$v]]$name, .fmark(paths[[1]][[1]], fmk))
  nline <- paste0("n = ", fmtn(sum(sizes)))
  # newfmt: heading that has to fit in a box of at most 1.05 inches (smaller letter down to 0.125, then two lines, then cut)
  fithead <- function(lab) {
    mw <- 1.05 - 0.14
    for (z in seq(TR$fn, 0.125, by = -0.005)) if (.tw(z, nchar(lab)) <= mw) return(list(lines = lab, h = z))
    ln <- lab
    for (w in seq_len(nchar(lab))) {
      ln <- .wrap(lab, w)
      if (length(ln) <= 2L) break
    }
    for (z in c(0.14, 0.13, 0.12)) if (length(ln) <= 2L && .tw(z, max(nchar(ln))) <= mw) return(list(lines = ln, h = z))
    cutl <- function(t) {
      while (nchar(t) > 1L && .tw(0.12, nchar(t) + 3L) > mw) t <- substr(t, 1L, nchar(t) - 1L)
      paste0(t, "...")
    }
    list(lines = vapply(ln, function(t) if (.tw(0.12, nchar(t)) > mw) cutl(t) else t, "", USE.NAMES = FALSE), h = 0.12)
  }
  # newfmt: box of a node with the heading (category of the branch), its size, and the box as wide as the name of the variable below
  nodebox <- function(nx, ny, hl, nline, svar) {
    fh <- fithead(hl)
    wbox <- max(0.9, .tw(fh$h, max(nchar(fh$lines))) + 0.14, .tw(TR$fnn, nchar(nline)) + 0.2, if (nzchar(svar)) .tw(TR$fn, nchar(svar)) + 0.2)
    boxes[[length(boxes) + 1L]] <<- c(nx - wbox / 2, ny - TR$hbox / 2, nx + wbox / 2, ny + TR$hbox / 2)
    top <- ny + TR$hbox / 2
    if (length(fh$lines) == 1L) {
      addtext(nx, top - 0.175, fh$lines, fh$h)
      addtext(nx, top - 0.355, nline, TR$fnn)
    } else {
      addtext(nx, top - 0.16, fh$lines, fh$h)
      addtext(nx, top - 0.385, nline, TR$fnn)
    }
  }
  # newfmt: block over the branch under the box of a parent: name of the splitting variable and, under it, its test
  bh <- if (showp) 0.3556 else 0.22
  block <- function(pxx, pyy, sv, stxt) {
    top <- pyy - TR$hbox / 2 - 0.0417
    bw <- max(0.9722, .tw(TR$fn, nchar(sv)) + 0.1, .tw(TR$fz, nchar(stxt)) + 0.1)
    whites[[length(whites) + 1L]] <<- c(pxx - bw / 2, top - bh, pxx + bw / 2, top)
    if (showp) {
      addtext(pxx, top - 0.095, sv, TR$fn)
      addtext(pxx, top - 0.265, stxt, TR$fz)
    } else addtext(pxx, top - bh / 2, sv, TR$fn)
  }
  if (newfmt) {
    svar <- svname(paths[[1]][[1]])
    nodebox(px, py, rootlab, nline, svar)
  } else {
    wbox <- max(0.7, .tw(TR$fn, nchar(svar)), .tw(TR$fnn, nchar(nline))) + 0.2
    boxes[[1]] <- c(px - wbox / 2, py - TR$hbox / 2, px + wbox / 2, py + TR$hbox / 2)
    addtext(px, py + TR$hbox / 2 - 0.15, nline, TR$fnn)
    addtext(px, py + TR$hbox / 2 - 0.34, svar, TR$fn)
  }
  for (u in seq_len(nn)) {
    key <- nodes[u]
    z <- info[[u]]
    du <- z$du
    members <- which(rowSums(Pk == key) > 0)
    m1 <- members[1L]
    isleaf <- any(b[members] == du)
    nx <- (min(xl[members]) + max(xl[members])) / 2
    if (isleaf) {
      cl <- members[b[members] == du][1L]
      nx <- xl[cl]
      ny <- ys - TR$topm - TR$hbox / 2 - D * dy
    } else {
      ny <- ys - TR$topm - TR$hbox / 2 - du * dy
    }
    if (du == 1L) {
      pxx <- (min(xl) + max(xl)) / 2
      pyy <- ys - TR$topm - TR$hbox / 2
      parkey <- ""
    } else {
      parkey <- Pk[m1, du - 1L]
      pm <- which(rowSums(Pk == parkey) > 0)
      pxx <- (min(xl[pm]) + max(xl[pm])) / 2
      pyy <- ys - TR$topm - TR$hbox / 2 - (du - 1L) * dy
    }
    gapu <- 0.38
    if (newfmt) gapu <- 0.0417 + bh + 0.0426
    else if (showp) gapu <- max(gapu, 2 * (0.06 + 0.07 * max(nlu[paru == paru[u]])) + 0.18)
    ymid <- pyy - TR$hbox / 2 - gapu
    segs <- rbind(segs, c(pxx, pyy - TR$hbox / 2, pxx, ymid), c(pxx, ymid, nx, ymid), c(nx, ymid, nx, ny + TR$hbox / 2))
    if (newfmt) {
      if (!(parkey %in% pdone)) {
        pdone <- c(pdone, parkey)
        stxt <- if (showp && nrow(sp) >= m1 && ncol(sp) >= du) paste0(.cstat(x$splitstat, m1, du, sname), .ptxt(sp[m1, du], pcut)) else ""
        block(pxx, pyy, svname(paths[[m1]][[du]]), stxt)
      }
      if (isleaf) {
        nline <- paste0("n = ", fmtn(sizes[cl]))
        svn <- ""
      } else {
        nline <- paste0("n = ", fmtn(sum(sizes[members])))
        svn <- svname(paths[[m1]][[du + 1L]])
      }
      nodebox(nx, ny, z$lab, nline, svn)
      if (isleaf) leaves[[length(leaves) + 1L]] <- list(cl = cl, nx = nx, ny = ny, ybt = ny - TR$hbox / 2 - TR$gapbar)
      next
    }
    if (showp && nrow(sp) >= m1 && ncol(sp) >= du && !(parkey %in% pdone)) {
      pdone <- c(pdone, parkey)
      addtext(pxx, (pyy - TR$hbox / 2 + ymid) / 2, paste0(.cstat(x$splitstat, m1, du, sname), .ptxt(sp[m1, du], pcut)), TR$fz, box = TRUE)
    }
    lines <- z$lines
    if (length(lines) > 3L) lines <- c(lines[1:2], paste0(lines[3L], " ..."))
    addtext(nx, ymid, lines, TR$fe, box = TRUE)
    if (isleaf) {
      sv <- sub("@", cl, nodeword, fixed = TRUE)
      nline <- paste0("n = ", fmtn(sizes[cl]))
    } else {
      sv <- paste0(preds[[paths[[m1]][[du + 1L]]$v]]$name, .fmark(paths[[m1]][[du + 1L]], fmk))
      nline <- paste0("n = ", fmtn(sum(sizes[members])))
    }
    wbox <- max(0.7, .tw(TR$fn, nchar(sv)), .tw(TR$fnn, nchar(nline))) + 0.2
    boxes[[length(boxes) + 1L]] <- c(nx - wbox / 2, ny - TR$hbox / 2, nx + wbox / 2, ny + TR$hbox / 2)
    if (isleaf) {
      addtext(nx, ny + TR$hbox / 2 - 0.175, sv, TR$fn)
      addtext(nx, ny + TR$hbox / 2 - 0.355, nline, TR$fnn)
      leaves[[length(leaves) + 1L]] <- list(cl = cl, nx = nx, ny = ny, ybt = ny - TR$hbox / 2 - TR$gapbar)
    } else {
      addtext(nx, ny + TR$hbox / 2 - 0.15, nline, TR$fnn)
      addtext(nx, ny + TR$hbox / 2 - 0.34, sv, TR$fn)
    }
  }
  list(segs = segs, boxes = boxes, whites = whites, texts = texts, leaves = leaves, xl = xl, D = D)
}

# open the device region and return the function that gives the cex of a text height in inches of the layout, and the one
# that gives the width of a line: the line widths are those designed for a drawing that fills a device at a scale of 0.6 or more,
# and shrink with the drawing (down to 0.4 times) so that the boxes of a wide tree do not get heavy borders;
# cex multiplies the texts as they are seen on the device (the layout does not change)
.tree_canvas <- function(xs, ys, new = TRUE, cex = 1) {
  op <- if (new) graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  if (new) graphics::plot.new()
  graphics::plot.window(xlim = c(0, xs), ylim = c(0, ys), asp = 1)
  pin <- graphics::par("pin")
  scale <- min(pin[1L] / xs, pin[2L] / ys)                   # inches of the device for an inch of the layout
  list(op = op, cexh = function(h) h * scale * 72 / graphics::par("ps") * 1.1 * cex,
       lw = function(w) w * max(0.4, min(1, scale / 0.6)))
}

.tree_draw_base <- function(g, lw) {
  if (!is.null(g$segs)) graphics::segments(g$segs[, 1], g$segs[, 2], g$segs[, 3], g$segs[, 4], col = "grey50", lwd = lw(0.8))
  for (wh in g$whites) graphics::rect(wh[1], wh[2], wh[3], wh[4], col = "white", border = NA)       # blocks over the branches
  for (bx in g$boxes) graphics::rect(bx[1], bx[2], bx[3], bx[4], border = "grey50", lwd = lw(0.8))
}

# can the current device write the dagger that marks the forced splits of the classic graph of mchaid? Devices with a
# single-byte encoding (pdf() on Linux and macOS, ISOLatin1) fail with an error, or a warning, for characters they do not have
.dagger_ok <- function() {
  tryCatch({
    w <- graphics::strwidth("\u2020")
    is.finite(w) && w > 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

.tree_draw_texts <- function(texts, cexh) {
  hasdag <- any(vapply(texts, function(t) any(grepl("\u2020", t$lab, fixed = TRUE)), NA))
  if (hasdag && !.dagger_ok()) {                               # the dagger becomes a "+" (the asterisk is already the mark of the modal category)
    texts <- lapply(texts, function(t) {
      t$lab <- gsub("\u2020", "+", t$lab, fixed = TRUE)
      t
    })
  }
  for (t in texts) {
    nl <- length(t$lab)
    h <- cexh(t$h)
    lh <- t$h * 1.12                                           # distance between the lines of a label, in inches of the layout
    yy <- t$y + ((nl - 1) / 2 - (seq_len(nl) - 1)) * lh
    if (t$box) {
      for (i in seq_len(nl)) {
        w <- graphics::strwidth(t$lab[i], cex = h, units = "user")
        hh <- graphics::strheight(t$lab[i], cex = h, units = "user")
        graphics::rect(t$x - w / 2 - 0.04, yy[i] - hh / 2 - 0.035, t$x + w / 2 + 0.04, yy[i] + hh / 2 + 0.035, col = "white", border = NA)
      }
    }
    graphics::text(t$x, yy, t$lab, cex = h, adj = c(t$adj, 0.5))
  }
}

plot.mchaid <- function(x, main = NULL, note = NULL, pct = TRUE, pval = TRUE, pcut = NULL, language = "en",
                        quartiles = NULL, col = NULL, palette = "default", ramp = FALSE, treestyle = c("boxes", "classic"), nodeplot = NULL,
                        whisker = c("p5", "minmax", "tukey"), shade = TRUE, cex = 1, nodehead = c("category", "number"), rootlabel = "total",
                        collapse = NULL, dir = NULL, ...) {
  treestyle <- .match_opt(treestyle, c("boxes", "classic"), "treestyle")
  if (treestyle == "classic") {                               # arguments of the style of boxes that the classic style ignores
    ign <- c(if (!is.null(nodeplot)) "nodeplot", if (!missing(nodehead)) "nodehead", if (!missing(whisker)) "whisker")
    if (length(ign)) warning(paste(sQuote(ign, FALSE), collapse = ", "), if (length(ign) > 1L) " are" else " is", " ignored with treestyle = \"classic\": use treestyle = \"boxes\"", call. = FALSE)
  } else if (!is.null(quartiles) && !isFALSE(quartiles)) warning("'quartiles' is ignored with treestyle = \"boxes\": use treestyle = \"classic\"", call. = FALSE)
  whisker <- .match_opt(whisker, c("p5", "minmax", "tukey"), "whisker")
  unote <- .user_note(note)
  main0 <- main                                               # the classic graph gives the title a default that the page does not take
  col0 <- col
  col <- .resp_col(x, col, palette, language, ramp)               # colours of the categories of the response (checks 'col' and 'palette')
  if (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex < 0.5 || cex > 3) stop("'cex' must be a number between 0.5 and 3")
  k <- x$N_clusters
  if (k == 1L) {
    message("No splits uncovered: there is no tree to draw.")
    return(invisible(NULL))
  }
  page <- function(size) .plot_page(size, x, dir, main = main0, note = note, treestyle = treestyle, nodeplot = nodeplot, whisker = whisker, shade = shade,
                                    pval = pval, pcut = pcut, language = language, col = col0, palette = palette, ramp = ramp, collapse = collapse,
                                    pct = pct, quartiles = quartiles, nodehead = nodehead, rootlabel = rootlabel)
  if (treestyle == "boxes") {
    if (!is.null(pcut) && (!is.numeric(pcut) || pcut <= 0 || pcut >= 1)) stop("'pcut' must be a number between 0 and 1")
    return(page(.plot_boxes(x, "mchaid", main = main, note = note, pval = pval, pcut = pcut, language = language, pct = pct, nodeplot = nodeplot,
                            whisker = whisker, shade = shade, col = col, palette = palette, cex = cex, nodehead = nodehead, rootlabel = rootlabel)))
  }
  if (!is.null(pcut) && (!is.numeric(pcut) || pcut <= 0 || pcut >= 1)) stop("'pcut' must be a number between 0 and 1")
  if (isTRUE(quartiles)) quartiles <- "auto"
  if (isFALSE(quartiles)) quartiles <- NULL
  if (!is.null(quartiles) && !(quartiles %in% c("auto", "all"))) stop("'quartiles' must be \"auto\" or \"all\"")
  TR <- .TREE
  tx <- .lang(language)
  r <- .results(x)
  rp <- r$rowpct
  nlev <- ncol(rp)
  dvl <- colnames(x$freq)
  if (x$missing && !x$dvordered && dvl[nlev] == "missing") dvl[nlev] <- tx$missing
  D <- max(lengths(x$paths))
  xs <- max(6, k * TR$dx + 0.8)
  lefttxt <- paste0(tx$risk, " = ", formatC(r$risk, format = "f", digits = 3), " (", tx$root, ": ", formatC(r$risk_root, format = "f", digits = 3), ")")
  cv <- abs(r$CramersV)
  if (is.finite(cv)) lefttxt <- paste0(lefttxt, "    ", tx$cramer, " = ", formatC(cv, format = "f", digits = 3))
  note <- tx$note
  if (.has_forced(x)) note <- paste0(note, "; \u2020 ", .forced_note(language))
  wl <- .tw(0.12, nchar(lefttxt))
  xs <- max(xs, wl + .tw(0.12, nchar(note)) + 0.9)
  nun <- length(unote)
  enote <- .note_room(nun)                                    # room under the legend for the note of the user
  if (nun) xs <- max(xs, .tw(TR$fz, max(nchar(unote))) + 0.45)
  fl <- TR$fl
  ordr <- x$dvordered && !is.null(quartiles)
  quar <- ordr && nlev >= 5L
  medtxt <- tx$median
  wmed <- .tw(fl, nchar(medtxt))
  wq <- .tw(fl, 6)
  legw <- 0.3 * (nlev - 1)
  for (j in seq_len(nlev)) legw <- legw + 0.16 + 0.07 + .tw(fl, nchar(dvl[j]))
  if (ordr) legw <- legw + 0.3 + 0.3 + 0.07 + wmed
  if (quar) legw <- legw + 0.3 + 0.3 + 0.07 + wq
  xs <- max(xs, legw + 0.6)
  ys <- TR$topm + TR$hbox + D * .tree_dy(pval) + TR$gapbar + TR$barh + TR$botm + enote
  lang <- tolower(trimws(language))
  if (lang == "po") lang <- "pt"
  g <- .tree_geometry(x, xs, ys, pval, pcut, tx$missing, tx$node, newfmt = TRUE, rootlab = .root_head(rootlabel, lang, x$depvar))
  texts <- g$texts
  fp <- TR$fp
  barh <- TR$barh
  B <- NULL
  M <- NULL
  for (lf in g$leaves) {
    cl <- lf$cl
    nx <- lf$nx
    ybb <- lf$ybt - barh
    cum <- 0
    mx <- max(rp[cl, ])
    for (j in seq_len(nlev)) {
      pc <- rp[cl, j]
      B <- rbind(B, c(nx, ybb + barh * cum / 100, ybb + barh * (cum + pc) / 100, j, pc, ybb + barh * (cum + pc / 2) / 100, as.numeric(pc == mx && pc > 0)))
      cum <- cum + pc
    }
    if (ordr) {
      labs <- matrix(numeric(0), 0, 2)
      if (pct) {
        cum <- 0
        for (j in seq_len(nlev)) {
          pc <- rp[cl, j]
          if (((pc >= 12) || (pc == mx && pc > 0)) && pc > 0) {
            labs <- rbind(labs, c(ybb + barh * (cum + pc / 2) / 100, .tw(fp, nchar(format(round(pc))) + as.numeric(pc == mx && pc > 0)) / 2 + 0.05))
          }
          cum <- cum + pc
        }
      }
      lv <- c(50, if (quar) c(25, 75))
      yq <- rep(NA_real_, length(lv))
      for (j in seq_along(lv)) {
        cum <- 0
        for (jq in seq_len(nlev)) {
          pc <- rp[cl, jq]
          if (pc > 0 && cum + pc >= lv[j] - 1e-6) {
            yq[j] <- ybb + barh * ((jq - 1) + min(1, max(0, (lv[j] - cum) / pc))) / nlev
            break
          }
          cum <- cum + pc
        }
      }
      nq <- length(lv)
      if (nq > 1L && !is.na(yq[1L]) && quartiles != "all") if (any(abs(yq[2:nq] - yq[1L]) < 0.07, na.rm = TRUE)) nq <- 1L
      yd <- numeric(0)
      for (j in seq_len(nq)) {
        ym <- yq[j]
        if (is.na(ym) || any(abs(yd - ym) < 1e-6)) next
        yd <- c(yd, ym)
        lt <- as.numeric(j > 1L)
        gp <- 0
        for (rr in seq_len(nrow(labs))) if (abs(labs[rr, 1L] - ym) < 0.09) gp <- max(gp, labs[rr, 2L])
        if (gp > 0) M <- rbind(M, c(ym, nx - 0.5, ym, nx - gp, lt), c(ym, nx + gp, ym, nx + 0.5, lt))
        else M <- rbind(M, c(ym, nx - 0.5, ym, nx + 0.5, lt))
      }
    }
  }
  # legend
  xleg <- (xs - legw) / 2
  yleg <- 0.55 + enote
  Lg <- NULL
  for (j in seq_len(nlev)) {
    Lg <- rbind(Lg, c(xleg + 0.08, yleg - 0.08, yleg + 0.08, j))
    wj <- .tw(fl, nchar(dvl[j]))
    texts[[length(texts) + 1L]] <- list(x = xleg + 0.16 + 0.07 + wj / 2, y = yleg, lab = dvl[j], h = fl, adj = 0.5, box = FALSE)
    xleg <- xleg + 0.16 + 0.07 + wj + 0.3
  }
  Lm <- NULL
  if (ordr) {
    Lm <- rbind(Lm, c(xleg, yleg, xleg + 0.3, yleg, 0))
    texts[[length(texts) + 1L]] <- list(x = xleg + 0.3 + 0.07 + wmed / 2, y = yleg, lab = medtxt, h = fl, adj = 0.5, box = FALSE)
    xleg <- xleg + 0.3 + 0.07 + wmed + 0.3
  }
  if (quar) {
    Lm <- rbind(Lm, c(xleg, yleg, xleg + 0.3, yleg, 1))
    texts[[length(texts) + 1L]] <- list(x = xleg + 0.3 + 0.07 + wq / 2, y = yleg, lab = "Q1, Q3", h = fl, adj = 0.5, box = FALSE)
  }
  if (is.null(main)) main <- paste0(tx$title, x$depvar)
  texts[[length(texts) + 1L]] <- list(x = xs / 2, y = ys - 0.33, lab = main, h = TR$ft, adj = 0.5, box = FALSE)
  texts[[length(texts) + 1L]] <- list(x = 0.15, y = 0.17 + enote, lab = lefttxt, h = TR$fz, adj = 0, box = FALSE)
  texts[[length(texts) + 1L]] <- list(x = (wl + 0.3 + xs) / 2, y = 0.17 + enote, lab = note, h = TR$fz, adj = 0.5, box = FALSE)
  for (i in seq_len(nun)) texts[[length(texts) + 1L]] <- list(x = 0.15, y = 0.12 + (nun - i) * 0.16, lab = unote[i], h = TR$fz, adj = 0, box = FALSE)
  col <- .resp_col(x, col, palette, "en")

  # ---- draw
  cv_ <- .tree_canvas(xs, ys, cex = cex)
  on.exit(graphics::par(cv_$op))
  cexh <- cv_$cexh
  lw <- cv_$lw
  .tree_draw_base(g, lw)
  if (!is.null(B)) graphics::rect(B[, 1] - 0.46, B[, 2], B[, 1] + 0.46, B[, 3], col = col[B[, 4]], border = "white", lwd = lw(0.5))
  if (pct && !is.null(B)) {
    for (i in seq_len(nrow(B))) {
      if (!is.na(B[i, 5]) && ((B[i, 5] >= 12) || B[i, 7] == 1) && B[i, 5] > 0) {
        graphics::text(B[i, 1], B[i, 6], paste0(round(B[i, 5]), if (B[i, 7] == 1) "*" else ""), cex = cexh(fp), col = .texton(col[B[i, 4]]))
      }
    }
  }
  green <- grDevices::rgb(0, 158, 115, maxColorValue = 255)
  if (!is.null(M)) {
    for (i in seq_len(nrow(M))) {
      graphics::segments(M[i, 2], M[i, 1], M[i, 4], M[i, 3], col = green,
                         lwd = lw(if (M[i, 5] == 0) 2.2 else if (quartiles == "all") 1 else 1.8), lty = if (M[i, 5] == 0) 1 else "44")
    }
  }
  graphics::rect(Lg[, 1] - 0.08, Lg[, 2], Lg[, 1] + 0.08, Lg[, 3], col = col[Lg[, 4]], border = "white", lwd = lw(0.5))
  if (!is.null(Lm)) {
    for (i in seq_len(nrow(Lm))) graphics::segments(Lm[i, 1], Lm[i, 2], Lm[i, 3], Lm[i, 4], col = green,
                                                   lwd = lw(if (Lm[i, 5] == 0) 2.2 else 1.8), lty = if (Lm[i, 5] == 0) 1 else "44")
  }
  .tree_draw_texts(texts, cexh)
  page(c(width = xs, height = ys))
}
