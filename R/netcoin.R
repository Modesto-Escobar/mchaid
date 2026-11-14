# The tree as a network: nodes (root, splits and terminal nodes) and links (branches), in the format of
# the netCoin package. The data frames are built with base R; netCoin is only called if asked for.

asnetCoin <- function(x, label = "label", size = "n", labsize = NULL, color = "auto", shape = "type", image = NULL,
                      legend = NULL, group = NULL, border = NULL, lwidth = "n", lweight = NULL, lcolor = NULL, ltext = "label",
                      build = TRUE, node = "Node", digits = 1, col = NULL, palette = "default", ramp = FALSE, ...) {
  ismaid <- inherits(x, "maid")
  if (!ismaid && !inherits(x, "mchaid")) stop("'x' must be an object returned by mchaid() or maid()")
  k <- x$N_clusters
  if (k == 1L) stop("No splits uncovered: there is no tree")
  paths <- x$paths
  preds <- x$predictors
  b <- lengths(paths)
  D <- max(b)
  if (!ismaid) {
    lev <- colnames(x$freq)
    nlev <- length(lev)
  }
  sig <- function(s) paste0(s$v, ":", paste(s$cats, collapse = ","))
  Pk <- matrix("", k, D)
  for (i in seq_len(k)) {
    key <- ""
    for (d in seq_len(b[i])) {
      key <- paste0(key, "|", sig(paths[[i]][[d]]))
      Pk[i, d] <- key
    }
  }
  keys <- c("", unique(as.vector(Pk[Pk != ""])))               # the root first
  depth <- c(0L, lengths(regmatches(keys[-1L], gregexpr("|", keys[-1L], fixed = TRUE))))
  ord <- order(depth)
  keys <- keys[ord]
  depth <- depth[ord]
  nn <- length(keys)
  xl <- as.numeric(seq_len(k))
  members <- lapply(keys, function(key) if (key == "") seq_len(k) else which(rowSums(Pk == key) > 0))
  isleaf <- vapply(seq_len(nn), function(u) keys[u] != "" && any(b[members[[u]]] == depth[u]), NA)
  leafid <- vapply(seq_len(nn), function(u) if (isleaf[u]) members[[u]][b[members[[u]]] == depth[u]][1L] else NA_integer_, 0L)
  names_ <- character(nn)
  ns <- 0L
  for (u in seq_len(nn)) {
    if (isleaf[u]) names_[u] <- paste(node, leafid[u])
    else {
      ns <- ns + 1L
      names_[u] <- if (u == 1L) "root" else paste0("split", ns)
    }
  }
  names_[1L] <- "root"
  parent <- vapply(seq_len(nn), function(u) {
    if (u == 1L) return(NA_character_)
    m1 <- members[[u]][1L]
    if (depth[u] == 1L) "root" else names_[match(Pk[m1, depth[u] - 1L], keys)]
  }, "")
  if (ismaid) {
    ns_ <- x$node[x$rows]                                      # node of the cases of the sample
    st <- t(vapply(seq_len(nn), function(u) {
      sel <- ns_ %in% members[[u]]
      .wsummary(x$y[sel], x$w[sel])
    }, numeric(8)))
    colnames(st) <- c("n", "mean", "sd", "min", "p25", "median", "p75", "max")
    n <- st[, "n"]
    gm <- sum(x$w * x$y) / sum(x$w)
    resp <- data.frame(mean = st[, "mean"], sd = st[, "sd"], min = st[, "min"], p25 = st[, "p25"], median = st[, "median"],
                       p75 = st[, "p75"], max = st[, "max"], dev = st[, "mean"] - gm)
    fmt <- function(v) format(round(v, 3), trim = TRUE)
    body <- function(u) paste0("mean = ", fmt(st[u, "mean"]), "<br>sd = ", fmt(st[u, "sd"]), "<br>median = ", fmt(st[u, "median"]),
                               "<br>min = ", fmt(st[u, "min"]), ", max = ", fmt(st[u, "max"]))
  } else {
    F <- t(vapply(seq_len(nn), function(u) colSums(x$freq[members[[u]], , drop = FALSE]), numeric(nlev)))
    n <- rowSums(F)
    pct <- round(100 * F / n, digits)
    colnames(pct) <- paste0("pct_", make.names(lev))
    modal <- lev[apply(F, 1, which.max)]
    resp <- data.frame(modal = modal, pct, stringsAsFactors = FALSE)
    body <- function(u) paste0(lev, ": ", format(pct[u, ], nsmall = digits, trim = TRUE), "%", collapse = "<br>")
  }
  m1 <- vapply(members, function(m) m[1L], 0L)
  splitvar <- vapply(seq_len(nn), function(u) if (isleaf[u]) NA_character_ else preds[[paths[[m1[u]]][[depth[u] + 1L]]$v]]$name, "")
  splitp <- vapply(seq_len(nn), function(u) if (isleaf[u]) NA_real_ else x$splitp[m1[u], depth[u] + 1L], 0)
  forced <- vapply(seq_len(nn), function(u) !isleaf[u] && isTRUE(paths[[m1[u]]][[depth[u] + 1L]]$forced), NA)      # the variable of the split was chosen with forcevars
  defin <- vapply(seq_len(nn), function(u) .path_text(preds, paths[[m1[u]]][seq_len(depth[u])]), "")
  type <- ifelse(seq_len(nn) == 1L, "root", ifelse(isleaf, "node", "split"))
  nlabel <- ifelse(isleaf, names_, splitvar)
  tip <- vapply(seq_len(nn), function(u) {
    paste0("<b>", if (isleaf[u]) names_[u] else splitvar[u], "</b><br>", defin[u], "<br>n = ", format(n[u], big.mark = ",", trim = TRUE), "<br>",
           body(u),
           if (!isleaf[u]) paste0("<br>split p = ", .pfmt(min(splitp[u], 1))) else "")
  }, "")
  nodes <- data.frame(name = names_, label = nlabel, type = type, depth = depth, n = n,
                      variable = splitvar, p = splitp, forced = forced, definition = defin, resp, ntext = tip, stringsAsFactors = FALSE)
  nodes$node <- ifelse(isleaf, leafid, NA_integer_)
  # links: one for each branch, from the parent to the node
  child <- which(seq_len(nn) > 1L)
  blabel <- vapply(child, function(u) {
    st <- paths[[m1[u]]][[depth[u]]]
    .levlab(preds[[st$v]], st$cats)
  }, "")
  ppar <- match(parent[child], names_)
  links <- data.frame(Source = parent[child], Target = names_[child], label = blabel, n = n[child],
                      p = splitp[ppar], variable = splitvar[ppar], pct_of_parent = round(100 * n[child] / n[ppar], digits),
                      stringsAsFactors = FALSE)
  # layout: terminal nodes in a row at the bottom, the other nodes above the middle of their descendants
  lx <- vapply(seq_len(nn), function(u) if (isleaf[u]) xl[leafid[u]] else (min(xl[members[[u]]]) + max(xl[members[[u]]])) / 2, 0)
  ly <- ifelse(isleaf, -D, -depth)
  layout <- cbind(x = lx, y = ly)
  rownames(layout) <- names_
  out <- list(nodes = nodes, links = links, layout = layout)
  if (!build) return(out)
  if (!requireNamespace("netCoin", quietly = TRUE)) {
    warning("package 'netCoin' is not installed: returning the data frames (nodes, links, layout)")
    return(out)
  }
  if (identical(color, "auto")) color <- if (ismaid) "mean" else "modal"
  if (identical(color, "modal") && !ismaid) color <- list(modal = stats::setNames(.resp_col(x, col, palette, "en", ramp), lev))
  if (identical(shape, "type")) shape <- list(type = c(root = "Triangle", split = "Circle", node = "Square"))
  args <- list(nodes = nodes, links = links, layout = layout, name = "name", label = label, size = size, labelSize = labsize,
               color = color, shape = shape, image = image, legend = legend, group = group, border = border,
               lwidth = lwidth, lweight = lweight, lcolor = lcolor, ltext = ltext, ntext = "ntext", showArrows = TRUE, showCoordinates = TRUE, mode = "network",
               main = paste(if (ismaid) "MAID tree:" else "CHAID tree:", x$depvar))
  args <- args[!vapply(args, is.null, NA)]
  extra <- list(...)
  args[names(extra)] <- extra
  do.call(netCoin::netCoin, args)
}
