# The CHAID algorithm (port of the Mata functions mchaid() and maid() of the Stata commands)
#
# engine   the tests (see engine.R)
# preds    list of splitting variables (see labels.R) with their codes for each case

# weighted counts of the response (length J)
.wtab1 <- function(dv, w, J) {
  m <- numeric(J)
  agg <- rowsum(w, dv)
  m[as.integer(rownames(agg))] <- agg[, 1]
  m
}

# weighted table of the categories of a splitting variable (rows) by the response (columns)
.wtab <- function(code, dv, w, K, J) {
  key <- code + K * (dv - 1L)
  m <- numeric(K * J)
  agg <- rowsum(w, key)
  m[as.integer(rownames(agg))] <- agg[, 1]
  matrix(m, K, J)
}

# statistics of each group of categories (the rows of the groups are added)
.group_table <- function(tab, groups) {
  t(vapply(groups, function(g) colSums(tab[g, , drop = FALSE]), numeric(ncol(tab))))
}

.grow <- function(engine, preds, minnode, minsplit, maxbranch, mergalpha, spltalpha, adj,
                  exhaust, renumber, steps, exact_bonf = FALSE, forced = integer(0)) {
  say <- function(...) if (steps) cat(..., "\n", sep = "")
  fmtn <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
  n <- length(preds[[1L]]$codes)
  P <- length(preds)
  cl <- rep.int(1L, n)
  nclus <- 1L
  paths <- list(list())
  splitp <- list(numeric(0))
  splitstat <- list(numeric(0))                                    # test statistic of each split (chi2, F), as splitp
  cur <- 1L
  nforce <- length(forced)                                        # indices of the variables of the first levels (forcevars)
  fallback <- character(0)                                         # nodes where the forced split was not possible
  tie <- function(x) signif(x, 9)                                  # p-values equal to 9 digits are ties
  while (cur <= nclus) {
    idx <- which(cl == cur)
    st <- engine$node(idx)
    nodeN <- st$N
    nodetxt <- .path_text(preds, paths[[cur]])
    depth <- length(paths[[cur]])
    if (st$pure || nodeN <= minsplit) {                              # pure, or too small to be split
      if (depth == 0L && nforce > 0L) stop("forcevars: the sample cannot be split: it is pure or has no more than minsplit (", minsplit, ") cases", call. = FALSE)
      if (steps) {
        say("")
        say("Node: ", nodetxt, " (n = ", fmtn(nodeN), ")")
        say("  Not split: ", sprintf(engine$msg, as.integer(minsplit)))
      }
      cur <- cur + 1L
      next
    }
    if (steps) {
      say("")
      say("Node: ", nodetxt, " (n = ", fmtn(nodeN), ")")
      say(strrep("-", 78))
    }
    isforced <- depth < nforce                                       # the variable of this level is chosen by the user
    repeat {
    best <- NULL
    cmp_p <- if (isforced) Inf else 1                                 # a forced variable is taken whatever its adjusted p-value
    cmp_aic <- Inf
    trrows <- character(0)
    if (isforced && steps) say("Split forced by forcevars: only ", preds[[forced[depth + 1L]]]$name, " is considered at this level")
    for (v in (if (isforced) forced[depth + 1L] else seq_len(P))) {
      pr <- preds[[v]]
      K <- pr$K
      tab <- engine$table(pr$codes[idx], K)
      groups <- as.list(which(engine$count(tab) > 0))
      if (steps) say("Variable ", pr$name, ": ", length(groups), " categories: ", .catlist(preds, v, groups))
      exh <- exhaust                                                 # a forced split that is not significant is merged down to two groups
      repeat {
      # ---- merging of categories
      repeat {
        g <- length(groups)
        if (g <= 2L) {
          if (steps) say("  Merging step: not needed, ", g, " categories remain")
          break
        }
        gt <- .group_table(tab, groups)
        floating <- pr$type == "o" && pr$float && length(groups[[g]]) == 1L && groups[[g]] == K
        if (pr$type == "u") prs <- utils::combn(g, 2)
        else if (!floating) prs <- rbind(1:(g - 1L), 2:g)
        else prs <- cbind(rbind(seq_len(g - 2L), 2:(g - 1L)), rbind(1:(g - 1L), g))
        res <- vapply(seq_len(ncol(prs)), function(k) engine$pair(gt[prs[1, k], ], gt[prs[2, k], ]), numeric(3))
        ps <- res[1, ]
        aics <- res[2, ]
        if (steps) {
          say("  Merging step: candidate pairs and p-value of their difference (the highest is merged if p > ", 1 - mergalpha, ")")
          for (k in seq_len(ncol(prs))) {
            say("      ", formatC(paste0(.grptext(pr, groups[[prs[1, k]]]), " + ", .grptext(pr, groups[[prs[2, k]]])), width = -46), formatC(.pfmt(ps[k]), width = 9))
          }
        }
        if (max(ps) > 1 - mergalpha || exh) {
          sel <- which(tie(ps) == tie(max(ps)))
          if (length(sel) > 1L) {                                    # ties: the largest AIC, then at random
            a <- aics[sel]
            keep <- if (all(is.na(a))) sel else sel[!is.na(a) & a == max(a, na.rm = TRUE)]
            sel <- keep
            if (length(sel) > 1L) sel <- sel[which.max(stats::runif(length(sel)))]
          }
          i <- prs[1, sel]
          j <- prs[2, sel]
          if (steps) say("  -> merged: ", .grptext(pr, groups[[i]]), " + ", .grptext(pr, groups[[j]]), " (p = ", .pfmt(ps[sel]), ")")
          merged <- c(groups[[i]], groups[[j]])
          if (pr$type == "u") {
            groups <- c(list(merged), groups[-c(i, j)])
          } else {
            groups[[i]] <- merged
            groups <- groups[-j]
          }
        } else {
          if (steps) say("  -> no pair exceeds p > ", 1 - mergalpha, ": merging stops")
          break
        }
      }
      # ---- splitting test and Bonferroni adjustment
      g <- length(groups)
      gt <- .group_table(tab, groups)
      gcount <- engine$count(gt)
      res <- if (g > 1L) engine$split(gt, pr$type) else c(1, NA, NA)
      raw <- res[1]
      aic <- res[2]
      stat <- res[3]
      mult <- if (!adj || g < 2L) 1 else .bonferroni(g, K, pr$type, exhaust, exact_bonf)
      padj <- raw * mult
      if (is.na(padj)) padj <- 1
      if (steps) {
        say("  Splitting test: categories ", .catlist(preds, v, groups, " | "), ": p = ", .pfmt(raw),
            ", Bonferroni multiplier = ", format(mult, digits = 4), ", adjusted p = ", .pfmt(min(padj, 1)))
        trrows <- c(trrows, paste0(formatC(pr$name, width = -14), formatC(.catlist(preds, v, groups, " | "), width = -44), formatC(.pfmt(min(padj, 1)), width = 9)))
      }
      if (isforced && !exh && g > 2L && min(padj, 1) >= spltalpha) {
        exh <- TRUE
        if (steps) say("  The forced split is not significant (adjusted p = ", .pfmt(min(padj, 1)), " is not below spltalpha(", spltalpha, ")): merging continues until two groups remain")
        next
      }
      break
      }
      aic_c <- if (is.na(aic)) Inf else aic
      take <- FALSE
      if (padj < cmp_p) {
        take <- TRUE
      } else if (padj == cmp_p) {
        if (aic_c < cmp_aic) take <- TRUE
        else if (aic_c == cmp_aic) {
          r <- stats::runif(2)
          take <- r[1] < r[2]
        }
      }
      if (take) {
        cmp_p <- padj
        cmp_aic <- aic_c
        best <- list(v = v, groups = groups, counts = gcount, stat = stat)
      }
    }
    can <- !is.null(best) && length(best$groups) > 1L && (isforced || cmp_p < spltalpha) && min(best$counts) >= minnode &&
      (maxbranch < 0 || depth < maxbranch)
    if (isforced && !can) {                                         # the forced split is not possible
      vname <- preds[[forced[depth + 1L]]]$name
      why <- if (is.null(best) || length(best$groups) <= 1L) "it has fewer than two categories in the node, or they are all merged" else paste0("a node would have n = ", fmtn(min(best$counts)), ", below minnode(", minnode, ")")
      if (depth == 0L) stop("forcevars: the sample cannot be split by '", vname, "': ", why, call. = FALSE)
      fallback <- c(fallback, nodetxt)
      if (steps) say("  -> the split forced by '", vname, "' is not possible (", why, "): the node grows freely")
      isforced <- FALSE
      next
    }
    break
    }
    if (steps) {
      say("  Candidate variables for the split (adjusted p-values):")
      for (r in trrows) say("      ", r)
      if (is.null(best) || length(best$groups) <= 1L) say("  -> no variable shows any association (adjusted p of 1 or more): the node is not split")
      else if (!isforced && !(cmp_p < spltalpha)) say("  -> best adjusted p = ", .pfmt(min(cmp_p, 1)), " is not below spltalpha(", spltalpha, "): the node is not split")
      else if (min(best$counts) < minnode) say("  -> the best split would create a node with n = ", fmtn(min(best$counts)), ", below minnode(", minnode, "): the node is not split")
      else if (maxbranch >= 0 && depth >= maxbranch) say("  -> maxbranch(", maxbranch, ") reached: the node is not split")
      else {
        say("  -> split on ", preds[[best$v]]$name, if (isforced) " (forced)" else "", " (adjusted p = ", .pfmt(min(cmp_p, 1)), "):")
        for (x in seq_along(best$groups)) say("        ", .grptext(preds[[best$v]], best$groups[[x]], TRUE), "  (n = ", fmtn(best$counts[x]), ")")
      }
    }
    if (can) {
      G <- best$groups
      parent_path <- paths[[cur]]
      parent_p <- splitp[[cur]]
      parent_s <- splitstat[[cur]]
      pval <- min(cmp_p, 1)
      codes <- preds[[best$v]]$codes[idx]
      paths[[cur]] <- c(parent_path, list(list(v = best$v, cats = G[[1]], forced = isforced)))
      splitp[[cur]] <- c(parent_p, pval)
      splitstat[[cur]] <- c(parent_s, best$stat)
      for (x in seq_along(G)[-1]) {
        nclus <- nclus + 1L
        paths[[nclus]] <- c(parent_path, list(list(v = best$v, cats = G[[x]], forced = isforced)))
        splitp[[nclus]] <- c(parent_p, pval)
        splitstat[[nclus]] <- c(parent_s, best$stat)
        cl[idx[codes %in% G[[x]]]] <- nclus
      }
    } else {
      cur <- cur + 1L
    }
  }
  # ---- number the nodes in tree order: depth first, following the lowest category of each branch
  k <- nclus
  if (renumber && k > 1L) {
    D <- max(lengths(paths))
    R <- matrix(0L, k, D)
    sig <- function(s) paste0(s$v, ":", paste(s$cats, collapse = ","))
    for (d in seq_len(D)) {
      has <- which(lengths(paths) >= d)
      par <- vapply(has, function(i) if (d == 1L) "" else paste(vapply(paths[[i]][seq_len(d - 1L)], sig, ""), collapse = "|"), "")
      key <- vapply(has, function(i) min(paths[[i]][[d]]$cats), 0)
      for (p in unique(par)) {
        sub <- which(par == p)
        R[has[sub], d] <- match(key[sub], sort(unique(key[sub])))
      }
    }
    ord <- do.call(order, lapply(seq_len(D), function(d) R[, d]))
    newid <- integer(k)
    newid[ord] <- seq_len(k)
    cl <- newid[cl]
    paths <- paths[ord]
    splitp <- splitp[ord]
    splitstat <- splitstat[ord]
  }
  list(node = cl, paths = paths, splitp = splitp, splitstat = splitstat, fallback = fallback)
}

# splitp as a matrix (nodes x levels of the tree)
.splitp_matrix <- function(splitp) {
  k <- length(splitp)
  D <- max(lengths(splitp), 1L)
  sp <- matrix(NA_real_, k, D)
  for (i in seq_len(k)) if (length(splitp[[i]])) sp[i, seq_along(splitp[[i]])] <- splitp[[i]]
  sp
}

# indices of the splitting variables whose splits are chosen by the user (forcevars): the first level of the tree is split
# by the first one, the second level by the second one, and so on
.check_forcevars <- function(forcevars, preds, maxbranch) {
  if (is.null(forcevars)) return(integer(0))
  if (!is.character(forcevars) || length(forcevars) == 0L || anyNA(forcevars)) stop("'forcevars' must be a character vector with the names of splitting variables")
  if (anyDuplicated(forcevars)) stop("'forcevars' has repeated variables: ", paste(unique(forcevars[duplicated(forcevars)]), collapse = ", "))
  i <- match(forcevars, names(preds))
  if (anyNA(i)) stop("forcevars: not splitting variables: ", paste(forcevars[is.na(i)], collapse = ", "),
                     " (the splitting variables are ", paste(names(preds), collapse = ", "), ")")
  if (maxbranch >= 0 && maxbranch < length(i)) stop("'maxbranch' (", maxbranch, ") is smaller than the number of variables of 'forcevars' (", length(i), ")")
  i
}

# one warning for the nodes where the forced split was not possible
.force_warning <- function(fallback) {
  if (length(fallback))
    warning("forcevars: the forced split was not possible in ", length(fallback), " node", if (length(fallback) > 1L) "s" else "",
            " (the variable has fewer than two categories there, or a node would be smaller than 'minnode'); ",
            if (length(fallback) > 1L) "they" else "it", " grew freely: ", paste(utils::head(fallback, 3L), collapse = "; "),
            if (length(fallback) > 3L) "; ..." else "", call. = FALSE)
}
