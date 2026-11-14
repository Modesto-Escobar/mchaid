# Tests with base R only. The reference values come from the Stata command mchaid (version 2.3)
# run on the same data (the Titanic table, with frequency weights).
library(mchaid)

stopifnot_equal <- function(a, b, tol = 1e-8, what = "") {
  if (!isTRUE(all.equal(a, b, tolerance = tol))) stop("test failed: ", what, "\n", paste(capture.output(print(a)), collapse = "\n"), "\nvs\n", paste(capture.output(print(b)), collapse = "\n"))
}

## ---- the tree of the Titanic table, as given by Stata
tt <- as.data.frame(Titanic)
set.seed(1)
fit <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq, minnode = 30, minsplit = 60)
stopifnot_equal(fit$N_clusters, 7L, what = "number of nodes")
stopifnot_equal(unname(fit$sizes), c(180, 59, 630, 862, 145, 129, 196), what = "sizes of the nodes")
stopifnot_equal(mchaid_node_labels(fit, FALSE)[6], "Sex = Female & Class in {2nd, Crew}", what = "node label")
stopifnot_equal(round(summary(fit)$risk, 3), 0.217, what = "risk estimate")

## ---- numbering in creation order keeps the same partition
fit2 <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq, minnode = 30, minsplit = 60, renumber = FALSE)
k <- !is.na(fit$node)
stopifnot(nrow(unique(cbind(fit$node[k], fit2$node[k]))) == 7L)

## ---- tests on tables
n <- rbind(c(30, 10, 5), c(10, 20, 15))
chi_manual <- {E <- outer(rowSums(n), colSums(n)) / sum(n); sum((n - E)^2 / E)}
lr_manual <- 2 * sum(n * log(n / E))
stopifnot_equal(mchaid:::.lr_pair(n[1, ], n[2, ], FALSE)[1], pchisq(lr_manual, 2, lower.tail = FALSE), what = "LR test of two groups")
stopifnot_equal(mchaid:::.lr_table(n)[1], pchisq(lr_manual, 2, lower.tail = FALSE), what = "LR test of a table")

## the linear-score multinomial test with two groups is the LR test of the table
stopifnot_equal(mchaid:::.lr_linear(n, c(1, 2))[1], mchaid:::.lr_table(n)[1], tol = 1e-6, what = "linear score with two groups")

## the ordered-logit test with two response levels is a logit: compare with glm
n2 <- rbind(c(40, 12), c(25, 33))
g <- stats::glm(cbind(n2[, 1], n2[, 2]) ~ c(0, 1), family = binomial)
stopifnot_equal(mchaid:::.ologit_test(n2, matrix(c(0, 1), 2, 1))[1],
                pchisq(g$null.deviance - g$deviance, 1, lower.tail = FALSE), tol = 1e-6, what = "ologit with two levels")

## ---- Bonferroni multipliers: Stirling numbers (exact) and the formula of the Stata command
stopifnot_equal(mchaid:::.bonferroni(2, 5, "u", FALSE, TRUE), 15, what = "S(5,2)")
stopifnot_equal(mchaid:::.bonferroni(3, 5, "u", FALSE, TRUE), 25, what = "S(5,3)")
stopifnot_equal(mchaid:::.bonferroni(2, 5, "u", FALSE, FALSE), 16, what = "Stata formula g = 2")
stopifnot_equal(mchaid:::.bonferroni(3, 5, "o", FALSE), 6, what = "ordered")

## ---- predict, classification, importance
stopifnot_equal(predict(fit, data.frame(Class = "1st", Sex = "Female", Age = "Adult")), 5L, what = "predict node")
cl <- classification(fit)
stopifnot_equal(cl$N_correct, 1724, what = "correctly classified")
set.seed(2)
imp <- importance(fit, nrep = 3)
stopifnot_equal(dim(imp), c(2L, 3L), what = "importance")

## ---- the tree as a network
net <- asnetCoin(fit, build = FALSE)
stopifnot_equal(nrow(net$nodes), 11L, what = "nodes of the network")
stopifnot_equal(nrow(net$links), 10L, what = "links of the network")
stopifnot_equal(sort(net$nodes$type), sort(c("root", rep("split", 3), rep("node", 7))), what = "types of node")
stopifnot_equal(sum(net$nodes$n[net$nodes$type == "node"]), 2201, what = "cases in the terminal nodes")

## ---- plot runs on a null device
pdf(NULL)
sz <- plot(fit)
dev.off()
stopifnot(all(sz > 0))

## ---- missing values and ordered response
set.seed(3)
d <- data.frame(y = factor(sample(1:4, 400, TRUE), ordered = TRUE), a = sample(c(1:3, NA), 400, TRUE), b = factor(sample(letters[1:3], 400, TRUE)))
d$y[d$b == "a" & d$a %in% 1] <- 4
f3 <- mchaid(y ~ a + b, d, missing = TRUE, minnode = 20, minsplit = 40)
stopifnot(inherits(f3, "mchaid"), f3$dvordered)
## ---- maid(): the tree and the statistics of the nodes, as given by the Stata command maid on mtcars
set.seed(1234)
fm <- maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb", ordered = c("cyl", "gear"),
           xtile = c(disp = 3, hp = 3), minnode = 5, minsplit = 10)
stopifnot_equal(fm$N_clusters, 3L, what = "nodes of maid")
stopifnot_equal(unname(fm$nodes[, "n"]), c(11, 7, 14), what = "sizes of the nodes of maid")
stopifnot_equal(unname(round(fm$nodes[, "mean"], 4)), c(26.6636, 19.7429, 15.1), what = "means of the nodes of maid")
stopifnot_equal(unname(round(fm$nodes[, "sd"], 4)), c(4.5098, 1.4536, 2.56), what = "sd of the nodes of maid")
stopifnot_equal(unname(fm$nodes[, "p50"]), c(26, 19.7, 15.2), what = "medians of the nodes of maid")
sm <- summary(fm)
stopifnot_equal(round(sm$r2, 5), 0.73246, what = "R-squared of maid")
stopifnot_equal(round(sm$F, 4), 39.6975, what = "F of maid")
stopifnot_equal(unname(predict(fm, mtcars[1:2, ], type = "mean")), c(19.74286, 19.74286), tol = 1e-5, what = "predict maid")

## F tests from sufficient statistics against lm() and t.test()
y <- c(3, 5, 4, 8, 9, 7, 12, 11, 13, 10)
grp <- rep(1:3, c(3, 3, 4))
tab <- cbind(n = tapply(rep(1, 10), grp, sum), s = tapply(y - mean(y), grp, sum), q = tapply((y - mean(y))^2, grp, sum))
stopifnot_equal(mchaid:::.f_split(unname(tab), "u", FALSE)[1], anova(lm(y ~ factor(grp)))[["Pr(>F)"]][1], tol = 1e-8, what = "F test of a factor")
stopifnot_equal(mchaid:::.f_split(unname(tab), "o", FALSE)[1], anova(lm(y ~ grp))[["Pr(>F)"]][1], tol = 1e-8, what = "F test of a linear term")
stopifnot_equal(mchaid:::.f_pair(unname(tab[1, ]), unname(tab[2, ]))[1], t.test(y[grp == 1], y[grp == 2], var.equal = TRUE)$p.value, tol = 1e-8, what = "F test of two groups")

## asnetCoin of a maid tree: colour reflects the mean
nm <- asnetCoin(fm, build = FALSE)
stopifnot_equal(nrow(nm$nodes), 4L, what = "nodes of the network of maid")
stopifnot_equal(round(nm$nodes$mean[nm$nodes$type == "root"], 5), round(mean(mtcars$mpg), 5), what = "mean of the root")
imp2 <- importance(fm, nrep = 2)
stopifnot_equal(ncol(imp2), 5L, what = "importance of maid")
pdf(NULL)
sz2 <- plot(fm, quartiles = "auto")
dev.off()
stopifnot(all(sz2 > 0))
## ---- test statistic of each split (chi2 of mchaid, F of maid): the same values as e(splitchi) and e(splitF) in Stata
stopifnot_equal(round(c(fit$splitstat[1, 1], fit$splitstat[1, 2], fit$splitstat[5, 2], fit$splitstat[2, 3]), 3), c(434.469, 27.443, 142.727, 22.058), what = "chi2 of the splits")
stopifnot(is.na(fit$splitstat[1, 3]))
stopifnot_equal(round(fm$splitstat[, 1], 3), rep(79.561, 3), what = "F of the split of maid")

## ---- statistics of the nodes for the graph of the boxes: the percentiles of summarize, detail (= type 2 of quantile())
set.seed(5)
yy <- round(rnorm(60, 10, 3), 1)
ww <- sample(1:3, 60, TRUE)
st <- mchaid:::.wstats(yy, ww)
ex <- rep(yy, ww)
stopifnot_equal(st[5:9], unname(quantile(ex, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 2)), what = "percentiles of .wstats")
stopifnot_equal(st[c(1:4, 10)], c(sum(ww), mean(ex), sd(ex), min(yy), max(yy)), what = "statistics of .wstats")
stopifnot_equal(mchaid:::.pcat(c(25, 50, 25))[4], 1.5, what = "median on the scale of the categories")
stopifnot_equal(mchaid:::.loc("1,234.5", "es"), "1.234,5", what = "numbers in Spanish")
stopifnot_equal(mchaid:::.loc("1,234.5", "fr"), "1 234,5", what = "numbers in French")

## ---- the graph of the boxes runs for every plot of the nodes and every language
pdf(NULL)
sz <- plot(fit, treestyle = "boxes", language = "es")
for (np in c("bar", "box", "both")) sz <- plot(fm, treestyle = "boxes", nodeplot = np, center = if (np == "box") "median" else "mean",
                                               whisker = c("p5", "minmax", "tukey")[match(np, c("bar", "box", "both"))], shade = np != "bar")
plot(fm, treestyle = "boxes", language = "fr", main = "Test", pval = FALSE)
plot(fit, treestyle = "boxes", language = "de", pcut = 0.001)
plot(fit, treestyle = "boxes", nodehead = "number")
plot(fit, treestyle = "boxes", rootlabel = "depvar")
plot(fm, treestyle = "boxes", nodeplot = "both", rootlabel = "Todo")
bad <- tryCatch(plot(fit, treestyle = "boxes", nodehead = "x"), error = function(e) "error")
stopifnot(identical(bad, "error"))
msg <- tryCatch(plot(fit, treestyle = "boxes", nodeplot = "boxes"), error = function(e) conditionMessage(e))
stopifnot(grepl("nodeplot", msg, fixed = TRUE))
w <- tryCatch(plot(fit, nodehead = "category"), warning = function(w) conditionMessage(w))
stopifnot(grepl("nodehead", w, fixed = TRUE), grepl("boxes", w, fixed = TRUE))
w <- tryCatch(plot(fm, nodeplot = "box", center = "mean"), warning = function(w) conditionMessage(w))
stopifnot(grepl("nodeplot", w, fixed = TRUE), grepl("center", w, fixed = TRUE))
set.seed(8)
d2 <- data.frame(y = factor(sample(1:5, 800, TRUE), ordered = TRUE), a = sample(1:3, 800, TRUE), b = sample(1:3, 800, TRUE))
d2$y[d2$a == 1] <- sample(4:5, sum(d2$a == 1), TRUE)
f5 <- mchaid(y ~ a + b, d2, minnode = 30, minsplit = 60)
for (wh in c("p5", "minmax", "tukey")) plot(f5, treestyle = "boxes", nodeplot = "box", whisker = wh, language = "ca")
plot(f5)
bad <- tryCatch(plot(fit, treestyle = "boxes", nodeplot = "box"), error = function(e) "error")
dev.off()
stopifnot(all(sz > 0), identical(bad, "error"))
## ---- the interactive tree (HTML page): files, data of the nodes and options
if (requireNamespace("jsonlite", quietly = TRUE)) {
  dd <- file.path(tempdir(), "pagina1")
  pg <- tree_html(fit, main = "Test", language = "es", dir = dd)
  stopifnot(inherits(pg, "tree_html"), file.exists(pg$file), file.exists(file.path(dd, "scripts", "tree.js")),
            file.exists(file.path(dd, "scripts", "d3.min.js")), file.exists(file.path(dd, "styles", "tree.css")))
  nd <- pg$data$nodes
  stopifnot(length(nd) > 1L, nd[[1L]]$id == 0L, nd[[1L]]$headlab == "Total",
            identical(pg$data$options$nodehead, "category"), all(vapply(nd, function(r) nzchar(r$headlab), TRUE)))
  stopifnot(identical(tree_html(fit, rootlabel = "depvar", dir = dd)$data$nodes[[1L]]$headlab, "Survived"),
            identical(tree_html(fit, rootlabel = "Todo", dir = dd)$data$nodes[[1L]]$headlab, "Todo"))
  stopifnot(identical(tree_html(fit, nodehead = "number", dir = dd)$data$options$nodehead, "number"))
  for (np in c("bar", "box", "both")) tree_html(fm, nodeplot = np, dir = dd)
  tree_html(f5, nodeplot = "box", language = "ca", dir = dd)
  stopifnot(identical(tryCatch(tree_html(fit, nodehead = "x", dir = dd), error = function(e) "error"), "error"))
  stopifnot(any(grepl("Total", readLines(file.path(dd, "index.html"), warn = FALSE, encoding = "UTF-8"), fixed = TRUE)))
  out <- capture.output(print(pg))
  stopifnot(grepl("index.html", out[1L], fixed = TRUE))
  ## a folder that is not a page of the package is not overwritten
  dx <- file.path(tempdir(), "otra"); dir.create(dx); writeLines("x", file.path(dx, "a.txt"))
  stopifnot(identical(tryCatch(tree_html(fit, dir = dx), error = function(e) "error"), "error"), !file.exists(file.path(dx, "index.html")),
            identical(readLines(file.path(dx, "a.txt")), "x"))
  ## an existing empty folder is accepted, and a page of the package is replaced
  de <- file.path(tempdir(), "vacia"); dir.create(de)
  stopifnot(file.exists(tree_html(fit, dir = de)$file), file.exists(file.path(de, "scripts", "tree.js")), file.exists(tree_html(fit, dir = de)$file))
}
## ---- decimals of the mean, median and SD of maid (digits)
stopifnot(identical(mchaid:::.fmtd(20.0937, 1), "20.1"), identical(mchaid:::.fmtd(-0.04, 1), "0.0"),
          identical(mchaid:::.fmtd(12345.678, 2, "es"), "12.345,68"), identical(mchaid:::.fmtd(7.5, 0), "8"), identical(mchaid:::.fmtd(NA_real_, 1), ""))
stopifnot(identical(tryCatch(plot(fm, treestyle = "boxes", digits = 7), error = function(e) "error"), "error"),
          identical(tryCatch(plot(fm, digits = 1.5), error = function(e) "error"), "error"))
for (dg in c(0, 1, 3)) {
  plot(fm, treestyle = "boxes", nodeplot = "both", digits = dg)
  plot(fm, digits = dg)
}
if (requireNamespace("jsonlite", quietly = TRUE)) {
  d1 <- mchaid:::.tree_data(fm, nodeplot = "bar")
  d2 <- mchaid:::.tree_data(fm, nodeplot = "bar", digits = 3)
  stopifnot(identical(d1$nodes[[1L]]$l3, paste0("Mean ", formatC(mean(mtcars$mpg), format = "f", digits = 1))),
            identical(d2$nodes[[1L]]$l3, paste0("Mean ", formatC(mean(mtcars$mpg), format = "f", digits = 3))),
            identical(d1$nodes[[1L]]$l4, paste0("SD ", formatC(sd(mtcars$mpg), format = "f", digits = 1))))
}
## ---- colours of the categories: named (netCoin style), sequential, list, palettes
cc <- mchaid:::.cat_col
lv <- c("No", "Yes")
stopifnot(identical(cc(c(Yes = "red", No = "blue"), lv, FALSE), c("blue", "red")),                      # by name, any order
          identical(cc(c("red", "blue"), lv, FALSE), c("red", "blue")),                                 # sequential
          identical(cc(list(Survived = c(Yes = "red", No = "blue")), lv, FALSE, depvar = "Survived"), c("blue", "red")),
          identical(cc(c(yes = "red", NO = "blue"), lv, FALSE), c("blue", "red")),                      # case does not matter
          identical(cc(NULL, lv, FALSE), mchaid:::.dv_palette(2, FALSE)),
          identical(cc(NULL, lv, FALSE, "Set 2"), unname(grDevices::palette.colors(2, "Set 2"))),
          identical(cc(NULL, lv, FALSE, "dark 3"), grDevices::hcl.colors(2, "Dark 3")),
          identical(length(suppressWarnings(cc(c(Yes = "red", No = "blue"), c("No", "Yes", "missing"), FALSE, "okabe", alt = c("No", "Yes", "Missing")))), 3L))
errs <- function(e) tryCatch({ e; "no" }, error = function(z) "error")
stopifnot(identical(errs(cc("red", lv, FALSE)), "error"),                                              # too few colours
          identical(errs(cc(c(Yes = "red", No = "bluee"), lv, FALSE)), "error"),                       # invalid colour
          identical(errs(cc(c(Yes = "red", yes = "blue"), lv, FALSE)), "error"),                       # same category twice
          identical(errs(cc(NULL, lv, FALSE, "nonexistent")), "error"),
          identical(errs(cc(NULL, as.character(1:12), FALSE, "Set 1")), "error"),                       # palette too short
          identical(errs(cc(list(a = "red", b = "blue"), lv, FALSE)), "error"))
w <- NULL
r <- withCallingHandlers(cc(c(Yes = "red", Maybe = "green"), lv, FALSE), warning = function(z) { w <<- c(w, conditionMessage(z)); invokeRestart("muffleWarning") })
stopifnot(length(w) == 2L, r[2L] == "red", r[1L] == mchaid:::.dv_palette(2, FALSE)[1L])                 # one category missing, one name extra
w <- NULL
r <- withCallingHandlers(cc(c("red", "blue", "green"), lv, FALSE), warning = function(z) { w <<- c(w, conditionMessage(z)); invokeRestart("muffleWarning") })
stopifnot(length(w) == 1L, identical(r, c("red", "blue")))
## maid: c(above =, below =)
stopifnot(identical(mchaid:::.maid_col(c(below = "orange", above = "navy")), c("navy", "orange")),
          identical(errs(mchaid:::.maid_col(c(up = "orange", down = "navy"))), "error"),
          identical(mchaid:::.maid_col(list(c("navy", "orange"))), c("navy", "orange")))
## in the graphs: the colours reach the three outputs and the legend
cl <- c(Yes = "#00AA00", No = "#AA0000")
plot(fit, col = cl)
plot(fit, treestyle = "boxes", col = cl)
plot(fit, treestyle = "boxes", col = c("#AA0000", "#00AA00"), nodehead = "number")
plot(fit, palette = "Set 2")
stopifnot(identical(errs(plot(fit, col = "red")), "error"), identical(errs(plot(fit, palette = "zzz")), "error"))
if (requireNamespace("jsonlite", quietly = TRUE)) {
  d1 <- mchaid:::.tree_data(fit, col = cl)
  d2 <- mchaid:::.tree_data(fit, col = c("#AA0000", "#00AA00"))
  stopifnot(identical(jsonlite::toJSON(d1$options, auto_unbox = TRUE), jsonlite::toJSON(d2$options, auto_unbox = TRUE)),
            grepl("#00AA00", jsonlite::toJSON(d1, auto_unbox = TRUE), fixed = TRUE))
}
## ---- ramp: shades of one colour, light to dark
r5 <- cc("forestgreen", as.character(1:5), TRUE, ramp = TRUE)
stopifnot(length(r5) == 5L, identical(r5[5L], "#228B22"), identical(cc("forestgreen", as.character(1:5), TRUE, ramp = "reverse"), rev(r5)),
          mchaid:::.rellum(r5[1L]) > mchaid:::.rellum(r5[3L]), mchaid:::.rellum(r5[3L]) > mchaid:::.rellum(r5[5L]),
          identical(cc("#228B22", "a", FALSE, ramp = TRUE), "#228B22"),
          identical(errs(cc(NULL, lv, FALSE, ramp = TRUE)), "error"),
          identical(errs(cc(c("red", "blue"), lv, FALSE, ramp = TRUE)), "error"),
          identical(errs(cc(c(a = "red"), lv, FALSE, ramp = TRUE)), "error"),
          identical(errs(cc("red", lv, FALSE, ramp = "up")), "error"),
          identical(errs(cc("red", lv, FALSE, ramp = FALSE)), "error"))
plot(fit, treestyle = "boxes", col = "forestgreen", ramp = TRUE)
plot(fit, col = "forestgreen", ramp = "reverse")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  d3 <- mchaid:::.tree_data(fit, col = "forestgreen", ramp = TRUE)
  stopifnot(grepl(substr(cc("forestgreen", lv, FALSE, ramp = TRUE)[1L], 2L, 7L), jsonlite::toJSON(d3, auto_unbox = TRUE), fixed = TRUE))
}
## ---- forcevars: the first levels of the tree chosen by the user
ti <- as.data.frame(Titanic)
rootvar <- function(f) names(f$predictors)[f$paths[[1L]][[1L]]$v]
nch1 <- function(f) length(unique(vapply(f$paths, function(p) paste(p[[1L]]$cats, collapse = ","), "")))
same_partition <- function(a, b) { tb <- table(a, b); all(rowSums(tb > 0) == 1L) && all(colSums(tb > 0) == 1L) }
ff <- mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, minnode = 30, minsplit = 60, forcevars = "Age")
stopifnot(rootvar(ff) == "Age", identical(ff$forcevars, "Age"), length(ff$force_fallback) == 0L)
for (g in c("Child", "Adult")) {                 # the subtree under a forced child is the free tree of that subsample
  sub <- ti[ti$Age == g, ]
  fs <- mchaid(Survived ~ Class + Sex + Age, sub, weights = Freq, minnode = 30, minsplit = 60)
  rows <- which(ti$Age == g & ti$Freq > 0)
  stopifnot(same_partition(ff$node[rows], fs$node[sub$Freq > 0]))
}
f2l <- mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, minnode = 30, minsplit = 60, forcevars = c("Sex", "Age"))
stopifnot(all(vapply(f2l$paths, function(p) names(f2l$predictors)[p[[1L]]$v] == "Sex" && names(f2l$predictors)[p[[2L]]$v] == "Age", NA)))
set.seed(7)
dn <- ti[rep(seq_len(nrow(ti)), ti$Freq), ]
dn$Freq <- 1
dn$noise <- factor(sample(letters[1:4], nrow(dn), TRUE))
fn <- mchaid(Survived ~ Class + Sex + Age + noise, dn, weights = Freq, minnode = 30, minsplit = 60, forcevars = "noise")
stopifnot(rootvar(fn) == "noise", nch1(fn) == 2L, fn$splitp[1L, 1L] >= 0.05)          # not significant: binary split
t2 <- ti[!(ti$Sex == "Female" & ti$Age == "Child"), ]
wm <- NULL
f4 <- withCallingHandlers(mchaid(Survived ~ Class + Sex + Age, t2, weights = Freq, minnode = 30, minsplit = 60, forcevars = c("Sex", "Age")),
                          warning = function(z) { wm <<- c(wm, conditionMessage(z)); invokeRestart("muffleWarning") })
stopifnot(length(wm) == 1L, length(f4$force_fallback) == 1L, grepl("grew freely", wm))   # deep node: grows freely, with a warning
stopifnot(identical(errs(mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, forcevars = "Nothing")), "error"),
          identical(errs(mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, forcevars = c("Sex", "Sex"))), "error"),
          identical(errs(mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, forcevars = c("Sex", "Age"), maxbranch = 1)), "error"),
          identical(errs(mchaid(Survived ~ Class + Sex + Age, ti, weights = Freq, minnode = 5000, minsplit = 6000, forcevars = "Sex")), "error"))
fmf <- suppressWarnings(maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb", ordered = c("cyl", "gear"),
                             xtile = c(disp = 3, hp = 3), minnode = 4, minsplit = 8, forcevars = c("gear", "carb")))
stopifnot(names(fmf$predictors)[fmf$paths[[1L]][[1L]]$v] == "gear", identical(fmf$forcevars, c("gear", "carb")))
plot(ff, treestyle = "boxes")
## ---- forced splits are marked with an asterisk (a dagger in the classic graph of mchaid) and explained in a note
stopifnot(mchaid:::.has_forced(f2l), !mchaid:::.has_forced(fit), grepl("forcevars", mchaid:::.forced_note("es")),
          all(nzchar(vapply(c("en", "es", "fr", "de", "it", "pt", "ca", "gl", "nl", "eu"), mchaid:::.forced_note, ""))),
          identical(mchaid:::.fmark(list(forced = TRUE)), "*"), identical(mchaid:::.fmark(list(v = 1L)), ""))
for (lg in c("en", "es", "eu")) {
  plot(f2l, language = lg)
  plot(f2l, treestyle = "boxes", language = lg)
  plot(f2l, treestyle = "boxes", nodehead = "number", language = lg)
}
plot(fmf)
plot(fmf, treestyle = "boxes", nodeplot = "both")
plot(fit, treestyle = "boxes")                                   # without forced splits nothing changes
if (requireNamespace("jsonlite", quietly = TRUE)) {
  dh <- mchaid:::.tree_data(f2l)
  sv <- unlist(lapply(dh$nodes, function(n) n$svar))
  stopifnot(all(sv[grepl("^Sex|^Age", sv)] %in% c("Sex*", "Age*")), grepl("forcevars", dh$options$forcednote),
            is.null(mchaid:::.tree_data(fit)$options$forcednote))
  pg <- tree_html(f2l, note = "Source", dir = file.path(tempdir(), "forced_page"))
  stopifnot(grepl("^Source\n\\* ", pg$data$options$note))
}
an <- asnetCoin(f2l, build = FALSE)
stopifnot(identical(sum(an$nodes$forced), sum(an$nodes$type != "node" & an$nodes$variable %in% c("Sex", "Age"))), any(an$nodes$forced))
## ---- nodeplot = "dots" (maid): the observations as points under the box plot
set.seed(5)
dd <- data.frame(y = c(rexp(900) * 10, 60, 75, -5), g = sample(c("a", "b", "c"), 903, TRUE), z = rnorm(903))
dd$y <- dd$y + (dd$g == "c") * 8
fd <- maid(y ~ g + z, data = dd, unordered = "g", xtile = c(z = 4), minnode = 100, minsplit = 200)
s1 <- mchaid:::.dots_select(dd$y, rep(1, 903), 1, 30, 200)
stopifnot(length(s1$v) == 200L, s1$sampled, identical(s1, mchaid:::.dots_select(dd$y, rep(1, 903), 1, 30, 200)),
          sum(s1$v < 1 | s1$v > 30) == sum(dd$y < 1 | dd$y > 30), all(abs(s1$j) <= 1),
          identical(mchaid:::.dots_select(c(1, 2, 3, 4), c(2, 1, 1, 1), 0, 10)$v, c(1, 1, 2, 3, 4)),
          !mchaid:::.dots_select(1:50, rep(1, 50), 10, 40)$sampled, length(mchaid:::.dots_select(1:50, rep(1, 50), 10, 40)$v) == 50L)
plot(fd, treestyle = "boxes", nodeplot = "dots")
plot(fd, treestyle = "boxes", nodeplot = "dots", maxdots = 40, whisker = "tukey", language = "es")
stopifnot(identical(errs(plot(fd, treestyle = "boxes", nodeplot = "dots", maxdots = 5)), "error"),
          identical(errs(plot(fit, treestyle = "boxes", nodeplot = "dots")), "error"))
if (requireNamespace("jsonlite", quietly = TRUE)) {
  hd <- tree_html(fd, nodeplot = "dots", maxdots = 100, dir = file.path(tempdir(), "dots_page"))
  nn <- hd$data$nodes
  stopifnot(hd$data$options$nodeplot == "dots", all(vapply(nn, function(n) length(n$dv) == length(n$dj) && length(n$dv) > 0L, TRUE)),
            all(vapply(nn, function(n) length(n$dv) <= 100L + sum(n$dv < n$lo | n$dv > n$hi), TRUE)), grepl("sample", hd$data$options$dotstxt),
            all(vapply(nn, function(n) all(n$dv >= hd$data$options$scale$vmin & n$dv <= hd$data$options$scale$vmax), TRUE)),
            grepl("\"dv\"", paste(readLines(hd$file, warn = FALSE), collapse = "")))
  stopifnot(is.null(tree_html(fd, dir = file.path(tempdir(), "nodots_page"))$data$nodes[[1L]]$dv))
}
## ---- numeric variable with value labels (haven style): the labels, not the codes, in the texts; the classic graphs (new format) draw
{
  set.seed(3)
  dl <- data.frame(g = sample(1:3, 400, TRUE), h = sample(1:2, 400, TRUE))
  attr(dl$g, "labels") <- c(low = 1, mid = 2, high = 3)
  dl$y <- dl$g * 2 + rnorm(400)
  dl$z <- factor(ifelse(dl$y + rnorm(400) > 4, "yes", "no"))
  fl <- maid(y ~ g + h, data = dl, unordered = c("g", "h"), minnode = 20, minsplit = 40)
  stopifnot(identical(fl$predictors$g$labels, c("low", "mid", "high")), identical(fl$predictors$h$labels, c("1", "2")))
  fc <- mchaid(z ~ g + h, data = dl, unordered = c("g", "h"), minnode = 20, minsplit = 40)
  stopifnot(identical(fc$predictors$g$labels, c("low", "mid", "high")))
  pf <- tempfile(fileext = ".pdf")
  grDevices::pdf(pf)
  r1 <- plot(fl, language = "es", rootlabel = "depvar")
  r2 <- plot(fc, pval = FALSE)
  r3 <- plot(fc, quartiles = FALSE)
  grDevices::dev.off()
  stopifnot(is.numeric(r1), is.numeric(r2), is.numeric(r3))
}
## ---- interactive tree in the classic style (treestyle = "classic")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  hc <- tree_html(fc, treestyle = "classic", dir = file.path(tempdir(), "classic_page"))
  nn <- hc$data$nodes
  stopifnot(identical(hc$data$options$treestyle, "classic"), isTRUE(hc$data$options$showp), nzchar(hc$data$options$foot$left),
            all(vapply(nn, function(n) grepl("^n = ", n$ncl), TRUE)), all(vapply(nn, function(n) is.null(n$stat) || grepl("^\u03c7\u00b2=", n$stat), TRUE)),
            any(vapply(nn, function(n) grepl("low|mid|high", n$headlab), TRUE)), identical(nn[[1L]]$headlab, "Total"))
  hm <- tree_html(fl, treestyle = "classic", quartiles = TRUE, dir = file.path(tempdir(), "classic_maid"))
  stopifnot(hm$data$options$maxdev > 0, !is.null(hm$data$nodes[[2L]]$mtxt), !is.null(hm$data$nodes[[2L]]$med), identical(hm$data$options$quartiles, "auto"))
  w <- tryCatch(tree_html(fc, treestyle = "classic", nodeplot = "bar", dir = file.path(tempdir(), "classic_w")), warning = function(w) conditionMessage(w))
  stopifnot(is.character(w), grepl("ignored", w))
  stopifnot(is.null(tree_html(fc, dir = file.path(tempdir(), "boxes_page"))$data$options$treestyle))      # the boxes page is as before
}
cat("all tests passed\n")
