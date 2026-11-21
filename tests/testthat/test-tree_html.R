skip_if_not_installed("jsonlite")

test_that("tree_html() writes the page and the data of the nodes", {
  dd <- new_dir("pagina")
  pg <- tree_html(fit, main = "Test", language = "es", dir = dd)
  expect_s3_class(pg, "tree_html")
  expect_true(file.exists(pg$file))
  expect_true(file.exists(file.path(dd, "scripts", "tree.js")))
  expect_true(file.exists(file.path(dd, "scripts", "d3.min.js")))
  expect_true(file.exists(file.path(dd, "styles", "tree.css")))
  nd <- pg$data$nodes
  expect_gt(length(nd), 1L)
  expect_equal(nd[[1L]]$id, 0L)
  expect_identical(nd[[1L]]$headlab, "Total")
  expect_identical(pg$data$options$nodehead, "category")
  expect_true(all(vapply(nd, function(r) nzchar(r$headlab), TRUE)))
  expect_true(any(grepl("Total", readLines(file.path(dd, "index.html"), warn = FALSE, encoding = "UTF-8"), fixed = TRUE)))
  expect_match(capture.output(print(pg))[1L], "index.html", fixed = TRUE)
})

test_that("the options of tree_html() reach the page", {
  dd <- new_dir("pagina")
  expect_identical(tree_html(fit, rootlabel = "depvar", dir = dd)$data$nodes[[1L]]$headlab, "Survived")
  expect_identical(tree_html(fit, rootlabel = "Todo", dir = dd)$data$nodes[[1L]]$headlab, "Todo")
  expect_identical(tree_html(fit, nodehead = "number", dir = dd)$data$options$nodehead, "number")
  for (np in c("bar", "box", "both")) expect_error(tree_html(fm, nodeplot = np, dir = dd), NA)
  expect_error(tree_html(f5, nodeplot = "box", language = "ca", dir = dd), NA)
  expect_error(tree_html(fit, nodehead = "x", dir = dd))
})

test_that("a folder that is not a page of the package is not overwritten", {
  dx <- new_dir("otra")
  dir.create(dx)
  writeLines("x", file.path(dx, "a.txt"))
  expect_error(tree_html(fit, dir = dx))
  expect_false(file.exists(file.path(dx, "index.html")))
  expect_identical(readLines(file.path(dx, "a.txt")), "x")
})

test_that("an existing empty folder is accepted and a page of the package is replaced", {
  de <- new_dir("vacia")
  dir.create(de)
  expect_true(file.exists(tree_html(fit, dir = de)$file))
  expect_true(file.exists(file.path(de, "scripts", "tree.js")))
  expect_true(file.exists(tree_html(fit, dir = de)$file))
})

test_that("nodeplot = 'dots' sends the points of the nodes", {
  hd <- tree_html(fd, nodeplot = "dots", maxdots = 100, dir = new_dir("dots"))
  nn <- hd$data$nodes
  expect_identical(hd$data$options$nodeplot, "dots")
  expect_true(all(vapply(nn, function(n) length(n$dv) == length(n$dj) && length(n$dv) > 0L, TRUE)))
  expect_true(all(vapply(nn, function(n) length(n$dv) <= 100L + sum(n$dv < n$lo | n$dv > n$hi), TRUE)))
  expect_match(hd$data$options$dotstxt, "sample", fixed = TRUE)
  expect_true(all(vapply(nn, function(n) all(n$dv >= hd$data$options$scale$vmin & n$dv <= hd$data$options$scale$vmax), TRUE)))
  expect_match(paste(readLines(hd$file, warn = FALSE), collapse = ""), "\"dv\"", fixed = TRUE)
  expect_null(tree_html(fd, dir = new_dir("nodots"))$data$nodes[[1L]]$dv)
})

test_that("the interactive tree in the classic style is built", {
  hc <- tree_html(fc, treestyle = "classic", dir = new_dir("classic"))
  nn <- hc$data$nodes
  expect_identical(hc$data$options$treestyle, "classic")
  expect_true(isTRUE(hc$data$options$showp))
  expect_true(nzchar(hc$data$options$foot$left))
  expect_true(all(vapply(nn, function(n) grepl("^n = ", n$ncl), TRUE)))
  expect_true(all(vapply(nn, function(n) is.null(n$stat) || grepl("^χ²=", n$stat), TRUE)))
  expect_true(any(vapply(nn, function(n) grepl("low|mid|high", n$headlab), TRUE)))
  expect_identical(nn[[1L]]$headlab, "Total")

  hm <- tree_html(fl, treestyle = "classic", quartiles = TRUE, dir = new_dir("classic_maid"))
  expect_gt(hm$data$options$maxdev, 0)
  expect_false(is.null(hm$data$nodes[[2L]]$mtxt))
  expect_false(is.null(hm$data$nodes[[2L]]$med))
  expect_identical(hm$data$options$quartiles, "auto")

  w <- first_warning(tree_html(fc, treestyle = "classic", nodeplot = "bar", dir = new_dir("classic_w")))
  expect_type(w, "character")
  expect_match(w, "ignored", fixed = TRUE)

  ## the page of the boxes is as before
  expect_null(tree_html(fc, dir = new_dir("boxes"))$data$options$treestyle)
})
