test_that("plot() draws the classic tree and returns its size", {
  with_pdf({
    sz <- plot(fit)
    expect_true(all(sz > 0))
    sz2 <- plot(fm, quartiles = "auto")
    expect_true(all(sz2 > 0))
    expect_error(plot(f5), NA)
  })
})

test_that("the graph of the boxes runs for every plot of the nodes and every language", {
  with_pdf({
    sz <- plot(fit, treestyle = "boxes", language = "es")
    expect_true(all(sz > 0))
    for (np in c("bar", "box", "both")) {
      sz <- plot(fm, treestyle = "boxes", nodeplot = np, center = if (np == "box") "median" else "mean",
                 whisker = c("p5", "minmax", "tukey")[match(np, c("bar", "box", "both"))], shade = np != "bar")
      expect_true(all(sz > 0), info = np)
    }
    expect_error({
      plot(fm, treestyle = "boxes", language = "fr", main = "Test", pval = FALSE)
      plot(fit, treestyle = "boxes", language = "de", pcut = 0.001)
      plot(fit, treestyle = "boxes", nodehead = "number")
      plot(fit, treestyle = "boxes", rootlabel = "depvar")
      plot(fm, treestyle = "boxes", nodeplot = "both", rootlabel = "Todo")
      for (wh in c("p5", "minmax", "tukey")) plot(f5, treestyle = "boxes", nodeplot = "box", whisker = wh, language = "ca")
    }, NA)
  })
})

test_that("plot() rejects wrong arguments and warns about the ones that are ignored", {
  with_pdf({
    expect_error(plot(fit, treestyle = "boxes", nodehead = "x"))
    expect_error(plot(fit, treestyle = "boxes", nodeplot = "boxes"), "nodeplot", fixed = TRUE)
    expect_error(plot(fit, treestyle = "boxes", nodeplot = "box"))   # a box plot needs a quantitative or ordered response
    w <- first_warning(plot(fit, nodehead = "category"))
    expect_match(w, "nodehead", fixed = TRUE)
    expect_match(w, "boxes", fixed = TRUE)
    w <- first_warning(plot(fm, nodeplot = "box", center = "mean"))
    expect_match(w, "nodeplot", fixed = TRUE)
    expect_match(w, "center", fixed = TRUE)
  })
})

test_that("the digits of the mean, median and SD are checked and used", {
  with_pdf({
    expect_error(plot(fm, treestyle = "boxes", digits = 7))
    expect_error(plot(fm, digits = 1.5))
    for (dg in c(0, 1, 3)) {
      expect_error({
        plot(fm, treestyle = "boxes", nodeplot = "both", digits = dg)
        plot(fm, digits = dg)
      }, NA)
    }
  })
})

test_that("the digits reach the data of the interactive tree", {
  skip_if_not_installed("jsonlite")
  d1 <- mchaid:::.tree_data(fm, nodeplot = "bar")
  d3 <- mchaid:::.tree_data(fm, nodeplot = "bar", digits = 3)
  expect_identical(d1$nodes[[1L]]$l3, paste0("Mean ", formatC(mean(mtcars$mpg), format = "f", digits = 1)))
  expect_identical(d3$nodes[[1L]]$l3, paste0("Mean ", formatC(mean(mtcars$mpg), format = "f", digits = 3)))
  expect_identical(d1$nodes[[1L]]$l4, paste0("SD ", formatC(sd(mtcars$mpg), format = "f", digits = 1)))
})

test_that("the colours reach the graphs", {
  cl <- c(Yes = "#00AA00", No = "#AA0000")
  with_pdf({
    expect_error({
      plot(fit, col = cl)
      plot(fit, treestyle = "boxes", col = cl)
      plot(fit, treestyle = "boxes", col = c("#AA0000", "#00AA00"), nodehead = "number")
      plot(fit, palette = "Set 2")
    }, NA)
    expect_error(plot(fit, col = "red"))
    expect_error(plot(fit, palette = "zzz"))
  })
})

test_that("the colours reach the data of the interactive tree", {
  skip_if_not_installed("jsonlite")
  cl <- c(Yes = "#00AA00", No = "#AA0000")
  d1 <- mchaid:::.tree_data(fit, col = cl)
  d2 <- mchaid:::.tree_data(fit, col = c("#AA0000", "#00AA00"))
  expect_identical(jsonlite::toJSON(d1$options, auto_unbox = TRUE), jsonlite::toJSON(d2$options, auto_unbox = TRUE))
  expect_match(as.character(jsonlite::toJSON(d1, auto_unbox = TRUE)), "#00AA00", fixed = TRUE)
})

test_that("ramp draws shades of one colour", {
  with_pdf({
    expect_error({
      plot(fit, treestyle = "boxes", col = "forestgreen", ramp = TRUE)
      plot(fit, col = "forestgreen", ramp = "reverse")
    }, NA)
  })
  skip_if_not_installed("jsonlite")
  d <- mchaid:::.tree_data(fit, col = "forestgreen", ramp = TRUE)
  first <- mchaid:::.cat_col("forestgreen", c("No", "Yes"), FALSE, ramp = TRUE)[1L]
  expect_match(as.character(jsonlite::toJSON(d, auto_unbox = TRUE)), substr(first, 2L, 7L), fixed = TRUE)
})

test_that("nodeplot = 'dots' selects the observations to draw", {
  s1 <- mchaid:::.dots_select(dd$y, rep(1, 903), 1, 30, 200)
  expect_length(s1$v, 200L)
  expect_true(s1$sampled)
  expect_identical(s1, mchaid:::.dots_select(dd$y, rep(1, 903), 1, 30, 200))          # reproducible
  expect_equal(sum(s1$v < 1 | s1$v > 30), sum(dd$y < 1 | dd$y > 30))                  # the outliers are always drawn
  expect_true(all(abs(s1$j) <= 1))
  expect_identical(mchaid:::.dots_select(c(1, 2, 3, 4), c(2, 1, 1, 1), 0, 10)$v, c(1, 1, 2, 3, 4))
  expect_false(mchaid:::.dots_select(1:50, rep(1, 50), 10, 40)$sampled)
  expect_length(mchaid:::.dots_select(1:50, rep(1, 50), 10, 40)$v, 50L)
})

test_that("nodeplot = 'dots' is drawn for a quantitative response only", {
  with_pdf({
    expect_error({
      plot(fd, treestyle = "boxes", nodeplot = "dots")
      plot(fd, treestyle = "boxes", nodeplot = "dots", maxdots = 40, whisker = "tukey", language = "es")
    }, NA)
    expect_error(plot(fd, treestyle = "boxes", nodeplot = "dots", maxdots = 5))
    expect_error(plot(fit, treestyle = "boxes", nodeplot = "dots"))
  })
})

test_that("the labels, not the codes, are drawn in the classic graphs", {
  with_pdf({
    r1 <- plot(fl, language = "es", rootlabel = "depvar")
    r2 <- plot(fc, pval = FALSE)
    r3 <- plot(fc, quartiles = FALSE)
  })
  expect_type(r1, "double")
  expect_type(r2, "double")
  expect_type(r3, "double")
})
