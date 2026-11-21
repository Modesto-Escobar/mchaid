test_that("the tree and the statistics of the nodes are those of the Stata command maid on mtcars", {
  expect_equal(fm$N_clusters, 3L)
  expect_equal(unname(fm$nodes[, "n"]), c(11, 7, 14))
  expect_equal(unname(round(fm$nodes[, "mean"], 4)), c(26.6636, 19.7429, 15.1))
  expect_equal(unname(round(fm$nodes[, "sd"], 4)), c(4.5098, 1.4536, 2.56))
  expect_equal(unname(fm$nodes[, "p50"]), c(26, 19.7, 15.2))
  sm <- summary(fm)
  expect_equal(round(sm$r2, 5), 0.73246)
  expect_equal(round(sm$F, 4), 39.6975)
  expect_equal(unname(predict(fm, mtcars[1:2, ], type = "mean")), c(19.74286, 19.74286), tolerance = 1e-5)
})

test_that("the F statistic of each split is the one of e(splitF) in Stata", {
  expect_equal(round(fm$splitstat[, 1], 3), rep(79.561, 3))
})

test_that("importance and asnetCoin() work for a quantitative response", {
  nm <- asnetCoin(fm, build = FALSE)
  expect_equal(nrow(nm$nodes), 4L)
  expect_equal(round(nm$nodes$mean[nm$nodes$type == "root"], 5), round(mean(mtcars$mpg), 5))
  set.seed(2)
  imp <- importance(fm, nrep = 2)
  expect_equal(ncol(imp), 5L)
})

test_that("the colours of maid are given as above and below", {
  expect_identical(mchaid:::.maid_col(c(below = "orange", above = "navy")), c("navy", "orange"))
  expect_error(mchaid:::.maid_col(c(up = "orange", down = "navy")))
  expect_identical(mchaid:::.maid_col(list(c("navy", "orange"))), c("navy", "orange"))
})
