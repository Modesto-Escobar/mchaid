test_that("the likelihood-ratio tests on tables are right", {
  n <- rbind(c(30, 10, 5), c(10, 20, 15))
  E <- outer(rowSums(n), colSums(n)) / sum(n)
  lr_manual <- 2 * sum(n * log(n / E))
  expect_equal(mchaid:::.lr_pair(n[1, ], n[2, ], FALSE)[1], pchisq(lr_manual, 2, lower.tail = FALSE))
  expect_equal(mchaid:::.lr_table(n)[1], pchisq(lr_manual, 2, lower.tail = FALSE))
  ## the linear-score multinomial test with two groups is the LR test of the table
  expect_equal(mchaid:::.lr_linear(n, c(1, 2))[1], mchaid:::.lr_table(n)[1], tolerance = 1e-6)
})

test_that("the ordered-logit test with two response levels is a logit", {
  n2 <- rbind(c(40, 12), c(25, 33))
  g <- stats::glm(cbind(n2[, 1], n2[, 2]) ~ c(0, 1), family = binomial)
  expect_equal(mchaid:::.ologit_test(n2, matrix(c(0, 1), 2, 1))[1],
               pchisq(g$null.deviance - g$deviance, 1, lower.tail = FALSE), tolerance = 1e-6)
})

test_that("the Bonferroni multipliers are the Stirling numbers and the formula of Stata", {
  expect_equal(mchaid:::.bonferroni(2, 5, "u", FALSE, TRUE), 15)   # S(5, 2)
  expect_equal(mchaid:::.bonferroni(3, 5, "u", FALSE, TRUE), 25)   # S(5, 3)
  expect_equal(mchaid:::.bonferroni(2, 5, "u", FALSE, FALSE), 16)  # Stata formula, g = 2
  expect_equal(mchaid:::.bonferroni(3, 5, "o", FALSE), 6)          # ordered
})

test_that("the F tests from sufficient statistics agree with lm() and t.test()", {
  y <- c(3, 5, 4, 8, 9, 7, 12, 11, 13, 10)
  grp <- rep(1:3, c(3, 3, 4))
  tab <- cbind(n = tapply(rep(1, 10), grp, sum), s = tapply(y - mean(y), grp, sum), q = tapply((y - mean(y))^2, grp, sum))
  expect_equal(mchaid:::.f_split(unname(tab), "u", FALSE)[1], anova(lm(y ~ factor(grp)))[["Pr(>F)"]][1])
  expect_equal(mchaid:::.f_split(unname(tab), "o", FALSE)[1], anova(lm(y ~ grp))[["Pr(>F)"]][1])
  expect_equal(mchaid:::.f_pair(unname(tab[1, ]), unname(tab[2, ]))[1], t.test(y[grp == 1], y[grp == 2], var.equal = TRUE)$p.value)
})

test_that("the weighted statistics of a node agree with the expanded data", {
  ## percentiles as in summarize, detail (= type 2 of quantile())
  set.seed(5)
  yy <- round(rnorm(60, 10, 3), 1)
  ww <- sample(1:3, 60, TRUE)
  st <- mchaid:::.wstats(yy, ww)
  ex <- rep(yy, ww)
  expect_equal(st[5:9], unname(quantile(ex, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 2)))
  expect_equal(st[c(1:4, 10)], c(sum(ww), mean(ex), sd(ex), min(yy), max(yy)))
  expect_equal(mchaid:::.pcat(c(25, 50, 25))[4], 1.5)   # median on the scale of the categories
})

test_that("numbers are formatted for each language", {
  expect_identical(mchaid:::.loc("1,234.5", "es"), "1.234,5")
  expect_identical(mchaid:::.loc("1,234.5", "fr"), "1 234,5")
})

test_that("the decimals of the mean, median and SD are formatted", {
  expect_identical(mchaid:::.fmtd(20.0937, 1), "20.1")
  expect_identical(mchaid:::.fmtd(-0.04, 1), "0.0")
  expect_identical(mchaid:::.fmtd(12345.678, 2, "es"), "12.345,68")
  expect_identical(mchaid:::.fmtd(7.5, 0), "8")
  expect_identical(mchaid:::.fmtd(NA_real_, 1), "")
})
