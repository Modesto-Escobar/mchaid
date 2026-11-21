test_that("the tree of the Titanic table is the one given by Stata", {
  expect_equal(fit$N_clusters, 7L)
  expect_equal(unname(fit$sizes), c(180, 59, 630, 862, 145, 129, 196))
  expect_equal(mchaid_node_labels(fit, FALSE)[6], "Sex = Female & Class in {2nd, Crew}")
  expect_equal(round(summary(fit)$risk, 3), 0.217)
})

test_that("numbering in creation order keeps the same partition", {
  fit2 <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq, minnode = 30, minsplit = 60, renumber = FALSE)
  k <- !is.na(fit$node)
  expect_equal(nrow(unique(cbind(fit$node[k], fit2$node[k]))), 7L)
})

test_that("the test statistic of each split is the one of e(splitchi) in Stata", {
  expect_equal(round(c(fit$splitstat[1, 1], fit$splitstat[1, 2], fit$splitstat[5, 2], fit$splitstat[2, 3]), 3),
               c(434.469, 27.443, 142.727, 22.058))
  expect_true(is.na(fit$splitstat[1, 3]))
})

test_that("predict, classification and importance work", {
  expect_equal(predict(fit, data.frame(Class = "1st", Sex = "Female", Age = "Adult")), 5L)
  cl <- classification(fit)
  expect_equal(cl$N_correct, 1724)
  set.seed(2)
  imp <- importance(fit, nrep = 3)
  expect_equal(dim(imp), c(2L, 3L))
})

test_that("missing values and an ordered response are accepted", {
  set.seed(3)
  d <- data.frame(y = factor(sample(1:4, 400, TRUE), ordered = TRUE), a = sample(c(1:3, NA), 400, TRUE),
                  b = factor(sample(letters[1:3], 400, TRUE)))
  d$y[d$b == "a" & d$a %in% 1] <- 4
  f3 <- mchaid(y ~ a + b, d, missing = TRUE, minnode = 20, minsplit = 40)
  expect_s3_class(f3, "mchaid")
  expect_true(f3$dvordered)
})

test_that("numeric variables with value labels use the labels in the texts", {
  expect_identical(fl$predictors$g$labels, c("low", "mid", "high"))
  expect_identical(fl$predictors$h$labels, c("1", "2"))
  expect_identical(fc$predictors$g$labels, c("low", "mid", "high"))
})

test_that("asnetCoin() converts the tree of mchaid to a network", {
  net <- asnetCoin(fit, build = FALSE)
  expect_equal(nrow(net$nodes), 11L)
  expect_equal(nrow(net$links), 10L)
  expect_equal(sort(net$nodes$type), sort(c("root", rep("split", 3), rep("node", 7))))
  expect_equal(sum(net$nodes$n[net$nodes$type == "node"]), 2201)
})

test_that("asnetCoin() marks the forced splits", {
  an <- asnetCoin(f2l, build = FALSE)
  expect_identical(sum(an$nodes$forced), sum(an$nodes$type != "node" & an$nodes$variable %in% c("Sex", "Age")))
  expect_true(any(an$nodes$forced))
})
