rootvar <- function(f) names(f$predictors)[f$paths[[1L]][[1L]]$v]
nch1 <- function(f) length(unique(vapply(f$paths, function(p) paste(p[[1L]]$cats, collapse = ","), "")))
same_partition <- function(a, b) {
  tb <- table(a, b)
  all(rowSums(tb > 0) == 1L) && all(colSums(tb > 0) == 1L)
}

test_that("forcevars chooses the first split of the tree", {
  ff <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60, forcevars = "Age")
  expect_identical(rootvar(ff), "Age")
  expect_identical(ff$forcevars, "Age")
  expect_length(ff$force_fallback, 0L)
  ## the subtree under a forced child is the free tree of that subsample
  for (g in c("Child", "Adult")) {
    sub <- tt[tt$Age == g, ]
    fs <- mchaid(Survived ~ Class + Sex + Age, sub, weights = Freq, minnode = 30, minsplit = 60)
    rows <- which(tt$Age == g & tt$Freq > 0)
    expect_true(same_partition(ff$node[rows], fs$node[sub$Freq > 0]), info = g)
  }
})

test_that("forcevars chooses several levels of the tree", {
  expect_true(all(vapply(f2l$paths, function(p) names(f2l$predictors)[p[[1L]]$v] == "Sex" && names(f2l$predictors)[p[[2L]]$v] == "Age", NA)))
})

test_that("a forced variable that is not significant gives a binary split", {
  set.seed(7)
  dn <- tt[rep(seq_len(nrow(tt)), tt$Freq), ]
  dn$Freq <- 1
  dn$noise <- factor(sample(letters[1:4], nrow(dn), TRUE))
  fn <- mchaid(Survived ~ Class + Sex + Age + noise, dn, weights = Freq, minnode = 30, minsplit = 60, forcevars = "noise")
  expect_identical(rootvar(fn), "noise")
  expect_equal(nch1(fn), 2L)
  expect_gte(fn$splitp[1L, 1L], 0.05)
})

test_that("a node too deep to be forced grows freely, with a warning", {
  t2 <- tt[!(tt$Sex == "Female" & tt$Age == "Child"), ]
  res <- collect_warnings(mchaid(Survived ~ Class + Sex + Age, t2, weights = Freq, minnode = 30, minsplit = 60,
                                 forcevars = c("Sex", "Age")))
  expect_length(res$warnings, 1L)
  expect_length(res$value$force_fallback, 1L)
  expect_match(res$warnings, "grew freely")
})

test_that("forcevars rejects wrong arguments", {
  f <- function(...) mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, ...)
  expect_error(f(forcevars = "Nothing"))
  expect_error(f(forcevars = c("Sex", "Sex")))
  expect_error(f(forcevars = c("Sex", "Age"), maxbranch = 1))
  expect_error(f(minnode = 5000, minsplit = 6000, forcevars = "Sex"))
})

test_that("forcevars works with maid", {
  fmf <- suppressWarnings(maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb", ordered = c("cyl", "gear"),
                               xtile = c(disp = 3, hp = 3), minnode = 4, minsplit = 8, forcevars = c("gear", "carb")))
  expect_identical(rootvar(fmf), "gear")
  expect_identical(fmf$forcevars, c("gear", "carb"))
})

test_that("forced splits are marked and explained in a note", {
  expect_true(mchaid:::.has_forced(f2l))
  expect_false(mchaid:::.has_forced(fit))
  expect_match(mchaid:::.forced_note("es"), "forcevars", fixed = TRUE)
  expect_true(all(nzchar(vapply(c("en", "es", "fr", "de", "it", "pt", "ca", "gl", "nl", "eu"), mchaid:::.forced_note, ""))))
  expect_identical(mchaid:::.fmark(list(forced = TRUE)), "*")
  expect_identical(mchaid:::.fmark(list(v = 1L)), "")
})

test_that("the forced splits are drawn in every language and style", {
  with_pdf({
    for (lg in c("en", "es", "eu")) {
      expect_error({
        plot(f2l, language = lg)
        plot(f2l, treestyle = "boxes", language = lg)
        plot(f2l, treestyle = "boxes", nodehead = "number", language = lg)
      }, NA)
    }
    ff <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60, forcevars = "Age")
    expect_error(plot(ff, treestyle = "boxes"), NA)
    fmf <- suppressWarnings(maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb", ordered = c("cyl", "gear"),
                                 xtile = c(disp = 3, hp = 3), minnode = 4, minsplit = 8, forcevars = c("gear", "carb")))
    expect_error({
      plot(fmf)
      plot(fmf, treestyle = "boxes", nodeplot = "both")
      plot(fit, treestyle = "boxes")   # without forced splits nothing changes
    }, NA)
  })
})

test_that("the classic graph replaces the dagger on devices that cannot write it", {
  ## a single-byte encoding without the dagger, as pdf() has on some systems
  grDevices::pdf(NULL, encoding = "KOI8-R.enc")
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  expect_false(mchaid:::.dagger_ok())
  expect_warning(expect_error(plot(f2l), NA), NA)
})

test_that("the forced splits reach the data of the interactive tree", {
  skip_if_not_installed("jsonlite")
  dh <- mchaid:::.tree_data(f2l)
  sv <- unlist(lapply(dh$nodes, function(n) n$svar))
  expect_true(all(sv[grepl("^Sex|^Age", sv)] %in% c("Sex*", "Age*")))
  expect_match(dh$options$forcednote, "forcevars", fixed = TRUE)
  expect_null(mchaid:::.tree_data(fit)$options$forcednote)
  pg <- tree_html(f2l, note = "Source", dir = new_dir("forced"))
  expect_match(pg$data$options$note, "^Source\n\\* ")
})
