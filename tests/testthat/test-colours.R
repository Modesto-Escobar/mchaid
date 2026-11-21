cc <- mchaid:::.cat_col
lv <- c("No", "Yes")

test_that("colours of the categories can be named, sequential, in a list or a palette", {
  expect_identical(cc(c(Yes = "red", No = "blue"), lv, FALSE), c("blue", "red"))           # by name, any order
  expect_identical(cc(c("red", "blue"), lv, FALSE), c("red", "blue"))                      # sequential
  expect_identical(cc(list(Survived = c(Yes = "red", No = "blue")), lv, FALSE, depvar = "Survived"), c("blue", "red"))
  expect_identical(cc(c(yes = "red", NO = "blue"), lv, FALSE), c("blue", "red"))           # case does not matter
  expect_identical(cc(NULL, lv, FALSE), mchaid:::.dv_palette(2, FALSE))
  expect_identical(cc(NULL, lv, FALSE, "Set 2"), unname(grDevices::palette.colors(2, "Set 2")))
  expect_identical(cc(NULL, lv, FALSE, "dark 3"), grDevices::hcl.colors(2, "Dark 3"))
  expect_length(suppressWarnings(cc(c(Yes = "red", No = "blue"), c("No", "Yes", "missing"), FALSE, "okabe",
                                    alt = c("No", "Yes", "Missing"))), 3L)
})

test_that("wrong colours are rejected", {
  expect_error(cc("red", lv, FALSE))                                  # too few colours
  expect_error(cc(c(Yes = "red", No = "bluee"), lv, FALSE))           # invalid colour
  expect_error(cc(c(Yes = "red", yes = "blue"), lv, FALSE))           # same category twice
  expect_error(cc(NULL, lv, FALSE, "nonexistent"))
  expect_error(cc(NULL, as.character(1:12), FALSE, "Set 1"))          # palette too short
  expect_error(cc(list(a = "red", b = "blue"), lv, FALSE))
})

test_that("colours that do not fit the categories give warnings", {
  ## one category missing, one name extra
  res <- collect_warnings(cc(c(Yes = "red", Maybe = "green"), lv, FALSE))
  expect_length(res$warnings, 2L)
  expect_identical(res$value[2L], "red")
  expect_identical(res$value[1L], mchaid:::.dv_palette(2, FALSE)[1L])
  ## one colour too many
  res <- collect_warnings(cc(c("red", "blue", "green"), lv, FALSE))
  expect_length(res$warnings, 1L)
  expect_identical(res$value, c("red", "blue"))
})

test_that("ramp gives shades of one colour, light to dark", {
  r5 <- cc("forestgreen", as.character(1:5), TRUE, ramp = TRUE)
  expect_length(r5, 5L)
  expect_identical(r5[5L], "#228B22")
  expect_identical(cc("forestgreen", as.character(1:5), TRUE, ramp = "reverse"), rev(r5))
  expect_gt(mchaid:::.rellum(r5[1L]), mchaid:::.rellum(r5[3L]))
  expect_gt(mchaid:::.rellum(r5[3L]), mchaid:::.rellum(r5[5L]))
  expect_identical(cc("#228B22", "a", FALSE, ramp = TRUE), "#228B22")
})

test_that("ramp rejects wrong arguments", {
  expect_error(cc(NULL, lv, FALSE, ramp = TRUE))
  expect_error(cc(c("red", "blue"), lv, FALSE, ramp = TRUE))
  expect_error(cc(c(a = "red"), lv, FALSE, ramp = TRUE))
  expect_error(cc("red", lv, FALSE, ramp = "up"))
  expect_error(cc("red", lv, FALSE, ramp = FALSE))
})
