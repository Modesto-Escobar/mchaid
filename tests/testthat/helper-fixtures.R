# Objects shared by the tests. The reference values come from the Stata commands mchaid (version 1.0.0)
# and maid, run on the same data (the Titanic table with frequency weights, and mtcars).

## the Titanic table: a categorical response, frequency weights
tt <- as.data.frame(Titanic)
set.seed(1)
fit <- mchaid(Survived ~ Class + Sex + Age, data = tt, weights = Freq, minnode = 30, minsplit = 60)

## mtcars: a quantitative response
set.seed(1234)
fm <- maid(mpg ~ cyl + gear + carb + disp + hp, data = mtcars, unordered = "carb", ordered = c("cyl", "gear"),
           xtile = c(disp = 3, hp = 3), minnode = 5, minsplit = 10)

## an ordered response with five levels
set.seed(8)
d5 <- data.frame(y = factor(sample(1:5, 800, TRUE), ordered = TRUE), a = sample(1:3, 800, TRUE), b = sample(1:3, 800, TRUE))
d5$y[d5$a == 1] <- sample(4:5, sum(d5$a == 1), TRUE)
f5 <- mchaid(y ~ a + b, d5, minnode = 30, minsplit = 60)

## forced splits: Sex first, then Age
f2l <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60, forcevars = c("Sex", "Age"))

## numeric variables with value labels (haven style)
set.seed(3)
dl <- data.frame(g = sample(1:3, 400, TRUE), h = sample(1:2, 400, TRUE))
attr(dl$g, "labels") <- c(low = 1, mid = 2, high = 3)
dl$y <- dl$g * 2 + rnorm(400)
dl$z <- factor(ifelse(dl$y + rnorm(400) > 4, "yes", "no"))
fl <- maid(y ~ g + h, data = dl, unordered = c("g", "h"), minnode = 20, minsplit = 40)
fc <- mchaid(z ~ g + h, data = dl, unordered = c("g", "h"), minnode = 20, minsplit = 40)

## a skewed response, for the points of the nodes
set.seed(5)
dd <- data.frame(y = c(rexp(900) * 10, 60, 75, -5), g = sample(c("a", "b", "c"), 903, TRUE), z = rnorm(903))
dd$y <- dd$y + (dd$g == "c") * 8
fd <- maid(y ~ g + z, data = dd, unordered = "g", xtile = c(z = 4), minnode = 100, minsplit = 200)

## ---- helpers

## evaluate the code on a null graphics device
with_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

## all the warnings of the code, as a list(value, warnings)
collect_warnings <- function(expr) {
  w <- character()
  value <- withCallingHandlers(expr, warning = function(z) {
    w <<- c(w, conditionMessage(z))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w)
}

## the message of the first warning of the code (the value of the code if there is none)
first_warning <- function(expr) tryCatch(expr, warning = function(w) conditionMessage(w))

## a new folder in the temporary directory, for the pages of tree_html()
new_dir <- function(name = "page") file.path(tempfile(name))
