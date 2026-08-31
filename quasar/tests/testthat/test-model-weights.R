# ============================================================
# QUASAR — qsr_model() weighted regression (survey weights)
# ============================================================

set.seed(1)
n <- 1500
df <- data.frame(
  y = rbinom(n, 1, 0.3),
  x = rnorm(n),
  g = sample(c("a", "b"), n, TRUE),
  w = runif(n, 0.5, 3)
)

test_that("qsr_model() weights by column name match lm()", {
  m <- qsr_model(y ~ x + g, type = "lm", data = df, weights = "w")
  expect_equal(unname(coef(m)), unname(coef(lm(y ~ x + g, data = df, weights = w))))
})

test_that("qsr_model() weights by vector match lm()", {
  m <- qsr_model(y ~ x + g, type = "lm", data = df, weights = df$w)
  expect_equal(unname(coef(m)), unname(coef(lm(y ~ x + g, data = df, weights = w))))
})

test_that("qsr_model() supports weighted logit", {
  m <- qsr_model(y ~ x, type = "logit", data = df, weights = "w")
  expect_s3_class(m, "glm")
  ref <- glm(y ~ x, data = df, family = binomial("logit"), weights = w)
  expect_equal(unname(coef(m)), unname(coef(ref)))
})

test_that("qsr_model() weights work when the column is NOT named 'w'", {
  # Regression test: previously the weight vector was passed to lm as a bare
  # local `w`, which lm resolved via the data/formula env, so it only worked
  # when a column literally named 'w' existed. Here the weight is 'wt' and there
  # is no 'w' column, which used to fail with "object 'w' not found".
  df2 <- data.frame(y = rnorm(200), x = rnorm(200), wt = runif(200, 0.5, 3))
  m <- qsr_model(y ~ x, type = "lm", data = df2, weights = "wt")
  expect_equal(unname(coef(m)), unname(coef(lm(y ~ x, data = df2, weights = wt))))
})

test_that("qsr_model() unweighted is unchanged", {
  m <- qsr_model(y ~ x, type = "lm", data = df)
  expect_equal(unname(coef(m)), unname(coef(lm(y ~ x, data = df))))
})

test_that("qsr_model() validates weights", {
  expect_error(qsr_model(y ~ x, data = df, weights = "nope"), "not found")
  expect_error(qsr_model(y ~ x, data = df, weights = 1:3), "length")
})
