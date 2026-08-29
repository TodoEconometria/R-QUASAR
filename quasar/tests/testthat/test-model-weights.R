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

test_that("qsr_model() unweighted is unchanged", {
  m <- qsr_model(y ~ x, type = "lm", data = df)
  expect_equal(unname(coef(m)), unname(coef(lm(y ~ x, data = df))))
})

test_that("qsr_model() validates weights", {
  expect_error(qsr_model(y ~ x, data = df, weights = "nope"), "not found")
  expect_error(qsr_model(y ~ x, data = df, weights = 1:3), "length")
})
