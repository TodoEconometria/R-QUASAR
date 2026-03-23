# ============================================================
# QUASAR — Tests for Analytics (qsr_clean, qsr_test, qsr_feature)
# ============================================================

# ---- qsr_clean() ----

test_that("qsr_clean() imputes with median", {
  qsr_reset()
  df <- data.frame(x = c(1, 2, NA, 4, 5), y = c(10, NA, 30, 40, 50))
  qsr_data(df)
  result <- qsr_clean(impute = "median", register = FALSE)
  expect_false(anyNA(result))
  expect_equal(result$x[3], 3)  # median of 1,2,4,5
  qsr_reset()
})

test_that("qsr_clean() imputes with mean", {
  df <- data.frame(x = c(1, 2, NA, 4, 5))
  result <- qsr_clean(data = df, impute = "mean", register = FALSE)
  expect_equal(result$x[3], 3)  # mean of 1,2,4,5
})

test_that("qsr_clean() drops NA rows", {
  df <- data.frame(x = c(1, 2, NA, 4), y = c(10, NA, 30, 40))
  result <- qsr_clean(data = df, impute = "drop", register = FALSE)
  expect_equal(nrow(result), 2)
})

test_that("qsr_clean() removes outliers", {
  set.seed(42)
  df <- data.frame(x = c(rnorm(100), 100, -100))  # two extreme outliers
  result <- qsr_clean(data = df, outliers = "remove",
                      outlier_threshold = 3, register = FALSE)
  expect_true(nrow(result) < 102)
})

test_that("qsr_clean() winsorizes outliers", {
  set.seed(42)
  df <- data.frame(x = c(rnorm(50, mean = 10, sd = 2), 100))
  result <- qsr_clean(data = df, outliers = "winsorize",
                      outlier_threshold = 3, register = FALSE)
  expect_true(max(result$x) < 100)
})

test_that("qsr_clean() normalizes with minmax", {
  df <- data.frame(x = c(0, 25, 50, 75, 100))
  result <- qsr_clean(data = df, normalize = "minmax", register = FALSE)
  expect_equal(min(result$x), 0)
  expect_equal(max(result$x), 1)
})

test_that("qsr_clean() normalizes with zscore", {
  df <- data.frame(x = rnorm(100, mean = 50, sd = 10))
  result <- qsr_clean(data = df, normalize = "zscore", register = FALSE)
  expect_equal(round(mean(result$x), 5), 0)
  expect_equal(round(sd(result$x), 5), 1)
})

test_that("qsr_clean() dummy-encodes categorical", {
  df <- data.frame(x = 1:6, g = c("A", "B", "C", "A", "B", "C"))
  result <- qsr_clean(data = df, encode = "dummy", register = FALSE)
  expect_true("g_B" %in% names(result))
  expect_true("g_C" %in% names(result))
  expect_false("g" %in% names(result))
})

test_that("qsr_clean() label-encodes categorical", {
  df <- data.frame(x = 1:3, g = c("A", "B", "C"))
  result <- qsr_clean(data = df, encode = "label", register = FALSE)
  expect_true(is.integer(result$g))
})

test_that("qsr_clean() registers in context", {
  qsr_reset()
  df <- data.frame(x = c(1, 2, NA))
  qsr_data(df)
  qsr_clean(impute = "mean")
  ctx <- .qsr_context$get_data()
  expect_false(anyNA(ctx))
  qsr_reset()
})

test_that("qsr_clean() drops high-NA columns", {
  df <- data.frame(x = 1:10, mostly_na = c(1, rep(NA, 9)))
  result <- qsr_clean(data = df, drop_na_cols = 0.5, register = FALSE)
  expect_false("mostly_na" %in% names(result))
})

# ---- qsr_test() ----

test_that("qsr_test() auto-detects t-test", {
  qsr_reset()
  qsr_data(mtcars)
  result <- qsr_test(mpg ~ am)
  expect_true(!is.null(result$p_value))
  expect_true(result$p_value < 0.05)
  qsr_reset()
})

test_that("qsr_test() runs shapiro test", {
  result <- qsr_test(~mpg, data = mtcars, test = "shapiro")
  expect_true(!is.null(result$p_value))
  expect_equal(result$test, "Shapiro-Wilk normality test")
})

test_that("qsr_test() runs correlation", {
  result <- qsr_test(mpg ~ wt, data = mtcars, test = "correlation")
  expect_true(result$estimate < 0)  # negative correlation
  expect_true(result$significant)
})

test_that("qsr_test() runs wilcoxon", {
  result <- suppressWarnings(
    qsr_test(mpg ~ am, data = mtcars, test = "wilcox")
  )
  expect_true(!is.null(result$p_value))
})

test_that("qsr_test() uses significance level from context", {
  qsr_reset()
  qsr_config(significance_level = 0.001)
  qsr_data(mtcars)
  result <- qsr_test(mpg ~ wt, test = "correlation")
  expect_true(result$significant)  # very strong correlation
  qsr_reset()
})

test_that("qsr_test() fails without formula", {
  expect_error(qsr_test(data = mtcars), "formula")
})

# ---- qsr_feature() ----

test_that("qsr_feature() creates log transforms", {
  result <- qsr_feature(data = mtcars, log = c("hp"), register = FALSE)
  expect_true("log_hp" %in% names(result))
  expect_equal(result$log_hp[1], log(mtcars$hp[1] + 1))
})

test_that("qsr_feature() creates polynomial features", {
  result <- qsr_feature(data = mtcars, polynomial = c(wt = 3), register = FALSE)
  expect_true("wt_pow2" %in% names(result))
  expect_true("wt_pow3" %in% names(result))
})

test_that("qsr_feature() creates interactions", {
  result <- qsr_feature(data = mtcars, interactions = c("wt:hp"), register = FALSE)
  expect_true("wt_x_hp" %in% names(result))
  expect_equal(result$wt_x_hp[1], mtcars$wt[1] * mtcars$hp[1])
})

test_that("qsr_feature() creates bins", {
  result <- qsr_feature(data = mtcars, bins = list(mpg = 4), register = FALSE)
  expect_true("mpg_bin" %in% names(result))
  expect_true(all(result$mpg_bin %in% 1:4))
})

test_that("qsr_feature() creates lag features", {
  df <- data.frame(value = 1:10)
  result <- qsr_feature(data = df, lag = list(value = 2), register = FALSE)
  expect_true("value_lag1" %in% names(result))
  expect_true("value_lag2" %in% names(result))
  expect_true(is.na(result$value_lag1[1]))
  expect_equal(result$value_lag1[3], 2)
})

test_that("qsr_feature() registers in context", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_feature(log = c("hp"))
  ctx <- .qsr_context$get_data()
  expect_true("log_hp" %in% names(ctx))
  qsr_reset()
})
