# ============================================================
# QUASAR — Tests for Time Series (qsr_ts)
# ============================================================

test_that("qsr_ts() fits ARIMA", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "arima", order = c(1, 1, 1))
  expect_true(!is.null(result$model))
  expect_equal(result$method, "arima")
  qsr_reset()
})

test_that("qsr_ts() forecasts with ARIMA", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "arima", order = c(1, 1, 1),
                   forecast = 12)
  expect_true(!is.null(result$forecast))
  qsr_reset()
})

test_that("qsr_ts() decomposes", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "decompose")
  expect_true(inherits(result$model, "decomposed.ts"))
  qsr_reset()
})

test_that("qsr_ts() fits Holt-Winters", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "hw", forecast = 6)
  expect_true(inherits(result$model, "HoltWinters"))
  expect_true(!is.null(result$forecast))
  qsr_reset()
})

test_that("qsr_ts() auto-ARIMA works", {
  skip_if_not_installed("forecast")
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "auto", forecast = 12)
  expect_true(!is.null(result$model))
  expect_true(!is.null(result$forecast))
  qsr_reset()
})

test_that("qsr_ts() ETS works", {
  skip_if_not_installed("forecast")
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "ets")
  expect_true(!is.null(result$model))
  qsr_reset()
})

test_that("qsr_ts() works from data frame column", {
  qsr_reset()
  df <- data.frame(month = 1:120, sales = sin(1:120 / 6) * 100 + 500 + rnorm(120, 0, 10))
  qsr_data(df)
  result <- qsr_ts(y = "sales", frequency = 12, method = "hw")
  expect_true(!is.null(result$model))
  qsr_reset()
})

test_that("qsr_ts() stores model in context", {
  qsr_reset()
  qsr_ts(AirPassengers, method = "arima", order = c(1, 0, 0), name = "my_ts")
  models <- .qsr_context$get_models()
  expect_true("my_ts" %in% names(models))
  qsr_reset()
})

test_that("qsr_ts() horizon alias works for Holt-Winters", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "hw", horizon = 6)
  expect_true(!is.null(result$forecast))
  expect_equal(length(result$forecast$mean), 6)
  qsr_reset()
})

test_that("qsr_ts() horizon alias works for auto.arima", {
  skip_if_not_installed("forecast")
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "auto", horizon = 12)
  expect_true(!is.null(result$forecast))
  qsr_reset()
})

test_that("qsr_ts() horizon alias works for ARIMA", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "arima", order = c(1, 1, 0),
                   horizon = 10)
  expect_true(!is.null(result$forecast))
  qsr_reset()
})

test_that("qsr_ts() forecast takes precedence over horizon", {
  qsr_reset()
  result <- qsr_ts(AirPassengers, method = "hw", forecast = 6, horizon = 12)
  expect_equal(length(result$forecast$mean), 6)
  qsr_reset()
})

test_that("qsr_ts() fails without data", {
  qsr_reset()
  expect_error(qsr_ts(), "No data")
  qsr_reset()
})
