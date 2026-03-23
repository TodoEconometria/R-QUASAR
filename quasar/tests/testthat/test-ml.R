# ============================================================
# QUASAR — Tests for ML (qsr_ml, qsr_cv, qsr_compare)
# ============================================================

# ---- qsr_ml() ----

test_that("qsr_ml() fits neural network", {
  qsr_reset()
  qsr_data(mtcars)
  model <- qsr_ml(mpg ~ wt + hp, method = "nn", size = 3)
  expect_true(inherits(model, "nnet"))
  qsr_reset()
})

test_that("qsr_ml() fits decision tree", {
  skip_if_not_installed("rpart")
  model <- qsr_ml(mpg ~ wt + hp, data = mtcars, method = "tree")
  expect_true(inherits(model, "rpart"))
})

test_that("qsr_ml() fits random forest", {
  skip_if_not_installed("ranger")
  model <- qsr_ml(mpg ~ wt + hp, data = mtcars, method = "rf")
  expect_true(inherits(model, "ranger"))
})

test_that("qsr_ml() fits SVM", {
  skip_if_not_installed("e1071")
  model <- qsr_ml(mpg ~ wt + hp, data = mtcars, method = "svm")
  expect_true(inherits(model, "svm"))
})

test_that("qsr_ml() fits lasso", {
  skip_if_not_installed("glmnet")
  model <- qsr_ml(mpg ~ wt + hp + disp, data = mtcars, method = "lasso")
  expect_true(inherits(model, "cv.glmnet"))
})

test_that("qsr_ml() fits ridge", {
  skip_if_not_installed("glmnet")
  model <- qsr_ml(mpg ~ wt + hp + disp, data = mtcars, method = "ridge")
  expect_true(inherits(model, "cv.glmnet"))
})

test_that("qsr_ml() fits k-means", {
  qsr_reset()
  qsr_data(mtcars)
  model <- qsr_ml(method = "kmeans", centers = 3)
  expect_true(inherits(model, "kmeans"))
  expect_equal(length(unique(model$cluster)), 3)
  qsr_reset()
})

test_that("qsr_ml() fits PCA", {
  qsr_reset()
  qsr_data(mtcars)
  model <- qsr_ml(method = "pca")
  expect_true(inherits(model, "prcomp"))
  qsr_reset()
})

test_that("qsr_ml() stores model in context", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_ml(mpg ~ wt, method = "nn", size = 2, name = "my_nn")
  models <- .qsr_context$get_models()
  expect_true("my_nn" %in% names(models))
  qsr_reset()
})

test_that("qsr_ml() uses seed from context", {
  qsr_reset()
  qsr_config(random_seed = 123)
  qsr_data(mtcars)
  m1 <- qsr_ml(mpg ~ wt + hp, method = "nn", size = 3)
  qsr_reset()
  qsr_config(random_seed = 123)
  qsr_data(mtcars)
  m2 <- qsr_ml(mpg ~ wt + hp, method = "nn", size = 3)
  # Same seed should give same weights
  expect_equal(m1$wts, m2$wts)
  qsr_reset()
})

test_that("qsr_ml() fails without data", {
  qsr_reset()
  expect_error(qsr_ml(mpg ~ wt, method = "nn"), "No data")
  qsr_reset()
})

test_that("qsr_ml() fails without formula for supervised", {
  qsr_reset()
  qsr_data(mtcars)
  expect_error(qsr_ml(method = "nn"), "formula")
  qsr_reset()
})

# ---- qsr_cv() ----

test_that("qsr_cv() works with decision tree", {
  skip_if_not_installed("rpart")
  qsr_reset()
  qsr_data(mtcars)
  result <- qsr_cv(mpg ~ wt + hp, method = "tree", k = 3)
  expect_true(!is.null(result$rmse))
  expect_true(result$rmse > 0)
  qsr_reset()
})

test_that("qsr_cv() works with neural network", {
  qsr_reset()
  qsr_data(mtcars)
  result <- qsr_cv(mpg ~ wt + hp, method = "nn", k = 3, size = 3)
  expect_true(!is.null(result$rmse))
  qsr_reset()
})

# ---- qsr_compare() ----

test_that("qsr_compare() compares multiple models", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt, name = "m1")
  qsr_model(mpg ~ wt + hp, name = "m2")
  result <- qsr_compare()
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("r_squared" %in% names(result))
  expect_true("aic" %in% names(result))
  qsr_reset()
})

test_that("qsr_compare() fails without models", {
  qsr_reset()
  expect_error(qsr_compare(), "No models")
  qsr_reset()
})
