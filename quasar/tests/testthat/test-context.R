# ============================================================
# QUASAR — Tests for Context (qsr_config, qsr_get, qsr_data,
#   qsr_model, qsr_reset, qsr_init)
# ============================================================

# ---- qsr_config() ----

test_that("qsr_config() sets parameters in context", {
  qsr_reset()
  qsr_config(significance_level = 0.01, random_seed = 99)
  expect_equal(qsr_get("significance_level"), 0.01)
  expect_equal(qsr_get("random_seed"), 99)
  qsr_reset()
})

test_that("qsr_config() accepts extra parameters via ...", {
  qsr_reset()
  qsr_config(my_custom = "hello")
  expect_equal(qsr_get("my_custom"), "hello")
  qsr_reset()
})

test_that("qsr_config() loads from YAML file", {
  qsr_reset()
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    project = list(name = "test_proj"),
    analysis = list(significance_level = 0.10)
  ), tmp)
  qsr_config(config_path = tmp)
  expect_equal(qsr_get("name"), "test_proj")
  expect_equal(qsr_get("significance_level"), 0.05)  # overridden by default arg
  unlink(tmp)
  qsr_reset()
})

test_that("qsr_config() fails with bad config path", {
  qsr_reset()
  expect_error(qsr_config(config_path = "nonexistent.yml"), "not found")
  qsr_reset()
})

# ---- qsr_get() ----

test_that("qsr_get() returns default for missing key", {
  qsr_reset()
  expect_null(qsr_get("nonexistent"))
  expect_equal(qsr_get("nonexistent", default = 42), 42)
  qsr_reset()
})

test_that("qsr_get() retrieves set values", {
  qsr_reset()
  qsr_config(random_seed = 777)
  expect_equal(qsr_get("random_seed"), 777)
  qsr_reset()
})

# ---- qsr_data() ----

test_that("qsr_data() rejects non-data.frame", {
  expect_error(qsr_data(1:10), "data frame")
  expect_error(qsr_data("hello"), "data frame")
})

test_that("qsr_data() stores and retrieves from context", {
  qsr_reset()
  qsr_data(iris)
  expect_equal(nrow(.qsr_context$get_data()), 150)
  qsr_reset()
})

# ---- qsr_model() ----

test_that("qsr_model() fits lm from context", {
  qsr_reset()
  qsr_data(mtcars)
  m <- qsr_model(mpg ~ wt)
  expect_true(inherits(m, "lm"))
  expect_equal(length(coef(m)), 2)
  qsr_reset()
})

test_that("qsr_model() fits logit", {
  qsr_reset()
  qsr_data(mtcars)
  m <- qsr_model(am ~ wt + hp, type = "logit")
  expect_true(inherits(m, "glm"))
  expect_equal(m$family$link, "logit")
  qsr_reset()
})

test_that("qsr_model() fits probit", {
  qsr_reset()
  qsr_data(mtcars)
  m <- qsr_model(am ~ wt + hp, type = "probit")
  expect_equal(m$family$link, "probit")
  qsr_reset()
})

test_that("qsr_model() stores with name", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt, name = "base")
  qsr_model(mpg ~ wt + hp, name = "full")
  models <- .qsr_context$get_models()
  expect_true(all(c("base", "full") %in% names(models)))
  qsr_reset()
})

test_that("qsr_model() fails without data", {
  qsr_reset()
  expect_error(qsr_model(mpg ~ wt), "No data")
  qsr_reset()
})

# ---- qsr_reset() ----

test_that("qsr_reset() clears everything", {
  qsr_data(mtcars)
  qsr_config(random_seed = 123)
  qsr_reset()
  expect_null(.qsr_context$get_data())
  expect_null(qsr_get("random_seed"))
  expect_equal(length(.qsr_context$get_models()), 0)
})

# ---- qsr_init() ----

test_that("qsr_init() creates project structure", {
  tmp <- tempdir()
  proj_path <- file.path(tmp, "test_project")
  if (dir.exists(proj_path)) unlink(proj_path, recursive = TRUE)

  result <- qsr_init("test_project", path = tmp, author = "Test")
  expect_true(dir.exists(proj_path))
  expect_true(file.exists(file.path(proj_path, "main.R")))
  expect_true(file.exists(file.path(proj_path, "config", "default.yml")))
  expect_true(dir.exists(file.path(proj_path, "R", "01_data")))
  expect_true(dir.exists(file.path(proj_path, "tests")))
  expect_true(dir.exists(file.path(proj_path, "outputs", "tables")))

  unlink(proj_path, recursive = TRUE)
})

test_that("qsr_init() rejects bad names", {
  expect_error(qsr_init("Bad Name"), "snake_case")
  expect_error(qsr_init("123bad"), "snake_case")
  expect_error(qsr_init(""), "empty")
})

test_that("qsr_init() rejects existing directory", {
  tmp <- tempdir()
  proj_path <- file.path(tmp, "existing_project")
  dir.create(proj_path, showWarnings = FALSE)
  expect_error(qsr_init("existing_project", path = tmp), "already exists")
  unlink(proj_path, recursive = TRUE)
})
