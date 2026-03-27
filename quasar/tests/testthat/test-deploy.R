# ============================================================
# QUASAR — Tests for Deploy & Data Sources
#   (qsr_deploy, qsr_search, qsr_guide, qsr_report)
# ============================================================

# ---- qsr_deploy() ----

test_that("qsr_deploy() generates Shiny app files", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt + hp, name = "test_model")

  tmp_dir <- file.path(tempdir(), "quasar_deploy_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  result <- qsr_deploy(type = "shiny", path = tmp_dir, launch = FALSE)

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(file.path(tmp_dir, "app.R")))
  expect_true(file.exists(file.path(tmp_dir, "quasar_context.rds")))

  # Check app.R content
  app_code <- readLines(file.path(tmp_dir, "app.R"))
  expect_true(any(grepl("library\\(shiny\\)", app_code)))
  expect_true(any(grepl("navbarPage", app_code)))
  expect_true(any(grepl("quasar_context.rds", app_code)))

  unlink(tmp_dir, recursive = TRUE)
  qsr_reset()
})

test_that("qsr_deploy() generates Plumber API files", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt, name = "api_model")

  tmp_dir <- file.path(tempdir(), "quasar_api_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  result <- qsr_deploy(type = "api", path = tmp_dir, launch = FALSE)

  expect_true(file.exists(file.path(tmp_dir, "plumber.R")))
  expect_true(file.exists(file.path(tmp_dir, "quasar_context.rds")))

  api_code <- readLines(file.path(tmp_dir, "plumber.R"))
  expect_true(any(grepl("library\\(plumber\\)", api_code)))
  expect_true(any(grepl("/health", api_code)))
  expect_true(any(grepl("/data", api_code)))

  unlink(tmp_dir, recursive = TRUE)
  qsr_reset()
})

test_that("qsr_deploy() fails with empty context", {
  qsr_reset()
  expect_error(qsr_deploy(), "Nothing to deploy")
  qsr_reset()
})

test_that("qsr_deploy() saves context correctly", {
  qsr_reset()
  qsr_data(iris)
  qsr_model(Sepal.Length ~ Sepal.Width, name = "iris_lm")

  tmp_dir <- file.path(tempdir(), "quasar_ctx_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  qsr_deploy(type = "shiny", path = tmp_dir, launch = FALSE)

  ctx <- readRDS(file.path(tmp_dir, "quasar_context.rds"))
  expect_equal(nrow(ctx$data), 150)
  expect_true("iris_lm" %in% names(ctx$models))

  unlink(tmp_dir, recursive = TRUE)
  qsr_reset()
})

# ---- qsr_search() ----

test_that("qsr_search() finds GDP in built-in catalog", {
  results <- qsr_search("gdp")
  expect_true(is.data.frame(results))
  expect_true(nrow(results) > 0)
  expect_true("source" %in% names(results))
  expect_true("id" %in% names(results))
})

test_that("qsr_search() filters by source", {
  results <- qsr_search("inflation", sources = "fred")
  expect_true(all(results$source == "fred"))
})

test_that("qsr_search() returns empty for nonsense query", {
  results <- qsr_search("xyznonexistent123")
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 0)
})

test_that("qsr_search() respects limit", {
  results <- qsr_search("inflation", sources = "fred", limit = 2)
  expect_true(nrow(results) <= 2)
})

# ---- qsr_guide() ----

test_that("qsr_guide() shows LAPOP guide", {
  expect_no_error(qsr_guide("lapop"))
})

test_that("qsr_guide() shows Colombia guide", {
  expect_no_error(qsr_guide("dane_colombia"))
})

test_that("qsr_guide() shows Bolivia guide with year", {
  expect_no_error(qsr_guide("ine_bolivia", year = 2023))
})

test_that("qsr_guide() shows Peru guide", {
  expect_no_error(qsr_guide("inei_peru"))
})

test_that("qsr_guide() fails on unknown source", {
  expect_error(qsr_guide("nonexistent_source"), "No guide")
})
