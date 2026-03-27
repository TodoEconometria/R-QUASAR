# ============================================================
# QUASAR — Tests for AI Module & Research Functions
#   (qsr_ai_*, qsr_load_folder, qsr_panel)
# ============================================================

# ---- qsr_ai_review() — error paths ----

test_that("qsr_ai_review() fails without API key", {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  expect_error(
    qsr_ai_review("a.pdf", "b.pdf"),
    "API key"
  )
})

# ---- qsr_ai_classify() — error paths ----

test_that("qsr_ai_classify() fails without API key", {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  df <- data.frame(text = c("coca", "cafe"))
  expect_error(
    qsr_ai_classify(df, "text", c("coca", "coffee")),
    "API key"
  )
})

test_that("qsr_ai_classify() fails on missing column", {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "test_key")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  df <- data.frame(text = c("a", "b"))
  expect_error(
    qsr_ai_classify(df, "nonexistent", c("a", "b")),
    "not found"
  )
})

# ---- qsr_ai_flag() — error paths ----

test_that("qsr_ai_flag() fails without API key", {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  df <- data.frame(x = 1:10)
  expect_error(
    qsr_ai_flag(df),
    "API key"
  )
})

# ---- qsr_load_folder() ----

test_that("qsr_load_folder() reads CSV files from folder", {
  qsr_reset()
  tmp_dir <- file.path(tempdir(), "quasar_folder_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create test CSVs
  write.csv(data.frame(id = 1:5, val = rnorm(5)),
            file.path(tmp_dir, "data1.csv"), row.names = FALSE)
  write.csv(data.frame(id = 6:10, val = rnorm(5)),
            file.path(tmp_dir, "data2.csv"), row.names = FALSE)

  results <- qsr_load_folder(tmp_dir, register = FALSE)
  expect_true(is.list(results))
  expect_equal(length(results), 2)
  expect_true(all(c("data1", "data2") %in% names(results)))
  qsr_reset()
})

test_that("qsr_load_folder() stacks files", {
  qsr_reset()
  tmp_dir <- file.path(tempdir(), "quasar_stack_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  write.csv(data.frame(id = 1:3, val = 1:3),
            file.path(tmp_dir, "a.csv"), row.names = FALSE)
  write.csv(data.frame(id = 4:6, val = 4:6),
            file.path(tmp_dir, "b.csv"), row.names = FALSE)

  result <- qsr_load_folder(tmp_dir, stack = TRUE, register = FALSE)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 6)
  expect_true("_source" %in% names(result))
  qsr_reset()
})

test_that("qsr_load_folder() filters by pattern", {
  qsr_reset()
  tmp_dir <- file.path(tempdir(), "quasar_pattern_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  write.csv(data.frame(x = 1), file.path(tmp_dir, "persona_2020.csv"),
            row.names = FALSE)
  write.csv(data.frame(x = 1), file.path(tmp_dir, "vivienda_2020.csv"),
            row.names = FALSE)

  results <- qsr_load_folder(tmp_dir, pattern = "persona", register = FALSE)
  expect_equal(length(results), 1)
  expect_true("persona_2020" %in% names(results))
  qsr_reset()
})

test_that("qsr_load_folder() fails on non-existent path", {
  expect_error(qsr_load_folder("/nonexistent/path"), "not found")
})

test_that("qsr_load_folder() fails on empty folder", {
  tmp_dir <- file.path(tempdir(), "quasar_empty_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(qsr_load_folder(tmp_dir), "No data files")
})

# ---- qsr_panel() ----

test_that("qsr_panel() merges two data frames", {
  qsr_reset()
  df1 <- data.frame(id = 1:5, year = 2020, income = rnorm(5, 1000, 100))
  df2 <- data.frame(id = 1:5, year = 2020, education = sample(1:5, 5))

  result <- qsr_panel(df1, df2, join_by = c("id", "year"), register = FALSE)
  expect_true(is.data.frame(result))
  expect_true(all(c("income", "education") %in% names(result)))
  expect_equal(nrow(result), 5)
  qsr_reset()
})

test_that("qsr_panel() merges named sources", {
  qsr_reset()
  df1 <- data.frame(id = 1:3, x = 10:12)
  df2 <- data.frame(id = 1:3, y = 20:22)

  result <- qsr_panel(
    survey = df1, census = df2,
    join_by = "id", register = FALSE
  )
  expect_true(all(c("x", "y") %in% names(result)))
  qsr_reset()
})

test_that("qsr_panel() fails without join_by", {
  df1 <- data.frame(id = 1:3)
  df2 <- data.frame(id = 1:3)
  expect_error(qsr_panel(df1, df2), "join_by")
})
