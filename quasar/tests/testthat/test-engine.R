# ============================================================
# QUASAR — Tests for Rust/Polars Engine (engine.R)
# ============================================================

# -- Helper: create a temp CSV for testing --
.make_test_csv <- function(n = 1000) {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    id    = seq_len(n),
    value = rnorm(n, mean = 50, sd = 10),
    group = sample(c("A", "B", "C"), n, replace = TRUE),
    flag  = sample(c(TRUE, FALSE), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  write.csv(df, tmp, row.names = FALSE)
  tmp
}

# ============================================================
# qsr_read() — CSV reading
# ============================================================

test_that("qsr_read() reads a CSV file correctly", {
  tmp <- .make_test_csv(500)
  on.exit(unlink(tmp))

  df <- qsr_read(tmp, register = FALSE)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 500)
  expect_true("id" %in% names(df))
  expect_true("value" %in% names(df))
  expect_true("group" %in% names(df))
})

test_that("qsr_read() respects n_rows", {
  tmp <- .make_test_csv(1000)
  on.exit(unlink(tmp))

  df <- qsr_read(tmp, n_rows = 100, register = FALSE)
  expect_equal(nrow(df), 100)
})

test_that("qsr_read() auto-registers in context", {
  qsr_reset()
  tmp <- .make_test_csv(50)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  qsr_read(tmp, name = "test_data", register = TRUE)
  ctx_data <- .qsr_context$get_data()
  expect_true(is.data.frame(ctx_data))
  expect_equal(nrow(ctx_data), 50)
})

test_that("qsr_read() auto-detects format from .csv extension", {
  tmp <- .make_test_csv(100)
  on.exit(unlink(tmp))

  # format = "auto" should detect csv
  df <- qsr_read(tmp, format = "auto", register = FALSE)
  expect_equal(nrow(df), 100)
})

test_that("qsr_read() fails on non-existent file", {
  expect_error(qsr_read("nonexistent_file.csv"), "not found")
})

test_that("qsr_read() honours a custom delimiter", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("a|b|c", "1|2|3", "4|5|6"), tmp)

  df <- qsr_read(tmp, delim = "|", register = FALSE)
  expect_equal(ncol(df), 3L)
  expect_equal(names(df), c("a", "b", "c"))
  expect_equal(nrow(df), 2L)
})

test_that("qsr_read() transcodes a latin1 file, preserving accents", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  con <- file(tmp, "w", encoding = "latin1")
  writeLines(c("prov|n", "A CORUÑA|1", "LEÓN|2"), con)
  close(con)

  df <- qsr_read(tmp, delim = "|", encoding = "latin1", register = FALSE)
  expect_true(any(grepl("CORUÑA", df$prov)))   # n-tilde survived
  expect_false(any(grepl("�", df$prov)))         # no replacement char
})

test_that("qsr_read() projection returns only requested columns", {
  tmp <- .make_test_csv(200)
  on.exit(unlink(tmp))

  df <- qsr_read(tmp, columns = c("id", "group"), register = FALSE)
  expect_equal(sort(names(df)), c("group", "id"))
})

test_that("qsr_sink_parquet() streams a delimited file to Parquet", {
  src <- tempfile(fileext = ".csv")
  out <- tempfile(fileext = ".parquet")
  on.exit(unlink(c(src, out)))
  writeLines(c("a|b|c", "1|x|10", "2|y|20", "3|z|30"), src)

  qsr_sink_parquet(src, out, delim = "|", columns = c("a", "c"))
  expect_true(file.exists(out))

  back <- qsr_read(out, register = FALSE)
  expect_equal(sort(names(back)), c("a", "c"))
  expect_equal(nrow(back), 3L)
})

test_that("qsr_read() fails on unknown extension with auto format", {
  tmp <- tempfile(fileext = ".xyz")
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_error(qsr_read(tmp, format = "auto"), "auto-detect")
})

# ============================================================
# qsr_fast_summary() — Descriptive stats
# ============================================================

test_that("qsr_fast_summary() returns correct structure", {
  tmp <- .make_test_csv(200)
  on.exit(unlink(tmp))

  result <- qsr_fast_summary(tmp)
  expect_true(is.data.frame(result))
  expect_true("column" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("std" %in% names(result))
  expect_true("min" %in% names(result))
  expect_true("max" %in% names(result))
  expect_true("count" %in% names(result))
  # Should have at least id and value as numeric columns
  expect_true(nrow(result) >= 2)
})

test_that("qsr_fast_summary() fails on non-existent file", {
  expect_error(qsr_fast_summary("nope.csv"), "not found")
})

# ============================================================
# qsr_fast_group() — Group-by aggregation
# ============================================================

test_that("qsr_fast_group() computes mean by group", {
  tmp <- .make_test_csv(300)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  df <- qsr_fast_group(tmp, by = "group", col = "value", fn = "mean",
                        register = FALSE)
  expect_true(is.data.frame(df))
  expect_true("group" %in% names(df))
  expect_true("value_mean" %in% names(df))
  # Should have 3 groups: A, B, C
  expect_equal(nrow(df), 3)
})

test_that("qsr_fast_group() supports all aggregation functions", {
  tmp <- .make_test_csv(200)
  on.exit(unlink(tmp))

  for (fn in c("mean", "sum", "count", "min", "max", "std")) {
    df <- qsr_fast_group(tmp, by = "group", col = "value", fn = fn,
                          register = FALSE)
    expected_col <- paste0("value_", fn)
    expect_true(expected_col %in% names(df),
                info = paste("Missing column for fn:", fn))
  }
})

test_that("qsr_fast_group() auto-registers in context", {
  qsr_reset()
  tmp <- .make_test_csv(100)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  qsr_fast_group(tmp, by = "group", col = "value", fn = "sum",
                  name = "grouped", register = TRUE)
  ctx_data <- .qsr_context$get_data()
  expect_true(is.data.frame(ctx_data))
})

test_that("qsr_fast_group() fails on non-existent file", {
  expect_error(qsr_fast_group("nope.csv", by = "x", col = "y"), "not found")
})

# ============================================================
# qsr_fast_filter() — Filtering
# ============================================================

test_that("qsr_fast_filter() filters rows correctly", {
  tmp <- .make_test_csv(500)
  on.exit(unlink(tmp))

  df <- qsr_fast_filter(tmp, col = "id", op = "lte", value = 100,
                         register = FALSE)
  expect_true(is.data.frame(df))
  expect_true(all(df$id <= 100))
  expect_equal(nrow(df), 100)
})

test_that("qsr_fast_filter() supports all operators", {
  tmp <- .make_test_csv(100)
  on.exit(unlink(tmp))

  for (op in c("gt", "lt", "gte", "lte", "eq", "neq")) {
    df <- qsr_fast_filter(tmp, col = "id", op = op, value = 50,
                           register = FALSE)
    expect_true(is.data.frame(df), info = paste("Failed for op:", op))
  }
})

test_that("qsr_fast_filter() auto-registers in context", {
  qsr_reset()
  tmp <- .make_test_csv(100)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  qsr_fast_filter(tmp, col = "id", op = "gt", value = 50,
                   name = "filtered", register = TRUE)
  ctx_data <- .qsr_context$get_data()
  expect_true(is.data.frame(ctx_data))
  expect_true(all(ctx_data$id > 50))
})

test_that("qsr_fast_filter() fails on non-existent file", {
  expect_error(qsr_fast_filter("nope.csv", col = "x", op = "gt", value = 1),
               "not found")
})

# ============================================================
# qsr_fast_sort() — Sorting
# ============================================================

test_that("qsr_fast_sort() sorts ascending by default", {
  tmp <- .make_test_csv(200)
  on.exit(unlink(tmp))

  df <- qsr_fast_sort(tmp, by = "value", register = FALSE)
  expect_true(is.data.frame(df))
  expect_true(all(diff(df$value) >= 0))
})

test_that("qsr_fast_sort() sorts descending when desc = TRUE", {
  tmp <- .make_test_csv(200)
  on.exit(unlink(tmp))

  df <- qsr_fast_sort(tmp, by = "value", desc = TRUE, register = FALSE)
  expect_true(all(diff(df$value) <= 0))
})

test_that("qsr_fast_sort() auto-registers in context", {
  qsr_reset()
  tmp <- .make_test_csv(50)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  qsr_fast_sort(tmp, by = "id", name = "sorted", register = TRUE)
  ctx_data <- .qsr_context$get_data()
  expect_true(is.data.frame(ctx_data))
})

test_that("qsr_fast_sort() fails on non-existent file", {
  expect_error(qsr_fast_sort("nope.csv", by = "x"), "not found")
})

# ============================================================
# Multi-call stability (segfault regression test)
# ============================================================

test_that("multiple Rust functions can be called in the same session", {
  tmp <- .make_test_csv(500)
  on.exit({
    unlink(tmp)
    qsr_reset()
  })

  # This sequence used to segfault before the thread pool fix
  df1 <- qsr_read(tmp, register = FALSE)
  expect_equal(nrow(df1), 500)

  summary <- qsr_fast_summary(tmp)
  expect_true(is.data.frame(summary))

  df2 <- qsr_fast_filter(tmp, col = "id", op = "lte", value = 100,
                           register = FALSE)
  expect_equal(nrow(df2), 100)

  df3 <- qsr_fast_group(tmp, by = "group", col = "value", fn = "mean",
                          register = FALSE)
  expect_equal(nrow(df3), 3)

  df4 <- qsr_fast_sort(tmp, by = "value", register = FALSE)
  expect_equal(nrow(df4), 500)
})

# ============================================================
# Adaptive process isolation
# ============================================================

test_that("adaptive call uses direct mode for small files", {
  tmp <- .make_test_csv(100)
  on.exit(unlink(tmp))

  # File is < 10MB, should use direct call (no subprocess)
  file_size <- file.info(tmp)$size
  expect_true(file_size < .QSR_PARALLEL_THRESHOLD)

  df <- qsr_read(tmp, register = FALSE)
  expect_equal(nrow(df), 100)
})

test_that("isolated subprocess works when rquasar is installed", {
  skip_if_not_installed("callr")
  # callr subprocess needs rquasar formally installed (not just load_all)
  # Check if rquasar is in a library path (not just loaded via devtools)
  installed <- nzchar(system.file(package = "rquasar")) &&
    file.exists(file.path(system.file(package = "rquasar"), "libs"))
  skip_if_not(installed, "rquasar not formally installed (using load_all)")

  tmp <- .make_test_csv(100)
  on.exit(unlink(tmp))

  # rust_read_csv signature is (path, n_rows, separator, columns) - pass all four.
  result <- .qsr_run_isolated("rust_read_csv", list(tmp, NULL, ",", NULL), n_cores = 2L)
  expect_true(is.list(result))
  expect_true("_nrows" %in% names(result))
  expect_equal(result[["_nrows"]], 100L)
})

test_that("benchmark function runs without error", {
  tmp <- .make_test_csv(10000)
  on.exit(unlink(tmp))

  result <- qsr_benchmark(tmp, n_runs = 1L)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("polars_rust" %in% result$method)
  expect_true("base_r" %in% result$method)
  # speedup should be a positive number (or NaN for very fast runs)
  expect_true(is.numeric(result$speedup[1]))
})
