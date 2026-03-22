# ============================================================
# Tests for Layer 3 — Connectors
# ============================================================

# ---- .qsr_require() --------------------------------------------------------

test_that(".qsr_require() gives clear install message", {
  expect_error(
    .qsr_require("definitely_not_a_real_package_xyz", "for testing"),
    "Install it with"
  )
})

test_that(".qsr_require() passes for installed packages", {
  expect_no_error(.qsr_require("stats", "for testing"))
})

# ---- qsr_db() — SQLite (no server needed) ----------------------------------

test_that("qsr_db() connects to in-memory SQLite", {
  qsr_reset()
  result <- qsr_db("sqlite", .conn = TRUE)
  expect_true(inherits(result, "DBIConnection"))
  DBI::dbDisconnect(result)
  qsr_reset()
})

test_that("qsr_db() reads a table from SQLite", {
  qsr_reset()
  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbWriteTable(con, "cars", mtcars)
  DBI::dbDisconnect(con)
  on.exit(unlink(tmp))

  result <- qsr_db("sqlite", dbname = tmp, table = "cars")
  expect_equal(nrow(result), 32)
  expect_equal(ncol(result), 11)
  qsr_reset()
})

test_that("qsr_db() executes a query", {
  qsr_reset()
  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbWriteTable(con, "cars", mtcars)
  DBI::dbDisconnect(con)
  on.exit(unlink(tmp))

  result <- qsr_db("sqlite", dbname = tmp,
                    query = "SELECT * FROM cars WHERE mpg > 30")
  expect_true(nrow(result) > 0)
  expect_true(all(result$mpg > 30))
  qsr_reset()
})

test_that("qsr_db() auto-registers data in context", {
  qsr_reset()
  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbWriteTable(con, "cars", mtcars)
  DBI::dbDisconnect(con)
  on.exit(unlink(tmp))

  qsr_db("sqlite", dbname = tmp, table = "cars")
  ctx_data <- .qsr_context$get_data()
  expect_equal(nrow(ctx_data), 32)
  qsr_reset()
})

test_that("qsr_db() with .conn = TRUE stores connection in context", {
  qsr_reset()
  qsr_db("sqlite", .conn = TRUE)
  conn <- .qsr_context$get_connection("db")
  expect_true(!is.null(conn))
  expect_true(inherits(conn, "DBIConnection"))
  qsr_reset()
})

test_that("qsr_db() fails when both query and table given", {
  expect_error(
    qsr_db("sqlite", query = "SELECT 1", table = "x"),
    "not both"
  )
})

test_that("qsr_db() without query/table just connects", {
  qsr_reset()
  result <- qsr_db("sqlite")
  expect_true(inherits(result, "DBIConnection"))
  qsr_reset()
})

# ---- qsr_db() + downstream pipeline ----------------------------------------

test_that("qsr_db() → qsr_model() → qsr_table() pipeline works", {
  qsr_reset()
  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbWriteTable(con, "cars", mtcars)
  DBI::dbDisconnect(con)
  on.exit(unlink(tmp))

  qsr_db("sqlite", dbname = tmp, table = "cars")
  qsr_model(mpg ~ wt + hp)
  tbl <- qsr_table()
  expect_s3_class(tbl, "gt_tbl")
  qsr_reset()
})

# ---- qsr_fetch() -----------------------------------------------------------

test_that("qsr_fetch() fails with unknown source", {
  expect_error(qsr_fetch("nonexistent_source"), "not recognized")
})

test_that("qsr_fetch() shows available sources on error", {
  expect_error(qsr_fetch("xxx"), "worldbank")
})

# ---- qsr_spark() -----------------------------------------------------------

test_that("qsr_spark() fails gracefully without sparklyr", {
  skip_if(requireNamespace("sparklyr", quietly = TRUE))
  expect_error(qsr_spark(), "sparklyr")
})

# ---- qsr_python() ----------------------------------------------------------

test_that("qsr_python() fails gracefully without reticulate", {
  skip_if(requireNamespace("reticulate", quietly = TRUE))
  expect_error(qsr_python("numpy"), "reticulate")
})

# ---- Context cleanup -------------------------------------------------------

test_that("qsr_reset() cleans up database connections", {
  qsr_reset()
  qsr_db("sqlite", .conn = TRUE)
  expect_true(!is.null(.qsr_context$get_connection("db")))
  qsr_reset()
  expect_true(is.null(.qsr_context$get_connection("db")))
})

# ---- Cache helpers ----------------------------------------------------------

test_that("cache write and read round-trips", {
  tmp_cache <- file.path(tempdir(), "outputs", ".cache")
  dir.create(tmp_cache, recursive = TRUE, showWarnings = FALSE)

  key <- "test_cache_key"
  test_df <- data.frame(a = 1:3, b = c("x", "y", "z"))

  # Write
  cache_file <- file.path(tmp_cache, paste0(key, ".rds"))
  saveRDS(test_df, cache_file)

  # Read
  result <- readRDS(cache_file)
  expect_equal(result, test_df)

  unlink(tmp_cache, recursive = TRUE)
})

test_that("digest_simple() produces consistent hashes", {
  h1 <- digest_simple("hello world")
  h2 <- digest_simple("hello world")
  h3 <- digest_simple("different string")
  expect_equal(h1, h2)
  expect_false(h1 == h3)
})
