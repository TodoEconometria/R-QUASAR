# ============================================================
# QUASAR — qsr_fast_group() on Parquet input (in-memory path)
# ============================================================

test_that("qsr_fast_group() aggregates a Parquet file", {
  skip_if_not_installed("arrow")
  tf <- tempfile(fileext = ".parquet")
  arrow::write_parquet(
    data.frame(g = c("A", "A", "B"), v = c(10, 20, 40)),
    tf
  )
  on.exit(unlink(tf))

  s <- qsr_fast_group(tf, by = "g", col = "v", fn = "sum", register = FALSE)
  expect_s3_class(s, "data.frame")
  expect_true("v_sum" %in% names(s))
  s <- s[order(s$g), ]
  expect_equal(s$v_sum, c(30, 40))          # A: 10+20, B: 40

  m <- qsr_fast_group(tf, by = "g", col = "v", fn = "mean", register = FALSE)
  m <- m[order(m$g), ]
  expect_equal(m$v_mean, c(15, 40))

  n <- qsr_fast_group(tf, by = "g", col = "v", fn = "count", register = FALSE)
  n <- n[order(n$g), ]
  expect_equal(n$v_count, c(2L, 1L))
})

test_that("qsr_fast_group() errors on missing column in Parquet", {
  skip_if_not_installed("arrow")
  tf <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(g = "A", v = 1), tf)
  on.exit(unlink(tf))

  expect_error(
    qsr_fast_group(tf, by = "nope", col = "v", fn = "sum", register = FALSE),
    "not found"
  )
})
