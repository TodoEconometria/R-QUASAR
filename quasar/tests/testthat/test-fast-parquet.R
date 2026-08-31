# ============================================================
# QUASAR — fast_* helpers on Parquet inputs (the Rust ops scan CSV; for
# Parquet we read columnar once and do the op in R). Group is covered in
# test-fast-group-parquet.R; here: filter, sort, summary.
# ============================================================

make_pq <- function() {
  skip_if_not_installed("arrow")
  set.seed(7)
  df <- data.frame(dep = rep(c("A", "B", "C"), each = 20),
                   sal = round(rnorm(60, 3000, 500)),
                   age = sample(20:60, 60, TRUE))
  tf <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, tf)
  list(path = tf, df = df)
}

test_that("qsr_fast_filter works on Parquet", {
  p <- make_pq(); on.exit(unlink(p$path))
  out <- qsr_fast_filter(p$path, col = "sal", op = "gt", value = 3000, register = FALSE)
  expect_equal(nrow(out), sum(p$df$sal > 3000))
  expect_true(all(out$sal > 3000))
})

test_that("qsr_fast_sort works on Parquet", {
  p <- make_pq(); on.exit(unlink(p$path))
  out <- qsr_fast_sort(p$path, by = "sal", desc = TRUE, register = FALSE)
  expect_equal(out$sal[1], max(p$df$sal))
  expect_false(is.unsorted(rev(out$sal)))
})

test_that("qsr_fast_summary works on Parquet", {
  p <- make_pq(); on.exit(unlink(p$path))
  out <- qsr_fast_summary(p$path)
  expect_equal(nrow(out), 2)                       # sal, age (numeric only)
  expect_setequal(out$column, c("sal", "age"))
  expect_equal(out$max[out$column == "sal"], max(p$df$sal))
})
