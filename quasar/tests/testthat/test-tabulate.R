# ============================================================
# QUASAR — Tests for qsr_tabulate() (weighted survey tabulation)
# ============================================================

df <- data.frame(
  g = c("A", "A", "B"),
  w = c(2, 3, 5),
  v = c(10, 20, 40),
  stringsAsFactors = FALSE
)

test_that("qsr_tabulate() computes weighted count and prop", {
  r <- qsr_tabulate(df, by = "g", weight = "w", stat = "count")
  expect_equal(sort(r$estimate), c(5, 5))          # A: 2+3, B: 5
  expect_equal(sort(r$n), c(1, 2))                  # unweighted rows

  p <- qsr_tabulate(df, by = "g", weight = "w", stat = "prop")
  expect_equal(sort(p$estimate), c(0.5, 0.5))
})

test_that("qsr_tabulate() computes weighted sum and mean", {
  s <- qsr_tabulate(df, by = "g", weight = "w", measure = "v", stat = "sum")
  s <- s[order(s$g), ]
  expect_equal(s$estimate, c(80, 200))             # A: 2*10+3*20, B: 5*40

  m <- qsr_tabulate(df, by = "g", weight = "w", measure = "v", stat = "mean")
  m <- m[order(m$g), ]
  expect_equal(m$estimate, c(16, 40))              # 80/5, 200/5
})

test_that("qsr_tabulate() total (no by) and weight auto-detect", {
  tot <- qsr_tabulate(df, weight = "w", stat = "count")
  expect_equal(tot$estimate, 10)

  d2 <- df; names(d2)[2] <- "factor"               # common survey weight name
  r <- qsr_tabulate(d2, by = "g", stat = "count")  # auto-detects "factor"
  expect_equal(sort(r$estimate), c(5, 5))
})

test_that("qsr_tabulate() validates inputs", {
  expect_error(qsr_tabulate(df, by = "nope", weight = "w"), "not found")
  expect_error(qsr_tabulate(df, by = "g", weight = "bad"), "not found")
  expect_error(qsr_tabulate(df, by = "g", weight = "w", stat = "mean"), "measure")
})
