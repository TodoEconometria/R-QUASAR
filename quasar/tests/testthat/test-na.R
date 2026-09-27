# ============================================================
# QUASAR — qsr_na(): recode survey missing codes to NA
# ============================================================

test_that("qsr_na recodes sentinels, ranges and blank strings", {
  d <- data.frame(
    edad    = c(30, 99, 45, -9, 99),
    ingreso = c(1000, 9999, 2000, 500, 9500),
    sexo    = c("H", "", "M", ".", "H"),
    stringsAsFactors = FALSE
  )
  out <- qsr_na(d, values = c(99, -9), ranges = list(c(9000, 9999)),
                na_strings = c("", "."), register = FALSE)
  expect_equal(sum(is.na(out$edad)), 3L)          # 99, -9, 99
  expect_equal(sum(is.na(out$ingreso)), 2L)        # 9999, 9500 in [9000, 9999]
  expect_equal(sum(is.na(out$sexo)), 2L)           # "" and "."
  expect_equal(out$edad[c(1, 3)], c(30, 45))       # untouched values survive
})

test_that("qsr_na restricts to `cols` when given", {
  d <- data.frame(a = c(99, 1), b = c(99, 2))
  out <- qsr_na(d, values = 99, cols = "a", register = FALSE)
  expect_true(is.na(out$a[1]))
  expect_equal(out$b[1], 99)                        # b left alone
})

test_that("qsr_na errors on an unknown column", {
  expect_error(qsr_na(data.frame(a = 1), values = 99, cols = "zzz", register = FALSE),
               "not found")
})
