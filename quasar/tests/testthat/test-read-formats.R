# ============================================================
# QUASAR — Tests for survey microdata readers
#   qsr_read_fwf() (fixed-width) and qsr_read() SPSS/Stata dispatch
# ============================================================

# ---- qsr_read_fwf() ----

test_that("qsr_read_fwf() reads by positions", {
  tf <- tempfile(fileext = ".txt")
  writeLines(c("00001ES12", "00042PT99"), tf)
  on.exit(unlink(tf))

  d <- qsr_read_fwf(
    tf,
    positions = list(c(1, 5), c(6, 7), c(8, 9)),
    col_names = c("id", "pais", "cod"),
    encoding = "utf8", register = FALSE
  )
  expect_s3_class(d, "data.frame")
  expect_equal(names(d), c("id", "pais", "cod"))
  expect_equal(d$id, c("00001", "00042"))
  expect_equal(d$pais, c("ES", "PT"))
  expect_equal(d$cod, c("12", "99"))
})

test_that("qsr_read_fwf() reads by widths and honours n_rows/trim", {
  tf <- tempfile(fileext = ".txt")
  writeLines(c("ab  12", "cd  34", "ef  56"), tf)
  on.exit(unlink(tf))

  d <- qsr_read_fwf(
    tf, widths = c(2, 4), col_names = c("a", "b"),
    encoding = "utf8", n_rows = 2, trim = TRUE, register = FALSE
  )
  expect_equal(nrow(d), 2)
  expect_equal(d$a, c("ab", "cd"))
  expect_equal(d$b, c("12", "34"))  # trimmed

  d2 <- qsr_read_fwf(
    tf, widths = c(2, 4), col_names = c("a", "b"),
    encoding = "utf8", trim = FALSE, register = FALSE
  )
  expect_equal(d2$b[1], "  12")  # untrimmed
})

test_that("qsr_read_fwf() errors on bad input", {
  tf <- tempfile(fileext = ".txt")
  writeLines("x", tf)
  on.exit(unlink(tf))

  expect_error(qsr_read_fwf(tf, register = FALSE), "positions|widths")
  expect_error(
    qsr_read_fwf(tf, widths = c(1), col_names = c("a", "b"), register = FALSE),
    "names"
  )
  expect_error(qsr_read_fwf("no_such_file.txt"), "not found")
})

# ---- qsr_read() SPSS / Stata dispatch (haven) ----

test_that("qsr_read() auto-detects and reads SPSS .sav", {
  skip_if_not_installed("haven")
  tf <- tempfile(fileext = ".sav")
  haven::write_sav(data.frame(x = 1:3, y = c("a", "b", "c")), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, register = FALSE)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 3)
  expect_equal(as.numeric(d$x), c(1, 2, 3))  # haven adds format attrs; compare values
})

test_that("qsr_read() reads a column subset from .sav", {
  skip_if_not_installed("haven")
  tf <- tempfile(fileext = ".sav")
  haven::write_sav(data.frame(a = 1:2, b = 3:4, c = 5:6), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, columns = c("a", "c"), register = FALSE)
  expect_equal(names(d), c("a", "c"))
})

test_that("qsr_read() auto-detects and reads Stata .dta", {
  skip_if_not_installed("haven")
  tf <- tempfile(fileext = ".dta")
  haven::write_dta(data.frame(x = 1:3), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, register = FALSE)
  expect_equal(nrow(d), 3)
})

test_that("qsr_read() auto-detects and reads SAS transport .xpt", {
  skip_if_not_installed("haven")
  tf <- tempfile(fileext = ".xpt")
  # SAS xpt names are limited to 8 chars; use a short one.
  haven::write_xpt(data.frame(x = 1:3), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, register = FALSE)
  expect_equal(nrow(d), 3)
  expect_equal(as.numeric(d$x), c(1, 2, 3))
})

test_that("qsr_read() reads Excel .xlsx", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  tf <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(x = 1:3, y = c("a", "b", "c")), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, register = FALSE)
  expect_equal(nrow(d), 3)
  expect_equal(as.numeric(d$x), c(1, 2, 3))

  d2 <- qsr_read(tf, n_rows = 2, register = FALSE)
  expect_equal(nrow(d2), 2)
})

test_that("qsr_read() reads dBase .dbf", {
  skip_if_not_installed("foreign")
  tf <- tempfile(fileext = ".dbf")
  foreign::write.dbf(data.frame(x = 1:2, g = c("a", "b")), tf)
  on.exit(unlink(tf))

  d <- qsr_read(tf, register = FALSE)
  expect_equal(nrow(d), 2)
  expect_true("g" %in% names(d))
})
