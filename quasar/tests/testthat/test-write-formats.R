# ============================================================
# QUASAR — qsr_write(): the write half of qsr_read()
# ============================================================

rt_df <- function() {
  data.frame(id = 1:5, grp = c("a", "b", "a", "b", "a"),
             val = c(1.5, 2.5, NA, 4, 5.5), stringsAsFactors = FALSE)
}

# helper: write then read back, expect 5 rows and >= 3 cols
expect_roundtrip <- function(ext, pkgs = character()) {
  for (p in pkgs) skip_if_not_installed(p)
  df <- rt_df()
  tf <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tf))
  qsr_write(df, tf)
  expect_true(file.exists(tf))
  back <- qsr_read(tf, register = FALSE)
  expect_equal(nrow(back), 5)
  expect_gte(ncol(back), 3)
}

test_that("qsr_write round-trips CSV", { expect_roundtrip("csv") })
test_that("qsr_write round-trips Parquet", { expect_roundtrip("parquet", "arrow") })
test_that("qsr_write round-trips SPSS .sav", { expect_roundtrip("sav", "haven") })
test_that("qsr_write round-trips Stata .dta", { expect_roundtrip("dta", "haven") })
test_that("qsr_write round-trips SAS .xpt", { expect_roundtrip("xpt", "haven") })
test_that("qsr_write round-trips Excel .xlsx", { expect_roundtrip("xlsx", c("writexl", "readxl")) })
test_that("qsr_write round-trips JSON", { expect_roundtrip("json", "jsonlite") })
test_that("qsr_write round-trips NDJSON", { expect_roundtrip("ndjson", "jsonlite") })
test_that("qsr_write round-trips Arrow", { expect_roundtrip("arrow", "arrow") })
test_that("qsr_write round-trips RDS", { expect_roundtrip("rds") })
test_that("qsr_write round-trips RData", { expect_roundtrip("RData") })

test_that("qsr_write refuses .sas7bdat with a helpful message", {
  tf <- tempfile(fileext = ".sas7bdat")
  expect_error(qsr_write(rt_df(), tf), "sas7bdat|xpt")
})

test_that("qsr_write errors when there is no data", {
  qsr_reset()
  expect_error(qsr_write(path = tempfile(fileext = ".csv")), "No data")
})
