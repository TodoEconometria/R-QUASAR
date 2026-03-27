# ============================================================
# QUASAR — Tests for Survey Tools (qsr_normalize_text,
#   qsr_crosswalk, qsr_currency, qsr_validate, qsr_decision_log)
# ============================================================

# ---- qsr_normalize_text() ----

test_that("qsr_normalize_text() fuzzy matches typos", {
  qsr_reset()
  df <- data.frame(
    crop = c("coca", "cacao", "cacoa", "cafe", "kafe", "banana"),
    stringsAsFactors = FALSE
  )
  result <- qsr_normalize_text(
    data = df,
    column = "crop",
    dictionary = c("coca", "cacao", "cafe", "banana"),
    max_dist = 2,
    review = FALSE,
    register = FALSE
  )
  expect_true("crop_clean" %in% names(result))
  # "cacoa" should be fuzzy matched (dist 1 to cacao or coca)
  expect_true(result$crop_clean[3] %in% c("cacao", "coca"))
  # "kafe" should match "cafe" (dist 1)
  expect_equal(result$crop_clean[5], "cafe")
  qsr_reset()
})

test_that("qsr_normalize_text() auto-generates dictionary", {
  qsr_reset()
  df <- data.frame(
    name = c("coca", "coca", "coca", "cafe", "cafe", "cafe", "koca", "kafe"),
    stringsAsFactors = FALSE
  )
  result <- qsr_normalize_text(
    data = df, column = "name", min_freq = 3,
    max_dist = 1, review = FALSE, register = FALSE
  )
  expect_true("name_clean" %in% names(result))
  # "koca" (dist 1 from "coca") should be corrected
  expect_equal(result$name_clean[7], "coca")
  qsr_reset()
})

test_that("qsr_normalize_text() fails on missing column", {
  df <- data.frame(x = 1:3)
  expect_error(
    qsr_normalize_text(data = df, column = "nonexistent", register = FALSE),
    "not found"
  )
})

# ---- qsr_crosswalk() ----

test_that("qsr_crosswalk() translates codes", {
  qsr_reset()
  df <- data.frame(
    id = 1:4,
    geo = c("A1", "A2", "B1", "C1"),
    stringsAsFactors = FALSE
  )
  cw <- data.frame(old = c("A1", "A2"), new = c("AA", "AA"))
  result <- qsr_crosswalk(data = df, crosswalk = cw, column = "geo",
                           register = FALSE)
  expect_equal(result$geo_harmonized, c("AA", "AA", "B1", "C1"))
  qsr_reset()
})

test_that("qsr_crosswalk() respects year filter", {
  qsr_reset()
  df <- data.frame(
    geo = c("A1", "A1", "A1"),
    year = c(2015, 2017, 2019),
    stringsAsFactors = FALSE
  )
  cw <- data.frame(old = "A1", new = "AA")
  result <- qsr_crosswalk(data = df, crosswalk = cw, column = "geo",
                           year_column = "year", apply_before = 2017,
                           register = FALSE)
  # Only 2015 should be translated
  expect_equal(result$geo_harmonized, c("AA", "A1", "A1"))
  qsr_reset()
})

test_that("qsr_crosswalk() reads CSV crosswalk", {
  qsr_reset()
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(old = c("X", "Y"), new = c("XX", "YY")),
                    tmp, row.names = FALSE)
  df <- data.frame(code = c("X", "Y", "Z"), stringsAsFactors = FALSE)
  result <- qsr_crosswalk(data = df, crosswalk = tmp, column = "code",
                           register = FALSE)
  expect_equal(result$code_harmonized, c("XX", "YY", "Z"))
  unlink(tmp)
  qsr_reset()
})

# ---- qsr_currency() ----

test_that("qsr_currency() converts with custom rates", {
  qsr_reset()
  df <- data.frame(
    year = c(2015, 2015, 2016),
    income = c(6910, 13820, 6910)
  )
  # 1 USD = 6.91 BOB, deflator 100 for both years
  result <- qsr_currency(
    data = df, columns = "income", from_currency = "BOB",
    base_year = 2015, year_column = "year",
    exchange_rates = c("2015" = 6.91, "2016" = 6.91),
    deflator = c("2015" = 100, "2016" = 100),
    register = FALSE
  )
  expect_true("income_usd" %in% names(result))
  expect_equal(round(result$income_usd[1], 0), 1000)
  expect_equal(round(result$income_usd[2], 0), 2000)
  qsr_reset()
})

test_that("qsr_currency() uses built-in rates for BOB", {
  qsr_reset()
  df <- data.frame(year = 2015, income = 6910)
  result <- qsr_currency(
    data = df, columns = "income", from_currency = "BOB",
    base_year = 2015, register = FALSE
  )
  expect_true("income_usd" %in% names(result))
  expect_true(!is.na(result$income_usd[1]))
  expect_true(result$income_usd[1] > 0)
  qsr_reset()
})

test_that("qsr_currency() fails on unsupported currency", {
  df <- data.frame(year = 2015, income = 100)
  expect_error(
    qsr_currency(data = df, columns = "income", from_currency = "XYZ",
                  register = FALSE),
    "No built-in"
  )
})

# ---- qsr_validate() ----

test_that("qsr_validate() basic preset passes on clean data", {
  qsr_reset()
  df <- data.frame(a = 1:20, b = rnorm(20))
  result <- qsr_validate(data = df, checks = "basic")
  expect_true(result)
  qsr_reset()
})

test_that("qsr_validate() basic preset detects empty data", {
  qsr_reset()
  df <- data.frame(a = numeric(0))
  result <- qsr_validate(data = df, checks = "basic")
  expect_false(result)
  qsr_reset()
})

test_that("qsr_validate() basic preset detects all-NA columns", {
  qsr_reset()
  df <- data.frame(a = 1:10, b = rep(NA, 10))
  result <- qsr_validate(data = df, checks = "basic")
  expect_false(result)
  qsr_reset()
})

test_that("qsr_validate() custom checks work", {
  qsr_reset()
  df <- data.frame(id = c(1, 2, 2, 3), value = c(10, -5, 20, 30))
  checks <- list(
    "No duplicates" = function(d) {
      if (any(duplicated(d$id))) "Duplicates found" else TRUE
    },
    "All positive" = function(d) {
      if (any(d$value < 0)) "Negative values" else TRUE
    }
  )
  result <- qsr_validate(data = df, checks = checks)
  expect_false(result)  # Both checks fail
  qsr_reset()
})

test_that("qsr_validate() stop_on_fail aborts", {
  qsr_reset()
  df <- data.frame()
  expect_error(
    qsr_validate(data = df, checks = "basic", stop_on_fail = TRUE),
    "Validation failed"
  )
  qsr_reset()
})

test_that("qsr_validate() survey preset checks for weights", {
  qsr_reset()
  df <- data.frame(a = 1:20, b = rnorm(20))
  result <- qsr_validate(data = df, checks = "survey")
  expect_false(result)  # No weight column
  qsr_reset()
})

test_that("qsr_validate() panel preset checks for year", {
  qsr_reset()
  df <- data.frame(a = 1:20, weight = rep(1, 20))
  result <- qsr_validate(data = df, checks = "panel")
  expect_false(result)  # No year column
  qsr_reset()
})

# ---- qsr_decision_log() ----

test_that("qsr_decision_log() creates and appends to log file", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  qsr_decision_log(
    id = "TEST-001", title = "Test decision",
    variable = "test_var", waves = "2020",
    problem = "Test problem", evidence = "Test evidence",
    decision = "Test decision", log_file = tmp
  )

  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("TEST-001", content)))
  expect_true(any(grepl("Test problem", content)))

  # Append second entry
  qsr_decision_log(
    id = "TEST-002", title = "Second decision",
    variable = "var2", waves = "2021",
    problem = "Problem 2", evidence = "Evidence 2",
    decision = "Decision 2", log_file = tmp
  )

  content2 <- readLines(tmp)
  expect_true(any(grepl("TEST-002", content2)))
  expect_true(length(content2) > length(content))
})
