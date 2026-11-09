# ============================================================
# QUASAR - qsr_poverty() and qsr_inequality() validated against convey
# Synthetic stratified two-stage sample (no real microdata).
# ============================================================

make_poverty_design <- function(n = 600) {
  withr::with_seed(2026, {
    d <- data.frame(
      strat = rep(1:4, each = n / 4),
      psu   = rep(seq_len(n / 10), each = 10),
      w     = stats::runif(n, 20, 200),
      reg   = sample(c("N", "S", "E"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    d$y <- exp(stats::rnorm(n, 7 + 0.2 * d$strat, 0.7))
  })
  d$z <- ifelse(d$reg == "N", 900, 1300)
  d$ratio <- d$y / d$z
  survey::svydesign(ids = ~psu, strata = ~strat, weights = ~w,
                    data = d, nest = TRUE)
}

test_that("qsr_poverty() FGT 0/1/2 match convey::svyfgt (Taylor)", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  des <- make_poverty_design()
  cd <- convey::convey_prep(des)
  out <- qsr_poverty(des, income = "y", line = 1100)
  expect_equal(out$measure, c("headcount", "gap", "severity"))
  for (g in 0:2) {
    ref <- convey::svyfgt(~y, cd, g = g, abs_thresh = 1100)
    expect_equal(out$estimate[g + 1], as.numeric(coef(ref)), tolerance = 1e-10)
    expect_equal(out$se[g + 1], as.numeric(survey::SE(ref)), tolerance = 1e-10)
  }
  expect_true(all(out$ci_low <= out$estimate & out$estimate <= out$ci_high))
})

test_that("qsr_poverty() by domain matches svyby(..., convey::svyfgt)", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  des <- make_poverty_design()
  cd <- convey::convey_prep(des)
  for (g in 0:2) {
    out <- qsr_poverty(des, income = "y", line = 1100, by = "reg", alpha = g)
    ref <- survey::svyby(~y, ~reg, cd, convey::svyfgt, g = g, abs_thresh = 1100)
    o <- out[match(ref$reg, out$reg), ]
    expect_equal(o$estimate, as.numeric(coef(ref)), tolerance = 1e-10)
    expect_equal(o$se, as.numeric(survey::SE(ref)), tolerance = 1e-10)
  }
})

test_that("qsr_poverty() with a line per unit equals convey on y/z with z = 1", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  des <- make_poverty_design()
  cd <- convey::convey_prep(des)
  out <- qsr_poverty(des, income = "y", line = "z")
  vec <- qsr_poverty(des, income = "y", line = des$variables$z)
  expect_equal(out$estimate, vec$estimate)
  for (g in 0:2) {
    ref <- convey::svyfgt(~ratio, cd, g = g, abs_thresh = 1)
    expect_equal(out$estimate[g + 1], as.numeric(coef(ref)), tolerance = 1e-10)
    expect_equal(out$se[g + 1], as.numeric(survey::SE(ref)), tolerance = 1e-10)
  }
})

test_that("qsr_inequality() Gini/Theil/Atkinson match convey by domain", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  des <- make_poverty_design()
  cd <- convey::convey_prep(des)
  out <- qsr_inequality(des, income = "y", by = "reg", epsilon = 0.5,
                        measures = c("gini", "theil_t", "theil_l", "atkinson"))
  refs <- list(
    gini     = survey::svyby(~y, ~reg, cd, convey::svygini),
    theil_t  = survey::svyby(~y, ~reg, cd, convey::svygei, epsilon = 1),
    theil_l  = survey::svyby(~y, ~reg, cd, convey::svygei, epsilon = 0),
    atkinson = survey::svyby(~y, ~reg, cd, convey::svyatk, epsilon = 0.5)
  )
  for (m in names(refs)) {
    ref <- refs[[m]]
    o <- out[out$measure == m, ]
    o <- o[match(ref$reg, o$reg), ]
    expect_equal(o$estimate, as.numeric(coef(ref)), tolerance = 1e-10, label = m)
    expect_equal(o$se, as.numeric(survey::SE(ref)), tolerance = 1e-10, label = m)
  }
})

test_that("qsr_inequality() overall Gini and Atkinson(1) match convey", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  des <- make_poverty_design()
  cd <- convey::convey_prep(des)
  out <- qsr_inequality(des, income = "y", measures = c("gini", "atkinson"))
  g <- convey::svygini(~y, cd)
  a <- convey::svyatk(~y, cd, epsilon = 1)
  expect_equal(out$estimate, as.numeric(c(coef(g), coef(a))), tolerance = 1e-10)
  expect_equal(out$se, as.numeric(c(survey::SE(g), survey::SE(a))), tolerance = 1e-10)
})

test_that("replicate-weight designs match convey's svyrep methods", {
  skip_if_not_installed("survey")
  skip_if_not_installed("convey")
  rep <- convey::convey_prep(survey::as.svrepdesign(make_poverty_design(),
                                                     type = "JKn"))
  pov <- qsr_poverty(rep, income = "y", line = 1100, by = "reg", alpha = 0)
  ref <- survey::svyby(~y, ~reg, rep, convey::svyfgt, g = 0, abs_thresh = 1100)
  o <- pov[match(ref$reg, pov$reg), ]
  expect_equal(o$estimate, as.numeric(coef(ref)), tolerance = 1e-10)
  expect_equal(o$se, as.numeric(survey::SE(ref)), tolerance = 1e-10)

  ineq <- qsr_inequality(rep, income = "y", measures = c("gini", "theil_t"))
  g <- convey::svygini(~y, rep)
  t <- convey::svygei(~y, rep, epsilon = 1)
  expect_equal(ineq$estimate, as.numeric(c(coef(g), coef(t))), tolerance = 1e-10)
  expect_equal(ineq$se, as.numeric(c(survey::SE(g), survey::SE(t))), tolerance = 1e-10)
})

test_that("headcount equals the weighted mean of the poverty indicator", {
  skip_if_not_installed("survey")
  des <- make_poverty_design()
  des <- stats::update(des, poor = as.numeric(y <= 1100))
  ref <- survey::svymean(~poor, des)
  out <- qsr_poverty(des, income = "y", line = 1100, alpha = 0)
  expect_equal(out$estimate, as.numeric(coef(ref)), tolerance = 1e-12)
  expect_equal(out$se, as.numeric(survey::SE(ref)), tolerance = 1e-12)
})

test_that("qsr_poverty() uses the registered design and the bundled data", {
  skip_if_not_installed("survey")
  on.exit(qsr_reset(), add = TRUE)
  qsr_svydesign(qsr_survey, strata = "region", weights = "weight")
  out <- qsr_poverty(income = "ingreso_total", line = 1200, by = "region")
  expect_equal(nrow(out), 6 * 3)
  expect_true(all(out$estimate >= 0 & out$estimate <= 1))
  expect_true(all(out$se > 0))
  ineq <- qsr_inequality(income = "ingreso_total")
  expect_true(all(ineq$estimate > 0 & ineq$estimate < 1))
})

test_that("missing values, bad lines and non-positive incomes are handled", {
  skip_if_not_installed("survey")
  d <- qsr_survey
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  # ingreso_laboral has NA and non-positive values on purpose
  expect_no_error(qsr_poverty(des, income = "ingreso_laboral", line = 1000))
  expect_error(qsr_poverty(des, income = "ingreso_laboral", line = 1000,
                           na_rm = FALSE), "missing")
  expect_error(qsr_poverty(des, income = "ingreso_total", line = 0), "positive")
  expect_error(qsr_poverty(des, income = "ingreso_total", line = c(1, 2)),
               "line")
  expect_warning(qsr_inequality(des, income = "ingreso_laboral",
                                measures = "theil_t"), "income <= 0")
  expect_error(qsr_poverty(design = "x", income = "y", line = 1), "design")
})
