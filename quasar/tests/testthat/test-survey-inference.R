# ============================================================
# QUASAR — Design-based survey inference
#   qsr_svydesign() + qsr_tabulate(se = TRUE) validated against survey::
# ============================================================

make_survey <- function(n = 400) {
  set.seed(42)
  data.frame(
    strat = rep(c("A", "B", "C", "D"), length.out = n),
    grp   = sample(c("x", "y"), n, replace = TRUE),
    y     = rbinom(n, 1, 0.3),
    val   = rnorm(n, 10, 3),
    w     = runif(n, 0.5, 4),
    stringsAsFactors = FALSE
  )
}

test_that("qsr_tabulate(se=TRUE) mean matches survey::svyby exactly", {
  skip_if_not_installed("survey")
  d <- make_survey()
  des <- survey::svydesign(ids = ~1, strata = ~strat, weights = ~w, data = d)
  ref <- as.data.frame(survey::svyby(~y, ~grp, des, survey::svymean, vartype = "se"))

  qsr_svydesign(data = d, strata = "strat", weights = "w")
  out <- qsr_tabulate(data = d, by = "grp", measure = "y", stat = "mean",
                      se = TRUE, strata = "strat", weight = "w")

  o <- out[match(ref$grp, out$grp), ]
  expect_equal(o$estimate, ref$y, tolerance = 1e-9)
  expect_equal(o$se,       ref$se, tolerance = 1e-9)
  expect_true(all(c("ci_low", "ci_high", "deff") %in% names(out)))
  expect_true(all(o$ci_low < o$estimate & o$estimate < o$ci_high))
})

test_that("qsr_tabulate(se=TRUE) prop matches survey::svymean of a factor", {
  skip_if_not_installed("survey")
  d <- make_survey()
  des <- survey::svydesign(ids = ~1, strata = ~strat, weights = ~w, data = d)
  ref <- survey::svymean(~factor(grp), des)
  refp <- as.numeric(coef(ref)); names(refp) <- sub("factor\\(grp\\)", "", names(coef(ref)))

  out <- qsr_tabulate(data = d, by = "grp", stat = "prop", se = TRUE,
                      strata = "strat", weight = "w")
  o <- out[match(names(refp), out$grp), ]
  expect_equal(o$estimate, unname(refp), tolerance = 1e-9)
  expect_equal(o$se, as.numeric(survey::SE(ref))[match(names(refp),
               sub("factor\\(grp\\)", "", names(coef(ref))))], tolerance = 1e-9)
  expect_equal(sum(out$estimate), 1, tolerance = 1e-9)  # proportions sum to 1
})

test_that("qsr_tabulate(se=TRUE) count returns weighted totals with SE", {
  skip_if_not_installed("survey")
  d <- make_survey()
  out <- qsr_tabulate(data = d, by = "grp", weight = "w", stat = "count",
                      se = TRUE, strata = "strat")
  # weighted totals sum to the total of the weights
  expect_equal(sum(out$estimate), sum(d$w), tolerance = 1e-6)
  expect_true(all(out$se > 0))
})

test_that("qsr_tabulate(se=TRUE) reuses a registered design when none is passed", {
  skip_if_not_installed("survey")
  d <- make_survey()
  des <- survey::svydesign(ids = ~1, strata = ~strat, weights = ~w, data = d)
  ref <- as.data.frame(survey::svyby(~y, ~grp, des, survey::svymean, vartype = "se"))

  qsr_data(d, name = "s")
  qsr_svydesign(strata = "strat", weights = "w")     # registered in context
  out <- qsr_tabulate(by = "grp", measure = "y", stat = "mean", se = TRUE)
  o <- out[match(ref$grp, out$grp), ]
  expect_equal(o$se, ref$se, tolerance = 1e-9)        # stratified SE, not weight-only
})

test_that("qsr_svydesign requires weights", {
  d <- make_survey()
  expect_error(qsr_svydesign(data = d), "weights")
})
