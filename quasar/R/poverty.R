# ============================================================
# QUASAR - Poverty and inequality with design-based inference
# qsr_poverty(), qsr_inequality()
#
# Point estimates and standard errors follow the estimators and the
# linearization (influence-function) variables of the 'convey' package
# (Pessoa, Damico and Jacob, 2024): Taylor designs use survey::svyrecvar()
# on the linearized variable, replicate designs recompute the statistic with
# every replicate weight and use survey::svrVar(). The tests check both
# against convey.
# ============================================================


#' Poverty measures (FGT) with design-based standard errors
#'
#' Estimates the Foster-Greer-Thorbecke family of poverty measures
#' \deqn{FGT_\alpha = \frac{\sum_i w_i \left(\frac{z_i - y_i}{z_i}\right)^\alpha
#'   1(y_i \le z_i)}{\sum_i w_i}}{FGT(alpha) = sum w ((z - y)/z)^alpha 1(y <= z) / sum w}
#' from a complex survey design: \eqn{\alpha = 0} is the **headcount ratio**
#' (incidence: share of people below the line), \eqn{\alpha = 1} the **poverty
#' gap** (average shortfall as a share of the line) and \eqn{\alpha = 2} the
#' **severity** (squared gap, which weighs the poorest more). The line \eqn{z} can
#' be one number or vary by unit (a column with a line per household, year or
#' region, e.g. urban/rural lines or lines in current prices).
#'
#' Standard errors are design-based: Taylor linearization for designs from
#' [qsr_svydesign()] / [survey::svydesign()] (strata, clusters, finite population
#' correction and post-stratification are honoured), or replicate weights for
#' [survey::svrepdesign()] designs. Domains (`by`) are estimated against the full
#' design, so their variance is correct for subpopulations.
#'
#' @param design A `survey.design` or `svyrep.design` object (from
#'   [qsr_svydesign()] or the 'survey' package). NULL (default) reuses the design
#'   registered by [qsr_svydesign()].
#' @param income Character. Name of the income or consumption variable (the
#'   welfare measure, per capita or equivalised as the poverty line requires).
#' @param line Poverty line \eqn{z}: a positive number, the name of a column of
#'   the design data with one line per unit, or a numeric vector with one value
#'   per row of the design data.
#' @param alpha Numeric vector of non-negative FGT parameters. Default
#'   `c(0, 1, 2)` (headcount, gap and severity).
#' @param by Character vector of domain variables (e.g. `"region"` or
#'   `c("region", "year")`). NULL (default) estimates for the whole population.
#' @param ci_level Confidence level. Default 0.95. Intervals are
#'   \eqn{\hat\theta \pm t_{df} \cdot SE}{estimate +/- t(df) * SE} with the design
#'   degrees of freedom ([survey::degf()]) and truncated to \eqn{[0, 1]}.
#' @param na_rm Logical. Drop units with a missing income or line (they get
#'   weight zero, as in `convey`). If FALSE and there are missing values the
#'   function stops. Default TRUE.
#' @param digits Integer or NULL. Round `estimate`, `se`, `ci_low` and `ci_high`.
#'
#' @return A data.frame with one row per domain and value of `alpha`: the `by`
#'   columns (or `domain = "Total"`), `measure` (`"headcount"`, `"gap"`,
#'   `"severity"` or `"fgt<alpha>"`), `alpha`, `n` (unweighted units used),
#'   `estimate`, `se`, `ci_low` and `ci_high`.
#'
#' @references
#' Foster, J., Greer, J. and Thorbecke, E. (1984). A class of decomposable
#' poverty measures. *Econometrica*, 52(3), 761-766. \doi{10.2307/1913475}
#'
#' Pessoa, D. G. C., Damico, A. and Jacob, G. (2024). *Poverty and Inequality
#' with Complex Survey Data*. <https://www.convey-r.org/>
#'
#' @seealso [qsr_inequality()], [qsr_svydesign()], [qsr_tabulate()].
#'
#' @examples
#' d <- qsr_svydesign(qsr_survey, strata = "region", weights = "weight",
#'                    register = FALSE)
#' # Headcount, gap and severity with a single line
#' qsr_poverty(d, income = "ingreso_total", line = 1200)
#'
#' # Headcount by region
#' qsr_poverty(d, income = "ingreso_total", line = 1200, alpha = 0,
#'             by = "region")
#'
#' # A line that changes every year (current prices)
#' qsr_survey$z <- 1000 * 1.03^(qsr_survey$year - 2010)
#' d2 <- qsr_svydesign(qsr_survey, strata = "region", weights = "weight",
#'                     register = FALSE)
#' qsr_poverty(d2, income = "ingreso_total", line = "z", by = "year",
#'             alpha = 0)
#'
#' @export
qsr_poverty <- function(design = NULL,
                        income,
                        line,
                        alpha = c(0, 1, 2),
                        by = NULL,
                        ci_level = 0.95,
                        na_rm = TRUE,
                        digits = NULL) {
  design <- .qsr_resolve_design(design)
  vars <- design$variables
  y <- .qsr_svy_numeric(vars, income, "income")
  z <- .qsr_svy_line(vars, line)
  if (!is.numeric(alpha) || !length(alpha) || anyNA(alpha) || any(alpha < 0)) {
    cli::cli_abort("{.arg alpha} must be a vector of non-negative numbers.")
  }
  if (any(z[!is.na(z)] <= 0)) {
    cli::cli_abort("The poverty {.arg line} must be strictly positive.")
  }

  w0 <- .qsr_base_weights(design)
  miss <- is.na(y) | is.na(z)
  if (any(miss & w0 > 0) && !na_rm) {
    cli::cli_abort(c(
      "{.val {sum(miss & w0 > 0)}} unit{?s} with missing {.arg income} or {.arg line}.",
      "i" = "Use {.code na_rm = TRUE} to drop them."
    ))
  }
  w0[miss] <- 0
  y[miss] <- 0
  z[miss] <- 1

  doms <- .qsr_domains(vars, by)
  rows <- list()
  for (a in alpha) {
    h <- ((z - y) / z)^a * (y <= z)
    est_fun <- function(w) sum(w * h) / sum(w)
    lin_fun <- function(w) {
      N <- sum(w)
      ifelse(w > 0, (h - sum(w * h) / N) / N, 0)
    }
    lab <- switch(as.character(a), "0" = "headcount", "1" = "gap",
                  "2" = "severity", paste0("fgt", a))
    for (k in seq_along(doms$masks)) {
      st <- .qsr_svy_stat(design, w0 * doms$masks[[k]], est_fun, lin_fun)
      rows[[length(rows) + 1L]] <- data.frame(
        doms$keys[k, , drop = FALSE], measure = lab, alpha = a,
        n = st[["n"]], estimate = st[["estimate"]], se = st[["se"]],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }
  out <- .qsr_svy_finish(rows, design, ci_level, digits, bounds = c(0, 1))
  cli::cli_alert_success(
    "FGT poverty ({paste(unique(out$measure), collapse = ', ')}) over {.val {sum(w0 > 0)}} units {.emph (design-based SE)}."
  )
  out
}


#' Inequality measures (Gini, Theil, Atkinson) with design-based standard errors
#'
#' Estimates income inequality from a complex survey design: the **Gini**
#' coefficient, the **Theil T** index (generalized entropy with
#' \eqn{\epsilon = 1}), the **Theil L** index or mean log deviation (generalized
#' entropy with \eqn{\epsilon = 0}) and, optionally, the **Atkinson** index with
#' inequality aversion `epsilon`. Standard errors use linearization
#' (influence functions; Langel and Tille, 2013, for the Gini) for Taylor
#' designs, or the replicate weights of a [survey::svrepdesign()] design.
#' Domains (`by`) are estimated against the full design.
#'
#' The estimators are the ones of the 'convey' package: the Gini is
#' \eqn{G = \frac{\sum_i (2 r_i - 1) w_i y_i}{N Y} - 1}{G = sum((2 r - 1) w y) / (N Y) - 1},
#' with \eqn{r_i} the cumulative weight of the units sorted by income,
#' \eqn{N = \sum w_i} and \eqn{Y = \sum w_i y_i}; Theil and Atkinson are the
#' usual weighted plug-in estimators.
#'
#' @inheritParams qsr_poverty
#' @param measures Character vector with any of `"gini"`, `"theil_t"`,
#'   `"theil_l"` and `"atkinson"`. Default `c("gini", "theil_t")`.
#' @param epsilon Inequality aversion of the Atkinson index (positive). Default
#'   1. Only used when `"atkinson"` is requested.
#' @param ci_level Confidence level. Default 0.95. Intervals are
#'   \eqn{\hat\theta \pm t_{df} \cdot SE}{estimate +/- t(df) * SE} with the design
#'   degrees of freedom ([survey::degf()]).
#' @param na_rm Logical. Drop units with a missing income (weight zero). If FALSE
#'   and there are missing values the function stops. Default TRUE.
#'
#' @details Theil and Atkinson indices are defined only for strictly positive
#'   incomes: units with income \eqn{\le 0} are left out of those measures with a
#'   warning ('convey' stops with an error instead). The Gini keeps them.
#'
#' @return A data.frame with one row per domain and measure: the `by` columns
#'   (or `domain = "Total"`), `measure`, `n` (unweighted units used),
#'   `estimate`, `se`, `ci_low` and `ci_high`.
#'
#' @references
#' Langel, M. and Tille, Y. (2013). Variance estimation of the Gini index:
#' revisiting a result several times published. *Journal of the Royal
#' Statistical Society: Series A*, 176(2), 521-540.
#' \doi{10.1111/j.1467-985X.2012.01048.x}
#'
#' Pessoa, D. G. C., Damico, A. and Jacob, G. (2024). *Poverty and Inequality
#' with Complex Survey Data*. <https://www.convey-r.org/>
#'
#' @seealso [qsr_poverty()], [qsr_svydesign()].
#'
#' @examples
#' d <- qsr_svydesign(qsr_survey, strata = "region", weights = "weight",
#'                    register = FALSE)
#' qsr_inequality(d, income = "ingreso_total")
#'
#' # All four measures by region
#' qsr_inequality(d, income = "ingreso_total", by = "region",
#'                measures = c("gini", "theil_t", "theil_l", "atkinson"))
#'
#' @export
qsr_inequality <- function(design = NULL,
                           income,
                           measures = c("gini", "theil_t"),
                           by = NULL,
                           epsilon = 1,
                           ci_level = 0.95,
                           na_rm = TRUE,
                           digits = NULL) {
  measures <- match.arg(measures, c("gini", "theil_t", "theil_l", "atkinson"),
                        several.ok = TRUE)
  if ("atkinson" %in% measures &&
      (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon <= 0)) {
    cli::cli_abort("{.arg epsilon} must be a single positive number.")
  }
  design <- .qsr_resolve_design(design)
  vars <- design$variables
  y <- .qsr_svy_numeric(vars, income, "income")

  w0 <- .qsr_base_weights(design)
  miss <- is.na(y)
  if (any(miss & w0 > 0) && !na_rm) {
    cli::cli_abort(c(
      "{.val {sum(miss & w0 > 0)}} unit{?s} with missing {.arg income}.",
      "i" = "Use {.code na_rm = TRUE} to drop them."
    ))
  }
  w0[miss] <- 0
  y[miss] <- 1

  nonpos <- y <= 0 & w0 > 0
  if (any(nonpos) && any(measures != "gini")) {
    cli::cli_warn(
      "{.val {sum(nonpos)}} unit{?s} with income <= 0 left out of the Theil/Atkinson indices."
    )
  }
  ypos <- ifelse(y > 0, y, 1)

  doms <- .qsr_domains(vars, by)
  rows <- list()
  for (m in measures) {
    fns <- switch(m,
      gini     = list(est = function(w) .qsr_gini(y, w),
                      lin = function(w) .qsr_gini_lin(y, w), keep = TRUE),
      theil_t  = list(est = function(w) .qsr_gei(ypos, w, 1),
                      lin = function(w) .qsr_gei_lin(ypos, w, 1), keep = y > 0),
      theil_l  = list(est = function(w) .qsr_gei(ypos, w, 0),
                      lin = function(w) .qsr_gei_lin(ypos, w, 0), keep = y > 0),
      atkinson = list(est = function(w) .qsr_atkinson(ypos, w, epsilon),
                      lin = function(w) .qsr_atkinson_lin(ypos, w, epsilon),
                      keep = y > 0)
    )
    for (k in seq_along(doms$masks)) {
      st <- .qsr_svy_stat(design, w0 * doms$masks[[k]] * fns$keep,
                          fns$est, fns$lin)
      rows[[length(rows) + 1L]] <- data.frame(
        doms$keys[k, , drop = FALSE], measure = m,
        n = st[["n"]], estimate = st[["estimate"]], se = st[["se"]],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }
  out <- .qsr_svy_finish(rows, design, ci_level, digits, bounds = NULL)
  cli::cli_alert_success(
    "Inequality ({paste(measures, collapse = ', ')}) over {.val {sum(w0 > 0)}} units {.emph (design-based SE)}."
  )
  out
}


# ============================================================
# Internal helpers
# ============================================================

# Design passed explicitly, or the one registered by qsr_svydesign().
.qsr_resolve_design <- function(design) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    cli::cli_abort(c(
      "Design-based estimation needs the {.pkg survey} package.",
      "i" = "Install it with {.code install.packages(\"survey\")}."
    ))
  }
  design <- design %||% .qsr_context$get("design")
  if (is.null(design)) {
    cli::cli_abort(c(
      "No survey design.",
      "i" = "Pass {.arg design} or declare one with {.fn qsr_svydesign}."
    ))
  }
  if (!inherits(design, c("survey.design2", "svyrep.design"))) {
    cli::cli_abort(
      "{.arg design} must be a {.cls survey.design2} or {.cls svyrep.design} object."
    )
  }
  design
}

.qsr_svy_numeric <- function(vars, col, what) {
  if (!is.character(col) || length(col) != 1L || !col %in% names(vars)) {
    cli::cli_abort("{.arg {what}} must name a column of the design data.")
  }
  x <- vars[[col]]
  if (!is.numeric(x)) cli::cli_abort("Column {.val {col}} is not numeric.")
  as.numeric(x)
}

.qsr_svy_line <- function(vars, line) {
  n <- nrow(vars)
  if (is.character(line) && length(line) == 1L) {
    return(.qsr_svy_numeric(vars, line, "line"))
  }
  if (!is.numeric(line) || !length(line) %in% c(1L, n)) {
    cli::cli_abort(
      "{.arg line} must be a number, a column name or a vector of length {n}."
    )
  }
  rep_len(as.numeric(line), n)
}

# Full-sample weights; zero for units dropped by an earlier subset().
.qsr_base_weights <- function(design) {
  w <- if (inherits(design, "svyrep.design")) {
    stats::weights(design, "sampling")
  } else {
    1 / design$prob
  }
  w <- as.numeric(w)
  w[!is.finite(w)] <- 0
  w
}

# One 0/1 mask over the full sample per domain, plus a data.frame of keys.
.qsr_domains <- function(vars, by) {
  n <- nrow(vars)
  if (is.null(by)) {
    return(list(masks = list(rep(1, n)),
                keys = data.frame(domain = "Total", stringsAsFactors = FALSE)))
  }
  miss <- setdiff(by, names(vars))
  if (length(miss)) cli::cli_abort("Domain column(s) not found: {.val {miss}}")
  cell <- interaction(vars[by], drop = TRUE, sep = "\r")
  idx <- match(levels(cell), cell)
  keys <- vars[idx, by, drop = FALSE]
  rownames(keys) <- NULL
  keys[] <- lapply(keys, function(x) if (is.factor(x)) as.character(x) else x)
  masks <- lapply(levels(cell), function(l) as.numeric(!is.na(cell) & cell == l))
  list(masks = masks, keys = keys)
}

# Estimate and SE of a statistic for analysis weights `w` (zero outside the
# domain). Taylor: variance of the linearized variable (convey/svyrecvar).
# Replicate: the statistic recomputed with every replicate weight (svrVar).
.qsr_svy_stat <- function(design, w, est_fun, lin_fun) {
  n_used <- sum(w > 0)
  if (n_used == 0L) {
    return(c(n = 0, estimate = NA_real_, se = NA_real_))
  }
  est <- est_fun(w)
  if (inherits(design, "svyrep.design")) {
    keep <- w > 0
    rw <- as.matrix(stats::weights(design, "analysis"))
    reps <- apply(rw, 2L, function(wr) est_fun(ifelse(keep, wr, 0)))
    v <- survey::svrVar(reps, design$scale, design$rscales,
                        mse = design$mse, coef = est)
  } else {
    lin <- lin_fun(w)
    v <- survey::svyrecvar(lin * w, design$cluster, design$strata,
                           design$fpc, postStrata = design$postStrata)
  }
  v <- as.numeric(v)[1L]
  c(n = n_used, estimate = est, se = if (is.finite(v)) sqrt(max(v, 0)) else NA_real_)
}

# Bind rows, add t-based intervals on the design df, round.
.qsr_svy_finish <- function(rows, design, ci_level, digits, bounds = NULL) {
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  df_design <- tryCatch(survey::degf(design), error = function(e) Inf)
  if (!is.finite(df_design) || df_design < 1) df_design <- Inf
  q <- stats::qt(1 - (1 - ci_level) / 2, df = df_design)
  out$ci_low  <- out$estimate - q * out$se
  out$ci_high <- out$estimate + q * out$se
  if (!is.null(bounds)) {
    out$ci_low  <- pmax(bounds[1], out$ci_low)
    out$ci_high <- pmin(bounds[2], out$ci_high)
  }
  if (!is.null(digits)) {
    for (cc in c("estimate", "se", "ci_low", "ci_high")) {
      out[[cc]] <- round(out[[cc]], digits)
    }
  }
  out
}

# Gini as in convey::svygini(): sum((2 r - 1) w y) / (N Y) - 1.
.qsr_gini <- function(y, w) {
  k <- w > 0
  y <- y[k]; w <- w[k]
  o <- order(y)
  y <- y[o]; w <- w[o]
  r <- cumsum(w)
  sum((2 * r - 1) * y * w) / (sum(w) * sum(w * y)) - 1
}

# Linearized variable of the Gini (Langel and Tille, 2013), zero where w == 0.
.qsr_gini_lin <- function(y, w) {
  out <- numeric(length(y))
  k <- which(w > 0)
  yy <- y[k]; ww <- w[k]
  o <- order(yy)
  yy <- yy[o]; ww <- ww[o]
  N <- sum(ww); Y <- sum(ww * yy)
  r <- cumsum(ww); G <- cumsum(yy * ww)
  T1 <- sum(r * yy * ww)
  T1lin <- Y - G + yy * ww + r * yy
  # G = (2 T1 - T2) / (T2 T3) - 1 with T2 = Y, T3 = N
  lin <- 2 / (Y * N) * T1lin - 2 * T1 / (Y^2 * N) * yy - (2 * T1 - Y) / (Y * N^2)
  out[k[o]] <- lin
  out
}

.qsr_gei_score <- function(u, epsilon) {
  if (epsilon == 0) -log(u)
  else if (epsilon == 1) u * log(u)
  else (u^epsilon - 1) / (epsilon^2 - epsilon)
}

# Generalized entropy index (Theil T: epsilon = 1; Theil L / MLD: epsilon = 0).
.qsr_gei <- function(y, w, epsilon) {
  N <- sum(w)
  ybar <- sum(w * y) / N
  sum(w * .qsr_gei_score(y / ybar, epsilon)) / N
}

.qsr_gei_lin <- function(y, w, epsilon) {
  N <- sum(w)
  ybar <- sum(w * y) / N
  gei <- .qsr_gei(y, w, epsilon)
  d_ybar <- if (epsilon == 0) 1 / ybar
            else if (epsilon == 1) -(gei + 1) / ybar
            else -((epsilon^2 - epsilon) * gei + 1) / (ybar * (epsilon - 1))
  lin <- (.qsr_gei_score(y / ybar, epsilon) - gei) / N + d_ybar * (y - ybar) / N
  ifelse(w > 0, lin, 0)
}

# Atkinson index with inequality aversion epsilon.
.qsr_atkinson <- function(y, w, epsilon) {
  U0 <- sum(w); U1 <- sum(w * y)
  if (epsilon == 1) {
    1 - (U0 / U1) * exp(sum(w * log(y)) / U0)
  } else {
    Ue <- sum(w * y^(1 - epsilon))
    1 - Ue^(1 / (1 - epsilon)) / (U0^(epsilon / (1 - epsilon)) * U1)
  }
}

.qsr_atkinson_lin <- function(y, w, epsilon) {
  U0 <- sum(w); U1 <- sum(w * y)
  if (epsilon == 1) {
    A <- .qsr_atkinson(y, w, 1)
    T0 <- sum(w * log(y))
    lin <- (A - 1) * (1 - T0 / U0) / U0 + y * (1 - A) / U1 +
      log(y) * (A - 1) / U0
  } else {
    Ue <- sum(w * y^(1 - epsilon))
    a <- epsilon / (1 - epsilon); b <- 1 / (1 - epsilon)
    lin <- a * Ue^b / (U1 * U0^b) + y * Ue^b / (U0^a * U1^2) -
      b * y^(1 - epsilon) * Ue^a / (U0^a * U1)
  }
  ifelse(w > 0, lin, 0)
}
