# ============================================================
# QUASAR - Analytics Layer
# qsr_clean(), qsr_test(), qsr_feature()
# "If it's complex in R, QUASAR makes it one line."
# ============================================================

# ---- qsr_clean() ----

#' Clean and prepare data for analysis
#'
#' One-line data cleaning: imputation, outlier handling, normalization, and
#' encoding. Reads from context if no data provided.
#'
#' @param data Data frame or NULL (reads from context).
#' @param impute Character. Imputation method: "none", "mean", "median",
#'   "mode", "drop". Default "none".
#' @param outliers Character. Outlier handling: "none", "remove", "winsorize".
#'   Default "none".
#' @param outlier_threshold Numeric. Z-score threshold for outlier detection.
#'   Default reads from context or 3.
#' @param normalize Character. Normalization: "none", "minmax", "zscore".
#'   Default "none".
#' @param encode Character. Categorical encoding: "none", "dummy", "label".
#'   Default "none".
#' @param drop_na_cols Numeric 0-1. Drop columns with NA proportion above this.
#'   Default NULL (don't drop).
#' @param name Character. Name for context registration. Default "data".
#' @param register Logical. Auto-register in context. Default TRUE.
#'
#' @return A cleaned data.frame (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_data(airquality)
#' qsr_clean(impute = "median", outliers = "remove")
#' qsr_table(type = "summary")
#'
#' # Full pipeline
#' qsr_clean(impute = "mean", normalize = "zscore", encode = "dummy")
#' }
#'
#' @export
qsr_clean <- function(data = NULL,
                      impute = c("none", "mean", "median", "mode", "drop"),
                      outliers = c("none", "remove", "winsorize"),
                      outlier_threshold = NULL,
                      normalize = c("none", "minmax", "zscore"),
                      encode = c("none", "dummy", "label"),
                      drop_na_cols = NULL,
                      name = "data",
                      register = TRUE) {

  impute <- match.arg(impute)
  outliers <- match.arg(outliers)
  normalize <- match.arg(normalize)
  encode <- match.arg(encode)

  # Get data from context if not provided
  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) {
      cli::cli_abort(c(
        "No data provided and none in context.",
        "i" = "Use {.fn qsr_data} first or pass data directly."
      ))
    }
  }

  df <- as.data.frame(data)
  n_before <- nrow(df)
  actions <- character()

  # Get threshold from context if not provided
  if (is.null(outlier_threshold)) {
    outlier_threshold <- .qsr_context$get("outlier_threshold") %||% 3
  }

  # Step 1: Drop columns with too many NAs
  if (!is.null(drop_na_cols) && drop_na_cols > 0 && drop_na_cols < 1) {
    na_prop <- colMeans(is.na(df))
    drop_cols <- names(na_prop[na_prop > drop_na_cols])
    if (length(drop_cols) > 0) {
      df <- df[, !names(df) %in% drop_cols, drop = FALSE]
      actions <- c(actions, paste0("dropped ", length(drop_cols),
                                   " cols (>", drop_na_cols * 100, "% NA)"))
    }
  }

  # Step 2: Imputation
  if (impute != "none") {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    char_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

    if (impute == "drop") {
      df <- df[stats::complete.cases(df), , drop = FALSE]
      actions <- c(actions, "dropped rows with NA")
    } else {
      # Numeric columns
      for (col in num_cols) {
        nas <- is.na(df[[col]])
        if (any(nas)) {
          val <- switch(impute,
            mean   = mean(df[[col]], na.rm = TRUE),
            median = stats::median(df[[col]], na.rm = TRUE),
            mode   = .qsr_mode(df[[col]])
          )
          df[[col]][nas] <- val
        }
      }
      # Character/factor columns: always use mode
      for (col in char_cols) {
        nas <- is.na(df[[col]])
        if (any(nas)) {
          df[[col]][nas] <- .qsr_mode(df[[col]])
        }
      }
      n_imputed <- sum(is.na(data)) - sum(is.na(df))
      if (n_imputed > 0) {
        actions <- c(actions, paste0("imputed ", n_imputed, " values (", impute, ")"))
      }
    }
  }

  # Step 3: Outlier handling (numeric columns only)
  if (outliers != "none") {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (outliers == "remove") {
      keep <- rep(TRUE, nrow(df))
      for (col in num_cols) {
        z <- abs(scale(df[[col]]))
        keep <- keep & (z <= outlier_threshold | is.na(z))
      }
      n_removed <- sum(!keep)
      df <- df[keep, , drop = FALSE]
      actions <- c(actions, paste0("removed ", n_removed,
                                   " outlier rows (|z|>", outlier_threshold, ")"))
    } else if (outliers == "winsorize") {
      for (col in num_cols) {
        z <- as.numeric(scale(df[[col]]))
        upper <- mean(df[[col]], na.rm = TRUE) + outlier_threshold * stats::sd(df[[col]], na.rm = TRUE)
        lower <- mean(df[[col]], na.rm = TRUE) - outlier_threshold * stats::sd(df[[col]], na.rm = TRUE)
        df[[col]] <- pmax(pmin(df[[col]], upper), lower)
      }
      actions <- c(actions, paste0("winsorized at |z|>", outlier_threshold))
    }
  }

  # Step 4: Normalization (numeric columns only)
  if (normalize != "none") {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    for (col in num_cols) {
      if (normalize == "minmax") {
        rng <- range(df[[col]], na.rm = TRUE)
        if (rng[2] > rng[1]) {
          df[[col]] <- (df[[col]] - rng[1]) / (rng[2] - rng[1])
        }
      } else if (normalize == "zscore") {
        m <- mean(df[[col]], na.rm = TRUE)
        s <- stats::sd(df[[col]], na.rm = TRUE)
        if (s > 0) df[[col]] <- (df[[col]] - m) / s
      }
    }
    actions <- c(actions, paste0("normalized (", normalize, ")"))
  }

  # Step 5: Encoding
  if (encode != "none") {
    char_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(char_cols) > 0) {
      if (encode == "dummy") {
        for (col in char_cols) {
          levels <- unique(df[[col]])
          for (lvl in levels[-1]) {  # drop first to avoid collinearity
            new_col <- paste0(col, "_", lvl)
            df[[new_col]] <- as.integer(df[[col]] == lvl)
          }
          df[[col]] <- NULL
        }
        actions <- c(actions, paste0("dummy-encoded ", length(char_cols), " cols"))
      } else if (encode == "label") {
        for (col in char_cols) {
          df[[col]] <- as.integer(as.factor(df[[col]]))
        }
        actions <- c(actions, paste0("label-encoded ", length(char_cols), " cols"))
      }
    }
  }

  # Report
  cli::cli_alert_success(
    "Cleaned: {.val {n_before}} -> {.val {nrow(df)}} rows, {.val {ncol(df)}} cols"
  )
  if (length(actions) > 0) {
    for (a in actions) cli::cli_alert_info("  {a}")
  }

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


# ---- qsr_test() ----

#' Run statistical tests with one line
#'
#' Auto-detects the appropriate test based on the data, or specify one
#' explicitly. Results are printed and stored in context.
#'
#' @param formula Formula (e.g., `y ~ group`) or NULL.
#' @param data Data frame or NULL (reads from context).
#' @param test Character. Test type: "auto", "t", "paired_t", "anova",
#'   "chi2", "wilcox", "kruskal", "shapiro", "correlation", "levene",
#'   "fisher". Default "auto".
#' @param ... Additional arguments passed to the underlying test function.
#'
#' @return A list with test results (invisibly). Also stored in context.
#'
#' @details
#' When `test = "auto"`, QUASAR chooses the test based on the formula:
#' - Numeric ~ Factor (2 levels): t-test
#' - Numeric ~ Factor (3+ levels): ANOVA
#' - Factor ~ Factor: chi-squared
#' - Single numeric variable: Shapiro-Wilk normality test
#' - Two numeric variables: Pearson correlation
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#'
#' # Auto-detect: t-test (mpg by transmission type)
#' qsr_test(mpg ~ am)
#'
#' # Auto-detect: ANOVA (mpg by number of cylinders)
#' qsr_test(mpg ~ cyl)
#'
#' # Explicit test
#' qsr_test(mpg ~ am, test = "wilcox")
#'
#' # Normality test
#' qsr_test(~mpg, test = "shapiro")
#'
#' # Correlation
#' qsr_test(mpg ~ wt, test = "correlation")
#' }
#'
#' @export
qsr_test <- function(formula = NULL,
                     data = NULL,
                     test = c("auto", "t", "paired_t", "anova", "chi2",
                              "wilcox", "kruskal", "shapiro",
                              "correlation", "levene", "fisher"),
                     ...) {

  test <- match.arg(test)

  # Get data from context
  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) {
      cli::cli_abort("No data provided and none in context.")
    }
  }

  df <- as.data.frame(data)

  # Parse formula
  if (is.null(formula)) {
    cli::cli_abort("A formula is required (e.g., {.code mpg ~ am}).")
  }

  vars <- all.vars(formula)
  has_response <- length(formula) == 3  # y ~ x

  if (has_response) {
    y_name <- vars[1]
    x_name <- vars[2]
    y <- df[[y_name]]
    x <- df[[x_name]]

    if (is.null(y) || is.null(x)) {
      cli::cli_abort("Variables {.val {vars}} not found in data.")
    }
  } else {
    y_name <- vars[1]
    y <- df[[y_name]]
    x <- NULL
    x_name <- NULL
  }

  # Auto-detect test
  if (test == "auto") {
    if (!has_response) {
      test <- "shapiro"
    } else if (is.numeric(y) && is.numeric(x)) {
      test <- "correlation"
    } else if (is.numeric(y) && (is.character(x) || is.factor(x) || is.logical(x))) {
      n_groups <- length(unique(x))
      test <- if (n_groups == 2) "t" else "anova"
    } else if ((is.character(y) || is.factor(y)) && (is.character(x) || is.factor(x))) {
      test <- "chi2"
    } else {
      # Coerce to factor if numeric with few levels
      if (is.numeric(x) && length(unique(x)) <= 10) {
        x <- as.factor(x)
        n_groups <- length(unique(x))
        test <- if (n_groups == 2) "t" else "anova"
      } else {
        test <- "correlation"
      }
    }
    cli::cli_alert_info("Auto-selected: {.strong {test}} test")
  }

  # Run test
  result <- switch(test,
    t = {
      if (is.numeric(x)) x <- as.factor(x)
      res <- stats::t.test(y ~ x, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    paired_t = {
      res <- stats::t.test(y, x, paired = TRUE, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    anova = {
      if (is.numeric(x)) x <- as.factor(x)
      res <- stats::aov(y ~ x)
      summary_aov <- summary(res)
      list(
        test = "ANOVA",
        formula = paste(y_name, "~", x_name),
        f_value = summary_aov[[1]][["F value"]][1],
        p_value = summary_aov[[1]][["Pr(>F)"]][1],
        df = paste(summary_aov[[1]][["Df"]], collapse = ", "),
        significant = summary_aov[[1]][["Pr(>F)"]][1] <
          (.qsr_context$get("significance_level") %||% 0.05),
        raw = res
      )
    },
    chi2 = {
      tbl <- table(y, x)
      res <- stats::chisq.test(tbl, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    wilcox = {
      if (is.numeric(x)) x <- as.factor(x)
      res <- stats::wilcox.test(y ~ x, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    kruskal = {
      if (is.numeric(x)) x <- as.factor(x)
      res <- stats::kruskal.test(y ~ x, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    shapiro = {
      y <- as.numeric(y)
      y <- y[!is.na(y)]
      samp <- if (length(y) > 5000) sample(y, 5000) else y
      res <- stats::shapiro.test(samp)
      .qsr_format_test(res, test, y_name, NULL)
    },
    correlation = {
      res <- stats::cor.test(y, x, ...)
      .qsr_format_test(res, test, y_name, x_name)
    },
    levene = {
      if (is.numeric(x)) x <- as.factor(x)
      groups <- split(y, x)
      group_names <- names(groups)
      n_groups <- length(groups)
      N <- length(y)
      group_medians <- vapply(groups, stats::median, na.rm = TRUE, numeric(1))
      z <- abs(y - group_medians[as.character(x)])
      res <- stats::aov(z ~ x)
      summary_res <- summary(res)
      list(
        test = "Levene's test",
        formula = paste(y_name, "~", x_name),
        f_value = summary_res[[1]][["F value"]][1],
        p_value = summary_res[[1]][["Pr(>F)"]][1],
        significant = summary_res[[1]][["Pr(>F)"]][1] <
          (.qsr_context$get("significance_level") %||% 0.05),
        raw = res
      )
    },
    fisher = {
      tbl <- table(y, x)
      res <- stats::fisher.test(tbl, ...)
      .qsr_format_test(res, test, y_name, x_name)
    }
  )

  # Print results
  sig_level <- .qsr_context$get("significance_level") %||% 0.05
  sig_star <- if (!is.null(result$p_value) && result$p_value < sig_level) " ***" else ""

  cli::cli_h3("{result$test}")
  if (!is.null(result$formula)) cli::cli_text("Formula: {result$formula}")
  if (!is.null(result$statistic)) cli::cli_text("Statistic: {.val {round(result$statistic, 4)}}")
  if (!is.null(result$p_value)) {
    cli::cli_text("p-value: {.val {format.pval(result$p_value, digits = 4)}}{sig_star}")
  }
  if (!is.null(result$estimate)) cli::cli_text("Estimate: {.val {round(result$estimate, 4)}}")
  if (!is.null(result$significant)) {
    if (result$significant) {
      cli::cli_alert_success("Significant at {.val {sig_level}} level")
    } else {
      cli::cli_alert_info("Not significant at {.val {sig_level}} level")
    }
  }

  invisible(result)
}


# ---- qsr_feature() ----

#' Automatic feature engineering
#'
#' Create new features from existing columns: interactions, polynomials,
#' lag/lead, binning, log transforms.
#'
#' @param data Data frame or NULL (reads from context).
#' @param interactions Character vector of column pairs (e.g., c("x1:x2")).
#' @param polynomial Character vector of columns + degree (e.g., c("x1" = 2)).
#' @param log Character vector of columns to log-transform.
#' @param bins List of columns to bin (e.g., list(age = 4)).
#' @param lag List of columns to lag (e.g., list(value = 1)).
#' @param name Character. Context name. Default "data".
#' @param register Logical. Default TRUE.
#'
#' @return Data frame with new features (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#' qsr_feature(log = c("hp", "wt"), interactions = c("wt:hp"),
#'             polynomial = c(wt = 2))
#' }
#'
#' @export
qsr_feature <- function(data = NULL,
                        interactions = NULL,
                        polynomial = NULL,
                        log = NULL,
                        bins = NULL,
                        lag = NULL,
                        name = "data",
                        register = TRUE) {

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) {
      cli::cli_abort("No data provided and none in context.")
    }
  }

  df <- as.data.frame(data)
  n_new <- 0L

  # Log transforms
  if (!is.null(log)) {
    for (col in log) {
      new_name <- paste0("log_", col)
      df[[new_name]] <- base::log(df[[col]] + 1)  # +1 to handle zeros
      n_new <- n_new + 1L
    }
  }

  # Polynomial features
  if (!is.null(polynomial)) {
    for (i in seq_along(polynomial)) {
      col <- names(polynomial)[i]
      degree <- polynomial[[i]]
      for (d in 2:degree) {
        new_name <- paste0(col, "_pow", d)
        df[[new_name]] <- df[[col]]^d
        n_new <- n_new + 1L
      }
    }
  }

  # Interactions
  if (!is.null(interactions)) {
    for (inter in interactions) {
      parts <- strsplit(inter, ":")[[1]]
      if (length(parts) == 2 && all(parts %in% names(df))) {
        new_name <- paste0(parts[1], "_x_", parts[2])
        df[[new_name]] <- df[[parts[1]]] * df[[parts[2]]]
        n_new <- n_new + 1L
      }
    }
  }

  # Binning
  if (!is.null(bins)) {
    for (col in names(bins)) {
      n_bins <- bins[[col]]
      new_name <- paste0(col, "_bin")
      df[[new_name]] <- cut(df[[col]], breaks = n_bins, labels = FALSE)
      n_new <- n_new + 1L
    }
  }

  # Lag features
  if (!is.null(lag)) {
    for (col in names(lag)) {
      n_lag <- lag[[col]]
      for (l in seq_len(n_lag)) {
        new_name <- paste0(col, "_lag", l)
        df[[new_name]] <- c(rep(NA, l), df[[col]][1:(nrow(df) - l)])
        n_new <- n_new + 1L
      }
    }
  }

  cli::cli_alert_success("Created {.val {n_new}} new features ({.val {ncol(df)}} total cols)")

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


# ---- Internal helpers ----

#' @keywords internal
.qsr_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @keywords internal
.qsr_format_test <- function(res, test_type, y_name, x_name) {
  sig_level <- .qsr_context$get("significance_level") %||% 0.05

  formula_str <- if (!is.null(x_name)) paste(y_name, "~", x_name) else y_name

  list(
    test = res$method %||% test_type,
    formula = formula_str,
    statistic = as.numeric(res$statistic),
    p_value = res$p.value,
    estimate = if (!is.null(res$estimate)) as.numeric(res$estimate) else NULL,
    conf_int = res$conf.int,
    significant = res$p.value < sig_level,
    raw = res
  )
}
