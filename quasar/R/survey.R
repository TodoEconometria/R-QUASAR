# ============================================================
# QUASAR - Survey Microdata Tools
# qsr_normalize_text(), qsr_crosswalk(), qsr_currency(),
# qsr_validate(), qsr_decision_log()
#
# Built from real experience cleaning Latin American household
# surveys. Every function here solves a problem that took
# researchers weeks to solve manually.
# ============================================================


#' Normalize text fields with fuzzy matching
#'
#' Handles the "patiTo" problem: text libre in surveys with typos,
#' inconsistent capitalization, abbreviations, and dialect variants.
#' Uses Levenshtein distance for fuzzy matching with human review.
#'
#' @param data Data frame or NULL (reads from context).
#' @param column Character. Name of the text column to normalize.
#' @param dictionary Character vector. Canonical values to match against.
#'   If NULL, auto-generates from most frequent values.
#' @param max_dist Integer. Maximum Levenshtein distance for a match.
#'   Default 2. Values >2 should be reviewed manually.
#' @param min_freq Integer. Minimum frequency to consider a value as
#'   canonical when auto-generating dictionary. Default 3.
#' @param review Logical. Print matches for manual review before applying.
#'   Default TRUE.
#' @param new_column Character. Name for normalized column. Default
#'   paste0(column, "_clean").
#' @param register Logical. Default TRUE.
#'
#' @return Data frame with normalized column (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_data(survey)
#' qsr_normalize_text(
#'   column = "crop_name",
#'   dictionary = c("coca", "cafe", "cacao", "banana", "arroz"),
#'   max_dist = 2,
#'   review = TRUE
#' )
#' }
#'
#' @export
qsr_normalize_text <- function(data = NULL,
                               column,
                               dictionary = NULL,
                               max_dist = 2L,
                               min_freq = 3L,
                               review = TRUE,
                               new_column = NULL,
                               register = TRUE) {

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("No data. Use {.fn qsr_data} first.")
  }

  df <- as.data.frame(data)

  if (!column %in% names(df)) {
    cli::cli_abort("Column {.val {column}} not found in data.")
  }

  if (is.null(new_column)) new_column <- paste0(column, "_clean")

  # Step 1: Basic normalization
  raw <- df[[column]]
  cleaned <- trimws(tolower(as.character(raw)))
  cleaned <- gsub("\\s+", " ", cleaned)  # collapse multiple spaces
  cleaned <- gsub("[[:punct:]]", "", cleaned)  # remove punctuation

  # Step 2: Build dictionary if not provided
  if (is.null(dictionary)) {
    freq_table <- sort(table(cleaned), decreasing = TRUE)
    dictionary <- names(freq_table[freq_table >= min_freq])
    cli::cli_alert_info(
      "Auto-generated dictionary: {.val {length(dictionary)}} canonical values (freq >= {min_freq})"
    )
  } else {
    dictionary <- trimws(tolower(dictionary))
  }

  # Step 3: Fuzzy matching
  unique_vals <- unique(cleaned[!is.na(cleaned) & !cleaned %in% dictionary])

  matches <- list()
  for (val in unique_vals) {
    if (nchar(val) == 0) next
    dists <- utils::adist(val, dictionary)[1, ]
    best_idx <- which.min(dists)
    best_dist <- dists[best_idx]

    if (best_dist <= max_dist) {
      matches[[val]] <- list(
        original = val,
        match = dictionary[best_idx],
        distance = best_dist,
        freq = sum(cleaned == val, na.rm = TRUE)
      )
    }
  }

  # Step 4: Review
  if (review && length(matches) > 0) {
    cli::cli_h3("Fuzzy matches for review (distance <= {max_dist})")
    review_df <- data.frame(
      original = vapply(matches, function(m) m$original, character(1)),
      matched_to = vapply(matches, function(m) m$match, character(1)),
      distance = vapply(matches, function(m) as.integer(m$distance), integer(1)),
      n_affected = vapply(matches, function(m) as.integer(m$freq), integer(1)),
      stringsAsFactors = FALSE
    )
    review_df <- review_df[order(review_df$distance, -review_df$n_affected), ]
    print(review_df, row.names = FALSE)
    cli::cli_alert_warning(
      "Review matches with distance > 1 carefully before proceeding."
    )
  }

  # Step 5: Apply matches
  result <- cleaned
  for (m in matches) {
    result[cleaned == m$original] <- m$match
  }

  df[[new_column]] <- result

  n_changed <- sum(result != cleaned, na.rm = TRUE)
  n_unique_before <- length(unique(cleaned[!is.na(cleaned)]))
  n_unique_after <- length(unique(result[!is.na(result)]))

  cli::cli_alert_success(
    "Normalized {.val {column}}: {.val {n_unique_before}} -> {.val {n_unique_after}} unique values ({.val {n_changed}} values corrected)"
  )

  if (register) qsr_data(df)
  invisible(df)
}


#' Apply a geographic crosswalk
#'
#' Translates geographic codes between periods (e.g., ubigeo pre/post
#' 2017 in Peru, DIVIPOLA changes in Colombia, municipio codes in Bolivia).
#'
#' @param data Data frame or NULL (reads from context).
#' @param crosswalk Data frame with columns `old` and `new`, or path
#'   to a CSV file with the crosswalk table.
#' @param column Character. Column in data containing the geographic code.
#' @param year_column Character. Column with the year (to apply crosswalk
#'   only to specific years). Default NULL (apply to all).
#' @param apply_before Integer. Apply crosswalk to years before this.
#'   Default NULL.
#' @param apply_after Integer. Apply crosswalk to years after this.
#'   Default NULL.
#' @param new_column Character. Name for harmonized column. Default
#'   paste0(column, "_harmonized").
#' @param register Logical. Default TRUE.
#'
#' @return Data frame with harmonized geographic codes (invisibly).
#'
#' @examples
#' \dontrun{
#' # Peru ubigeo crosswalk
#' qsr_crosswalk(
#'   column = "ubigeo",
#'   crosswalk = "data/raw/spatial/peru/ubigeo_crosswalk_2017.csv",
#'   year_column = "year",
#'   apply_before = 2017
#' )
#'
#' # Colombia DIVIPOLA
#' qsr_crosswalk(
#'   column = "cod_municipio",
#'   crosswalk = data.frame(old = c("05001", "05002"), new = c("05001", "05899"))
#' )
#' }
#'
#' @export
qsr_crosswalk <- function(data = NULL,
                          crosswalk,
                          column,
                          year_column = NULL,
                          apply_before = NULL,
                          apply_after = NULL,
                          new_column = NULL,
                          register = TRUE) {

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("No data.")
  }
  df <- as.data.frame(data)

  if (is.null(new_column)) new_column <- paste0(column, "_harmonized")

  # Load crosswalk
  if (is.character(crosswalk) && length(crosswalk) == 1 && file.exists(crosswalk)) {
    cw <- utils::read.csv(crosswalk, stringsAsFactors = FALSE, colClasses = "character")
  } else {
    cw <- as.data.frame(crosswalk, stringsAsFactors = FALSE)
  }

  # Expect columns named 'old' and 'new' (or first two columns)
  if (!all(c("old", "new") %in% names(cw))) {
    names(cw)[1:2] <- c("old", "new")
  }

  cw$old <- as.character(cw$old)
  cw$new <- as.character(cw$new)

  # Apply crosswalk
  df[[new_column]] <- as.character(df[[column]])

  if (!is.null(year_column) && (!is.null(apply_before) || !is.null(apply_after))) {
    # Apply only to specific years
    mask <- rep(TRUE, nrow(df))
    if (!is.null(apply_before)) mask <- mask & df[[year_column]] < apply_before
    if (!is.null(apply_after))  mask <- mask & df[[year_column]] >= apply_after

    idx <- match(df[[new_column]][mask], cw$old)
    matched <- !is.na(idx)
    df[[new_column]][mask][matched] <- cw$new[idx[matched]]

    n_translated <- sum(matched)
    n_total <- sum(mask)
  } else {
    idx <- match(df[[new_column]], cw$old)
    matched <- !is.na(idx)
    df[[new_column]][matched] <- cw$new[idx[matched]]

    n_translated <- sum(matched)
    n_total <- nrow(df)
  }

  cli::cli_alert_success(
    "Crosswalk applied: {.val {n_translated}}/{.val {n_total}} codes translated"
  )

  if (register) qsr_data(df)
  invisible(df)
}


#' Convert monetary values to constant USD
#'
#' Converts local currency values to USD using exchange rates and
#' deflators. Handles BOB, PEN, COP, BRL, and any other currency
#' with available exchange rate data.
#'
#' @param data Data frame or NULL (reads from context).
#' @param columns Character vector. Columns with monetary values.
#' @param from_currency Character. Source currency code: "BOB", "PEN",
#'   "COP", "BRL", etc.
#' @param base_year Integer. Base year for constant prices. Default 2015.
#' @param year_column Character. Column with the year. Default "year".
#' @param exchange_rates Named numeric vector or data frame. Exchange
#'   rates (local currency per USD) by year. If NULL, uses built-in
#'   rates from World Bank.
#' @param deflator Named numeric vector or data frame. CPI deflator by
#'   year. If NULL, uses built-in deflators.
#' @param suffix Character. Suffix for new columns. Default "_usd".
#' @param register Logical. Default TRUE.
#'
#' @return Data frame with converted columns (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_data(bolivia_survey)
#' qsr_currency(
#'   columns = c("ingreso_laboral", "ingreso_total"),
#'   from_currency = "BOB",
#'   base_year = 2015,
#'   year_column = "year"
#' )
#' }
#'
#' @export
qsr_currency <- function(data = NULL,
                         columns,
                         from_currency,
                         base_year = 2015L,
                         year_column = "year",
                         exchange_rates = NULL,
                         deflator = NULL,
                         suffix = "_usd",
                         register = TRUE) {

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("No data.")
  }
  df <- as.data.frame(data)

  # Built-in exchange rates (annual average, local per USD)
  if (is.null(exchange_rates)) {
    exchange_rates <- .qsr_builtin_exchange_rates(from_currency)
  }

  # Built-in CPI deflator (base_year = 100)
  if (is.null(deflator)) {
    deflator <- .qsr_builtin_deflator(from_currency, base_year)
  }

  # Convert rates to lookup
  if (is.data.frame(exchange_rates)) {
    er_lookup <- stats::setNames(exchange_rates[[2]], as.character(exchange_rates[[1]]))
  } else {
    er_lookup <- exchange_rates
  }

  if (is.data.frame(deflator)) {
    defl_lookup <- stats::setNames(deflator[[2]], as.character(deflator[[1]]))
  } else {
    defl_lookup <- deflator
  }

  years <- as.character(df[[year_column]])

  for (col in columns) {
    new_col <- paste0(col, suffix)

    er <- er_lookup[years]
    defl <- defl_lookup[years]

    # Convert: local nominal -> USD nominal -> USD constant
    df[[new_col]] <- (df[[col]] / er) * (100 / defl)

    n_converted <- sum(!is.na(df[[new_col]]) & !is.na(df[[col]]))
    n_missing_rate <- sum(is.na(er) & !is.na(df[[col]]))

    if (n_missing_rate > 0) {
      cli::cli_alert_warning(
        "{.val {col}}: {.val {n_missing_rate}} obs without exchange rate (year not in table)"
      )
    }
  }

  cli::cli_alert_success(
    "Converted {.val {length(columns)}} columns: {.val {from_currency}} -> USD constant {.val {base_year}}"
  )

  if (register) qsr_data(df)
  invisible(df)
}


#' Run validation checks on survey data
#'
#' Framework for automated validation of cleaned survey data. Runs
#' predefined or custom checks and reports results. Designed to catch
#' the problems documented in DATA_CONSTRUCTION_PLAN.md.
#'
#' @param data Data frame or NULL (reads from context).
#' @param checks Named list of validation functions, or character
#'   preset: "basic", "survey", "panel". Each function takes a
#'   data frame and returns TRUE (pass) or a character message (fail).
#' @param stop_on_fail Logical. Stop execution if any check fails.
#'   Default FALSE (report all, continue).
#'
#' @return Invisible TRUE if all pass, FALSE if any fail. Prints report.
#'
#' @examples
#' \dontrun{
#' qsr_data(cleaned_survey)
#'
#' # Basic checks
#' qsr_validate(checks = "basic")
#'
#' # Custom checks
#' qsr_validate(checks = list(
#'   "No duplicate IDs" = function(df) {
#'     if (any(duplicated(df$id))) "Duplicate IDs found" else TRUE
#'   },
#'   "Income positive" = function(df) {
#'     if (any(df$income < 0, na.rm = TRUE)) "Negative income values" else TRUE
#'   },
#'   "Weight sums reasonable" = function(df) {
#'     ws <- sum(df$weight, na.rm = TRUE)
#'     if (ws < 1e6 || ws > 1e9) paste("Weight sum:", ws) else TRUE
#'   }
#' ))
#' }
#'
#' @export
qsr_validate <- function(data = NULL,
                         checks = "basic",
                         stop_on_fail = FALSE) {

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("No data.")
  }
  df <- as.data.frame(data)

  # Load preset checks
  if (is.character(checks)) {
    checks <- .qsr_validation_presets(checks, df)
  }

  cli::cli_h2("QUASAR Validation Report")
  cli::cli_text("Dataset: {.val {nrow(df)}} rows x {.val {ncol(df)}} cols")
  cli::cli_text("")

  pass_count <- 0L
  fail_count <- 0L
  results <- list()

  for (check_name in names(checks)) {
    result <- tryCatch(
      checks[[check_name]](df),
      error = function(e) paste("ERROR:", e$message)
    )

    if (isTRUE(result)) {
      cli::cli_alert_success("{check_name}")
      pass_count <- pass_count + 1L
      results[[check_name]] <- TRUE
    } else {
      cli::cli_alert_danger("{check_name}: {result}")
      fail_count <- fail_count + 1L
      results[[check_name]] <- result
    }
  }

  cli::cli_text("")
  cli::cli_text(
    "Results: {.val {pass_count}} passed, {.val {fail_count}} failed"
  )

  if (fail_count > 0 && stop_on_fail) {
    cli::cli_abort("Validation failed. Fix issues before proceeding.")
  }

  invisible(fail_count == 0)
}


#' Log a research decision
#'
#' Creates a structured entry in the decision log markdown file.
#' Essential for replication packages and reviewer responses.
#'
#' @param id Character. Decision ID (e.g., "BOL-D001").
#' @param title Character. Short title.
#' @param variable Character. Variable(s) affected.
#' @param waves Character. Waves/years affected.
#' @param problem Character. Description of the problem.
#' @param evidence Character. How you detected it.
#' @param decision Character. What you decided to do.
#' @param code_ref Character. File and line reference.
#' @param impact_n Character. Impact on sample size.
#' @param alternative Character. Alternative considered and why rejected.
#' @param log_file Character. Path to decision log. Default
#'   "decisions_log.md".
#'
#' @return Invisible path to log file.
#'
#' @examples
#' \dontrun{
#' qsr_decision_log(
#'   id = "BOL-D001",
#'   title = "Income reference period correction",
#'   variable = "ingreso_laboral, ingreso_total",
#'   waves = "2012 onwards",
#'   problem = "Reference period changed from monthly to weekly without documentation",
#'   evidence = "Mean income 2011=1847 BOB vs 2012=412 BOB (4.5x ratio)",
#'   decision = "Multiply 2012+ incomes by 4.3 (weeks per month)",
#'   code_ref = "harmonize_bol.R > fix_income_reference_period() > line 87",
#'   impact_n = "None (no observations dropped)",
#'   alternative = "Exclude 2012-2023. Rejected: loses 11 years of data."
#' )
#' }
#'
#' @export
qsr_decision_log <- function(id,
                             title,
                             variable,
                             waves,
                             problem,
                             evidence,
                             decision,
                             code_ref = "",
                             impact_n = "",
                             alternative = "",
                             log_file = "decisions_log.md") {

  entry <- paste0(
    "\n### [", id, "] ", title, "\n\n",
    "**Date:** ", Sys.Date(), "\n",
    "**Variable affected:** `", variable, "`\n",
    "**Waves affected:** ", waves, "\n",
    "**Problem:** ", problem, "\n",
    "**Evidence:** ", evidence, "\n",
    "**Decision:** ", decision, "\n",
    if (nzchar(code_ref)) paste0("**Code:** ", code_ref, "\n") else "",
    if (nzchar(impact_n)) paste0("**Impact on N:** ", impact_n, "\n") else "",
    if (nzchar(alternative)) paste0("**Alternative considered:** ", alternative, "\n") else "",
    "\n---\n"
  )

  # Create file with header if it doesn't exist
  if (!file.exists(log_file)) {
    header <- paste0(
      "# Decision Log - Research Data Cleaning\n\n",
      "Author: Juan Marcelo Gutierrez Miranda\n",
      "Generated by QUASAR | qsr_decision_log()\n\n",
      "This log documents every non-obvious data cleaning decision.\n",
      "It is part of the replication package.\n\n---\n"
    )
    writeLines(header, log_file)
  }

  cat(entry, file = log_file, append = TRUE)

  cli::cli_alert_success("Decision {.val {id}} logged to {.file {log_file}}")
  invisible(log_file)
}


# ============================================================
# Internal helpers
# ============================================================

#' @keywords internal
.qsr_validation_presets <- function(preset, df) {
  basic <- list(
    "No empty dataset" = function(d) {
      if (nrow(d) == 0) "Dataset is empty" else TRUE
    },
    "No all-NA columns" = function(d) {
      all_na <- names(d)[colSums(!is.na(d)) == 0]
      if (length(all_na) > 0) paste("All-NA columns:", paste(all_na, collapse = ", ")) else TRUE
    },
    "No duplicate rows" = function(d) {
      n_dup <- sum(duplicated(d))
      if (n_dup > 0) paste(n_dup, "duplicate rows") else TRUE
    },
    "Reasonable dimensions" = function(d) {
      if (nrow(d) < 10) "Less than 10 rows" else TRUE
    }
  )

  survey <- c(basic, list(
    "Has weight column" = function(d) {
      weight_names <- c("weight", "factor", "fexp", "factor_expansion",
                        "factor07", "fex_c")
      if (!any(weight_names %in% names(d))) "No weight column found" else TRUE
    },
    "Weights sum > 0" = function(d) {
      weight_names <- c("weight", "factor", "fexp", "factor_expansion",
                        "factor07", "fex_c")
      w_col <- intersect(weight_names, names(d))[1]
      if (is.na(w_col)) return("No weight column")
      ws <- sum(d[[w_col]], na.rm = TRUE)
      if (ws <= 0) paste("Weight sum:", ws) else TRUE
    },
    "No negative monetary values" = function(d) {
      income_names <- grep("ingreso|income|salario|wage", names(d),
                           ignore.case = TRUE, value = TRUE)
      if (length(income_names) == 0) return(TRUE)
      for (col in income_names) {
        if (any(d[[col]] < 0, na.rm = TRUE)) {
          return(paste("Negative values in", col))
        }
      }
      TRUE
    },
    "NA rate < 50% per column" = function(d) {
      na_rates <- colMeans(is.na(d))
      high_na <- names(na_rates[na_rates > 0.5])
      if (length(high_na) > 0) {
        paste("High NA (>50%):", paste(utils::head(high_na, 5), collapse = ", "))
      } else TRUE
    }
  ))

  panel <- c(survey, list(
    "Has year column" = function(d) {
      if (!"year" %in% names(d)) "No 'year' column" else TRUE
    },
    "Year range reasonable" = function(d) {
      if (!"year" %in% names(d)) return("No year column")
      yr <- range(d$year, na.rm = TRUE)
      if (yr[2] - yr[1] > 50) paste("Year range too wide:", yr[1], "-", yr[2]) else TRUE
    },
    "Balanced across years" = function(d) {
      if (!"year" %in% names(d)) return("No year column")
      counts <- table(d$year)
      ratio <- max(counts) / min(counts)
      if (ratio > 10) paste("Unbalanced: max/min ratio =", round(ratio, 1)) else TRUE
    }
  ))

  switch(preset,
    basic = basic,
    survey = survey,
    panel = panel,
    cli::cli_abort("Unknown preset: {.val {preset}}. Use: basic, survey, panel.")
  )
}

#' @keywords internal
.qsr_builtin_exchange_rates <- function(currency) {
  # Annual average exchange rates (local currency per 1 USD)
  # Source: World Bank / Central Banks
  rates <- list(
    BOB = c("2006"=8.01,"2007"=7.85,"2008"=7.25,"2009"=7.02,"2010"=7.02,
            "2011"=6.94,"2012"=6.91,"2013"=6.91,"2014"=6.91,"2015"=6.91,
            "2016"=6.91,"2017"=6.91,"2018"=6.91,"2019"=6.91,"2020"=6.91,
            "2021"=6.91,"2022"=6.91,"2023"=6.91),
    PEN = c("2006"=3.27,"2007"=3.13,"2008"=2.93,"2009"=3.01,"2010"=2.83,
            "2011"=2.75,"2012"=2.64,"2013"=2.70,"2014"=2.84,"2015"=3.19,
            "2016"=3.38,"2017"=3.26,"2018"=3.29,"2019"=3.34,"2020"=3.50,
            "2021"=3.88,"2022"=3.83,"2023"=3.74),
    COP = c("2006"=2358,"2007"=2078,"2008"=1967,"2009"=2157,"2010"=1899,
            "2011"=1848,"2012"=1798,"2013"=1869,"2014"=2001,"2015"=2742,
            "2016"=3055,"2017"=2951,"2018"=2956,"2019"=3281,"2020"=3693,
            "2021"=3743,"2022"=4255,"2023"=4327),
    BRL = c("2006"=2.18,"2007"=1.95,"2008"=1.83,"2009"=2.00,"2010"=1.76,
            "2011"=1.67,"2012"=1.95,"2013"=2.16,"2014"=2.35,"2015"=3.33,
            "2016"=3.49,"2017"=3.19,"2018"=3.65,"2019"=3.94,"2020"=5.15,
            "2021"=5.39,"2022"=5.16,"2023"=4.99)
  )

  currency <- toupper(currency)
  if (!currency %in% names(rates)) {
    cli::cli_abort(c(
      "No built-in exchange rates for {.val {currency}}.",
      "i" = "Available: {paste(names(rates), collapse = ', ')}",
      "i" = "Pass custom rates via {.arg exchange_rates} parameter."
    ))
  }
  rates[[currency]]
}

#' @keywords internal
.qsr_builtin_deflator <- function(currency, base_year) {
  # CPI deflator (base_year = 100)
  # Source: World Bank WDI FP.CPI.TOTL
  cpi_raw <- list(
    BOB = c("2006"=56.3,"2007"=62.8,"2008"=71.7,"2009"=74.1,"2010"=76.8,
            "2011"=84.2,"2012"=88.0,"2013"=93.0,"2014"=98.2,"2015"=100.0,
            "2016"=103.6,"2017"=106.5,"2018"=108.8,"2019"=110.7,"2020"=111.4,
            "2021"=112.3,"2022"=116.0,"2023"=119.5),
    PEN = c("2006"=73.0,"2007"=74.2,"2008"=78.5,"2009"=78.7,"2010"=79.9,
            "2011"=82.6,"2012"=85.5,"2013"=87.9,"2014"=90.7,"2015"=100.0,
            "2016"=103.2,"2017"=106.4,"2018"=107.8,"2019"=109.9,"2020"=111.7,
            "2021"=116.1,"2022"=125.5,"2023"=132.8),
    COP = c("2006"=68.5,"2007"=72.3,"2008"=77.4,"2009"=80.8,"2010"=82.7,
            "2011"=85.5,"2012"=88.2,"2013"=90.0,"2014"=92.6,"2015"=100.0,
            "2016"=107.5,"2017"=112.2,"2018"=115.8,"2019"=119.8,"2020"=122.8,
            "2021"=127.2,"2022"=140.5,"2023"=159.0),
    BRL = c("2006"=65.8,"2007"=68.2,"2008"=71.9,"2009"=75.4,"2010"=79.2,
            "2011"=84.4,"2012"=88.9,"2013"=94.2,"2014"=100.2,"2015"=100.0,
            "2016"=108.7,"2017"=112.3,"2018"=116.4,"2019"=120.7,"2020"=124.5,
            "2021"=134.8,"2022"=147.3,"2023"=154.0)
  )

  currency <- toupper(currency)
  if (!currency %in% names(cpi_raw)) {
    cli::cli_abort("No built-in CPI for {.val {currency}}.")
  }

  raw <- cpi_raw[[currency]]
  base_val <- raw[as.character(base_year)]
  if (is.na(base_val)) {
    cli::cli_abort("No CPI data for base year {.val {base_year}}")
  }

  # Rebase to base_year = 100
  raw / base_val * 100
}
