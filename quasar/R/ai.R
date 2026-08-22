# ============================================================
# QUASAR - AI Module
# qsr_ai_review(), qsr_ai_classify(), qsr_ai_flag()
# Uses Claude API for intelligent codebook review,
# text classification, and data quality checks.
# ============================================================


# ---- qsr_ai_review() -------------------------------------------------------

#' AI-assisted codebook/questionnaire review
#'
#' Sends two documents (e.g., codebooks from different survey waves) to
#' Claude for comparison. Returns structured findings about variable
#' changes, question rewording, and potential data quality issues.
#'
#' @param doc_before Path to the earlier document (PDF, text, or CSV).
#' @param doc_after Path to the later document (PDF, text, or CSV).
#' @param focus Character. What to focus on: `"all"`, `"income"`,
#'   `"education"`, `"employment"`, `"ethnicity"`, `"agriculture"`.
#' @param api_key Character. Anthropic API key. If `NULL`, reads from
#'   `ANTHROPIC_API_KEY` environment variable.
#' @param model Character. Claude model to use. Default `"claude-sonnet-4-6-20250514"`.
#' @param provider Character. `"anthropic"` (default, Claude API) or `"ollama"`
#'   (a local Ollama server -- no API key, nothing leaves the machine; set the
#'   model with the `QSR_OLLAMA_MODEL` env var and the host with `OLLAMA_HOST`).
#'
#' @return A list with `findings` (character vector of changes detected),
#'   `severity` (high/medium/low for each), and `raw_response`.
#' @export
#'
#' @examples
#' \dontrun{
#' qsr_ai_review(
#'   doc_before = "codebook_2011.pdf",
#'   doc_after  = "codebook_2012.pdf",
#'   focus      = "income"
#' )
#' }
qsr_ai_review <- function(doc_before,
                           doc_after,
                           focus    = "all",
                           api_key  = NULL,
                           model    = "claude-sonnet-4-6-20250514",
                           provider = c("anthropic", "ollama")) {

  provider <- match.arg(provider)
  api_key <- api_key %||% Sys.getenv("ANTHROPIC_API_KEY")
  if (provider == "anthropic" && api_key == "") {
    cli::cli_abort(c(
      "Anthropic API key not found.",
      "i" = "Set it with: {.code Sys.setenv(ANTHROPIC_API_KEY = 'your-key')}",
      "i" = "Or pass it directly: {.code qsr_ai_review(..., api_key = 'your-key')}",
      "i" = "Or run locally: {.code qsr_ai_review(..., provider = 'ollama')}"
    ))
  }

  .qsr_require("httr2", "for AI module")

  # Read documents
  text_before <- .qsr_ai_read_doc(doc_before)
  text_after  <- .qsr_ai_read_doc(doc_after)

  focus_instruction <- if (focus != "all") {
    sprintf("Focus specifically on %s-related variables and questions.", focus)
  } else {
    "Review all variables and questions."
  }

  prompt <- sprintf(
    "You are a survey methodologist reviewing two versions of a household survey codebook/questionnaire.

DOCUMENT 1 (earlier wave):
%s

DOCUMENT 2 (later wave):
%s

TASK: %s

For each change you find, report:
1. VARIABLE: name/code of the variable affected
2. CHANGE: what changed (question wording, response codes, reference period, skip pattern)
3. SEVERITY: HIGH (breaks comparability), MEDIUM (requires adjustment), LOW (cosmetic)
4. RECOMMENDATION: how to handle this in a harmonized panel

Be specific. Include variable codes. Flag any change in reference period (e.g., monthly to weekly income) as HIGH severity.
Report findings as a numbered list.",
    substr(text_before, 1, 15000),
    substr(text_after, 1, 15000),
    focus_instruction
  )

  response <- .qsr_ai_call(prompt, api_key, model, provider)

  # Parse findings
  findings <- strsplit(response, "\n")[[1]]
  findings <- findings[nchar(trimws(findings)) > 0]

  severity <- vapply(findings, function(f) {
    if (grepl("HIGH", f, ignore.case = TRUE)) "high"
    else if (grepl("MEDIUM", f, ignore.case = TRUE)) "medium"
    else "low"
  }, character(1))

  cli::cli_alert_success("AI review complete: {length(findings)} findings")

  list(
    findings     = findings,
    severity     = unname(severity),
    raw_response = response
  )
}


# ---- qsr_ai_classify() -----------------------------------------------------

#' AI-assisted text classification
#'
#' Uses Claude to classify free-text fields (crop names, ethnicities, etc.)
#' into standardized categories. Handles typos, abbreviations, and
#' dialectal variants.
#'
#' @param data A data frame.
#' @param column Character. Name of the text column to classify.
#' @param categories Character vector. Target categories.
#' @param context Character. Cultural/geographic context to help
#'   disambiguation (e.g., "Rural Bolivian agricultural survey").
#' @param api_key Character. Anthropic API key.
#' @param batch_size Integer. Number of unique values to send per API call.
#'   Default 100.
#' @param provider Character. `"anthropic"` (default) or `"ollama"` (local, no
#'   API key -- data never leaves the machine).
#'
#' @return The input data frame with a new column `{column}_classified`
#'   and `{column}_confidence`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- qsr_ai_classify(
#'   data       = survey_data,
#'   column     = "crop_name",
#'   categories = c("coca", "coffee", "cacao", "banana", "other"),
#'   context    = "Rural Bolivian survey, Chapare region"
#' )
#' }
qsr_ai_classify <- function(data,
                              column,
                              categories,
                              context   = "",
                              api_key   = NULL,
                              batch_size = 100,
                              provider  = c("anthropic", "ollama")) {

  provider <- match.arg(provider)
  api_key <- api_key %||% Sys.getenv("ANTHROPIC_API_KEY")
  if (provider == "anthropic" && api_key == "") {
    cli::cli_abort("Anthropic API key not found. Set ANTHROPIC_API_KEY (or use provider = 'ollama').")
  }

  .qsr_require("httr2", "for AI module")

  if (!column %in% names(data)) {
    cli::cli_abort("Column {.val {column}} not found in data.")
  }

  # Get unique values to classify
  unique_vals <- unique(as.character(data[[column]]))
  unique_vals <- unique_vals[!is.na(unique_vals) & nchar(trimws(unique_vals)) > 0]

  cli::cli_alert_info("Classifying {length(unique_vals)} unique values into {length(categories)} categories")

  # Process in batches
  results <- list()
  for (i in seq(1, length(unique_vals), by = batch_size)) {
    batch <- unique_vals[i:min(i + batch_size - 1, length(unique_vals))]

    prompt <- sprintf(
      "Classify each text value into one of these categories: %s

Context: %s

For each value, respond with EXACTLY this format (one per line):
VALUE | CATEGORY | CONFIDENCE

Where CONFIDENCE is high, medium, or low.

Values to classify:
%s",
      paste(categories, collapse = ", "),
      context,
      paste(batch, collapse = "\n")
    )

    response <- .qsr_ai_call(prompt, api_key, "claude-haiku-4-5-20251001", provider)

    # Parse response
    lines <- strsplit(response, "\n")[[1]]
    for (line in lines) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 3) {
        results[[parts[1]]] <- list(
          category   = parts[2],
          confidence = parts[3]
        )
      }
    }
  }

  # Map back to data
  new_col <- paste0(column, "_classified")
  conf_col <- paste0(column, "_confidence")
  data[[new_col]]  <- vapply(as.character(data[[column]]), function(v) {
    if (v %in% names(results)) results[[v]]$category else NA_character_
  }, character(1))
  data[[conf_col]] <- vapply(as.character(data[[column]]), function(v) {
    if (v %in% names(results)) results[[v]]$confidence else NA_character_
  }, character(1))

  cli::cli_alert_success("Classification complete: {sum(!is.na(data[[new_col]]))} values classified")
  data
}


# ---- qsr_ai_flag() ----------------------------------------------------------

#' AI-assisted data quality flagging
#'
#' Sends summary statistics and distribution anomalies to Claude for
#' interpretation. Returns human-readable explanations of potential
#' data quality issues.
#'
#' @param data A data frame.
#' @param checks Character. Type of checks: `"distributions"`,
#'   `"outliers"`, `"wave_breaks"`, `"all"`.
#' @param context Character. Context about the survey.
#' @param api_key Character. Anthropic API key.
#' @param provider Character. `"anthropic"` (default) or `"ollama"` (local, no
#'   API key -- data never leaves the machine).
#'
#' @return A data frame with columns: `variable`, `issue`, `severity`,
#'   `recommendation`.
#' @export
#'
#' @examples
#' \dontrun{
#' flags <- qsr_ai_flag(
#'   data    = panel,
#'   checks  = "wave_breaks",
#'   context = "Bolivia EH household survey, 2006-2024"
#' )
#' }
qsr_ai_flag <- function(data,
                          checks  = "all",
                          context = "",
                          api_key = NULL,
                          provider = c("anthropic", "ollama")) {

  provider <- match.arg(provider)
  api_key <- api_key %||% Sys.getenv("ANTHROPIC_API_KEY")
  if (provider == "anthropic" && api_key == "") {
    cli::cli_abort("Anthropic API key not found. Set ANTHROPIC_API_KEY (or use provider = 'ollama').")
  }

  .qsr_require("httr2", "for AI module")

  # Build summary statistics
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  summary_text <- character()

  for (col in num_cols[1:min(20, length(num_cols))]) {
    vals <- data[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) next

    s <- sprintf("%s: N=%d, mean=%.2f, sd=%.2f, min=%.2f, max=%.2f, NA%%=%.1f",
                 col, length(vals), mean(vals), sd(vals),
                 min(vals), max(vals),
                 mean(is.na(data[[col]])) * 100)
    summary_text <- c(summary_text, s)

    # Check for year-level breaks if 'year' column exists
    if ("year" %in% names(data)) {
      by_year <- tapply(data[[col]], data[["year"]], function(x) {
        c(mean = mean(x, na.rm = TRUE), pct_na = mean(is.na(x)) * 100)
      })
      means <- vapply(by_year, `[`, numeric(1), "mean")
      if (any(diff(means) / means[-length(means)] > 2, na.rm = TRUE)) {
        s <- sprintf("  -> %s has >2x jump between consecutive years!", col)
        summary_text <- c(summary_text, s)
      }
    }
  }

  prompt <- sprintf(
    "You are a data quality expert reviewing summary statistics from a household survey panel.

Context: %s

Summary statistics:
%s

Identify potential data quality issues. For each issue, report:
VARIABLE: [name]
ISSUE: [description]
SEVERITY: HIGH / MEDIUM / LOW
RECOMMENDATION: [what to do]

Focus on: impossible values, distribution anomalies, sudden jumps between waves, suspiciously high NA rates, and potential measurement changes.",
    context,
    paste(summary_text, collapse = "\n")
  )

  response <- .qsr_ai_call(prompt, api_key, "claude-sonnet-4-6-20250514", provider)

  # Parse into structured output
  lines <- strsplit(response, "\n")[[1]]
  flags <- list()
  current <- list()

  for (line in lines) {
    line <- trimws(line)
    if (grepl("^VARIABLE:", line)) {
      if (length(current) > 0) flags[[length(flags) + 1]] <- current
      current <- list(variable = trimws(sub("VARIABLE:", "", line)))
    } else if (grepl("^ISSUE:", line)) {
      current$issue <- trimws(sub("ISSUE:", "", line))
    } else if (grepl("^SEVERITY:", line)) {
      current$severity <- trimws(sub("SEVERITY:", "", line))
    } else if (grepl("^RECOMMENDATION:", line)) {
      current$recommendation <- trimws(sub("RECOMMENDATION:", "", line))
    }
  }
  if (length(current) > 0) flags[[length(flags) + 1]] <- current

  result <- do.call(rbind, lapply(flags, as.data.frame, stringsAsFactors = FALSE))
  if (is.null(result) || nrow(result) == 0) {
    result <- data.frame(variable = character(), issue = character(),
                          severity = character(), recommendation = character())
  }

  cli::cli_alert_success("AI flagging complete: {nrow(result)} issues found")
  result
}


# ============================================================
# INTERNAL HELPERS - AI
# ============================================================

#' Call the LLM backend (Anthropic Claude, or a local Ollama server)
#'
#' provider = "ollama" sends the prompt to a local Ollama server
#' (default http://localhost:11434, override with OLLAMA_HOST). Nothing leaves
#' the machine and no API key is used. provider = "anthropic" (default) calls
#' the Claude API with the key passed as a header (never logged or returned).
#' @noRd
.qsr_ai_call <- function(prompt, api_key = NULL, model = NULL,
                         provider = c("anthropic", "ollama")) {
  provider <- match.arg(provider)

  if (provider == "ollama") {
    host <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434")
    # Ignore Claude model names passed by the callers; use a local model.
    if (is.null(model) || startsWith(model, "claude")) {
      model <- Sys.getenv("QSR_OLLAMA_MODEL", "llama3.1")
    }
    resp <- httr2::request(paste0(host, "/api/chat")) |>
      httr2::req_body_json(list(
        model    = model,
        messages = list(list(role = "user", content = prompt)),
        stream   = FALSE
      )) |>
      httr2::req_timeout(300) |>
      httr2::req_perform()
    parsed <- httr2::resp_body_json(resp)
    return(parsed$message$content)
  }

  # Anthropic (Claude)
  model <- model %||% "claude-sonnet-4-6-20250514"
  body <- list(
    model = model,
    max_tokens = 4096,
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key`         = api_key,
      `anthropic-version`  = "2023-06-01",
      `content-type`       = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(120) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  parsed$content[[1]]$text
}

#' Read a document for AI processing
#' @noRd
.qsr_ai_read_doc <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    pdf = {
      if (.qsr_has_pkg("pdftools")) {
        paste(pdftools::pdf_text(path), collapse = "\n")
      } else {
        cli::cli_abort("Install {.pkg pdftools} to read PDFs: {.code install.packages('pdftools')}")
      }
    },
    csv = {
      paste(readLines(path, n = 500), collapse = "\n")
    },
    txt = paste(readLines(path), collapse = "\n"),
    md  = paste(readLines(path), collapse = "\n"),
    sav = {
      if (.qsr_has_pkg("haven")) {
        df <- haven::read_sav(path)
        # Return variable names + labels as text
        labels <- vapply(names(df), function(n) {
          lbl <- attr(df[[n]], "label")
          if (!is.null(lbl)) paste0(n, ": ", lbl) else n
        }, character(1))
        paste(labels, collapse = "\n")
      } else {
        cli::cli_abort("Install {.pkg haven} to read .sav files")
      }
    },
    {
      # Try reading as text
      tryCatch(
        paste(readLines(path, n = 1000), collapse = "\n"),
        error = function(e) cli::cli_abort("Cannot read file: {.path {path}}")
      )
    }
  )
}
