# ============================================================
# QUASAR - Rust-Powered Engine (Phase 2)
# High-performance data operations via Polars
# The user calls qsr_read() - Rust does the heavy lifting
# ============================================================

# Adaptive Process Isolation:
# - Small files (<10MB): direct call, single thread (fast, zero overhead)
# - Large files (>10MB): subprocess via callr with ALL CPU cores
# This prevents segfaults while maximizing performance.
# Nobody else does this in R.

# Size threshold for switching to subprocess (bytes)
.QSR_PARALLEL_THRESHOLD <- 128 * 1024 * 1024  # 128 MB (below this, run in-process:
# the callr subprocess startup would dominate the Polars computation for medium files)

#' Run a Rust/Polars function in an isolated subprocess
#'
#' Uses callr to execute Polars operations in a separate R process,
#' allowing full multi-threading without risk of GC conflicts.
#'
#' @param rust_fn Character. Name of the internal Rust function.
#' @param args List. Arguments to pass to the Rust function.
#' @param n_cores Integer. Number of cores to use. Default: all available.
#'
#' @return The result from the Rust function.
#'
#' @keywords internal
.qsr_run_isolated <- function(rust_fn, args, n_cores = parallel::detectCores()) {
  callr::r(
    function(fn_name, fn_args, cores) {
      Sys.setenv(POLARS_MAX_THREADS = as.character(cores))
      fn <- get(fn_name, envir = asNamespace("rquasar"))
      do.call(fn, fn_args)
    },
    args = list(fn_name = rust_fn, fn_args = args, cores = n_cores),
    package = "rquasar"
  )
}

#' Transcode a text file to a temporary UTF-8 copy (streaming, constant memory)
#'
#' Polars assumes UTF-8. Source files in single-byte encodings (latin1/ISO-8859-1,
#' windows-1252) are converted here in binary chunks so accents and " n-tilde" survive
#' and memory stays flat regardless of file size. Returns the temp file path.
#'
#' @param path Character. Source file path.
#' @param from Character. Source encoding (e.g. "latin1").
#' @param n_lines Integer or NULL. If set, transcode only the first `n_lines` + 1
#'   lines (header + rows) instead of the whole file -- keeps sampling cheap on huge files.
#' @param chunk Integer. Bytes per chunk for the full-file path. Default 32 MiB.
#'
#' @return Character. Path to the temporary UTF-8 file (caller unlinks it).
#'
#' @keywords internal
.qsr_transcode_utf8 <- function(path, from, n_lines = NULL, chunk = 32L * 1024L^2) {
  out <- tempfile(fileext = paste0(".", tools::file_ext(path)))

  # Sampling: only the first header + n_lines rows are needed. Read them via a
  # connection in the source encoding and write a UTF-8 connection -- no full pass.
  if (!is.null(n_lines)) {
    con_in <- file(path, "r", encoding = from)
    con_out <- file(out, "w", encoding = "UTF-8")
    on.exit({ close(con_in); close(con_out) }, add = TRUE)
    lines <- readLines(con_in, n = as.integer(n_lines) + 1L, warn = FALSE)
    writeLines(lines, con_out)
    return(out)
  }

  # Full file: transcode in Rust (streaming, flat memory). ~80x faster than the old R
  # `rawToChar` loop on large inputs, and it STRIPS embedded NUL instead of aborting -- so a
  # partially corrupt file (e.g. a truncated disk-recovery copy) still yields its readable
  # rows. Single-byte encodings never split a code point across a chunk boundary.
  transcode <- get("rust_transcode_utf8", envir = asNamespace("rquasar"))
  transcode(path, out, from)
  out
}

#' Decide whether to run in-process or isolated based on file size
#'
#' @param path Character. File path.
#' @param rust_fn Character. Rust function name.
#' @param args List. Arguments for the Rust function.
#'
#' @return The result from the Rust function.
#'
#' @keywords internal
.qsr_adaptive_call <- function(path, rust_fn, args) {
  file_size <- file.info(path)$size

  if (!is.na(file_size) && file_size > .QSR_PARALLEL_THRESHOLD &&
      requireNamespace("callr", quietly = TRUE)) {
    # Large file: subprocess with full multi-threading
    tryCatch(
      .qsr_run_isolated(rust_fn, args),
      error = function(e) {
        # Fallback to direct call if subprocess fails
        # (e.g., package not formally installed during development)
        fn <- get(rust_fn, envir = asNamespace("rquasar"))
        do.call(fn, args)
      }
    )
  } else {
    # Small file or callr not available: direct single-threaded call
    fn <- get(rust_fn, envir = asNamespace("rquasar"))
    do.call(fn, args)
  }
}

#' Read data files at high speed using Polars (Rust backend)
#'
#' Reads CSV, Parquet, or JSON files using the Polars engine written in Rust.
#' Typically 10-30x faster than `read.csv()` and 2-5x faster than
#' `data.table::fread()` on large files.
#'
#' @param path Character. Path to the data file.
#' @param format Character. File format: "csv", "parquet", or "auto" (default).
#'   When "auto", the format is detected from the file extension.
#' @param n_rows Integer or NULL. Maximum number of rows to read. Default NULL
#'   reads all rows.
#' @param columns Character vector or NULL. Column names to select. Default NULL
#'   reads all columns. For CSV, the projection is pushed down to the Polars scan,
#'   so only these columns are read from disk (key for wide files).
#' @param delim Character or NULL. CSV field delimiter (e.g. "|", ";", "\\t").
#'   Default NULL uses "," (or "\\t" for a `.tsv` file).
#' @param encoding Character. Source file encoding. Default "utf8". Any other value
#'   (e.g. "latin1"/"ISO-8859-1") triggers a streaming transcode to a temporary
#'   UTF-8 copy before reading, preserving accents and "ñ".
#' @param name Character. Name to register the data under in the QUASAR context.
#'   Default "data".
#' @param register Logical. Whether to auto-register in context via
#'   `qsr_data()`. Default TRUE.
#'
#' @return A data.frame (invisibly). Also registered in context if
#'   `register = TRUE`.
#'
#' @details
#' The Rust backend uses Polars lazy evaluation - it only reads the columns
#' and rows you need, which makes it extremely efficient for large files.
#'
#' Supported formats:
#' - **CSV**: `.csv`, `.tsv`, `.txt`
#' - **Parquet**: `.parquet`, `.pq`
#' - **SPSS**: `.sav`, `.zsav`, `.por` (via haven; survey microdata)
#' - **Stata**: `.dta` (via haven)
#' - **SAS**: `.sas7bdat`, `.xpt` (via haven)
#'
#' For fixed-width (positional) survey files, use [qsr_read_fwf()].
#'
#' @examples
#' \dontrun{
#' # Read a CSV - 10-30x faster than read.csv
#' qsr_read("data/large_file.csv")
#'
#' # Read only specific columns
#' qsr_read("data/big.csv", columns = c("id", "value", "date"))
#'
#' # Read first 1000 rows
#' qsr_read("data/big.csv", n_rows = 1000)
#'
#' # Read Parquet (auto-detected from extension)
#' qsr_read("data/results.parquet")
#'
#' # After qsr_read(), data is in context - use qsr_table(), qsr_plot() directly
#' qsr_read("data/survey.csv")
#' qsr_table(type = "summary")
#' qsr_plot(type = "correlation")
#' }
#'
#' @export
qsr_read <- function(path,
                     format = "auto",
                     n_rows = NULL,
                     columns = NULL,
                     delim = NULL,
                     encoding = "utf8",
                     name = "data",
                     register = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "File not found: {.file {path}}",
      "i" = "Check the file path and try again."
    ))
  }

  # Detect format from extension
  ext <- tolower(tools::file_ext(path))
  if (format == "auto") {
    format <- switch(ext,
      csv = , tsv = , txt = "csv",
      parquet = , pq = "parquet",
      sav = , zsav = , por = , dta = , sas7bdat = , xpt = "haven",
      cli::cli_abort(c(
        "Cannot auto-detect format for extension {.val .{ext}}",
        "i" = "Supported: {.val csv}, {.val parquet}, and stat packages ({.val .sav}, {.val .dta}, {.val .sas7bdat}, {.val .xpt}, {.val .por})",
        "i" = "For fixed-width files use {.fn qsr_read_fwf}"
      ))
    )
  }

  # Survey microdata from statistical packages: SPSS (.sav/.zsav/.por), Stata
  # (.dta) and SAS (.sas7bdat/.xpt) via haven. These are the native formats of
  # ENAHO/EH/EPA-style surveys, so qsr_read handles them directly.
  if (format %in% c("haven", "spss", "stata", "sas")) {
    return(.qsr_read_haven(path, n_rows, columns, name, register))
  }

  # Resolve delimiter: explicit `delim` wins; else tab for .tsv, comma otherwise.
  separator <- if (!is.null(delim)) delim else if (identical(ext, "tsv")) "\t" else ","

  # Encoding: Polars assumes UTF-8. For other encodings (e.g. latin1) transcode to a
  # temporary UTF-8 copy in a streaming pass, so accents/ñ survive and memory stays flat.
  read_path <- path
  if (format == "csv" && !tolower(encoding) %in% c("utf8", "utf-8", "")) {
    read_path <- .qsr_transcode_utf8(path, from = encoding, n_lines = n_rows)
    on.exit(unlink(read_path), add = TRUE)
  }

  # Call Rust backend - adaptive: subprocess for large files, direct for small
  result <- switch(format,
    csv = .qsr_adaptive_call(read_path, "rust_read_csv",
                             list(read_path, n_rows, separator, columns)),
    parquet = .qsr_adaptive_call(path, "rust_read_parquet", list(path)),
    cli::cli_abort("Unsupported format: {.val {format}}")
  )

  # Extract metadata
  elapsed <- result[["_elapsed_secs"]]
  nrows <- result[["_nrows"]]

  # Remove metadata columns and convert to data.frame
  data_cols <- setdiff(names(result), c("_elapsed_secs", "_nrows"))
  df <- as.data.frame(result[data_cols], stringsAsFactors = FALSE)

  # Column projection is pushed down to the Polars scan (see rust_read_csv). This
  # R-side subset only re-orders/guards the result to the requested columns.
  if (!is.null(columns)) {
    keep <- intersect(columns, names(df))
    if (length(keep)) df <- df[, keep, drop = FALSE]
  }

  cli::cli_alert_success(
    "Read {.val {nrows}} rows x {.val {length(data_cols)}} cols from {.file {basename(path)}} in {.val {round(elapsed, 3)}}s {.emph (Polars/Rust)}"
  )


  # Register in context
  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


# Read statistical-package survey microdata (SPSS/Stata/SAS) via haven. Isolated
# so the haven dependency is only required when actually reading these formats.
# The reader is chosen from the file extension. Value labels are preserved
# (haven labelled vectors, which behave numerically downstream).
.qsr_read_haven <- function(path, n_rows, columns, name, register) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Reading statistical-package files needs the {.pkg haven} package.",
      "i" = "Install it with {.code install.packages(\"haven\")}."
    ))
  }
  ext <- tolower(tools::file_ext(path))
  reader <- switch(ext,
    sav = , zsav = haven::read_sav,
    por = haven::read_por,
    dta = haven::read_dta,
    sas7bdat = haven::read_sas,
    xpt = haven::read_xpt,
    cli::cli_abort("Unsupported statistical-package extension {.val .{ext}}.")
  )
  kind <- switch(ext, sav = , zsav = , por = "spss", dta = "stata",
                 sas7bdat = , xpt = "sas", ext)
  t0 <- proc.time()[["elapsed"]]
  args <- list(path)
  if (!is.null(columns)) args$col_select <- columns
  if (!is.null(n_rows)) args$n_max <- n_rows
  df <- as.data.frame(do.call(reader, args), stringsAsFactors = FALSE)
  elapsed <- proc.time()[["elapsed"]] - t0
  cli::cli_alert_success(
    "Read {.val {nrow(df)}} rows x {.val {ncol(df)}} cols from {.file {basename(path)}} in {.val {round(elapsed, 3)}}s {.emph (haven/{kind})}"
  )
  if (register) {
    qsr_data(df, name = name)
  }
  invisible(df)
}


#' Read a fixed-width (positional) file
#'
#' Reads fixed-width microdata — the native format of classic survey files
#' (EPF/EPA/ENAHO record layouts, INE "diseño de registro"), where each variable
#' occupies a fixed byte range. Fields are given either by `positions`
#' (start-end pairs, matching a record layout) or by `widths`. Encoding is
#' handled explicitly (survey files are often `latin1` or `cp850`).
#'
#' @param path Character. Path to the fixed-width text file.
#' @param positions List of length-2 numeric vectors `c(start, end)` (1-based,
#'   inclusive), or a two-column matrix/data.frame of start/end. One per field.
#' @param widths Integer vector of field widths (alternative to `positions`;
#'   fields are taken consecutively from column 1).
#' @param col_names Character vector of column names (one per field). Defaults to
#'   `V1, V2, ...`.
#' @param encoding Character. Source encoding for reading the lines. Default
#'   "latin1"; use "cp850" for old DOS survey layouts, "utf8" if already UTF-8.
#' @param n_rows Integer or NULL. Read at most this many rows (NULL = all).
#' @param trim Logical. Trim leading/trailing whitespace from each field. Default TRUE.
#' @param name Character. Name to register the data under in the context.
#' @param register Logical. Register the result in the context. Default TRUE.
#'
#' @return A data.frame (invisibly). All columns are character; cast as needed.
#'
#' @examples
#' \dontrun{
#' # EPF record layout: household id (1-5), province (7-8), status (15-16)
#' qsr_read_fwf(
#'   "TIPREG3.TXT",
#'   positions = list(c(1, 5), c(7, 8), c(15, 16)),
#'   col_names = c("hogar", "provincia", "situacion"),
#'   encoding = "cp850"
#' )
#' }
#'
#' @export
qsr_read_fwf <- function(path,
                         positions = NULL,
                         widths = NULL,
                         col_names = NULL,
                         encoding = "latin1",
                         n_rows = NULL,
                         trim = TRUE,
                         name = "data",
                         register = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "File not found: {.file {path}}",
      "i" = "Check the file path and try again."
    ))
  }
  if (is.null(positions) && is.null(widths)) {
    cli::cli_abort(c(
      "Provide either {.arg positions} (start-end pairs) or {.arg widths}.",
      "i" = "e.g. {.code positions = list(c(1, 5), c(7, 8))}"
    ))
  }

  # Resolve field start/end (1-based, inclusive) from positions or widths.
  if (!is.null(positions)) {
    if (is.matrix(positions) || is.data.frame(positions)) {
      pm <- as.matrix(positions)
      starts <- as.integer(pm[, 1]); ends <- as.integer(pm[, 2])
    } else {
      starts <- vapply(positions, function(p) as.integer(p[1]), integer(1))
      ends <- vapply(positions, function(p) as.integer(p[2]), integer(1))
    }
  } else {
    widths <- as.integer(widths)
    ends <- cumsum(widths)
    starts <- ends - widths + 1L
  }
  n_fields <- length(starts)
  if (is.null(col_names)) col_names <- paste0("V", seq_len(n_fields))
  if (length(col_names) != n_fields) {
    cli::cli_abort("{.arg col_names} has {length(col_names)} names but there are {n_fields} fields.")
  }

  t0 <- proc.time()[["elapsed"]]
  con <- file(path, "r", encoding = encoding)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, n = if (is.null(n_rows)) -1L else n_rows, warn = FALSE)

  cols <- lapply(seq_len(n_fields), function(i) {
    v <- substr(lines, starts[i], ends[i])
    if (trim) v <- trimws(v)
    v
  })
  names(cols) <- col_names
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  elapsed <- proc.time()[["elapsed"]] - t0

  cli::cli_alert_success(
    "Read {.val {nrow(df)}} rows x {.val {n_fields}} cols from {.file {basename(path)}} in {.val {round(elapsed, 3)}}s {.emph (fixed-width)}"
  )
  if (register) {
    qsr_data(df, name = name)
  }
  invisible(df)
}


#' Stream a large delimited file to Parquet (constant memory)
#'
#' Converts a big CSV/text file to Parquet using the Polars streaming engine,
#' without ever holding the whole dataset in memory. This is the recommended
#' entry point for files that are too large for `qsr_read()`: convert once to
#' Parquet (columnar, compressed, typed), then read/query the Parquet at speed.
#'
#' @param path Character. Source delimited file.
#' @param out Character. Destination Parquet path.
#' @param delim Character or NULL. Field delimiter (e.g. "|"). Default NULL uses
#'   "," (or "\\t" for a `.tsv` file).
#' @param columns Character vector or NULL. Columns to keep; pushed down to the
#'   scan so only these are read and written.
#' @param encoding Character. Source encoding. Default "utf8"; other values
#'   (e.g. "latin1") transcode to a temporary UTF-8 copy first.
#'
#' @return The output path (invisibly).
#'
#' @details
#' Memory stays bounded regardless of input size, so 10s of GB convert on a
#' laptop. When `encoding` needs transcoding the file is passed once through a
#' streaming UTF-8 conversion before the sink (two disk passes, still flat memory).
#'
#' @examples
#' \dontrun{
#' # 13 GB pipe-separated, latin1 -> keep 6 columns -> Parquet
#' qsr_sink_parquet(
#'   "EXTR_PB.csv", "bronze/extr_pb.parquet",
#'   delim = "|", encoding = "latin1",
#'   columns = c("TELEFONO", "PROVINCIA_NOR", "SEGMENTO_TELCO")
#' )
#' qsr_read("bronze/extr_pb.parquet")  # fast columnar read afterwards
#' }
#'
#' @export
qsr_sink_parquet <- function(path, out, delim = NULL, columns = NULL,
                             encoding = "utf8") {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }
  ext <- tolower(tools::file_ext(path))
  separator <- if (!is.null(delim)) delim else if (identical(ext, "tsv")) "\t" else ","

  read_path <- path
  if (!tolower(encoding) %in% c("utf8", "utf-8", "")) {
    read_path <- .qsr_transcode_utf8(path, from = encoding)  # full file: flat memory
    on.exit(unlink(read_path), add = TRUE)
  }

  cols <- if (is.null(columns)) NULL else as.character(columns)
  out_dir <- dirname(out)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  elapsed <- .qsr_adaptive_call(
    read_path, "rust_sink_parquet", list(read_path, out, separator, cols)
  )

  cli::cli_alert_success(
    "Streamed to {.file {basename(out)}} in {.val {elapsed}}s {.emph (streaming Polars/Rust)}"
  )
  invisible(out)
}


#' Fast summary statistics using Polars (Rust backend)
#'
#' Computes descriptive statistics (count, mean, std, min, max) using the
#' Polars engine. Useful for quick exploration of large files without loading
#' them fully into R.
#'
#' @param path Character. Path to a CSV file.
#'
#' @return A data.frame with summary statistics.
#'
#' @examples
#' \dontrun{
#' qsr_fast_summary("data/large_file.csv")
#' }
#'
#' @export
qsr_fast_summary <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  result <- .qsr_adaptive_call(path, "rust_describe", list(path))
  df <- data.frame(
    column = result[["column"]],
    count  = result[["count"]],
    mean   = result[["mean"]],
    std    = result[["std"]],
    min    = result[["min"]],
    max    = result[["max"]],
    stringsAsFactors = FALSE
  )

  cli::cli_alert_success("Summary of {.val {nrow(df)}} numeric columns via {.emph Polars/Rust}")
  df
}


#' Fast group-by aggregation using Polars (Rust backend)
#'
#' Performs group-by operations directly on a file using Polars - no need
#' to load the full dataset into R first.
#'
#' @param path Character. Path to a CSV file.
#' @param by Character. Column name to group by.
#' @param col Character. Column name to aggregate.
#' @param fn Character. Aggregation function: "mean", "sum", "count", "min",
#'   "max", "std".
#' @param name Character. Name for context registration. Default "grouped_data".
#' @param register Logical. Auto-register result in context. Default TRUE.
#'
#' @return A data.frame with grouped results (invisibly).
#'
#' @examples
#' \dontrun{
#' # Average salary by department
#' qsr_fast_group("employees.csv", by = "department", col = "salary", fn = "mean")
#'
#' # Then immediately plot or table it
#' qsr_table()
#' qsr_plot(type = "bar", x = department, y = salary_mean)
#' }
#'
#' @export
qsr_fast_group <- function(path,
                           by,
                           col,
                           fn = "mean",
                           name = "grouped_data",
                           register = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  fn <- match.arg(fn, c("mean", "sum", "count", "min", "max", "std"))

  # The Rust group engine scans CSV. For Parquet, read it (columnar) and
  # aggregate in R so `.parquet` inputs work instead of failing.
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "pq")) {
    t0 <- proc.time()[["elapsed"]]
    df <- .qsr_group_parquet(path, by, col, fn)
    cli::cli_alert_success(
      "Grouped {.val {col}} by {.val {by}} ({fn}) in {.val {round(proc.time()[['elapsed']] - t0, 3)}}s {.emph (Parquet, in-memory)}"
    )
    if (register) {
      qsr_data(df, name = name)
    }
    return(invisible(df))
  }

  result <- .qsr_adaptive_call(path, "rust_group_agg", list(path, by, col, fn))
  elapsed <- result[["_elapsed_secs"]]
  data_cols <- setdiff(names(result), c("_elapsed_secs", "_nrows"))
  df <- as.data.frame(result[data_cols], stringsAsFactors = FALSE)

  cli::cli_alert_success(
    "Grouped {.val {col}} by {.val {by}} ({fn}) in {.val {round(elapsed, 3)}}s {.emph (Polars/Rust)}"
  )

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


# Group-aggregate a Parquet file in R (the Rust engine only scans CSV). Reads
# the columnar file via the Parquet reader, then aggregates with base R.
# Output shape matches the CSV path: `by` columns + a `<col>_<fn>` column.
.qsr_group_parquet <- function(path, by, col, fn) {
  res <- .qsr_adaptive_call(path, "rust_read_parquet", list(path))
  data_cols <- setdiff(names(res), c("_elapsed_secs", "_nrows"))
  df <- as.data.frame(res[data_cols], stringsAsFactors = FALSE)

  miss <- setdiff(c(by, if (fn != "count") col), names(df))
  if (length(miss)) {
    cli::cli_abort("Column(s) not found in Parquet: {.val {miss}}")
  }

  groups <- lapply(by, function(b) df[[b]])
  names(groups) <- by
  out_col <- paste0(col, "_", fn)

  if (fn == "count") {
    agg <- stats::aggregate(
      list(.x = rep(1L, nrow(df))), by = groups, FUN = length
    )
  } else {
    x <- suppressWarnings(as.numeric(df[[col]]))
    ffun <- switch(fn,
      mean = function(v) mean(v, na.rm = TRUE),
      sum  = function(v) sum(v, na.rm = TRUE),
      min  = function(v) min(v, na.rm = TRUE),
      max  = function(v) max(v, na.rm = TRUE),
      std  = function(v) stats::sd(v, na.rm = TRUE)
    )
    agg <- stats::aggregate(list(.x = x), by = groups, FUN = ffun)
  }
  names(agg)[names(agg) == ".x"] <- out_col
  agg
}


#' Fast filter using Polars (Rust backend)
#'
#' Filters rows from a file directly using Polars - processes the file
#' without loading it entirely into R memory.
#'
#' @param path Character. Path to a CSV file.
#' @param col Character. Column name to filter on.
#' @param op Character. Operator: "gt" (>), "lt" (<), "gte" (>=), "lte" (<=),
#'   "eq" (==), "neq" (!=).
#' @param value Numeric. Value to compare against.
#' @param name Character. Name for context registration. Default "filtered_data".
#' @param register Logical. Auto-register result in context. Default TRUE.
#'
#' @return A data.frame with filtered results (invisibly).
#'
#' @examples
#' \dontrun{
#' # Filter large dataset without loading into memory
#' qsr_fast_filter("data/sales.csv", col = "amount", op = "gt", value = 1000)
#'
#' # Chain: filter -> table
#' qsr_fast_filter("data/survey.csv", col = "age", op = "gte", value = 18)
#' qsr_table(type = "summary")
#' }
#'
#' @export
qsr_fast_filter <- function(path,
                            col,
                            op,
                            value,
                            name = "filtered_data",
                            register = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  op <- match.arg(op, c("gt", "lt", "gte", "lte", "eq", "neq"))

  result <- .qsr_adaptive_call(path, "rust_filter", list(path, col, op, value))
  elapsed <- result[["_elapsed_secs"]]
  nrows <- result[["_nrows"]]
  data_cols <- setdiff(names(result), c("_elapsed_secs", "_nrows"))
  df <- as.data.frame(result[data_cols], stringsAsFactors = FALSE)

  cli::cli_alert_success(
    "Filtered {.val {nrows}} rows where {.val {col}} {op} {.val {value}} in {.val {round(elapsed, 3)}}s {.emph (Polars/Rust)}"
  )

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


#' Fast sort using Polars (Rust backend)
#'
#' Sorts a file by a column using Polars without loading the full file
#' into R first.
#'
#' @param path Character. Path to a CSV file.
#' @param by Character. Column name to sort by.
#' @param desc Logical. Sort in descending order? Default FALSE.
#' @param name Character. Name for context registration. Default "sorted_data".
#' @param register Logical. Auto-register result in context. Default TRUE.
#'
#' @return A data.frame with sorted results (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_fast_sort("data/results.csv", by = "score", desc = TRUE)
#' qsr_table()
#' }
#'
#' @export
qsr_fast_sort <- function(path,
                          by,
                          desc = FALSE,
                          name = "sorted_data",
                          register = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  result <- .qsr_adaptive_call(path, "rust_sort", list(path, by, desc))
  elapsed <- result[["_elapsed_secs"]]
  nrows <- result[["_nrows"]]
  data_cols <- setdiff(names(result), c("_elapsed_secs", "_nrows"))
  df <- as.data.frame(result[data_cols], stringsAsFactors = FALSE)

  cli::cli_alert_success(
    "Sorted {.val {nrows}} rows by {.val {by}} in {.val {round(elapsed, 3)}}s {.emph (Polars/Rust)}"
  )

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}


#' Run a QUASAR benchmark comparing Rust vs Base R
#'
#' Reads the same CSV file with both Polars/Rust and base R's `read.csv()`,
#' and reports the speedup. Useful for demonstrating QUASAR's performance
#' advantage.
#'
#' @param path Character. Path to a CSV file.
#' @param n_runs Integer. Number of runs for each method. Default 3.
#'
#' @return A data.frame with benchmark results (invisibly).
#'
#' @examples
#' \dontrun{
#' # Create a test file and benchmark
#' write.csv(data.frame(x = rnorm(1e6), y = rnorm(1e6)), "test_big.csv")
#' qsr_benchmark("test_big.csv")
#' }
#'
#' @export
qsr_benchmark <- function(path, n_runs = 3L) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  cli::cli_h2("QUASAR Benchmark: Polars/Rust vs Base R")
  cli::cli_text("File: {.file {basename(path)}}")
  cli::cli_text("Runs: {.val {n_runs}}")
  cli::cli_text("")

  # Benchmark Rust/Polars (uses adaptive call for large files)
  rust_times <- numeric(n_runs)
  for (i in seq_len(n_runs)) {
    t0 <- proc.time()["elapsed"]
    result <- .qsr_adaptive_call(path, "rust_read_csv", list(path, NULL, ",", NULL))
    rust_times[i] <- proc.time()["elapsed"] - t0
  }

  # Benchmark base R
  base_times <- numeric(n_runs)
  for (i in seq_len(n_runs)) {
    t0 <- proc.time()["elapsed"]
    utils::read.csv(path)
    base_times[i] <- proc.time()["elapsed"] - t0
  }

  rust_mean <- mean(rust_times)
  base_mean <- mean(base_times)
  speedup <- base_mean / rust_mean

  cli::cli_alert_success("Polars/Rust: {.val {round(rust_mean, 4)}}s (avg)")
  cli::cli_alert_info("Base R:      {.val {round(base_mean, 4)}}s (avg)")
  cli::cli_alert_success(
    "Speedup: {.strong {round(speedup, 1)}x faster} with QUASAR"
  )

  results <- data.frame(
    method = c("polars_rust", "base_r"),
    mean_seconds = c(rust_mean, base_mean),
    speedup = c(speedup, 1.0)
  )

  invisible(results)
}
