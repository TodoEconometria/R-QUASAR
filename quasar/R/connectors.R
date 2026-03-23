# ============================================================
# QUASAR — Layer 3: Connectors
# qsr_db(), qsr_spark(), qsr_fetch(), qsr_python()
# Zero-friction access to databases, Spark, APIs, and Python
# ============================================================


# ---- Internal: require package guard ----------------------------------------

#' Check that a suggested package is installed, abort with install command if not
#' @noRd
.qsr_require <- function(pkg, reason = "") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- if (nzchar(reason)) {
      "Package {.pkg {pkg}} is required {reason}."
    } else {
      "Package {.pkg {pkg}} is required."
    }
    cli::cli_abort(c(
      msg,
      "i" = 'Install it with: {.code install.packages("{pkg}")}'
    ))
  }
}


# ---- qsr_db() ---------------------------------------------------------------

#' Connect to a database and load data
#'
#' One-line database access. Connects, queries, and registers the result
#' in context automatically. Uses SQLite in-memory by default — zero setup.
#'
#' @param driver Character. Database driver: `"sqlite"`, `"postgres"`,
#'   `"mysql"`, `"odbc"`. Default `"sqlite"`.
#' @param dbname Character. Database name or file path (SQLite).
#'   Defaults to `":memory:"` for SQLite.
#' @param host Character. Server hostname. Default `"localhost"`.
#' @param port Integer. Port number. Auto-detected from driver if `NULL`.
#' @param user Character. Username. If `NULL`, reads from env var
#'   `QUASAR_DB_USER`.
#' @param password Character. Password. If `NULL`, reads from env var
#'   `QUASAR_DB_PASSWORD`.
#' @param query Character. SQL query to execute. Mutually exclusive with
#'   `table`.
#' @param table Character. Table name to read entirely. Mutually exclusive
#'   with `query`.
#' @param name Character. Name for context registration. Default `"db_data"`.
#' @param .conn Logical. If `TRUE`, returns the raw DBI connection instead
#'   of auto-querying. Default `FALSE`.
#' @param ... Additional arguments passed to [DBI::dbConnect()].
#'
#' @return A data frame (invisibly) or a DBI connection if `.conn = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' # In-memory SQLite (zero setup)
#' qsr_db("sqlite")
#'
#' # Read a table
#' qsr_db("sqlite", dbname = "research.sqlite", table = "surveys")
#'
#' # Run a query
#' qsr_db("postgres", dbname = "research",
#'        query = "SELECT * FROM surveys WHERE year > 2020")
#' }
qsr_db <- function(driver   = c("sqlite", "postgres", "mysql", "odbc"),
                   dbname   = NULL,
                   host     = "localhost",
                   port     = NULL,
                   user     = NULL,
                   password = NULL,
                   query    = NULL,
                   table    = NULL,
                   name     = "db_data",
                   .conn    = FALSE,
                   ...) {

  driver <- match.arg(driver)

  # Validate mutually exclusive

  if (!is.null(query) && !is.null(table)) {
    cli::cli_abort("Specify either {.arg query} or {.arg table}, not both.")
  }

  # Get driver object and require package
  drv <- .qsr_db_driver(driver)

  # Build connection args

  conn_args <- list(drv = drv, ...)

  if (driver == "sqlite") {
    conn_args$dbname <- dbname %||% ":memory:"
  } else {
    conn_args$dbname   <- dbname
    conn_args$host     <- host
    conn_args$port     <- port %||% .qsr_db_default_port(driver)
    conn_args$user     <- user %||% Sys.getenv("QUASAR_DB_USER", unset = NA)
    conn_args$password <- password %||% Sys.getenv("QUASAR_DB_PASSWORD", unset = NA)

    if (is.na(conn_args$user)) conn_args$user <- NULL
    if (is.na(conn_args$password)) conn_args$password <- NULL
  }

  # Connect
  con <- tryCatch(
    do.call(DBI::dbConnect, conn_args),
    error = function(e) {
      cli::cli_abort(c(
        "Could not connect to database.",
        "x" = conditionMessage(e),
        "i" = "Check credentials, hostname, and that the server is running."
      ))
    }
  )

  # Store connection in context
  .qsr_context$set_connection(con, "db")

  # If just connection requested, return it
  if (.conn) {
    db_display <- dbname %||% ":memory:"
    cli::cli_alert_success("Connected to {.val {driver}}: {db_display}")
    return(invisible(con))
  }

  # If no query/table, just connect and inform
  if (is.null(query) && is.null(table)) {
    db_display <- dbname %||% ":memory:"
    cli::cli_alert_success("Connected to {.val {driver}}: {db_display}")
    cli::cli_alert_info("Use {.arg query} or {.arg table} to load data.")
    return(invisible(con))
  }

  # Fetch data
  df <- tryCatch({
    if (!is.null(table)) {
      DBI::dbReadTable(con, table)
    } else {
      DBI::dbGetQuery(con, query)
    }
  }, error = function(e) {
    cli::cli_abort(c(
      "Query failed.",
      "x" = conditionMessage(e)
    ))
  })

  # Auto-register in context
  .qsr_context$set_data(df)
  .qsr_context$set("data_name", name)

  # Disconnect (data already loaded)
  DBI::dbDisconnect(con)
  .qsr_context$set_connection(NULL, "db")

  nr <- nrow(df)
  nc <- ncol(df)
  cli::cli_alert_success(
    "Loaded {.val {name}}: {nr} rows x {nc} cols from {.val {driver}}"
  )

  invisible(df)
}


# ---- qsr_spark() ------------------------------------------------------------

#' Connect to Apache Spark
#'
#' One-line Spark setup. Connects with sensible defaults and optionally
#' loads data in a single call.
#'
#' @param master Character. Spark master URL. Default `"local"`.
#' @param memory Character. Memory for driver and executor. Default `"4G"`.
#' @param read Character. Optional path to a file to load (CSV, Parquet, JSON).
#'   Format auto-detected from extension.
#' @param name Character. Name for context registration. Default `"spark_data"`.
#' @param version Character. Spark version. If `NULL`, uses installed version.
#' @param ... Additional arguments passed to `sparklyr::spark_config()`.
#'
#' @return Invisible Spark connection, or a Spark DataFrame if `read` is given.
#' @export
#'
#' @examples
#' \dontrun{
#' qsr_spark()                                # local, 4G
#' qsr_spark(master = "yarn", memory = "8G")  # cluster
#' qsr_spark(read = "data/big_file.csv")      # connect + load
#' }
qsr_spark <- function(master  = "local",
                      memory  = "4G",
                      read    = NULL,
                      name    = "spark_data",
                      version = NULL,
                      ...) {

  .qsr_require("sparklyr", "for Spark connections")

  # Build config
  config <- sparklyr::spark_config()
  config$`sparklyr.shell.driver-memory`   <- memory
  config$`sparklyr.shell.executor-memory` <- memory

  # Apply extra config
  dots <- list(...)
  for (k in names(dots)) config[[k]] <- dots[[k]]

  # Connect
  connect_args <- list(master = master, config = config)
  if (!is.null(version)) connect_args$version <- version

  sc <- tryCatch(
    do.call(sparklyr::spark_connect, connect_args),
    error = function(e) {
      cli::cli_abort(c(
        "Could not connect to Spark.",
        "x" = conditionMessage(e),
        "i" = "If Spark is not installed, run: {.code sparklyr::spark_install()}"
      ))
    }
  )

  .qsr_context$set_connection(sc, "spark")
  cli::cli_alert_success("Spark connected: {.val {master}} ({memory})")

  # Load data if requested
  if (!is.null(read)) {
    fmt <- .qsr_spark_detect_format(read)
    sdf <- switch(fmt,
      csv     = sparklyr::spark_read_csv(sc, name = name, path = read),
      parquet = sparklyr::spark_read_parquet(sc, name = name, path = read),
      json    = sparklyr::spark_read_json(sc, name = name, path = read)
    )

    .qsr_context$set("spark_df", sdf)

    # Collect preview for downstream R functions
    preview <- as.data.frame(head(sdf, 1000))
    .qsr_context$set_data(preview)
    .qsr_context$set("data_name", name)

    nr <- sparklyr::sdf_nrow(sdf)
    cli::cli_alert_success("Loaded {.val {name}}: {nr} rows ({fmt})")

    return(invisible(sdf))
  }

  invisible(sc)
}


# ---- qsr_fetch() ------------------------------------------------------------

#' Fetch data from academic APIs and URLs
#'
#' One-line access to World Bank, FRED, Census, or any URL.
#' Data is auto-registered in context and optionally cached.
#'
#' @param source Character. Data source: `"worldbank"`, `"fred"`,
#'   `"census"`, `"url"`.
#' @param ... Arguments passed to the source-specific fetcher (e.g.,
#'   `indicator`, `series_id`, `url`, `format`).
#' @param name Character. Name for context registration. If `NULL`,
#'   uses the source name.
#' @param cache Logical. Cache results for 24h to avoid repeat API calls.
#'   Default `TRUE`.
#'
#' @return A data frame (invisibly), also registered in context.
#' @export
#'
#' @examples
#' \dontrun{
#' # World Bank GDP per capita
#' qsr_fetch("worldbank", indicator = "NY.GDP.PCAP.CD",
#'           country = "all", start = 2000, end = 2023)
#'
#' # FRED unemployment rate
#' qsr_fetch("fred", series_id = "UNRATE")
#'
#' # Any CSV from URL
#' qsr_fetch("url", url = "https://example.com/data.csv", format = "csv")
#' }
qsr_fetch <- function(source, ..., name = NULL, cache = TRUE) {

  source <- tolower(source)

  # Dispatch registry — 12 sources
  fetchers <- list(
    worldbank = .qsr_fetch_worldbank,
    fred      = .qsr_fetch_fred,
    census    = .qsr_fetch_census,
    url       = .qsr_fetch_url,
    eurostat  = .qsr_fetch_eurostat,
    oecd      = .qsr_fetch_oecd,
    imf       = .qsr_fetch_imf,
    ine_spain = .qsr_fetch_ine_spain,
    inegi     = .qsr_fetch_inegi,
    bls       = .qsr_fetch_bls,
    gtrends   = .qsr_fetch_gtrends
  )

  if (!(source %in% names(fetchers))) {
    available <- paste(names(fetchers), collapse = ", ")
    cli::cli_abort(c(
      "Source {.val {source}} not recognized.",
      "i" = "Available sources: {available}"
    ))
  }

  # Check cache
  cache_args <- list(source = source, ...)
  cache_key  <- .qsr_cache_key(cache_args)
  if (cache) {
    cached <- .qsr_cache_read(cache_key)
    if (!is.null(cached)) {
      name <- name %||% source
      .qsr_context$set_data(cached)
      .qsr_context$set("data_name", name)
      nr <- nrow(cached)
      nc <- ncol(cached)
      cli::cli_alert_info(
        "Using cached {.val {name}}: {nr} rows x {nc} cols (< 24h old)"
      )
      return(invisible(cached))
    }
  }

  # Fetch
  df <- fetchers[[source]](...)

  # Cache
  if (cache) {
    .qsr_cache_write(df, cache_key)
  }

  # Register in context
  name <- name %||% source
  .qsr_context$set_data(df)
  .qsr_context$set("data_name", name)

  nr <- nrow(df)
  nc <- ncol(df)
  cli::cli_alert_success("Fetched {.val {name}}: {nr} rows x {nc} cols")

  invisible(df)
}


# ---- qsr_python() -----------------------------------------------------------

#' Run Python code from R
#'
#' Zero-friction Python interop. Import modules, call functions,
#' fit sklearn/prophet models — all without touching reticulate directly.
#'
#' @param package Character. Python module to import (e.g., `"prophet"`,
#'   `"sklearn.linear_model"`).
#' @param fn Character. Function or class name to call. If `NULL`,
#'   returns the imported module.
#' @param args Named list. Arguments to pass to `fn()`.
#' @param fit List. If provided, calls `.fit()` on the result.
#'   For sklearn: `list(X, y)`. For single-arg: `list(df)`.
#' @param predict Data to predict on. If provided, calls `.predict()`
#'   and registers the result in context.
#' @param envname Character. Python virtualenv name. Default `"quasar-py"`.
#' @param install Logical. If `TRUE`, pip-installs the package first.
#' @param name Character. Name for context registration of predictions.
#' @param ... Additional arguments.
#'
#' @return The Python object or prediction results (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple function call
#' qsr_python("math", fn = "sqrt", args = list(16))
#'
#' # Import a module
#' np <- qsr_python("numpy")
#'
#' # Fit a sklearn model
#' qsr_python("sklearn.linear_model", fn = "LinearRegression",
#'            fit = list(X_train, y_train), predict = X_test)
#' }
qsr_python <- function(package,
                       fn       = NULL,
                       args     = list(),
                       fit      = NULL,
                       predict  = NULL,
                       envname  = "quasar-py",
                       install  = FALSE,
                       name     = "python_result",
                       ...) {

  .qsr_require("reticulate", "for Python interop")

  # Check Python availability
  if (!reticulate::py_available(initialize = TRUE)) {
    cli::cli_abort(c(
      "Python is not available.",
      "i" = "Install Python or run: {.code reticulate::install_python()}"
    ))
  }

  # Ensure virtualenv
  .qsr_python_ensure_env(envname)

  # Install package if requested
  if (install) {
    cli::cli_alert_info("Installing {.pkg {package}}...")
    reticulate::py_install(package, envname = envname)
    cli::cli_alert_success("Installed {.pkg {package}}")
  }

  # Import module
  mod <- tryCatch(
    reticulate::import(package),
    error = function(e) {
      cli::cli_abort(c(
        "Python module {.val {package}} not found.",
        "i" = 'Install it with: {.code qsr_python("{package}", install = TRUE)}'
      ))
    }
  )

  # If no function requested, return the module
  if (is.null(fn)) {
    cli::cli_alert_success("Imported Python module: {.val {package}}")
    .qsr_context$set_connection(list(module = mod), "python_env")
    return(invisible(mod))
  }

  # Call function/constructor
  obj <- do.call(mod[[fn]], args)

  # Fit if requested
  if (!is.null(fit)) {
    if (length(fit) == 2) {
      obj$fit(fit[[1]], fit[[2]])
    } else {
      obj$fit(fit[[1]])
    }
    cli::cli_alert_success("Fitted {.val {package}}.{fn}")
  }

  # Predict if requested
  if (!is.null(predict)) {
    result <- obj$predict(predict)
    # Convert to R data frame if possible
    if (!is.data.frame(result)) {
      result <- tryCatch(
        as.data.frame(result),
        error = function(e) result
      )
    }
    if (is.data.frame(result)) {
      .qsr_context$set_data(result)
      .qsr_context$set("data_name", name)
      nr <- nrow(result)
      cli::cli_alert_success("Prediction: {nr} rows registered as {.val {name}}")
    }
    .qsr_context$set_connection(list(module = mod, object = obj), "python_env")
    return(invisible(result))
  }

  .qsr_context$set_connection(list(module = mod, object = obj), "python_env")

  if (!is.null(fn)) {
    cli::cli_alert_success("Created {.val {package}}.{fn}")
  }

  invisible(obj)
}


# ============================================================
# INTERNAL HELPERS — Database
# ============================================================

#' Get DBI driver object from string
#' @noRd
.qsr_db_driver <- function(driver) {
  switch(driver,
    sqlite = {
      .qsr_require("RSQLite", "for SQLite connections")
      RSQLite::SQLite()
    },
    postgres = {
      .qsr_require("RPostgres", "for PostgreSQL connections")
      RPostgres::Postgres()
    },
    mysql = {
      .qsr_require("RMariaDB", "for MySQL/MariaDB connections")
      RMariaDB::MariaDB()
    },
    odbc = {
      .qsr_require("odbc", "for ODBC connections")
      odbc::odbc()
    }
  )
}

#' Default port by database driver
#' @noRd
.qsr_db_default_port <- function(driver) {
  switch(driver,
    postgres = 5432L,
    mysql    = 3306L,
    odbc     = NULL
  )
}


# ============================================================
# INTERNAL HELPERS — Spark
# ============================================================

#' Detect file format from extension
#' @noRd
.qsr_spark_detect_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    csv     = "csv",
    parquet = "parquet",
    json    = "json",
    {
      cli::cli_abort(c(
        "Cannot detect format for extension {.val {ext}}.",
        "i" = "Supported: csv, parquet, json."
      ))
    }
  )
}


# ============================================================
# INTERNAL HELPERS — Fetch (API dispatchers)
# ============================================================

#' Fetch from World Bank via WDI
#' @noRd
.qsr_fetch_worldbank <- function(indicator, country = "all",
                                  start = 1960, end = NULL, ...) {
  .qsr_require("WDI", "for World Bank data")
  if (is.null(end)) end <- as.integer(format(Sys.Date(), "%Y"))
  df <- WDI::WDI(indicator = indicator, country = country,
                  start = start, end = end, ...)
  # Clean up
  df <- df[!is.na(df[[indicator]]), ]
  df
}

#' Fetch from FRED
#' @noRd
.qsr_fetch_fred <- function(series_id, start = NULL, end = NULL, ...) {
  .qsr_require("fredr", "for FRED data")

  # Check API key
  key <- Sys.getenv("FRED_API_KEY", unset = "")
  if (!nzchar(key)) {
    cli::cli_abort(c(
      "FRED requires an API key.",
      "i" = "Get one at: {.url https://fred.stlouisfed.org/docs/api/api_key.html}",
      "i" = 'Set it with: {.code Sys.setenv(FRED_API_KEY = "your_key")}'
    ))
  }

  fredr::fredr_set_key(key)

  fetch_args <- list(series_id = series_id, ...)
  if (!is.null(start)) fetch_args$observation_start <- as.Date(start)
  if (!is.null(end))   fetch_args$observation_end   <- as.Date(end)

  as.data.frame(do.call(fredr::fredr, fetch_args))
}

#' Fetch from US Census
#' @noRd
.qsr_fetch_census <- function(variables, year = 2020,
                               geography = "state", ...) {
  .qsr_require("tidycensus", "for Census data")
  as.data.frame(tidycensus::get_acs(
    geography = geography,
    variables = variables,
    year      = year,
    ...
  ))
}

#' Fetch from a URL (CSV, JSON, Excel)
#' @noRd
.qsr_fetch_url <- function(url, format = NULL, ...) {

  # Auto-detect format from URL extension
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(url))
    format <- switch(ext,
      csv  = "csv",
      json = "json",
      xlsx = "excel",
      xls  = "excel",
      tsv  = "tsv",
      "csv"  # default
    )
  }

  format <- match.arg(format, c("csv", "json", "tsv", "excel"))

  switch(format,
    csv = {
      utils::read.csv(url, stringsAsFactors = FALSE, ...)
    },
    tsv = {
      utils::read.delim(url, stringsAsFactors = FALSE, ...)
    },
    json = {
      .qsr_require("jsonlite", "for JSON parsing")
      jsonlite::fromJSON(url, flatten = TRUE, ...)
    },
    excel = {
      .qsr_require("readxl", "for Excel files")
      tmp <- tempfile(fileext = paste0(".", tools::file_ext(url)))
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      as.data.frame(readxl::read_excel(tmp, ...))
    }
  )
}


# ============================================================
# INTERNAL HELPERS — Cache
# ============================================================

#' Generate cache key from arguments
#' @noRd
.qsr_cache_key <- function(args) {
  args_str <- paste(sort(paste(names(args), args, sep = "=")), collapse = "&")
  paste0("qsr_", substr(digest_simple(args_str), 1, 12))
}

#' Simple string hash (no external deps)
#' @noRd
digest_simple <- function(x) {
  chars <- utf8ToInt(x)
  hash <- 5381.0
  for (ch in chars) {
    hash <- (hash * 33.0 + ch) %% 2147483647.0
  }
  sprintf("%08x", as.integer(hash))
}

#' Read from cache if valid (< 24h)
#' @noRd
.qsr_cache_read <- function(key) {
  ctx_path <- .qsr_context$get("data_path")
  cache_dir <- if (!is.null(ctx_path)) file.path(ctx_path, ".cache") else file.path("outputs", ".cache")
  cache_file <- file.path(cache_dir, paste0(key, ".rds"))

  if (!file.exists(cache_file)) return(NULL)

  # Check age
  age_hours <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
  if (as.numeric(age_hours) > 24) return(NULL)

  tryCatch(readRDS(cache_file), error = function(e) NULL)
}

#' Write to cache
#' @noRd
.qsr_cache_write <- function(df, key) {
  ctx_path <- .qsr_context$get("data_path")
  cache_dir <- if (!is.null(ctx_path)) file.path(ctx_path, ".cache") else file.path("outputs", ".cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_file <- file.path(cache_dir, paste0(key, ".rds"))
  tryCatch(saveRDS(df, cache_file), error = function(e) NULL)
}


# ============================================================
# INTERNAL HELPERS — Python
# ============================================================

#' Ensure Python virtualenv exists
#' @noRd
.qsr_python_ensure_env <- function(envname) {
  if (!reticulate::virtualenv_exists(envname)) {
    tryCatch({
      reticulate::virtualenv_create(envname)
      cli::cli_alert_success("Created Python env: {.val {envname}}")
    }, error = function(e) {
      # Fallback: use system Python without virtualenv
      cli::cli_alert_warning(
        "Could not create virtualenv. Using system Python."
      )
    })
  }
  tryCatch(
    reticulate::use_virtualenv(envname, required = FALSE),
    error = function(e) NULL
  )
}
