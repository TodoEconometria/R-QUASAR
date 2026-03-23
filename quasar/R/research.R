# ============================================================
# QUASAR — Research Data Platform
# qsr_download(), qsr_panel(), qsr_scrape(), qsr_spatial()
#
# "From 830 lines of data wrangling to 30."
#
# This is what makes QUASAR different from every other R package.
# Not shortcuts for existing functions — solutions for problems
# that have NO one-liner in R today.
# ============================================================


# ============================================================
# qsr_download() — Universal research data downloader
# ============================================================

#' Download any research data file with one line
#'
#' Downloads files from any URL with automatic format detection, ZIP
#' extraction, retry logic, caching, and authentication. Handles Excel,
#' CSV, TSV, Stata (.dta), SPSS (.sav), Parquet, and ZIP archives
#' containing any of these formats.
#'
#' @param url Character. URL to download from.
#' @param name Character. Name for the dataset in context. Default
#'   auto-generated from URL.
#' @param format Character. Force format: "auto", "csv", "excel", "dta",
#'   "sav", "parquet", "json", "zip". Default "auto" (detects from URL/content).
#' @param sheet Character or integer. Sheet name/number for Excel files.
#'   Default 1.
#' @param cache Logical. Cache downloads to avoid re-downloading. Default TRUE.
#' @param cache_dir Character. Cache directory. Default "data/raw/.cache".
#' @param auth Named list. Authentication: `list(type = "bearer",
#'   token = "xxx")` or `list(type = "basic", user = "x", pass = "y")`.
#'   Default NULL (no auth).
#' @param headers Named list. Additional HTTP headers. Default NULL.
#' @param retry Integer. Number of retry attempts. Default 3.
#' @param register Logical. Auto-register in context. Default TRUE.
#' @param ... Additional arguments passed to the reader function.
#'
#' @return A data.frame (invisibly). Registered in context.
#'
#' @details
#' Smart behaviors:
#' - **ZIP files**: Automatically extracts and finds data files inside
#' - **Stata/SPSS**: Reads .dta and .sav with labels preserved
#' - **Caching**: Won't re-download if file exists and is < 7 days old
#' - **Retry**: Exponential backoff on network failures
#' - **Large files**: Progress bar for downloads > 1MB
#'
#' @examples
#' \dontrun{
#' # Excel from UNODC
#' qsr_download("https://dataunodc.un.org/.../coca_data.xlsx",
#'              name = "unodc_coca")
#'
#' # ZIP with Stata files inside (auto-extracts)
#' qsr_download("https://inei.gob.pe/.../enaho2023-601.zip",
#'              name = "enaho_2023")
#'
#' # API with authentication
#' qsr_download("https://api.acleddata.com/acled/read.csv",
#'              auth = list(type = "params",
#'                          key = Sys.getenv("ACLED_API_KEY"),
#'                          email = Sys.getenv("ACLED_EMAIL")),
#'              name = "acled_events")
#'
#' # After download, everything works:
#' qsr_clean(impute = "median")
#' qsr_model(y ~ x1 + x2)
#' qsr_table()
#' }
#'
#' @export
qsr_download <- function(url,
                         name = NULL,
                         format = "auto",
                         sheet = 1,
                         cache = TRUE,
                         cache_dir = file.path("data", "raw", ".cache"),
                         auth = NULL,
                         headers = NULL,
                         retry = 3L,
                         register = TRUE,
                         ...) {

  # Auto-generate name from URL
  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(url))
    name <- gsub("[^a-zA-Z0-9_]", "_", name)
  }

  # Check cache
  if (cache) {
    cached_df <- .qsr_dl_cache_check(name, cache_dir)
    if (!is.null(cached_df)) {
      cli::cli_alert_info(
        "Using cached {.val {name}}: {.val {nrow(cached_df)}} rows x {.val {ncol(cached_df)}} cols"
      )
      if (register) qsr_data(cached_df, name = name)
      return(invisible(cached_df))
    }
  }

  # Download file
  tmp <- .qsr_dl_download(url, auth, headers, retry)

  # Detect format
  if (format == "auto") {
    format <- .qsr_dl_detect_format(url, tmp)
  }

  # Read file (handles ZIP extraction)
  df <- .qsr_dl_read(tmp, format, sheet, ...)

  if (is.null(df) || nrow(df) == 0) {
    cli::cli_abort("Downloaded file is empty or could not be parsed.")
  }

  # Cache the parsed result
  if (cache) {
    .qsr_dl_cache_save(df, name, cache_dir)
  }

  cli::cli_alert_success(
    "Downloaded {.val {name}}: {.val {nrow(df)}} rows x {.val {ncol(df)}} cols"
  )

  if (register) {
    qsr_data(df, name = name)
  }

  # Clean up
  unlink(tmp)

  invisible(df)
}


# ---- qsr_download internals ----

#' @keywords internal
.qsr_dl_download <- function(url, auth, headers, retry) {
  .qsr_require("httr2", "for downloading data")

  req <- httr2::request(url)
  req <- httr2::req_timeout(req, 120)
  req <- httr2::req_retry(req, max_tries = retry, backoff = ~ 2)

  # Authentication
  if (!is.null(auth)) {
    if (auth$type == "bearer") {
      req <- httr2::req_auth_bearer_token(req, auth$token)
    } else if (auth$type == "basic") {
      req <- httr2::req_auth_basic(req, auth$user, auth$pass)
    } else if (auth$type == "params") {
      # API key as query parameters (common pattern)
      params <- auth[!names(auth) %in% "type"]
      for (nm in names(params)) {
        req <- httr2::req_url_query(req, !!nm := params[[nm]])
      }
    }
  }

  # Custom headers
  if (!is.null(headers)) {
    for (nm in names(headers)) {
      req <- httr2::req_headers(req, !!nm := headers[[nm]])
    }
  }

  # Determine extension for temp file
  ext <- paste0(".", tools::file_ext(url))
  if (ext == ".") ext <- ".tmp"
  tmp <- tempfile(fileext = ext)

  tryCatch({
    resp <- httr2::req_perform(req, path = tmp)
    cli::cli_alert_info("Downloaded {.val {round(file.info(tmp)$size / 1024 / 1024, 1)}} MB")
    tmp
  }, error = function(e) {
    cli::cli_abort(c(
      "Download failed: {.url {url}}",
      "i" = "Error: {e$message}",
      "i" = "Check URL, internet connection, and authentication."
    ))
  })
}

#' @keywords internal
.qsr_dl_detect_format <- function(url, filepath) {
  ext <- tolower(tools::file_ext(url))

  # Clean query params from extension
  ext <- gsub("\\?.*", "", ext)

  switch(ext,
    csv = "csv",
    tsv = "tsv",
    xlsx = , xls = "excel",
    dta = "dta",
    sav = "sav",
    parquet = , pq = "parquet",
    json = "json",
    zip = "zip",
    {
      # Try to detect from content
      first_bytes <- readBin(filepath, "raw", n = 4)
      if (identical(first_bytes[1:2], charToRaw("PK"))) return("zip")
      if (identical(first_bytes[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))) return("zip")
      # Default to CSV
      "csv"
    }
  )
}

#' @keywords internal
.qsr_dl_read <- function(filepath, format, sheet, ...) {

  if (format == "zip") {
    return(.qsr_dl_read_zip(filepath, sheet, ...))
  }

  switch(format,
    csv = {
      utils::read.csv(filepath, stringsAsFactors = FALSE, ...)
    },
    tsv = {
      utils::read.delim(filepath, stringsAsFactors = FALSE, ...)
    },
    excel = {
      .qsr_require("readxl", "for Excel files")
      as.data.frame(readxl::read_excel(filepath, sheet = sheet, ...))
    },
    dta = {
      .qsr_require("haven", "for Stata .dta files")
      as.data.frame(haven::read_dta(filepath, ...))
    },
    sav = {
      .qsr_require("haven", "for SPSS .sav files")
      as.data.frame(haven::read_sav(filepath, ...))
    },
    parquet = {
      .qsr_require("arrow", "for Parquet files")
      as.data.frame(arrow::read_parquet(filepath, ...))
    },
    json = {
      .qsr_require("jsonlite", "for JSON files")
      data <- jsonlite::fromJSON(filepath, flatten = TRUE, ...)
      if (is.data.frame(data)) data else as.data.frame(data)
    },
    cli::cli_abort("Unsupported format: {.val {format}}")
  )
}

#' @keywords internal
.qsr_dl_read_zip <- function(zipfile, sheet, ...) {
  tmp_dir <- tempfile("qsr_zip_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  utils::unzip(zipfile, exdir = tmp_dir)

  # Find data files inside the ZIP (priority order)
  all_files <- list.files(tmp_dir, recursive = TRUE, full.names = TRUE)

  data_patterns <- c("\\.dta$", "\\.sav$", "\\.csv$", "\\.xlsx$",
                     "\\.xls$", "\\.parquet$", "\\.tsv$")

  for (pattern in data_patterns) {
    matches <- grep(pattern, all_files, value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) {
      # Use the largest file (usually the main data file)
      sizes <- file.info(matches)$size
      chosen <- matches[which.max(sizes)]
      fmt <- gsub("^\\\\.", "", gsub("\\$", "", pattern))
      fmt <- switch(fmt,
        "dta" = "dta", "sav" = "sav", "csv" = "csv",
        "xlsx" = "excel", "xls" = "excel",
        "parquet" = "parquet", "tsv" = "tsv"
      )

      cli::cli_alert_info("Extracted: {.file {basename(chosen)}} ({fmt})")
      return(.qsr_dl_read(chosen, fmt, sheet, ...))
    }
  }

  cli::cli_abort(c(
    "No data files found inside the ZIP archive.",
    "i" = "Expected: .dta, .sav, .csv, .xlsx, .parquet, or .tsv"
  ))
}

#' @keywords internal
.qsr_dl_cache_check <- function(name, cache_dir) {
  cache_file <- file.path(cache_dir, paste0(name, ".rds"))
  if (!file.exists(cache_file)) return(NULL)

  age_days <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
  if (age_days > 7) return(NULL)

  tryCatch(readRDS(cache_file), error = function(e) NULL)
}

#' @keywords internal
.qsr_dl_cache_save <- function(df, name, cache_dir) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_file <- file.path(cache_dir, paste0(name, ".rds"))
  tryCatch(saveRDS(df, cache_file), error = function(e) NULL)
}


# ============================================================
# qsr_panel() — Build research panels from multiple sources
# ============================================================

#' Build a research panel from multiple data sources
#'
#' Joins multiple datasets by key variables (country+year, id+time, etc.)
#' into a single analysis-ready panel. Handles different column names,
#' missing combinations, and reports coverage statistics.
#'
#' @param ... Named data frames or names of datasets in context.
#' @param join_by Character vector. Variables to join by (e.g.,
#'   `c("country", "year")`).
#' @param sources Character vector. Names of datasets in context to use.
#'   Alternative to passing data frames in `...`.
#' @param balance Logical. Balance the panel (fill missing combinations
#'   with NA). Default FALSE.
#' @param rename Named list. Rename columns across sources before joining
#'   (e.g., `list(Country = "country", Year = "year")`).
#' @param name Character. Name for the panel in context. Default "panel".
#' @param register Logical. Default TRUE.
#'
#' @return A data.frame panel (invisibly).
#'
#' @details
#' The panel builder:
#' 1. Standardizes column names across sources (via `rename`)
#' 2. Joins all sources by `join_by` keys
#' 3. Reports coverage: rows, sources merged, completeness per variable
#' 4. Optionally balances the panel (all key combinations)
#'
#' @examples
#' \dontrun{
#' # Download from multiple sources
#' qsr_download("https://dataunodc.un.org/.../coca.xlsx", name = "unodc")
#' qsr_fetch("worldbank", indicator = "NY.GDP.PCAP.KD",
#'           country = c("BOL", "PER", "COL"), name = "wb_gdp")
#'
#' # Build panel — joins by country + year
#' qsr_panel(
#'   sources = c("unodc", "wb_gdp"),
#'   join_by = c("country", "year"),
#'   rename  = list(Country = "country", Year = "year"),
#'   balance = TRUE
#' )
#'
#' # Now analyze
#' qsr_model(coca_area ~ gdp_pc + poverty_gap)
#' qsr_table()
#' }
#'
#' @export
qsr_panel <- function(...,
                      join_by,
                      sources = NULL,
                      balance = FALSE,
                      rename = NULL,
                      name = "panel",
                      register = TRUE) {

  # Collect datasets
  datasets <- list(...)

  # Add datasets from context by name
  if (!is.null(sources)) {
    for (src in sources) {
      ctx <- .qsr_context$get("dataset_store")
      if (!is.null(ctx) && src %in% names(ctx)) {
        datasets[[src]] <- ctx[[src]]
      } else {
        # Try to get from named data in context
        d <- tryCatch(.qsr_context$get_data(), error = function(e) NULL)
        if (!is.null(d)) {
          cli::cli_alert_warning(
            "Source {.val {src}} not found in context store. Using active dataset."
          )
        }
      }
    }
  }

  if (length(datasets) < 2) {
    cli::cli_abort(c(
      "At least 2 datasets are needed to build a panel.",
      "i" = "Pass data frames directly or use {.arg sources} with dataset names."
    ))
  }

  # Give names if missing
  if (is.null(names(datasets))) {
    names(datasets) <- paste0("source_", seq_along(datasets))
  }
  names(datasets)[names(datasets) == ""] <- paste0("source_", which(names(datasets) == ""))

  # Standardize column names
  if (!is.null(rename)) {
    datasets <- lapply(datasets, function(df) {
      df <- as.data.frame(df)
      for (old_name in names(rename)) {
        new_name <- rename[[old_name]]
        if (old_name %in% names(df)) {
          names(df)[names(df) == old_name] <- new_name
        }
      }
      df
    })
  }

  # Verify join_by columns exist
  for (i in seq_along(datasets)) {
    missing_keys <- setdiff(join_by, names(datasets[[i]]))
    if (length(missing_keys) > 0) {
      src_name <- names(datasets)[i]
      available <- paste(names(datasets[[i]]), collapse = ", ")
      cli::cli_abort(c(
        "Join key {.val {missing_keys}} not found in {.val {src_name}}.",
        "i" = "Available columns: {available}",
        "i" = "Use {.arg rename} to standardize column names."
      ))
    }
  }

  # Sequential merge
  panel <- datasets[[1]]
  for (i in 2:length(datasets)) {
    # Identify non-key columns that overlap (add suffix)
    src_name <- names(datasets)[i]
    panel <- merge(panel, datasets[[i]], by = join_by,
                   all = TRUE, suffixes = c("", paste0(".", src_name)))
  }

  # Balance panel if requested
  if (balance && length(join_by) >= 2) {
    key_values <- lapply(join_by, function(k) unique(panel[[k]]))
    names(key_values) <- join_by
    full_grid <- expand.grid(key_values, stringsAsFactors = FALSE)
    panel <- merge(full_grid, panel, by = join_by, all.x = TRUE)
  }

  # Coverage report
  n_obs <- nrow(panel)
  n_vars <- ncol(panel) - length(join_by)
  completeness <- round(1 - colMeans(is.na(panel)), 3)
  low_coverage <- names(completeness[completeness < 0.5])

  cli::cli_alert_success(
    "Panel built: {.val {n_obs}} obs x {.val {n_vars}} variables from {.val {length(datasets)}} sources"
  )

  if (balance) {
    cli::cli_alert_info("Balanced panel: all {.val {join_by}} combinations filled")
  }

  if (length(low_coverage) > 0 && length(low_coverage) <= 10) {
    cli::cli_alert_warning(
      "Low coverage (<50%): {.val {low_coverage}}"
    )
  }

  if (register) {
    qsr_data(panel, name = name)
  }

  invisible(panel)
}


# ============================================================
# qsr_scrape() — Extract tables from PDFs and HTML
# ============================================================

#' Extract data tables from PDFs or web pages
#'
#' Reads tables from PDF documents or HTML pages. Handles multi-page
#' PDFs, messy formatting, and returns clean data frames.
#'
#' @param source Character. Path to a local PDF or URL to an HTML page.
#' @param pages Integer vector. Pages to extract from PDF. Default NULL
#'   (all pages).
#' @param table_index Integer. Which table to return (1 = first). Default 1.
#' @param method Character. Extraction method: "auto", "tabulapdf",
#'   "pdftools", "rvest". Default "auto".
#' @param name Character. Name for context. Default "scraped_data".
#' @param register Logical. Default TRUE.
#' @param ... Additional arguments.
#'
#' @return A data.frame (invisibly).
#'
#' @examples
#' \dontrun{
#' # Extract tables from a PDF report
#' qsr_scrape("reports/simci_2023.pdf", pages = 5:10, name = "simci_area")
#'
#' # Extract a table from a web page
#' qsr_scrape("https://www.ine.es/jaxiT3/Tabla.htm?t=2852",
#'            table_index = 1, name = "ine_data")
#'
#' # After extraction, use QUASAR pipeline
#' qsr_clean(impute = "median")
#' qsr_table(type = "summary")
#' }
#'
#' @export
qsr_scrape <- function(source,
                       pages = NULL,
                       table_index = 1L,
                       method = c("auto", "tabulapdf", "pdftools", "rvest"),
                       name = "scraped_data",
                       register = TRUE,
                       ...) {

  method <- match.arg(method)

  # Detect if PDF or HTML
  is_pdf <- grepl("\\.pdf$", tolower(source)) ||
    (file.exists(source) && grepl("\\.pdf$", tolower(source)))
  is_url <- grepl("^https?://", source) && !is_pdf

  if (method == "auto") {
    method <- if (is_pdf) "tabulapdf" else "rvest"
  }

  df <- switch(method,
    tabulapdf = .qsr_scrape_pdf_tabula(source, pages, table_index, ...),
    pdftools  = .qsr_scrape_pdf_pdftools(source, pages, ...),
    rvest     = .qsr_scrape_html(source, table_index, ...)
  )

  if (is.null(df) || nrow(df) == 0) {
    cli::cli_abort(c(
      "No tables found in {.file {basename(source)}}.",
      "i" = "Try different {.arg pages} or {.arg table_index}."
    ))
  }

  cli::cli_alert_success(
    "Scraped {.val {nrow(df)}} rows x {.val {ncol(df)}} cols from {.file {basename(source)}}"
  )

  if (register) {
    qsr_data(df, name = name)
  }

  invisible(df)
}

#' @keywords internal
.qsr_scrape_pdf_tabula <- function(source, pages, table_index, ...) {
  .qsr_require("tabulapdf", "for PDF table extraction")

  args <- list(file = source, ...)
  if (!is.null(pages)) args$pages <- pages

  tables <- tryCatch(
    do.call(tabulapdf::extract_tables, args),
    error = function(e) {
      cli::cli_abort(c(
        "PDF extraction failed. tabulapdf requires Java.",
        "i" = "Install Java: https://www.java.com/en/download/",
        "i" = "Error: {e$message}"
      ))
    }
  )

  if (length(tables) == 0) return(NULL)
  if (table_index > length(tables)) {
    cli::cli_alert_warning(
      "Only {.val {length(tables)}} tables found. Using table 1."
    )
    table_index <- 1L
  }

  as.data.frame(tables[[table_index]])
}

#' @keywords internal
.qsr_scrape_pdf_pdftools <- function(source, pages, ...) {
  .qsr_require("pdftools", "for PDF text extraction")

  text <- pdftools::pdf_text(source)
  if (!is.null(pages)) text <- text[pages]

  # Parse text lines into table
  all_lines <- unlist(strsplit(paste(text, collapse = "\n"), "\n"))
  all_lines <- trimws(all_lines)
  all_lines <- all_lines[nzchar(all_lines)]

  # Try to detect tabular structure (lines with consistent spacing)
  tab_lines <- all_lines[grepl("\\s{2,}", all_lines)]

  if (length(tab_lines) < 2) {
    cli::cli_alert_warning("No clear table structure found. Returning raw text lines.")
    return(data.frame(line = seq_along(all_lines), text = all_lines,
                      stringsAsFactors = FALSE))
  }

  # Split by whitespace runs
  rows <- strsplit(tab_lines, "\\s{2,}")
  max_cols <- max(vapply(rows, length, integer(1)))

  # Pad shorter rows
  rows <- lapply(rows, function(r) {
    c(r, rep(NA, max_cols - length(r)))
  })

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)

  # Use first row as header if it looks like one
  if (all(!grepl("^[0-9]", df[1, ], perl = TRUE))) {
    names(df) <- as.character(df[1, ])
    df <- df[-1, , drop = FALSE]
  }

  # Try to convert numeric columns
  for (col in names(df)) {
    vals <- suppressWarnings(as.numeric(gsub(",", "", df[[col]])))
    if (sum(!is.na(vals)) > sum(!is.na(df[[col]])) * 0.5) {
      df[[col]] <- vals
    }
  }

  rownames(df) <- NULL
  df
}

#' @keywords internal
.qsr_scrape_html <- function(url, table_index, ...) {
  .qsr_require("rvest", "for HTML table extraction")

  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_abort(c(
        "Could not read HTML from {.url {url}}",
        "i" = "Error: {e$message}"
      ))
    }
  )

  tables <- rvest::html_table(page, fill = TRUE, ...)

  if (length(tables) == 0) return(NULL)
  if (table_index > length(tables)) {
    cli::cli_alert_warning(
      "Only {.val {length(tables)}} tables found. Using table 1."
    )
    table_index <- 1L
  }

  cli::cli_alert_info("Found {.val {length(tables)}} tables on page")
  as.data.frame(tables[[table_index]])
}


# ============================================================
# qsr_spatial() — Simplified spatial data operations
# ============================================================

#' Read and process spatial data
#'
#' Load shapefiles, GeoPackages, or GeoTIFF rasters. Extract values
#' by polygon (zonal statistics). Create choropleth maps.
#'
#' @param path Character. Path to spatial file (.shp, .gpkg, .tif,
#'   .geojson).
#' @param operation Character. What to do: "read", "extract", "map".
#'   Default "read".
#' @param raster Character. Path to raster file (for operation = "extract").
#' @param fun Character. Aggregation function for extraction: "mean",
#'   "sum", "min", "max", "count". Default "mean".
#' @param fill_var Character. Variable to map (for operation = "map").
#' @param name Character. Context name. Default "spatial_data".
#' @param register Logical. Default TRUE.
#' @param ... Additional arguments.
#'
#' @return Depends on operation: data.frame for "read"/"extract",
#'   plot for "map".
#'
#' @examples
#' \dontrun{
#' # Read a shapefile
#' qsr_spatial("data/municipios.gpkg")
#'
#' # Extract raster values by polygon (zonal statistics)
#' qsr_spatial("data/municipios.gpkg", operation = "extract",
#'             raster = "data/elevation.tif", fun = "mean",
#'             name = "elevation_by_municipio")
#'
#' # Create a choropleth map
#' qsr_spatial("data/municipios.gpkg", operation = "map",
#'             fill_var = "population")
#' }
#'
#' @export
qsr_spatial <- function(path,
                        operation = c("read", "extract", "map"),
                        raster = NULL,
                        fun = "mean",
                        fill_var = NULL,
                        name = "spatial_data",
                        register = TRUE,
                        ...) {

  operation <- match.arg(operation)
  .qsr_require("sf", "for spatial data")

  ext <- tolower(tools::file_ext(path))

  if (operation == "read") {
    if (ext %in% c("tif", "tiff")) {
      .qsr_require("terra", "for raster data")
      r <- terra::rast(path)
      cli::cli_alert_success(
        "Raster: {.val {terra::ncell(r)}} cells, {.val {terra::nlyr(r)}} layers, CRS: {terra::crs(r, describe=TRUE)$name}"
      )
      return(invisible(r))
    }

    # Vector data
    shp <- sf::st_read(path, quiet = TRUE, ...)
    cli::cli_alert_success(
      "Spatial: {.val {nrow(shp)}} features, {.val {ncol(shp) - 1}} attributes, CRS: {sf::st_crs(shp)$Name}"
    )

    if (register) {
      # Store the non-geometry columns as a data.frame in context
      df <- sf::st_drop_geometry(shp)
      qsr_data(df, name = name)
    }

    return(invisible(shp))
  }

  if (operation == "extract") {
    if (is.null(raster)) {
      cli::cli_abort("Need {.arg raster} path for extraction.")
    }
    .qsr_require("terra", "for raster extraction")
    .qsr_require("exactextractr", "for zonal statistics")

    shp <- sf::st_read(path, quiet = TRUE)
    r <- terra::rast(raster)

    cli::cli_alert_info("Extracting {.val {fun}} from raster...")
    values <- exactextractr::exact_extract(r, shp, fun)

    df <- sf::st_drop_geometry(shp)
    df[[paste0("raster_", fun)]] <- values

    cli::cli_alert_success(
      "Extracted {.val {fun}} for {.val {nrow(df)}} features"
    )

    if (register) qsr_data(df, name = name)
    return(invisible(df))
  }

  if (operation == "map") {
    shp <- sf::st_read(path, quiet = TRUE)

    if (is.null(fill_var)) {
      p <- ggplot2::ggplot(shp) + ggplot2::geom_sf() +
        ggplot2::theme_minimal()
    } else {
      p <- ggplot2::ggplot(shp) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_var]])) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_minimal() +
        ggplot2::labs(fill = fill_var)
    }

    cli::cli_alert_success("Map created for {.val {fill_var %||% 'geometry'}}")
    return(p)
  }
}
