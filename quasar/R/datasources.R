# ============================================================
# QUASAR — Data Sources Registry
# qsr_search() + expanded qsr_fetch() dispatchers
# "Find any research data in one line."
# ============================================================

#' Search for research data across all sources
#'
#' Searches across World Bank, Eurostat, OECD, IMF, FRED, and other
#' sources for indicators matching a keyword. Returns a table of
#' available datasets with source, ID, and description.
#'
#' @param query Character. Search term (e.g., "GDP", "unemployment",
#'   "inflation", "population").
#' @param sources Character vector. Sources to search in. Default NULL
#'   searches all available sources.
#' @param limit Integer. Max results per source. Default 10.
#'
#' @return A data.frame with columns: source, id, description.
#'
#' @details
#' Searches a built-in catalog of popular research indicators, plus
#' live searches on sources that support it (World Bank, Eurostat).
#'
#' After finding what you need, fetch it with:
#' `qsr_fetch("source_name", indicator = "indicator_id")`
#'
#' @examples
#' \dontrun{
#' # Search for GDP data across all sources
#' qsr_search("GDP")
#'
#' # Search only in Eurostat and OECD
#' qsr_search("inflation", sources = c("eurostat", "oecd"))
#'
#' # Then fetch what you found
#' qsr_fetch("worldbank", indicator = "NY.GDP.PCAP.CD")
#' }
#'
#' @export
qsr_search <- function(query, sources = NULL, limit = 10L) {

  query <- tolower(query)

  if (is.null(sources)) {
    sources <- names(.qsr_source_catalog)
  }

  results <- list()

  for (src in sources) {
    if (src %in% names(.qsr_source_catalog)) {
      catalog <- .qsr_source_catalog[[src]]
      # Search description and id
      matches <- vapply(seq_len(nrow(catalog)), function(i) {
        grepl(query, tolower(catalog$description[i])) ||
        grepl(query, tolower(catalog$id[i])) ||
        grepl(query, tolower(catalog$topic[i]))
      }, logical(1))

      if (any(matches)) {
        matched <- catalog[matches, , drop = FALSE]
        matched <- utils::head(matched, limit)
        matched$source <- src
        results <- c(results, list(matched[, c("source", "id", "description", "topic")]))
      }
    }
  }

  # Also try live search on World Bank if WDI is available
  if ("worldbank" %in% sources && requireNamespace("WDI", quietly = TRUE)) {
    tryCatch({
      wb_results <- WDI::WDIsearch(query)
      if (nrow(wb_results) > 0) {
        wb_df <- data.frame(
          source = "worldbank",
          id = wb_results[, "indicator"],
          description = wb_results[, "name"],
          topic = "various",
          stringsAsFactors = FALSE
        )
        results <- c(results, list(utils::head(wb_df, limit)))
      }
    }, error = function(e) NULL)
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No results for {.val {query}}. Try broader terms.")
    return(invisible(data.frame(
      source = character(), id = character(),
      description = character(), topic = character()
    )))
  }

  out <- do.call(rbind, results)
  out <- out[!duplicated(paste(out$source, out$id)), ]
  rownames(out) <- NULL

  cli::cli_alert_success("Found {.val {nrow(out)}} datasets matching {.val {query}}")
  cli::cli_text("")
  print(out, right = FALSE, row.names = FALSE)
  cli::cli_text("")
  cli::cli_alert_info(
    'Fetch with: {.code qsr_fetch("source", indicator = "id")}'
  )

  invisible(out)
}


# ============================================================
# Built-in catalog of popular research indicators
# ============================================================

.qsr_build_catalog <- function() {
  list(
    worldbank = data.frame(
      id = c(
        "NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
        "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.ZS", "SP.POP.TOTL",
        "SP.DYN.LE00.IN", "SE.ADT.LITR.ZS", "SI.POV.GINI",
        "BX.KLT.DINV.WD.GD.ZS", "NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS",
        "GC.DOD.TOTL.GD.ZS", "NY.GNS.ICTR.ZS", "SP.DYN.TFRT.IN",
        "SH.XPD.CHEX.GD.ZS", "SE.XPD.TOTL.GD.ZS", "EG.USE.ELEC.KH.PC",
        "EN.ATM.CO2E.PC", "IT.NET.USER.ZS"
      ),
      description = c(
        "GDP per capita (current US$)", "GDP (current US$)", "GDP growth (annual %)",
        "Inflation, CPI (annual %)", "Unemployment (% of labor force)", "Population, total",
        "Life expectancy at birth", "Literacy rate, adult", "Gini index",
        "FDI net inflows (% of GDP)", "Exports of goods and services (% of GDP)",
        "Imports of goods and services (% of GDP)",
        "Government debt (% of GDP)", "Gross savings (% of GDP)",
        "Fertility rate (births per woman)",
        "Health expenditure (% of GDP)", "Education expenditure (% of GDP)",
        "Electric power consumption (kWh per capita)",
        "CO2 emissions (metric tons per capita)", "Internet users (% of population)"
      ),
      topic = c(
        "gdp", "gdp", "gdp", "inflation", "employment", "population",
        "health", "education", "inequality", "investment", "trade", "trade",
        "debt", "savings", "population", "health", "education",
        "energy", "environment", "technology"
      ),
      stringsAsFactors = FALSE
    ),

    eurostat = data.frame(
      id = c(
        "nama_10_gdp", "nama_10_pc", "prc_hicp_aind", "une_rt_m",
        "demo_pjan", "demo_mlexpec", "educ_uoe_enrt01",
        "tec00114", "sts_inpr_m", "ext_lt_maineu",
        "tour_occ_nim", "env_air_gge", "isoc_ci_ifp_iu",
        "hlth_silc_17", "earn_ses_annual"
      ),
      description = c(
        "GDP and main components", "GDP per capita in PPS",
        "HICP - annual average inflation", "Unemployment rate monthly",
        "Population on 1 January", "Life expectancy by age and sex",
        "Students enrolled in education",
        "Government debt (% of GDP)", "Industrial production index",
        "International trade by partner",
        "Tourism nights spent", "Greenhouse gas emissions",
        "Internet use by individuals", "Self-perceived health",
        "Annual earnings"
      ),
      topic = c(
        "gdp", "gdp", "inflation", "employment", "population", "health",
        "education", "debt", "industry", "trade",
        "tourism", "environment", "technology", "health", "wages"
      ),
      stringsAsFactors = FALSE
    ),

    oecd = data.frame(
      id = c(
        "GDP_PER_CAPITA", "CPI", "UNEMPLOYMENT_RATE", "POPULATION",
        "TRADE_IN_GOODS", "FDI_FLOWS", "EDUCATION_SPENDING",
        "HEALTH_SPENDING", "GINI_COEFFICIENT", "R_D_SPENDING",
        "HOUSING_PRICES", "INTEREST_RATES", "EXCHANGE_RATES"
      ),
      description = c(
        "GDP per capita (USD PPP)", "Consumer prices (annual change)",
        "Harmonised unemployment rate", "Population total",
        "Trade in goods (exports + imports)", "FDI flows",
        "Education spending (% of GDP)", "Health spending (% of GDP)",
        "Gini coefficient (income inequality)",
        "R&D spending (% of GDP)", "Housing prices index",
        "Long-term interest rates", "Exchange rates"
      ),
      topic = c(
        "gdp", "inflation", "employment", "population", "trade",
        "investment", "education", "health", "inequality",
        "innovation", "housing", "finance", "finance"
      ),
      stringsAsFactors = FALSE
    ),

    imf = data.frame(
      id = c(
        "NGDPDPC", "PCPIPCH", "LUR", "BCA_NGDPD",
        "GGXWDG_NGDP", "GGR_NGDP", "GGX_NGDP",
        "LP", "TM_RPCH", "TX_RPCH"
      ),
      description = c(
        "GDP per capita, current prices", "Inflation, average consumer prices",
        "Unemployment rate", "Current account balance (% of GDP)",
        "Government gross debt (% of GDP)", "Government revenue (% of GDP)",
        "Government expenditure (% of GDP)",
        "Population (millions)", "Import volume growth",
        "Export volume growth"
      ),
      topic = c(
        "gdp", "inflation", "employment", "trade", "debt",
        "fiscal", "fiscal", "population", "trade", "trade"
      ),
      stringsAsFactors = FALSE
    ),

    fred = data.frame(
      id = c(
        "GDPC1", "CPIAUCSL", "UNRATE", "FEDFUNDS", "DGS10",
        "M2SL", "HOUST", "PAYEMS", "INDPRO", "UMCSENT",
        "SP500", "DEXUSEU", "T10Y2Y", "MORTGAGE30US",
        "PCE", "PCEPI"
      ),
      description = c(
        "Real GDP (USA)", "Consumer Price Index (Urban)",
        "Unemployment Rate", "Federal Funds Rate", "10-Year Treasury Rate",
        "M2 Money Supply", "Housing Starts",
        "Nonfarm Payrolls", "Industrial Production Index",
        "Consumer Sentiment (Michigan)",
        "S&P 500 Index", "USD/EUR Exchange Rate",
        "10Y-2Y Treasury Spread", "30-Year Fixed Mortgage Rate",
        "Personal Consumption Expenditures",
        "PCE Price Index (Fed's preferred inflation)"
      ),
      topic = c(
        "gdp", "inflation", "employment", "finance", "finance",
        "money", "housing", "employment", "industry", "sentiment",
        "markets", "finance", "finance", "housing",
        "consumption", "inflation"
      ),
      stringsAsFactors = FALSE
    ),

    ine_spain = data.frame(
      id = c(
        "IPC", "EPA", "PIB", "COMERCIO_EXTERIOR",
        "TURISMO", "HIPOTECAS", "DEMOGRAFICA",
        "ENCUESTA_SALARIOS", "IPI"
      ),
      description = c(
        "Indice de Precios al Consumo", "Encuesta de Poblacion Activa",
        "Contabilidad Nacional - PIB", "Comercio exterior",
        "Estadistica de turismo", "Estadistica de hipotecas",
        "Cifras de poblacion", "Encuesta anual de estructura salarial",
        "Indice de Produccion Industrial"
      ),
      topic = c(
        "inflation", "employment", "gdp", "trade",
        "tourism", "housing", "population", "wages", "industry"
      ),
      stringsAsFactors = FALSE
    ),

    inegi_mexico = data.frame(
      id = c(
        "PIB", "INPC", "ENOE", "BALANZA_COMERCIAL",
        "CENSO_POBLACION", "ENIGH", "INDUSTRIA_MANUFACTURERA"
      ),
      description = c(
        "Producto Interno Bruto", "Indice Nacional de Precios al Consumidor",
        "Encuesta Nacional de Ocupacion y Empleo", "Balanza comercial",
        "Censo de Poblacion y Vivienda", "Encuesta Nacional de Ingresos y Gastos",
        "Encuesta Mensual de la Industria Manufacturera"
      ),
      topic = c(
        "gdp", "inflation", "employment", "trade",
        "population", "income", "industry"
      ),
      stringsAsFactors = FALSE
    ),

    bls_usa = data.frame(
      id = c(
        "CES0000000001", "LNS14000000", "CUUR0000SA0",
        "CES0500000003", "PRS85006092"
      ),
      description = c(
        "Total Nonfarm Employment", "Unemployment Rate",
        "CPI All Urban Consumers", "Average Hourly Earnings",
        "Nonfarm Business Labor Productivity"
      ),
      topic = c(
        "employment", "employment", "inflation", "wages", "productivity"
      ),
      stringsAsFactors = FALSE
    ),

    google_trends = data.frame(
      id = c("keyword_search"),
      description = c("Google search trends by keyword, country, and time period"),
      topic = c("trends"),
      stringsAsFactors = FALSE
    )
  )
}

# Build catalog at package load
.qsr_source_catalog <- .qsr_build_catalog()


# ============================================================
# Smart download guidance for auth-required portals
# ============================================================

#' @noRd
.qsr_download_guides <- list(
  ine_bolivia = list(
    name = "INE Bolivia — Encuestas de Hogares",
    url = "https://anda.ine.gob.bo/index.php/catalog",
    auth = "registration + form",
    steps = c(
      "Go to: https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/",
      "Select 'Encuestas de Hogares {YEAR}' from the dropdown",
      "Click the catalog link -> fill the usage form -> accept terms",
      "Download the ZIP file",
      "Save to your data_path folder (e.g., E:/quasar_data/raw/bolivia/)"
    ),
    then = 'qsr_load_folder("E:/quasar_data/raw/bolivia/", extract_zips = TRUE)',
    catalog_ids = c(
      "2012" = 51, "2013" = 39, "2014" = 38, "2015" = 53,
      "2016" = 54, "2017" = 55, "2018" = 78, "2019" = 84,
      "2020" = 88, "2021" = 93, "2022" = 106, "2023" = 108,
      "2024" = 163
    )
  ),
  inei_peru = list(
    name = "INEI Peru — ENAHO",
    url = "https://iinei.inei.gob.pe/microdatos/",
    auth = "direct download (navigate portal)",
    steps = c(
      "Go to: https://iinei.inei.gob.pe/microdatos/",
      "Select: Encuesta Nacional de Hogares (ENAHO)",
      "Select year and module (601 = agricultural, 100 = household)",
      "Download the ZIP file",
      "Save to your data_path folder"
    ),
    then = 'qsr_load_folder("E:/quasar_data/raw/peru/", pattern = "601", extract_zips = TRUE)'
  ),
  dane_colombia = list(
    name = "DANE Colombia — ENCV/GEIH",
    url = "https://microdatos.dane.gov.co/",
    auth = "registration + download",
    steps = c(
      "Go to: https://microdatos.dane.gov.co/",
      "Search: Encuesta Nacional de Calidad de Vida",
      "Select year -> download microdata",
      "Save to your data_path folder"
    ),
    then = 'qsr_load_folder("E:/quasar_data/raw/colombia/", extract_zips = TRUE)'
  ),
  lapop = list(
    name = "LAPOP — Latin American Public Opinion Project",
    url = "https://www.vanderbilt.edu/lapop/data-access.php",
    auth = "formal request (1-2 weeks)",
    steps = c(
      "Go to: https://www.vanderbilt.edu/lapop/data-access.php",
      "Submit data access request with research description",
      "Wait for approval (~1-2 weeks)",
      "Download .dta files for Bolivia, Peru, Colombia",
      "Save to your data_path folder"
    ),
    then = 'qsr_load_folder("E:/quasar_data/raw/lapop/", extract_zips = TRUE)'
  ),
  pnis_colombia = list(
    name = "ARN Colombia — PNIS beneficiary data",
    url = "https://www.renovacionterritorio.gov.co/",
    auth = "formal written request (1-3 months)",
    steps = c(
      "Email: comunicaciones@renovacionterritorio.gov.co",
      "Legal basis: Art. 23, Ley 1712 de 2014 (Colombia)",
      "Request: PNIS beneficiary database with follow-up data",
      "Attach: CV + DOI of published paper as credential",
      "Wait: 10 business days (official), 1-3 months (real)"
    ),
    then = 'qsr_load_folder("E:/quasar_data/raw/pnis/", extract_zips = TRUE)'
  )
)


#' Show download guidance for a data source that requires manual steps
#'
#' When a data source requires authentication, form submission, or
#' formal request, QUASAR guides the researcher through the manual
#' steps and tells them what to run after downloading.
#'
#' @param source Character. Source name: "ine_bolivia", "inei_peru",
#'   "dane_colombia", "lapop", "pnis_colombia".
#' @param year Character or integer. Specific year (if applicable).
#'
#' @examples
#' \dontrun{
#' qsr_guide("ine_bolivia")
#' qsr_guide("inei_peru")
#' qsr_guide("pnis_colombia")
#' }
#'
#' @export
qsr_guide <- function(source, year = NULL) {

  source <- tolower(source)

  if (!source %in% names(.qsr_download_guides)) {
    available <- paste(names(.qsr_download_guides), collapse = ", ")
    cli::cli_abort(c(
      "No guide for {.val {source}}.",
      "i" = "Available: {available}"
    ))
  }

  g <- .qsr_download_guides[[source]]

  cli::cli_h2(g$name)
  cli::cli_alert_warning("This source requires: {.strong {g$auth}}")
  cli::cli_alert_info("Portal: {.url {g$url}}")
  cli::cli_text("")

  cli::cli_h3("Manual steps:")
  for (i in seq_along(g$steps)) {
    step <- g$steps[i]
    if (!is.null(year)) {
      step <- gsub("\\{YEAR\\}", year, step)
    }
    cli::cli_text("  {.strong {i}.} {step}")
  }

  # Show catalog URLs for INE Bolivia
  if (!is.null(g$catalog_ids) && !is.null(year)) {
    yr <- as.character(year)
    if (yr %in% names(g$catalog_ids)) {
      cat_id <- g$catalog_ids[yr]
      cli::cli_text("")
      cli::cli_alert_info(
        "Direct link: {.url {paste0('https://anda.ine.gob.bo/index.php/catalog/', cat_id, '/get-microdata')}}"
      )
    }
  }

  if (!is.null(g$catalog_ids) && is.null(year)) {
    cli::cli_text("")
    cli::cli_h3("Available years:")
    for (yr in names(g$catalog_ids)) {
      cat_id <- g$catalog_ids[yr]
      cli::cli_text(
        "  {yr}: {.url {paste0('https://anda.ine.gob.bo/index.php/catalog/', cat_id, '/get-microdata')}}"
      )
    }
  }

  data_path <- .qsr_context$get("data_path")
  cli::cli_text("")
  cli::cli_h3("After downloading:")
  if (!is.null(data_path)) {
    cli::cli_text("  Save files to: {.path {data_path}}")
  }
  cli::cli_text("  Then run:")
  cli::cli_code(g$then)
}


# ============================================================
# Expanded fetch dispatchers
# ============================================================

#' @noRd
.qsr_fetch_eurostat <- function(dataset, ...) {
  .qsr_require("eurostat", "for Eurostat data")
  df <- eurostat::get_eurostat(dataset, ...)
  as.data.frame(df)
}

#' @noRd
.qsr_fetch_oecd <- function(dataset, country = NULL, ...) {
  .qsr_require("OECD", "for OECD data")
  if (is.null(country)) {
    df <- OECD::get_dataset(dataset, ...)
  } else {
    filter_list <- list(country)
    df <- OECD::get_dataset(dataset, filter = filter_list, ...)
  }
  as.data.frame(df)
}

#' @noRd
.qsr_fetch_imf <- function(indicator, country = "all", start = 1980,
                            end = NULL, ...) {
  if (is.null(end)) end <- as.integer(format(Sys.Date(), "%Y"))
  .qsr_require("httr2", "for IMF API")
  .qsr_require("jsonlite", "for JSON parsing")

  # IMF World Economic Outlook API
  base_url <- "https://www.imf.org/external/datamapper/api/v1"

  url <- paste0(base_url, "/", indicator)
  resp <- tryCatch({
    req <- httr2::request(url)
    req <- httr2::req_timeout(req, 30)
    httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to connect to IMF API.",
      "i" = "Check your internet connection and indicator ID: {.val {indicator}}"
    ))
  })

  body <- httr2::resp_body_json(resp)
  values <- body$values[[indicator]]

  if (is.null(values)) {
    cli::cli_abort("Indicator {.val {indicator}} not found in IMF database.")
  }

  # Parse the nested JSON into a data frame
  rows <- list()
  for (cty in names(values)) {
    for (yr in names(values[[cty]])) {
      year_num <- as.integer(yr)
      if (year_num >= start && year_num <= end) {
        if (country == "all" || cty %in% country) {
          rows <- c(rows, list(data.frame(
            country = cty,
            year = year_num,
            value = as.numeric(values[[cty]][[yr]]),
            stringsAsFactors = FALSE
          )))
        }
      }
    }
  }

  if (length(rows) == 0) {
    cli::cli_abort("No data found for {.val {indicator}} in the specified range.")
  }

  do.call(rbind, rows)
}

#' @noRd
.qsr_fetch_ine_spain <- function(table_id, ...) {
  .qsr_require("httr2", "for INE Spain API")
  .qsr_require("jsonlite", "for JSON parsing")

  # INE Spain JSON API
  url <- paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/", table_id)

  resp <- tryCatch({
    req <- httr2::request(url)
    req <- httr2::req_timeout(req, 30)
    httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to connect to INE Spain API.",
      "i" = "Check table ID: {.val {table_id}}",
      "i" = "Browse tables at: {.url https://www.ine.es/dyngs/INEbase/listaoperaciones.htm}"
    ))
  })

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (length(body) == 0) {
    cli::cli_abort("No data found for table {.val {table_id}}")
  }

  # Parse INE response format
  rows <- list()
  for (series in body) {
    nombre <- series$Nombre
    if (!is.null(series$Data)) {
      for (d in series$Data) {
        rows <- c(rows, list(data.frame(
          indicator = nombre,
          date = d$Fecha %||% NA,
          value = d$Valor %||% NA,
          stringsAsFactors = FALSE
        )))
      }
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

#' @noRd
.qsr_fetch_inegi <- function(indicator, token = NULL, ...) {
  .qsr_require("httr2", "for INEGI API")
  .qsr_require("jsonlite", "for JSON parsing")

  token <- token %||% Sys.getenv("INEGI_TOKEN", unset = "")
  if (!nzchar(token)) {
    cli::cli_abort(c(
      "INEGI requires an API token.",
      "i" = "Get one at: {.url https://www.inegi.org.mx/app/api/indicadores/interna_v1_1/tokenVerify.aspx}",
      "i" = 'Set it with: {.code Sys.setenv(INEGI_TOKEN = "your_token")}'
    ))
  }

  url <- paste0(
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
    indicator, "/es/0700/false/BIE/2.0/", token, "?type=json"
  )

  resp <- tryCatch({
    req <- httr2::request(url)
    req <- httr2::req_timeout(req, 30)
    httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_abort("Failed to connect to INEGI API.")
  })

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  observations <- body$Series[[1]]$OBSERVATIONS
  if (is.null(observations) || length(observations) == 0) {
    cli::cli_abort("No data found for indicator {.val {indicator}}")
  }

  data.frame(
    date = observations$TIME_PERIOD,
    value = as.numeric(observations$OBS_VALUE),
    stringsAsFactors = FALSE
  )
}

#' @noRd
.qsr_fetch_bls <- function(series_id, start_year = 2000, end_year = NULL, ...) {
  if (is.null(end_year)) end_year <- as.integer(format(Sys.Date(), "%Y"))
  .qsr_require("httr2", "for BLS API")
  .qsr_require("jsonlite", "for JSON parsing")

  url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

  body <- list(
    seriesid = list(series_id),
    startyear = as.character(start_year),
    endyear = as.character(end_year)
  )

  key <- Sys.getenv("BLS_API_KEY", unset = "")
  if (nzchar(key)) body$registrationkey <- key

  resp <- tryCatch({
    req <- httr2::request(url)
    req <- httr2::req_body_json(req, body)
    req <- httr2::req_timeout(req, 30)
    httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_abort("Failed to connect to BLS API.")
  })

  result <- httr2::resp_body_json(resp)

  if (result$status != "REQUEST_SUCCEEDED") {
    cli::cli_abort("BLS API error: {result$message[[1]]}")
  }

  series_data <- result$Results$series[[1]]$data
  rows <- lapply(series_data, function(d) {
    data.frame(
      year = as.integer(d$year),
      period = d$period,
      value = as.numeric(d$value),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' @noRd
.qsr_fetch_gtrends <- function(keyword, geo = "", time = "today 12-m", ...) {
  .qsr_require("gtrendsR", "for Google Trends")
  result <- gtrendsR::gtrends(keyword = keyword, geo = geo, time = time, ...)
  as.data.frame(result$interest_over_time)
}
