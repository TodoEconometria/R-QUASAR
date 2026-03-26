# ============================================================
# QUASAR - Time Series Layer
# qsr_ts()
# "ARIMA, decomposition, forecasting - one line."
# ============================================================

#' Fit and forecast time series models
#'
#' One-line time series analysis: auto-ARIMA, exponential smoothing,
#' decomposition, or custom ARIMA orders. Reads data from context.
#'
#' @param y Numeric vector, ts object, or column name. NULL reads from context.
#' @param data Data frame or NULL.
#' @param method Character. Method: "auto" (auto.arima), "arima", "ets"
#'   (exponential smoothing), "decompose", "hw" (Holt-Winters). Default "auto".
#' @param forecast Integer. Number of periods to forecast. Default 0 (no forecast).
#' @param frequency Integer. Time series frequency (12=monthly, 4=quarterly,
#'   365=daily). Default 12.
#' @param order Integer vector of length 3. ARIMA(p,d,q) order. Only for
#'   method="arima".
#' @param ... Additional arguments.
#' @param name Character. Context name. Default "ts_model".
#'
#' @return A list with model and forecast (invisibly).
#'
#' @examples
#' \dontrun{
#' # Auto-ARIMA with forecast
#' qsr_ts(AirPassengers, method = "auto", forecast = 12)
#'
#' # From a data frame column
#' qsr_data(economics)
#' qsr_ts(y = "unemploy", frequency = 12, forecast = 24)
#'
#' # Decomposition
#' qsr_ts(AirPassengers, method = "decompose")
#'
#' # Holt-Winters
#' qsr_ts(AirPassengers, method = "hw", forecast = 12)
#' }
#'
#' @export
qsr_ts <- function(y = NULL,
                   data = NULL,
                   method = c("auto", "arima", "ets", "decompose", "hw"),
                   forecast = 0L,
                   frequency = 12L,
                   order = NULL,
                   ...,
                   name = "ts_model") {

  method <- match.arg(method)

  # Get time series data
  if (is.null(y)) {
    data <- data %||% .qsr_context$get_data()
    if (is.null(data)) {
      cli::cli_abort("No data. Pass a numeric vector or use {.fn qsr_data} first.")
    }
    # Use first numeric column
    num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(num_cols) == 0) {
      cli::cli_abort("No numeric columns found in data.")
    }
    y <- data[[num_cols[1]]]
    cli::cli_alert_info("Using column: {.val {num_cols[1]}}")
  } else if (is.character(y) && length(y) == 1) {
    data <- data %||% .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("Column name given but no data.")
    y <- data[[y]]
  }

  # Convert to ts object if not already
  if (!stats::is.ts(y)) {
    y <- stats::ts(y, frequency = frequency)
  }

  # Fit model
  result <- switch(method,
    auto = {
      .qsr_require("forecast", "for auto.arima")
      model <- forecast::auto.arima(y, ...)
      cli::cli_alert_success(
        "Model {.strong Auto-ARIMA}: {.val {paste(model$arma[c(1,6,2)], collapse=',')}}"
      )
      list(model = model, method = "auto.arima")
    },
    arima = {
      if (is.null(order)) order <- c(1, 1, 1)
      model <- stats::arima(y, order = order, ...)
      cli::cli_alert_success(
        "Model {.strong ARIMA}({.val {paste(order, collapse=',')}})"
      )
      list(model = model, method = "arima")
    },
    ets = {
      .qsr_require("forecast", "for ETS")
      model <- forecast::ets(y, ...)
      cli::cli_alert_success(
        "Model {.strong ETS}: {model$method}"
      )
      list(model = model, method = "ets")
    },
    decompose = {
      dec <- stats::decompose(y, ...)
      cli::cli_alert_success("Decomposed: trend + seasonal + remainder")
      list(model = dec, method = "decompose")
    },
    hw = {
      model <- stats::HoltWinters(y, ...)
      cli::cli_alert_success(
        "Model {.strong Holt-Winters}: alpha={.val {round(model$alpha, 3)}}, beta={.val {round(model$beta, 3)}}"
      )
      list(model = model, method = "hw")
    }
  )

  # Forecast
  if (forecast > 0 && method != "decompose") {
    if (method %in% c("auto", "ets")) {
      .qsr_require("forecast", "for forecasting")
      fc <- forecast::forecast(result$model, h = forecast)
    } else if (method == "hw") {
      fc <- stats::predict(result$model, n.ahead = forecast)
      fc <- list(mean = fc[, 1])
    } else {
      fc <- stats::predict(result$model, n.ahead = forecast)
      fc <- list(mean = fc$pred)
    }
    result$forecast <- fc

    cli::cli_alert_success("Forecast: {.val {forecast}} periods ahead")
  }

  result$ts <- y
  .qsr_context$set_model(result, name)

  invisible(result)
}
