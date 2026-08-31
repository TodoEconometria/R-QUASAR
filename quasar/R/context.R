#' QUASAR Context - Global project configuration and object store
#' @noRd
QuasarContext <- R6::R6Class(
  classname = "QuasarContext",
  private = list(
    .config      = list(),
    .data        = NULL,
    .models      = list(),
    .tables      = list(),
    .plots       = list(),
    .connections = list()
  ),
  public = list(

    #' @description Set a configuration value
    set = function(key, value) {
      private$.config[[key]] <- value
      invisible(self)
    },

    #' @description Get a configuration value
    get = function(key, default = NULL) {
      if (key %in% names(private$.config)) {
        private$.config[[key]]
      } else {
        default
      }
    },

    #' @description Get all configuration values
    all = function() {
      private$.config
    },

    #' @description Store the active dataset
    set_data = function(data) {
      private$.data <- data
      invisible(self)
    },

    #' @description Get the active dataset
    get_data = function() {
      private$.data
    },

    #' @description Store a model (named or as 'last')
    set_model = function(model, name = "last") {
      private$.models[[name]] <- model
      invisible(self)
    },

    #' @description Get a model by name
    get_model = function(name = "last") {
      private$.models[[name]]
    },

    #' @description Get all stored models
    get_models = function() {
      private$.models
    },

    #' @description Store a generated table
    store_table = function(tbl, name = NULL) {
      name <- name %||% paste0("table_", length(private$.tables) + 1)
      private$.tables[[name]] <- tbl
      invisible(self)
    },

    #' @description Get stored tables
    get_tables = function() {
      private$.tables
    },

    #' @description Store a generated plot
    store_plot = function(plt, name = NULL) {
      name <- name %||% paste0("plot_", length(private$.plots) + 1)
      private$.plots[[name]] <- plt
      invisible(self)
    },

    #' @description Get stored plots
    get_plots = function() {
      private$.plots
    },

    #' @description Store a connection
    set_connection = function(conn, name) {
      private$.connections[[name]] <- conn
      invisible(self)
    },

    #' @description Get a connection by name
    get_connection = function(name) {
      private$.connections[[name]]
    },

    #' @description Get all connections
    get_connections = function() {
      private$.connections
    },

    #' @description Reset context to empty state
    reset = function() {
      # Gracefully disconnect active connections
      purrr::walk(names(private$.connections), function(name) {
        conn <- private$.connections[[name]]
        if (is.null(conn)) return()
        tryCatch({
          if (inherits(conn, "DBIConnection") && DBI::dbIsValid(conn)) {
            DBI::dbDisconnect(conn)
          } else if (inherits(conn, "spark_connection") &&
                     requireNamespace("sparklyr", quietly = TRUE)) {
            sparklyr::spark_disconnect(conn)
          }
        }, error = function(e) NULL)
      })
      private$.config      <- list()
      private$.data        <- NULL
      private$.models      <- list()
      private$.tables      <- list()
      private$.plots       <- list()
      private$.connections <- list()
      invisible(self)
    },

    #' @description Print context summary
    print = function(...) {
      cli::cli_h2("QUASAR Project Context")
      if (length(private$.config) == 0 && is.null(private$.data) &&
          length(private$.models) == 0) {
        cli::cli_alert_warning("Empty. Start with {.fn qsr_data} and {.fn qsr_config}.")
        return(invisible(self))
      }

      # Config params
      if (length(private$.config) > 0) {
        cli::cli_text("{.strong Config:}")
        purrr::iwalk(private$.config, function(val, key) {
          if (!is.data.frame(val) && !is.list(val) && length(val) == 1) {
            cli::cli_inform("  {.field {key}}: {.val {val}}")
          }
        })
      }

      # Data
      if (!is.null(private$.data)) {
        nr <- nrow(private$.data)
        nc <- ncol(private$.data)
        cli::cli_text("{.strong Data:} {nr} rows x {nc} cols")
      }

      # Models
      if (length(private$.models) > 0) {
        n_models <- length(private$.models)
        model_names <- paste(names(private$.models), collapse = ", ")
        cli::cli_text("{.strong Models:} {n_models} ({model_names})")
      }

      # Connections
      if (length(private$.connections) > 0) {
        conn_names <- paste(names(private$.connections), collapse = ", ")
        cli::cli_text("{.strong Connections:} {conn_names}")
      }

      # Outputs
      n_tables <- length(private$.tables)
      n_plots  <- length(private$.plots)
      if (n_tables > 0 || n_plots > 0) {
        cli::cli_text("{.strong Outputs:} {n_tables} tables, {n_plots} plots")
      }

      invisible(self)
    }
  )
)

# Global singleton instance - one per R session
.qsr_context <- QuasarContext$new()


#' Set global QUASAR project configuration
#'
#' Defines the project context once. All QUASAR functions read from
#' this configuration automatically without requiring repeated arguments.
#'
#' @param significance_level Numeric. Default significance level. Default 0.05.
#' @param random_seed Integer. Random seed for reproducibility. Default 42.
#' @param output_format Character. Output format. One of "apa7", "chicago", "custom".
#' @param outlier_threshold Numeric. SD threshold for outlier detection. Default 3.
#' @param data_path Character. Optional path for storing downloaded data and cache.
#' @param config_path Character. Optional path to a YAML config file.
#' @param ... Additional named parameters stored in context.
#'
#' @return Invisible - the global context object.
#' @export
#'
#' @examples
#' qsr_config(
#'   significance_level = 0.05,
#'   random_seed        = 42,
#'   output_format      = "apa7"
#' )
qsr_config <- function(significance_level = 0.05,
                       random_seed        = 42,
                       output_format      = c("apa7", "chicago", "custom"),
                       outlier_threshold  = 3,
                       data_path          = NULL,
                       config_path        = NULL,
                       ...) {

  output_format <- match.arg(output_format)

  # Load from YAML file if provided
  if (!is.null(config_path)) {
    if (!file.exists(config_path)) {
      cli::cli_abort("Config file not found: {.path {config_path}}")
    }
    yaml_config <- yaml::read_yaml(config_path)
    purrr::iwalk(yaml_config, function(val, key) {
      purrr::iwalk(val, function(v, k) {
        .qsr_context$set(k, v)
      })
    })
    cli::cli_alert_success("Loaded config from {.path {config_path}}")
  }

  # Set core parameters
  .qsr_context$set("significance_level", significance_level)
  .qsr_context$set("random_seed",        random_seed)
  .qsr_context$set("output_format",      output_format)
  .qsr_context$set("outlier_threshold",  outlier_threshold)

  # Data storage path - defaults to project data/ or user-specified
  if (!is.null(data_path)) {
    data_path <- normalizePath(data_path, mustWork = FALSE)
    if (!dir.exists(data_path)) {
      dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
    }
    .qsr_context$set("data_path", data_path)
    cli::cli_alert_info("Data storage: {.path {data_path}}")
  }

  # Store any extra parameters
  dots <- list(...)
  if (length(dots) > 0) {
    purrr::iwalk(dots, function(val, key) {
      .qsr_context$set(key, val)
    })
  }

  # Set random seed globally
  set.seed(random_seed)

  cli::cli_alert_success("QUASAR context configured")
  print(.qsr_context)

  invisible(.qsr_context)
}


#' Register the active dataset
#'
#' Stores a data frame in the QUASAR context so all downstream functions
#' (`qsr_model`, `qsr_table`, `qsr_plot`) use it automatically.
#'
#' @param data A data frame.
#' @param name Character. Optional display name for the dataset.
#'
#' @return Invisible - the data frame.
#' @export
#'
#' @examples
#' qsr_data(mtcars)
qsr_data <- function(data, name = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  .qsr_context$set_data(data)

  if (!is.null(name)) {
    .qsr_context$set("data_name", name)
  }

  nr <- nrow(data)
  nc <- ncol(data)
  display <- name %||% "dataset"
  cli::cli_alert_success("Registered {.val {display}}: {nr} rows x {nc} cols")

  invisible(data)
}


#' Fit a model with minimal syntax
#'
#' Fits a statistical model using the active dataset from context.
#' No need to pass data - QUASAR already knows it.
#'
#' @param formula A model formula (e.g., `mpg ~ wt + hp`).
#' @param type Character. Model type: `"lm"`, `"glm"`, `"logit"`, `"probit"`.
#'   Default `"lm"`.
#' @param data Optional data frame. If `NULL`, uses the active dataset
#'   from context.
#' @param name Character. Name to store the model under. Default `"last"`.
#' @param family For GLM models. Ignored for `"lm"`.
#' @param weights Optional survey/regression weights: either the name of a
#'   column in `data` or a numeric vector of length `nrow(data)`. Essential for
#'   survey microdata, where estimates must use the elevation factor.
#' @param design Optional complex-survey design for **design-based** inference:
#'   a `survey.design` from [qsr_svydesign()] (or [survey::svrepdesign()]), or
#'   `TRUE` to reuse the design registered by [qsr_svydesign()]. Fits via
#'   [survey::svyglm()], so the standard errors account for strata/clusters, not
#'   just the weights. Supersedes `weights` and `data` when supplied.
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return The fitted model object (also stored in context).
#' @export
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#' qsr_model(mpg ~ wt + hp)
#' qsr_model(mpg ~ wt + hp + disp, name = "full")
#' qsr_model(am ~ wt + hp, type = "logit")
#'
#' # Weighted regression on survey microdata:
#' qsr_read("EPA2016.sav")
#' qsr_model(parado ~ sexo + edad + educacion, weights = "FACTOREL")
#' }
qsr_model <- function(formula,
                      type   = c("lm", "glm", "logit", "probit"),
                      data   = NULL,
                      name   = "last",
                      family = NULL,
                      weights = NULL,
                      design = NULL,
                      ...) {

  type <- match.arg(type)

  # Design-based fit via survey::svyglm for complex-survey inference. The point
  # estimates match a weighted lm/glm, but the SEs account for strata/clusters
  # (what a paper needs). `design = TRUE` reuses the design from qsr_svydesign().
  if (!is.null(design)) {
    if (!requireNamespace("survey", quietly = TRUE)) {
      cli::cli_abort(c(
        "{.arg design} needs the {.pkg survey} package.",
        "i" = "Install it with {.code install.packages(\"survey\")}."
      ))
    }
    des <- if (isTRUE(design)) .qsr_context$get("design") else design
    if (is.null(des) || !inherits(des, "survey.design")) {
      cli::cli_abort(c(
        "{.arg design} must be a {.cls survey.design} (from {.fn qsr_svydesign}) or {.code TRUE} to reuse the registered one.",
        "i" = "Register one first with {.fn qsr_svydesign}."
      ))
    }
    fam <- switch(type,
      lm     = stats::gaussian(),
      glm    = family %||% stats::gaussian(),
      logit  = stats::binomial(link = "logit"),
      probit = stats::binomial(link = "probit")
    )
    model <- survey::svyglm(formula, design = des, family = fam, ...)
    .qsr_context$set_model(model, name)
    resp  <- as.character(formula)[2]
    preds <- as.character(formula)[3]
    cli::cli_alert_success(
      "Model {.val {name}} (svyglm/{type}): {resp} ~ {preds} {.emph (design-based SE)}"
    )
    return(invisible(model))
  }

  # Get data from context if not provided
  data <- data %||% .qsr_context$get_data()
  if (is.null(data)) {
    cli::cli_abort(c(
      "No data available.",
      "i" = "Register data first with {.fn qsr_data} or pass {.arg data}."
    ))
  }

  # Resolve weights to a local numeric vector (a column name or a vector), so
  # lm/glm evaluate them correctly from this frame (survey weights need this).
  w <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1L) {
      if (!weights %in% names(data)) {
        cli::cli_abort("Weight column {.val {weights}} not found in data.")
      }
      w <- as.numeric(data[[weights]])
    } else if (is.numeric(weights) && length(weights) == nrow(data)) {
      w <- as.numeric(weights)
    } else {
      cli::cli_abort(
        "{.arg weights} must be a column name in {.arg data} or a numeric vector of length {nrow(data)}."
      )
    }
  }

  # Fit model (branch on whether weights were supplied so unweighted calls are
  # unchanged).
  model <- if (is.null(w)) {
    switch(type,
      lm     = stats::lm(formula, data = data, ...),
      glm    = stats::glm(formula, data = data, family = family %||% stats::gaussian(), ...),
      logit  = stats::glm(formula, data = data, family = stats::binomial(link = "logit"), ...),
      probit = stats::glm(formula, data = data, family = stats::binomial(link = "probit"), ...)
    )
  } else {
    switch(type,
      lm     = stats::lm(formula, data = data, weights = w, ...),
      glm    = stats::glm(formula, data = data, family = family %||% stats::gaussian(), weights = w, ...),
      logit  = stats::glm(formula, data = data, family = stats::binomial(link = "logit"), weights = w, ...),
      probit = stats::glm(formula, data = data, family = stats::binomial(link = "probit"), weights = w, ...)
    )
  }

  # Store in context
  .qsr_context$set_model(model, name)

  # Report
  resp  <- as.character(formula)[2]
  preds <- as.character(formula)[3]
  cli::cli_alert_success(
    "Model {.val {name}} ({type}): {resp} ~ {preds}"
  )

  invisible(model)
}


#' Retrieve a value from the QUASAR context
#'
#' Used internally by all QUASAR functions to read configuration.
#'
#' @param key Character. The configuration key to retrieve.
#' @param default Default value if key is not found.
#'
#' @return The stored value or default.
#' @export
#'
#' @examples
#' qsr_config(random_seed = 123)
#' qsr_get("random_seed")
qsr_get <- function(key, default = NULL) {
  .qsr_context$get(key, default)
}


#' Reset the QUASAR context
#'
#' Clears all configuration, data, models, and outputs from the session.
#'
#' @return Invisible.
#' @export
#'
#' @examples
#' qsr_reset()
qsr_reset <- function() {
  .qsr_context$reset()
  cli::cli_alert_info("QUASAR context reset.")
  invisible(NULL)
}
