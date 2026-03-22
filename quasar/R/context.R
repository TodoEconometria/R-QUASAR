#' QUASAR Context — Global project configuration
#'
#' @description
#' R6 singleton that holds the global configuration for a QUASAR project.
#' All framework functions read from this context automatically.
#'
#' @keywords internal
QuasarContext <- R6::R6Class(
  classname = "QuasarContext",
  private = list(
    .config = list()
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

    #' @description Reset context to empty state
    reset = function() {
      private$.config <- list()
      invisible(self)
    },

    #' @description Print context summary
    print = function(...) {
      cli::cli_h2("QUASAR Project Context")
      if (length(private$.config) == 0) {
        cli::cli_alert_warning("No configuration set. Run {.fn qsr_config} first.")
      } else {
        purrr::iwalk(private$.config, function(val, key) {
          cli::cli_inform("  {.field {key}}: {.val {val}}")
        })
      }
      invisible(self)
    }
  )
)

# Global singleton instance — one per R session
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
#' @param config_path Character. Optional path to a YAML config file.
#' @param ... Additional named parameters stored in context.
#'
#' @return Invisible — the global context object.
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


#' Retrieve a value from the QUASAR context
#'
#' Used internally by all QUASAR functions to read configuration.
#'
#' @param key Character. The configuration key to retrieve.
#' @param default Default value if key is not foun

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
#' Clears all configuration from the current session context.
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
