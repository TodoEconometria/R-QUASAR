# nocov start

#' @importFrom R6 R6Class
#' @importFrom utils head tail
#' @importFrom rlang .data %||%
#' @importFrom stats sd
NULL

# Suppress R CMD check notes for data.table operators
utils::globalVariables(c(":=", ".SD", ".N", ".GRP"))

.onLoad <- function(libname, pkgname) {
  # Ensure context singleton exists in package namespace
  assign(".qsr_context", QuasarContext$new(), envir = parent.env(environment()))
  # Polars sizes its thread pool from POLARS_MAX_THREADS the first time it runs.
  # Cap it (2 by default, CRAN policy) unless the user already chose a value.
  if (!nzchar(Sys.getenv("POLARS_MAX_THREADS"))) {
    Sys.setenv(POLARS_MAX_THREADS = as.character(.qsr_threads()))
  }
  invisible()
}

# Number of threads rquasar may use: option `rquasar.threads` (default 2),
# never above OMP_THREAD_LIMIT when that is set.
.qsr_threads <- function() {
  n <- suppressWarnings(as.integer(getOption("rquasar.threads", 2L))[1L])
  if (is.na(n) || n < 1L) n <- 1L
  lim <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT", "")))
  if (!is.na(lim) && lim >= 1L) n <- min(n, lim)
  n
}

# Seed the RNG for the caller only: the user's .Random.seed is restored when
# the calling function exits. NULL leaves the RNG untouched.
.qsr_local_seed <- function(seed, envir = parent.frame()) {
  if (is.null(seed)) return(invisible(FALSE))
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  restore <- function() {
    if (had) {
      assign(".Random.seed", old, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }
  do.call(base::on.exit, list(as.call(list(restore)), TRUE, FALSE), envir = envir)
  set.seed(seed)
  invisible(TRUE)
}

# nocov end
