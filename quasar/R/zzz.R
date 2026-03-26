# nocov start

#' @importFrom R6 R6Class
#' @importFrom utils head
#' @importFrom rlang .data %||%
#' @importFrom stats sd
NULL

# Suppress R CMD check notes for data.table operators
utils::globalVariables(c(":=", ".SD", ".N", ".GRP"))

.onLoad <- function(libname, pkgname) {
  # Ensure context singleton exists in package namespace
  assign(".qsr_context", QuasarContext$new(), envir = parent.env(environment()))
  invisible()
}

# nocov end
