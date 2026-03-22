# nocov start

.onLoad <- function(libname, pkgname) {
  # Ensure context singleton exists in package namespace
  assign(".qsr_context", QuasarContext$new(), envir = parent.env(environment()))
  invisible()
}

# nocov end
