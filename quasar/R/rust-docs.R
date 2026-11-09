# ============================================================
# QUASAR - Return values and examples for the exported rust_* wrappers.
# R/extendr-wrappers.R is regenerated from src/rust/src/lib.rs, so the
# extra documentation lives here and is merged into the same Rd pages.
# ============================================================

#' @return A named list with one element per column (the data), plus
#'   `_elapsed_secs` (time spent in Rust) and `_nrows` (rows returned).
#'   [qsr_read()] turns it into a data.frame.
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(g = c("a", "b", "a"), x = c(1, 5, 3)), tmp,
#'                  row.names = FALSE)
#' str(rust_read_csv(tmp, NULL, ",", NULL))
#' unlink(tmp)
#' @rdname rust_read_csv
#' @name rust_read_csv
NULL

#' @return A named list with one element per column, plus `_elapsed_secs`
#'   and `_nrows`, as [rust_read_csv()].
#' @examples
#' csv <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(g = c("a", "b"), x = c(1, 5)), csv,
#'                  row.names = FALSE)
#' pq <- tempfile(fileext = ".parquet")
#' qsr_sink_parquet(csv, pq)
#' str(rust_read_parquet(pq))
#' unlink(c(csv, pq))
#' @rdname rust_read_parquet
#' @name rust_read_parquet
NULL

#' @return A named list with `column`, `count`, `mean`, `std`, `min` and `max`
#'   (one entry per numeric column).
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(x = c(1, 5, 3), y = c(2, 2, 8)), tmp,
#'                  row.names = FALSE)
#' str(rust_describe(tmp))
#' unlink(tmp)
#' @rdname rust_describe
#' @name rust_describe
NULL

#' @return A named list with the group column, the aggregated column
#'   (named `<agg_col>_<agg_fn>`), `_elapsed_secs` and `_nrows`.
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(g = c("a", "b", "a"), x = c(1, 5, 3)), tmp,
#'                  row.names = FALSE)
#' str(rust_group_agg(tmp, "g", "x", "mean"))
#' unlink(tmp)
#' @rdname rust_group_agg
#' @name rust_group_agg
NULL

#' @return A named list with the rows that pass the filter (one element per
#'   column), plus `_elapsed_secs` and `_nrows`.
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(g = c("a", "b", "a"), x = c(1, 5, 3)), tmp,
#'                  row.names = FALSE)
#' str(rust_filter(tmp, "x", "gt", 2))
#' unlink(tmp)
#' @rdname rust_filter
#' @name rust_filter
NULL

#' @return A named list with the sorted rows (one element per column), plus
#'   `_elapsed_secs` and `_nrows`.
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(g = c("a", "b", "a"), x = c(1, 5, 3)), tmp,
#'                  row.names = FALSE)
#' str(rust_sort(tmp, "x", TRUE))
#' unlink(tmp)
#' @rdname rust_sort
#' @name rust_sort
NULL
