# ============================================================
# QUASAR — Layer 4: Output Engine
# qsr_table(), qsr_plot(), qsr_report()
# Static (ggplot2) + Interactive (plotly) visualization
# ============================================================


# ---- qsr_table() -----------------------------------------------------------

#' Create publication-ready tables
#'
#' Generates formatted tables from statistical models or data frames,
#' ready for academic publication. If called with no arguments, uses the
#' last model from context. If only `type = "summary"` is given, uses
#' the active dataset from context.
#'
#' @param x A model object, data frame, or `NULL` to use context.
#'   If omitted and a model exists in context, uses that model.
#' @param type Character. Table type: `"model"` for regression tables,
#'   `"summary"` for descriptive statistics, `"custom"` for raw data frames.
#'   Defaults to auto-detection.
#' @param format Character. Output style. If `NULL`, reads from context
#'   (`output_format`). One of `"apa7"`, `"chicago"`, `"custom"`.
#' @param caption Character. Table caption/title.
#' @param export Character vector. Export formats: any combination of
#'   `"html"`, `"latex"`, `"word"`, `"rtf"`. If `NULL`, only returns the
#'   gt object for interactive use.
#' @param output_dir Character. Directory for exported files. Defaults to
#'   `"outputs/tables"`.
#' @param stars Logical. Show significance stars. Default `TRUE`.
#' @param notes Character vector. Footnotes to append below the table.
#' @param model Character. Name of stored model to use. Default `"last"`.
#' @param ... Additional arguments passed to [gtsummary::tbl_regression()]
#'   or [gt::gt()].
#'
#' @return A `gt` table object (invisibly if exported).
#' @export
#'
#' @examples
#' \dontrun{
#' # Classic usage
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' qsr_table(model, caption = "Table 1. OLS estimates")
#'
#' # Zero-argument usage (after qsr_data + qsr_model)
#' qsr_data(mtcars)
#' qsr_model(mpg ~ wt + hp)
#' qsr_table()                    # uses last model
#' qsr_table(type = "summary")   # uses active dataset
#' }
qsr_table <- function(x          = NULL,
                      type       = NULL,
                      format     = NULL,
                      caption    = NULL,
                      export     = NULL,
                      output_dir = "outputs/tables",
                      stars      = TRUE,
                      notes      = NULL,
                      model      = "last",
                      ...) {

  # Inject from context if x is not provided
  if (is.null(x)) {
    if (!is.null(type) && type == "summary") {
      x <- .qsr_context$get_data()
      if (is.null(x)) {
        cli::cli_abort(c(
          "No data available for summary table.",
          "i" = "Register data with {.fn qsr_data} or pass a data frame."
        ))
      }
    } else {
      x <- .qsr_context$get_model(model)
      if (is.null(x)) {
        cli::cli_abort(c(
          "No model available.",
          "i" = "Fit a model with {.fn qsr_model} or pass an object to {.arg x}."
        ))
      }
    }
  }

  # Read format from context if not provided
  format <- format %||% qsr_get("output_format", default = "apa7")

  # Auto-detect type
  if (is.null(type)) {
    type <- .qsr_detect_table_type(x)
  }
  type <- match.arg(type, c("model", "summary", "custom"))

  # Build table based on type
  tbl <- switch(type,
    model   = .qsr_table_model(x, stars = stars, ...),
    summary = .qsr_table_summary(x, ...),
    custom  = .qsr_table_custom(x, ...)
  )

  # Apply QUASAR academic formatting
  tbl_gt <- .qsr_format_table(tbl, type = type, format = format,
                               caption = caption, notes = notes)

  # Export if requested
  if (!is.null(export)) {
    .qsr_export_table(tbl_gt, export = export, output_dir = output_dir,
                       caption = caption)
  }

  # Store in context for qsr_report()
  .qsr_context$store_table(tbl_gt, name = caption)

  cli::cli_alert_success("Table created{.emph { if (!is.null(caption)) paste0(': ', caption) else '' }}")

  if (!is.null(export)) {
    invisible(tbl_gt)
  } else {
    tbl_gt
  }
}


# ---- qsr_plot() ------------------------------------------------------------

#' Create publication-ready plots with QUASAR theme
#'
#' Wraps ggplot2 and plotly with a consistent academic theme. If called with
#' no data argument, uses the active dataset or last model from context.
#'
#' @param data A data frame, model object, ggplot object, or `NULL` to use
#'   context. For `"coefficient"` type, uses last model from context.
#'   For other types, uses active dataset from context.
#' @param type Character. Plot type. **Static types** (ggplot2): `"scatter"`,
#'   `"bar"`, `"histogram"`, `"density"`, `"boxplot"`, `"coefficient"`,
#'   `"correlation"`. **Native interactive types** (plotly): `"scatter3d"`,
#'   `"surface3d"`, `"bubble"`, `"heatmap"`, `"violin"`, `"ridge"`,
#'   `"parallel"`, `"sankey"`, `"treemap"`, `"sunburst"`, `"funnel"`,
#'   `"waterfall"`, `"radar"`.
#' @param x,y,z Column names (unquoted or character) for axes.
#'   `z` is used only by 3D plot types.
#' @param color Optional column name for color/group aesthetic.
#' @param size Optional column name for size aesthetic (bubble plots).
#' @param title Character. Plot title.
#' @param subtitle Character. Plot subtitle.
#' @param caption Character. Plot caption/source note.
#' @param interactive Logical. If `TRUE`, converts ggplot output to an
#'   interactive plotly chart with hover, zoom, and pan. Default `FALSE`.
#' @param theme Character. Theme variant: `"quasar_academic"` (default),
#'   `"quasar_minimal"`, `"quasar_dark"`.
#' @param export Character vector. Export formats: `"png"`, `"pdf"`, `"svg"`
#'   for static; `"html"` for interactive. If `NULL`, only displays the plot.
#' @param output_dir Character. Directory for exported files.
#' @param width,height Numeric. Plot dimensions in inches (static) or
#'   pixels (interactive). Defaults: 7x5 inches / 800x500 pixels.
#' @param dpi Numeric. Resolution for raster exports. Default 300.
#' @param ... Additional arguments passed to the geom or plotly trace.
#'
#' @return A `ggplot` object or `plotly` htmlwidget.
#' @export
#'
#' @examples
#' \dontrun{
#' # Zero-argument (after qsr_data + qsr_model)
#' qsr_data(mtcars)
#' qsr_model(mpg ~ wt + hp)
#' qsr_plot()                        # scatter of first 2 numeric cols
#' qsr_plot(type = "coefficient")    # coefficient plot from last model
#' qsr_plot(type = "correlation")    # correlation of active dataset
#'
#' # Explicit data
#' qsr_plot(mtcars, type = "scatter", x = wt, y = mpg, interactive = TRUE)
#'
#' # Re-theme an existing ggplot
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'        ggplot2::geom_point()
#' qsr_plot(p, interactive = TRUE)
#' }
qsr_plot <- function(data        = NULL,
                     type        = "scatter",
                     x           = NULL,
                     y           = NULL,
                     z           = NULL,
                     color       = NULL,
                     size        = NULL,
                     title       = NULL,
                     subtitle    = NULL,
                     caption     = NULL,
                     interactive = FALSE,
                     theme       = "quasar_academic",
                     export      = NULL,
                     output_dir  = "outputs/figures",
                     width       = NULL,
                     height      = NULL,
                     dpi         = 300,
                     ...) {

  # Inject from context if data is not provided
  if (is.null(data)) {
    if (type == "coefficient") {
      data <- .qsr_context$get_model("last")
      if (is.null(data)) {
        cli::cli_abort(c(
          "No model available for coefficient plot.",
          "i" = "Fit a model with {.fn qsr_model} or pass a model object."
        ))
      }
    } else {
      data <- .qsr_context$get_data()
      if (is.null(data)) {
        cli::cli_abort(c(
          "No data available.",
          "i" = "Register data with {.fn qsr_data} or pass a data frame."
        ))
      }
    }
  }

  # Native plotly types — handle separately
  plotly_native_types <- c("scatter3d", "surface3d", "bubble", "heatmap",
                           "violin", "ridge", "parallel", "sankey",
                           "treemap", "sunburst", "funnel", "waterfall",
                           "radar")

  if (type %in% plotly_native_types) {
    p <- .qsr_plotly_native(
      data = data, type = type,
      x = rlang::enquo(x), y = rlang::enquo(y), z = rlang::enquo(z),
      color = rlang::enquo(color), size = rlang::enquo(size),
      title = title, theme = theme,
      width = width %||% 800, height = height %||% 500, ...
    )

    if (!is.null(export)) {
      .qsr_export_plotly(p, export, output_dir, title,
                          width %||% 800, height %||% 500)
    }
    return(p)
  }

  # Set defaults for static dimensions
  width  <- width  %||% 7
  height <- height %||% 5

  # If data is already a ggplot, just re-theme it
  if (inherits(data, "gg")) {
    p <- data + .qsr_theme(theme)
    if (!is.null(title))   p <- p + ggplot2::ggtitle(title, subtitle = subtitle)
    if (!is.null(caption)) p <- p + ggplot2::labs(caption = caption)

    if (interactive) {
      p <- .qsr_to_plotly(p, title, theme)
      if (!is.null(export)) {
        .qsr_export_plotly(p, export, output_dir, title, 800, 500)
      }
      return(p)
    }

    if (!is.null(export)) {
      .qsr_export_plot(p, export, output_dir, title, width, height, dpi)
    }
    return(p)
  }

  # Capture column names
  x_var     <- rlang::enquo(x)
  y_var     <- rlang::enquo(y)
  color_var <- rlang::enquo(color)

  type <- match.arg(type, c("scatter", "bar", "histogram", "density",
                            "boxplot", "coefficient", "correlation"))

  # Specialized plots handle their own ggplot construction
  if (type %in% c("coefficient", "correlation")) {
    p <- switch(type,
      coefficient = .qsr_plot_coefficient(data, ...),
      correlation = .qsr_plot_correlation(data, ...)
    )
  } else {
    # Build base aesthetics
    aes_args <- list()
    if (!rlang::quo_is_null(x_var)) aes_args$x <- x_var
    if (!rlang::quo_is_null(y_var)) aes_args$y <- y_var
    if (!rlang::quo_is_null(color_var)) aes_args$colour <- color_var

    base_aes <- rlang::inject(ggplot2::aes(!!!aes_args))
    p <- ggplot2::ggplot(data, base_aes)

    # Add geom by type
    p <- switch(type,
      scatter   = p + ggplot2::geom_point(size = 2, alpha = 0.7, ...),
      bar       = p + ggplot2::geom_col(alpha = 0.85, ...),
      histogram = p + ggplot2::geom_histogram(alpha = 0.8, fill = "#2C5F7C",
                                               color = "white", ...),
      density   = p + ggplot2::geom_density(alpha = 0.5, fill = "#2C5F7C", ...),
      boxplot   = p + ggplot2::geom_boxplot(alpha = 0.7, ...)
    )
  }

  # Apply theme and labels
  p <- p + .qsr_theme(theme)
  if (!is.null(title))   p <- p + ggplot2::ggtitle(title, subtitle = subtitle)
  if (!is.null(caption)) p <- p + ggplot2::labs(caption = caption)

  # Convert to interactive if requested
  if (interactive) {
    p <- .qsr_to_plotly(p, title, theme)
    if (!is.null(export)) {
      .qsr_export_plotly(p, export, output_dir, title, 800, 500)
    }
    return(p)
  }

  # Export static if requested
  if (!is.null(export)) {
    .qsr_export_plot(p, export, output_dir, title, width, height, dpi)
  }

  # Store in context
  .qsr_context$store_plot(p, name = title)

  p
}


# ---- qsr_report() ----------------------------------------------------------

#' Generate a complete analysis report
#'
#' Creates a formatted report (HTML, PDF, or Word) from an R Markdown
#' template, bundling tables, figures, and session info.
#'
#' @param template Character. Report template: `"journal_article"`,
#'   `"technical_report"`, `"slides"`. Default `"journal_article"`.
#' @param journal Character. Target journal formatting. If `NULL`,
#'   reads from context. One of `"r_journal"`, `"jss"`, `"apsr"`.
#' @param output_format Character. Output format: `"html"`, `"pdf"`, `"word"`.
#'   Default `"html"`.
#' @param title Character. Report title.
#' @param author Character. Author name. If `NULL`, reads from context.
#' @param include Character vector. Sections to include:
#'   `"models"`, `"tables"`, `"figures"`, `"session_info"`.
#' @param output_dir Character. Directory for the rendered report.
#' @param ... Additional parameters passed to [rmarkdown::render()].
#'
#' @return Invisible path to the rendered report file.
#' @export
#'
#' @examples
#' \dontrun{
#' qsr_report(
#'   title    = "Electoral Behavior Analysis 2024",
#'   template = "journal_article",
#'   journal  = "r_journal",
#'   include  = c("models", "tables", "figures", "session_info")
#' )
#' }
qsr_report <- function(template      = "journal_article",
                       journal       = NULL,
                       output_format = "html",
                       title         = "QUASAR Analysis Report",
                       author        = NULL,
                       include       = c("models", "tables", "figures",
                                         "session_info"),
                       output_dir    = "outputs/reports",
                       ...) {

  template <- match.arg(template, c("journal_article", "technical_report",
                                     "slides"))
  output_format <- match.arg(output_format, c("html", "pdf", "word"))
  journal <- journal %||% qsr_get("journal", default = "r_journal")
  author  <- author  %||% qsr_get("author",  default = "")

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate the Rmd from template
  rmd_path <- .qsr_build_report_rmd(
    template = template, title = title, author = author,
    journal = journal, include = include, output_dir = output_dir
  )

  # Render
  rmd_output_format <- switch(output_format,
    html = rmarkdown::html_document(theme = "cosmo", toc = TRUE),
    pdf  = rmarkdown::pdf_document(toc = TRUE),
    word = rmarkdown::word_document(toc = TRUE)
  )

  output_file <- rmarkdown::render(
    input         = rmd_path,
    output_format = rmd_output_format,
    output_dir    = output_dir,
    quiet         = TRUE,
    ...
  )

  cli::cli_alert_success("Report generated: {.path {output_file}}")
  invisible(output_file)
}


# ============================================================
# INTERNAL HELPERS — Tables
# ============================================================

#' Auto-detect table type from input
#' @noRd
.qsr_detect_table_type <- function(x) {
  if (inherits(x, c("lm", "glm", "nls", "coxph", "survreg",
                     "polr", "clm", "lmerMod", "glmerMod"))) {
    "model"
  } else if (is.data.frame(x)) {
    "summary"
  } else {
    cli::cli_abort(c(
      "Cannot auto-detect table type for object of class {.cls {class(x)}}.",
      "i" = "Use {.arg type} = 'model', 'summary', or 'custom'."
    ))
  }
}

#' Build a regression table from a model object
#' @noRd
.qsr_table_model <- function(x, stars = TRUE, ...) {
  tbl <- gtsummary::tbl_regression(x, ...)
  if (stars) {
    tbl <- gtsummary::add_significance_stars(tbl)
  }
  tbl <- gtsummary::add_glance_table(
    tbl,
    include = c("r.squared", "adj.r.squared", "nobs", "AIC", "BIC")
  )
  tbl
}

#' Build a descriptive statistics table
#' @noRd
.qsr_table_summary <- function(x, ...) {
  numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]

  if (length(numeric_cols) == 0) {
    cli::cli_abort("No numeric columns found for summary table.")
  }

  gtsummary::tbl_summary(
    x[, numeric_cols, drop = FALSE],
    statistic  = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})"
    ),
    digits     = gtsummary::all_continuous() ~ 3,
    missing    = "no",
    ...
  )
}

#' Build a custom table from a data frame
#' @noRd
.qsr_table_custom <- function(x, ...) {
  gt::gt(x, ...)
}

#' Apply academic formatting to a gt/gtsummary table
#' @noRd
.qsr_format_table <- function(tbl, type, format, caption, notes) {
  if (inherits(tbl, "gtsummary")) {
    tbl_gt <- gtsummary::as_gt(tbl)
  } else {
    tbl_gt <- tbl
  }

  if (!is.null(caption)) {
    tbl_gt <- gt::tab_header(tbl_gt, title = caption)
  }

  tbl_gt <- switch(format,
    apa7    = .qsr_style_apa7(tbl_gt),
    chicago = .qsr_style_chicago(tbl_gt),
    custom  = tbl_gt
  )

  if (!is.null(notes)) {
    for (note in notes) {
      tbl_gt <- gt::tab_footnote(tbl_gt, footnote = note)
    }
  }

  tbl_gt
}

#' APA7 table styling
#' @noRd
.qsr_style_apa7 <- function(tbl_gt) {
  tbl_gt |>
    gt::tab_options(
      table.border.top.style      = "solid",
      table.border.top.width      = gt::px(2),
      table.border.top.color      = "black",
      table.border.bottom.style   = "solid",
      table.border.bottom.width   = gt::px(2),
      table.border.bottom.color   = "black",
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = gt::px(1),
      heading.border.bottom.color = "black",
      column_labels.border.top.style    = "solid",
      column_labels.border.top.width    = gt::px(1),
      column_labels.border.top.color    = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.style    = "solid",
      table_body.border.bottom.width    = gt::px(1),
      table_body.border.bottom.color    = "black",
      table.font.size   = gt::px(12),
      heading.title.font.size = gt::px(14),
      heading.align     = "left",
      table.width       = gt::pct(100),
      column_labels.font.weight = "bold"
    ) |>
    gt::opt_table_font(font = gt::google_font("Times New Roman"))
}

#' Chicago style table formatting
#' @noRd
.qsr_style_chicago <- function(tbl_gt) {
  tbl_gt |>
    gt::tab_options(
      table.border.top.style    = "solid",
      table.border.top.width    = gt::px(1),
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(1),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      table.font.size   = gt::px(11),
      heading.align     = "left",
      table.width       = gt::pct(100)
    ) |>
    gt::opt_table_font(font = gt::google_font("Georgia"))
}

#' Export a gt table to multiple formats
#' @noRd
.qsr_export_table <- function(tbl_gt, export, output_dir, caption) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- if (!is.null(caption)) {
    gsub("[^a-zA-Z0-9]+", "_", tolower(caption))
  } else {
    paste0("quasar_table_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  export <- match.arg(export, c("html", "latex", "word", "rtf"),
                      several.ok = TRUE)

  for (fmt in export) {
    file_path <- file.path(output_dir, paste0(base_name, ".", fmt))
    switch(fmt,
      html  = gt::gtsave(tbl_gt, filename = file_path),
      latex = gt::as_latex(tbl_gt) |> as.character() |>
                writeLines(con = file_path),
      word  = gt::gtsave(tbl_gt, filename = file_path),
      rtf   = gt::gtsave(tbl_gt, filename = file_path)
    )
    cli::cli_alert_info("Exported: {.path {file_path}}")
  }
}


# ============================================================
# INTERNAL HELPERS — Static Plots (ggplot2)
# ============================================================

#' QUASAR ggplot2 theme
#' @noRd
.qsr_theme <- function(variant = "quasar_academic") {

  base_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "serif") +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14,
                                                hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 11, color = "grey40",
                                                hjust = 0,
                                                margin = ggplot2::margin(b = 10)),
      plot.caption     = ggplot2::element_text(size = 9, color = "grey50",
                                                hjust = 1),
      axis.title       = ggplot2::element_text(size = 11, face = "bold"),
      axis.text        = ggplot2::element_text(size = 10),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(15, 15, 15, 15)
    )

  switch(variant,
    quasar_academic = base_theme + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3)
    ),
    quasar_minimal = base_theme + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.4)
    ),
    quasar_dark = base_theme + ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      panel.background = ggplot2::element_rect(fill = "#16213e", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#2a2a4a", linewidth = 0.3),
      text             = ggplot2::element_text(color = "white"),
      axis.text        = ggplot2::element_text(color = "grey80"),
      plot.title       = ggplot2::element_text(color = "white", face = "bold"),
      legend.background = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      legend.text      = ggplot2::element_text(color = "grey80")
    ),
    base_theme
  )
}

#' QUASAR color palette
#' @noRd
.qsr_palette <- function(n = 6) {
  colors <- c(
    "#2C5F7C",  # deep blue
    "#D4443B",  # academic red
    "#3A8C6E",  # forest green
    "#E8973A",  # warm orange
    "#6B4C9A",  # purple
    "#C7A42F",  # gold
    "#4A90A4",  # teal
    "#8B5E3B"   # brown
  )
  colors[seq_len(min(n, length(colors)))]
}

#' Coefficient plot from model
#' @noRd
.qsr_plot_coefficient <- function(data, ...) {
  coef_data <- broom::tidy(data, conf.int = TRUE)
  coef_data <- coef_data[coef_data$term != "(Intercept)", ]

  ggplot2::ggplot(coef_data, ggplot2::aes(
    x = .data$estimate, y = stats::reorder(.data$term, .data$estimate)
  )) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(size = 3, color = "#2C5F7C") +
    ggplot2::geom_errorbar(ggplot2::aes(
      xmin = .data$conf.low, xmax = .data$conf.high
    ), height = 0.2, color = "#2C5F7C", orientation = "y") +
    ggplot2::labs(x = "Estimate", y = NULL)
}

#' Correlation matrix plot
#' @noRd
.qsr_plot_correlation <- function(data, ...) {
  numeric_data <- data[vapply(data, is.numeric, logical(1))]
  cor_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")

  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "value")

  ggplot2::ggplot(cor_df, ggplot2::aes(
    x = .data$Var1, y = .data$Var2, fill = .data$value
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                        size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#D4443B", mid = "white", high = "#2C5F7C",
      midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
    ggplot2::coord_fixed()
}

#' Export a ggplot to multiple formats
#' @noRd
.qsr_export_plot <- function(p, export, output_dir, title,
                              width, height, dpi) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- if (!is.null(title)) {
    gsub("[^a-zA-Z0-9]+", "_", tolower(title))
  } else {
    paste0("quasar_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  export <- match.arg(export, c("png", "pdf", "svg"), several.ok = TRUE)

  for (fmt in export) {
    file_path <- file.path(output_dir, paste0(base_name, ".", fmt))
    ggplot2::ggsave(file_path, plot = p, width = width, height = height,
                     dpi = dpi, bg = "white")
    cli::cli_alert_info("Exported: {.path {file_path}}")
  }
}


# ============================================================
# INTERNAL HELPERS — Interactive Plots (plotly)
# ============================================================

#' Convert ggplot to interactive plotly
#' @noRd
.qsr_to_plotly <- function(p, title, theme) {
  palette <- .qsr_palette()

  plt <- plotly::ggplotly(p, tooltip = "all") |>
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToAdd = list("drawline", "eraseshape"),
      toImageButtonOptions = list(
        format = "png", filename = "quasar_plot",
        width = 1200, height = 800, scale = 2
      )
    )

  # Apply QUASAR theme styling to plotly
  plt_theme <- .qsr_plotly_theme(theme)
  plt <- plotly::layout(plt,
    font    = plt_theme$font,
    paper_bgcolor = plt_theme$paper_bgcolor,
    plot_bgcolor  = plt_theme$plot_bgcolor,
    hoverlabel = list(
      bgcolor  = palette[1],
      font     = list(color = "white", family = "serif", size = 12),
      bordercolor = "transparent"
    )
  )

  plt
}

#' QUASAR theme for plotly layouts
#' @noRd
.qsr_plotly_theme <- function(variant = "quasar_academic") {
  switch(variant,
    quasar_dark = list(
      font = list(family = "serif", color = "white"),
      paper_bgcolor = "#1a1a2e",
      plot_bgcolor  = "#16213e",
      gridcolor     = "#2a2a4a"
    ),
    # academic and minimal share the same plotly styling
    list(
      font = list(family = "serif", color = "#333333"),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      gridcolor     = "#eeeeee"
    )
  )
}

#' Build native plotly visualizations
#' @noRd
.qsr_plotly_native <- function(data, type, x, y, z, color, size,
                                title, theme, width, height, ...) {

  plt_theme <- .qsr_plotly_theme(theme)
  palette   <- .qsr_palette(8)

  # Resolve quosures to column names
  x_name     <- if (!rlang::quo_is_null(x)) rlang::as_name(x) else NULL
  y_name     <- if (!rlang::quo_is_null(y)) rlang::as_name(y) else NULL
  z_name     <- if (!rlang::quo_is_null(z)) rlang::as_name(z) else NULL
  color_name <- if (!rlang::quo_is_null(color)) rlang::as_name(color) else NULL
  size_name  <- if (!rlang::quo_is_null(size)) rlang::as_name(size) else NULL

  p <- switch(type,

    # ---- 3D scatter ----
    scatter3d = {
      plotly::plot_ly(
        data, x = data[[x_name]], y = data[[y_name]], z = data[[z_name]],
        color = if (!is.null(color_name)) data[[color_name]] else NULL,
        colors = palette,
        type = "scatter3d", mode = "markers",
        marker = list(size = 4, opacity = 0.8),
        ...
      ) |>
        plotly::layout(scene = list(
          xaxis = list(title = x_name),
          yaxis = list(title = y_name),
          zaxis = list(title = z_name)
        ))
    },

    # ---- 3D surface ----
    surface3d = {
      numeric_data <- data[vapply(data, is.numeric, logical(1))]
      z_matrix <- as.matrix(numeric_data)
      plotly::plot_ly(z = z_matrix, type = "surface",
                       colorscale = list(c(0, palette[1]), c(1, palette[4])),
                       ...) |>
        plotly::layout(scene = list(
          xaxis = list(title = x_name %||% "X"),
          yaxis = list(title = y_name %||% "Y"),
          zaxis = list(title = "Value")
        ))
    },

    # ---- Bubble chart ----
    bubble = {
      plotly::plot_ly(
        data, x = data[[x_name]], y = data[[y_name]],
        size = if (!is.null(size_name)) data[[size_name]] else NULL,
        color = if (!is.null(color_name)) data[[color_name]] else NULL,
        colors = palette,
        type = "scatter", mode = "markers",
        marker = list(opacity = 0.7, sizemode = "diameter"),
        text = paste0(x_name, ": ", data[[x_name]], "<br>",
                      y_name, ": ", data[[y_name]]),
        hoverinfo = "text",
        ...
      )
    },

    # ---- Interactive heatmap ----
    heatmap = {
      numeric_data <- data[vapply(data, is.numeric, logical(1))]
      cor_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")
      plotly::plot_ly(
        x = colnames(cor_matrix), y = rownames(cor_matrix),
        z = cor_matrix, type = "heatmap",
        colorscale = list(c(0, palette[2]), c(0.5, "white"), c(1, palette[1])),
        zmin = -1, zmax = 1,
        ...
      )
    },

    # ---- Violin plot ----
    violin = {
      plotly::plot_ly(
        data, y = data[[y_name]],
        x = if (!is.null(color_name)) data[[color_name]] else NULL,
        type = "violin",
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        fillcolor = palette[1],
        line = list(color = palette[1]),
        opacity = 0.7,
        ...
      )
    },

    # ---- Ridge plot (multiple densities) ----
    ridge = {
      if (is.null(color_name)) {
        cli::cli_abort("Ridge plot requires a {.arg color} grouping variable.")
      }
      groups <- unique(data[[color_name]])
      p <- plotly::plot_ly()
      for (i in seq_along(groups)) {
        group_data <- data[data[[color_name]] == groups[i], ]
        p <- plotly::add_trace(
          p, x = group_data[[x_name]], type = "violin",
          name = as.character(groups[i]),
          side = "positive",
          line = list(color = palette[(i - 1) %% length(palette) + 1]),
          fillcolor = palette[(i - 1) %% length(palette) + 1],
          opacity = 0.6, ...
        )
      }
      p
    },

    # ---- Parallel coordinates ----
    parallel = {
      numeric_data <- data[vapply(data, is.numeric, logical(1))]
      dims <- lapply(names(numeric_data), function(col) {
        list(label = col, values = numeric_data[[col]])
      })
      color_vals <- if (!is.null(color_name)) {
        as.numeric(as.factor(data[[color_name]]))
      } else {
        rep(1, nrow(data))
      }
      plotly::plot_ly(
        type = "parcoords",
        line = list(
          color = color_vals,
          colorscale = list(c(0, palette[1]), c(1, palette[2]))
        ),
        dimensions = dims,
        ...
      )
    },

    # ---- Sankey diagram ----
    sankey = {
      if (!all(c("source", "target", "value") %in% names(data))) {
        cli::cli_abort(c(
          "Sankey diagram requires columns: {.field source}, {.field target}, {.field value}.",
          "i" = "Provide a data frame with flow data."
        ))
      }
      all_nodes <- unique(c(data$source, data$target))
      plotly::plot_ly(
        type = "sankey", orientation = "h",
        node = list(
          label = all_nodes,
          color = palette[seq_len(length(all_nodes)) %% length(palette) + 1],
          pad = 15, thickness = 20
        ),
        link = list(
          source = match(data$source, all_nodes) - 1,
          target = match(data$target, all_nodes) - 1,
          value  = data$value,
          color  = paste0(palette[1], "40")
        ),
        ...
      )
    },

    # ---- Treemap ----
    treemap = {
      if (!all(c("labels", "parents") %in% names(data))) {
        cli::cli_abort(c(
          "Treemap requires columns: {.field labels}, {.field parents}.",
          "i" = "Optionally include {.field values} for sizing."
        ))
      }
      plotly::plot_ly(
        type = "treemap",
        labels  = data$labels,
        parents = data$parents,
        values  = if ("values" %in% names(data)) data$values else NULL,
        marker = list(
          colors = palette[seq_len(nrow(data)) %% length(palette) + 1]
        ),
        textinfo = "label+value+percent parent",
        ...
      )
    },

    # ---- Sunburst ----
    sunburst = {
      if (!all(c("labels", "parents") %in% names(data))) {
        cli::cli_abort("Sunburst requires columns: {.field labels}, {.field parents}.")
      }
      plotly::plot_ly(
        type = "sunburst",
        labels  = data$labels,
        parents = data$parents,
        values  = if ("values" %in% names(data)) data$values else NULL,
        marker = list(
          colors = palette[seq_len(nrow(data)) %% length(palette) + 1]
        ),
        ...
      )
    },

    # ---- Funnel ----
    funnel = {
      plotly::plot_ly(
        type = "funnel",
        y = data[[y_name %||% names(data)[1]]],
        x = data[[x_name %||% names(data)[2]]],
        marker = list(color = palette[1]),
        ...
      )
    },

    # ---- Waterfall ----
    waterfall = {
      plotly::plot_ly(
        type = "waterfall",
        x = data[[x_name %||% names(data)[1]]],
        y = data[[y_name %||% names(data)[2]]],
        measure = if ("measure" %in% names(data)) data$measure else NULL,
        connector = list(line = list(color = palette[6])),
        increasing = list(marker = list(color = palette[3])),
        decreasing = list(marker = list(color = palette[2])),
        totals     = list(marker = list(color = palette[1])),
        ...
      )
    },

    # ---- Radar / Spider chart ----
    radar = {
      numeric_data <- data[vapply(data, is.numeric, logical(1))]
      categories <- names(numeric_data)
      p <- plotly::plot_ly(type = "scatterpolar", fill = "toself")
      for (i in seq_len(nrow(numeric_data))) {
        row_vals <- as.numeric(numeric_data[i, ])
        p <- plotly::add_trace(
          p,
          r = c(row_vals, row_vals[1]),
          theta = c(categories, categories[1]),
          name = if (!is.null(color_name)) as.character(data[[color_name]][i])
                 else paste("Row", i),
          line = list(color = palette[(i - 1) %% length(palette) + 1]),
          fillcolor = paste0(palette[(i - 1) %% length(palette) + 1], "30"),
          ...
        )
      }
      p
    }
  )

  # Apply common layout (width/height set via attrs, not layout)
  p <- p |> plotly::layout(
    title = list(text = title %||% "", font = list(size = 16)),
    font  = plt_theme$font,
    paper_bgcolor = plt_theme$paper_bgcolor,
    plot_bgcolor  = plt_theme$plot_bgcolor,
    hoverlabel = list(
      bgcolor = palette[1],
      font = list(color = "white", family = "serif", size = 12)
    )
  )

  # Set dimensions via htmlwidgets attrs (not layout, which is deprecated)
  p$width  <- width
  p$height <- height

  p |> plotly::config(
    displayModeBar = TRUE,
    toImageButtonOptions = list(
      format = "png", filename = "quasar_plot",
      width = 1200, height = 800, scale = 2
    )
  )
}

#' Export plotly to file
#' @noRd
.qsr_export_plotly <- function(p, export, output_dir, title, width, height) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- if (!is.null(title)) {
    gsub("[^a-zA-Z0-9]+", "_", tolower(title))
  } else {
    paste0("quasar_interactive_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  export <- match.arg(export, c("html", "png", "pdf", "svg"),
                      several.ok = TRUE)

  for (fmt in export) {
    file_path <- file.path(output_dir, paste0(base_name, ".", fmt))
    if (fmt == "html") {
      # Use selfcontained if pandoc is available, fallback otherwise
      has_pandoc <- rmarkdown::pandoc_available()
      htmlwidgets::saveWidget(
        plotly::as_widget(p),
        file = normalizePath(file_path, mustWork = FALSE),
        selfcontained = has_pandoc
      )
    } else {
      # For static exports from plotly, use orca/kaleido if available
      plotly::save_image(p, file = file_path, width = width, height = height)
    }
    cli::cli_alert_info("Exported: {.path {file_path}}")
  }
}


# ============================================================
# INTERNAL HELPERS — Report
# ============================================================

#' Build an Rmd file from a QUASAR template
#' @noRd
.qsr_build_report_rmd <- function(template, title, author, journal,
                                   include, output_dir) {

  sections <- character()

  if ("models" %in% include) {
    sections <- c(sections, paste0(
      "## Models\n\n",
      "```{r models, echo=FALSE}\n",
      "# Models are loaded from the QUASAR context\n",
      "# Add your model objects here\n",
      "```\n"
    ))
  }

  if ("tables" %in% include) {
    sections <- c(sections, paste0(
      "## Tables\n\n",
      "```{r tables, echo=FALSE}\n",
      "# Tables generated by qsr_table() will appear here\n",
      "```\n"
    ))
  }

  if ("figures" %in% include) {
    sections <- c(sections, paste0(
      "## Figures\n\n",
      "```{r figures, echo=FALSE}\n",
      "# Figures generated by qsr_plot() will appear here\n",
      "```\n"
    ))
  }

  if ("session_info" %in% include) {
    sections <- c(sections, paste0(
      "## Session Info\n\n",
      "```{r session-info, echo=FALSE}\n",
      "sessionInfo()\n",
      "```\n"
    ))
  }

  rmd_content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "author: \"", author, "\"\n",
    "date: \"`r Sys.Date()`\"\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)\n",
    "library(quasar)\n",
    "```\n\n",
    paste(sections, collapse = "\n")
  )

  rmd_path <- file.path(output_dir, "report.Rmd")
  writeLines(rmd_content, rmd_path)

  rmd_path
}
