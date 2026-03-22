# ============================================================
# Tests for Layer 4 — Output Engine
# Static (ggplot2) + Interactive (plotly)
# ============================================================

# ---- qsr_table() tests -----------------------------------------------------

test_that("qsr_table() creates a table from a linear model", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- qsr_table(model, caption = "Test model table")
  expect_s3_class(result, "gt_tbl")
})

test_that("qsr_table() auto-detects model type", {
  model <- lm(mpg ~ wt, data = mtcars)
  result <- qsr_table(model)
  expect_s3_class(result, "gt_tbl")
})

test_that("qsr_table() creates summary table from data frame", {
  result <- qsr_table(mtcars, type = "summary")
  expect_s3_class(result, "gt_tbl")
})

test_that("qsr_table() creates custom table from data frame", {
  result <- qsr_table(head(mtcars), type = "custom")
  expect_s3_class(result, "gt_tbl")
})

test_that("qsr_table() respects format from context", {
  qsr_config(output_format = "chicago")
  model <- lm(mpg ~ wt, data = mtcars)
  result <- qsr_table(model)
  expect_s3_class(result, "gt_tbl")
  qsr_reset()
})

test_that("qsr_table() exports to file", {
  model <- lm(mpg ~ wt, data = mtcars)
  tmp_dir <- tempfile("quasar_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- qsr_table(model, caption = "Export test",
                       export = "html", output_dir = tmp_dir)
  html_files <- list.files(tmp_dir, pattern = "\\.html$")
  expect_length(html_files, 1)
})

test_that("qsr_table() fails on unsupported input", {
  expect_error(qsr_table("not_a_model", type = NULL))
})

# ---- qsr_plot() static tests -----------------------------------------------

test_that("qsr_plot() creates a scatter plot", {
  p <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg)
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() creates a histogram", {
  p <- qsr_plot(mtcars, type = "histogram", x = mpg, bins = 15)
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() creates a boxplot", {
  p <- qsr_plot(mtcars, type = "boxplot", x = cyl, y = mpg)
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() creates a density plot", {
  p <- qsr_plot(mtcars, type = "density", x = mpg)
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() creates a coefficient plot from model", {
  model <- lm(mpg ~ wt + hp + disp, data = mtcars)
  p <- qsr_plot(model, type = "coefficient")
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() creates a correlation matrix", {
  p <- qsr_plot(mtcars[, c("mpg", "wt", "hp")], type = "correlation")
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() re-themes an existing ggplot", {
  existing_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  p <- qsr_plot(existing_plot, title = "Re-themed")
  expect_s3_class(p, "gg")
})

test_that("qsr_plot() applies different theme variants", {
  p1 <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                  theme = "quasar_academic")
  p2 <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                  theme = "quasar_minimal")
  p3 <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                  theme = "quasar_dark")
  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")
  expect_s3_class(p3, "gg")
})

test_that("qsr_plot() exports static to file", {
  tmp_dir <- tempfile("quasar_plot_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  p <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                 title = "Export test", export = "png",
                 output_dir = tmp_dir)
  png_files <- list.files(tmp_dir, pattern = "\\.png$")
  expect_length(png_files, 1)
})

# ---- qsr_plot() interactive tests -------------------------------------------

test_that("qsr_plot() converts to interactive with interactive = TRUE", {
  p <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                 interactive = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive works with histogram", {
  p <- qsr_plot(mtcars, type = "histogram", x = mpg,
                 interactive = TRUE, bins = 15)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive works with boxplot", {
  df <- mtcars
  df$cyl_f <- factor(df$cyl)
  p <- qsr_plot(df, type = "boxplot", x = cyl_f, y = mpg,
                 interactive = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive works with coefficient plot", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  p <- qsr_plot(model, type = "coefficient", interactive = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive works with correlation", {
  p <- qsr_plot(mtcars[, c("mpg", "wt", "hp")], type = "correlation",
                 interactive = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() re-themes existing ggplot to plotly", {
  existing <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  p <- qsr_plot(existing, interactive = TRUE, title = "Interactive re-theme")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive applies dark theme", {
  p <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                 interactive = TRUE, theme = "quasar_dark")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() interactive export to HTML", {
  tmp_dir <- tempfile("quasar_plotly_export_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  p <- qsr_plot(mtcars, type = "scatter", x = wt, y = mpg,
                 interactive = TRUE, title = "HTML export",
                 export = "html", output_dir = tmp_dir)
  html_files <- list.files(tmp_dir, pattern = "\\.html$")
  expect_length(html_files, 1)
})

# ---- qsr_plot() native plotly types ----------------------------------------

test_that("qsr_plot() creates 3D scatter", {
  p <- qsr_plot(mtcars, type = "scatter3d", x = wt, y = hp, z = mpg)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates 3D scatter with color", {
  p <- qsr_plot(mtcars, type = "scatter3d", x = wt, y = hp, z = mpg,
                 color = cyl)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates bubble chart", {
  p <- qsr_plot(mtcars, type = "bubble", x = wt, y = mpg,
                 size = hp, color = cyl)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates interactive heatmap", {
  p <- qsr_plot(mtcars[, c("mpg", "wt", "hp", "disp")], type = "heatmap")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates violin plot", {
  p <- qsr_plot(mtcars, type = "violin", y = mpg, color = cyl)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates parallel coordinates", {
  p <- qsr_plot(mtcars[, c("mpg", "wt", "hp", "disp", "cyl")],
                 type = "parallel", color = cyl)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates sankey diagram", {
  sankey_data <- data.frame(
    source = c("A", "A", "B", "B"),
    target = c("C", "D", "C", "D"),
    value  = c(10, 20, 15, 25)
  )
  p <- qsr_plot(sankey_data, type = "sankey")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates treemap", {
  tree_data <- data.frame(
    labels  = c("Total", "A", "B", "A1", "A2", "B1"),
    parents = c("",      "Total", "Total", "A", "A", "B"),
    values  = c(100, 60, 40, 35, 25, 40)
  )
  p <- qsr_plot(tree_data, type = "treemap")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates sunburst", {
  sun_data <- data.frame(
    labels  = c("Total", "A", "B", "A1", "A2", "B1"),
    parents = c("",      "Total", "Total", "A", "A", "B"),
    values  = c(100, 60, 40, 35, 25, 40)
  )
  p <- qsr_plot(sun_data, type = "sunburst")
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates funnel chart", {
  funnel_data <- data.frame(
    stage = c("Visitors", "Leads", "Qualified", "Converted"),
    count = c(1000, 500, 200, 50)
  )
  p <- qsr_plot(funnel_data, type = "funnel", y = stage, x = count)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() creates radar chart", {
  radar_data <- data.frame(
    team  = c("Team A", "Team B"),
    speed = c(8, 6),
    power = c(7, 9),
    agility = c(9, 5),
    defense = c(6, 8)
  )
  p <- qsr_plot(radar_data, type = "radar", color = team)
  expect_s3_class(p, "plotly")
})

test_that("qsr_plot() sankey fails without required columns", {
  expect_error(qsr_plot(mtcars, type = "sankey"))
})

test_that("qsr_plot() native plotly with dark theme", {
  p <- qsr_plot(mtcars, type = "scatter3d", x = wt, y = hp, z = mpg,
                 theme = "quasar_dark", title = "Dark 3D")
  expect_s3_class(p, "plotly")
})

# ---- Context injection tests ------------------------------------------------

test_that("qsr_data() registers dataset in context", {
  qsr_reset()
  qsr_data(mtcars)
  expect_equal(nrow(.qsr_context$get_data()), 32)
  qsr_reset()
})

test_that("qsr_model() fits model from context data", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt + hp)
  model <- .qsr_context$get_model("last")
  expect_s3_class(model, "lm")
  qsr_reset()
})

test_that("qsr_model() supports named models", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt, name = "simple")
  qsr_model(mpg ~ wt + hp + disp, name = "full")
  expect_s3_class(.qsr_context$get_model("simple"), "lm")
  expect_s3_class(.qsr_context$get_model("full"), "lm")
  qsr_reset()
})

test_that("qsr_model() supports logit", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(am ~ wt + hp, type = "logit")
  model <- .qsr_context$get_model("last")
  expect_s3_class(model, "glm")
  qsr_reset()
})

test_that("qsr_model() fails without data", {
  qsr_reset()
  expect_error(qsr_model(mpg ~ wt))
})

test_that("qsr_table() zero-argument uses last model from context", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt + hp)
  result <- qsr_table()
  expect_s3_class(result, "gt_tbl")
  qsr_reset()
})

test_that("qsr_table(type = 'summary') uses active dataset", {
  qsr_reset()
  qsr_data(mtcars)
  result <- qsr_table(type = "summary")
  expect_s3_class(result, "gt_tbl")
  qsr_reset()
})

test_that("qsr_table() fails with no context", {
  qsr_reset()
  expect_error(qsr_table())
})

test_that("qsr_plot() zero-argument uses active dataset", {
  qsr_reset()
  qsr_data(mtcars)
  p <- qsr_plot(type = "histogram", x = mpg)
  expect_s3_class(p, "gg")
  qsr_reset()
})

test_that("qsr_plot(type = 'coefficient') uses last model", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt + hp)
  p <- qsr_plot(type = "coefficient")
  expect_s3_class(p, "gg")
  qsr_reset()
})

test_that("qsr_plot(type = 'correlation') uses active dataset", {
  qsr_reset()
  qsr_data(mtcars)
  p <- qsr_plot(type = "correlation")
  expect_s3_class(p, "gg")
  qsr_reset()
})

test_that("qsr_plot() fails with no context", {
  qsr_reset()
  expect_error(qsr_plot())
})

test_that("Full pipeline: qsr_data -> qsr_model -> qsr_table -> qsr_plot", {
  qsr_reset()
  qsr_data(mtcars)
  qsr_model(mpg ~ wt + hp)

  tbl <- qsr_table()
  expect_s3_class(tbl, "gt_tbl")

  p1 <- qsr_plot(type = "coefficient")
  expect_s3_class(p1, "gg")

  p2 <- qsr_plot(type = "correlation", interactive = TRUE)
  expect_s3_class(p2, "plotly")

  p3 <- qsr_plot(type = "scatter", x = wt, y = mpg, interactive = TRUE)
  expect_s3_class(p3, "plotly")

  qsr_reset()
})

# ---- Theme and palette tests -----------------------------------------------

test_that(".qsr_palette() returns correct number of colors", {
  expect_length(.qsr_palette(3), 3)
  expect_length(.qsr_palette(6), 6)
  expect_length(.qsr_palette(10), 8)  # max 8 colors
})

test_that(".qsr_theme() returns a ggplot theme", {
  theme <- .qsr_theme("quasar_academic")
  expect_s3_class(theme, "theme")
})

test_that(".qsr_plotly_theme() returns correct structure", {
  th <- .qsr_plotly_theme("quasar_academic")
  expect_true("font" %in% names(th))
  expect_true("paper_bgcolor" %in% names(th))

  th_dark <- .qsr_plotly_theme("quasar_dark")
  expect_equal(th_dark$paper_bgcolor, "#1a1a2e")
})
