# ============================================================
# QUASAR — qsr_read_layout(): fixed-width via an INE-style record layout
# ============================================================

test_that("qsr_read_layout applies names, types, implied decimals and labels", {
  layout <- data.frame(
    Variable    = c("id", "edad", "peso"),
    Longitud    = c(2, 3, 5),
    Tipo        = c("A", "N", "N"),
    Decimales   = c(NA, 0, 2),
    "Posición" = c(1, 3, 6),
    "Descripción" = c("Identificador", "Edad en años",
                           "Peso en kg\r\nTodas las personas"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # id(2) edad(3) peso(5, 2 implied decimals) -> width 10, no separators
  tf <- tempfile(fileext = ".txt")
  writeLines(c("A104507350", "B203612345"), tf)   # 45/36 ; 07350/12345 -> 73.50/123.45

  d <- qsr_read_layout(tf, layout, register = FALSE)

  expect_equal(names(d), c("id", "edad", "peso"))
  expect_type(d$id, "character")
  expect_equal(d$id, c("A1", "B2"), ignore_attr = TRUE)
  expect_type(d$edad, "double")
  expect_equal(d$edad, c(45, 36), ignore_attr = TRUE)
  expect_equal(d$peso, c(73.50, 123.45), ignore_attr = TRUE)  # implied 2 decimals applied
  expect_equal(attr(d$peso, "label"), "Peso en kg")  # first line of the description
})

test_that("qsr_read_layout falls back to cumulative widths without a position column", {
  layout <- data.frame(Variable = c("a", "b"), Longitud = c(1, 2),
                       Tipo = c("N", "N"), stringsAsFactors = FALSE)
  tf <- tempfile(fileext = ".txt")
  writeLines(c("112", "945"), tf)                 # a=1/9, b=12/45
  d <- qsr_read_layout(tf, layout, register = FALSE)
  expect_equal(d$a, c(1, 9))
  expect_equal(d$b, c(12, 45))
})

test_that("qsr_read_layout attaches value labels from the layout code tables", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if_not_installed("haven")
  diseno <- data.frame(
    Variable = c("id", "sexo"),
    Longitud = c(2, 1),
    Tipo = c("A", "N"),
    Decimales = c(NA, NA),
    "Posición" = c(1, 3),
    "Descripción" = c("Identificador", "Sexo"),
    "Diccionario de la variable" = c(NA, "TSEXO"),
    "Diccionario ubicado en la hoja…" = c(NA, "Tablas1"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # Tablas sheet: writexl adds a header row, so the block starts on row 2.
  tablas1 <- data.frame(
    A = c("TSEXO", "Código", "1", "6"),
    B = c(NA, "Descripción", "Hombre", "Mujer"),
    C = c("sexo", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  xf <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list("Diseño" = diseno, "Tablas1" = tablas1), xf)
  tf <- tempfile(fileext = ".txt")
  writeLines(c("A11", "B26"), tf)          # id=A1/B2 ; sexo=1/6

  d <- qsr_read_layout(tf, xf, sheet = "Diseño", register = FALSE)
  vl <- attr(d$sexo, "labels")
  expect_equal(unname(vl), c(1, 6))
  expect_equal(names(vl), c("Hombre", "Mujer"))
  expect_equal(as.character(haven::as_factor(d$sexo)), c("Hombre", "Mujer"))
})

test_that("qsr_read_layout drops non-variable rows (notes) in the layout", {
  layout <- data.frame(
    Variable = c("x", "Nota: esto no es una variable", "y"),
    Longitud = c(1, NA, 1), Tipo = c("N", NA, "N"),
    stringsAsFactors = FALSE
  )
  tf <- tempfile(fileext = ".txt")
  writeLines(c("12", "34"), tf)
  d <- qsr_read_layout(tf, layout, register = FALSE)
  expect_equal(names(d), c("x", "y"))
})
