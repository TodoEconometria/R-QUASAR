# ============================================================
# QUASAR — qsr_codebook(): dataset codebook from existing labels
# ============================================================

test_that("qsr_codebook reports type, missing rate and value labels", {
  skip_if_not_installed("haven")
  sexo <- haven::labelled(c(1, 2, 1, 2, NA), c(Hombre = 1, Mujer = 2))
  attr(sexo, "label") <- "Sexo del jefe"
  d <- data.frame(id = 1:5, sexo = sexo,
                  ingreso = c(1000, 2000, NA, 4000, 5000),
                  reg = c("N", "S", "N", "E", "S"), stringsAsFactors = FALSE)
  cb <- qsr_codebook(d, register = FALSE)

  expect_equal(nrow(cb), 4L)
  expect_true(all(c("variable", "label", "type", "n", "n_missing",
                    "pct_missing", "n_unique", "values") %in% names(cb)))
  s <- cb[cb$variable == "sexo", ]
  expect_equal(s$type, "labelled")
  expect_equal(s$label, "Sexo del jefe")
  expect_equal(s$pct_missing, 20)
  expect_match(s$values, "Hombre")
  expect_match(s$values, "Mujer")
  expect_equal(cb$type[cb$variable == "reg"], "character")
})

test_that("qsr_codebook truncates long value lists at max_values", {
  d <- data.frame(g = factor(letters[1:10]))
  cb <- qsr_codebook(d, max_values = 3, register = FALSE)
  expect_match(cb$values[1], "…")     # ellipsis appended
})

test_that("qsr_codebook honours `cols`", {
  d <- data.frame(a = 1:3, b = 4:6)
  expect_equal(qsr_codebook(d, cols = "a", register = FALSE)$variable, "a")
})
