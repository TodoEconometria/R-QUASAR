
# QUASAR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/TodoEconometria/R-QUASAR)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Tests](https://img.shields.io/badge/tests-85%20passing-brightgreen)]()
<!-- badges: end -->

**Query-driven Unified Automated Stack for Analytical Runtime**

> Stop rewriting the same 200 lines every project.
> One framework for data -> insights -> output.

QUASAR is an R framework that eliminates boilerplate and standardizes the
architecture of data analysis projects. Designed for academic researchers
and data scientists who are tired of writing the same setup, formatting, and
export code for every analysis.

## What Makes QUASAR Different

| Task | Base R / Tidyverse | QUASAR |
|------|-------------------|--------|
| Project setup | Manual folder creation | `qsr_init("my_study")` |
| Load from database | 6-8 lines (DBI + driver) | `qsr_db("postgres", table = "data")` |
| Fit a model | `lm(y ~ x, data = df)` | `qsr_model(y ~ x)` |
| APA7 regression table | 15-25 lines (broom + gt) | `qsr_table()` |
| Coefficient plot | 10-15 lines (broom + ggplot2) | `qsr_plot(type = "coefficient")` |
| Make it interactive | +5 lines (plotly) | `interactive = TRUE` |
| 3D scatter plot | 10+ lines (plotly) | `qsr_plot(type = "scatter3d")` |
| Export report | 20+ lines (rmarkdown) | `qsr_report()` |

## Installation

```r
# install.packages("pak")
pak::pak("TodoEconometria/R-QUASAR/quasar")
```

## Quick Start

```r
library(quasar)

# Configure once
qsr_config()

# Register data
qsr_data(mtcars)

# Fit model (data comes from context — no need to pass it)
qsr_model(mpg ~ wt + hp)

# Output — zero arguments needed
qsr_table()                                    # APA7 regression table
qsr_table(type = "summary")                   # Descriptive statistics
qsr_plot(type = "coefficient")                 # Coefficient plot
qsr_plot(type = "scatter", x = wt, y = mpg,
         interactive = TRUE)                   # Interactive scatter
```

## Architecture

```
                    QUASAR PUBLIC API
        What you touch — clean, simple, zero-argument
+------------------+------------------+---------------------+
|   LAYER 1        |    LAYER 2       |     LAYER 3         |
|   Scaffold       |    Context       |     Connectors      |
|                  |                  |                     |
| qsr_init()       | qsr_config()     | qsr_db()            |
|                  | qsr_data()       | qsr_spark()         |
|                  | qsr_model()      | qsr_fetch()         |
|                  | qsr_get()        | qsr_python()        |
+------------------+------------------+---------------------+
|                       LAYER 4                             |
|                    Output Engine                          |
|      qsr_table() - qsr_plot() - qsr_report()             |
|   20 plot types | APA7/Chicago | Static + Interactive     |
+-------------------------------------------------------+
```

## Features

### Tables
- APA7 and Chicago formatting out of the box
- Auto-detects model vs. data frame input
- Export to HTML, LaTeX, Word, RTF

### Plots (20 types)
**Static (ggplot2):** scatter, bar, histogram, density, boxplot, coefficient, correlation

**Interactive (plotly):** scatter3d, surface3d, bubble, heatmap, violin, ridge, parallel, sankey, treemap, sunburst, funnel, waterfall, radar

**Any static plot becomes interactive** with `interactive = TRUE`.

**Three themes:** `quasar_academic`, `quasar_minimal`, `quasar_dark`

### Connectors
- **Databases:** SQLite, PostgreSQL, MySQL, ODBC
- **Academic APIs:** World Bank, FRED, US Census, any URL
- **Spark:** One-line connection via sparklyr
- **Python:** Zero-friction interop via reticulate

### Context Injection
Register data and models once. Every downstream function reads from context:

```r
qsr_data(survey_results)
qsr_model(satisfaction ~ age + income)

# These all work with zero arguments:
qsr_table()
qsr_plot(type = "coefficient")
qsr_plot(type = "correlation")
```

## Complete Pipeline Example

From database to publication in 10 lines:

```r
library(quasar)

qsr_config(output_format = "apa7")
qsr_db("sqlite", dbname = "study.sqlite", table = "responses")
qsr_model(satisfaction ~ age + income + education)
qsr_table(caption = "Table 1. OLS Estimates")
qsr_table(type = "summary", caption = "Table 2. Descriptive Statistics")
qsr_plot(type = "coefficient", title = "Figure 1. Effect Sizes")
qsr_plot(type = "correlation", interactive = TRUE, export = "html")
qsr_report(title = "Satisfaction Analysis", journal = "r_journal")
```

## Roadmap

- [x] **Phase 1** — Core R framework (4 layers, 13 exported functions)
- [ ] **Phase 2** — Rust backend via rextendr + Polars + DataFusion (10-30x speedup)
- [ ] **Phase 3** — CRAN submission + The R Journal paper

## License

GPL-3

## Author

Juan Marcelo Gutierrez Miranda — [TodoEconometria](https://github.com/TodoEconometria)
