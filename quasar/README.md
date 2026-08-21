
# rquasar <img src="man/figures/logo.png" align="right" height="120" alt="TodoEconometria" />

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Tests](https://img.shields.io/badge/tests-336%20passing-brightgreen)
![R](https://img.shields.io/badge/R-%3E%3D%204.2-blue)
<!-- badges: end -->

> **QUASAR** — Query-Driven Unified Automated Stack for Analytical Runtime
> By **Juan Marcelo Gutierrez Miranda** · [TodoEconometria](https://github.com/TodoEconometria)

**Query-driven Unified Automated Stack for Analytical Runtime**

> The R framework for applied economists who need to go from survey
> microdata to publication-ready output without boilerplate.

QUASAR eliminates the repetitive infrastructure code between raw data and
journal submission. Register your data once, and every downstream
function — models, tables, plots, reports — reads from context
automatically.

## Who Is This For

- Applied economists working with household survey microdata (ENAHO, EH,
  GEIH, ENIGH)
- Researchers who need APA7/Chicago tables, coefficient plots, and LaTeX
  export in minutes, not hours
- Teams processing large panel datasets (100K+ observations) who want
  Rust-level speed without leaving R

## Installation

```r
# install.packages("pak")
pak::pak("TodoEconometria/R-QUASAR/quasar")
```

**System requirements:** R >= 4.2, Rust toolchain (for Polars backend).

## Quick Start

```r
library(rquasar)

qsr_config()
qsr_data(mtcars)
qsr_model(mpg ~ wt + hp)

qsr_table()                        # APA7 regression table
qsr_plot(type = "coefficient")     # coefficient plot
```

Five calls. No `broom::tidy()`, no `gt::tab_options()`, no 15-line
ggplot2 theme blocks.

## What QUASAR Does That Nothing Else Does

| Problem | Existing solution | QUASAR |
|---------|------------------|--------|
| Register data once, use everywhere | None (pass `data=` every time) | `qsr_data()` context engine |
| Fuzzy match survey text fields | Manual scripting | `qsr_normalize_text()` |
| Harmonize geo codes across waves | Manual crosswalk tables | `qsr_crosswalk()` |
| Convert BOB/PEN/COP to constant USD | Write exchange rate logic | `qsr_currency()` (built-in rates 2006-2023) |
| Validate survey data quality | Ad hoc checks | `qsr_validate(checks = "survey")` |
| Log research decisions for replication | README notes | `qsr_decision_log()` structured markdown |
| Read 50M-row CSV fast | `data.table::fread()` | `qsr_read()` Polars/Rust backend |
| APA7 table from model | 15-25 lines (broom + gt) | `qsr_table()` |

## Core Modules

### Context Engine
```r
qsr_config(significance_level = 0.05, random_seed = 42)
qsr_data(survey_panel)
qsr_model(log_income ~ education + indigenous, type = "probit")

# Everything downstream reads from context:
qsr_table()
qsr_plot(type = "coefficient")
```

### Survey Microdata Tools
```r
qsr_normalize_text(column = "crop_name",
                   dictionary = c("coca", "cafe", "cacao"))
qsr_crosswalk(column = "ubigeo", crosswalk = "crosswalk_2017.csv",
              year_column = "year", apply_before = 2017)
qsr_currency(columns = "ingreso", from_currency = "PEN", base_year = 2015)
qsr_validate(checks = "panel")
```

### Machine Learning (12 methods)
```r
qsr_ml(mpg ~ ., method = "rf")           # Random Forest
qsr_ml(mpg ~ ., method = "xgboost")      # XGBoost
qsr_cv(mpg ~ ., method = "rf", k = 10)   # Cross-validation
qsr_compare()                              # Side-by-side comparison
```

### Time Series
```r
qsr_ts(AirPassengers, method = "auto", horizon = 12)
qsr_ts(AirPassengers, method = "hw", forecast = 24)
```

### Rust/Polars Backend
```r
qsr_read("data/large_file.csv")
qsr_fast_group("sales.csv", by = "region", col = "revenue", fn = "mean")
qsr_fast_filter("logs.csv", col = "status", op = "eq", value = 200)
```

The Polars engine performs aggregations, filtering, and sorting directly
on files without loading them fully into R. For pure CSV reading speed,
`data.table::fread()` remains faster. The QUASAR backend shines on
file-level operations and will improve with release builds.

### Output Engine (20 plot types)
```r
qsr_table(caption = "Table 1. OLS Estimates", export = "latex")
qsr_plot(type = "scatter", x = wt, y = mpg, interactive = TRUE)
qsr_plot(type = "scatter3d", x = wt, y = hp, z = mpg)
qsr_report(title = "Analysis", journal = "r_journal")
```

### Connectors
```r
qsr_db("postgres", table = "surveys")     # databases
qsr_fetch("worldbank", indicator = "NY.GDP.PCAP.CD")  # APIs
qsr_search("GDP")                          # search across sources
qsr_guide("ine_bolivia", year = 2023)      # download guidance
```

## Numbers

- **48 exported functions** across 15 modules
- **336 tests**, 0 failures
- **7,485 lines** of R + **290 lines** of Rust
- **3 vignettes** with worked examples
- Built-in exchange rates and CPI deflators for BOB, PEN, COP, BRL (2006-2023)

## Validated on Real Data

QUASAR was developed and validated on 1.14 million observations from
Bolivia (Encuesta de Hogares, 2006-2024) and Peru (ENAHO, 2004-2023)
household surveys. Models estimated include Probit, Ordered Probit,
Tobit, Heckman, Double Hurdle, and Multinomial Logit.

## License

GPL-3

## Author

Juan Marcelo Gutierrez Miranda — [TodoEconometria](https://github.com/TodoEconometria)
