
# rquasar <img src="man/figures/logo.png" align="right" height="120" alt="TodoEconometria" />

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Tests](https://img.shields.io/badge/tests-336%20passing-brightgreen)
![R](https://img.shields.io/badge/R-%3E%3D%204.2-blue)
<!-- badges: end -->

> **QUASAR** — Query-Driven Unified Automated Stack for Analytical Runtime
> By **Juan Marcelo Gutierrez Miranda** · [TodoEconometria](https://github.com/TodoEconometria)

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
- Teams processing large datasets (100K+ observations) who want
  out-of-core, low-memory file operations without leaving R

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
| Read **any** survey format (SPSS/Stata/SAS/Excel/dBase/JSON/Arrow/Parquet/fixed-width) | One package per format | `qsr_read()` / `qsr_read_fwf()`, auto-detected |
| Write it back to any of those formats | Another package per format | `qsr_write()` (the mirror of `qsr_read()`) |
| Design-based SE / CI / deff on a weighted tabulation | Build a `svydesign` + `svyby` by hand | `qsr_tabulate(se = TRUE)` + `qsr_svydesign()` |
| Work on a large CSV/Parquet without loading it into RAM | Manual chunking | `qsr_read()` / `qsr_fast_*()` Polars/Rust backend |
| APA7 table from model | 15-25 lines (broom + gt) | `qsr_table()` |

## Core Modules

### Context Engine
```r
qsr_config(significance_level = 0.05, random_seed = 42)
qsr_data(qsr_survey)              # synthetic survey panel bundled with the package
qsr_model(log_income ~ education + experience + indigenous)

# Everything downstream reads from context:
qsr_table()
qsr_plot(type = "coefficient")
```

### Survey Microdata Tools
```r
# Read the native format directly (SPSS/Stata/SAS/Excel/dBase/fixed-width/…):
qsr_read("EPA2016.sav", labels = "factor")        # value labels -> readable factors
qsr_read_fwf("md_EPA.txt", positions = ..., encoding = "cp850")  # INE record layouts

# Population estimates WITH design-based standard errors (Taylor linearization):
qsr_svydesign(strata = "CCAA", weights = "FACTOREL")
qsr_tabulate(by = "AOI", stat = "prop", se = TRUE)   # estimate, SE, 95% CI, deff
qsr_model(parado ~ mujer + extranjero, design = TRUE)  # svyglm, design-based SE

# Clean, harmonize, deflate, validate, and write back out:
qsr_normalize_text(column = "crop_name", dictionary = c("coca", "cafe", "cacao"))
qsr_crosswalk(column = "ubigeo",
              crosswalk = data.frame(old = "0502", new = "0503"),
              year_column = "year", apply_before = 2017)
qsr_currency(columns = "ingreso_laboral", from_currency = "PEN", base_year = 2015)
qsr_validate(checks = "survey")
qsr_write(path = "epa_clean.dta")                 # mirror of qsr_read: any format
```

The design-based numbers match the `survey` package exactly (validated to machine
precision) — QUASAR just gives you one call instead of building the design and
calling `svyby()` by hand. For replicate-weight designs (BRR/jackknife/bootstrap),
pass a `survey::svrepdesign()` to `design =`.

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
qsr_read("data/large_file.parquet")       # CSV/Parquet/Arrow + stat packages, auto-detected
qsr_fast_group("sales.parquet", by = "region", col = "revenue", fn = "mean")
qsr_fast_filter("logs.csv", col = "status", op = "eq", value = 200)
qsr_sink_parquet("huge.csv", "bronze.parquet")   # stream CSV -> Parquet, flat memory
```

The Polars engine performs aggregations, filtering, and sorting directly
on files (CSV **and** Parquet) without loading them fully into R. Its real advantage is **peak
memory** — a fraction of what `data.table` or `dplyr` use — which matters
when a file does not fit comfortably in RAM. For raw speed on data that
already fits in memory, `data.table::fread()` remains faster, and QUASAR
says so. Measured, honest benchmarks (wall-clock **and** memory) live in the
[rquasar-benchmarks](https://github.com/TodoEconometria/rquasar-benchmarks)
companion repository.

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

- **50+ exported functions** across 15 modules
- `R CMD check --as-cran`: **0 errors, 0 warnings** (1 note: new submission)
- **6 vignettes** with worked, runnable examples
- Reads **10+ data formats** (CSV, Parquet, Arrow/Feather, SPSS, Stata, SAS,
  Excel, dBase, JSON/NDJSON, RDS/RData, fixed-width) — and writes them back
- **`qsr_survey`** — a bundled synthetic household-survey dataset (`?qsr_survey`)
- Built-in exchange rates and CPI deflators for BOB, PEN, COP, BRL (2006-2023)

## Vignettes

- **Getting Started** — the five-call promise on `mtcars`
- **From Survey Microdata to Publication Tables** — the survey toolchain, on
  `wooldridge::wage1` (Mincer) and the bundled `qsr_survey`
- **Fast Data with the Rust Backend** — file operations on `nycflights13`
- **Complex Survey Designs** — design-based standard errors, CIs and design
  effects with `qsr_svydesign()` + `qsr_tabulate(se = TRUE)`, validated against
  the `survey` package on `survey::api`
- **Machine Learning Pipeline** and **Setup & AI** — the ML and AI-assist modules

## Validated on Real Data

QUASAR was developed and validated on 1.14 million observations from
Bolivia (Encuesta de Hogares, 2006-2024) and Peru (ENAHO, 2004-2023)
household surveys. Models estimated include Probit, Ordered Probit,
Tobit, Heckman, Double Hurdle, and Multinomial Logit.

## License

GPL-3

## Author

Juan Marcelo Gutierrez Miranda — [TodoEconometria](https://github.com/TodoEconometria)
