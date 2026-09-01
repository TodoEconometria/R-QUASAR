<img src="quasar/man/figures/logo.png" align="right" height="120" alt="TodoEconometria" />

# rquasar

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R](https://img.shields.io/badge/R-%3E%3D%204.2-blue)
![check](https://img.shields.io/badge/R--CMD--check-0E%2F0W%2F1N-brightgreen)
<!-- badges: end -->

> **QUASAR** — Query-Driven Unified Automated Stack for Analytical Runtime
> By **Juan Marcelo Gutierrez Miranda** · [TodoEconometria](https://github.com/TodoEconometria)

**The R framework for applied economists who go from survey microdata to
publication-ready output — without boilerplate.** Declare your data once; every
downstream model, table, plot and report reads from context. File-level
operations on large data are delegated to a **Rust/Polars backend** for
out-of-core, low-memory processing.

> **The package lives in [`quasar/`](quasar/).** Full documentation, vignettes
> and the manual are there — start with [`quasar/README.md`](quasar/README.md).

## Install

```r
# install.packages("pak")
pak::pak("TodoEconometria/R-QUASAR/quasar")
```

Requires R ≥ 4.2 and the Rust toolchain (for the Polars backend).

## What it does

| | |
|---|---|
| **Read any survey format** | SPSS/Stata/SAS, Excel, dBase, CSV/Parquet/Arrow, JSON/NDJSON, RDS/RData, fixed-width record layouts — auto-detected (`qsr_read()`, `qsr_read_fwf()`) |
| **Write them back** | `qsr_write()` — the mirror of `qsr_read()` across the same formats |
| **Design-based inference** | `qsr_svydesign()` + `qsr_tabulate(se = TRUE)` + `qsr_model(design =)` — SE, CI and design effects in one call, matching the `survey` package to machine precision |
| **Scale past RAM** | `qsr_read()` / `qsr_fast_*()` / `qsr_sink_parquet()` — Polars/Rust, low peak memory |
| **Publication output** | `qsr_table()` (APA7/LaTeX), `qsr_plot()` (20 types), `qsr_report()` |

## Quick start

```r
library(rquasar)
qsr_config(); qsr_data(mtcars)
qsr_model(mpg ~ wt + hp)
qsr_table()                     # APA7 regression table
qsr_plot(type = "coefficient")  # coefficient plot
```

## Documentation

- [Package README](quasar/README.md) — full feature tour
- [Vignettes](quasar/vignettes/) — Getting Started, Survey to Publication,
  Complex Survey Designs (design-based SE), Rust Backend, ML Pipeline, Setup & AI
- [Manual](quasar/docs/QUASAR_MANUAL.md)
- Companion open benchmarks: [rquasar-benchmarks](https://github.com/TodoEconometria/rquasar-benchmarks)

## License

GPL-3 · © Juan Marcelo Gutierrez Miranda — [TodoEconometria](https://github.com/TodoEconometria)
