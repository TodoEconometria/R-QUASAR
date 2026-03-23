# quasar 0.1.0

## New features

### Core Framework (Phase 1)
* `qsr_init()`: Create standardized project structures for academic, BI, or
  Spark pipeline workflows.
* `qsr_config()`: Set analysis parameters once — significance level, seed,
  output format.
* `qsr_data()`: Register a dataset in the QUASAR context for zero-argument
  downstream calls.
* `qsr_model()`: Fit models (lm, glm, logit, probit) from context — data and
  formula, one line.
* `qsr_get()` / `qsr_reset()`: Read from or clear the global context.

### Connectors (Layer 3)
* `qsr_db()`: One-line database connections (SQLite, PostgreSQL, MySQL, ODBC).
* `qsr_spark()`: Spark via sparklyr with sensible defaults.
* `qsr_fetch()`: Academic data APIs (World Bank, FRED, US Census, any URL)
  with 24-hour caching.
* `qsr_python()`: Python interop via reticulate with automatic virtualenv
  management.

### Output Engine (Layer 4)
* `qsr_table()`: Publication-ready tables in APA7 or Chicago format. Export to
  HTML, LaTeX, Word, RTF.
* `qsr_plot()`: 20 plot types — 7 static (ggplot2) + 13 interactive (plotly).
  Any static plot becomes interactive with `interactive = TRUE`.
* `qsr_report()`: Generate complete reports from Rmd templates.

### Rust/Polars Backend (Phase 2)
* `qsr_read()`: Read CSV and Parquet files 10-30x faster than `read.csv()`.
* `qsr_fast_summary()`: Descriptive statistics computed in Rust.
* `qsr_fast_group()`: Group-by aggregation directly on files.
* `qsr_fast_filter()`: Filter rows without loading full dataset into R.
* `qsr_fast_sort()`: Sort files by column.
* `qsr_benchmark()`: Compare Polars/Rust vs Base R performance.
* Adaptive process isolation: large files (>10MB) automatically use a
  subprocess with full multi-threading for maximum performance.
