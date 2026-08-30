# rquasar (development version)

## New features

* **`qsr_model()` now accepts `weights`** (a column name or a numeric vector) for weighted regression -- essential for survey microdata (the elevation factor). Unweighted calls are unchanged.

* **`qsr_read()` now reads SPSS (`.sav`, `.zsav`, `.por`), Stata (`.dta`) and SAS (`.sas7bdat`, `.xpt`), plus Excel (`.xlsx`, `.xls`) and dBase (`.dbf`)
  files** (via haven, already in Suggests), auto-detected from the extension.
  These are the native formats of survey microdata (EPA/ENAHO/EH), so the survey
  workflow no longer needs a separate import step. `columns` and `n_rows` are
  pushed to haven's `col_select` / `n_max`.
* **`qsr_read()` also reads modern interchange and native R formats**:
  JSON / NDJSON (`.json`, `.ndjson`, `.jsonl`, via jsonlite), Arrow / Feather IPC
  (`.arrow`, `.feather`, `.ipc`, via arrow) and native R (`.rds`, and
  `.rdata`/`.rda` — the first data.frame in the container is loaded). RDS/RData
  let an analyst resume a session from a saved data.frame with the same
  `qsr_read()` call; JSON must be an array of flat objects (or NDJSON), and a
  nested/non-tabular payload aborts with a clear message.
* **New `qsr_read_fwf()`** reads fixed-width (positional) files — the format of
  classic survey record layouts (INE "diseño de registro"). Fields are given by
  `positions` (start-end pairs) or `widths`, with explicit `encoding`
  (`latin1`/`cp850`/`utf8`) for old survey files.
* **New `qsr_tabulate()`** — weighted survey tabulation. Computes weighted
  counts, proportions, means or sums by group, applying the survey weight
  (auto-detected from common names: `factor`, `factorel`, `weight`, `fexp`, …).
  This turns raw survey microdata into population estimates (tasa de paro,
  millones de personas, participación por grupo) in one call.

## Bug fixes

* `qsr_fast_group()` now accepts **Parquet** inputs (previously it scanned any
  file as CSV and failed on `.parquet`). Parquet files are read columnar and
  aggregated in memory; CSV keeps the streaming Rust path.


# rquasar 0.1.0

First public release.

## Breaking changes

* **Default output locations moved out of the working directory.** To avoid
  writing to the user's current directory (and to close a cache-poisoning
  vector), these now default to `tempdir()` or `tools::R_user_dir()`:
  * `qsr_table()`, `qsr_plot()`, `qsr_report()` — `output_dir`
  * `qsr_deploy()` — `path`
  * `qsr_decision_log()` — `log_file`
  * the `qsr_fetch()` / `qsr_download()` cache

  Pass an explicit path to restore the previous behaviour, e.g.
  `qsr_table(output_dir = "outputs/tables")`.

## Features

* `qsr_survey`: a bundled, fully synthetic household-survey dataset (shaped like
  ENAHO / Encuesta de Hogares, with no real observations) so every survey tool
  can be shown on runnable, self-contained data. See `?qsr_survey`.
* Vignettes for *Getting Started*, *From Survey Microdata to Publication
  Tables*, *Fast Data with the Rust Backend*, *Complex Survey Designs*, and
  *Setup: Installation, Requirements, and AI*.
* AI helpers (`qsr_ai_review()`, `qsr_ai_classify()`, `qsr_ai_flag()`) support a
  **local Ollama backend** via `provider = "ollama"` in addition to Anthropic
  Claude: no API key, and the data never leaves the machine.
* `qsr_read()` handles real-world delimited files: a `delim` argument for any
  field separator (e.g. `"|"`, `";"`, tab) and an `encoding` argument that
  transcodes non-UTF-8 sources (e.g. `"latin1"`) to UTF-8 on the fly, preserving
  accents. For CSV, `columns` is now a genuine **projection pushdown** to the
  Polars scan, so wide files read only the columns you ask for.
* `qsr_sink_parquet()`: stream a very large delimited file straight to Parquet
  using the Polars streaming engine, in bounded memory. Convert tens of
  gigabytes to columnar Parquet on a laptop, then read/query it at speed. Honours
  `delim`, `encoding` and `columns` (projection pushed to the scan).

## Security

* `qsr_report()` now escapes `title` and `author` before they are written into
  the generated R Markdown / LaTeX, preventing code injection through those
  arguments.
* Archive extraction (`qsr_download()`, `qsr_load_folder()`) rejects path
  traversal and caps the uncompressed size and entry count (zip-bomb guard).
* Caches are read/written under `tools::R_user_dir()` and validated after
  `readRDS()`.

## Bug fixes

* `qsr_read(columns = ...)` now returns the requested columns; the argument was
  previously ignored.
* Raised the in-process/subprocess threshold from 10 MB to 128 MB so medium
  files are not slowed down by subprocess startup.
