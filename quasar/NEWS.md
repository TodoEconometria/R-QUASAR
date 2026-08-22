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
