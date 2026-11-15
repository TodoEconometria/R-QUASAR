## Submission summary

This is a new submission of **rquasar**. The package reads survey microdata in
the formats used by statistical offices, produces design-based survey estimates
(building on 'survey'), poverty (FGT) and inequality (Gini, Theil, Atkinson)
measures with design-based standard errors, and publication tables. It includes
a small 'Rust' backend (via 'extendr' and 'Polars') that filters, groups and
converts large delimited files to Parquet in bounded memory.

## Test environments

* Local: Windows 11 x64, R 4.6.1 (ucrt), rustc/cargo 1.97.1, `R CMD check --as-cran`
  on the final tarball (15-nov-2026): 0 errors, 0 warnings, 1 note.
* Linux (Debian bookworm, Docker, 2 CPUs) with the oldest supported toolchain,
  rustc 1.82: package compiles and installs.
* win-builder (R-release 4.6.2 and R-devel r90890, 15-nov-2026): 0 errors,
  0 warnings, 1 note (new submission only).
* GitHub Actions (R-release): Ubuntu, Windows and R-devel on Ubuntu, all passing.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Juan Marcelo Gutierrez Miranda <info@todoeconometria.com>'
  New submission

This is the first submission of the package.

The installed size is 44.0 Mb, of which 42.4 Mb is the compiled 'Rust' library
in `libs/` (the 'Polars' backend). The source tarball is 9.86 MB; most of it is
the vendored crates in `src/rust/vendor.tar.xz`, required for offline
compilation as described in "Using Rust in CRAN packages". The vendored set was
filtered to the platforms CRAN builds on, with tests, benchmarks, documentation,
test data and all prebuilt binaries removed, and only the 'Polars' features the
package uses enabled.

## Rust

* `SystemRequirements: Cargo (Rust's package manager), rustc >= 1.82, xz`. The
  minimum version is the real one required by the vendored crates and was
  tested with rustc 1.82 on Linux.
* `configure`/`configure.win` check that `cargo` and `rustc` are present and
  meet the minimum version; otherwise they stop with an explanatory message and
  never try to install a toolchain. They report the versions found.
* All crates are vendored (`src/rust/vendor.tar.xz`) and compiled with
  `cargo build -j 2 --offline --locked`. No network access is used during
  installation, and `CARGO_HOME` points to a temporary directory inside the
  build tree, so nothing is written to `~/.cargo`.
* Installation time: about 12 minutes on Windows (2 jobs) and about 6 minutes
  on Linux with 2 CPUs.
* Authorship and copyright of the 181 vendored crates are listed in
  `inst/AUTHORS` and acknowledged in `Authors@R` (role "cph") and the
  `Copyright` field. All crate licences are compatible with GPL (>= 3).
* At run time the package uses at most two threads unless the user raises the
  limit with `options(rquasar.threads = )`, and never more than
  `OMP_THREAD_LIMIT`.

## Downstream dependencies

There are currently no reverse dependencies (new package).
