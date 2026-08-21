## Submission summary

This is a new submission of **rquasar**, a framework that eliminates
boilerplate between raw survey microdata and publication-ready output, with a
Rust/Polars backend for file-level operations on large datasets.

## Test environments

* Local: Windows 11, R 4.6.1, Rust 1.97 (stable-x86_64-pc-windows-gnu)
* [PENDIENTE antes de enviar] win-builder: R-release and R-devel
* [PENDIENTE antes de enviar] R-hub v2: Linux (ubuntu-latest) and macOS

## R CMD check results

`R CMD check --as-cran` (local):

```
Status: 1 NOTE
0 errors | 0 warnings | 1 note
```

The single NOTE contains:

1. **New submission.** Expected for a first submission.

2. **Package size (tarball ~31 MB).** The package bundles a Rust backend
   (extendr + Polars). Per the CRAN policy "Using Rust in CRAN packages", all
   crate sources are vendored offline in `src/rust/vendor.tar.xz` so the build
   requires no network access. The vendored sources account for essentially all
   of the tarball size; the R code and compiled artifacts are small. Feature
   flags on the `polars` dependency are already restricted to the minimum
   required (csv, parquet, lazy, and the aggregation/filter/sort kernels used by
   the exported functions).

3. **Invalid URL (repository).** The `URL`/`BugReports` links point to the
   project repository, which is being made public at submission time; the 404
   resolves once the repository is public.

## System requirements

The package requires the Rust toolchain (Cargo, rustc >= 1.65.0) and `xz` to
build, as declared in `SystemRequirements`. `configure`/`configure.win` verify
the toolchain and unpack the vendored sources; the build respects CRAN's Cargo
flags and does not set non-portable target flags (no `target-cpu=native`).

## Downstream dependencies

There are currently no reverse dependencies (new package).
