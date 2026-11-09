# Check that 'cargo' and 'rustc' are available and recent enough before
# compiling. Sourced from tools/config.R by configure / configure.win.
# It never installs a toolchain: it reports the versions found or stops with
# an explanatory message.

desc <- read.dcf("DESCRIPTION")

if (!"SystemRequirements" %in% colnames(desc)) {
  stop(paste(c(
    "`SystemRequirements` not found in `DESCRIPTION`.",
    "Please specify `SystemRequirements: Cargo (Rust's package manager), rustc`"
  ), collapse = "\n"))
}

sysreqs <- gsub("\\s+", " ", desc[, "SystemRequirements"])

if (!grepl("cargo", sysreqs, ignore.case = TRUE) ||
    !grepl("rustc", sysreqs, ignore.case = TRUE)) {
  stop("`SystemRequirements` must name `Cargo (Rust's package manager), rustc`.")
}

# e.g. "rustc >= 1.80" -> "1.80"
parts <- strsplit(sysreqs, ",\\s*")[[1]]
rustc_req <- trimws(parts[grepl("rustc", parts)][1])

extract_semver <- function(ver) {
  ver <- paste(ver, collapse = " ")
  if (grepl("\\d+\\.\\d+(\\.\\d+)?", ver)) {
    sub("^[^0-9]*?(\\d+\\.\\d+(\\.\\d+)?).*$", "\\1", ver, perl = TRUE)
  } else {
    NA_character_
  }
}
msrv <- extract_semver(rustc_req)

install_hint <- c(
  "Install Rust (rustc and cargo) from https://www.rust-lang.org/tools/install",
  "or from your OS package manager:",
  " - Debian/Ubuntu: apt-get install cargo rustc",
  " - Fedora/RHEL:   dnf install cargo rustc",
  " - macOS:         brew install rust",
  " - Windows:       rustup with the 'x86_64-pc-windows-gnu' target"
)

# The default rustup location is often missing from PATH in R sessions.
home <- Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME")
cargo_bin <- file.path(home, ".cargo", "bin")
if (nzchar(home) && dir.exists(cargo_bin)) {
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), cargo_bin, sep = .Platform$path.sep))
}

# First line of `<tool> --version`, or NA when the tool is missing or fails.
# Sys.which() avoids the silent character(0) that system() returns on Unix.
tool_version <- function(tool) {
  if (!nzchar(Sys.which(tool))) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(system2(tool, "--version", stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )
  status <- attr(out, "status")
  if (length(out) == 0L || (!is.null(status) && status != 0L)) NA_character_ else out[1L]
}

rustc_version <- tool_version("rustc")
cargo_version <- tool_version("cargo")

if (is.na(rustc_version) || is.na(cargo_version)) {
  missing <- c("rustc", "cargo")[c(is.na(rustc_version), is.na(cargo_version))]
  stop(paste(c(
    "",
    "------------------------- [RUST NOT FOUND] -------------------------",
    sprintf("Could not run %s.", paste(sprintf("'%s'", missing), collapse = " and ")),
    sprintf("rquasar needs %s and cargo to compile its Rust backend.", rustc_req),
    install_hint,
    "--------------------------------------------------------------------"
  ), collapse = "\n"), call. = FALSE)
}

current <- extract_semver(rustc_version)
if (!is.na(msrv) && !is.na(current) &&
    utils::compareVersion(msrv, current) == 1L) {
  stop(paste(c(
    "",
    "-------------------- [UNSUPPORTED RUST VERSION] --------------------",
    sprintf("rquasar needs rustc >= %s; found '%s'.", msrv, rustc_version),
    "Update with `rustup update stable`, or:",
    install_hint,
    "--------------------------------------------------------------------"
  ), collapse = "\n"), call. = FALSE)
}

# CRAN asks to report the versions used before compiling.
message(sprintf("Using %s\nUsing %s", cargo_version, rustc_version))
