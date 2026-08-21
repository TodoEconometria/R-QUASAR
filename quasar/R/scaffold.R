#' Initialize a new QUASAR project
#'
#' Creates a standardized project structure following QUASAR conventions.
#' This is the entry point of the QUASAR framework.
#'
#' @param name Character. Name of the project. Use snake_case.
#' @param type Character. Type of project. One of "academic", "bi_dashboard",
#'   or "spark_pipeline". Defaults to "academic".
#' @param author Character. Author name. Defaults to global option if set.
#' @param path Character. Where to create the project. Defaults to current directory.
#' @param journal Character. Target journal for academic output formatting.
#'   One of "r_journal", "jss", "apsr". Defaults to "r_journal".
#'
#' @return Invisible path to the created project.
#' @export
#'
#' @examples
#' \dontrun{
#' qsr_init("electoral_analysis", type = "academic", author = "Nache")
#' }
qsr_init <- function(name,
                     type    = c("academic", "bi_dashboard", "spark_pipeline"),
                     author  = getOption("rquasar.author", default = ""),
                     path    = ".",
                     journal = c("r_journal", "jss", "apsr")) {

  # Validate inputs
  type    <- match.arg(type)
  journal <- match.arg(journal)

  if (missing(name) || !nzchar(name)) {
    cli::cli_abort("Project {.arg name} cannot be empty.")
  }

  if (!grepl("^[a-z][a-z0-9_]*$", name)) {
    cli::cli_abort(c(
      "{.arg name} must be snake_case.",
      "i" = "Use lowercase letters, numbers and underscores only.",
      "x" = "You provided: {.val {name}}"
    ))
  }

  project_path <- file.path(path, name)

  if (dir.exists(project_path)) {
    cli::cli_abort("Directory {.path {project_path}} already exists.")
  }

  # Build project
  cli::cli_h1("Initializing QUASAR project: {.val {name}}")

  .qsr_create_dirs(project_path, type)
  .qsr_create_config(project_path, name, author, type, journal)
  .qsr_create_main(project_path, name, author)

  cli::cli_h2("Project structure")
  .qsr_print_tree(project_path)

  cli::cli_alert_success(
    "QUASAR project {.val {name}} created at {.path {project_path}}"
  )
  cli::cli_inform(c(
    "i" = "Next step: open {.path {file.path(project_path, 'main.R')}}",
    "i" = "Load config with {.fn qsr_config}"
  ))

  invisible(project_path)
}


# Internal helpers --------------------------------------------------------

#' Create project directory structure
#' @noRd
.qsr_create_dirs <- function(project_path, type) {

  # Core dirs shared by all project types
  core_dirs <- c(
    "R/01_data",
    "R/02_transform",
    "R/03_model",
    "R/04_output",
    "tests",
    "outputs/tables",
    "outputs/figures",
    "outputs/reports",
    "config"
  )

  # Extra dirs depending on project type
  extra_dirs <- switch(type,
    academic       = c("R/05_paper", "outputs/paper"),
    bi_dashboard   = c("R/05_dashboard", "outputs/dashboard"),
    spark_pipeline = c("R/05_pipeline", "logs")
  )

  all_dirs <- c(core_dirs, extra_dirs)

  purrr::walk(all_dirs, function(d) {
    full <- file.path(project_path, d)
    dir.create(full, recursive = TRUE, showWarnings = FALSE)
    # Keep empty dirs in git
    file.create(file.path(full, ".gitkeep"), showWarnings = FALSE)
  })

  cli::cli_alert_success("Created project directories")
}


#' Create YAML config files
#' @noRd
.qsr_create_config <- function(project_path, name, author, type, journal) {

  config <- list(
    project = list(
      name    = name,
      author  = author,
      type    = type,
      journal = journal,
      created = format(Sys.Date(), "%Y-%m-%d")
    ),
    analysis = list(
      significance_level = 0.05,
      random_seed        = 42,
      outlier_threshold  = 3
    ),
    output = list(
      format = "apa7",
      locale = "en_US"
    )
  )

  yaml::write_yaml(config, file.path(project_path, "config", "default.yml"))
  yaml::write_yaml(
    list(environment = "dev"),
    file.path(project_path, "config", "dev.yml")
  )
  yaml::write_yaml(
    list(environment = "prod"),
    file.path(project_path, "config", "prod.yml")
  )

  # Create rquasar.lock for reproducibility
  lock <- list(
    rquasar_version = utils::packageVersion("rquasar"),
    r_version      = R.version.string,
    created        = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  yaml::write_yaml(lock, file.path(project_path, "rquasar.lock"))

  cli::cli_alert_success("Created config files")
}


#' Create main.R entry point
#' @noRd
.qsr_create_main <- function(project_path, name, author) {

  main_content <- glue::glue(
    '# ============================================================
# Project: {name}
# Author:  {author}
# Created: {Sys.Date()}
# Framework: QUASAR | TodoEconometria
# ============================================================

library(rquasar)

# Load project configuration
qsr_config(config_path = "config/default.yml")

# ---- 01 Data ---------------------------------------------------
source("R/01_data/load.R")

# ---- 02 Transform ----------------------------------------------
source("R/02_transform/clean.R")

# ---- 03 Model --------------------------------------------------
source("R/03_model/estimate.R")

# ---- 04 Output -------------------------------------------------
source("R/04_output/report.R")
'
  )

  writeLines(main_content, file.path(project_path, "main.R"))
  cli::cli_alert_success("Created {.file main.R}")
}


#' Print project tree to console
#' @noRd
.qsr_print_tree <- function(project_path) {
  dirs <- list.dirs(project_path, recursive = TRUE, full.names = FALSE)
  dirs <- dirs[nzchar(dirs)]
  purrr::walk(dirs, function(d) {
    depth  <- length(strsplit(d, "/")[[1]])
    indent <- strrep("  ", depth - 1)
    cli::cli_inform("{indent}{cli::col_cyan('\U1F4C1')} {d}")
  })
}