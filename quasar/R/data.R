#' Synthetic household-survey panel
#'
#' A fully synthetic household-survey panel that mirrors the *public structure*
#' of Latin American household surveys - ENAHO (INEI, Peru) and the Encuesta de
#' Hogares (INE, Bolivia): geographic codes, income in local currency, expansion
#' weights, indigenous self-identification, and free-text fields with realistic
#' data-entry noise. **No real observation is used**; every value is simulated
#' from a fixed seed via a Mincer earnings data-generating process, so downstream
#' models recover textbook coefficients (about a 7% return to schooling and a
#' negative ethnic income gap) and the dataset carries no licensing or privacy
#' constraints. It exists so the survey tools - [qsr_normalize_text()],
#' [qsr_crosswalk()], [qsr_currency()] and [qsr_validate()] - can be shown on
#' runnable, self-contained data.
#'
#' @format A data frame with 3,600 rows and 14 variables:
#' \describe{
#'   \item{id}{Household identifier.}
#'   \item{year}{Survey wave, 2010-2018.}
#'   \item{region}{Region (factor, 6 levels).}
#'   \item{ubigeo}{Geographic code; two pre-2017 codes are remapped by the 2017
#'     boundary update (used to demonstrate [qsr_crosswalk()]).}
#'   \item{indigenous}{Indigenous self-identification (0/1).}
#'   \item{education}{Years of schooling.}
#'   \item{experience}{Potential labor-market experience, in years.}
#'   \item{tenure}{Years in the current job.}
#'   \item{occupation}{Occupation, free text with deliberate data-entry noise.}
#'   \item{crop_name}{Main crop, free text with deliberate data-entry noise.}
#'   \item{ingreso_laboral}{Monthly labor income, local currency (PEN); a few
#'     values are NA or negative on purpose, to exercise [qsr_validate()].}
#'   \item{ingreso_total}{Total household income, local currency (PEN).}
#'   \item{log_income}{Natural log of \code{ingreso_total}.}
#'   \item{weight}{Survey expansion weight.}
#' }
#' @source Simulated. Generator: \code{data-raw/make_qsr_survey.R}. The structure
#'   imitates the published codebooks of ENAHO (INEI) and the Encuesta de Hogares
#'   (INE); it contains no data from them.
#' @examples
#' qsr_config()
#' qsr_data(qsr_survey)
#' qsr_model(log_income ~ education + experience + indigenous)
"qsr_survey"
