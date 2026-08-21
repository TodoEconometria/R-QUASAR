# -----------------------------------------------------------------------------
# make_qsr_survey.R
#
# Synthetic household-survey panel that MIRRORS the *public structure* of Latin
# American household surveys — ENAHO (INEI, Peru) and Encuesta de Hogares
# (INE, Bolivia): variable names, ubigeo geo-codes, income in local currency,
# expansion weights, free-text fields with data-entry noise — WITHOUT using a
# single real observation. Every value is simulated from a fixed seed, so the
# dataset is fully reproducible and carries no licensing or privacy constraints.
#
# This is the canonical generator. The distilled dataset it produces
# (`qsr_survey`) graduates into the rquasar package `data/` directory, where it
# powers runnable examples and the survey vignette. Keeping the generator here
# (and not in the package) is deliberate: the package ships the small result,
# the lab keeps the recipe.
#
# Teaching note: this is a worked template for "how to synthesize survey
# microdata from a published codebook" — draw the structure from the public
# documentation (INEI ENAHO, INE EH), then simulate the joint distribution with
# an economically sensible data-generating process (here, a Mincer earnings
# equation) so downstream models recover meaningful coefficients.
# -----------------------------------------------------------------------------

set.seed(42)

n_hh  <- 3600L
waves <- 2010:2018

# --- Geography: ubigeo codes, a few of which are redrawn at the 2017 boundary
#     update (the classic reason qsr_crosswalk() exists) -------------------------
regions <- c("Altiplano", "Valles", "Llanos", "Costa", "Sierra", "Selva")
region_code <- c(Altiplano = 1, Valles = 2, Llanos = 3,
                 Costa = 4, Sierra = 5, Selva = 6)

# base provinces per region (2 each) -> old ubigeo codes
old_ubigeo <- sprintf("%02d%02d",
                      rep(region_code, each = 2),
                      rep(1:2, times = length(regions)))

# 2017 redistricting: two provinces get new codes
ubigeo_crosswalk <- data.frame(
  old  = c("0502", "0602"),
  new  = c("0503", "0699"),
  stringsAsFactors = FALSE
)

# --- Household-year rows --------------------------------------------------------
df <- data.frame(
  id     = seq_len(n_hh),
  year   = sample(waves, n_hh, replace = TRUE),
  region = factor(sample(regions, n_hh, replace = TRUE), levels = regions),
  stringsAsFactors = FALSE
)

# ubigeo consistent with region; older waves keep the pre-2017 codes so the
# crosswalk has something to remap
df$ubigeo <- vapply(seq_len(n_hh), function(i) {
  rc <- region_code[[as.character(df$region[i])]]
  sprintf("%02d%02d", rc, sample(1:2, 1))
}, character(1))

# --- Demographics ---------------------------------------------------------------
df$indigenous <- rbinom(
  n_hh, 1,
  prob = c(Altiplano = .72, Valles = .45, Llanos = .18,
           Costa = .12, Sierra = .55, Selva = .30)[as.character(df$region)]
)

age <- pmin(70L, pmax(18L, round(rnorm(n_hh, 39, 12))))
df$education  <- pmin(18L, pmax(0L, round(rnorm(n_hh, 9.5, 3.6))))
df$experience <- pmax(0L, age - df$education - 6L)
df$tenure     <- pmin(df$experience, rpois(n_hh, 5))

# --- Earnings: Mincer data-generating process (income in PEN) -------------------
region_fe <- c(Altiplano = -.18, Valles = -.05, Llanos = .10,
               Costa = .22, Sierra = -.08, Selva = .02)
log_wage <- 6.10 +
  0.085 * df$education +
  0.031 * df$experience -
  0.00052 * df$experience^2 -
  0.148 * df$indigenous +
  region_fe[as.character(df$region)] +
  rnorm(n_hh, 0, 0.55)

df$ingreso_laboral <- round(exp(log_wage), 1)                    # monthly labor income, PEN
other_income       <- round(exp(rnorm(n_hh, 5.4, 0.9)), 1)       # transfers, rents, etc.
df$ingreso_total   <- df$ingreso_laboral + other_income
df$log_income      <- log(df$ingreso_total)

# survey expansion weight (inflation factor)
df$weight <- round(rlnorm(n_hh, meanlog = 4.6, sdlog = 0.35), 1)

# --- Free-text fields with realistic data-entry noise ---------------------------
inject_typos <- function(x, rate = 0.14) {
  hit <- runif(length(x)) < rate
  x[hit] <- vapply(x[hit], function(w) {
    if (nchar(w) < 3) return(w)
    op <- sample(c("dup", "drop", "swap"), 1)
    p  <- sample(seq_len(nchar(w) - 1), 1)
    ch <- strsplit(w, "")[[1]]
    if (op == "dup")  return(paste0(paste(ch[1:p], collapse = ""), ch[p], paste(ch[(p+1):length(ch)], collapse = "")))
    if (op == "drop") return(paste0(ch[-p], collapse = ""))
    tmp <- ch[p]; ch[p] <- ch[p+1]; ch[p+1] <- tmp; paste(ch, collapse = "")
  }, character(1))
  x
}

crops <- c("coca", "cafe", "cacao", "banana", "arroz")
occs  <- c("agricultor", "comerciante", "minero", "profesor", "obrero", "empleado")
df$crop_name  <- inject_typos(sample(crops, n_hh, replace = TRUE))
df$occupation <- inject_typos(sample(occs,  n_hh, replace = TRUE))

# --- A little dirt so qsr_validate(checks = "survey") has something to report ---
df$ingreso_laboral[sample(n_hh, 30)] <- NA          # item non-response
df$ingreso_laboral[sample(which(!is.na(df$ingreso_laboral)), 3)] <- -1  # coding error

# column order (survey-first, then model vars)
df <- df[, c("id", "year", "region", "ubigeo", "indigenous",
             "education", "experience", "tenure", "occupation", "crop_name",
             "ingreso_laboral", "ingreso_total", "log_income", "weight")]

qsr_survey <- df

# --- Persist (run from the repo root: Rscript data-raw/make_qsr_survey.R) -------
if (!dir.exists("data")) dir.create("data", showWarnings = FALSE)
save(qsr_survey, file = "data/qsr_survey.rda", compress = "xz")
write.csv(ubigeo_crosswalk, "data/ubigeo_crosswalk_2017.csv", row.names = FALSE)

cat(sprintf("qsr_survey: %d rows x %d cols  (%s)\n",
            nrow(qsr_survey), ncol(qsr_survey),
            paste(names(qsr_survey), collapse = ", ")))
cat(sprintf("saved data/qsr_survey.rda (%s)\n",
            format(structure(file.size("data/qsr_survey.rda"), class = "object_size"), units = "auto")))
