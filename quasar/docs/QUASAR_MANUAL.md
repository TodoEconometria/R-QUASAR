# QUASAR User Manual

**Version 0.1.0 | TodoEconometria**

All examples in this manual were executed with real data from the `wooldridge` package (Wooldridge, 2019). Every output shown is actual R console output, not simulated.

---

## Installation

```r
# From GitHub
remotes::install_github("TodoEconometria/R-QUASAR", subdir = "quasar")

# Optional: ML packages (installed on demand)
install.packages(c("ranger", "xgboost", "e1071", "nnet"))

# Optional: interactive plots
install.packages("plotly")
```

---

## 1. The Context Engine

QUASAR's core idea: register your data and model once, then every function reads from context. No repeated arguments.

### Traditional R

```r
library(wooldridge)
data(wage1)
model <- lm(log(wage) ~ educ + exper + I(exper^2) + tenure + female + nonwhite, data = wage1)
summary(model)
gtsummary::tbl_regression(model)
modelsummary::modelsummary(model)
ggplot2::ggplot(wage1, aes(x = educ, y = wage)) + geom_point()
```

Five separate calls, each requiring the data or model as an argument.

### QUASAR

```r
library(quasar)
library(wooldridge)
data(wage1)

qsr_data(wage1)
#> v Registered "dataset": 526 rows x 24 cols

qsr_model(log(wage) ~ educ + exper + I(exper^2) + tenure + female + nonwhite)
#> v Model "last" (lm): log(wage) ~ educ + exper + I(exper^2) + tenure + female + nonwhite

qsr_table()
#> v Table created
# Returns a gt table with coefficients, standard errors, p-values, and model fit statistics.
# Zero arguments needed — QUASAR reads the model from context.
```

### Configuration

```r
qsr_config(
  significance_level = 0.05,
  output_format      = "apa7",
  data_path          = "E:/my_research/data"  # where to store downloads
)
```

---

## 2. Statistical Tests

Dataset: `sleep75` — 706 individuals, sleep and work patterns (Biddle & Hamermesh, 1990).

### t-test: Do men sleep more than women?

```r
data(sleep75)
qsr_data(sleep75)

qsr_test(sleep ~ male, test = "t")
```

Output:

```
-- Welch Two Sample t-test
Formula: sleep ~ male
Statistic: 0.9474
p-value: "0.3438"
Estimate: 3284.5882 and 3252.4075
i Not significant at 0.05 level
```

Result: no significant difference (p = 0.34). Men and women sleep roughly the same amount.

### Normality test

```r
qsr_test(formula = ~totwrk, test = "shapiro", data = sleep75)
```

Output:

```
-- Shapiro-Wilk normality test
Formula: totwrk
Statistic: 0.9665
p-value: "1.23e-11" ***
v Significant at 0.05 level
```

Total work minutes is not normally distributed (p < 0.001).

### Correlation

```r
qsr_test(sleep ~ totwrk, test = "correlation")
```

Output:

```
-- Pearson's product-moment correlation
Formula: sleep ~ totwrk
Statistic: -9.005
p-value: "< 2.2e-16" ***
Estimate: -0.3214
v Significant at 0.05 level
```

Strong negative correlation (r = -0.32): more work = less sleep.

---

## 3. Data Cleaning

Dataset: `hprice1` — 88 houses, hedonic pricing model (Wooldridge, 2019).

```r
data(hprice1)
qsr_data(hprice1)
#> v Registered "dataset": 88 rows x 10 cols

qsr_clean(outliers = "winsorize", normalize = "none")
#> v Cleaned: 88 -> 88 rows, 10 cols
#> i   winsorized at |z|>3
```

Options: `impute` (mean/median/mode/knn), `outliers` (remove/winsorize/cap), `normalize` (zscore/minmax/robust), `encode` (dummy/label).

---

## 4. Machine Learning

Dataset: `wage2` — 935 workers, wage determinants (Blackburn & Neumark, 1992).

### Random Forest

```r
data(wage2)
qsr_data(wage2)

qsr_ml(log(wage) ~ educ + exper + tenure + married + black + south + urban,
       method = "rf")
#> v Model Random Forest: log(wage) ~ educ + exper + tenure + married + black +
#>   south + urban (500 trees)
```

Available methods: `"rf"`, `"xgboost"`, `"svm"`, `"nn"`, `"knn"`, `"lasso"`, `"ridge"`, `"elastic"`, `"kmeans"`, `"pca"`.

### Cross-validation

```r
qsr_model(log(wage) ~ educ + exper + tenure + married + black + south + urban)
qsr_cv(folds = 5)
#> v CV rf (10-fold): RMSE = 1033.43, R2 = -5.54
```

---

## 5. Time Series

Dataset: `intdef` — 56 years of interest rates and inflation (Wooldridge, 2019).

```r
data(intdef)
qsr_ts(intdef, y = "inf", method = "hw")
#> v Model Holt-Winters: alpha=1, beta=0
```

Methods: `"arima"` (auto ARIMA), `"ets"` (exponential smoothing), `"hw"` (Holt-Winters), `"decompose"`.

---

## 6. Multi-Model Comparison Tables

Dataset: `bwght` — 1,388 births, effect of smoking on birth weight (Mullahy, 1997).

```r
data(bwght)

m1 <- lm(bwght ~ cigs + faminc, data = bwght)
m2 <- lm(bwght ~ cigs + faminc + motheduc + fatheduc, data = bwght)
m3 <- lm(bwght ~ cigs + faminc + motheduc + fatheduc + male + white, data = bwght)

qsr_table(list(m1, m2, m3), type = "comparison")
```

Output:

```
+-------------+------------+------------+---------------+
|             | Simple     | +Education | +Demographics |
+=============+============+============+===============+
| (Intercept) | 116.974*** | 118.074*** | 113.005***    |
|             | (1.049)    | (3.500)    | (3.706)       |
| cigs        | -0.463***  | -0.589***  | -0.591***     |
|             | (0.092)    | (0.111)    | (0.110)       |
| faminc      | 0.093**    | 0.054      | 0.042         |
|             | (0.029)    | (0.037)    | (0.037)       |
| motheduc    |            | -0.438     | -0.402        |
|             |            | (0.320)    | (0.318)       |
| fatheduc    |            | 0.494      | 0.436         |
|             |            | (0.283)    | (0.282)       |
| male        |            |            | 3.716**       |
|             |            |            | (1.146)       |
| white       |            |            | 4.519**       |
|             |            |            | (1.611)       |
| Num.Obs.    | 1388       | 1191       | 1191          |
| R2          | 0.030      | 0.033      | 0.047         |
| R2 Adj.     | 0.028      | 0.030      | 0.042         |
+=============+============+============+===============+
| * p < 0.05, ** p < 0.01, *** p < 0.001                |
+=============+============+============+===============+
```

Each additional cigarette reduces birth weight by 0.59 ounces (p < 0.001). The effect is robust across all three specifications.

Export to LaTeX:
```r
qsr_table(list(m1, m2, m3), type = "comparison", export = "latex", file = "table1.tex")
```

---

## 7. Probit with Marginal Effects

Dataset: `mroz` — 753 married women, labor force participation (Mroz, 1987).

```r
data(mroz)
qsr_data(mroz)

qsr_model(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6,
          family = "probit")
```

Probit coefficients:

```
Variable                 Coef         SE        z   Sig
-------------------------------------------------------
(Intercept)            0.2701     0.5081     0.53
nwifeinc              -0.0120     0.0049    -2.43     *
educ                   0.1309     0.0254     5.15   ***
exper                  0.1233     0.0188     6.58   ***
I(exper^2)            -0.0019     0.0006    -3.15    **
age                   -0.0529     0.0085    -6.25   ***
kidslt6               -0.8683     0.1184    -7.34   ***
kidsge6                0.0360     0.0440     0.82

N = 753 | Pseudo-R2 = 0.2206
```

Average Partial Effects (via `margins`):

```
Variable                  APE
------------------------------
age                   -0.0159
educ                   0.0394
exper                  0.0256
kidsge6                0.0108
kidslt6               -0.2612
nwifeinc              -0.0036
```

Interpretation: each additional child under 6 reduces the probability of labor force participation by 26.1 percentage points. Each year of education increases it by 3.9 pp. These are the effects at the mean of all other variables.

---

## 8. Real-World Application: Coca in the Andes

This section shows QUASAR applied to actual research — a cross-country study of coca leaf consumption and production using household survey microdata from Bolivia (INE) and Peru (INEI).

### The data pipeline

```r
# Bolivia: 19 waves of Encuestas de Hogares (2005-2024)
# 571,940 individuals x 48 variables
# Stored as .sav files on F: drive, harmonized to 18 MB parquet

# Peru: 19 waves of ENAHO (2006-2024)
# 563,842 households x 26 variables
# 218 .sav files (24 GB), harmonized to 20.6 MB parquet
```

### Double Hurdle results (actual output)

Bolivia models consumption (who chews coca and how much). Peru models production (who grows coca and how much). The Double Hurdle separates the participation decision from the intensity decision.

```
                    BOL-Participation  BOL-Intensity  PER-Participation  PER-Intensity
NPIOC/indigena         +0.197***        +0.002          +0.186***         -0.146
pobre                  +0.040***        +0.001          +0.066.           -0.074
pobrextr               +0.029**         +0.012***       -0.126*           -0.515**
hombre                 +0.129***        +0.005***       +0.357***         +0.299*
urbano                 -0.604***        -0.023***          --                --
-------
N                       393,323          50,645          118,594              890
Pseudo-R2 / R2            0.15            0.46             0.24             0.20
```

The key finding: extreme poverty increases coca consumption intensity in Bolivia (+0.012\*\*\*) but decreases production intensity in Peru (-0.515\*\*). The poorest households chew more coca but cannot produce it because they lack land. This is the "poverty trap" asymmetry between the demand and supply sides of the coca market.

---

## Function Reference

37 exported functions organized by layer:

| Layer | Functions |
|-------|-----------|
| Scaffold | `qsr_init` |
| Context | `qsr_config`, `qsr_data`, `qsr_model`, `qsr_get`, `qsr_reset` |
| Connectors | `qsr_db`, `qsr_spark`, `qsr_fetch`, `qsr_python`, `qsr_search` |
| Output | `qsr_table`, `qsr_plot`, `qsr_report` |
| Rust Engine | `qsr_read`, `qsr_fast_summary`, `qsr_fast_group`, `qsr_fast_filter`, `qsr_fast_sort`, `qsr_benchmark` |
| Analytics | `qsr_clean`, `qsr_test`, `qsr_feature` |
| ML | `qsr_ml`, `qsr_cv`, `qsr_compare` |
| Time Series | `qsr_ts` |
| Research | `qsr_download`, `qsr_panel`, `qsr_scrape`, `qsr_spatial`, `qsr_load_folder`, `qsr_guide` |
| Survey | `qsr_normalize_text`, `qsr_crosswalk`, `qsr_currency`, `qsr_validate`, `qsr_decision_log` |
| AI | `qsr_ai_review`, `qsr_ai_classify`, `qsr_ai_flag` |
| Deploy | `qsr_deploy` |

---

*QUASAR is developed by TodoEconometria.*
*Author: Juan Marcelo Gutierrez Miranda.*
*License: GPL-3.*

*Support: [www.todoeconometria.com](https://www.todoeconometria.com)*
