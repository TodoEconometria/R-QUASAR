# ============================================================
# QUASAR - Machine Learning Layer
# qsr_ml(), qsr_cv(), qsr_compare()
# "Neural nets, XGBoost, Random Forest - all in one line."
# ============================================================

#' Fit machine learning models with one line
#'
#' Unified interface for 12+ ML methods. Reads data from context,
#' trains the model, and stores it for downstream use with `qsr_table()`,
#' `qsr_plot()`, and `qsr_compare()`.
#'
#' @param formula Formula (e.g., `mpg ~ .`). NULL reads from context.
#' @param data Data frame or NULL (reads from context).
#' @param method Character. ML method to use:
#'   \describe{
#'     \item{"rf"}{Random Forest (ranger)}
#'     \item{"xgboost"}{Gradient Boosting (xgboost)}
#'     \item{"svm"}{Support Vector Machine (e1071)}
#'     \item{"nn"}{Neural Network (nnet - single hidden layer)}
#'     \item{"knn"}{K-Nearest Neighbors (class)}
#'     \item{"lasso"}{Lasso Regression (glmnet, alpha=1)}
#'     \item{"ridge"}{Ridge Regression (glmnet, alpha=0)}
#'     \item{"elastic"}{Elastic Net (glmnet, alpha=0.5)}
#'     \item{"tree"}{Decision Tree (rpart)}
#'     \item{"nb"}{Naive Bayes (e1071)}
#'     \item{"kmeans"}{K-Means Clustering (stats)}
#'     \item{"pca"}{Principal Component Analysis (stats)}
#'   }
#' @param ... Additional arguments passed to the underlying method.
#' @param name Character. Name for the model in context. Default "last".
#' @param seed Integer or NULL. Random seed. Default reads from context.
#'
#' @return The fitted model object (invisibly). Stored in context.
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#'
#' # Random Forest
#' qsr_ml(mpg ~ ., method = "rf")
#'
#' # XGBoost
#' qsr_ml(mpg ~ ., method = "xgboost")
#'
#' # Neural Network
#' qsr_ml(mpg ~ wt + hp, method = "nn", size = 10)
#'
#' # SVM
#' qsr_ml(am ~ wt + hp, method = "svm")
#'
#' # Clustering (no formula needed)
#' qsr_ml(method = "kmeans", centers = 3)
#'
#' # After fitting, everything works:
#' qsr_table()
#' qsr_plot(type = "importance")
#' }
#'
#' @export
qsr_ml <- function(formula = NULL,
                   data = NULL,
                   method = c("rf", "xgboost", "svm", "nn", "knn",
                              "lasso", "ridge", "elastic",
                              "tree", "nb", "kmeans", "pca"),
                   ...,
                   name = "last",
                   seed = NULL) {

  method <- match.arg(method)

  # Get data from context
  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) {
      cli::cli_abort("No data. Use {.fn qsr_data} first.")
    }
  }
  df <- as.data.frame(data)

  # Set seed
  seed <- seed %||% .qsr_context$get("random_seed") %||% 42
  set.seed(seed)

  # Unsupervised methods (no formula needed)
  if (method %in% c("kmeans", "pca")) {
    model <- switch(method,
      kmeans = .qsr_ml_kmeans(df, formula, ...),
      pca    = .qsr_ml_pca(df, formula, ...)
    )
    .qsr_context$set_model(model, name)
    return(invisible(model))
  }

  # Supervised methods need a formula
  if (is.null(formula)) {
    cli::cli_abort("A formula is required for {.val {method}} (e.g., {.code mpg ~ .}).")
  }

  # Dispatch to method
  model <- switch(method,
    rf      = .qsr_ml_rf(formula, df, ...),
    xgboost = .qsr_ml_xgboost(formula, df, ...),
    svm     = .qsr_ml_svm(formula, df, ...),
    nn      = .qsr_ml_nn(formula, df, ...),
    knn     = .qsr_ml_knn(formula, df, ...),
    lasso   = .qsr_ml_glmnet(formula, df, alpha = 1, ...),
    ridge   = .qsr_ml_glmnet(formula, df, alpha = 0, ...),
    elastic = .qsr_ml_glmnet(formula, df, alpha = 0.5, ...),
    tree    = .qsr_ml_tree(formula, df, ...),
    nb      = .qsr_ml_nb(formula, df, ...)
  )

  # Store in context
  .qsr_context$set_model(model, name)

  invisible(model)
}


# ============================================================
# Internal ML implementations
# ============================================================

#' @keywords internal
.qsr_ml_rf <- function(formula, df, ...) {
  .qsr_require("ranger", "for Random Forest")
  dots <- list(...)
  args <- c(list(formula = formula, data = df), dots)
  if (is.null(args$importance)) args$importance <- "impurity"
  model <- do.call(ranger::ranger, args)
  cli::cli_alert_success(
    "Model {.strong Random Forest}: {deparse(formula)} ({.val {model$num.trees}} trees)"
  )
  model
}

#' @keywords internal
.qsr_ml_xgboost <- function(formula, df, ...) {
  .qsr_require("xgboost", "for XGBoost")
  # Prepare matrix format
  mf <- stats::model.frame(formula, data = df, na.action = stats::na.pass)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]  # remove intercept

  dots <- list(...)
  nrounds <- dots$nrounds %||% 100
  dots$nrounds <- NULL
  objective <- dots$objective
  dots$objective <- NULL

  # Auto-detect objective
  if (is.null(objective)) {
    if (is.factor(y) || length(unique(y)) <= 10) {
      y_num <- as.integer(as.factor(y)) - 1L
      n_class <- length(unique(y_num))
      if (n_class == 2) {
        objective <- "binary:logistic"
      } else {
        objective <- "multi:softprob"
      }
    } else {
      y_num <- as.numeric(y)
      objective <- "reg:squarederror"
    }
  } else {
    y_num <- if (is.factor(y)) as.integer(y) - 1L else as.numeric(y)
  }

  dtrain <- xgboost::xgb.DMatrix(data = x, label = y_num)

  params <- c(list(objective = objective), dots)
  if (objective == "multi:softprob") {
    params$num_class <- length(unique(y_num))
  }

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )

  # Attach metadata for downstream use
  attr(model, "formula") <- formula
  attr(model, "feature_names") <- colnames(x)

  cli::cli_alert_success(
    "Model {.strong XGBoost}: {deparse(formula)} ({.val {nrounds}} rounds, {objective})"
  )
  model
}

#' @keywords internal
.qsr_ml_svm <- function(formula, df, ...) {
  .qsr_require("e1071", "for SVM")
  model <- e1071::svm(formula, data = df, ...)
  cli::cli_alert_success(
    "Model {.strong SVM}: {deparse(formula)} (kernel: {model$kernel})"
  )
  model
}

#' @keywords internal
.qsr_ml_nn <- function(formula, df, ...) {
  dots <- list(...)
  size <- dots$size %||% 5
  dots$size <- NULL
  maxit <- dots$maxit %||% 200
  dots$maxit <- NULL
  linout <- dots$linout

  # Auto-detect: regression vs classification
  y_var <- all.vars(formula)[1]
  y <- df[[y_var]]
  if (is.null(linout)) {
    linout <- is.numeric(y) && length(unique(y)) > 10
  }
  dots$linout <- NULL

  args <- c(list(formula = formula, data = df, size = size,
                 maxit = maxit, linout = linout, trace = FALSE), dots)
  model <- do.call(nnet::nnet, args)

  task <- if (linout) "regression" else "classification"
  cli::cli_alert_success(
    "Model {.strong Neural Network}: {deparse(formula)} ({.val {size}} hidden units, {task})"
  )
  model
}

#' @keywords internal
.qsr_ml_knn <- function(formula, df, ...) {
  .qsr_require("class", "for KNN")
  dots <- list(...)
  k <- dots$k %||% 5

  mf <- stats::model.frame(formula, data = df)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]

  # KNN needs to be wrapped since it predicts at call time
  model <- list(
    train_x = x,
    train_y = y,
    k = k,
    formula = formula,
    method = "knn"
  )
  class(model) <- "qsr_knn"

  cli::cli_alert_success(
    "Model {.strong KNN}: {deparse(formula)} (k={.val {k}})"
  )
  model
}

#' @keywords internal
.qsr_ml_glmnet <- function(formula, df, alpha = 1, ...) {
  .qsr_require("glmnet", "for Lasso/Ridge/Elastic Net")

  mf <- stats::model.frame(formula, data = df)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]

  method_name <- if (alpha == 1) "Lasso" else if (alpha == 0) "Ridge" else "Elastic Net"

  model <- glmnet::cv.glmnet(x, y, alpha = alpha, ...)

  attr(model, "formula") <- formula
  attr(model, "alpha") <- alpha

  cli::cli_alert_success(
    "Model {.strong {method_name}}: {deparse(formula)} (lambda.min={.val {round(model$lambda.min, 4)}})"
  )
  model
}

#' @keywords internal
.qsr_ml_tree <- function(formula, df, ...) {
  .qsr_require("rpart", "for Decision Trees")
  model <- rpart::rpart(formula, data = df, ...)
  cli::cli_alert_success(
    "Model {.strong Decision Tree}: {deparse(formula)} ({.val {nrow(model$frame)}} nodes)"
  )
  model
}

#' @keywords internal
.qsr_ml_nb <- function(formula, df, ...) {
  .qsr_require("e1071", "for Naive Bayes")
  model <- e1071::naiveBayes(formula, data = df, ...)
  cli::cli_alert_success(
    "Model {.strong Naive Bayes}: {deparse(formula)}"
  )
  model
}

#' @keywords internal
.qsr_ml_kmeans <- function(df, formula, ...) {
  dots <- list(...)
  centers <- dots$centers %||% 3
  dots$centers <- NULL

  # Select numeric columns
  if (!is.null(formula)) {
    vars <- all.vars(formula)
    num_df <- df[, vars, drop = FALSE]
  } else {
    num_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  }

  num_df <- stats::na.omit(num_df)
  scaled <- scale(num_df)

  model <- stats::kmeans(scaled, centers = centers, nstart = 25)
  attr(model, "columns") <- names(num_df)

  cli::cli_alert_success(
    "Model {.strong K-Means}: {.val {centers}} clusters on {.val {ncol(num_df)}} features"
  )
  model
}

#' @keywords internal
.qsr_ml_pca <- function(df, formula, ...) {
  if (!is.null(formula)) {
    vars <- all.vars(formula)
    num_df <- df[, vars, drop = FALSE]
  } else {
    num_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  }

  num_df <- stats::na.omit(num_df)
  model <- stats::prcomp(num_df, scale. = TRUE, ...)

  # Variance explained
  var_exp <- summary(model)$importance[2, ]
  cum_var <- summary(model)$importance[3, ]
  n_90 <- which(cum_var >= 0.9)[1]

  cli::cli_alert_success(
    "Model {.strong PCA}: {.val {ncol(num_df)}} features -> {.val {n_90}} components explain 90% variance"
  )
  model
}

#' @keywords internal
.qsr_require <- function(pkg, reason = "") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required {reason}.",
      "i" = 'Install it with: {.code install.packages("{pkg}")}'
    ))
  }
}


# ============================================================
# qsr_cv() - Cross-validation
# ============================================================

#' Cross-validate a model with one line
#'
#' Performs k-fold cross-validation on the last model or a specified method.
#' Reports accuracy (classification) or RMSE (regression).
#'
#' @param formula Formula or NULL (reads from context).
#' @param data Data frame or NULL (reads from context).
#' @param method Character. ML method (same as `qsr_ml()`).
#' @param k Integer. Number of folds. Default 10.
#' @param ... Additional arguments passed to the underlying method.
#'
#' @return A list with CV results (invisibly).
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#' qsr_cv(mpg ~ ., method = "rf", k = 5)
#' qsr_cv(am ~ wt + hp, method = "nn", size = 5, k = 10)
#' }
#'
#' @export
qsr_cv <- function(formula = NULL,
                   data = NULL,
                   method = c("rf", "xgboost", "svm", "nn",
                              "lasso", "ridge", "elastic", "tree"),
                   k = 10L,
                   ...) {

  method <- match.arg(method)

  if (is.null(data)) {
    data <- .qsr_context$get_data()
    if (is.null(data)) cli::cli_abort("No data. Use {.fn qsr_data} first.")
  }
  df <- as.data.frame(data)

  if (is.null(formula)) {
    model <- .qsr_context$get_model()
    if (!is.null(model) && !is.null(model$terms)) {
      formula <- formula(model)
    } else {
      cli::cli_abort("A formula is required for cross-validation. Pass one or use {.fn qsr_model} first.")
    }
  }

  seed <- .qsr_context$get("random_seed") %||% 42
  set.seed(seed)

  y_var <- all.vars(formula)[1]
  y <- df[[y_var]]
  is_classification <- is.factor(y) || is.character(y) || length(unique(y)) <= 10

  # Create folds
  n <- nrow(df)
  fold_ids <- sample(rep(seq_len(k), length.out = n))

  predictions <- rep(NA, n)
  actuals <- rep(NA, n)

  cli::cli_progress_bar("Cross-validating", total = k)

  for (i in seq_len(k)) {
    train_idx <- fold_ids != i
    test_idx <- fold_ids == i

    train_data <- df[train_idx, , drop = FALSE]
    test_data <- df[test_idx, , drop = FALSE]

    # Fit model on training fold
    model <- tryCatch(
      suppressMessages(
        qsr_ml(formula, data = train_data, method = method, name = ".cv_temp", ...)
      ),
      error = function(e) NULL
    )

    if (!is.null(model)) {
      # Predict on test fold
      preds <- tryCatch(
        .qsr_cv_predict(model, test_data, method, formula),
        error = function(e) rep(NA, sum(test_idx))
      )
      predictions[test_idx] <- preds
      actuals[test_idx] <- if (is_classification) as.integer(as.factor(y[test_idx])) else y[test_idx]
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # Calculate metrics
  valid <- !is.na(predictions) & !is.na(actuals)
  if (is_classification) {
    accuracy <- mean(predictions[valid] == actuals[valid])
    cli::cli_alert_success(
      "CV {.strong {method}} ({.val {k}}-fold): Accuracy = {.val {round(accuracy * 100, 1)}}%"
    )
    result <- list(method = method, k = k, metric = "accuracy", value = accuracy)
  } else {
    rmse <- sqrt(mean((predictions[valid] - actuals[valid])^2))
    r2 <- 1 - sum((actuals[valid] - predictions[valid])^2) / sum((actuals[valid] - mean(actuals[valid]))^2)
    cli::cli_alert_success(
      "CV {.strong {method}} ({.val {k}}-fold): RMSE = {.val {round(rmse, 4)}}, R\u00b2 = {.val {round(r2, 4)}}"
    )
    result <- list(method = method, k = k, metric = "rmse", rmse = rmse, r2 = r2)
  }

  invisible(result)
}


#' @keywords internal
.qsr_cv_predict <- function(model, newdata, method, formula) {
  mf <- stats::model.frame(formula, data = newdata, na.action = stats::na.pass)
  y <- stats::model.response(mf)

  if (method == "rf") {
    preds <- stats::predict(model, data = newdata)$predictions
    if (is.matrix(preds)) preds <- apply(preds, 1, which.max)
  } else if (method == "xgboost") {
    x <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]
    dtest <- xgboost::xgb.DMatrix(data = x)
    preds <- stats::predict(model, dtest)
    if (grepl("binary", model$params$objective %||% "")) {
      preds <- round(preds)
    }
  } else if (method %in% c("lasso", "ridge", "elastic")) {
    x <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]
    preds <- as.numeric(stats::predict(model, newx = x, s = "lambda.min"))
  } else {
    preds <- stats::predict(model, newdata = newdata)
    if (is.factor(preds)) preds <- as.integer(preds)
  }

  as.numeric(preds)
}


# ============================================================
# qsr_compare() - Model comparison
# ============================================================

#' Compare multiple models side by side
#'
#' Compares all models stored in context (or specified models) using
#' standard metrics: AIC, BIC, R2, RMSE, accuracy.
#'
#' @param ... Named model objects. If empty, uses all models from context.
#'
#' @return A data.frame comparison table (invisibly). Also printed.
#'
#' @examples
#' \dontrun{
#' qsr_data(mtcars)
#' qsr_model(mpg ~ wt, name = "simple")
#' qsr_model(mpg ~ wt + hp, name = "medium")
#' qsr_model(mpg ~ wt + hp + disp, name = "full")
#' qsr_compare()
#' }
#'
#' @export
qsr_compare <- function(...) {
  models <- list(...)

  # If no models provided, get all from context
  if (length(models) == 0) {
    models <- .qsr_context$get_models()
    if (length(models) == 0) {
      cli::cli_abort("No models to compare. Fit models with {.fn qsr_model} or {.fn qsr_ml} first.")
    }
  }

  # Build comparison table
  rows <- list()
  for (nm in names(models)) {
    m <- models[[nm]]
    row <- list(model = nm)

    # Extract metrics based on model type
    if (inherits(m, "lm") || inherits(m, "glm")) {
      s <- summary(m)
      row$type <- class(m)[1]
      row$r_squared <- if (!is.null(s$r.squared)) round(s$r.squared, 4) else NA
      row$adj_r_squared <- if (!is.null(s$adj.r.squared)) round(s$adj.r.squared, 4) else NA
      row$aic <- round(stats::AIC(m), 1)
      row$bic <- round(stats::BIC(m), 1)
      row$rmse <- round(sqrt(mean(s$residuals^2)), 4)
    } else if (inherits(m, "ranger")) {
      row$type <- "Random Forest"
      row$r_squared <- if (!is.null(m$r.squared)) round(m$r.squared, 4) else NA
      row$adj_r_squared <- NA
      row$aic <- NA
      row$bic <- NA
      row$rmse <- if (!is.null(m$prediction.error)) round(sqrt(m$prediction.error), 4) else NA
    } else if (inherits(m, "xgb.Booster")) {
      row$type <- "XGBoost"
      row$r_squared <- NA
      row$adj_r_squared <- NA
      row$aic <- NA
      row$bic <- NA
      row$rmse <- NA
    } else if (inherits(m, "nnet")) {
      row$type <- "Neural Network"
      row$r_squared <- NA
      row$adj_r_squared <- NA
      row$aic <- NA
      row$bic <- NA
      row$rmse <- NA
    } else {
      row$type <- class(m)[1]
      row$r_squared <- NA
      row$adj_r_squared <- NA
      row$aic <- tryCatch(round(stats::AIC(m), 1), error = function(e) NA)
      row$bic <- tryCatch(round(stats::BIC(m), 1), error = function(e) NA)
      row$rmse <- NA
    }

    rows <- c(rows, list(row))
  }

  comparison <- do.call(rbind, lapply(rows, as.data.frame))

  cli::cli_h3("Model Comparison")
  print(comparison, row.names = FALSE)

  invisible(comparison)
}
