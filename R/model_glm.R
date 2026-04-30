# Presence/background GLM fitting and diagnostics.

sample_background_points <- function(env_train_scaled, n, seed = 42, presence_xy = NULL) {
  set.seed(seed)
  n <- as.integer(n)
  if (is.na(n) || n < 100) n <- 100

  presence_cells <- integer()
  if (!is.null(presence_xy) && nrow(presence_xy) > 0) {
    presence_cells <- tryCatch(terra::cellFromXY(env_train_scaled[[1]], presence_xy), error = function(e) integer())
    presence_cells <- unique(stats::na.omit(as.integer(presence_cells)))
  }

  sample_from <- function(r, size, check_values = FALSE) {
    size <- min(as.integer(size), terra::ncell(r))
    if (is.na(size) || size < 1) return(NULL)
    pts <- try(terra::spatSample(r, size = size, method = "random", na.rm = TRUE,
                                 xy = TRUE, cells = TRUE, as.points = FALSE,
                                 values = check_values), silent = TRUE)
    if (inherits(pts, "try-error") || is.null(pts)) return(NULL)
    pts <- as.data.frame(pts)
    if (nrow(pts) == 0 || !all(c("x", "y") %in% names(pts))) return(NULL)
    value_cols <- setdiff(names(pts), c("cell", "x", "y"))
    if (length(value_cols) > 0) pts <- pts[stats::complete.cases(pts[, value_cols, drop = FALSE]), , drop = FALSE]
    if ("cell" %in% names(pts) && length(presence_cells) > 0) {
      pts <- pts[!(pts$cell %in% presence_cells), , drop = FALSE]
    }
    pts
  }

  pts <- sample_from(env_train_scaled, max(n, ceiling(n * 1.25)), check_values = TRUE)

  if (is.null(pts) || nrow(pts) < n) {
    valid_mask <- try(terra::app(env_train_scaled, fun = function(...) all(is.finite(c(...)))), silent = TRUE)
    if (!inherits(valid_mask, "try-error")) {
      valid_mask <- terra::ifel(valid_mask, 1, NA)
      if (length(presence_cells) > 0) valid_mask[presence_cells] <- NA
      pts <- sample_from(valid_mask, n)
    }
  }

  if (is.null(pts) || nrow(pts) == 0) {
    stop("No valid background cells available in the training raster.", call. = FALSE)
  }
  if (nrow(pts) > n) pts <- pts[sample.int(nrow(pts), n), , drop = FALSE]
  xy <- pts[, c("x", "y"), drop = FALSE]
  if (nrow(xy) < n) {
    warning("Sampled fewer background points than requested after excluding incomplete and presence cells.", call. = FALSE)
  }
  xy
}

extract_covariates <- function(r, xy) {
  vals <- terra::extract(r, xy)
  if ("ID" %in% names(vals)) vals <- vals[, setdiff(names(vals), "ID"), drop = FALSE]
  vals
}

class_balance_weights <- function(y) {
  y <- as.integer(y)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  n <- length(y)
  if (n1 == 0 || n0 == 0) return(rep(1, n))
  ifelse(y == 1, n / (2 * n1), n / (2 * n0))
}

make_sdm_formula <- function(covariates, include_quadratic = TRUE) {
  covariates <- make.names(covariates)
  terms <- covariates
  if (include_quadratic) terms <- c(terms, sprintf("I(%s^2)", covariates))
  stats::as.formula(paste("presence ~", paste(terms, collapse = " + ")))
}

auc_rank <- function(obs, score) {
  ok <- is.finite(obs) & is.finite(score)
  obs <- obs[ok]
  score <- score[ok]
  n1 <- sum(obs == 1)
  n0 <- sum(obs == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score, ties.method = "average")
  as.numeric((sum(r[obs == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0))
}

cross_validate_glm <- function(model_data, formula, k = 3, seed = 42, n_cores = 1) {
  k <- as.integer(k)
  if (is.na(k) || k < 2) return(list(k = 0, auc_mean = NA_real_, auc_sd = NA_real_, fold_auc = numeric()))
  k <- min(k, sum(model_data$presence == 1), sum(model_data$presence == 0))
  if (k < 2) return(list(k = 0, auc_mean = NA_real_, auc_sd = NA_real_, fold_auc = numeric()))
  n_cores <- min(normalize_core_count(n_cores), k)

  set.seed(seed)
  fold_id <- integer(nrow(model_data))
  pres_idx <- which(model_data$presence == 1)
  bg_idx <- which(model_data$presence == 0)
  fold_id[pres_idx] <- sample(rep(seq_len(k), length.out = length(pres_idx)))
  fold_id[bg_idx] <- sample(rep(seq_len(k), length.out = length(bg_idx)))

  fit_one_fold <- function(i, model_data_arg, fold_id_arg, formula_arg) {
    train <- model_data_arg[fold_id_arg != i, , drop = FALSE]
    test <- model_data_arg[fold_id_arg == i, , drop = FALSE]
    y <- as.integer(train$presence)
    n1 <- sum(y == 1)
    n0 <- sum(y == 0)
    n <- length(y)
    w <- if (n1 == 0 || n0 == 0) rep(1, n) else ifelse(y == 1, n / (2 * n1), n / (2 * n0))
    train$case_weight_sdm <- w
    fit <- stats::glm(formula_arg, data = train, family = stats::binomial(), weights = case_weight_sdm, control = stats::glm.control(maxit = 60))
    pred <- stats::predict(fit, newdata = test, type = "response")
    obs <- test$presence
    ok <- is.finite(obs) & is.finite(pred)
    obs <- obs[ok]
    pred <- pred[ok]
    n1t <- sum(obs == 1)
    n0t <- sum(obs == 0)
    if (n1t == 0 || n0t == 0) return(NA_real_)
    ranks <- rank(pred, ties.method = "average")
    as.numeric((sum(ranks[obs == 1]) - n1t * (n1t + 1) / 2) / (n1t * n0t))
  }

  run_single_core_cv <- function() {
    vapply(seq_len(k), fit_one_fold, numeric(1), model_data_arg = model_data, fold_id_arg = fold_id, formula_arg = formula)
  }

  fold_auc <- if (n_cores > 1 && k > 1) {
    parallel_result <- tryCatch({
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      unlist(parallel::parLapply(cl, seq_len(k), fit_one_fold, model_data_arg = model_data, fold_id_arg = fold_id, formula_arg = formula), use.names = FALSE)
    }, error = function(e) e)
    if (inherits(parallel_result, "error")) {
      warning("Parallel cross-validation failed; falling back to single-core CV: ", conditionMessage(parallel_result), call. = FALSE)
      run_single_core_cv()
    } else {
      parallel_result
    }
  } else {
    run_single_core_cv()
  }

  list(k = k, auc_mean = mean(fold_auc, na.rm = TRUE), auc_sd = stats::sd(fold_auc, na.rm = TRUE), fold_auc = fold_auc)
}

fit_fast_sdm <- function(occ, env_train_scaled, background_n = sdm_default_background_n, include_quadratic = TRUE,
                         cv_folds = 3, seed = 42, n_cores = 1, log_fun = NULL) {
  covariates <- names(env_train_scaled)
  if (length(covariates) < 2) stop("At least two covariates are required for modelling.", call. = FALSE)

  pres_xy <- occ[, c("longitude", "latitude"), drop = FALSE]
  names(pres_xy) <- c("x", "y")
  pres_vals <- extract_covariates(env_train_scaled, pres_xy)
  pres_keep <- stats::complete.cases(pres_vals)
  if (sum(!pres_keep) > 0) log_message(log_fun, "Dropped ", sum(!pres_keep), " occurrence records with missing covariates")
  pres_vals <- pres_vals[pres_keep, , drop = FALSE]
  occ_used <- occ[pres_keep, , drop = FALSE]
  if (nrow(pres_vals) < 20) stop("Too few presence records with complete environmental data.", call. = FALSE)

  bg_xy <- sample_background_points(env_train_scaled, background_n, seed = seed, presence_xy = pres_xy[pres_keep, , drop = FALSE])
  bg_vals <- extract_covariates(env_train_scaled, bg_xy)
  bg_keep <- stats::complete.cases(bg_vals)
  bg_vals <- bg_vals[bg_keep, , drop = FALSE]
  bg_xy <- bg_xy[bg_keep, , drop = FALSE]
  if (nrow(bg_vals) < 100) stop("Too few background points could be sampled.", call. = FALSE)

  model_data <- rbind(
    data.frame(presence = 1L, pres_vals, check.names = FALSE),
    data.frame(presence = 0L, bg_vals, check.names = FALSE)
  )
  names(model_data) <- make.names(names(model_data))
  covariates <- make.names(covariates)
  formula <- make_sdm_formula(covariates, include_quadratic = include_quadratic)
  environment(formula) <- baseenv()

  log_message(log_fun, "Fitting fast GLM SDM with ", nrow(pres_vals), " presences and ", nrow(bg_vals), " background points")
  model_data$case_weight_sdm <- class_balance_weights(model_data$presence)
  model <- stats::glm(formula, data = model_data, family = stats::binomial(), weights = case_weight_sdm, control = stats::glm.control(maxit = 80))

  cv <- cross_validate_glm(model_data, formula, k = cv_folds, seed = seed, n_cores = n_cores)
  if (is.finite(cv$auc_mean)) {
    log_message(log_fun, "Cross-validation AUC: ", sprintf("%.3f", cv$auc_mean), if (is.finite(cv$auc_sd)) paste0(" +/- ", sprintf("%.3f", cv$auc_sd)) else "")
  }

  coefficients <- as.data.frame(summary(model)$coefficients)
  coefficients$term <- rownames(coefficients)
  rownames(coefficients) <- NULL
  coefficients <- coefficients[, c("term", setdiff(names(coefficients), "term")), drop = FALSE]

  model$model <- NULL
  model$data <- NULL
  model$y <- NULL
  model$formula <- formula
  if (!is.null(model$formula)) environment(model$formula) <- baseenv()
  if (!is.null(model$terms)) attr(model$terms, ".Environment") <- baseenv()
  model$call <- base::call("glm", formula = formula, family = stats::binomial())

  list(model = model, formula = formula, coefficients = coefficients, model_data = model_data,
       occurrence_used = occ_used, background_xy = bg_xy, cv = cv, covariates = covariates)
}
