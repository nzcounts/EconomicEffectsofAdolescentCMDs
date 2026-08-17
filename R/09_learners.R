# Generated from the reviewed v8.28 production source.
# Original lines: 5929-6679.
# Module role: Super Learner registry.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 7) LEARNER REGISTRY FOR FINAL TMLE
# =============================================================================
# Plain-English role: decide which machine-learning algorithms the final
# TMLE stage will stack together to form the outcome (Q), treatment (g), and
# outcome-missingness (pi) estimators. We keep the library intentionally
# lean: mean, GLM, ridge/lasso, random forest, gradient boosting, and
# (optionally for Q) multivariate adaptive regression splines. Every learner
# is forced to single-threaded operation so parallelism cannot exhaust RAM.
# The SuperLearner meta-learner picks the convex combination of these that
# minimizes held-out loss inside each training fold. Cross-fitting then
# prevents the resulting Q, g, pi from overfitting the target parameter.

.SL_RUNTIME_ENV <- new.env(parent = emptyenv())

set_sl_glmnet_context <- function(target, seed, nfolds = 5L) {
  target <- match.arg(target, c("Q", "g", "pi"))
  .SL_RUNTIME_ENV$glmnet_context <- list(
    target = target, seed = as.integer(seed), nfolds = as.integer(nfolds),
    h1fs_processed_columns = .SL_RUNTIME_ENV$h1fs_processed_columns %||% character(0))
  invisible(NULL)
}

clear_sl_glmnet_context <- function() {
  if (exists("glmnet_context", envir = .SL_RUNTIME_ENV, inherits = FALSE))
    rm("glmnet_context", envir = .SL_RUNTIME_ENV)
  invisible(NULL)
}

with_sl_glmnet_context <- function(target, seed, nfolds, expr) {
  had_context <- exists("glmnet_context", envir = .SL_RUNTIME_ENV, inherits = FALSE)
  old_context <- if (had_context)
    get("glmnet_context", envir = .SL_RUNTIME_ENV, inherits = FALSE) else NULL
  set_sl_glmnet_context(target, seed, nfolds)
  on.exit({
    if (had_context) assign("glmnet_context", old_context, envir = .SL_RUNTIME_ENV)
    else clear_sl_glmnet_context()
  }, add = TRUE)
  force(expr)
}


validate_cv_glmnet_fit <- function(cvfit, selected_lambda = NULL,
                                   expected_ncol = NULL, label = "cv.glmnet") {
  if (is.null(cvfit) || !inherits(cvfit, "cv.glmnet"))
    stop(label, " did not return a cv.glmnet object.", call. = FALSE)
  lam_path <- as.numeric(cvfit$lambda)
  cvm <- as.numeric(cvfit$cvm)
  if (!length(lam_path) || length(cvm) != length(lam_path) ||
      any(!is.finite(lam_path)) || any(lam_path <= 0) ||
      sum(is.finite(cvm)) < 2L)
    stop(label, " returned an invalid lambda path or CV risk curve.", call. = FALSE)
  if (is.null(selected_lambda)) selected_lambda <- cvfit$lambda.min
  selected_lambda <- as.numeric(selected_lambda)[1L]
  if (!is.finite(selected_lambda) || selected_lambda <= 0 ||
      selected_lambda < min(lam_path) * (1 - 1e-8) ||
      selected_lambda > max(lam_path) * (1 + 1e-8))
    stop(label, " returned an invalid selected lambda.", call. = FALSE)
  if (!is.null(expected_ncol)) {
    b <- tryCatch(as.numeric(stats::coef(cvfit, s = selected_lambda)),
                  error = function(e) e)
    if (inherits(b, "error") || length(b) != as.integer(expected_ncol) + 1L ||
        any(!is.finite(b)))
      stop(label, " returned an invalid coefficient vector at the selected lambda.",
           call. = FALSE)
  }
  jerr <- suppressWarnings(as.integer(cvfit$glmnet.fit$jerr %||% 0L))[1L]
  if (is.finite(jerr) && jerr > 0L)
    stop(sprintf("%s encountered fatal glmnet error code %d.", label, jerr),
         call. = FALSE)
  if (is.finite(jerr) && jerr < 0L)
    warning(sprintf(paste0("%s returned nonfatal glmnet path code %d; the selected lambda and ",
                           "coefficient vector were verified within the returned path."),
                    label, jerr), call. = FALSE)
  invisible(list(lambda = selected_lambda, jerr = jerr,
                 n_lambda = length(lam_path), n_finite_cvm = sum(is.finite(cvm))))
}

summarize_prediction_clipping <- function(pred, lower, upper, fold,
                                                  prediction_set) {
  pred <- as.numeric(pred)
  if (!length(pred) || any(!is.finite(pred)))
    stop("Q clipping summary requires nonempty finite predictions.", call. = FALSE)
  clipped <- pmin(pmax(pred, lower), upper)
  below <- pred < lower
  above <- pred > upper
  disp <- abs(clipped - pred)
  data.frame(
    fold = as.integer(fold),
    prediction_set = as.character(prediction_set),
    n_predictions = length(pred),
    raw_min = min(pred),
    raw_max = max(pred),
    n_below = sum(below),
    n_above = sum(above),
    n_clipped = sum(below | above),
    fraction_clipped = mean(below | above),
    mean_absolute_clipping_displacement = mean(disp),
    max_absolute_clipping_displacement = max(disp),
    lower_bound = lower,
    upper_bound = upper,
    stringsAsFactors = FALSE
  )
}

register_custom_learners <- function(cfg) {
  lcfg <- cfg$learners
  # Random forest with num.threads = 1L to avoid nested parallelism.
  if (isTRUE(lcfg$Q$use_ranger) || isTRUE(lcfg$g$use_ranger) || isTRUE(lcfg$pi$use_ranger)) {
    assign("SL.ranger.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        X <- as.data.frame(X, check.names = FALSE)
        newX <- as.data.frame(newX, check.names = FALSE)
        newX <- newX[, names(X), drop = FALSE]
        family_name <- family$family
        y_fit <- if (identical(family_name, "binomial"))
          factor(as.integer(Y), levels = c(0L, 1L)) else as.numeric(Y)
        rf <- ranger::ranger(
          y = y_fit, x = X, num.trees = lcfg$ranger$num.trees,
          probability = identical(family_name, "binomial"),
          case.weights = normalize_positive_weights(obsWeights, length(Y), "SL.ranger.fixed weights"),
          num.threads = 1L)
        pred_obj <- stats::predict(rf, data = newX, num.threads = 1L)$predictions
        pred <- if (identical(family_name, "binomial")) {
          if (!is.matrix(pred_obj) || !"1" %in% colnames(pred_obj))
            stop("SL.ranger.fixed did not return a named event-probability column '1'.", call. = FALSE)
          pred_obj[, "1"]
        } else as.numeric(pred_obj)
        fit <- list(object = rf, family_name = family_name, train_names = names(X))
        class(fit) <- "SL.ranger.fixed"
        list(pred = as.numeric(pred), fit = fit)
      }, envir = .GlobalEnv)
    assign("predict.SL.ranger.fixed", function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      pr <- stats::predict(object$object, data = newdata, num.threads = 1L)$predictions
      if (identical(object$family_name, "binomial")) {
        if (!is.matrix(pr) || !"1" %in% colnames(pr)) stop("Ranger event column '1' missing.", call. = FALSE)
        pr <- pr[, "1"]
      }
      as.numeric(pr)
    }, envir = .GlobalEnv)
  }
  # Gradient boosting with nthread = 1 for the same reason.
  if (isTRUE(lcfg$Q$use_xgboost) || isTRUE(lcfg$g$use_xgboost) || isTRUE(lcfg$pi$use_xgboost)) {
    assign("SL.xgboost.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        X <- as.data.frame(X, check.names = FALSE); newX <- as.data.frame(newX, check.names = FALSE)
        newX <- newX[, names(X), drop = FALSE]
        Xm <- data.matrix(X); newXm <- data.matrix(newX)
        storage.mode(Xm) <- "double"; storage.mode(newXm) <- "double"
        if (any(!is.finite(Xm)) || any(!is.finite(newXm)))
          stop("SL.xgboost.fixed received non-finite predictors.", call. = FALSE)
        xgb_w <- normalize_positive_weights(obsWeights, length(Y), "SL.xgboost.fixed weights")
        family_name <- family$family
        dtrain <- xgboost::xgb.DMatrix(Xm, label = Y, weight = xgb_w)
        fit_obj <- xgboost::xgb.train(
          params = list(
            eta = lcfg$xgboost$shrinkage, max_depth = lcfg$xgboost$max_depth,
            min_child_weight = lcfg$xgboost$min_child_weight %||% 20, nthread = 1L,
            objective = if (family_name == "binomial") "binary:logistic" else "reg:squarederror",
            eval_metric = if (family_name == "binomial") "logloss" else "rmse"),
          data = dtrain, nrounds = lcfg$xgboost$ntrees, verbose = 0)
        pred <- stats::predict(fit_obj, newdata = xgboost::xgb.DMatrix(newXm))
        fit <- list(object = fit_obj, train_names = names(X), family_name = family_name)
        class(fit) <- "SL.xgboost.fixed"
        list(pred = as.numeric(pred), fit = fit)
      }, envir = .GlobalEnv)
    assign("predict.SL.xgboost.fixed", function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      Xm <- data.matrix(newdata); storage.mode(Xm) <- "double"
      if (any(!is.finite(Xm))) stop("XGBoost prediction data are non-finite.", call. = FALSE)
      as.numeric(stats::predict(object$object, newdata = xgboost::xgb.DMatrix(Xm)))
    }, envir = .GlobalEnv)
  }
  # Separate Q-only richer booster for the designated sensitivity arm. It has
  # its own configuration and learner name, so g and pi remain unchanged.
  if (isTRUE(lcfg$Q$use_xgboost_rich %||% FALSE)) {
    assign("SL.xgboost.rich",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        X <- as.data.frame(X, check.names = FALSE)
        newX <- as.data.frame(newX, check.names = FALSE)
        missing_new <- setdiff(names(X), names(newX))
        if (length(missing_new))
          stop("SL.xgboost.rich prediction data are missing columns: ",
               paste(missing_new, collapse = ", "), call. = FALSE)
        newX <- newX[, names(X), drop = FALSE]
        Xm <- data.matrix(X); newXm <- data.matrix(newX)
        storage.mode(Xm) <- "double"; storage.mode(newXm) <- "double"
        if (any(!is.finite(Xm)) || any(!is.finite(newXm)))
          stop("SL.xgboost.rich received non-finite predictors.", call. = FALSE)
        xgb_w <- normalize_positive_weights(
          obsWeights, length(Y), "SL.xgboost.rich weights")
        family_name <- family$family
        dtrain <- xgboost::xgb.DMatrix(Xm, label = Y, weight = xgb_w)
        fit_obj <- xgboost::xgb.train(
          params = list(
            eta = lcfg$xgboost_rich$shrinkage,
            max_depth = as.integer(lcfg$xgboost_rich$max_depth),
            min_child_weight = lcfg$xgboost_rich$min_child_weight,
            nthread = 1L,
            objective = if (family_name == "binomial")
              "binary:logistic" else "reg:squarederror",
            eval_metric = if (family_name == "binomial") "logloss" else "rmse"),
          data = dtrain,
          nrounds = as.integer(lcfg$xgboost_rich$ntrees),
          verbose = 0)
        pred <- stats::predict(fit_obj, newdata = xgboost::xgb.DMatrix(newXm))
        if (length(pred) != nrow(newX) || any(!is.finite(pred)))
          stop("SL.xgboost.rich produced invalid predictions.", call. = FALSE)
        fit <- list(object = fit_obj, train_names = names(X),
                    family_name = family_name)
        class(fit) <- "SL.xgboost.rich"
        list(pred = as.numeric(pred), fit = fit)
      }, envir = .GlobalEnv)
    assign("predict.SL.xgboost.rich", function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      missing_new <- setdiff(object$train_names, names(newdata))
      if (length(missing_new))
        stop("predict.SL.xgboost.rich missing columns: ",
             paste(missing_new, collapse = ", "), call. = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      Xm <- data.matrix(newdata); storage.mode(Xm) <- "double"
      if (any(!is.finite(Xm)))
        stop("SL.xgboost.rich prediction data are non-finite.", call. = FALSE)
      pred <- stats::predict(object$object, newdata = xgboost::xgb.DMatrix(Xm))
      if (length(pred) != nrow(newdata) || any(!is.finite(pred)))
        stop("predict.SL.xgboost.rich produced invalid predictions.", call. = FALSE)
      as.numeric(pred)
    }, envir = .GlobalEnv)
  }
  if (isTRUE(lcfg$Q$use_earth)) {
    assign("SL.earth.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        # v6 Fix A: earth rebuilds the call via a formula interface in some
        # versions, which calls model.frame and requires a data.frame input.
        # Force X and newX to data.frame and ensure all columns are numeric.
        if (!is.data.frame(X))    X    <- as.data.frame(X,    stringsAsFactors = FALSE)
        if (!is.data.frame(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
        X[]    <- lapply(X,    function(c) if (is.numeric(c)) c else suppressWarnings(as.numeric(as.character(c))))
        newX[] <- lapply(newX, function(c) if (is.numeric(c)) c else suppressWarnings(as.numeric(as.character(c))))
        if (any(!is.finite(as.matrix(X))) || any(!is.finite(as.matrix(newX))) || any(!is.finite(Y)))
          stop("SL.earth.fixed received non-finite data.", call. = FALSE)
        earth_w <- normalize_positive_weights(obsWeights, length(Y), "SL.earth.fixed weights")
        earth_obj <- earth::earth(x = X, y = Y, weights = earth_w,
          glm = if (family$family == "binomial") list(family = stats::binomial()) else NULL,
          degree = lcfg$earth$degree %||% 2, nprune = lcfg$earth$nprune)
        pred <- stats::predict(earth_obj, newdata = newX,
          type = if (family$family == "binomial") "response" else "link")
        fit <- list(object = earth_obj, train_names = names(X),
                    family_name = family$family)
        class(fit) <- "SL.earth.fixed"
        list(pred = as.numeric(pred), fit = fit)
      }, envir = .GlobalEnv)
    assign("predict.SL.earth.fixed", function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      as.numeric(stats::predict(object$object, newdata = newdata,
        type = if (object$family_name == "binomial") "response" else "link"))
    }, envir = .GlobalEnv)
  }
  # BUG FIX: stock SL.glmnet hardcodes alpha = 1 (pure LASSO) and ignores
  # whatever is in cfg$learners$glmnet$alpha. With ~70% of columns being
  # dummy-expanded factors, pure LASSO arbitrarily picks one dummy per
  # correlated cluster and zeros the rest, producing high cross-fold variance.
  # This wrapper passes alpha and nlambda from cfg through to cv.glmnet so
  # the elastic-net configuration in cfg$learners$glmnet actually takes effect.
  if (isTRUE(lcfg$Q$use_glmnet) || isTRUE(lcfg$g$use_glmnet) || isTRUE(lcfg$pi$use_glmnet)) {
    assign("SL.glmnet.fixed",
      function(Y, X, newX, family, obsWeights = NULL, id = NULL, ...) {
        if (!is.data.frame(X))    X    <- as.data.frame(X,    stringsAsFactors = FALSE)
        if (!is.data.frame(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
        # glmnet wants a numeric matrix; model.matrix expands any residual
        # factors to dummies. With Fix 2 the data has already been
        # numerically expanded upstream, so this is mostly a no-op, but
        # remains for safety in case factor columns slip through.
        # Defensive: coerce NAs to zero (numeric) or "Missing" (factor) so
        # model.matrix's default na.action doesn't silently drop rows.
        for (cn in names(X)) {
          col <- X[[cn]]
          if (is.numeric(col)) {
            col[!is.finite(col)] <- 0
            X[[cn]] <- col
          } else if (is.factor(col)) {
            if (any(is.na(col))) {
              col <- addNA(col)
              levels(col)[is.na(levels(col))] <- "Missing"
              X[[cn]] <- col
            }
          }
        }
        for (cn in names(newX)) {
          col <- newX[[cn]]
          if (is.numeric(col)) {
            col[!is.finite(col)] <- 0
            newX[[cn]] <- col
          } else if (is.factor(col)) {
            if (any(is.na(col))) {
              col <- addNA(col)
              levels(col)[is.na(levels(col))] <- "Missing"
              newX[[cn]] <- col
            }
          }
        }
        Xm    <- stats::model.matrix(~ . - 1, data = X,    na.action = stats::na.pass)
        newXm <- stats::model.matrix(~ . - 1, data = newX, na.action = stats::na.pass)
        if (nrow(Xm) != nrow(X) || nrow(newXm) != nrow(newX)) {
          stop(sprintf("SL.glmnet.fixed model.matrix row mismatch: train %d/%d, valid %d/%d.",
                       nrow(Xm), nrow(X), nrow(newXm), nrow(newX)), call. = FALSE)
        }
        # Align newXm columns to Xm columns. If validation is missing a
        # training column (factor level not seen in validation), fill it
        # with 0. If validation has an extra column not in training, drop
        # it (the model can't use it).
        miss_cols <- setdiff(colnames(Xm), colnames(newXm))
        if (length(miss_cols) > 0L) {
          fill <- matrix(0, nrow = nrow(newXm), ncol = length(miss_cols),
                         dimnames = list(NULL, miss_cols))
          newXm <- cbind(newXm, fill)
        }
        extra_cols <- setdiff(colnames(newXm), colnames(Xm))
        if (length(extra_cols) > 0L)
          newXm <- newXm[, !colnames(newXm) %in% extra_cols, drop = FALSE]
        newXm <- newXm[, colnames(Xm), drop = FALSE]
        if (ncol(Xm) == 1L) {
          Xm <- cbind(Xm, .glmnet_pad = 0)
          newXm <- cbind(newXm, .glmnet_pad = 0)
        }
        if (is.null(id) || length(id) != length(Y))
          stop("SL.glmnet.fixed requires the cluster id vector.", call. = FALSE)
        if (!exists("glmnet_context", envir = .SL_RUNTIME_ENV, inherits = FALSE))
          stop("SL.glmnet.fixed has no externally supplied tuning context.", call. = FALSE)
        ctx <- get("glmnet_context", envir = .SL_RUNTIME_ENV, inherits = FALSE)
        ids <- as.character(id)
        w_glmnet <- normalize_positive_weights(obsWeights, length(Y), "SL.glmnet.fixed weights")
        if (identical(ctx$target, "Q")) {
          if (!"A" %in% names(X)) stop("SL.glmnet.fixed Q tuning requires A in X.", call. = FALSE)
          A_tune <- normalize_binary_var(X$A, "SL.glmnet.fixed Q treatment")
          delta_tune <- rep(1L, length(Y))
        } else if (identical(ctx$target, "g")) {
          A_tune <- normalize_binary_var(Y, "SL.glmnet.fixed g outcome")
          delta_tune <- rep(1L, length(Y))
        } else {
          if (!"A" %in% names(X)) stop("SL.glmnet.fixed pi tuning requires A in X.", call. = FALSE)
          A_tune <- normalize_binary_var(X$A, "SL.glmnet.fixed pi treatment")
          delta_tune <- normalize_binary_var(Y, "SL.glmnet.fixed pi outcome")
        }
        foldid_glmnet <- do.call(make_cluster_folds_balanced, c(list(
          cluster = ids, A = A_tune, k = ctx$nfolds,
          seed = ctx$seed, weights = w_glmnet, delta = delta_tune,
          balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights)),
          fold_control_from_cfg(cfg, "internal")))
        if (length(unique(foldid_glmnet)) < 3L)
          stop("SL.glmnet.fixed requires at least three cluster-aware tuning folds in every SuperLearner training subset.", call. = FALSE)
        x_glmnet <- if (requireNamespace("Matrix", quietly = TRUE)) Matrix::Matrix(Xm, sparse = TRUE) else Xm
        fit <- glmnet::cv.glmnet(
          x = x_glmnet, y = Y, family = family$family,
          alpha = lcfg$glmnet$alpha %||% 1,
          nlambda = lcfg$glmnet$nlambda %||% 100L,
          weights = w_glmnet, foldid = foldid_glmnet,
          standardize = isTRUE(lcfg$glmnet$standardize %||% TRUE),
          maxit = lcfg$glmnet$maxit %||% 100000L)
        # lambda choice: "min" (default) or "1se"
        lam <- if (identical(lcfg$glmnet$lambda_choice %||% "min", "1se"))
                 fit$lambda.1se else fit$lambda.min
        validate_cv_glmnet_fit(fit, lam, expected_ncol = ncol(Xm),
                               label = paste0("SL.glmnet.fixed[", ctx$target, "]"))
        pred <- stats::predict(fit, newx = newXm, s = lam,
          type = if (family$family == "binomial") "response" else "link")
        pred <- as.numeric(pred)
        if (length(pred) != nrow(newXm) || any(!is.finite(pred)))
          stop("SL.glmnet.fixed produced invalid predictions.", call. = FALSE)
        if (family$family == "binomial" && any(pred < 0 | pred > 1))
          stop("SL.glmnet.fixed produced binomial predictions outside [0,1].", call. = FALSE)
        fit_out <- list(object = fit, lambda = lam, columns = colnames(Xm),
                        train_names = names(X), family_name = family$family)
        class(fit_out) <- "SL.glmnet.fixed"
        list(pred = pred, fit = fit_out)
      }, envir = .GlobalEnv)
    assign("predict.SL.glmnet.fixed", function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      missing_raw <- setdiff(object$train_names, names(newdata))
      if (length(missing_raw)) stop("predict.SL.glmnet.fixed missing raw columns: ", paste(missing_raw, collapse = ", "), call. = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      for (cn in names(newdata)) {
        col <- newdata[[cn]]
        if (is.numeric(col)) {
          col[!is.finite(col)] <- 0
          newdata[[cn]] <- col
        } else if (is.factor(col) && anyNA(col)) {
          col <- addNA(col)
          levels(col)[is.na(levels(col))] <- "Missing"
          newdata[[cn]] <- col
        }
      }
      mm <- stats::model.matrix(~ . - 1, data = newdata, na.action = stats::na.pass)
      if (nrow(mm) != nrow(newdata) || any(!is.finite(mm)))
        stop("predict.SL.glmnet.fixed produced an invalid model matrix.", call. = FALSE)
      missing_cols <- setdiff(object$columns, colnames(mm))
      if (length(missing_cols)) {
        add <- matrix(0, nrow(mm), length(missing_cols), dimnames = list(NULL, missing_cols))
        mm <- cbind(mm, add)
      }
      mm <- mm[, object$columns, drop = FALSE]
      as.numeric(stats::predict(object$object, newx = mm, s = object$lambda,
        type = if (object$family_name == "binomial") "response" else "link"))
    }, envir = .GlobalEnv)
  }
  # Dedicated elastic-net sensitivity learners. These are additional
  # SuperLearner candidates and do not alter the baseline SL.glmnet.fixed.
  register_penalized_glmnet_sensitivity <- function(sl_name, mode) {
    assign(sl_name,
      function(Y, X, newX, family, obsWeights = NULL, id = NULL, ...) {
        X <- as.data.frame(X, check.names = FALSE)
        newX <- as.data.frame(newX, check.names = FALSE)
        missing_raw <- setdiff(names(X), names(newX))
        if (length(missing_raw)) stop(sl_name, " newX missing columns: ", paste(missing_raw, collapse = ", "), call. = FALSE)
        newX <- newX[, names(X), drop = FALSE]
        sanitize <- function(d) {
          for (cn in names(d)) {
            z <- d[[cn]]
            if (is.numeric(z)) { z[!is.finite(z)] <- 0; d[[cn]] <- z }
            else if (is.factor(z) && anyNA(z)) { z <- addNA(z); levels(z)[is.na(levels(z))] <- "Missing"; d[[cn]] <- z }
          }
          d
        }
        X <- sanitize(X); newX <- sanitize(newX)
        Xm <- stats::model.matrix(~ . - 1, X, na.action = stats::na.pass)
        newXm <- stats::model.matrix(~ . - 1, newX, na.action = stats::na.pass)
        miss <- setdiff(colnames(Xm), colnames(newXm))
        if (length(miss)) newXm <- cbind(newXm, matrix(0, nrow(newXm), length(miss), dimnames=list(NULL, miss)))
        newXm <- newXm[, colnames(Xm), drop = FALSE]
        if (ncol(Xm) == 1L) { Xm <- cbind(Xm, .glmnet_pad=0); newXm <- cbind(newXm, .glmnet_pad=0) }
        if (is.null(id) || length(id) != length(Y)) stop(sl_name, " requires cluster ids.", call. = FALSE)
        ctx <- get("glmnet_context", envir=.SL_RUNTIME_ENV, inherits=FALSE)
        if (mode == "h1fs" && !identical(ctx$target, "g")) stop(sl_name, " is g-only.", call. = FALSE)
        if (mode == "A" && !identical(ctx$target, "pi")) stop(sl_name, " is pi-only.", call. = FALSE)
        w <- normalize_positive_weights(obsWeights, length(Y), paste0(sl_name, " weights"))
        if (identical(ctx$target, "g")) { At <- normalize_binary_var(Y, paste0(sl_name, " g outcome")); dt <- rep(1L,length(Y)) }
        else { At <- normalize_binary_var(X$A, paste0(sl_name, " pi treatment")); dt <- normalize_binary_var(Y, paste0(sl_name, " pi outcome")) }
        foldid <- do.call(make_cluster_folds_balanced, c(list(cluster=as.character(id), A=At, k=ctx$nfolds,
          seed=ctx$seed, weights=w, delta=dt,
          balance_on_weights=isTRUE(cfg$final_tmle$internal_fold_balance_on_weights)), fold_control_from_cfg(cfg,"internal")))
        pf <- rep(1, ncol(Xm)); names(pf) <- colnames(Xm)
        if (mode == "A") {
          a_cols <- which(colnames(Xm) == "A")
          if (length(a_cols) != 1L) stop(sl_name, " could not identify exactly one A column.", call. = FALSE)
          pf[a_cols] <- as.numeric(lcfg$glmnet_pi_A$A_penalty_multiplier %||% 0)
        } else {
          hcols <- intersect(ctx$h1fs_processed_columns %||% character(0), colnames(Xm))
          if (!length(hcols)) stop(sl_name, " found no mapped H1FS processed columns.", call. = FALSE)
          pf[hcols] <- as.numeric(lcfg$glmnet_h1fs$h1fs_penalty_multiplier %||% 0.25)
        }
        xsp <- if (requireNamespace("Matrix", quietly=TRUE)) Matrix::Matrix(Xm, sparse=TRUE) else Xm
        fit <- glmnet::cv.glmnet(x=xsp, y=Y, family=family$family,
          alpha=lcfg$glmnet$alpha %||% 0.5, nlambda=lcfg$glmnet$nlambda %||% 100L,
          weights=w, foldid=foldid, standardize=isTRUE(lcfg$glmnet$standardize %||% TRUE),
          maxit=lcfg$glmnet$maxit %||% 100000L, penalty.factor=pf)
        lam <- if (identical(lcfg$glmnet$lambda_choice %||% "min", "1se")) fit$lambda.1se else fit$lambda.min
        validate_cv_glmnet_fit(fit, lam, expected_ncol=ncol(Xm), label=paste0(sl_name,"[",ctx$target,"]"))
        pred <- as.numeric(stats::predict(fit, newx=newXm, s=lam, type=if (family$family=="binomial") "response" else "link"))
        if (length(pred)!=nrow(newXm) || any(!is.finite(pred))) stop(sl_name, " produced invalid predictions.", call. = FALSE)
        fo <- list(
          object = fit, lambda = lam, columns = colnames(Xm),
          train_names = names(X), family_name = family$family,
          penalty_factor = pf, sensitivity_mode = mode)
        class(fo) <- sl_name
        list(pred=pred, fit=fo)
      }, envir=.GlobalEnv)
    pred_name <- paste0("predict.", sl_name)
    assign(pred_name, function(object, newdata, ...) {
      newdata <- as.data.frame(newdata, check.names = FALSE)
      missing_raw <- setdiff(object$train_names, names(newdata))
      if (length(missing_raw))
        stop(pred_name, " missing raw columns: ", paste(missing_raw, collapse = ", "), call. = FALSE)
      newdata <- newdata[, object$train_names, drop = FALSE]
      for (cn in names(newdata)) {
        z <- newdata[[cn]]
        if (is.numeric(z)) {
          z[!is.finite(z)] <- 0
          newdata[[cn]] <- z
        } else if (is.factor(z) && anyNA(z)) {
          z <- addNA(z)
          levels(z)[is.na(levels(z))] <- "Missing"
          newdata[[cn]] <- z
        }
      }
      mm <- stats::model.matrix(~ . - 1, data = newdata, na.action = stats::na.pass)
      if (nrow(mm) != nrow(newdata) || any(!is.finite(mm)))
        stop(pred_name, " produced an invalid model matrix.", call. = FALSE)
      miss <- setdiff(object$columns, colnames(mm))
      if (length(miss))
        mm <- cbind(mm, matrix(0, nrow(mm), length(miss), dimnames = list(NULL, miss)))
      extra <- setdiff(colnames(mm), object$columns)
      if (length(extra)) mm <- mm[, !colnames(mm) %in% extra, drop = FALSE]
      mm <- mm[, object$columns, drop = FALSE]
      pred <- as.numeric(stats::predict(object$object, newx = mm, s = object$lambda,
        type = if (object$family_name == "binomial") "response" else "link"))
      if (length(pred) != nrow(mm) || any(!is.finite(pred)))
        stop(pred_name, " produced invalid predictions.", call. = FALSE)
      pred
    }, envir = .GlobalEnv)
  }
  if (isTRUE(lcfg$g$use_glmnet_h1fs %||% FALSE)) register_penalized_glmnet_sensitivity("SL.glmnet.h1fs", "h1fs")
  if (isTRUE(lcfg$pi$use_glmnet_A_unpenalized %||% FALSE)) register_penalized_glmnet_sensitivity("SL.glmnet.pi_A_unpenalized", "A")

  # Safeguarded GAM learner used for the outcome-observation model pi.
  if (isTRUE(lcfg$Q$use_gam) || isTRUE(lcfg$g$use_gam) || isTRUE(lcfg$pi$use_gam)) {
    assign("SL.gam.fixed",
      function(Y, X, newX, family, obsWeights = NULL, id = NULL, ...) {
        if (!requireNamespace("mgcv", quietly = TRUE))
          stop("SL.gam.fixed requires the mgcv package.", call. = FALSE)
        X <- as.data.frame(X, check.names = FALSE)
        newX <- as.data.frame(newX, check.names = FALSE)
        if (nrow(X) != length(Y)) stop("SL.gam.fixed training-row mismatch.", call. = FALSE)
        if (".obsw" %in% names(X) || "Y" %in% names(X))
          stop("SL.gam.fixed received a predictor name reserved for its model data.", call. = FALSE)
        missing_new <- setdiff(names(X), names(newX))
        if (length(missing_new)) stop("SL.gam.fixed newX missing columns: ", paste(missing_new, collapse = ", "), call. = FALSE)
        newX <- newX[, names(X), drop = FALSE]
        X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
        newX[] <- lapply(newX, function(z) suppressWarnings(as.numeric(z)))
        if (any(!is.finite(as.matrix(X))) || any(!is.finite(as.matrix(newX))) || any(!is.finite(Y)))
          stop("SL.gam.fixed received non-finite data.", call. = FALSE)
        if (is.null(obsWeights)) obsWeights <- rep(1, length(Y))
        obsWeights <- as.numeric(obsWeights)
        if (length(obsWeights) != length(Y) || any(!is.finite(obsWeights)) || any(obsWeights <= 0))
          stop("SL.gam.fixed received invalid observation weights.", call. = FALSE)
        obsWeights <- obsWeights / mean(obsWeights)
        family_name <- family$family
        eps_gam <- lcfg$gam$eps %||% 1e-6
        sanitize_pred <- function(z, n_expected) {
          z <- as.numeric(z)
          if (length(z) != n_expected || any(!is.finite(z)))
            stop("SL.gam.fixed produced invalid predictions.", call. = FALSE)
          if (identical(family_name, "binomial")) z <- pmin(pmax(z, eps_gam), 1 - eps_gam)
          z
        }
        make_fit <- function(object) {
          z <- list(object = object, fallback = FALSE, family_name = family_name,
                    eps = eps_gam, train_names = names(X))
          class(z) <- "SL.gam.fixed"
          z
        }
        if (length(unique(Y)) < 2L)
          stop("SL.gam.fixed requires outcome variation in every training subset.", call. = FALSE)
        unique_n <- vapply(X, function(z) length(unique(z)), integer(1))
        sm <- names(X)[unique_n > (lcfg$gam$smooth_unique_min %||% 10L)]
        lin <- setdiff(names(X), sm)
        quote_nm <- function(z) paste0("`", gsub("`", "", z, fixed = TRUE), "`")
        terms <- c(if (length(sm)) sprintf("s(%s, k=%d)", quote_nm(sm), as.integer(lcfg$gam$k %||% 4L)),
                   if (length(lin)) quote_nm(lin))
        form <- stats::as.formula(paste("Y ~", if (length(terms)) paste(terms, collapse = " + ") else "1"))
        environment(form) <- asNamespace("mgcv")
        # Keep observation weights inside the model data. mgcv evaluates the
        # weights expression through model.frame; a wrapper-local symbol such as
        # obsWeights is not reliably visible after SuperLearner evaluates the
        # learner call. Storing the normalized weights as .obsw and referring to
        # that data column prevents the verified "object 'obsWeights' not found"
        # hard failure without allowing .obsw to enter the explicitly constructed
        # predictor formula.
        train_dat <- data.frame(Y = Y, X, .obsw = obsWeights, check.names = FALSE)
        valid_dat <- data.frame(newX, check.names = FALSE)
        fit <- tryCatch(
          withCallingHandlers(
            mgcv::gam(form, data = train_dat, family = family,
                      weights = .obsw, method = "REML", select = TRUE,
                      na.action = stats::na.fail,
                      control = mgcv::gam.control(maxit = lcfg$gam$maxit %||% 100L)),
            warning = function(w) {
              if (grepl("non-integer #successes", conditionMessage(w), fixed = TRUE))
                invokeRestart("muffleWarning")
            }),
          error = function(e) e)
        if (inherits(fit, "error") || (!is.null(fit$converged) && !isTRUE(fit$converged))) {
          reason <- if (inherits(fit, "error")) conditionMessage(fit) else "mgcv convergence failure"
          stop("SL.gam.fixed failed: ", reason, call. = FALSE)
        }
        pred <- tryCatch(as.numeric(mgcv::predict.gam(fit, newdata = valid_dat, type = "response")),
                         error = function(e) e)
        if (inherits(pred, "error") || length(pred) != nrow(newX) || any(!is.finite(pred))) {
          reason <- if (inherits(pred, "error")) conditionMessage(pred) else "invalid predictions"
          stop("SL.gam.fixed prediction failed: ", reason, call. = FALSE)
        }
        pred <- sanitize_pred(pred, nrow(newX))
        list(pred = pred, fit = make_fit(fit))
      }, envir = .GlobalEnv)
    assign("predict.SL.gam.fixed",
      function(object, newdata, ...) {
        newdata <- as.data.frame(newdata, check.names = FALSE)
        missing_new <- setdiff(object$train_names, names(newdata))
        if (length(missing_new)) stop("predict.SL.gam.fixed missing columns: ", paste(missing_new, collapse = ", "), call. = FALSE)
        newdata <- newdata[, object$train_names, drop = FALSE]
        newdata[] <- lapply(newdata, function(z) suppressWarnings(as.numeric(z)))
        pred <- tryCatch(
          as.numeric(mgcv::predict.gam(object$object, newdata = newdata, type = "response")),
          error = function(e) stop("predict.SL.gam.fixed failed: ", conditionMessage(e), call. = FALSE))
        if (length(pred) != nrow(newdata) || any(!is.finite(pred)))
          stop("predict.SL.gam.fixed produced invalid predictions.", call. = FALSE)
        if (identical(object$family_name, "binomial")) pred <- pmin(pmax(pred, object$eps), 1 - object$eps)
        as.numeric(pred)
      }, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

build_sl_library <- function(cfg, target = c("Q","g","pi")) {
  target <- match.arg(target)
  tcfg <- cfg$learners[[target]]; lib <- character()
  if (isTRUE(tcfg$use_mean))   lib <- c(lib, "SL.mean")
  if (isTRUE(tcfg$use_glm))    lib <- c(lib, "SL.glm")
  if (isTRUE(tcfg$use_glmnet)) lib <- c(lib, "SL.glmnet.fixed")
  if (identical(target, "g") && isTRUE(tcfg$use_glmnet_h1fs %||% FALSE))
    lib <- c(lib, "SL.glmnet.h1fs")
  if (identical(target, "pi") && isTRUE(tcfg$use_glmnet_A_unpenalized %||% FALSE))
    lib <- c(lib, "SL.glmnet.pi_A_unpenalized")
  if (isTRUE(tcfg$use_ranger)) lib <- c(lib, "SL.ranger.fixed")
  if (isTRUE(tcfg$use_xgboost)) lib <- c(lib, "SL.xgboost.fixed")
  if (isTRUE(tcfg$use_xgboost_rich %||% FALSE) && target == "Q")
    lib <- c(lib, "SL.xgboost.rich")
  if (isTRUE(tcfg$use_earth) && target == "Q") lib <- c(lib, "SL.earth.fixed")
  if (isTRUE(tcfg$use_gam))    lib <- c(lib, "SL.gam.fixed")
  if (isTRUE(tcfg$use_svm))    lib <- c(lib, "SL.svm")
  if (isTRUE(tcfg$use_nnet))   lib <- c(lib, "SL.nnet")
  if (length(lib) == 0L)
    stop(sprintf("SuperLearner library for %s is empty.", target), call. = FALSE)
  lib
}


assert_superlearner_fit <- function(fit, expected_library, target, outer_fold,
                                    expected_prediction_n) {
  if (is.null(fit))
    stop(sprintf("Fold %d %s SuperLearner fit is NULL.", outer_fold, target), call. = FALSE)
  if (!inherits(fit, "SuperLearner"))
    stop(sprintf("Fold %d %s did not return a SuperLearner object.", outer_fold, target), call. = FALSE)
  expected_library <- as.character(expected_library)
  if (!length(expected_library) || anyNA(expected_library) || any(!nzchar(expected_library)) ||
      anyDuplicated(expected_library))
    stop(sprintf("Fold %d %s configured SuperLearner library is empty, invalid, or duplicated.",
                 outer_fold, target), call. = FALSE)
  normalize_sl_name <- function(z) sub("_All$", "", as.character(z))
  library_names <- as.character(fit$libraryNames %||% names(fit$coef) %||% character(0))
  library_base <- normalize_sl_name(library_names)
  if (anyDuplicated(library_base))
    stop(sprintf("Fold %d %s SuperLearner returned duplicated learner names.",
                 outer_fold, target), call. = FALSE)
  if (length(library_base) != length(expected_library) ||
      !setequal(library_base, expected_library))
    stop(sprintf("Fold %d %s SuperLearner library does not match the configured library.",
                 outer_fold, target), call. = FALSE)

  align_flag <- function(flag, label) {
    if (is.null(flag))
      stop(sprintf("Fold %d %s SuperLearner lacks required %s diagnostics.",
                   outer_fold, target, label), call. = FALSE)
    flag <- as.logical(flag)
    if (length(flag) != length(library_names))
      stop(sprintf("Fold %d %s SuperLearner %s length mismatch.",
                   outer_fold, target, label), call. = FALSE)
    nm <- names(flag)
    if (!is.null(nm) && length(nm) == length(flag)) {
      idx <- match(expected_library, normalize_sl_name(nm))
    } else {
      idx <- match(expected_library, library_base)
    }
    if (anyNA(idx) || anyNA(flag[idx]))
      stop(sprintf("Fold %d %s SuperLearner could not align %s to configured learners.",
                   outer_fold, target, label), call. = FALSE)
    setNames(flag[idx], expected_library)
  }
  err_cv <- align_flag(fit$errorsInCVLibrary, "errorsInCVLibrary")
  err_full <- align_flag(fit$errorsInLibrary, "errorsInLibrary")
  failed <- names(err_cv)[err_cv | err_full]
  if (length(failed))
    stop(sprintf(paste0("Fold %d %s SuperLearner learner failure(s) detected by the package: %s. ",
                        "Strict production mode forbids silently zero-weighting failed learners."),
                 outer_fold, target, paste(failed, collapse = ", ")), call. = FALSE)

  pred_sl <- as.numeric(fit$SL.predict)
  if (length(pred_sl) != expected_prediction_n || any(!is.finite(pred_sl)))
    stop(sprintf("Fold %d %s SuperLearner returned invalid ensemble predictions.",
                 outer_fold, target), call. = FALSE)

  coef_names <- names(fit$coef) %||% library_names
  risk_names <- names(fit$cvRisk) %||% library_names
  coef_base <- normalize_sl_name(coef_names)
  risk_base <- normalize_sl_name(risk_names)
  coef_ix <- match(expected_library, coef_base)
  risk_ix <- match(expected_library, risk_base)
  if (anyNA(coef_ix) || anyNA(risk_ix))
    stop(sprintf("Fold %d %s SuperLearner omitted a configured coefficient or CV risk.",
                 outer_fold, target), call. = FALSE)
  coef_values <- as.numeric(fit$coef[coef_ix])
  risk_values <- as.numeric(fit$cvRisk[risk_ix])
  if (any(!is.finite(coef_values)) || any(coef_values < -1e-10) ||
      !isTRUE(all.equal(sum(coef_values), 1, tolerance = 1e-6)) ||
      any(!is.finite(risk_values)))
    stop(sprintf("Fold %d %s SuperLearner has invalid ensemble coefficients or CV risks.",
                 outer_fold, target), call. = FALSE)

  lp <- fit$library.predict
  if (is.null(lp) || !is.matrix(lp) || nrow(lp) != expected_prediction_n ||
      ncol(lp) != length(library_names) || any(!is.finite(lp)))
    stop(sprintf("Fold %d %s SuperLearner has invalid base-learner predictions on newX.",
                 outer_fold, target), call. = FALSE)
  if (!is.null(colnames(lp)) &&
      !identical(normalize_sl_name(colnames(lp)), library_base))
    stop(sprintf("Fold %d %s SuperLearner base-prediction columns are misaligned with its learner library.",
                 outer_fold, target), call. = FALSE)
  Z <- fit$Z
  if (is.null(Z) || !is.matrix(Z) || ncol(Z) != length(library_names) ||
      any(!is.finite(Z)))
    stop(sprintf("Fold %d %s SuperLearner has an invalid internal CV prediction matrix.",
                 outer_fold, target), call. = FALSE)

  if (is.null(fit$fitLibrary) || length(fit$fitLibrary) != length(library_names) ||
      any(vapply(fit$fitLibrary, is.null, logical(1))))
    stop(sprintf(paste0("Fold %d %s SuperLearner has an incomplete fitted learner library. ",
                        "Production calls require saveFitLibrary=TRUE."),
                 outer_fold, target), call. = FALSE)
  fitlib_names <- names(fit$fitLibrary)
  if (!is.null(fitlib_names) &&
      !identical(normalize_sl_name(fitlib_names), library_base))
    stop(sprintf("Fold %d %s SuperLearner fitted-library entries are misaligned.",
                 outer_fold, target), call. = FALSE)
  fallback <- vapply(fit$fitLibrary, function(z) {
    is.list(z) && isTRUE(z$fallback %||% FALSE)
  }, logical(1))
  if (any(fallback))
    stop(sprintf("Fold %d %s SuperLearner contains an internal learner fallback: %s.",
                 outer_fold, target,
                 paste(library_names[fallback], collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}


extract_pi_counterfactual_blocks <- function(preds, n_expected) {
  preds <- as.numeric(preds)
  n_expected <- as.integer(n_expected)[1L]
  if (!is.finite(n_expected) || n_expected < 1L ||
      length(preds) != 3L * n_expected || any(!is.finite(preds)))
    stop(sprintf("pi counterfactual extraction expected %d finite predictions but received %d.",
                 3L * n_expected, length(preds)), call. = FALSE)
  list(
    pi_1W = preds[seq_len(n_expected)],
    pi_0W = preds[n_expected + seq_len(n_expected)],
    pi_AW = preds[2L * n_expected + seq_len(n_expected)])
}

# =============================================================================
