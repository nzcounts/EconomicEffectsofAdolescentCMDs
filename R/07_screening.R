# Generated from the reviewed v8.28 production source.
# Original lines: 5453-5653.
# Module role: Nested rough screening.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 5) STAGE 1: ROUGH PRE-SCREEN
# =============================================================================
# Plain-English role: a fast first-pass filter. For every candidate variable
# we fit a single-variable cross-validated model predicting (a) the exposure
# and (b) the outcome, compute a score (logloss- or MSE-based R^2), then keep
# the variables whose scores exceed a "knee" cutoff. The purpose is to drop
# obvious noise columns before final TMLE. In the default
# pipeline, the rough screen is nested inside final TMLE folds and always
# uses whole-school validation folds.

build_single_var_screen_df <- function(nm, X, impute_method = "median") {
  if (grepl("(_missA|_miss97)$", nm)) return(NULL)
  col <- X[[nm]]
  miss_names <- paste0(nm, c("_missA", "_miss97"))
  miss_names <- miss_names[miss_names %in% names(X)]
  if (is.numeric(col)) {
    x <- as.numeric(col)
    fill_value <- compute_simple_impute(x, impute_method)
    x[is.na(x)] <- fill_value
    out <- data.frame(value = x, check.names = FALSE)
  } else {
    f  <- addNA(as.factor(col))
    mm <- stats::model.matrix(~ f, data = data.frame(f = f), na.action = stats::na.pass)
    if (nrow(mm) != length(f)) stop(sprintf("build_single_var_screen_df(): variable '%s' produced %d rows but expected %d.", nm, nrow(mm), length(f)), call. = FALSE)
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    if (ncol(mm) == 0L) return(NULL)
    out <- as.data.frame(mm, check.names = FALSE)
  }
  if (length(miss_names) > 0L)
    out <- cbind(out, as.data.frame(X[, miss_names, drop = FALSE], check.names = FALSE))
  out
}

screen_binom_linear <- function(y, X, K = 5L, seed = 1L, eps = 1e-15,
                                fold = NULL, weights = NULL, ridge_lambda = 1.0,
                                glmnet_maxit = 1000000L, glmnet_thresh = 1e-5) {
  keep <- !is.na(y)
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  y <- y[keep]; X <- X[keep, , drop = FALSE]
  w <- normalize_positive_weights(if (is.null(weights)) NULL else weights[keep],
                                  length(y), "binary screen weights")
  stopifnot(all(y %in% 0:1))
  n <- length(y)
  if (n < max(10L, K)) return(setNames(rep(NA_real_, ncol(X)), names(X)))
  if (is.null(fold)) {
    fold <- with_local_seed(seed, sample(rep(seq_len(K), length.out = n)))
  } else fold <- fold[keep]
  if (length(fold) != n || anyNA(fold) || any(fold < 1L))
    stop("Binary screen received invalid fold assignments.", call. = FALSE)
  K <- max(as.integer(fold))
  clip <- function(p) pmin(pmax(p, eps), 1 - eps)
  wmean <- function(z, ww) sum(ww * z) / sum(ww)
  logloss <- function(yy, p, ww) {
    p <- clip(p)
    sum(ww * (-(yy * log(p) + (1 - yy) * log(1 - p)))) / sum(ww)
  }
  p0 <- numeric(n)
  for (k in seq_len(K)) {
    tr <- fold != k; te <- !tr
    p0[te] <- wmean(y[tr], w[tr])
  }
  ll0 <- logloss(y, p0, w)
  if (!is.finite(ll0) || ll0 <= 0) return(setNames(rep(NA_real_, ncol(X)), names(X)))

  fit_predict <- function(Xtr, ytr, Xte, wtr) {
    mu_y <- wmean(ytr, wtr)
    if (length(unique(ytr)) < 2L || nrow(Xtr) < 5L) return(rep(mu_y, nrow(Xte)))
    Xtr <- suppressWarnings(data.matrix(Xtr)); Xte <- suppressWarnings(data.matrix(Xte))
    Xtr[!is.finite(Xtr)] <- 0; Xte[!is.finite(Xte)] <- 0
    sds <- if (ncol(Xtr)) apply(Xtr, 2, stats::sd) else numeric(0)
    keep_cols <- is.finite(sds) & sds > 0
    nc0 <- sum(keep_cols)
    if (nc0 == 0L) return(rep(mu_y, nrow(Xte)))
    Xtr <- Xtr[, keep_cols, drop = FALSE]; Xte <- Xte[, keep_cols, drop = FALSE]
    wtr <- normalize_positive_weights(wtr, length(ytr), "binary screen training weights")
    if (nc0 == 1L) {
      xs_mu <- wmean(Xtr[, 1], wtr)
      xs_sd <- sqrt(sum(wtr * (Xtr[, 1] - xs_mu)^2) / sum(wtr))
      if (!is.finite(xs_sd) || xs_sd <= 0) return(rep(mu_y, nrow(Xte)))
      ztr <- (Xtr[, 1] - xs_mu) / xs_sd; zte <- (Xte[, 1] - xs_mu) / xs_sd
      Ztr <- cbind(value = ztr, .glmnet_pad = 0); Zte <- cbind(value = zte, .glmnet_pad = 0)
      fit1 <- tryCatch(glmnet::glmnet(
        x = Ztr, y = ytr, family = "binomial", weights = wtr,
        alpha = 0, lambda = ridge_lambda, standardize = FALSE,
        intercept = TRUE, thresh = glmnet_thresh, maxit = glmnet_maxit),
        error = function(e) NULL)
      if (is.null(fit1)) return(rep(NA_real_, nrow(Xte)))
      pr <- tryCatch(as.numeric(stats::predict(
        fit1, newx = Zte, type = "response", s = ridge_lambda)),
        error = function(e) NULL)
      if (is.null(pr) || !all(is.finite(pr))) return(rep(NA_real_, nrow(Xte)))
      return(pr)
    }
    fit <- tryCatch(glmnet::glmnet(
      x = Xtr, y = ytr, family = "binomial", weights = wtr,
      alpha = 0, lambda = ridge_lambda, standardize = TRUE,
      intercept = TRUE, thresh = glmnet_thresh, maxit = glmnet_maxit),
      error = function(e) NULL)
    if (is.null(fit)) return(rep(NA_real_, nrow(Xte)))
    pr <- tryCatch(as.numeric(stats::predict(
      fit, newx = Xte, type = "response", s = ridge_lambda)),
      error = function(e) NULL)
    if (is.null(pr) || !all(is.finite(pr))) rep(NA_real_, nrow(Xte)) else pr
  }

  vapply(names(X), function(nm) {
    if (grepl("(_missA|_miss97)$", nm)) return(NA_real_)
    Xnm <- tryCatch(build_single_var_screen_df(nm, X, impute_method = "median"),
                    error = function(e) NULL)
    if (is.null(Xnm) || ncol(Xnm) == 0L) return(NA_real_)
    p <- numeric(n)
    for (k in seq_len(K)) {
      tr <- fold != k; te <- !tr
      p[te] <- fit_predict(Xnm[tr, , drop = FALSE], y[tr],
                           Xnm[te, , drop = FALSE], w[tr])
    }
    ll <- logloss(y, p, w)
    1 - ll / ll0
  }, numeric(1))
}
screen_gauss_linear <- function(y, X, K = 5L, seed = 1L,
                                fold = NULL, weights = NULL) {
  keep <- is.finite(y)
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  y <- y[keep]; X <- X[keep, , drop = FALSE]
  w <- normalize_positive_weights(if (is.null(weights)) NULL else weights[keep],
                                  length(y), "gaussian screen weights")
  n <- length(y)
  if (n < max(10L, K)) return(setNames(rep(NA_real_, ncol(X)), names(X)))
  if (is.null(fold)) {
    fold <- with_local_seed(seed, sample(rep(seq_len(K), length.out = n)))
  } else fold <- fold[keep]
  if (length(fold) != n || anyNA(fold) || any(fold < 1L))
    stop("Gaussian screen received invalid fold assignments.", call. = FALSE)
  K <- max(as.integer(fold))
  wmean <- function(z, ww) sum(ww * z) / sum(ww)
  mse <- function(yy, yhat, ww) sum(ww * (yy - yhat)^2) / sum(ww)
  y0 <- numeric(n)
  for (k in seq_len(K)) {
    tr <- fold != k; te <- !tr
    y0[te] <- wmean(y[tr], w[tr])
  }
  mse0 <- mse(y, y0, w)
  if (!is.finite(mse0) || mse0 <= 0) return(setNames(rep(NA_real_, ncol(X)), names(X)))

  vapply(names(X), function(nm) {
    col <- X[[nm]]
    if (is.matrix(col)) { if (ncol(col) == 1L) col <- col[, 1] else return(NA_real_) }
    if (grepl("(_missA|_miss97)$", nm)) return(NA_real_)
    yhat <- numeric(n)
    if (is.numeric(col)) {
      miss_names <- paste0(nm, c("_missA", "_miss97"))
      miss_names <- miss_names[miss_names %in% names(X)]
      for (k in seq_len(K)) {
        tr <- fold != k; te <- !tr
        xt <- col[tr]; yt <- y[tr]; wt <- w[tr]
        good <- is.finite(yt) & is.finite(wt) & wt > 0
        xt <- xt[good]; yt <- yt[good]; wt <- wt[good]
        if (length(yt) < 2L) { yhat[te] <- wmean(y[tr], w[tr]); next }
        mu <- wmean(xt, wt)
        if (!is.finite(mu)) { yhat[te] <- wmean(yt, wt); next }
        xtr <- xt - mu; xtr[is.na(xtr)] <- 0
        Xtr <- matrix(1, nrow = length(yt), ncol = 1)
        use_x <- !all(xtr == 0)
        if (use_x) Xtr <- cbind(Xtr, xtr)
        if (length(miss_names) > 0L) {
          Mtr <- as.matrix(X[tr, miss_names, drop = FALSE])[good, , drop = FALSE]
          Mtr[is.na(Mtr)] <- 0; Xtr <- cbind(Xtr, Mtr)
        }
        b <- tryCatch(stats::lm.wfit(Xtr, yt, w = wt)$coefficients,
                      error = function(e) rep(NA_real_, ncol(Xtr)))
        b[!is.finite(b)] <- 0
        xte <- col[te] - mu; xte[is.na(xte)] <- 0
        Xte <- matrix(1, nrow = sum(te), ncol = 1)
        if (use_x) Xte <- cbind(Xte, xte)
        if (length(miss_names) > 0L) {
          Mte <- data.matrix(X[te, miss_names, drop = FALSE])
          Mte[is.na(Mte)] <- 0; Xte <- cbind(Xte, Mte)
        }
        yhat[te] <- drop(Xte %*% b)
      }
    } else {
      f <- addNA(as.factor(col))
      for (k in seq_len(K)) {
        tr <- fold != k; te <- !tr
        yt <- y[tr]; ft <- f[tr]; wt <- w[tr]
        lev_num <- tapply(wt * yt, ft, sum)
        lev_den <- tapply(wt, ft, sum)
        levm <- lev_num / lev_den
        ybar <- wmean(yt, wt)
        mk <- levm[as.character(f[te])]
        yhat[te] <- ifelse(is.na(mk), ybar, mk)
      }
    }
    1 - mse(y, yhat, w) / mse0
  }, numeric(1))
}

# Orchestrator for the rough prescreen. Writes the shortlist to CSV and
# saves knee-curve PNGs so reviewers can see where the cutoff landed.
# =============================================================================
