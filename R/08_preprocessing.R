# 6) FINAL W PREPROCESSING HELPERS
# =============================================================================
# Plain-English role: these helpers learn preprocessing rules on each final
# TMLE training fold and apply the frozen rules to that fold's validation rows.
# They are used by the nested fold-specific rough screen and the optional
# augmented multivariable elastic-net union screening step inside final TMLE.

winsorize_vec <- function(x, probs) {
  if (anyNA(x)) stop("winsorize_vec: NAs should have been handled upstream.", call. = FALSE)
  q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 8)
  pmin(pmax(x, q[1]), q[2])
}

nonconstant_cols <- function(M, tol) {
  if (ncol(M) == 0L) return(logical(0))
  vv <- apply(M, 2L, stats::var)
  is.finite(vv) & vv > tol
}

infer_var_types <- function(df, numeric_imputation) {
  types <- vapply(df, function(col) {
    if (is.factor(col))  return("factor")
    if (is.logical(col)) return("numeric")
    if (is.numeric(col)) return("numeric")
    if (is.character(col)) return("factor")
    "factor"
  }, character(1))
  types
}

prep_numeric_train <- function(x, cfg_gp, cfg_pre, support = NULL, variable_name = NULL) {
  n <- length(x)
  n_obs <- if (!is.null(support$n_obs)) as.integer(support$n_obs) else sum(is.finite(x))
  p_obs <- if (!is.null(support$p_obs)) as.numeric(support$p_obs) else n_obs / max(1, n)
  # A retained numeric variable must satisfy both absolute and proportional
  # original-support requirements. Support is measured before imputation and
  # excludes every frozen general-missing and structural-skip code.
  if (n_obs < cfg_gp$numeric_min_observed_n || p_obs < cfg_gp$numeric_min_observed_prop)
    return(NULL)
  x_ok <- as.numeric(x[is.finite(x)])
  if (!length(x_ok)) return(NULL)
  is_indicator <- !is.null(variable_name) && grepl("(_missA|_miss97)$", variable_name)
  ux <- sort(unique(x_ok))
  is_binary <- length(ux) <= 2L && all(ux %in% c(0, 1))
  is_low_cardinality <- length(ux) <= (cfg_pre$factor_unique_threshold %||% 10L)
  do_winsor <- !(is_indicator || is_binary || is_low_cardinality)
  if (is_indicator || is_binary) {
    return(list(mu = 0, s = 1, win_q = c(-Inf, Inf), fill = 0,
                identity = TRUE, n_obs_original = n_obs, p_obs_original = p_obs))
  }
  win_q <- if (do_winsor)
    stats::quantile(x_ok, probs = cfg_gp$winsor_probs, na.rm = TRUE, names = FALSE, type = 8)
  else c(-Inf, Inf)
  win <- pmin(pmax(x_ok, win_q[1]), win_q[2])
  mu <- mean(win, na.rm = TRUE); sdev <- stats::sd(win, na.rm = TRUE)
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sdev) || sdev < cfg_pre$scale_eps) sdev <- 1
  list(mu = mu, s = sdev, win_q = win_q, fill = stats::median(win, na.rm = TRUE),
       identity = FALSE, n_obs_original = n_obs, p_obs_original = p_obs)
}

apply_numeric_transform <- function(x, prep) {
  obs <- is.finite(x)
  val <- as.numeric(x); val[!obs] <- prep$fill
  val <- pmin(pmax(val, prep$win_q[1]), prep$win_q[2])
  if (isTRUE(prep$identity)) return(as.matrix(val))
  as.matrix((val - prep$mu) / prep$s)
}


deterministic_factor_levels <- function(values, special_levels = character(0)) {
  vals <- unique(as.character(values))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  special_levels <- unique(as.character(special_levels))
  ordinary <- setdiff(vals, special_levels)
  num <- suppressWarnings(as.numeric(ordinary))
  if (length(ordinary) && all(is.finite(num))) {
    ordinary <- ordinary[order(num, ordinary)]
  } else {
    ordinary <- sort(ordinary, method = "radix")
  }
  unique(c(ordinary, special_levels))
}


prep_factor_train <- function(x, cfg_gp, cfg_pre, A = NULL,
                              preserve_substantive_levels = FALSE) {
  x_chr <- canonicalize_factor_text(x)
  is_na <- is.na(x_chr)

  other_label   <- cfg_pre$factor_other_label   %||% "_Other_"
  missing_label <- cfg_pre$factor_missing_label %||% "Missing"
  skip_label    <- cfg_pre$factor_skip_label    %||% "Skip"
  skip_present <- skip_label %in% x_chr
  special_levels <- unique(c(missing_label,
                             if (skip_present) skip_label else character(0),
                             other_label))
  x_chr[is_na] <- missing_label

  tab <- table(x_chr, useNA = "no")
  if (length(tab) == 0L) {
    return(list(
      levels = deterministic_factor_levels(c(missing_label, other_label),
                                           c(missing_label, other_label)),
      other_label = other_label,
      missing_label = missing_label,
      skip_label = skip_label,
      preserve_substantive_levels = isTRUE(preserve_substantive_levels),
      observed_substantive_levels = character(0)
    ))
  }

  observed_substantive <- setdiff(unique(x_chr), special_levels)
  if (!isTRUE(preserve_substantive_levels)) {
    rare_levels <- names(tab)[tab < cfg_gp$rare_level_min_n &
                                !(names(tab) %in% special_levels)]
    if (length(rare_levels)) x_chr[x_chr %in% rare_levels] <- other_label

    min_exp <- cfg_gp$factor_min_exposed_per_level %||% 0L
    if (!is.null(A) && min_exp > 0L && length(A) == length(x_chr)) {
      A01 <- suppressWarnings(as.integer(A))
      if (anyNA(A01) || any(!A01 %in% c(0L, 1L)))
        stop("prep_factor_train received an invalid treatment vector.", call. = FALSE)
      tab_all <- table(x_chr, useNA = "no")
      tab_exp <- table(x_chr[A01 == 1L], useNA = "no")
      exp_counts <- setNames(rep(0L, length(tab_all)), names(tab_all))
      exp_counts[names(tab_exp)] <- as.integer(tab_exp)
      low_exp_levels <- names(exp_counts)[
        exp_counts < min_exp & !(names(exp_counts) %in% special_levels)
      ]
      if (length(low_exp_levels)) x_chr[x_chr %in% low_exp_levels] <- other_label
    }

    current_levels <- unique(x_chr)
    if (length(current_levels) > cfg_gp$factor_max_levels_after_collapse) {
      ordinary_by_frequency <- setdiff(
        names(sort(table(x_chr), decreasing = TRUE)), special_levels)
      n_special <- length(intersect(special_levels, unique(c(x_chr, special_levels))))
      n_top <- max(0L, cfg_gp$factor_max_levels_after_collapse - n_special)
      top_levels <- head(ordinary_by_frequency, n_top)
      keep_levels <- unique(c(top_levels, special_levels))
      x_chr[!(x_chr %in% keep_levels)] <- other_label
    }
  }

  # Deterministic ordering prevents fold-dependent reference categories caused
  # solely by row order. Protected H1FS variables retain every substantive
  # category observed in the training fold; unseen validation categories map
  # to _Other_ through the frozen recipe.
  final_levels <- deterministic_factor_levels(x_chr, special_levels)
  if (length(final_levels) < 2L) final_levels <- unique(c(final_levels, other_label))

  list(
    levels = final_levels,
    other_label = other_label,
    missing_label = missing_label,
    skip_label = skip_label,
    preserve_substantive_levels = isTRUE(preserve_substantive_levels),
    observed_substantive_levels = deterministic_factor_levels(
      observed_substantive, character(0))
  )
}

apply_factor_transform <- function(x, prep) {
  x_chr <- canonicalize_factor_text(x)

  levels_safe <- unique(c(
    prep$levels,
    prep$missing_label,
    prep$other_label
  ))

  x_chr[is.na(x_chr)] <- prep$missing_label
  x_chr[!(x_chr %in% levels_safe)] <- prep$other_label

  f <- factor(x_chr, levels = levels_safe)
  mm_full <- stats::model.matrix(
    ~ f,
    data = data.frame(f = f),
    na.action = stats::na.pass
  )
  if (nrow(mm_full) != length(x_chr))
    stop(sprintf("apply_factor_transform(): factor produced %d rows but expected %d.",
                 nrow(mm_full), length(x_chr)), call. = FALSE)
  # Drop the intercept/reference column so factor levels are represented once
  # without the exact full-dummy-plus-intercept dependency in regression and
  # GAM learners. Missing and Skip remain available either as explicit dummies
  # or through the reference pattern.
  mm <- mm_full[, colnames(mm_full) != "(Intercept)", drop = FALSE]
  colnames(mm) <- gsub("^f", "", colnames(mm))
  as.matrix(mm)
}

# Build design matrix for a set of columns. Returns processed matrix and
# a "group" id per column (all dummies for one factor share a group id).
# Enforces a hard stop at hard_max_processed_columns to prevent memory
# blowouts on the small-memory workstations used for this project.

build_grouped_design_train <- function(df, cfg_gp, cfg_pre, hard_max_cols = NULL,
                                       A = NULL, protected_raw_vars = character(0)) {
  if (ncol(df) == 0L) return(list(X = matrix(0, nrow = nrow(df), ncol = 0),
                                  group = integer(0), recipes = list()))
  protected_raw_vars <- unique(as.character(protected_raw_vars %||% character(0)))
  recipes <- list(); mats <- list(); groups <- integer(0); grp <- 0L
  types <- infer_var_types(df, cfg_pre$numeric_imputation)
  support_map <- attr(df, "numeric_support") %||% list()
  for (nm in names(df)) {
    col <- df[[nm]]
    if (identical(types[[nm]], "numeric")) {
      prep <- prep_numeric_train(as.numeric(col), cfg_gp, cfg_pre,
                                 support = support_map[[nm]], variable_name = nm)
      if (is.null(prep)) next
      M <- apply_numeric_transform(as.numeric(col), prep)
      colnames(M) <- nm
      rec_obj <- list(type = "numeric", prep = prep)
    } else {
      preserve <- nm %in% protected_raw_vars
      prep <- prep_factor_train(
        col, cfg_gp, cfg_pre, A = A,
        preserve_substantive_levels = preserve)
      if (is.null(prep) || length(prep$levels) < 2L) next
      M <- apply_factor_transform(col, prep)
      if (ncol(M) == 0L) next
      colnames(M) <- paste0(nm, "_", colnames(M))
      rec_obj <- list(type = "factor", prep = prep)
    }
    keep <- nonconstant_cols(M, tol = cfg_pre$constant_variance_tol)
    M <- M[, keep, drop = FALSE]
    if (ncol(M) == 0L) next
    recipes[[nm]] <- rec_obj
    grp <- grp + 1L
    groups <- c(groups, rep(grp, ncol(M)))
    mats[[nm]] <- M
    if (!is.null(hard_max_cols) && length(groups) > hard_max_cols) {
      stop(sprintf(
        "Processed design matrix exceeded hard_max_processed_columns = %d columns. ",
        hard_max_cols),
        "Tighten nested rough-screen caps, collapse nonprotected long factors more aggressively, or ",
        "raise hard_max_processed_columns if RAM allows.",
        call. = FALSE)
    }
  }
  if (length(mats) == 0L)
    return(list(X = matrix(0, nrow = nrow(df), ncol = 0),
                group = integer(0), recipes = recipes))
  X <- do.call(cbind, mats)
  list(X = X, group = groups, recipes = recipes)
}


apply_preprocess_recipe <- function(df, recipes) {
  mats <- list()
  for (nm in names(recipes)) {
    if (!nm %in% names(df))
      stop("apply_preprocess_recipe is missing required variable '", nm, "'.", call. = FALSE)
    col <- df[[nm]]; rec <- recipes[[nm]]
    if (identical(rec$type, "numeric")) {
      M <- apply_numeric_transform(as.numeric(col), rec$prep); colnames(M) <- nm
    } else {
      M <- apply_factor_transform(col, rec$prep)
      if (ncol(M) > 0L) colnames(M) <- paste0(nm, "_", colnames(M))
    }
    if (nrow(M) != nrow(df)) {
      stop(sprintf(
        "apply_preprocess_recipe(): variable '%s' produced %d rows but expected %d. This usually means factor levels created NA rows during model.matrix().",
        nm, nrow(M), nrow(df)
      ), call. = FALSE)
    }
    if (ncol(M) > 0L) mats[[nm]] <- M
  }
  if (length(mats) == 0L) return(matrix(0, nrow = nrow(df), ncol = 0))
  do.call(cbind, mats)
}

# =============================================================================
