# Generated from the reviewed v8.28 production source.
# Original lines: 6680-10369.
# Module role: Cross-fitted TMLE and ATT estimation.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 8) FINAL CV-TMLE
# =============================================================================
# Plain-English role: the main estimator. Splits the data into outer folds
# (balanced on exposure at the cluster level), then for each fold:
# (a) Runs nested cluster-aware rough screening on training rows only
# to decide which raw covariates W to keep (no selection leakage).
# (b) Builds a fold-specific processed design matrix W using only the
# training rows' transformation recipes (levels, medians, winsor
# bounds), and sanitizes column names.
# (c) Fits three SuperLearner models on the training rows: Q (outcome),
# g (propensity), pi (outcome-observation). Each uses mandatory whole-school cluster-aware
# internal CV; row-level internal CV is not a supported production path.
# (d) Predicts Q1W, Q0W, QAW, gn, piAW, pi1W, pi0W on validation rows.
# (e) Runs the TMLE fluctuation step on held-out predictions to produce
# targeted Qbar1W*, Qbar0W*.
# (f) Assembles the ATE estimator and the efficient influence function
# on the bounded [0,1] outcome scale, back-transformed for variance.
# (g) Uses the GSWGT1-weighted, REGION-stratified, PSUSCID-clustered
# with-replacement survey design to build a 95% EIF interval.
# v5 ADDITIONS:
# * Cluster-aware SuperLearner internal CV via validRows in cvControl.
# * Per-fold checkpointing: fold results written to RDS so that if the
# process crashes, a rerun resumes from the last completed fold.
# * Per-fold SL summaries (coef, CV risk, fallback flags) written to CSV.
# * Both-sided clipping on pi predictions; overlap product diagnostics.
# * EIF residuals built on bounded [0,1] Y-scale, scaled back for
# reporting, so heavy-tailed outliers above the 99th percentile do
# not inflate the reported standard error.
# * Fold-level assertions: every nuisance estimate is checked to be
# finite and within admissible range as the fold completes.

compute_continuous_cap <- function(y, weights, probability, cfg) {
  keep <- is.finite(y) & is.finite(weights) & weights > 0
  if (!any(keep)) stop("Cannot compute outcome cap: no finite observed outcomes with positive weights.", call. = FALSE)
  yk <- as.numeric(y[keep]); wk <- as.numeric(weights[keep])
  if (!is.finite(probability) || probability <= 0 || probability > 1)
    stop("Outcome-cap probability must lie in (0, 1].", call. = FALSE)
  if (probability >= 1) return(max(yk))
  if (isTRUE(cfg$outcome$continuous_cap_weighted %||% TRUE)) {
    dat <- data.frame(y = yk, w = wk)
    des <- survey::svydesign(ids = ~1, weights = ~w, data = dat)
    q <- survey::svyquantile(~y, design = des, quantiles = probability,
                             qrule = cfg$outcome$continuous_cap_qrule %||% "hf8",
                             ci = FALSE, se = FALSE, na.rm = TRUE)
    q_value <- as.numeric(stats::coef(q))[1L]
    if (!is.finite(q_value)) stop("Weighted outcome quantile is not finite.", call. = FALSE)
    return(q_value)
  }
  as.numeric(stats::quantile(yk, probs = probability, na.rm = TRUE,
                             names = FALSE, type = 8))
}

get_outcome_support_spec <- function(cfg, outcome_type, constructor_support = NULL) {
  if (identical(outcome_type, "binary"))
    return(list(lower = 0, upper = 1, lower_rule = "fixed_binary", upper_rule = "fixed_binary"))
  fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
  if (!is.null(constructor_support)) {
    lower <- constructor_support$natural_lower %||% constructor_support$lower %||%
      fam_cfg$natural_lower_bound %||% NA_real_
    upper <- constructor_support$natural_upper %||% constructor_support$upper %||%
      fam_cfg$natural_upper_bound %||% Inf
    lower_rule <- constructor_support$lower_rule %||% fam_cfg$lower_bound_rule %||% "empirical"
    upper_rule <- constructor_support$upper_rule %||% fam_cfg$upper_bound_rule %||% "quantile"
  } else {
    lower <- fam_cfg$natural_lower_bound %||% NA_real_
    upper <- fam_cfg$natural_upper_bound %||% Inf
    lower_rule <- fam_cfg$lower_bound_rule %||% "empirical"
    upper_rule <- fam_cfg$upper_bound_rule %||% "quantile"
  }
  list(lower = as.numeric(lower), upper = as.numeric(upper),
       lower_rule = lower_rule, upper_rule = upper_rule)
}

prepare_final_analysis_data <- function(main_df, cfg) {
  constructor_support <- attr(main_df, "outcome_support")
  df <- main_df
  A <- normalize_binary_var(df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  outcome_info <- prepare_modeled_outcome(
    df[[cfg$analysis$outcome_var]], cfg$analysis$outcome_type, cfg$analysis$outcome_var)
  Y_raw        <- outcome_info$values
  outcome_type <- outcome_info$type
  outcome_obs  <- outcome_info$observed
  delta_Y <- as.integer(outcome_obs)
  delta_name <- cfg$analysis$outcome_observed_var
  if (delta_name %in% names(df)) {
    delta_stored <- normalize_binary_var(df[[delta_name]], delta_name, require_both = FALSE)
    if (!identical(delta_stored, delta_Y))
      stop("Stored outcome-observation indicator is inconsistent with the modeled outcome.",
           call. = FALSE)
  }
  w_raw <- as.numeric(df[[cfg$analysis$weight_var]])
  # Invalid weights must have been removed during main-dataset construction.
  # Silently dropping rows here would desynchronize the analytic sample from
  # the frozen sample gates and the full survey-domain design frame.
  keep_w <- is.finite(w_raw) & w_raw > 0
  if (any(!keep_w))
    stop(sprintf(
      "Final TMLE received %d row(s) with non-positive or missing sampling weights. Rebuild the main dataset; rows may not be dropped after the survey-domain frame is frozen.",
      sum(!keep_w)), call. = FALSE)
  cluster <- df[[cfg$analysis$cluster_var]]
  if (anyNA(cluster))
    stop(sprintf("Final TMLE analytic sample contains %d row(s) with missing %s; folds cannot be defined without a PSU.",
                 sum(is.na(cluster)), cfg$analysis$cluster_var), call. = FALSE)
  if (!cfg$analysis$strata_var %in% names(df))
    stop(sprintf("Required Add Health design stratum '%s' is absent from the analytic data.",
                 cfg$analysis$strata_var), call. = FALSE)
  strata <- df[[cfg$analysis$strata_var]]
  if (anyNA(strata) || any(trimws(as.character(strata)) == ""))
    stop(sprintf("Final TMLE analytic sample contains missing or blank %s values; stratified PSU inference cannot be computed.",
                 cfg$analysis$strata_var), call. = FALSE)
  expected_h <- as.integer(cfg$analysis$expected_strata_n %||% 4L)
  observed_h <- length(unique(as.character(strata)))
  if (observed_h != expected_h)
    stop(sprintf("Final TMLE requires exactly %d observed %s levels; found %d.",
                 expected_h, cfg$analysis$strata_var, observed_h), call. = FALSE)
  # Bound Y to an outcome-specific finite range for TMLE. Verified natural
  # support is used when available; otherwise the empirical lower bound and
  # configured upper-cap rule define the range. The fluctuation runs on [0,1].
  y_obs_vals <- Y_raw[delta_Y == 1L & is.finite(Y_raw)]
  if (length(y_obs_vals) == 0L)
    stop("Final TMLE has no observed finite outcomes from which to define the bounded outcome.", call. = FALSE)
  support <- get_outcome_support_spec(cfg, outcome_type, constructor_support)
  if (outcome_type == "binary") {
    if (!isTRUE(all.equal(as.numeric(c(support$lower, support$upper)), c(0, 1),
                          tolerance = 0)))
      stop("Binary outcome support must be exactly 0/1.", call. = FALSE)
    if (!all(y_obs_vals %in% c(0, 1)))
      stop("Binary outcome contains values other than 0/1 after preparation.", call. = FALSE)
    # Binary outcomes must remain on their exact natural support regardless of
    # any continuous-outcome epsilon setting.
    y_lower <- 0
    y_upper <- 1
    cap_value <- 1
  } else {
    q_up <- compute_continuous_cap(
      Y_raw[delta_Y == 1L], w_raw[delta_Y == 1L],
      cfg$outcome$continuous_upper_quantile, cfg)
    if (identical(support$lower_rule, "natural") && is.finite(support$lower)) {
      if (any(y_obs_vals < support$lower))
        stop("Observed outcome values fall below the verified natural lower bound.", call. = FALSE)
      y_lower <- support$lower - cfg$outcome$continuous_bound_eps
    } else {
      y_lower <- min(y_obs_vals, na.rm = TRUE) - cfg$outcome$continuous_bound_eps
    }
    if (is.finite(support$upper) && any(y_obs_vals > support$upper))
      stop("Observed outcome values exceed the verified natural upper bound.", call. = FALSE)
    applied_upper <- min(q_up, support$upper)
    y_upper <- applied_upper + cfg$outcome$continuous_bound_eps
    cap_value <- applied_upper
  }
  y_range <- y_upper - y_lower
  if (!is.finite(y_range) || y_range <= 0) stop("Invalid bounded-Y range.", call. = FALSE)
  obs_i <- which(delta_Y == 1L & is.finite(Y_raw))
  Y_bounded_orig <- rep(NA_real_, length(Y_raw))
  Y_bounded_orig[obs_i] <- pmin(pmax(Y_raw[obs_i], y_lower), y_upper)
  Y_star <- rep(NA_real_, length(Y_raw))
  Y_star[obs_i] <- (Y_bounded_orig[obs_i] - y_lower) / y_range
  if (any(Y_star[obs_i] < -1e-12 | Y_star[obs_i] > 1 + 1e-12))
    stop("Canonical bounded outcome escaped [0,1].", call. = FALSE)
  list(df = df, A = A, Y_raw = Y_raw, Y_bounded_orig = Y_bounded_orig,
       Y_star = Y_star, delta_Y = delta_Y,
       weights = w_raw, cluster = cluster,
       strata = strata,
       outcome_type = outcome_type, y_lower = y_lower, y_upper = y_upper,
       y_range = y_range,
       cap_value = if (identical(outcome_type, "continuous")) cap_value else NA_real_,
       cap_weighted = identical(outcome_type, "continuous") &&
         isTRUE(cfg$outcome$continuous_cap_weighted %||% TRUE),
       cap_qrule = if (identical(outcome_type, "continuous"))
         cfg$outcome$continuous_cap_qrule %||% "hf8" else NA_character_)
}

make_final_cv_folds <- function(data_pack, cfg) {
  do.call(make_cluster_folds_balanced, c(list(
    cluster = data_pack$cluster, A = data_pack$A, k = cfg$final_tmle$vfolds,
    seed = seed_for(cfg, 7777L), weights = data_pack$weights,
    delta = data_pack$delta_Y,
    balance_on_weights = isTRUE(cfg$final_tmle$outer_fold_balance_on_weights)),
    fold_control_from_cfg(cfg, "outer")))
}

# Learn and apply final-W preprocessing recipes --------------------------------
# Semantic missing-code families and factor declarations come from the frozen,
# full-Wave-I outcome-blind dictionary. Only imputation, skip-level retention,
# factor levels, rare-level pooling, winsorization, scaling, dummy levels, and
# column retention are learned from the outer-fold training rows.

learn_final_missing_recipe <- function(df, cfg_pre) {
  df <- classify_factors_by_uniques(df, cfg_pre)
  numeric_names <- names(df)[vapply(df, function(z) is.numeric(z) || is.integer(z), logical(1))]
  assert_no_missing_indicator_name_collisions(
    names(df), numeric_names, context = "learn_final_missing_recipe")
  recipes <- list()
  miss_label  <- cfg_pre$factor_missing_label %||% "Missing"
  skip_label  <- cfg_pre$factor_skip_label %||% "Skip"
  other_label <- cfg_pre$factor_other_label %||% "_Other_"
  min_n_skip  <- cfg_pre$factor_special_code_min_n %||% 30L
  min_pr_skip <- cfg_pre$factor_special_code_min_prop %||% 0.02

  for (nm in names(df)) {
    col <- df[[nm]]
    rule <- get_missing_rule(col, cfg_pre, variable_name = nm)
    masks <- missing_masks_from_rule(col, rule)
    if (is.factor(col) || is.character(col)) {
      xc <- canonicalize_factor_text(col)
      missA <- masks$general; miss97 <- masks$skip
      assert_reserved_factor_labels_safe(
        col, missA, miss97, c(miss_label, skip_label, other_label),
        nm, "learn_final_missing_recipe")
      n_skip <- sum(miss97); pr_skip <- n_skip / length(xc)
      retain_skip <- n_skip >= min_n_skip || pr_skip >= min_pr_skip
      if (retain_skip) {
        xc[missA] <- NA_character_
        xc[miss97] <- skip_label
      } else {
        xc[missA | miss97] <- NA_character_
      }
      f <- addNA(factor(xc))
      levels(f)[is.na(levels(f))] <- miss_label
      levs <- unique(c(levels(f), other_label))
      recipes[[nm]] <- list(
        type = "factor", general_codes = rule$general_codes,
        skip_codes = rule$skip_codes, numeric_coded = rule$numeric_coded,
        classifier_rule = rule, retain_skip = retain_skip,
        levels = levs, missing_label = miss_label,
        skip_label = skip_label, other_label = other_label)
    } else {
      x <- masks$numeric
      fill <- compute_simple_impute(x[!(masks$general | masks$skip)],
                                    cfg_pre$numeric_imputation)
      recipes[[nm]] <- list(
        type = "numeric", fill = fill,
        general_codes = rule$general_codes, skip_codes = rule$skip_codes,
        numeric_coded = rule$numeric_coded, classifier_rule = rule)
    }
  }
  recipes
}

apply_final_missing_recipe <- function(df, recipes, cfg_pre) {
  n <- nrow(df)
  out <- data.frame(row_id_internal = seq_len(n))[0]
  numeric_support <- list()
  for (nm in names(recipes)) {
    rec <- recipes[[nm]]
    if (!nm %in% names(df))
      stop("apply_final_missing_recipe is missing required raw variable '", nm, "'.", call. = FALSE)
    col <- df[[nm]]
    rule <- rec$classifier_rule %||% list(
      numeric_coded = rec$numeric_coded %||% TRUE,
      general_codes = rec$general_codes %||% numeric(0),
      skip_codes = rec$skip_codes %||% numeric(0))
    masks <- missing_masks_from_rule(col, rule)
    missA <- masks$general; miss97 <- masks$skip
    if (identical(rec$type, "numeric")) {
      x <- masks$numeric
      originally_observed <- !(missA | miss97) & is.finite(x)
      numeric_support[[nm]] <- list(n_obs = sum(originally_observed),
                                    p_obs = mean(originally_observed))
      x[!originally_observed] <- rec$fill
      out[[nm]] <- x
      # Numeric missingness indicators remain exact 0/1 variables and are not
      # winsorized or standardized later. They include coded nonresponse and
      # structural skips, not only native NA values.
      out[[paste0(nm, "_missA")]]  <- as.integer(missA)
      out[[paste0(nm, "_miss97")]] <- as.integer(miss97)
      numeric_support[[paste0(nm, "_missA")]] <- list(n_obs = n, p_obs = 1)
      numeric_support[[paste0(nm, "_miss97")]] <- list(n_obs = n, p_obs = 1)
    } else {
      xc <- canonicalize_factor_text(col)
      if (isTRUE(rec$retain_skip)) {
        xc[missA] <- NA_character_
        xc[miss97] <- rec$skip_label
      } else {
        xc[missA | miss97] <- NA_character_
      }
      xc[is.na(xc)] <- rec$missing_label
      xc[!(xc %in% rec$levels)] <- rec$other_label
      out[[nm]] <- factor(xc, levels = rec$levels)
    }
  }
  attr(out, "numeric_support") <- numeric_support
  out
}


# Priority-aware ordering for final-W post-expansion caps. This avoids letting
# arbitrary union/traversal order decide which raw variables survive when factor
# expansion exceeds the processed-column cap. Variables with outcome,
# missingness, and joint A/Y evidence are protected ahead of A-only variables.
order_vars_for_final_cap <- function(vars, priority_table = NULL) {
  vars <- unique(vars)
  if (length(vars) == 0L || is.null(priority_table) || !"variable" %in% names(priority_table)) {
    return(vars)
  }
  pt <- priority_table[match(vars, priority_table$variable), , drop = FALSE]
  flag <- function(nm) if (nm %in% names(pt)) isTRUE_vec(pt[[nm]]) else rep(FALSE, nrow(pt))
  score <- function(nm) if (nm %in% names(pt)) suppressWarnings(as.numeric(pt[[nm]])) else rep(NA_real_, nrow(pt))
  lasso_y <- flag("selected_by_lasso_Y")
  joint   <- flag("selected_by_joint_AY")
  lasso_d <- flag("selected_by_lasso_delta")
  outc    <- flag("selected_by_outcome")
  delt    <- flag("selected_by_delta")
  lasso_a <- flag("selected_by_lasso_A")
  both_ay <- lasso_a & lasso_y
  aonly   <- flag("selected_by_exposure_only") | flag("selected_by_exposure_candidate_for_lasso")
  priority <- ifelse(both_ay, 1L,
              ifelse(joint, 2L,
              ifelse(lasso_y, 3L,
              ifelse(lasso_d, 4L,
              ifelse(outc, 5L,
              ifelse(delt, 6L,
              ifelse(lasso_a, 7L,
              ifelse(aonly, 8L, 9L))))))))
  best_score <- pmax(score("outcome_score"), score("delta_score"), score("exposure_score"), na.rm = TRUE)
  best_score[!is.finite(best_score)] <- -Inf
  vars[order(priority, -best_score, vars)]
}

isTRUE_vec <- function(x) {
  if (is.null(x)) return(logical(0))
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}

build_final_W_train_valid <- function(train_df, valid_df, selected_vars, cfg, priority_table = NULL, processed_cap_override = NULL) {
  cand_vars <- intersect(unique(selected_vars), names(train_df))
  cand_vars <- order_vars_for_final_cap(cand_vars, priority_table)
  protected_W <- intersect(cfg$final_tmle$protected_W %||% character(0), names(train_df))
  mandatory_W <- intersect(get_mandatory_W(cfg), names(train_df))
  cand_vars <- c(mandatory_W, setdiff(cand_vars, mandatory_W))
  original_cand_vars <- cand_vars
  missing_selected <- setdiff(unique(selected_vars), names(train_df))
  if (!length(cand_vars))
    stop("Final W construction received no valid selected variables.", call. = FALSE)

  t0 <- proc.time()[3]
  nonprotected_budget <- as.integer(processed_cap_override %||%
    cfg$final_tmle$final_max_processed_columns %||% 260L)
  hard_total <- as.integer(cfg$final_tmle$hard_max_processed_columns %||% 450L)
  if (!is.finite(nonprotected_budget) || nonprotected_budget < 1L ||
      !is.finite(hard_total) || hard_total < 1L)
    stop("Invalid processed-column budgets.", call. = FALSE)

  raw_all_tr <- train_df[, cand_vars, drop = FALSE]
  miss_all <- learn_final_missing_recipe(raw_all_tr, cfg$preprocessing)
  proc_all <- apply_final_missing_recipe(raw_all_tr, miss_all, cfg$preprocessing)
  support_all <- attr(proc_all, "numeric_support") %||% list()
  A_factor <- normalize_binary_var(train_df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)

  # Count each raw variable's processed representation before constructing the
  # combined matrix. This makes the engineering budget preventive rather than
  # attempting to trim only after an oversized design already exists.
  processed_counts <- setNames(integer(length(cand_vars)), cand_vars)
  for (v in cand_vars) {
    cols <- intersect(names(proc_all), c(v, paste0(v, "_missA"), paste0(v, "_miss97")))
    one <- proc_all[, cols, drop = FALSE]
    attr(one, "numeric_support") <- support_all[intersect(names(support_all), cols)]
    d <- build_grouped_design_train(one, cfg$final_preprocess, cfg$preprocessing,
                                    hard_max_cols = hard_total, A = A_factor,
                                    protected_raw_vars = protected_W)
    processed_counts[[v]] <- ncol(d$X)
  }
  if (any(processed_counts[protected_W] < 1L)) {
    bad <- protected_W[processed_counts[protected_W] < 1L]
    stop("Protected H1FS item(s) produced no usable processed columns: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  keep_vars <- mandatory_W
  used_nonmandatory <- 0L
  for (v in setdiff(cand_vars, mandatory_W)) {
    add <- processed_counts[[v]]
    if (add < 1L) next
    if (used_nonmandatory + add <= nonprotected_budget) {
      keep_vars <- c(keep_vars, v)
      used_nonmandatory <- used_nonmandatory + add
    }
  }
  keep_vars <- unique(keep_vars)
  if (!length(keep_vars)) stop("Processed-column allocation retained no variables.", call. = FALSE)
  expected_total <- sum(processed_counts[keep_vars])
  if (expected_total > hard_total)
    stop(sprintf("Final design requires %d processed columns, exceeding hard total guard %d.",
                 expected_total, hard_total), call. = FALSE)

  raw_tr <- train_df[, keep_vars, drop = FALSE]
  raw_te <- valid_df[, keep_vars, drop = FALSE]
  miss_rec <- learn_final_missing_recipe(raw_tr, cfg$preprocessing)
  proc_tr <- apply_final_missing_recipe(raw_tr, miss_rec, cfg$preprocessing)
  proc_te <- apply_final_missing_recipe(raw_te, miss_rec, cfg$preprocessing)
  des_tr <- build_grouped_design_train(proc_tr, cfg$final_preprocess, cfg$preprocessing,
                                       hard_max_cols = hard_total, A = A_factor,
                                       protected_raw_vars = protected_W)
  if (!ncol(des_tr$X)) stop("Final W preprocessing produced an empty training design.", call. = FALSE)
  if (ncol(des_tr$X) > hard_total) stop("Final design exceeded the hard total column guard.", call. = FALSE)

  X_te <- apply_preprocess_recipe(proc_te, des_tr$recipes)
  tr_cols <- colnames(des_tr$X)
  missing_in_te <- setdiff(tr_cols, colnames(X_te))
  extra_in_te <- setdiff(colnames(X_te), tr_cols)
  if (length(missing_in_te)) {
    add <- matrix(0, nrow(X_te), length(missing_in_te), dimnames = list(NULL, missing_in_te))
    X_te <- cbind(X_te, add)
  }
  if (length(extra_in_te)) X_te <- X_te[, !colnames(X_te) %in% extra_in_te, drop = FALSE]
  X_te <- X_te[, tr_cols, drop = FALSE]

  W_tr <- as.data.frame(des_tr$X, check.names = FALSE)
  W_te <- as.data.frame(X_te, check.names = FALSE)
  raw_map <- strip_missing_suffix(names(des_tr$recipes)[des_tr$group])
  protected_map <- table(factor(raw_map[raw_map %in% protected_W], levels = protected_W))
  mandatory_map <- table(factor(raw_map[raw_map %in% mandatory_W], levels = mandatory_W))
  if (length(mandatory_W) && any(mandatory_map < 1L)) {
    bad <- mandatory_W[mandatory_map < 1L]
    stop("At least one mandatory W variable disappeared from the final processed design: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (length(protected_W) && any(protected_map < 1L))
    stop("At least one protected H1FS item disappeared from the final processed design.", call. = FALSE)
  protected_level_audit <- do.call(rbind, lapply(protected_W, function(v) {
    rec <- des_tr$recipes[[v]]
    if (is.null(rec) || !identical(rec$type, "factor")) {
      return(data.frame(variable = v, observed_substantive_levels = NA_character_,
                        retained_recipe_levels = NA_character_,
                        all_observed_levels_retained = FALSE,
                        stringsAsFactors = FALSE))
    }
    observed <- rec$prep$observed_substantive_levels %||% character(0)
    retained <- rec$prep$levels %||% character(0)
    missing_levels <- setdiff(observed, retained)
    data.frame(variable = v,
               observed_substantive_levels = paste(observed, collapse = ";"),
               retained_recipe_levels = paste(retained, collapse = ";"),
               all_observed_levels_retained = length(missing_levels) == 0L,
               stringsAsFactors = FALSE)
  }))
  if (length(protected_W) &&
      (!is.data.frame(protected_level_audit) ||
       any(!protected_level_audit$all_observed_levels_retained))) {
    bad <- protected_level_audit$variable[!protected_level_audit$all_observed_levels_retained]
    stop("Protected H1FS substantive level(s) were lost during final preprocessing: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(cfg$preprocessing$sanitize_column_names_for_model_matrix)) {
    clean_names <- make.unique(gsub("[^A-Za-z0-9_]", "_", names(W_tr)))
    names(W_tr) <- clean_names; names(W_te) <- clean_names
  }
  n_protected <- sum(raw_map %in% protected_W)
  n_mandatory <- sum(raw_map %in% mandatory_W)
  n_nonprotected <- ncol(W_tr) - n_protected
  n_budgeted <- ncol(W_tr) - n_mandatory
  if (n_budgeted > nonprotected_budget)
    stop("Final optional design exceeded its declared processed-column budget.", call. = FALSE)
  message(sprintf("  [final W] retained %d mandatory (%d protected H1FS) and %d budgeted processed columns (budget=%d; total=%d; hard total=%d).",
                  n_mandatory, n_protected, n_budgeted, nonprotected_budget, ncol(W_tr), hard_total))

  list(train = W_tr, valid = W_te, n_raw = length(keep_vars),
       n_original_raw = length(original_cand_vars), kept_raw_vars = keep_vars,
       dropped_by_column_cap = setdiff(original_cand_vars, keep_vars),
       n_missing_selected = length(missing_selected), n_processed = ncol(W_tr),
       n_processed_nonprotected = n_nonprotected,
       n_processed_optional_budgeted = n_budgeted,
       n_protected_cols = n_protected,
       n_mandatory_cols = n_mandatory,
       mandatory_raw_to_processed = as.integer(mandatory_map),
       mandatory_raw_names = mandatory_W,
       protected_raw_to_processed = as.integer(protected_map),
       protected_raw_names = protected_W,
       processed_raw_map = stats::setNames(raw_map, names(W_tr)),
       protected_level_audit = protected_level_audit,
       nonprotected_budget = nonprotected_budget,
       hard_total_budget = hard_total, missing_in_validation = length(missing_in_te),
       extra_in_validation = length(extra_in_te), seconds = proc.time()[3] - t0)
}


# high-dimensional safety helpers ---------------------------------------
truthy_or_false <- function(x) isTRUE(x)

compact_var_signature_for_duplicate <- function(x, rule = NULL) {
  if (!is.null(rule)) {
    masks <- missing_masks_from_rule(x, rule)
    token <- rep("<NA>", length(x))
    token[masks$general] <- "<GENERAL_MISSING>"
    token[masks$skip] <- "<STRUCTURAL_SKIP>"
    if (isTRUE(rule$numeric_coded)) {
      z <- masks$numeric
      substantive <- !(masks$general | masks$skip) & is.finite(z)
      token[substantive] <- format(round(z[substantive], 8), scientific = FALSE)
    } else {
      xc <- as.character(x)
      substantive <- !(masks$general | masks$skip) & !is.na(xc)
      token[substantive] <- xc[substantive]
    }
    return(paste(token, collapse = "\r"))
  }
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    z <- suppressWarnings(as.numeric(x))
    z[!is.finite(z)] <- NA_real_
    paste(ifelse(is.na(z), "<NA>", format(round(z, 8), scientific = FALSE)), collapse = "\r")
  } else {
    z <- as.character(x)
    paste(ifelse(is.na(z), "<NA>", z), collapse = "\r")
  }
}

prefilter_candidate_vars_for_screen <- function(df, vars, cfg, fold_id = NA_integer_) {
  vars <- unique(vars[vars %in% names(df)])
  if (!isTRUE(cfg$final_tmle$rough_prefilter_enable %||% TRUE) || length(vars) == 0L) {
    return(list(vars = vars, log = data.frame(variable = vars, prefilter_kept = TRUE,
      prefilter_reason = "not_run", stringsAsFactors = FALSE)))
  }
  max_miss <- cfg$final_tmle$rough_prefilter_max_missing_prop %||% 0.95
  min_obs  <- cfg$final_tmle$rough_prefilter_min_observed_n %||% 30L
  min_uniq <- cfg$final_tmle$rough_prefilter_min_unique %||% 2L
  min_nonmodal <- cfg$final_tmle$rough_prefilter_min_nonmodal_n %||% 3L
  # two-arm estimability rule parameters and exposure vector.
  two_arm_min_n <- cfg$final_tmle$rough_prefilter_two_arm_min_n %||% 0L
  two_arm_max_levels <- cfg$final_tmle$rough_prefilter_two_arm_max_levels %||% 3L
  A_vec <- NULL
  if (two_arm_min_n > 0L) {
    ev <- cfg$analysis$exposure_var
    if (!is.null(ev) && ev %in% names(df)) {
      A_vec <- suppressWarnings(as.integer(as.character(df[[ev]])))
      if (length(unique(A_vec[is.finite(A_vec)])) < 2L) A_vec <- NULL
    }
  }
  log_rows <- vector("list", length(vars))
  keep <- rep(TRUE, length(vars))
  for (i in seq_along(vars)) {
    v <- vars[[i]]; x <- df[[v]]
    rule <- get_missing_rule(x, cfg$preprocessing, variable_name = v)
    masks <- missing_masks_from_rule(x, rule)
    obs <- !(masks$general | masks$skip)
    if (is.numeric(x) || is.integer(x)) vals <- masks$numeric[obs]
    else vals <- as.character(x)[obs]
    n_obs <- sum(obs); miss_prop <- 1 - n_obs / length(x)
    n_uniq <- length(unique(vals))
    nonmodal <- 0L
    if (n_obs > 0L && n_uniq > 0L) {
      tab <- table(vals, useNA = "no")
      nonmodal <- n_obs - max(tab)
    }
    reason <- "kept"
    if (miss_prop > max_miss) reason <- "too_missing"
    else if (n_obs < min_obs) reason <- "too_few_observed"
    else if (n_uniq < min_uniq) reason <- "too_few_unique"
    else if (nonmodal < min_nonmodal) reason <- "near_constant"
    # two-arm minimum-cell rule for (near-)binary indicators. Applied
    # only after the variable passed the basic checks above, only when an
    # exposure vector is available, and only to low-cardinality indicators.
    # capture two-arm audit fields so the exclusion reason is fully
    # transparent (which minority level, how many exposed/unexposed, threshold).
    ta_minority_level <- NA_character_
    ta_n_exp_present  <- NA_integer_
    ta_n_unexp_present <- NA_integer_
    ta_threshold      <- NA_integer_
    if (identical(reason, "kept") && !is.null(A_vec) &&
        n_uniq >= 2L && n_uniq <= two_arm_max_levels) {
      # Minority (rarest) SUBSTANTIVE observed level is the cell that drives
      # rare-cell positivity failures. `vals` already excludes only the exact
      # codes recognized by the frozen complete-Wave-I classifier, so no
      # additional blanket removal of 6-9 or 96-99 is performed here.
      vals_ta <- as.character(vals)
      tab_obs <- table(vals_ta, useNA = "no")
      if (length(tab_obs) >= 2L) {
        minority_level <- names(tab_obs)[which.min(as.integer(tab_obs))]
        present <- obs
        present[obs] <- as.character(vals) == minority_level
        a_known <- is.finite(A_vec)
        n_exp_present   <- sum(present & a_known & A_vec == 1L, na.rm = TRUE)
        n_unexp_present <- sum(present & a_known & A_vec == 0L, na.rm = TRUE)
        ta_minority_level  <- as.character(minority_level)
        ta_n_exp_present   <- as.integer(n_exp_present)
        ta_n_unexp_present <- as.integer(n_unexp_present)
        ta_threshold       <- as.integer(two_arm_min_n)
        if (n_exp_present < two_arm_min_n || n_unexp_present < two_arm_min_n)
          reason <- "rare_cell_two_arm"
      }
    }
    keep[[i]] <- identical(reason, "kept")
    log_rows[[i]] <- data.frame(
      fold = fold_id, variable = v, prefilter_kept = keep[[i]],
      prefilter_reason = reason, missing_prop = miss_prop,
      n_observed = n_obs, n_unique = n_uniq, n_nonmodal = nonmodal,
      missing_classifier_support = rule$support_type,
      questionnaire_source = isTRUE(rule$questionnaire_source),
      categorical_questionnaire = isTRUE(rule$categorical_questionnaire),
      forced_factor = isTRUE(rule$forced_factor),
      known_codebook_overlap = v %in% (cfg$preprocessing$known_codebook_overlap_vars %||% character(0)),
      percentage_like = isTRUE(rule$percentage_like),
      dense_small_count = isTRUE(rule$dense_small_count),
      missing_scheme_decision = rule$scheme_decision,
      missing_classifier_families = paste(rule$recognized_families, collapse = ";"),
      exact_general_codes = paste(rule$general_codes, collapse = ";"),
      exact_skip_codes = paste(rule$skip_codes, collapse = ";"),
      minority_level = ta_minority_level,
      n_exp_present = ta_n_exp_present,
      n_unexp_present = ta_n_unexp_present,
      two_arm_threshold = ta_threshold,
      stringsAsFactors = FALSE)
  }
  log_df <- do.call(rbind, log_rows)

  # Optional exact-duplicate removal after basic missingness/variation filters.
  if (isTRUE(cfg$final_tmle$rough_prefilter_drop_exact_duplicates %||% TRUE) && any(keep)) {
    kept_vars <- vars[keep]
    sig <- vapply(kept_vars, function(v) {
      rule <- get_missing_rule(df[[v]], cfg$preprocessing, variable_name = v)
      compact_var_signature_for_duplicate(df[[v]], rule = rule)
    }, character(1))
    dup <- duplicated(sig)
    if (any(dup)) {
      dup_vars <- kept_vars[dup]
      log_df$prefilter_kept[match(dup_vars, log_df$variable)] <- FALSE
      log_df$prefilter_reason[match(dup_vars, log_df$variable)] <- "exact_duplicate"
      keep[match(dup_vars, vars)] <- FALSE
    }
  }
  out_vars <- vars[keep]
  n_rare_cell <- sum(log_df$prefilter_reason == "rare_cell_two_arm")
  message(sprintf("    [fold %s prefilter] %d -> %d raw candidates; dropped %d (incl. %d rare-cell two-arm).",
                  as.character(fold_id), length(vars), length(out_vars),
                  length(vars) - length(out_vars), n_rare_cell))
  if (length(out_vars) == 0L) stop("Candidate prefilter removed every candidate variable.", call. = FALSE)
  list(vars = out_vars, log = log_df)
}

kish_ess_safe <- function(w) {
  w <- as.numeric(w); w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  (sum(w)^2) / sum(w^2)
}

internal_fold_cell_counts <- function(ix, A_vec, delta_vec, weights) {
  tr <- setdiff(seq_along(A_vec), ix)
  one <- function(rows, a, d) {
    hit <- rows[A_vec[rows] == a & delta_vec[rows] == d]
    c(n = length(hit), ess = kish_ess_safe(weights[hit]))
  }
  cells <- expand.grid(a = 0:1, d = 0:1)
  out <- list()
  for (j in seq_len(nrow(cells))) {
    a <- cells$a[j]; d <- cells$d[j]; nm <- paste0("a", a, "d", d)
    vv <- one(ix, a, d); tt <- one(tr, a, d)
    out[[paste0("valid_n_", nm)]] <- vv[["n"]]
    out[[paste0("valid_ess_", nm)]] <- vv[["ess"]]
    out[[paste0("train_n_", nm)]] <- tt[["n"]]
    out[[paste0("train_ess_", nm)]] <- tt[["ess"]]
  }
  out
}

warn_internal_valid_rows <- function(validRows, A_vec, label, cfg, fold_id, delta_vec = NULL, weights = NULL) {
  if (!length(validRows)) stop("Internal CV produced no validation folds.", call. = FALSE)
  if (is.null(delta_vec)) delta_vec <- rep(1L, length(A_vec))
  if (is.null(weights)) weights <- rep(1, length(A_vec))
  A_vec <- as.integer(A_vec); delta_vec <- as.integer(delta_vec); weights <- as.numeric(weights)
  global_cells <- table(factor(A_vec, levels = 0:1), factor(delta_vec, levels = 0:1))
  required <- which(global_cells > 0, arr.ind = TRUE)
  rows <- list()
  for (j in seq_along(validRows)) {
    ix <- validRows[[j]]
    cc <- internal_fold_cell_counts(ix, A_vec, delta_vec, weights)
    for (r in seq_len(nrow(required))) {
      a <- required[r, 1] - 1L; d <- required[r, 2] - 1L; nm <- paste0("a", a, "d", d)
      if (cc[[paste0("valid_n_", nm)]] < 1L || cc[[paste0("train_n_", nm)]] < 1L)
        stop(sprintf("Fold %d internal %s CV lacks support for A=%d, delta=%d in validation or its training complement.",
                     fold_id, label, a, d), call. = FALSE)
    }
    rows[[j]] <- cc
  }
  treated <- vapply(validRows, function(ix) sum(A_vec[ix] == 1L), integer(1))
  observed_treated <- vapply(validRows, function(ix) sum(A_vec[ix] == 1L & delta_vec[ix] == 1L), integer(1))
  ess_t <- vapply(validRows, function(ix) kish_ess_safe(weights[ix][A_vec[ix] == 1L]), numeric(1))
  message(sprintf("    [fold %d] Internal %s support: treated=[%s], observed-treated=[%s], treated ESS=[%s].",
                  fold_id, label, paste(treated, collapse = ","),
                  paste(observed_treated, collapse = ","),
                  paste(sprintf("%.1f", ess_t), collapse = ",")))
  min_t <- cfg$final_tmle$internal_fold_min_treated_warning_n %||% 8L
  min_ot <- cfg$final_tmle$internal_fold_min_observed_treated_warning_n %||% 6L
  if (any(treated < min_t))
    warning(sprintf("Fold %d internal %s CV has validation treated count below %d.", fold_id, label, min_t), call. = FALSE)
  if (any(observed_treated < min_ot))
    warning(sprintf("Fold %d internal %s CV has validation observed-treated count below %d.", fold_id, label, min_ot), call. = FALSE)
  invisible(rows)
}


make_wave1_cache_fingerprint <- function(cfg) {
  list(version = cfg$global$version %||% "v8.28_final_production",
       script = pipeline_script_fingerprint(
      cfg, strict = isTRUE(cfg$global$require_script_md5 %||% FALSE)),
       paths = lapply(cfg$paths[c("wave1_inhome", "birth_records", "neighborhood_w1", "inschool_w1",
                                  "contextual_w1", "health_w1", "spatial_w1", "stchr95_w1",
                                  "polcon_w1", "weights_w1", "school_admin_w1")], file_fingerprint),
       dictionary_exclusions = sort(get_common_exclusion_vars(cfg, include_analysis_outputs = FALSE)),
       analysis_roles = cfg$analysis[c("id_var", "cluster_var", "strata_var", "weight_var",
                                       "extra_exclude_from_candidates", "transform_time_variables")],
       exposure_definition = cfg$exposure,
       outcome_drop_vars = sort(get_outcome_drop_vars(cfg)),
       causal_governance = cfg$causal_governance,
       mortality_sensitivity = cfg$mortality_sensitivity,
       preprocessing_core = cfg$preprocessing[setdiff(names(cfg$preprocessing),
                                                       c("global_missing_dictionary",
                                                         "variable_source_registry"))])
}

load_wave1_cache <- function(path, cfg) {
  obj <- readRDS(path)
  current_fp <- make_wave1_cache_fingerprint(cfg)
  if (is.list(obj) && !is.null(obj$data) && !is.null(obj$fingerprint)) {
    if (identical(obj$fingerprint, current_fp)) {
      if (isTRUE(cfg$preprocessing$global_missing_dictionary_required %||% TRUE) &&
          (is.null(attr(obj$data, "global_missing_dictionary")) ||
           !length(attr(obj$data, "global_missing_dictionary")))) {
        message("  [cache] Cached Wave 1 merge lacks the frozen missing-code dictionary; rebuilding.")
        return(NULL)
      }
      if (is.null(attr(obj$data, "full_survey_design_frame")) ||
          !is.data.frame(attr(obj$data, "full_survey_design_frame"))) {
        message("  [cache] Cached Wave 1 merge lacks the full survey-design frame; rebuilding.")
        return(NULL)
      }
      registry <- attr(obj$data, "variable_source_registry")
      if (is.null(registry)) {
        message("  [cache] Cached Wave 1 merge lacks the descriptive source registry; rebuilding.")
        return(NULL)
      }
      validate_variable_source_registry(registry, "cached Wave-I source registry")
      return(obj$data)
    }
    msg <- "  [cache] Cached Wave 1 merge fingerprint is stale."
    if (isTRUE(cfg$safety$stop_on_stale_wave1_cache %||% FALSE)) stop(msg, call. = FALSE)
    message(msg, " Rebuilding Wave 1 merge.")
    return(NULL)
  }
  if (isTRUE(cfg$cache$use_wave1_fingerprint %||% TRUE)) {
    message("  [cache] Cached Wave 1 merge lacks fingerprint; rebuilding Wave 1 merge.")
    return(NULL)
  }
  obj
}

save_wave1_cache <- function(w1_all, path, cfg) {
  if (isTRUE(cfg$cache$use_wave1_fingerprint %||% TRUE)) {
    atomic_save_rds(list(data = w1_all, fingerprint = make_wave1_cache_fingerprint(cfg)), path, overwrite = TRUE)
  } else {
    atomic_save_rds(w1_all, path, overwrite = TRUE)
  }
}

make_main_dataset_cache_fingerprint <- function(cfg, w1_all = NULL) {
  list(
    version = cfg$global$version %||% "v8.28_final_production",
    script = pipeline_script_fingerprint(
      cfg, strict = isTRUE(cfg$global$require_script_md5 %||% FALSE)),
    outcome_family = cfg$outcome$family,
    # include the ACTIVE family's full config so two runs of the same
    # family with different settings (e.g. a PassThrough negative control with a
    # different source_var) do not silently reuse each other's cached outcome Y.
    outcome_family_config = cfg$outcome$families[[cfg$outcome$family]],
    outcome_wave = cfg$outcome$current_wave %||% cfg$outcome$waves,
    family_member = cfg$outcome$family_member %||% NA_character_,
    exposure_definition = cfg$exposure,
    weight_var = cfg$analysis$weight_var,
    cluster_var = cfg$analysis$cluster_var,
    strata_var = cfg$analysis$strata_var,
    exposure_var = cfg$analysis$exposure_var,
    outcome_var = cfg$analysis$outcome_var,
    sample_gates = cfg$analysis[c("enforce_expected_sample_gates",
                                  "enforce_expected_cutpoint_gate",
                                  "enforce_expected_treated_gate",
                                  "expected_complete_cesd_n", "expected_final_n",
                                  "expected_exposure_cutpoint", "expected_treated_n",
                                  "expected_cluster_n", "expected_strata_n")],
    # winsorization changes the stored weight column, so it must be
    # part of the cache identity or a changed quantile would silently reuse
    # a stale dataset.
    weight_winsor_quantile = cfg$analysis$weight_winsor_quantile %||% NA_real_,
    transform_time_variables = isTRUE(cfg$analysis$transform_time_variables),
    weight_winsor_renormalize = isTRUE(cfg$analysis$weight_winsor_renormalize),
    paths = lapply(cfg$paths, file_fingerprint),
    log_transform = cfg$outcome$log_transform,
    compensation_transform = cfg$outcome$compensation_transform,
    compensation_asinh_scale = cfg$outcome$compensation_asinh_scale,
    compensation_exact_only = cfg$outcome$compensation_exact_only,
    continuous_upper_quantile = cfg$outcome$continuous_upper_quantile,
    continuous_cap_weighted = cfg$outcome$continuous_cap_weighted,
    continuous_cap_censoring_adjusted = cfg$outcome$continuous_cap_censoring_adjusted,
    continuous_cap_qrule = cfg$outcome$continuous_cap_qrule,
    mortality_sensitivity = cfg$mortality_sensitivity,
    diagnostic_construction = cfg$diagnostics[c(
      "enable_wave2_completion_diagnostic"
    )],
    causal_governance = cfg$causal_governance,
    common_exclusions = sort(get_common_exclusion_vars(cfg)),
    preprocessing_core = cfg$preprocessing[setdiff(names(cfg$preprocessing),
                                                    c("global_missing_dictionary",
                                                      "variable_source_registry"))],
    global_missing_dictionary_md5 = if (is.null(w1_all)) NA_character_ else
      object_md5(attr(w1_all, "global_missing_dictionary")),
    variable_source_registry_md5 = if (is.null(w1_all)) NA_character_ else
      object_md5(attr(w1_all, "variable_source_registry")),
    n_w1 = if (is.null(w1_all)) NA_integer_ else nrow(w1_all),
    names_w1 = if (is.null(w1_all)) character(0) else names(w1_all)
  )
}

load_main_dataset_cache <- function(path, cfg, w1_all) {
  obj <- readRDS(path)
  current_fp <- make_main_dataset_cache_fingerprint(cfg, w1_all)
  if (is.list(obj) && !is.null(obj$data) && !is.null(obj$fingerprint)) {
    if (identical(obj$fingerprint, current_fp)) {
      if (isTRUE(cfg$preprocessing$global_missing_dictionary_required %||% TRUE) &&
          (is.null(attr(obj$data, "global_missing_dictionary")) ||
           !length(attr(obj$data, "global_missing_dictionary")))) {
        message("  [cache] Cached main dataset lacks the frozen missing-code dictionary; rebuilding.")
        return(NULL)
      }
      if (is.null(attr(obj$data, "survey_design_frame")) ||
          !is.data.frame(attr(obj$data, "survey_design_frame"))) {
        message("  [cache] Cached main dataset lacks the survey-domain design frame; rebuilding.")
        return(NULL)
      }
      registry <- attr(obj$data, "variable_source_registry")
      if (is.null(registry)) {
        message("  [cache] Cached main dataset lacks the descriptive source registry; rebuilding.")
        return(NULL)
      }
      validate_variable_source_registry(registry, "cached main-data source registry")
      if (isTRUE(cfg$diagnostics$enable_wave2_completion_diagnostic %||% TRUE)) {
        completion_status <- attr(obj$data, "wave2_completion_status", exact = TRUE)
        if (is.null(completion_status) || !is.data.frame(completion_status)) {
          message("  [cache] Cached main dataset lacks the Wave-II completion-status frame; rebuilding.")
          return(NULL)
        }
      }
      attr(obj$data, "main_dataset_cache_fingerprint") <- obj$fingerprint
      return(obj$data)
    }
    message("  [cache] Cached main dataset fingerprint is stale; rebuilding main dataset.")
    return(NULL)
  }
  if (isTRUE(cfg$cache$use_main_dataset_fingerprint %||% TRUE)) {
    message("  [cache] Cached main dataset lacks fingerprint; rebuilding main dataset.")
    return(NULL)
  }
  obj
}

save_main_dataset_cache <- function(main_df, path, cfg, w1_all) {
  if (isTRUE(cfg$cache$use_main_dataset_fingerprint %||% TRUE)) {
    fp <- make_main_dataset_cache_fingerprint(cfg, w1_all)
    attr(main_df, "main_dataset_cache_fingerprint") <- fp
    atomic_save_rds(list(data = main_df, fingerprint = fp), path, overwrite = TRUE)
  } else {
    atomic_save_rds(main_df, path, overwrite = TRUE)
  }
}

# Nested data-driven screen run on a final TMLE fold's training rows only.
# This is the reviewer-responsive selection step. It uses marginal rough
# screens only as a broad first pass, applies data-driven redundancy control,
# and optionally runs a nested multivariable elastic-net screen on the rough
# candidate pool. It does NOT force inclusion of exposure-only predictors.

top_score_names <- function(scores, n) {
  s <- scores[is.finite(scores)]
  if (length(s) == 0L || n <= 0L) return(character(0))
  names(sort(s, decreasing = TRUE))[seq_len(min(length(s), n))]
}

top_joint_score_names <- function(exposure_scores, outcome_scores, n) {
  if (n <= 0L) return(character(0))
  common <- intersect(names(exposure_scores), names(outcome_scores))
  common <- common[is.finite(exposure_scores[common]) & is.finite(outcome_scores[common])]
  if (length(common) == 0L) return(character(0))
  rA <- rank(-exposure_scores[common], ties.method = "average")
  rY <- rank(-outcome_scores[common], ties.method = "average")
  joint_rank <- rA + rY
  names(sort(joint_rank, decreasing = FALSE))[seq_len(min(length(joint_rank), n))]
}

strip_missing_suffix <- function(x) sub("(_missA|_miss97)$", "", x)

score_summary_msg <- function(scores) {
  s <- scores[is.finite(scores)]
  if (length(s) == 0L) return("no finite scores")
  sprintf("n=%d, min=%.4f, median=%.4f, max=%.4f",
          length(s), min(s), stats::median(s), max(s))
}

# Build one empirical signature per raw variable for data-driven redundancy
# control. Numeric variables use their imputed standardized value; factor
# variables use the first singular vector of their one-variable dummy matrix.
# This avoids manual grouping while still preventing a single correlated block
# from consuming the rough-screen shortlist.
make_screen_signature_matrix <- function(vars, X_base, impute_method = "median") {
  vars <- unique(vars[vars %in% names(X_base)])
  if (length(vars) == 0L) return(matrix(0, nrow = nrow(X_base), ncol = 0))
  sigs <- list()
  for (nm in vars) {
    Xnm <- tryCatch(build_single_var_screen_df(nm, X_base, impute_method = impute_method),
                    error = function(e) NULL)
    if (is.null(Xnm) || ncol(Xnm) == 0L) next
    M <- suppressWarnings(data.matrix(Xnm))
    M[!is.finite(M)] <- 0
    if (ncol(M) == 1L) {
      v <- M[, 1]
    } else {
      M <- scale(M)
      M[!is.finite(M)] <- 0
      v <- tryCatch({
        sv <- svd(M, nu = 1L, nv = 0L)
        as.numeric(sv$u[, 1L] * sv$d[1L])
      }, error = function(e) rowMeans(M))
    }
    if (!any(is.finite(v)) || stats::sd(v, na.rm = TRUE) <= 1e-12) next
    v <- as.numeric(scale(v))
    v[!is.finite(v)] <- 0
    sigs[[nm]] <- v
  }
  if (length(sigs) == 0L) return(matrix(0, nrow = nrow(X_base), ncol = 0))
  out <- do.call(cbind, sigs)
  colnames(out) <- names(sigs)
  out
}

greedy_redundancy_filter <- function(ranked_vars, X_base, max_vars,
                                     cor_threshold = 0.90,
                                     impute_method = "median",
                                     enable = TRUE) {
  ranked_vars <- unique(ranked_vars[ranked_vars %in% names(X_base)])
  if (length(ranked_vars) == 0L || max_vars <= 0L)
    return(list(selected = character(0), dropped = character(0)))
  if (!isTRUE(enable))
    return(list(selected = ranked_vars[seq_len(min(length(ranked_vars), max_vars))],
                dropped = character(0)))
  sig <- make_screen_signature_matrix(ranked_vars, X_base, impute_method = impute_method)
  selected <- character(0); dropped <- character(0)
  for (v in ranked_vars) {
    if (length(selected) >= max_vars) break
    if (!v %in% colnames(sig)) {
      selected <- c(selected, v)
      next
    }
    sig_selected <- intersect(selected, colnames(sig))
    if (length(sig_selected) == 0L) {
      selected <- c(selected, v)
      next
    }
    cors <- suppressWarnings(abs(stats::cor(sig[, v], sig[, sig_selected, drop = FALSE])))
    cors <- as.numeric(cors)
    if (any(is.finite(cors) & cors >= cor_threshold)) dropped <- c(dropped, v)
    else selected <- c(selected, v)
  }
  list(selected = selected, dropped = dropped)
}

# deterministic correlation-clustering redundancy filter.
# Replaces the greedy keep-first filter. Variables are grouped into clusters by
# their signature correlation structure using hierarchical clustering, which is
# order-independent and seed-independent (the same correlation matrix always
# yields the same clusters). Each cluster is represented by a single
# deterministically-chosen member: the variable whose signature is closest (in
# absolute correlation) to the cluster's SIGN-ALIGNED, standardized mean
# signature. Sign-alignment flips members that are negatively correlated with a
# reference before averaging, so the mean reinforces the shared signal instead
# of cancelling it. This eliminates the seed-dependent "which member survives"
# churn while keeping real variables (no synthetic columns injected downstream).
cluster_redundancy_filter <- function(ranked_vars, X_base, max_vars,
                                      cor_threshold = 0.90,
                                      linkage = "complete",
                                      impute_method = "median",
                                      enable = TRUE) {
  ranked_vars <- unique(ranked_vars[ranked_vars %in% names(X_base)])
  if (length(ranked_vars) == 0L || max_vars <= 0L)
    return(list(selected = character(0), dropped = character(0),
                n_clusters = 0L, n_singletons = 0L))
  if (!isTRUE(enable))
    return(list(selected = ranked_vars[seq_len(min(length(ranked_vars), max_vars))],
                dropped = character(0), n_clusters = NA_integer_, n_singletons = NA_integer_))

  sig <- make_screen_signature_matrix(ranked_vars, X_base, impute_method = impute_method)
  has_sig <- colnames(sig)
  # Variables without a usable signature cannot be clustered; pass them through
  # in their original (ranked) order as their own singletons.
  no_sig <- setdiff(ranked_vars, has_sig)

  reps <- character(0); dropped <- character(0); n_clusters <- 0L; n_singletons <- 0L

  if (length(has_sig) >= 2L) {
    # Correlation distance on |cor| so negatively-correlated (redundant)
    # variables are treated as close. Guard non-finite correlations to distance
    # 1 (treated as unrelated) so clustering never fails on degenerate columns.
    cmat <- suppressWarnings(stats::cor(sig[, has_sig, drop = FALSE]))
    cmat[!is.finite(cmat)] <- 0
    dmat <- 1 - abs(cmat)
    dmat[!is.finite(dmat)] <- 1
    diag(dmat) <- 0
    hc <- stats::hclust(stats::as.dist(dmat), method = linkage)
    # Cut so that members of a cluster are correlated >= cor_threshold. Under
    # complete linkage, cutting at height (1 - cor_threshold) guarantees every
    # pair within a cluster has |cor| >= cor_threshold.
    cut_h <- 1 - cor_threshold
    grp <- stats::cutree(hc, h = cut_h)
    # Process clusters in the order their first (highest-ranked) member appears,
    # so representative selection and the max_vars cap remain deterministic and
    # ranking-respecting at the cluster level.
    first_rank <- tapply(seq_along(has_sig), grp, min)
    cluster_order <- as.integer(names(sort(first_rank)))
    for (g in cluster_order) {
      members <- has_sig[grp == g]
      if (length(members) == 1L) {
        reps <- c(reps, members); n_singletons <- n_singletons + 1L
        next
      }
      n_clusters <- n_clusters + 1L
      S <- sig[, members, drop = FALSE]
      # Sign-align to the first member, then standardized mean.
      ref <- S[, 1L]
      aligned <- S
      for (j in seq_len(ncol(S))) {
        rj <- suppressWarnings(stats::cor(S[, j], ref))
        if (is.finite(rj) && rj < 0) aligned[, j] <- -S[, j]
      }
      mean_sig <- rowMeans(aligned)
      if (stats::sd(mean_sig) <= 1e-12) {
        # Degenerate mean: fall back to the highest-ranked member.
        rep_var <- members[1L]
      } else {
        cors_to_mean <- suppressWarnings(abs(stats::cor(S, mean_sig)))
        cors_to_mean[!is.finite(cors_to_mean)] <- -Inf
        rep_var <- members[which.max(cors_to_mean)]
      }
      reps <- c(reps, rep_var)
      dropped <- c(dropped, setdiff(members, rep_var))
    }
  } else {
    # 0 or 1 signature variable: nothing to cluster among the signatured set.
    reps <- c(reps, has_sig); n_singletons <- n_singletons + length(has_sig)
  }

  # Reassemble in the original ranked order (representatives + passthroughs),
  # then apply the max_vars cap deterministically.
  kept_set <- unique(c(reps, no_sig))
  selected <- ranked_vars[ranked_vars %in% kept_set]
  if (length(selected) > max_vars) {
    over <- selected[(max_vars + 1L):length(selected)]
    dropped <- unique(c(dropped, over))
    selected <- selected[seq_len(max_vars)]
  }
  list(selected = selected, dropped = unique(dropped),
       n_clusters = n_clusters, n_singletons = n_singletons)
}

# (Option A): de-duplicate the FULL candidate set by correlation
# clustering BEFORE marginal scoring. This prevents large blocks of mutually
# redundant variables (e.g. ~85 contextual CST/BST codes measuring one spatial
# construct) from consuming most of the top-N ranking slots and crowding out
# structurally distinct weak confounders. Because scoring has not happened yet,
# there is no ranking; clustering uses only the correlation structure
# (deterministic, seed-independent) and the stable column order of X_base for
# tie-breaking. Each cluster collapses to one deterministic representative (the
# member closest to the sign-aligned, standardized cluster mean). Returns the
# representative variable names plus all unclustered singletons. Guarded by a
# size cap: above max_cluster_vars the full O(n^2) correlation/hclust is skipped
# (returns all vars unchanged) to protect runtime/memory.
cluster_dedupe_candidates <- function(vars, X_base,
                                      cor_threshold = 0.90,
                                      linkage = "complete",
                                      impute_method = "median",
                                      max_cluster_vars = 4000L,
                                      enable = TRUE) {
  vars <- unique(vars[vars %in% names(X_base)])
  empty_assign <- data.frame(cluster_id = integer(0), cluster_size = integer(0),
               representative_variable = character(0), member_variable = character(0),
               correlation_to_representative = numeric(0), stringsAsFactors = FALSE)
  empty <- list(kept = vars, dropped = character(0),
                n_clusters = 0L, n_singletons = length(vars), skipped = FALSE,
                assignments = empty_assign)
  if (!isTRUE(enable) || length(vars) < 2L) return(empty)
  if (length(vars) > max_cluster_vars) {
    empty$skipped <- TRUE
    return(empty)
  }
  sig <- make_screen_signature_matrix(vars, X_base, impute_method = impute_method)
  has_sig <- colnames(sig)
  no_sig <- setdiff(vars, has_sig)
  if (length(has_sig) < 2L)
    return(list(kept = vars, dropped = character(0),
                n_clusters = 0L, n_singletons = length(vars), skipped = FALSE,
                assignments = empty_assign))

  cmat <- suppressWarnings(stats::cor(sig[, has_sig, drop = FALSE]))
  cmat[!is.finite(cmat)] <- 0
  dmat <- 1 - abs(cmat)
  dmat[!is.finite(dmat)] <- 1
  diag(dmat) <- 0
  hc  <- stats::hclust(stats::as.dist(dmat), method = linkage)
  grp <- stats::cutree(hc, h = 1 - cor_threshold)

  reps <- character(0); dropped <- character(0); n_clusters <- 0L; n_singletons <- 0L
  assign_rows <- list()   # per-member cluster-assignment diagnostic
  # Deterministic cluster order: by the first (lowest column-index) member, so
  # the result does not depend on anything seed-related.
  first_idx <- tapply(seq_along(has_sig), grp, min)
  for (g in as.integer(names(sort(first_idx)))) {
    members <- has_sig[grp == g]
    if (length(members) == 1L) {
      reps <- c(reps, members); n_singletons <- n_singletons + 1L
      assign_rows[[length(assign_rows) + 1L]] <- data.frame(
        cluster_id = g, cluster_size = 1L, representative_variable = members,
        member_variable = members, correlation_to_representative = 1,
        stringsAsFactors = FALSE)
      next
    }
    n_clusters <- n_clusters + 1L
    S <- sig[, members, drop = FALSE]
    ref <- S[, 1L]
    aligned <- S
    for (j in seq_len(ncol(S))) {
      rj <- suppressWarnings(stats::cor(S[, j], ref))
      if (is.finite(rj) && rj < 0) aligned[, j] <- -S[, j]
    }
    mean_sig <- rowMeans(aligned)
    if (stats::sd(mean_sig) <= 1e-12) {
      rep_var <- members[1L]
    } else {
      cors_to_mean <- suppressWarnings(abs(stats::cor(S, mean_sig)))
      cors_to_mean[!is.finite(cors_to_mean)] <- -Inf
      rep_var <- members[which.max(cors_to_mean)]
    }
    reps <- c(reps, rep_var)
    dropped <- c(dropped, setdiff(members, rep_var))
    # Record each member's correlation to the chosen representative.
    rep_sig <- sig[, rep_var]
    cor_to_rep <- vapply(members, function(mm) {
      cc <- suppressWarnings(stats::cor(sig[, mm], rep_sig))
      if (is.finite(cc)) abs(cc) else NA_real_
    }, numeric(1))
    assign_rows[[length(assign_rows) + 1L]] <- data.frame(
      cluster_id = g, cluster_size = length(members),
      representative_variable = rep_var, member_variable = members,
      correlation_to_representative = as.numeric(cor_to_rep),
      stringsAsFactors = FALSE)
  }
  # Preserve original column order in the kept set for downstream stability.
  kept_set <- unique(c(reps, no_sig))
  kept <- vars[vars %in% kept_set]
  assignments <- if (length(assign_rows)) do.call(rbind, assign_rows) else
    data.frame(cluster_id = integer(0), cluster_size = integer(0),
               representative_variable = character(0), member_variable = character(0),
               correlation_to_representative = numeric(0), stringsAsFactors = FALSE)
  list(kept = kept, dropped = unique(dropped),
       n_clusters = n_clusters, n_singletons = n_singletons, skipped = FALSE,
       assignments = assignments)
}


fit_nested_glmnet_screen <- function(X, y, family, foldid, weights, raw_map, cfg) {
  if (ncol(X) == 0L) return(character(0))
  if (length(y) != nrow(X))
    stop(sprintf("Nested elastic-net outcome length (%d) does not match design rows (%d).",
                 length(y), nrow(X)), call. = FALSE)
  if (length(raw_map) != ncol(X)) {
    stop(sprintf("Elastic-net raw-variable map length (%d) does not match processed columns (%d).",
                 length(raw_map), ncol(X)), call. = FALSE)
  }
  names(raw_map) <- colnames(X)
  y <- as.numeric(y)
  if (any(!is.finite(y)))
    stop("Nested elastic-net screen received a nonfinite outcome.", call. = FALSE)
  if (family == "binomial" && (!all(y %in% c(0, 1)) || length(unique(y)) < 2L))
    return(character(0))
  foldid <- as.integer(foldid)
  if (length(foldid) != nrow(X) || anyNA(foldid) || length(unique(foldid)) < 3L)
    stop("Nested elastic-net tuning requires aligned whole-cluster folds with at least three levels.",
         call. = FALSE)
  if (any(!is.finite(X)))
    stop("Nested elastic-net screen received nonfinite predictors.", call. = FALSE)
  w <- normalize_positive_weights(weights, length(y), "nested elastic-net screen weights")
  X_fit <- X
  pad_added <- FALSE
  if (ncol(X_fit) == 1L) {
    X_fit <- cbind(X_fit, .glmnet_pad = 0)
    pad_added <- TRUE
  }
  cvfit <- tryCatch(
    glmnet::cv.glmnet(
      x = if (requireNamespace("Matrix", quietly = TRUE)) Matrix::Matrix(X_fit, sparse = TRUE) else X_fit,
      y = y, family = family,
      alpha = cfg$final_tmle$lasso_screen_alpha %||% 0.25,
      nlambda = cfg$final_tmle$lasso_screen_nlambda %||% 50L,
      weights = w, foldid = foldid,
      standardize = isTRUE(cfg$learners$glmnet$standardize %||% TRUE),
      maxit = cfg$final_tmle$lasso_screen_glmnet_maxit %||% 100000L),
    error = function(e) e)
  if (inherits(cvfit, "error"))
    stop("Nested elastic-net screen failed: ", conditionMessage(cvfit), call. = FALSE)
  lam <- switch(cfg$final_tmle$lasso_screen_lambda_choice %||% "lambda.min",
                "lambda.1se" = cvfit$lambda.1se,
                "lambda.min" = cvfit$lambda.min,
                cvfit$lambda.min)
  validate_cv_glmnet_fit(cvfit, lam, expected_ncol = ncol(X_fit),
                         label = paste0("nested elastic-net ", family, " screen"))
  beta_all <- as.numeric(stats::coef(cvfit, s = lam))
  if (length(beta_all) != ncol(X_fit) + 1L || any(!is.finite(beta_all)))
    stop("Nested elastic-net screen produced an invalid coefficient vector.", call. = FALSE)
  # The zero-valued pad exists only to satisfy glmnet's one-predictor edge
  # case. It is never eligible for selection and is discarded here.
  beta_fit <- beta_all[-1L]
  beta <- beta_fit[seq_len(ncol(X))]
  if (isTRUE(pad_added) && length(beta_fit) != 2L)
    stop("Nested elastic-net one-predictor padding produced an unexpected coefficient vector.",
         call. = FALSE)
  nz <- which(abs(beta) > 0)
  if (!length(nz)) return(character(0))
  unique(strip_missing_suffix(raw_map[nz]))
}


run_nested_multivar_lasso_after_rough <- function(tr_df, rough_vars, cfg, fold_id,
                                                  A_tr, y_out, outcome_type,
                                                  outcome_obs, delta, cl_tr) {
  # Multivariable union screen fit only on the final fold's
  # training rows. It fits A~W, Y~W, and delta_Y~W, then takes the union.
  # This keeps the procedure data-driven while avoiding a half-blind screen
  # that can drop variables predictive of exposure and weakly/moderately
  # predictive of outcome. The rough screen supplies a diversified candidate
  # pool; A-only variables are not forced directly into final W.
  t0 <- proc.time()[3]
  empty <- list(selected = character(0), selected_a = character(0),
                selected_y = character(0), selected_delta = character(0),
                seconds = 0, n_processed = 0, n_raw_screened = 0)
  rough_vars <- unique(rough_vars[rough_vars %in% names(tr_df)])
  if (length(rough_vars) == 0L) return(empty)

  raw_dat <- tr_df[, rough_vars, drop = FALSE]
  miss_rec <- learn_final_missing_recipe(raw_dat, cfg$preprocessing)
  proc <- apply_final_missing_recipe(raw_dat, miss_rec, cfg$preprocessing)

  # Allocate the elastic-net screening design before materializing the full
  # matrix. The configured cap is a nonprotected-column budget; H1FS remains
  # mandatory but the total must stay below the genuine hard matrix guard.
  processed_cap <- as.integer(cfg$final_tmle$lasso_screen_max_processed_cols %||% 240L)
  hard_total <- as.integer(cfg$final_tmle$hard_max_processed_columns %||% 450L)
  protected_lasso <- intersect(cfg$final_tmle$protected_W %||% character(0), rough_vars)
  mandatory_lasso <- intersect(get_mandatory_W(cfg), rough_vars)
  ordered_vars <- c(mandatory_lasso, setdiff(rough_vars, mandatory_lasso))
  support_map <- attr(proc, "numeric_support") %||% list()
  counts <- setNames(integer(length(ordered_vars)), ordered_vars)
  for (v in ordered_vars) {
    cols <- intersect(names(proc), c(v, paste0(v, "_missA"), paste0(v, "_miss97")))
    one <- proc[, cols, drop = FALSE]
    attr(one, "numeric_support") <- support_map[intersect(names(support_map), cols)]
    d1 <- build_grouped_design_train(one, cfg$final_preprocess, cfg$preprocessing,
                                     hard_max_cols = hard_total, A = A_tr,
                                     protected_raw_vars = protected_lasso)
    counts[[v]] <- ncol(d1$X)
  }
  current_vars <- mandatory_lasso
  used_np <- 0L
  for (v in setdiff(ordered_vars, mandatory_lasso)) {
    add <- counts[[v]]
    if (add > 0L && used_np + add <= processed_cap) {
      current_vars <- c(current_vars, v); used_np <- used_np + add
    }
  }
  current_vars <- unique(current_vars)
  if (!length(current_vars)) {
    out <- empty; out$seconds <- proc.time()[3] - t0; return(out)
  }
  expected_total <- sum(counts[current_vars])
  if (expected_total > hard_total)
    stop(sprintf("Elastic-net screening design requires %d columns, above hard total %d.",
                 expected_total, hard_total), call. = FALSE)
  proc_cols <- intersect(names(proc), c(current_vars, paste0(current_vars, "_missA"), paste0(current_vars, "_miss97")))
  proc_cur <- proc[, proc_cols, drop = FALSE]
  attr(proc_cur, "numeric_support") <- support_map[intersect(names(support_map), proc_cols)]
  des <- build_grouped_design_train(proc_cur, cfg$final_preprocess, cfg$preprocessing,
                                    hard_max_cols = hard_total, A = A_tr,
                                    protected_raw_vars = protected_lasso)
  if (ncol(des$X) == 0L) {
    out <- empty; out$seconds <- proc.time()[3] - t0; return(out)
  }
  message(sprintf("    [fold %s elastic-net] design uses %d mandatory (%d protected H1FS) + %d budgeted processed columns (total %d).",
                  as.character(fold_id), sum(counts[mandatory_lasso]),
                  sum(counts[protected_lasso]), used_np, ncol(des$X)))


  raw_map <- strip_missing_suffix(names(des$recipes)[des$group])
  X <- des$X
  storage.mode(X) <- "double"
  X[!is.finite(X)] <- 0
  K <- cfg$final_tmle$lasso_screen_folds %||% 3L
  wt <- as.numeric(tr_df[[cfg$analysis$weight_var]])

  # Exposure screen: included for union screening. It is multivariable and
  # penalized, not a forced marginal A-only inclusion rule.
  selected_a <- character(0)
  if (length(unique(A_tr)) == 2L) {
    fold_a <- do.call(make_cluster_folds_balanced, c(list(
      cluster = cl_tr, A = A_tr, k = K,
      seed = seed_for(cfg, 14000L + ifelse(is.na(fold_id), 0L, fold_id)),
      weights = wt, delta = rep(1L, length(A_tr)),
      balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights)),
      fold_control_from_cfg(cfg, "internal")))
    selected_a <- fit_nested_glmnet_screen(
      X, A_tr, "binomial", fold_a, wt, raw_map, cfg)
  }

  # Outcome screen, fit only among rows with observed outcome.
  selected_y <- character(0)
  keep_y <- outcome_obs & is.finite(y_out)
  if (sum(keep_y) >= max(25L, K + 2L)) {
    yy <- y_out[keep_y]
    fam_y <- if (identical(outcome_type, "binary")) "binomial" else "gaussian"
    if (fam_y != "binomial" || length(unique(yy)) == 2L) {
      fold_y <- do.call(make_cluster_folds_balanced, c(list(
        cluster = cl_tr[keep_y], A = A_tr[keep_y], k = K,
        seed = seed_for(cfg, 15000L + ifelse(is.na(fold_id), 0L, fold_id)),
        weights = wt[keep_y], delta = delta[keep_y],
        balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights)),
        fold_control_from_cfg(cfg, "internal")))
      selected_y <- fit_nested_glmnet_screen(
        X[keep_y, , drop = FALSE], yy, fam_y, fold_y,
        wt[keep_y], raw_map, cfg)
    }
  }

  # Outcome-observation screen. This is included because missing Y is part of
  # the observed-data problem.
  selected_delta <- character(0)
  if (isTRUE(cfg$final_tmle$lasso_screen_include_delta) && length(unique(delta)) == 2L) {
    fold_d <- do.call(make_cluster_folds_balanced, c(list(
      cluster = cl_tr, A = A_tr, k = K,
      seed = seed_for(cfg, 16000L + ifelse(is.na(fold_id), 0L, fold_id)),
      weights = wt, delta = delta,
      balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights)),
      fold_control_from_cfg(cfg, "internal")))
    selected_delta <- fit_nested_glmnet_screen(
      X, delta, "binomial", fold_d, wt, raw_map, cfg)
  }
  selected <- unique(c(selected_a, selected_y, selected_delta))
  selected <- selected[selected %in% current_vars]
  list(selected = selected, selected_a = selected_a,
       selected_y = selected_y, selected_delta = selected_delta,
       seconds = proc.time()[3] - t0, n_processed = ncol(X),
       n_raw_screened = length(current_vars))
}
run_nested_rough_prescreen_for_final <- function(main_df, train_idx, cfg, fold_id = NA_integer_) {
  t_all <- proc.time()[3]
  tr_df <- main_df[train_idx, , drop = FALSE]
  protected_screen <- intersect(cfg$final_tmle$protected_W %||% character(0), names(tr_df))
  mandatory_screen <- intersect(get_mandatory_W(cfg), names(tr_df))
  cand <- get_candidate_vars(tr_df, cfg)
  if (length(cand) == 0L)
    stop("Nested rough screen has zero candidate variables.", call. = FALSE)
  prefilter_info <- prefilter_candidate_vars_for_screen(tr_df, cand, cfg, fold_id = fold_id)
  if (isTRUE(cfg$final_tmle$mandatory_W_bypass_screening %||% TRUE) && length(mandatory_screen)) {
    cand <- unique(c(mandatory_screen, prefilter_info$vars))
    hit <- match(mandatory_screen, prefilter_info$log$variable)
    hit <- hit[is.finite(hit)]
    if (length(hit)) {
      prefilter_info$log$prefilter_kept[hit] <- TRUE
      prefilter_info$log$prefilter_reason[hit] <- "mandatory_override"
    }
  } else {
    cand <- prefilter_info$vars
  }

  message(sprintf("    [fold %s screen] candidates=%d after prefilter; preprocessing for rough scores...",
                  as.character(fold_id), length(cand)))
  t_prep <- proc.time()[3]
  X_base <- tr_df[, cand, drop = FALSE]
  X_base <- classify_factors_by_uniques(X_base, cfg$preprocessing)
  fac <- names(X_base)[vapply(X_base, is.factor, logical(1))]
  num <- names(X_base)[vapply(X_base, is.numeric, logical(1))]
  X_base <- add_dual_missingness_indicators(X_base, fac, num, cfg$preprocessing)
  X_base <- remove_constant_columns(
    X_base, tol = cfg$preprocessing$constant_variance_tol,
    verbose = isTRUE(cfg$global$verbose))
  message(sprintf("    [fold %s screen] preprocessing done in %.1fs: %d columns after indicators/constants.",
                  as.character(fold_id), proc.time()[3] - t_prep, ncol(X_base)))

  # (Option A): cluster the FULL candidate set BEFORE marginal scoring, so
  # large redundant blocks collapse to single representatives before the top-N
  # ranking caps are applied. This stops a block of ~85 mutually-correlated
  # contextual variables from consuming most of the ranking slots and crowding
  # out distinct weak confounders. Deterministic and seed-independent. Only the
  # surviving representatives are scored and ranked below.
  # IMPORTANT: cluster only the SUBSTANTIVE (non-missingness-indicator) columns.
  # add_dual_missingness_indicators can roughly double the column count, which
  # would push the candidate count past cluster_dedupe_max_vars and silently
  # skip clustering. Missingness indicators are derived columns and are not the
  # source of the contextual-block redundancy, so we exclude them from the
  # clustering input (and hence from the size-guard count). When a substantive
  # variable is collapsed into a representative, its own missingness indicators
  # are dropped alongside it; representatives keep their indicators.
  cluster_assignments <- NULL   # returned for the cluster-assignment diagnostic
  if (identical(cfg$final_tmle$redundancy_method %||% "cluster", "cluster") &&
      isTRUE(cfg$final_tmle$rough_redundancy_control)) {
    t_clu <- proc.time()[3]
    all_cols   <- names(X_base)
    is_indic   <- grepl("(_missA|_miss97)$", all_cols)
    subst_cols <- all_cols[!is_indic]
    indic_cols <- all_cols[is_indic]
    clusterable_subst <- setdiff(subst_cols, mandatory_screen)
    dedupe <- cluster_dedupe_candidates(
      clusterable_subst, X_base,
      cor_threshold = cfg$final_tmle$rough_redundancy_cor_threshold %||% 0.90,
      linkage = cfg$final_tmle$redundancy_linkage %||% "complete",
      impute_method = cfg$preprocessing$numeric_imputation,
      max_cluster_vars = cfg$final_tmle$cluster_dedupe_max_vars %||% 6000L,
      enable = TRUE)
    if (isTRUE(dedupe$skipped)) {
      message(sprintf("    [fold %s screen] pre-score clustering SKIPPED: %d substantive candidates exceed cap %d; scoring full set.",
                      as.character(fold_id), length(clusterable_subst),
                      cfg$final_tmle$cluster_dedupe_max_vars %||% 6000L))
    } else {
      # Keep representatives + singletons, plus indicators whose base variable
      # survived. An indicator survives iff its stripped base name is kept.
      kept_base   <- unique(c(mandatory_screen, dedupe$kept))
      keep_indic  <- indic_cols[strip_missing_suffix(indic_cols) %in% kept_base]
      keep_cols   <- all_cols[all_cols %in% c(kept_base, keep_indic)]
      X_base <- X_base[, keep_cols, drop = FALSE]
      if (!is.null(dedupe$assignments) && nrow(dedupe$assignments) > 0L) {
        cluster_assignments <- cbind(
          data.frame(fold = fold_id, stringsAsFactors = FALSE),
          dedupe$assignments)
      }
      message(sprintf("    [fold %s screen] pre-score clustering in %.1fs: %d substantive kept (%d collapsed across %d clusters); %d -> %d total columns.",
                      as.character(fold_id), proc.time()[3] - t_clu,
                      length(kept_base), length(dedupe$dropped), dedupe$n_clusters,
                      length(all_cols), ncol(X_base)))
    }
  }

  A_tr <- normalize_binary_var(tr_df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  outcome_info <- prepare_modeled_outcome(
    tr_df[[cfg$analysis$outcome_var]], cfg$analysis$outcome_type, cfg$analysis$outcome_var)
  y_out <- outcome_info$values
  outcome_type <- outcome_info$type
  if (identical(outcome_type, "continuous")) {
    lo_screen <- cfg$final_tmle$screen_y_lower
    hi_screen <- cfg$final_tmle$screen_y_upper
    if (!is.finite(lo_screen) || !is.finite(hi_screen) || hi_screen <= lo_screen)
      stop("Nested screen is missing valid bounded-outcome limits.", call. = FALSE)
    obs_screen <- is.finite(y_out)
    y_out[obs_screen] <- pmin(pmax(y_out[obs_screen], lo_screen), hi_screen)
  }
  outcome_obs <- outcome_info$observed
  delta <- as.integer(outcome_obs)
  cl_tr <- tr_df[[cfg$analysis$cluster_var]]
  wt_tr <- normalize_positive_weights(
    tr_df[[cfg$analysis$weight_var]], nrow(tr_df), "rough-screen survey weights")

  K <- cfg$final_tmle$rough_folds %||% cfg$rough_prescreen$folds %||% 3L
  base_seed <- seed_for(cfg, 12000L + ifelse(is.na(fold_id), 0L, fold_id * 10L))
  fold_A <- make_rough_fold_ids(nrow(tr_df), K, base_seed,
                                cluster_vec = cl_tr, A_vec = A_tr, weights = wt_tr,
                                cluster_aware = TRUE, fold_control = fold_control_from_cfg(cfg, "internal"))
  fold_Y <- make_rough_fold_ids(nrow(tr_df), K, base_seed + 1L,
                                cluster_vec = cl_tr, A_vec = A_tr, delta_vec = delta,
                                weights = wt_tr, cluster_aware = TRUE,
                                fold_control = fold_control_from_cfg(cfg, "internal"))
  fold_D <- make_rough_fold_ids(nrow(tr_df), K, base_seed + 2L,
                                cluster_vec = cl_tr, A_vec = A_tr, delta_vec = delta,
                                weights = wt_tr, cluster_aware = TRUE,
                                fold_control = fold_control_from_cfg(cfg, "internal"))
  K_A <- max(fold_A); K_Y <- max(fold_Y); K_D <- max(fold_D)

  # A is scored only to build a joint A/Y ranking. A-only predictors are not
  # force-selected by default.
  t_A <- proc.time()[3]
  exposure_scores <- screen_binom_linear(
    A_tr, X_base, K = K_A, seed = base_seed,
    eps = cfg$rough_prescreen$binomial_eps, fold = fold_A, weights = wt_tr,
    glmnet_maxit = cfg$rough_prescreen$glmnet_maxit %||% 1000000L,
    glmnet_thresh = cfg$rough_prescreen$glmnet_thresh %||% 1e-5,
    ridge_lambda = cfg$rough_prescreen$ridge_lambda %||% 1.0)
  message(sprintf("    [fold %s screen] exposure score pass in %.1fs (%s).",
                  as.character(fold_id), proc.time()[3] - t_A,
                  score_summary_msg(exposure_scores)))

  t_Y <- proc.time()[3]
  if (outcome_type == "binary") {
    outcome_scores <- screen_binom_linear(
      y_out[outcome_obs], X_base[outcome_obs, , drop = FALSE],
      K = K_Y, seed = base_seed + 1L,
      eps = cfg$rough_prescreen$binomial_eps,
      fold = fold_Y[outcome_obs], weights = wt_tr[outcome_obs],
      glmnet_maxit = cfg$rough_prescreen$glmnet_maxit %||% 1000000L,
      glmnet_thresh = cfg$rough_prescreen$glmnet_thresh %||% 1e-5,
      ridge_lambda = cfg$rough_prescreen$ridge_lambda %||% 1.0)
  } else {
    outcome_scores <- screen_gauss_linear(
      y_out, X_base, K = K_Y, seed = base_seed + 1L, fold = fold_Y, weights = wt_tr)
  }
  message(sprintf("    [fold %s screen] outcome score pass in %.1fs (%s).",
                  as.character(fold_id), proc.time()[3] - t_Y,
                  score_summary_msg(outcome_scores)))

  missing_scores <- setNames(rep(NA_real_, length(exposure_scores)), names(exposure_scores))
  t_D <- proc.time()[3]
  if (length(unique(delta)) == 2L) {
    missing_scores <- screen_binom_linear(
      delta, X_base, K = K_D, seed = base_seed + 2L,
      eps = cfg$rough_prescreen$binomial_eps, fold = fold_D, weights = wt_tr,
      glmnet_maxit = cfg$rough_prescreen$glmnet_maxit %||% 1000000L,
      glmnet_thresh = cfg$rough_prescreen$glmnet_thresh %||% 1e-5,
      ridge_lambda = cfg$rough_prescreen$ridge_lambda %||% 1.0)
    message(sprintf("    [fold %s screen] delta_Y score pass in %.1fs (%s).",
                    as.character(fold_id), proc.time()[3] - t_D,
                    score_summary_msg(missing_scores)))
  } else {
    message(sprintf("    [fold %s screen] delta_Y score pass skipped: no variation in outcome observation.",
                    as.character(fold_id)))
  }

  nY  <- cfg$final_tmle$rough_top_n_outcome %||% 100L
  nD  <- cfg$final_tmle$rough_top_n_missingness %||% 40L
  nAY <- cfg$final_tmle$rough_top_n_joint_AY %||% 50L
  nA0 <- cfg$final_tmle$rough_top_n_exposure_only %||% 0L
  nA_lasso <- if (isTRUE(cfg$final_tmle$nested_lasso_after_rough))
    cfg$final_tmle$rough_top_n_exposure_for_lasso %||% 60L else nA0
  vars_Y  <- top_score_names(outcome_scores, nY)
  vars_D  <- top_score_names(missing_scores, nD)
  vars_AY <- top_joint_score_names(exposure_scores, outcome_scores, nAY)
  vars_A_for_lasso <- top_score_names(exposure_scores, nA_lasso)
  vars_A_only <- character(0)
  if (nA0 > 0L) {
    a_ranked <- top_score_names(exposure_scores, nA0 + length(vars_Y) + length(vars_D) + length(vars_AY))
    vars_A_only <- setdiff(a_ranked, unique(c(vars_Y, vars_D, vars_AY)))
    vars_A_only <- vars_A_only[seq_len(min(length(vars_A_only), nA0))]
  }

  # Final-eligible rough variables exclude exposure-only LASSO candidates.
  # Those candidates are available to the nested A~W elastic-net, but can enter
  # final W only if selected by that multivariable model or by another permitted
  # outcome/joint/missingness pathway.
  final_ranked_pool <- unique(c(mandatory_screen, vars_AY, vars_Y, vars_D, vars_A_only))
  final_ranked_pool <- final_ranked_pool[final_ranked_pool %in% names(X_base)]
  lasso_candidate_pool <- unique(c(final_ranked_pool, vars_A_for_lasso))
  lasso_candidate_pool <- lasso_candidate_pool[lasso_candidate_pool %in% names(X_base)]
  ranked_pool <- final_ranked_pool
  pool_cap <- cfg$final_tmle$rough_candidate_pool_max %||% 175L
  if (identical(cfg$final_tmle$redundancy_method %||% "cluster", "cluster")) {
    # Redundancy was already removed by pre-score clustering of the full
    # candidate set, so X_base (and hence ranked_pool) contains only cluster
    # representatives. Here we only apply the size cap; no second clustering.
    # Define `red`/`red_frac` from the pre-score dedupe result so the downstream
    # selection-audit table (which references red$dropped and red_frac) is
    # populated correctly. Guard for the case where clustering was skipped or
    # disabled, in which case nothing was dropped as redundant.
    pre_dropped <- if (exists("dedupe") && is.list(dedupe))
      dedupe$dropped %||% character(0) else character(0)
    pre_kept <- if (exists("dedupe") && is.list(dedupe))
      dedupe$kept %||% character(0) else character(0)
    pre_assignments <- if (exists("dedupe") && is.list(dedupe))
      dedupe$assignments %||% data.frame() else data.frame()
    red <- list(dropped = pre_dropped, assignments = pre_assignments)
    red_denom <- length(unique(c(pre_kept, pre_dropped)))
    red_frac <- if (red_denom > 0L) length(unique(pre_dropped)) / red_denom else NA_real_
    n_pre_cap <- length(ranked_pool)
    nonprotected_ranked <- setdiff(ranked_pool, mandatory_screen)
    rough_pool <- unique(c(mandatory_screen,
      nonprotected_ranked[seq_len(min(length(nonprotected_ranked), pool_cap))]))
    message(sprintf("    [fold %s screen] ranked pool (already de-duplicated pre-score): %d -> %d after size cap %d; %d collapsed pre-score.",
                    as.character(fold_id), n_pre_cap, length(rough_pool), pool_cap, length(red$dropped)))
  } else {
    red <- greedy_redundancy_filter(
      setdiff(ranked_pool, mandatory_screen), X_base,
      max_vars = pool_cap,
      cor_threshold = cfg$final_tmle$rough_redundancy_cor_threshold %||% 0.80,
      impute_method = cfg$preprocessing$numeric_imputation,
      enable = isTRUE(cfg$final_tmle$rough_redundancy_control))
    red$assignments <- data.frame()
    rough_pool <- unique(c(mandatory_screen, red$selected))
    red_denom <- length(rough_pool) + length(red$dropped)
    red_frac <- if (red_denom > 0L) length(red$dropped) / red_denom else NA_real_
    message(sprintf("    [fold %s screen] rough pool after redundancy: %d kept, %d dropped as redundant (%.1f%%).",
                    as.character(fold_id), length(rough_pool), length(red$dropped), 100 * red_frac))
    if (is.finite(red_frac) && red_frac < 0.10 && isTRUE(cfg$final_tmle$rough_redundancy_control))
      warning(sprintf("Fold %s redundancy filter dropped only %.1f%% of pool variables; threshold may be too loose.",
                      as.character(fold_id), 100 * red_frac))
    if (is.finite(red_frac) && red_frac > 0.70 && isTRUE(cfg$final_tmle$rough_redundancy_control))
      warning(sprintf("Fold %s redundancy filter dropped %.1f%% of pool variables; threshold may be too tight.",
                      as.character(fold_id), 100 * red_frac))
  }

  lasso_info <- list(selected = character(0), selected_a = character(0),
                     selected_y = character(0), selected_delta = character(0),
                     seconds = 0, n_processed = 0, n_raw_screened = 0)
  if (isTRUE(cfg$final_tmle$nested_lasso_after_rough)) {
    lasso_info <- run_nested_multivar_lasso_after_rough(
      tr_df = tr_df, rough_vars = unique(c(rough_pool, lasso_candidate_pool)), cfg = cfg, fold_id = fold_id,
      A_tr = A_tr, y_out = y_out, outcome_type = outcome_type,
      outcome_obs = outcome_obs, delta = delta, cl_tr = cl_tr)
    message(sprintf("    [fold %s screen] nested multivariable elastic-net union in %.1fs: raw=%d, processed=%d, selected A=%d, Y=%d, delta=%d, union=%d.",
                    as.character(fold_id), lasso_info$seconds,
                    lasso_info$n_raw_screened, lasso_info$n_processed,
                    length(lasso_info$selected_a), length(lasso_info$selected_y),
                    length(lasso_info$selected_delta), length(lasso_info$selected)))
  }

  # Final selected variables: nested LASSO augments the diversified
  # rough pool; it never acts as a brittle hard bottleneck. This avoids
  # pathological runs where glmnet convergence/separation leaves only a few
  # variables and the final TMLE becomes underadjusted.
  # Nested LASSO is an augmenting multivariable union screen, not a
  # replacement gate. The diversified rough pool stays eligible so that
  # convergence issues or rare-event folds cannot collapse the final W set
  # to a handful of variables.
  selected <- unique(c(mandatory_screen, lasso_info$selected, rough_pool))

  min_total <- cfg$final_tmle$rough_min_total_vars %||% 1L
  min_lasso_augmented <- if (isTRUE(cfg$final_tmle$nested_lasso_after_rough))
    cfg$final_tmle$lasso_screen_min_vars %||% 30L else min_total
  target_min <- max(min_total, min_lasso_augmented)

  if (length(selected) < target_min) {
    add_pool <- setdiff(final_ranked_pool, selected)
    add_needed <- target_min - length(selected)
    selected <- unique(c(selected, add_pool[seq_len(min(add_needed, length(add_pool)))]))
  }

  max_lasso <- if (isTRUE(cfg$final_tmle$nested_lasso_after_rough))
    cfg$final_tmle$lasso_screen_max_vars %||% cfg$final_tmle$rough_max_total_vars else
    cfg$final_tmle$rough_max_total_vars
  max_total <- min(cfg$final_tmle$rough_max_total_vars %||% 90L, max_lasso %||% 90L)
  nonprotected_selected <- setdiff(selected, mandatory_screen)
  if (length(nonprotected_selected) > max_total) {
    # Protected H1FS items are cap-exempt. Among all other variables, preserve
    # joint A-and-Y evidence before outcome-only precision variables.
    both_ay_sel <- nonprotected_selected %in% intersect(lasso_info$selected_a, lasso_info$selected_y)
    priority <- ifelse(both_ay_sel, 1L,
                ifelse(nonprotected_selected %in% vars_AY, 2L,
                ifelse(nonprotected_selected %in% lasso_info$selected_y, 3L,
                ifelse(nonprotected_selected %in% lasso_info$selected_delta, 4L,
                ifelse(nonprotected_selected %in% vars_Y, 5L,
                ifelse(nonprotected_selected %in% vars_D, 6L,
                ifelse(nonprotected_selected %in% lasso_info$selected_a, 7L, 8L)))))))
    best_score <- pmax(outcome_scores[nonprotected_selected], missing_scores[nonprotected_selected],
                       exposure_scores[nonprotected_selected], na.rm = TRUE)
    best_score[!is.finite(best_score)] <- -Inf
    nonprotected_selected <- nonprotected_selected[order(priority, -best_score)]
    nonprotected_selected <- nonprotected_selected[seq_len(max_total)]
  }
  selected <- unique(c(mandatory_screen, nonprotected_selected))

  a_candidate_only <- setdiff(
    vars_A_for_lasso,
    unique(c(mandatory_screen, vars_AY, vars_Y, vars_D, vars_A_only)))
  illegal_a_only <- setdiff(intersect(selected, a_candidate_only), lasso_info$selected)
  if (length(illegal_a_only))
    stop("Exposure-only LASSO candidate(s) entered final W without an allowed selection pathway: ",
         paste(illegal_a_only, collapse = ", "), call. = FALSE)

  selected <- unique(selected[selected %in% names(tr_df)])
  if (length(lasso_info$selected) < min_lasso_augmented && isTRUE(cfg$final_tmle$nested_lasso_after_rough)) {
    warning(sprintf("Fold %s nested elastic-net selected only %d variables (< %d); using augmented rough-pool fallback.",
                    as.character(fold_id), length(lasso_info$selected), min_lasso_augmented))
  }
  if (length(selected) < min_total) {
    warning(sprintf("Fold %s final nested screen selected only %d variables (< %d).",
                    as.character(fold_id), length(selected), min_total))
  }
  if (length(selected) == 0L)
    stop("Nested screen selected zero variables.", call. = FALSE)

  audit_vars <- unique(c(mandatory_screen, names(exposure_scores),
                         names(outcome_scores), names(missing_scores), red$dropped))
  redundancy_map <- if (is.data.frame(red$assignments) && nrow(red$assignments) > 0L) {
    red$assignments[!duplicated(red$assignments$member_variable), , drop = FALSE]
  } else data.frame()
  red_match <- if (nrow(redundancy_map))
    match(audit_vars, redundancy_map$member_variable) else rep(NA_integer_, length(audit_vars))
  sel_tab <- data.frame(
    fold = fold_id,
    variable = audit_vars,
    exposure_score = as.numeric(exposure_scores[audit_vars]),
    outcome_score = as.numeric(outcome_scores[audit_vars]),
    delta_score = as.numeric(missing_scores[audit_vars]),
    in_rough_pool = audit_vars %in% rough_pool,
    in_lasso_candidate_pool = audit_vars %in% lasso_candidate_pool,
    dropped_redundant = audit_vars %in% red$dropped,
    redundancy_drop_fraction = red_frac,
    redundancy_cluster_id = if (nrow(redundancy_map))
      as.integer(redundancy_map$cluster_id[red_match]) else NA_integer_,
    redundancy_cluster_size = if (nrow(redundancy_map))
      as.integer(redundancy_map$cluster_size[red_match]) else NA_integer_,
    redundancy_representative = if (nrow(redundancy_map))
      as.character(redundancy_map$representative_variable[red_match]) else NA_character_,
    redundancy_correlation_to_representative = if (nrow(redundancy_map))
      as.numeric(redundancy_map$correlation_to_representative[red_match]) else NA_real_,
    selected = audit_vars %in% selected,
    selected_by_joint_AY = audit_vars %in% vars_AY,
    selected_by_outcome = audit_vars %in% vars_Y,
    selected_by_delta = audit_vars %in% vars_D,
    selected_by_exposure_candidate_for_lasso = audit_vars %in% vars_A_for_lasso,
    selected_by_exposure_only = audit_vars %in% vars_A_only,
    selected_by_lasso_A = audit_vars %in% lasso_info$selected_a,
    selected_by_lasso_Y = audit_vars %in% lasso_info$selected_y,
    selected_by_lasso_delta = audit_vars %in% lasso_info$selected_delta,
    protected_fixed = audit_vars %in% protected_screen,
    mandatory_fixed = audit_vars %in% mandatory_screen,
    stringsAsFactors = FALSE)
  if (exists("prefilter_info") && !is.null(prefilter_info$log)) {
    sel_tab <- merge(sel_tab, prefilter_info$log, by = c("fold", "variable"),
                     all.x = TRUE, sort = FALSE)
    dropped_pf <- prefilter_info$log[!prefilter_info$log$prefilter_kept, , drop = FALSE]
    if (nrow(dropped_pf) > 0L) {
      dropped_rows <- data.frame(
        fold = dropped_pf$fold, variable = dropped_pf$variable,
        exposure_score = NA_real_, outcome_score = NA_real_, delta_score = NA_real_,
        in_rough_pool = FALSE, in_lasso_candidate_pool = FALSE, dropped_redundant = FALSE, redundancy_drop_fraction = red_frac,
        selected = FALSE, selected_by_joint_AY = FALSE, selected_by_outcome = FALSE,
        selected_by_delta = FALSE, selected_by_exposure_candidate_for_lasso = FALSE,
        selected_by_exposure_only = FALSE, selected_by_lasso_A = FALSE,
        selected_by_lasso_Y = FALSE, selected_by_lasso_delta = FALSE,
        protected_fixed = dropped_pf$variable %in% protected_screen,
        mandatory_fixed = dropped_pf$variable %in% mandatory_screen,
        stringsAsFactors = FALSE)
      dropped_rows <- merge(dropped_rows, dropped_pf, by = c("fold", "variable"),
                            all.x = TRUE, sort = FALSE)
      missing_cols <- setdiff(names(sel_tab), names(dropped_rows))
      for (cc in missing_cols) dropped_rows[[cc]] <- NA
      missing_cols2 <- setdiff(names(dropped_rows), names(sel_tab))
      for (cc in missing_cols2) sel_tab[[cc]] <- NA
      sel_tab <- rbind(sel_tab, dropped_rows[, names(sel_tab), drop = FALSE])
    }
  }
  sel_tab <- sel_tab[order(!sel_tab$selected,
                           !sel_tab$in_rough_pool,
                           -pmax(sel_tab$outcome_score, sel_tab$delta_score,
                                  sel_tab$exposure_score, na.rm = TRUE)), , drop = FALSE]

  message(sprintf(paste0(
    "    [fold %s screen] final selected=%d (rough_pool=%d, ",
    "jointAY=%d, Y=%d, delta=%d, A_candidates=%d, A_only=%d, lasso=%s) in %.1fs."),
    as.character(fold_id), length(selected), length(rough_pool),
    length(vars_AY), length(vars_Y), length(vars_D), length(vars_A_for_lasso), length(vars_A_only),
    ifelse(isTRUE(cfg$final_tmle$nested_lasso_after_rough), "on", "off"),
    proc.time()[3] - t_all))
  # annotate cluster assignments with final-selection status so the
  # diagnostic shows whether each cluster's representative (and thus the cluster)
  # survived into the final selected confounder set.
  if (!is.null(cluster_assignments) && nrow(cluster_assignments) > 0L) {
    cluster_assignments$selected_member <- cluster_assignments$member_variable %in% selected
    cluster_assignments$selected_representative <-
      cluster_assignments$representative_variable %in% selected
  }
  list(selected_vars = selected, selection_table = sel_tab,
       cluster_assignments = cluster_assignments)
}

# Checkpoint fingerprinting ----------------------------------------------------
compact_fingerprint <- function(x) {
  paste(utils::capture.output(str(x, max.level = 3, give.attr = FALSE)), collapse = "\n")
}

object_md5 <- function(x) {
  tf <- tempfile("fingerprint_", fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf, version = 2)
  unname(tools::md5sum(tf))
}

make_fold_checkpoint_fingerprint <- function(cfg, main_df, data_pack, outer_fold, fold_id) {
  cfg_pre <- cfg$preprocessing
  dict <- cfg_pre$global_missing_dictionary
  cfg_pre$global_missing_dictionary <- NULL
  list(
    pipeline_version = cfg$global$version,
    script = pipeline_script_fingerprint(
      cfg, strict = isTRUE(cfg$global$require_script_md5 %||% FALSE)),
    run_tag = build_run_tag(cfg), fold_id = fold_id,
    ids = as.character(main_df[[cfg$analysis$id_var]]),
    clusters = as.character(data_pack$cluster),
    outer_fold = as.integer(outer_fold), exposure = as.integer(data_pack$A),
    delta_Y = as.integer(data_pack$delta_Y),
    weights = round(as.numeric(data_pack$weights), 8),
    Y_raw_md5 = object_md5(round(as.numeric(data_pack$Y_raw), 8)),
    Y_bounded_md5 = object_md5(round(as.numeric(data_pack$Y_bounded_orig), 8)),
    candidate_data_md5 = object_md5(main_df[, get_candidate_vars(main_df, cfg), drop = FALSE]),
    global_missing_dictionary_md5 = object_md5(dict),
    source_files = lapply(cfg$paths, file_fingerprint),
    y_lower = data_pack$y_lower, y_upper = data_pack$y_upper,
    final_tmle = cfg$final_tmle, final_preprocess = cfg$final_preprocess,
    learners = cfg$learners, preprocessing = cfg_pre,
    causal_governance = cfg$causal_governance,
    mortality_sensitivity = cfg$mortality_sensitivity,
    policy = cfg$policy,
    safety = cfg$safety)
}

# Cluster-aware validRows for SuperLearner's internal CV. For each internal
# fold, the validation rows are whole clusters. This lets SuperLearner choose
# its meta-weights using held-out clusters, matching the outer-fold scheme.
build_cluster_valid_rows <- function(cluster_vec, A_vec, V, seed, weights = NULL,
                                     delta = NULL, balance_on_weights = FALSE,
                                     fold_control = NULL) {
  ctl <- fold_control %||% list()
  fold_ids <- do.call(make_cluster_folds_balanced, c(list(
    cluster = cluster_vec, A = A_vec, k = V, seed = seed,
    weights = weights, delta = delta,
    balance_on_weights = balance_on_weights), ctl))
  lapply(seq_len(max(fold_ids)), function(v) which(fold_ids == v))
}

make_internal_fold_support <- function(validRows, A_vec, delta_vec = NULL, weights = NULL,
                                       label = "", outer_fold = NA_integer_) {
  if (is.null(delta_vec)) delta_vec <- rep(1L, length(A_vec))
  if (is.null(weights)) weights <- rep(1, length(A_vec))
  do.call(rbind, lapply(seq_along(validRows), function(j) {
    ix <- validRows[[j]]
    cc <- internal_fold_cell_counts(ix, as.integer(A_vec), as.integer(delta_vec), as.numeric(weights))
    data.frame(
      outer_fold = outer_fold, nuisance = label, internal_fold = j,
      n = length(ix), treated = sum(A_vec[ix] == 1L),
      observed = sum(delta_vec[ix] == 1L),
      observed_treated = sum(A_vec[ix] == 1L & delta_vec[ix] == 1L),
      ess = kish_ess_safe(weights[ix]),
      ess_treated = kish_ess_safe(weights[ix][A_vec[ix] == 1L]),
      as.data.frame(cc, check.names = FALSE), stringsAsFactors = FALSE)
  }))
}


solve_target_score <- function(offset, H, Y, w, score_tol = 1e-10,
                               max_expand = 60L, label = "targeting") {
  offset <- as.numeric(offset); H <- as.numeric(H); Y <- as.numeric(Y); w <- as.numeric(w)
  ok <- is.finite(offset) & is.finite(H) & is.finite(Y) & is.finite(w) & w > 0
  if (!all(ok))
    stop(sprintf("%s: %d invalid targeting row(s); nuisance predictions or weights are not admissible.",
                 label, sum(!ok)), call. = FALSE)
  if (length(Y) < 2L || !any(abs(H) > 0))
    stop(label, ": insufficient nonzero clever-covariate support.", call. = FALSE)
  score <- function(eps) sum(w * H * (Y - stats::plogis(offset + eps * H)))
  denom <- max(1, sum(w * abs(H)))
  normalized <- function(eps) abs(score(eps)) / denom
  if (normalized(0) <= score_tol)
    return(list(epsilon = 0, normalized_score = normalized(0), iterations = 0L))
  lo <- -1; hi <- 1; s_lo <- score(lo); s_hi <- score(hi)
  j <- 0L
  while ((!is.finite(s_lo) || !is.finite(s_hi) || s_lo * s_hi > 0) && j < max_expand) {
    lo <- lo * 2; hi <- hi * 2
    s_lo <- score(lo); s_hi <- score(hi); j <- j + 1L
  }
  if (!is.finite(s_lo) || !is.finite(s_hi) || s_lo * s_hi > 0)
    stop(label, ": could not bracket the targeting-score root; inspect positivity and clever-covariate extremes.", call. = FALSE)
  ans <- stats::uniroot(score, interval = c(lo, hi), tol = 1e-12, maxiter = 1000L)
  nscore <- normalized(ans$root)
  if (!is.finite(nscore) || nscore > score_tol)
    stop(sprintf("%s: normalized targeting score %.3e exceeds tolerance %.3e.",
                 label, nscore, score_tol), call. = FALSE)
  list(epsilon = ans$root, normalized_score = nscore,
       iterations = ans$iter %||% NA_integer_)
}

cluster_inference_from_eif <- function(
    D, weights, cluster, estimate, strata = NULL,
    design_frame = NULL, domain_ids = NULL,
    id_var = NULL, cluster_var = NULL, strata_var = NULL, weight_var = NULL) {
  D <- as.numeric(D); weights <- as.numeric(weights); cluster <- as.character(cluster)
  if (length(D) != length(weights) || length(D) != length(cluster) ||
      any(!is.finite(D)) || any(!is.finite(weights)) || any(weights <= 0) || anyNA(cluster))
    stop("cluster_inference_from_eif received invalid analytic-sample vectors.", call. = FALSE)
  w_norm <- weights / mean(weights)
  cl <- tapply(w_norm * D, cluster, sum)
  J <- length(cl); n <- length(D)
  if (J < 2L) stop("Cluster inference requires at least two PSUs.", call. = FALSE)
  fsc <- J / (J - 1L)
  se_cluster <- sqrt(fsc * sum(cl^2) / n^2)
  df_cluster <- J - 1L
  crit_cluster <- stats::qt(0.975, df_cluster)
  stat_cluster <- if (se_cluster > 0) estimate / se_cluster else NA_real_
  p_cluster <- if (is.finite(stat_cluster)) 2 * stats::pt(-abs(stat_cluster), df_cluster) else NA_real_

  s2 <- sum(cl^2); s4 <- sum(cl^4)
  Gstar <- if (is.finite(s4) && s4 > 0) (s2^2) / s4 else NA_real_

  use_full_domain_design <- !is.null(design_frame)
  if (use_full_domain_design) {
    required_names <- c(id_var, cluster_var, strata_var, weight_var, ".analysis_domain")
    if (any(vapply(required_names, function(z) is.null(z) || length(z) != 1L || is.na(z) || !nzchar(z), logical(1))))
      stop("Full-domain survey inference requires nonempty design-field names.", call. = FALSE)
    assert_required_columns(design_frame, required_names, "full survey-design frame")
    if (is.null(domain_ids) || length(domain_ids) != length(D))
      stop("Full-domain survey inference requires one domain respondent ID per EIF value.", call. = FALSE)
    design_ids <- as.character(design_frame[[id_var]])
    domain_ids <- as.character(domain_ids)
    if (anyNA(design_ids) || anyDuplicated(design_ids) || anyNA(domain_ids) || anyDuplicated(domain_ids))
      stop("Full-domain survey inference requires complete unique respondent IDs.", call. = FALSE)
    idx <- match(domain_ids, design_ids)
    if (anyNA(idx))
      stop("At least one analytic respondent is absent from the full survey-design frame.", call. = FALSE)
    domain_flag <- as.logical(design_frame$.analysis_domain)
    if (anyNA(domain_flag) || sum(domain_flag) != length(D) || any(!domain_flag[idx]))
      stop("Full survey-design domain indicator is inconsistent with the analytic EIF rows.", call. = FALSE)
    design_weights <- suppressWarnings(as.numeric(design_frame[[weight_var]]))
    if (any(!is.finite(design_weights)) || any(design_weights <= 0))
      stop("Full survey-design frame contains invalid sampling weights.", call. = FALSE)
    if (!isTRUE(all.equal(design_weights[idx], weights, tolerance = 1e-10)))
      stop("Analytic EIF weights differ from their full survey-design-frame weights.", call. = FALSE)
    design_psu <- trimws(as.character(design_frame[[cluster_var]]))
    design_strata <- trimws(as.character(design_frame[[strata_var]]))
    analytic_psu <- trimws(as.character(cluster))
    if (is.null(strata) || length(strata) != length(D))
      stop("Full-domain survey inference requires an analytic strata vector aligned with the EIF.", call. = FALSE)
    analytic_strata <- trimws(as.character(strata))
    if (anyNA(design_frame[[cluster_var]]) || any(!nzchar(design_psu)) ||
        anyNA(design_frame[[strata_var]]) || any(!nzchar(design_strata)))
      stop("Full survey-design frame contains missing PSU or stratum values.", call. = FALSE)
    if (anyNA(cluster) || any(!nzchar(analytic_psu)) ||
        anyNA(strata) || any(!nzchar(analytic_strata)))
      stop("Analytic EIF rows contain missing PSU or stratum values.", call. = FALSE)
    if (length(analytic_psu) != length(idx) || any(design_psu[idx] != analytic_psu))
      stop("Analytic PSU values differ from their full survey-design-frame values.", call. = FALSE)
    if (length(analytic_strata) != length(idx) || any(design_strata[idx] != analytic_strata))
      stop("Analytic stratum values differ from their full survey-design-frame values.", call. = FALSE)
    psu_strata_n <- tapply(design_strata, design_psu, function(z) length(unique(z)))
    if (any(psu_strata_n != 1L))
      stop("A PSU maps to multiple strata in the full survey-design frame.", call. = FALSE)
    psu_per_stratum <- tapply(design_psu, design_strata, function(z) length(unique(z)))
    if (any(psu_per_stratum < 2L))
      stop("A full-design stratum contains fewer than two sampled PSUs.", call. = FALSE)

    dat <- data.frame(
      eif = 0,
      w = design_weights,
      psu = factor(design_psu),
      stratum = factor(design_strata),
      domain = domain_flag,
      stringsAsFactors = FALSE)
    dat$eif[idx] <- D
    des_full <- survey::svydesign(
      ids = ~psu, strata = ~stratum, weights = ~w,
      data = dat, nest = TRUE)
    des_domain <- subset(des_full, domain)
    est0 <- survey::svymean(~eif, design = des_domain, na.rm = FALSE)
    se <- sqrt(as.numeric(stats::vcov(est0)[1L, 1L]))
    df <- survey::degf(des_domain)
    if (!is.finite(df) || df < 1L)
      stop("Survey-domain design degrees of freedom are invalid.", call. = FALSE)
    crit <- stats::qt(0.975, df)
    statistic <- if (is.finite(se) && se > 0) estimate / se else NA_real_
    p <- if (is.finite(statistic)) 2 * stats::pt(-abs(statistic), df) else NA_real_
    method <- sprintf(
      "%s-weighted %s-stratified %s-clustered with-replacement EIF variance using the complete valid-weight Wave-I design and analytic-domain subpopulation",
      weight_var, strata_var, cluster_var)
    n_design <- nrow(design_frame)
    n_domain <- sum(domain_flag)
    J_design <- length(unique(design_psu))
    H_design <- length(unique(design_strata))
  } else if (!is.null(strata)) {
    strata <- as.character(strata)
    if (length(strata) != length(D) || anyNA(strata))
      stop("Design-stratified inference requires a complete strata vector aligned with the EIF.", call. = FALSE)
    psu_per_stratum <- tapply(cluster, strata, function(z) length(unique(z)))
    if (any(psu_per_stratum < 2L))
      stop("Design-stratified inference found a stratum with fewer than two sampled PSUs.", call. = FALSE)
    dat <- data.frame(eif = D, w = weights, psu = factor(cluster), stratum = factor(strata))
    des <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w,
                             data = dat, nest = TRUE)
    est0 <- survey::svymean(~eif, design = des, na.rm = FALSE)
    se <- sqrt(as.numeric(stats::vcov(est0)[1L, 1L]))
    df <- survey::degf(des)
    if (!is.finite(df) || df < 1L) stop("Survey design degrees of freedom are invalid.", call. = FALSE)
    crit <- stats::qt(0.975, df)
    statistic <- if (is.finite(se) && se > 0) estimate / se else NA_real_
    p <- if (is.finite(statistic)) 2 * stats::pt(-abs(statistic), df) else NA_real_
    method <- sprintf("%s-weighted %s-stratified %s-clustered with-replacement EIF variance on the analytic subset",
                      weight_var %||% "sampling-weight", strata_var %||% "stratum",
                      cluster_var %||% "PSU")
    n_design <- n; n_domain <- n; J_design <- J; H_design <- length(unique(strata))
  } else {
    se <- se_cluster; df <- df_cluster; crit <- crit_cluster
    statistic <- stat_cluster; p <- p_cluster
    method <- sprintf("%s-weighted %s-cluster EIF variance (%s unavailable)",
                      weight_var %||% "sampling-weight", cluster_var %||% "PSU",
                      strata_var %||% "strata")
    n_design <- n; n_domain <- n; J_design <- J; H_design <- NA_integer_
  }
  list(se = se, ci = estimate + c(-1, 1) * crit * se, statistic = statistic,
       reference_distribution = "t", p = p, cluster_eic = cl, J = J, fsc = fsc,
       Gstar = Gstar, df = df, method = method,
       design_n = n_design, domain_n = n_domain,
       design_psu_n = J_design, design_strata_n = H_design,
       se_cluster_only = se_cluster,
       ci_cluster_only = estimate + c(-1, 1) * crit_cluster * se_cluster,
       p_cluster_only = p_cluster, df_cluster_only = df_cluster)
}


# The main driver. Runs one outer fold at a time; optional checkpoints are
# disabled in the fixed first production run and may be enabled only for
# exact resumptions after a successful fresh run.
run_final_cv_tmle <- function(cfg, main_df, timers = NULL) {
  if (is.null(main_df) || !is.data.frame(main_df))
    stop("run_final_cv_tmle requires a non-null main data frame.", call. = FALSE)
  if (is.null(cfg$preprocessing$global_missing_dictionary))
    cfg$preprocessing$global_missing_dictionary <- attr(main_df, "global_missing_dictionary")
  if (is.null(cfg$preprocessing$global_missing_dictionary) ||
      !length(cfg$preprocessing$global_missing_dictionary))
    stop("run_final_cv_tmle requires the frozen global Wave I missing-code dictionary.", call. = FALSE)
  cfg$preprocessing$variable_source_registry <- get_variable_source_registry(
    main_df, cfg, required = TRUE)
  main_df <- canonicalize_mandatory_W_columns(main_df, cfg)
  attr(main_df, "variable_source_registry") <- cfg$preprocessing$variable_source_registry
  validate_candidate_governance(main_df, cfg)
  if (!is.null(timers)) timers$start("final_cv_tmle")
  msg("\n===== STAGE: Final CV-TMLE =====", cfg = cfg)
  msg(sprintf("  V outer folds: %d  |  Internal SL folds: %d  |  Cluster-aware internal CV: %s",
    cfg$final_tmle$vfolds, cfg$final_tmle$internal_superlearner_folds,
    isTRUE(cfg$final_tmle$cluster_aware_internal_cv)), cfg = cfg)
  msg(sprintf("  g bounds: [%g, %g]  |  pi bounds: [%g, %g]",
    cfg$final_tmle$g_lower, cfg$final_tmle$g_upper,
    cfg$final_tmle$pi_lower, cfg$final_tmle$pi_upper), cfg = cfg)
  msg(sprintf("  Nested rough screen in-fold: %s  |  Fold checkpoints: %s",
    isTRUE(cfg$final_tmle$nested_rough_prescreen_in_final_cv),
    isTRUE(cfg$final_tmle$use_fold_checkpoints)), cfg = cfg)
  msg(sprintf("  Nested screen caps: Y top %d, delta top %d, joint A/Y top %d, final cap %d.",
    cfg$final_tmle$rough_top_n_outcome, cfg$final_tmle$rough_top_n_missingness,
    cfg$final_tmle$rough_top_n_joint_AY, cfg$final_tmle$rough_max_total_vars), cfg = cfg)
  ensure_output_dir(cfg$global$output_dir)
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    try(RhpcBLASctl::blas_set_num_threads(1L), silent = TRUE)
    msg("  [TMLE] BLAS restricted to 1 thread (prevents nested OOM).", cfg = cfg)
  }
  msg("  [TMLE] Registering custom SuperLearner wrappers (ranger/xgboost/earth/glmnet/gam)...", cfg = cfg)
  register_custom_learners(cfg)
  checkpoint_dir <- file.path(cfg$global$output_dir, cfg$global$checkpoint_subdir)
  ensure_output_dir(checkpoint_dir)
  msg(sprintf("  [TMLE] Checkpoint directory: %s", checkpoint_dir), cfg = cfg)

  full_survey_design_frame <- attr(main_df, "survey_design_frame")
  if (is.null(full_survey_design_frame) || !is.data.frame(full_survey_design_frame))
    stop("Final CV-TMLE requires the complete valid-weight Wave-I survey-design frame for domain variance estimation; rebuild the main dataset cache.", call. = FALSE)
  msg("  [TMLE] Preparing final analysis data (bounding Y, building delta)...", cfg = cfg)
  data_pack <- prepare_final_analysis_data(main_df, cfg)
  main_df   <- data_pack$df
  missing_mandatory <- setdiff(get_mandatory_W(cfg), names(main_df))
  if (length(missing_mandatory) > 0L) {
    stop("Missing mandatory Wave-I confounder(s): ",
         paste(missing_mandatory, collapse = ", "), call. = FALSE)
  }
  A         <- data_pack$A
  Y_raw     <- data_pack$Y_raw
  Y_bounded_orig <- data_pack$Y_bounded_orig
  Y_star    <- data_pack$Y_star
  delta_Y   <- data_pack$delta_Y
  weights   <- data_pack$weights
  cluster   <- data_pack$cluster
  strata    <- data_pack$strata
  respondent_ids <- as.character(main_df[[cfg$analysis$id_var]])
  if (length(respondent_ids) != length(weights) || anyNA(respondent_ids) || anyDuplicated(respondent_ids))
    stop("Final CV-TMLE requires complete unique respondent IDs aligned with the EIF rows.", call. = FALSE)
  infer_primary_eif <- function(D_eif, estimate_eif) {
    cluster_inference_from_eif(
      D = D_eif, weights = weights, cluster = cluster, estimate = estimate_eif,
      strata = strata, design_frame = full_survey_design_frame,
      domain_ids = respondent_ids, id_var = cfg$analysis$id_var,
      cluster_var = cfg$analysis$cluster_var, strata_var = cfg$analysis$strata_var,
      weight_var = cfg$analysis$weight_var)
  }
  if (is.null(strata))
    stop(sprintf("Required Add Health design stratum '%s' is absent from the analytic data.",
                 cfg$analysis$strata_var), call. = FALSE)
  outcome_type <- data_pack$outcome_type
  compensation_transform <- tolower(cfg$outcome$compensation_transform %||% "identity")
  is_raw_dollar_outcome <- identical(cfg$outcome$family, "Compensation") &&
    identical(compensation_transform, "identity")
  family_cfg_current <- cfg$outcome$families[[cfg$outcome$family]]
  report_ratio_translations <- compensation_ratio_translation_enabled(cfg)
  y_lower <- data_pack$y_lower; y_upper <- data_pack$y_upper
  y_range <- data_pack$y_range
  cfg$final_tmle$screen_y_lower <- y_lower
  cfg$final_tmle$screen_y_upper <- y_upper
  n <- nrow(main_df); V <- cfg$final_tmle$vfolds
  msg(sprintf("  [TMLE] Analytic sample: n=%d, clusters=%d, exposed=%d (%.1f%%), outcome-observed=%d (%.1f%%).",
    n, length(unique(cluster)), sum(A == 1L), 100 * mean(A),
    sum(delta_Y == 1L), 100 * mean(delta_Y)), cfg = cfg)
  msg(sprintf("  [TMLE] Y bounded to [%.3f, %.3f] (range=%.3f) for fluctuation step.",
    y_lower, y_upper, y_range), cfg = cfg)

  msg("  [TMLE] Constructing cluster-balanced outer folds...", cfg = cfg)
  outer_fold <- make_final_cv_folds(data_pack, cfg)
  V_requested <- V
  V <- max(outer_fold)
  if (V != V_requested)
    msg(sprintf("    Requested %d outer folds; support constraints permit %d.", V_requested, V), cfg = cfg)
  msg(sprintf("    Outer fold sizes:      %s", paste(as.vector(table(outer_fold)), collapse = ", ")), cfg = cfg)
  msg(sprintf("    Treated per fold:      %s",
    paste(tapply(A, outer_fold, sum), collapse = ", ")), cfg = cfg)

  Qbar1W <- Qbar0W <- QbarAW <- rep(NA_real_, n)
  gn     <- rep(NA_real_, n); pi_AW <- pi_1W <- pi_0W <- rep(NA_real_, n)
  gn_raw <- rep(NA_real_, n); pi_AW_raw <- pi_1W_raw <- pi_0W_raw <- rep(NA_real_, n)
  Y_star_obs <- Y_star

  per_fold_log <- list()
  fold_support_log <- list()
  internal_fold_support_log <- list()
  fold_times   <- numeric(V)
  selected_by_fold <- list()
  protected_mapping_by_fold <- list()
  nested_selection_log <- list()
  cluster_assignment_log <- list()   # per-fold cluster assignments
  run_manifest_rows <- list()
  q_clip_log <- list()

  lib_Q  <- build_sl_library(cfg, "Q")
  lib_g  <- build_sl_library(cfg, "g")
  lib_pi <- build_sl_library(cfg, "pi")
  msg(sprintf("  [TMLE] SL libraries:  Q=[%s]", paste(lib_Q, collapse = ", ")), cfg = cfg)
  msg(sprintf("                        g=[%s]", paste(lib_g, collapse = ", ")), cfg = cfg)
  msg(sprintf("                       pi=[%s]", paste(lib_pi, collapse = ", ")), cfg = cfg)
  base_seed <- seed_for(cfg, 9000L)

  for (v in seq_len(V)) {
    fold_ck <- file.path(checkpoint_dir, sprintf("fold_%02d.rds", v))
    tr <- which(outer_fold != v); te <- which(outer_fold == v)
    current_fp <- if (isTRUE(cfg$final_tmle$use_fold_checkpoints)) {
      make_fold_checkpoint_fingerprint(cfg, main_df, data_pack, outer_fold, v)
    } else {
      list(pipeline_version = cfg$global$version, fold_id = v,
           checkpoints_disabled = TRUE)
    }

    if (isTRUE(cfg$final_tmle$use_fold_checkpoints) && file.exists(fold_ck)) {
      msg(sprintf("\n  [fold %d/%d] Found checkpoint %s; validating fingerprint...", v, V, fold_ck), cfg = cfg)
      cached <- readRDS(fold_ck)
      if (!is.null(cached$fingerprint) && identical(cached$fingerprint, current_fp)) {
        te <- cached$valid_idx
        Qbar1W[te] <- cached$Qbar1W; Qbar0W[te] <- cached$Qbar0W
        QbarAW[te] <- cached$QbarAW
        gn[te]    <- cached$gn
        pi_AW[te] <- cached$pi_AW; pi_1W[te] <- cached$pi_1W; pi_0W[te] <- cached$pi_0W
        gn_raw[te] <- cached$gn_raw %||% cached$gn
        pi_AW_raw[te] <- cached$pi_AW_raw %||% cached$pi_AW
        pi_1W_raw[te] <- cached$pi_1W_raw %||% cached$pi_1W
        pi_0W_raw[te] <- cached$pi_0W_raw %||% cached$pi_0W
        per_fold_log[[v]] <- cached$sl_log
        fold_times[v]     <- cached$fold_time
        selected_by_fold[[v]] <- cached$selected_vars
        if (!is.null(cached$protected_raw_to_processed))
          protected_mapping_by_fold[[v]] <- cached$protected_raw_to_processed
        if (!is.null(cached$selection_table)) nested_selection_log[[v]] <- cached$selection_table
        if (!is.null(cached$outer_support)) fold_support_log[[v]] <- cached$outer_support
        if (!is.null(cached$run_manifest_row)) run_manifest_rows[[v]] <- cached$run_manifest_row
        if (!is.null(cached$q_clip_log)) q_clip_log[[v]] <- cached$q_clip_log
        if (!is.null(cached$internal_support)) internal_fold_support_log[[paste0(v, "_cached")]] <- cached$internal_support
        # (point 2): also restore cached cluster assignments, so a fold
        # reused from checkpoint is not silently dropped from cluster_assignments.csv
        # (needed to audit H1FS/H1GH over-collapse across ALL folds).
        if (!is.null(cached$cluster_assignments) && nrow(cached$cluster_assignments) > 0L)
          cluster_assignment_log[[length(cluster_assignment_log) + 1L]] <- cached$cluster_assignments
        msg(sprintf("    Fingerprint OK; reused %d cached nuisance estimates (fold originally took %.1fs).",
          length(te), cached$fold_time), cfg = cfg)
        next
      } else {
        msg("    Checkpoint fingerprint missing or stale; recomputing this fold.", cfg = cfg)
      }
    }

    t0 <- proc.time()[3]
    msg(sprintf("\n  [fold %d/%d] NEW fold: training rows=%d, validation rows=%d.",
                v, V, length(tr), length(te)), cfg = cfg)
    msg(sprintf("    Training: %d exposed (%.1f%%), %d outcome-observed (%.1f%%).",
      sum(A[tr] == 1L), 100 * mean(A[tr]),
      sum(delta_Y[tr] == 1L), 100 * mean(delta_Y[tr])), cfg = cfg)
    obs_treated_train <- sum(A[tr] == 1L & delta_Y[tr] == 1L)
    obs_treated_valid <- sum(A[te] == 1L & delta_Y[te] == 1L)
    # (point 3): per-fold binary event counts among OBSERVED rows
    # (delta==1), so a fold with treated rows but very few treated events or
    # non-events is visible for a binary outcome (e.g. LFP). NA for continuous.
    .isbin_fs <- identical(outcome_type, "binary")
    .evc <- function(idx, a, yv) if (.isbin_fs)
      sum(delta_Y[idx] == 1L & A[idx] == a & Y_raw[idx] == yv, na.rm = TRUE) else NA_integer_
    fold_support_log[[v]] <- data.frame(
      fold = v, n_train = length(tr), n_valid = length(te),
      clusters_train = length(unique(cluster[tr])), clusters_valid = length(unique(cluster[te])),
      treated_train = sum(A[tr] == 1L), treated_valid = sum(A[te] == 1L),
      observed_train = sum(delta_Y[tr] == 1L), observed_valid = sum(delta_Y[te] == 1L),
      observed_treated_train = obs_treated_train, observed_treated_valid = obs_treated_valid,
      ess_train = kish_ess_safe(weights[tr]), ess_valid = kish_ess_safe(weights[te]),
      ess_treated_train = kish_ess_safe(weights[tr][A[tr] == 1L]),
      ess_treated_valid = kish_ess_safe(weights[te][A[te] == 1L]),
      Y1_treated_train = .evc(tr, 1L, 1), Y0_treated_train = .evc(tr, 1L, 0),
      Y1_control_train = .evc(tr, 0L, 1), Y0_control_train = .evc(tr, 0L, 0),
      Y1_treated_valid = .evc(te, 1L, 1), Y0_treated_valid = .evc(te, 1L, 0),
      Y1_control_valid = .evc(te, 0L, 1), Y0_control_valid = .evc(te, 0L, 0),
      stringsAsFactors = FALSE)
    if (sum(A[tr] == 1L) < cfg$final_tmle$min_treated_warning_n)
      warning(sprintf("Fold %d has only %d treated training rows.",
                      v, sum(A[tr] == 1L)))
    if (obs_treated_train < (cfg$final_tmle$min_observed_treated_warning_n %||% 20L))
      warning(sprintf("Fold %d has only %d outcome-observed treated training rows.",
                      v, obs_treated_train))
    if (is.finite(fold_support_log[[v]]$ess_treated_train) &&
        fold_support_log[[v]]$ess_treated_train < (cfg$final_tmle$min_ess_treated_train_warning %||% 20))
      warning(sprintf("Fold %d has treated effective sample size %.1f below warning threshold %.1f.",
                      v, fold_support_log[[v]]$ess_treated_train,
                      cfg$final_tmle$min_ess_treated_train_warning %||% 20))
    # --- Nested data-driven screen on training rows only ----------------
    # Reset fold-local selection objects so optional checkpoint metadata can
    # never inherit an object created in a previous outer fold.
    rough_sel <- NULL
    # if a pre-specified W is supplied, bypass the nested screen and
    # use the fixed, theoretically-motivated variable list (intersected with
    # available columns). This supports the pre-specified-W sensitivity
    # scenario that addresses the "data-driven selection introduced bias"
    # objection. The list is the same in every fold by construction.
    prespec_W <- cfg$final_tmle$prespecified_W
    if (!is.null(prespec_W) && length(prespec_W) > 0L) {
      sel_vars <- intersect(prespec_W, names(main_df))
      if (length(sel_vars) == 0L)
        stop("prespecified_W contains no variables present in the dataset.", call. = FALSE)
      nested_selection_log[[v]] <- data.frame(
        fold = v, variable = sel_vars, source = "prespecified",
        stringsAsFactors = FALSE)
      msg(sprintf("    [fold %d] Using pre-specified W: %d of %d variables present.",
        v, length(sel_vars), length(prespec_W)), cfg = cfg)
    } else if (isTRUE(cfg$final_tmle$nested_rough_prescreen_in_final_cv)) {
      t_sel <- proc.time()[3]
      rough_sel <- run_nested_rough_prescreen_for_final(main_df, tr, cfg, fold_id = v)
      sel_vars <- rough_sel$selected_vars
      nested_selection_log[[v]] <- rough_sel$selection_table
      if (!is.null(rough_sel$cluster_assignments) && nrow(rough_sel$cluster_assignments) > 0L)
        cluster_assignment_log[[length(cluster_assignment_log) + 1L]] <- rough_sel$cluster_assignments
      msg(sprintf("    [fold %d] Nested data-driven screen selected %d variables in %.1fs.",
        v, length(sel_vars), proc.time()[3] - t_sel), cfg = cfg)
    } else {
      stop("No final screening path is active. Enable nested_rough_prescreen_in_final_cv=TRUE. Silent fallback to all candidates is disabled.",
           call. = FALSE)
    }
    if (length(sel_vars) == 0L)
      stop(sprintf("Fold %d selected zero variables.", v), call. = FALSE)
    selected_by_fold[[v]] <- sel_vars
    # Fold-pure W preprocessing: learn every preprocessing rule on training
    # rows, apply the frozen recipe to validation rows, then sanitize names.
    t_W <- proc.time()[3]
    # Fixed nonprotected processed-column engineering budget. Statistical
    # adequacy is assessed through cluster-held-out nuisance performance,
    # positivity, convergence, ESS diagnostics, and full-refit budget
    # sensitivities rather than an events-per-column formula.
    column_budget_applied <- as.integer(cfg$final_tmle$final_max_processed_columns %||% 260L)
    W_pack <- build_final_W_train_valid(
      train_df = main_df[tr, , drop = FALSE],
      valid_df = main_df[te, , drop = FALSE],
      selected_vars = sel_vars,
      cfg = cfg,
      priority_table = nested_selection_log[[v]],
      processed_cap_override = column_budget_applied)
    W_tr <- W_pack$train
    W_te <- W_pack$valid
    protected_mapping_by_fold[[v]] <- stats::setNames(
      as.integer(W_pack$protected_raw_to_processed), W_pack$protected_raw_names)
    msg(sprintf(
      "    [fold %d] W recipe built in %.1fs: %d raw vars -> %d processed cols; missing selected=%d; validation missing cols filled=%d; validation extras dropped=%d.",
      v, W_pack$seconds, W_pack$n_raw, W_pack$n_processed,
      W_pack$n_missing_selected, W_pack$missing_in_validation,
      W_pack$extra_in_validation), cfg = cfg)
    if (!is.null(nested_selection_log[[v]]) && "variable" %in% names(nested_selection_log[[v]])) {
      nested_selection_log[[v]]$kept_in_final_W <- nested_selection_log[[v]]$variable %in% W_pack$kept_raw_vars
      nested_selection_log[[v]]$dropped_by_column_cap <- nested_selection_log[[v]]$variable %in% W_pack$dropped_by_column_cap
    }
    # per-fold run manifest -- the design-verification numbers for this
    # fold (cap applied, columns produced, whether the cap bound, support).
    run_manifest_rows[[v]] <- data.frame(
      pipeline_version = cfg$global$version,
      run_id = cfg$global$run_id %||% NA_character_,
      script_md5 = get_frozen_source_fingerprint(cfg)$md5,
      analysis_spec_md5 = get_frozen_config_hash(cfg, "analysis"),
      resolved_run_config_md5 = get_frozen_config_hash(cfg, "resolved"),
      R_version = cfg$provenance$runtime$R_version,
      runtime_platform = cfg$provenance$runtime$platform,
      package_versions = cfg$provenance$runtime$packages,
      pipeline_seed = cfg$global$pipeline_seed,
      checkpoint_subdir = cfg$global$checkpoint_subdir,
      main_dataset_cache_fingerprint_md5 = object_md5(
        attr(main_df, "main_dataset_cache_fingerprint") %||%
          list(status = "unavailable_for_direct_main_df_input")),
      fold_checkpoint_fingerprint_md5 = object_md5(current_fp),
      target_population = "Wave I probability-sample adolescents with complete Wave II Feelings Scale",
      weight_variable = cfg$analysis$weight_var,
      outcome_family = cfg$outcome$family,
      outcome_wave = cfg$outcome$current_wave %||% cfg$outcome$waves,
      outcome_scale = configured_outcome_scale(cfg, outcome_type),
      outcome_cap_probability = if (identical(outcome_type, "continuous")) cfg$outcome$continuous_upper_quantile else NA_real_,
      outcome_cap_value = data_pack$cap_value,
      outcome_bound_lower = y_lower,
      outcome_bound_upper = y_upper,
      outcome_cap_weighted = if (identical(outcome_type, "continuous")) isTRUE(cfg$outcome$continuous_cap_weighted) else FALSE,
      outcome_cap_quantile_rule = if (identical(outcome_type, "continuous")) cfg$outcome$continuous_cap_qrule else NA_character_,
      earnings_exact_valid_min = if (identical(cfg$outcome$family, "Compensation")) cfg$outcome$families$Compensation$exact_valid_min %||% NA_real_ else NA_real_,
      earnings_exact_valid_max = if (identical(cfg$outcome$family, "Compensation")) cfg$outcome$families$Compensation$exact_valid_max %||% NA_real_ else NA_real_,
      primary_estimand = cfg$final_tmle$primary_estimand,
      policy_primary_percentage = cfg$final_tmle$percentage_primary,
      policy_components_enabled = isTRUE(cfg$policy$enable_policy_components %||% TRUE),
      policy_translation_enabled = isTRUE(cfg$policy$enable_att_prevalence_translation %||% TRUE),
      mortality_sensitivity_enabled = isTRUE(cfg$mortality_sensitivity$enabled %||% FALSE),
      mortality_composite_zero_at_death =
        isTRUE(cfg$mortality_sensitivity$composite_zero_at_death %||% FALSE),
      mortality_fail_on_observed_outcome_contradiction = isTRUE(
        cfg$mortality_sensitivity$fail_on_death_with_observed_original_outcome %||% TRUE),
      mortality_source_variable = cfg$mortality_sensitivity$source_var %||% NA_character_,
      mortality_death_year_start = cfg$mortality_sensitivity$death_year_start %||% NA_integer_,
      mortality_death_year_end = cfg$mortality_sensitivity$death_year_end %||% NA_integer_,
      wave2_completion_diagnostic_enabled =
        isTRUE(cfg$diagnostics$enable_wave2_completion_diagnostic %||% TRUE),
      mnar_pattern_mixture_enabled =
        isTRUE(cfg$diagnostics$enable_mnar_pattern_mixture %||% TRUE),
      evalue_enabled = isTRUE(cfg$diagnostics$enable_evalue %||% FALSE),
      global_missing_dictionary_md5 = object_md5(cfg$preprocessing$global_missing_dictionary),
      protected_W = paste(cfg$final_tmle$protected_W %||% character(0), collapse = ";"),
      mandatory_W = paste(get_mandatory_W(cfg), collapse = ";"),
      cap_priority_version = "protected;A_and_Y_lasso;joint_AY;Y_lasso;delta_lasso;marginal_Y;marginal_delta;A_lasso;A_only",
      q_library = paste(lib_Q, collapse = ";"),
      g_library = paste(lib_g, collapse = ";"),
      pi_library = paste(lib_pi, collapse = ";"),
      fixed_xgboost_settings = paste(names(unlist(cfg$learners$xgboost)), unlist(cfg$learners$xgboost), sep = "=", collapse = ";"),
      q_xgboost_rich_settings = paste(names(unlist(cfg$learners$xgboost_rich)), unlist(cfg$learners$xgboost_rich), sep = "=", collapse = ";"),
      glmnet_settings = paste(names(unlist(cfg$learners$glmnet)), unlist(cfg$learners$glmnet), sep = "=", collapse = ";"),
      g_h1fs_penalty_multiplier = cfg$learners$glmnet_h1fs$h1fs_penalty_multiplier %||% NA_real_,
      pi_A_penalty_multiplier = cfg$learners$glmnet_pi_A$A_penalty_multiplier %||% NA_real_,
      run_label = cfg$global$run_label %||% NA_character_,
      outcome_definition = cfg$provenance$outcome_definition %||% configured_outcome_definition(cfg),
      outer_fold_balance = if (isTRUE(cfg$final_tmle$outer_fold_balance_on_weights))
        "size_first_weighted_treatment_with_hard_A_delta_support" else "size_first_raw_treatment_with_hard_A_delta_support",
      internal_fold_balance = if (isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
        "size_first_weighted_treatment_with_hard_A_delta_support" else "size_first_raw_treatment_with_hard_A_delta_support",
      fold = v,
      treated_train = sum(A[tr] == 1L),
      treated_valid = sum(A[te] == 1L),
      observed_treated_train = sum(A[tr] == 1L & delta_Y[tr] == 1L),
      observed_treated_valid = sum(A[te] == 1L & delta_Y[te] == 1L),
      clusters_valid = length(unique(cluster[te])),
      rows_valid = length(te),
      rows_train = length(tr),
      outer_fold_size_ratio = attr(outer_fold, "fold_diagnostics")$size_ratio %||% NA_real_,
      outer_fold_max_size_deviation_prop = attr(outer_fold, "fold_diagnostics")$size_deviation_prop %||% NA_real_,
      nonprotected_column_budget = column_budget_applied,
      n_selected_vars = length(sel_vars),
      n_raw_into_W = W_pack$n_raw,
      n_processed_cols = W_pack$n_processed,
      n_processed_cols_nonprotected = W_pack$n_processed_nonprotected,
      n_processed_cols_optional_budgeted = W_pack$n_processed_optional_budgeted,
      n_protected_cols = W_pack$n_protected_cols,
      n_mandatory_cols = W_pack$n_mandatory_cols,
      raw_treated_per_nonprotected_column = if (W_pack$n_processed_nonprotected > 0)
        sum(A[tr] == 1L) / W_pack$n_processed_nonprotected else NA_real_,
      raw_treated_per_optional_budgeted_column =
        if (W_pack$n_processed_optional_budgeted > 0)
          sum(A[tr] == 1L) / W_pack$n_processed_optional_budgeted else NA_real_,
      treated_ess_per_total_column_diagnostic = if (W_pack$n_processed > 0)
        kish_ess_safe(weights[tr][A[tr] == 1L]) / W_pack$n_processed else NA_real_,
      cap_binding = length(W_pack$dropped_by_column_cap) > 0L,
      ess_treated_train = kish_ess_safe(weights[tr][A[tr] == 1L]),
      stringsAsFactors = FALSE)

    A_tr <- A[tr]; A_te <- A[te]
    Y_star_tr <- Y_star[tr]; delta_tr <- delta_Y[tr]
    w_tr <- weights[tr]; cl_tr <- cluster[tr]

    # Internal CV folds are always whole-PSU. Q and g require treatment support;
    # pi additionally balances treatment-by-outcome-observation cells. No fold uses continuous outcome values.
    V_int <- cfg$final_tmle$internal_superlearner_folds
    lcfg_nfolds <- as.integer(cfg$learners$glmnet$internal_folds %||% 5L)
    if (lcfg_nfolds < 3L) stop("learners$glmnet$internal_folds must be at least 3.", call. = FALSE)
    validRows_Q  <- build_cluster_valid_rows(cl_tr[delta_tr == 1L],
                                             A_tr[delta_tr == 1L],
                                             V_int, base_seed + v,
                                             weights = w_tr[delta_tr == 1L],
                                             delta = rep(1L, sum(delta_tr == 1L)),
                                             balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights),
                                             fold_control = fold_control_from_cfg(cfg, "internal"))
    validRows_g  <- build_cluster_valid_rows(cl_tr, A_tr, V_int, base_seed + v + 100L,
                                             weights = w_tr, delta = rep(1L, length(A_tr)),
                                             balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights),
                                             fold_control = fold_control_from_cfg(cfg, "internal"))
    validRows_pi <- build_cluster_valid_rows(cl_tr, A_tr, V_int, base_seed + v + 200L,
                                             weights = w_tr, delta = delta_tr,
                                             balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights),
                                             fold_control = fold_control_from_cfg(cfg, "internal"))
    cvQ  <- list(V = length(validRows_Q),  validRows = validRows_Q,  stratifyCV = FALSE, shuffle = FALSE)
    cvg  <- list(V = length(validRows_g),  validRows = validRows_g,  stratifyCV = FALSE, shuffle = FALSE)
    cvpi <- list(V = length(validRows_pi), validRows = validRows_pi, stratifyCV = FALSE, shuffle = FALSE)
    msg(sprintf("    [fold %d] Internal SL folds: Q=%d, g=%d, pi=%d.",
      v, length(validRows_Q), length(validRows_g), length(validRows_pi)), cfg = cfg)
    warn_internal_valid_rows(validRows_Q, A_tr[delta_tr == 1L], "Q", cfg, v,
                             delta_vec = rep(1L, sum(delta_tr == 1L)), weights = w_tr[delta_tr == 1L])
    warn_internal_valid_rows(validRows_g, A_tr, "g", cfg, v, delta_vec = rep(1L, length(A_tr)), weights = w_tr)
    warn_internal_valid_rows(validRows_pi, A_tr, "pi", cfg, v, delta_vec = delta_tr, weights = w_tr)
    internal_fold_support_log[[paste0(v, "_Q")]] <- make_internal_fold_support(
      validRows_Q, A_tr[delta_tr == 1L], rep(1L, sum(delta_tr == 1L)),
      w_tr[delta_tr == 1L], "Q", v)
    internal_fold_support_log[[paste0(v, "_g")]] <- make_internal_fold_support(
      validRows_g, A_tr, rep(1L, length(A_tr)), w_tr, "g", v)
    internal_fold_support_log[[paste0(v, "_pi")]] <- make_internal_fold_support(
      validRows_pi, A_tr, delta_tr, w_tr, "pi", v)

    .SL_RUNTIME_ENV$h1fs_processed_columns <- names(W_tr)[
      W_pack$processed_raw_map[names(W_tr)] %in% (cfg$final_tmle$protected_W %||% character(0))]
    if (isTRUE(cfg$learners$g$use_glmnet_h1fs %||% FALSE) &&
        length(.SL_RUNTIME_ENV$h1fs_processed_columns) != W_pack$n_protected_cols)
      stop(sprintf("Fold %d H1FS penalty mapping mismatch: mapped %d columns, expected %d.",
                   v, length(.SL_RUNTIME_ENV$h1fs_processed_columns), W_pack$n_protected_cols), call. = FALSE)

    # --- Fit Q on rows with observed outcome --------------------------------
    idx_obs <- which(delta_tr == 1L)
    msg(sprintf("    [fold %d] Fitting Q on %d outcome-observed training rows...", v, length(idx_obs)), cfg = cfg)
    t_Q <- proc.time()[3]
    # v6 Fix B: data.frame (not cbind) guarantees a data.frame result.
    # cbind(vector, data.frame) can return a matrix under R's method dispatch,
    # which breaks earth's formula path and other learners that call model.frame.
    X_Q_tr  <- data.frame(A = A_tr, W_tr, check.names = FALSE)[idx_obs, , drop = FALSE]
    X_Q_te_A1 <- data.frame(A = rep(1L, length(te)), W_te, check.names = FALSE)
    X_Q_te_A0 <- data.frame(A = rep(0L, length(te)), W_te, check.names = FALSE)
    X_Q_te_AA <- data.frame(A = A_te,                W_te, check.names = FALSE)
    Q_fit <- tryCatch(with_sl_glmnet_context(
      "Q", base_seed + v + 10000L, lcfg_nfolds,
      with_local_seed(base_seed + v + 30000L,
        SuperLearner::SuperLearner(
          Y = Y_star_tr[idx_obs], X = X_Q_tr,
          newX = rbind(X_Q_te_A1, X_Q_te_A0, X_Q_te_AA),
          family = if (identical(data_pack$outcome_type, "binary")) stats::binomial() else stats::gaussian(),
          SL.library = lib_Q,
          obsWeights = w_tr[idx_obs], id = as.character(cl_tr[idx_obs]),
          cvControl = cvQ,
          control = list(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE),
          verbose = FALSE))),
      error = function(e) { warning("Q SuperLearner failed: ", conditionMessage(e)); NULL })
    n_te <- length(te)
    q_lo <- cfg$final_tmle$Q_pred_lower %||% 0.005
    q_hi <- cfg$final_tmle$Q_pred_upper %||% 0.995
    if (is.null(Q_fit)) {
      if (isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE))
        stop(sprintf("Fold %d Q SuperLearner failed; strict production mode forbids fallback.", v), call. = FALSE)
      ybar <- stats::weighted.mean(Y_star_tr[idx_obs], w_tr[idx_obs], na.rm = TRUE)
      raw_mean <- rep(ybar, n_te)
      Qbar1W_v <- pmin(pmax(raw_mean, q_lo), q_hi)
      Qbar0W_v <- Qbar1W_v
      QbarAW_v <- Qbar1W_v
      q_fallback <- TRUE
      q_clip_log[[v]] <- rbind(
        summarize_prediction_clipping(raw_mean, q_lo, q_hi, v, "Q1W_fallback"),
        summarize_prediction_clipping(raw_mean, q_lo, q_hi, v, "Q0W_fallback"),
        summarize_prediction_clipping(raw_mean, q_lo, q_hi, v, "QAW_fallback"))
      q_coef <- numeric(0); q_risk <- numeric(0)
      msg(sprintf("      [fold %d] Q FALLBACK to grand mean %.4f (SL failed).", v, ybar), cfg = cfg)
    } else {
      assert_superlearner_fit(Q_fit, lib_Q, "Q", v, 3L * n_te)
      # Bound the held-out ensemble predictions before they enter the
      # logit-fluctuation step. Gaussian-loss learners can predict outside
      # [0,1]; this post-fit bound stabilizes qlogis. SuperLearner CV risks are
      # computed before this bound, so the clipping fraction is logged as a
      # separate diagnostic rather than treated as part of meta-learning.
      preds <- as.numeric(Q_fit$SL.predict)
      if (length(preds) != 3L * n_te || any(!is.finite(preds)))
        stop(sprintf("Fold %d Q returned %d predictions; expected %d finite values.", v, length(preds), 3L * n_te), call. = FALSE)
      pred_q1 <- preds[1:n_te]
      pred_q0 <- preds[(n_te+1):(2*n_te)]
      pred_qa <- preds[(2*n_te+1):(3*n_te)]
      q_clip_log[[v]] <- rbind(
        summarize_prediction_clipping(pred_q1, q_lo, q_hi, v, "Q1W"),
        summarize_prediction_clipping(pred_q0, q_lo, q_hi, v, "Q0W"),
        summarize_prediction_clipping(pred_qa, q_lo, q_hi, v, "QAW"),
        summarize_prediction_clipping(preds, q_lo, q_hi, v, "pooled"))
      n_clip_q <- q_clip_log[[v]]$n_clipped[q_clip_log[[v]]$prediction_set == "pooled"]
      Qbar1W_v <- pmin(pmax(pred_q1, q_lo), q_hi)
      Qbar0W_v <- pmin(pmax(pred_q0, q_lo), q_hi)
      QbarAW_v <- pmin(pmax(pred_qa, q_lo), q_hi)
      q_fallback <- FALSE
      q_coef <- Q_fit$coef; q_risk <- Q_fit$cvRisk
      top_q <- names(q_coef)[which.max(q_coef)]
      msg(sprintf("      [fold %d] Q fit in %.1fs. Top learner: %s (weight=%.3f). Qbar1W in [%.3f, %.3f].",
        v, proc.time()[3] - t_Q, top_q, max(q_coef),
        min(Qbar1W_v), max(Qbar1W_v)), cfg = cfg)
      msg(sprintf("      [fold %d] Q predictions: %d of %d (%.1f%%) hit a bound and were clipped to [%.3f, %.3f].",
        v, n_clip_q, length(preds), 100 * n_clip_q / length(preds),
        q_lo, q_hi), cfg = cfg)
    }

    # --- Fit g (propensity) -----------------------------------------------
    msg(sprintf("    [fold %d] Fitting g (propensity) on %d training rows...", v, length(tr)), cfg = cfg)
    t_g <- proc.time()[3]
    g_fit <- tryCatch(with_sl_glmnet_context(
      "g", base_seed + v + 11000L, lcfg_nfolds,
      with_local_seed(base_seed + v + 31000L,
        SuperLearner::SuperLearner(
          Y = A_tr, X = W_tr, newX = W_te, family = stats::binomial(),
          SL.library = lib_g, obsWeights = w_tr, id = as.character(cl_tr),
          cvControl = cvg,
          control = list(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE),
          verbose = FALSE))),
      error = function(e) { warning("g SuperLearner failed: ", conditionMessage(e)); NULL })
    if (is.null(g_fit)) {
      if (isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE))
        stop(sprintf("Fold %d g SuperLearner failed; strict production mode forbids fallback.", v), call. = FALSE)
      g_mean <- stats::weighted.mean(A_tr, w_tr)
      g_te <- rep(g_mean, n_te); g_fallback <- TRUE
      g_coef <- numeric(0); g_risk <- numeric(0)
      msg(sprintf("      [fold %d] g FALLBACK to weighted mean(A)=%.3f (SL failed).", v, g_mean), cfg = cfg)
    } else {
      assert_superlearner_fit(g_fit, lib_g, "g", v, n_te)
      g_te <- as.numeric(g_fit$SL.predict)
      if (length(g_te) != n_te || any(!is.finite(g_te)))
        stop(sprintf("Fold %d g returned invalid held-out predictions.", v), call. = FALSE)
      g_fallback <- FALSE
      g_coef <- g_fit$coef; g_risk <- g_fit$cvRisk
      top_g <- names(g_coef)[which.max(g_coef)]
      msg(sprintf("      [fold %d] g fit in %.1fs. Top learner: %s (weight=%.3f).",
        v, proc.time()[3] - t_g, top_g, max(g_coef)), cfg = cfg)
    }
    g_te_preclip <- g_te
    gn_raw[te] <- g_te_preclip
    g_te <- pmin(pmax(g_te, cfg$final_tmle$g_lower), cfg$final_tmle$g_upper)
    n_clip <- sum(g_te != g_te_preclip)
    msg(sprintf("      [fold %d] g predictions: range=[%.3f, %.3f], %d of %d clipped to [%g, %g].",
      v, min(g_te), max(g_te), n_clip, n_te, cfg$final_tmle$g_lower, cfg$final_tmle$g_upper), cfg = cfg)

    # --- Fit pi (outcome-observed | A, W) ---------------------------------
    msg(sprintf("    [fold %d] Fitting pi (outcome-observed | A,W)...", v), cfg = cfg)
    t_pi <- proc.time()[3]
    # v6 Fix B: data.frame guarantees a data.frame; see Q block above.
    X_pi_tr    <- data.frame(A = A_tr,              W_tr, check.names = FALSE)
    X_pi_te_A1 <- data.frame(A = rep(1L, n_te),     W_te, check.names = FALSE)
    X_pi_te_A0 <- data.frame(A = rep(0L, n_te),     W_te, check.names = FALSE)
    X_pi_te_AA <- data.frame(A = A_te,              W_te, check.names = FALSE)
    pi_deterministic <- FALSE
    if (all(delta_tr == 1L)) {
      pi_1W_v <- rep(1, n_te); pi_0W_v <- rep(1, n_te); pi_AW_v <- rep(1, n_te)
      pi_fallback <- FALSE; pi_deterministic <- TRUE
      pi_coef <- c(deterministic_all_observed = 1)
      pi_risk <- c(deterministic_all_observed = 0)
      msg(sprintf("      [fold %d] pi is deterministically one because all training outcomes are observed.", v), cfg = cfg)
    } else if (all(delta_tr == 0L)) {
      stop(sprintf("Fold %d has no observed training outcomes; Q and pi are unidentified.", v),
           call. = FALSE)
    } else {
        pi_fit <- tryCatch(with_sl_glmnet_context(
        "pi", base_seed + v + 12000L, lcfg_nfolds,
        with_local_seed(base_seed + v + 32000L,
          SuperLearner::SuperLearner(
            Y = delta_tr, X = X_pi_tr,
            newX = rbind(X_pi_te_A1, X_pi_te_A0, X_pi_te_AA),
            family = stats::binomial(), SL.library = lib_pi,
            obsWeights = w_tr, id = as.character(cl_tr),
            cvControl = cvpi,
            control = list(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE),
            verbose = FALSE))),
        error = function(e) { warning("pi SuperLearner failed: ", conditionMessage(e)); NULL })
      if (is.null(pi_fit)) {
        if (isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE))
          stop(sprintf("Fold %d pi SuperLearner failed; strict production mode forbids fallback.", v), call. = FALSE)
        p <- stats::weighted.mean(delta_tr, w_tr)
        pi_1W_v <- rep(p, n_te); pi_0W_v <- rep(p, n_te); pi_AW_v <- rep(p, n_te)
        pi_fallback <- TRUE
        pi_coef <- numeric(0); pi_risk <- numeric(0)
        msg(sprintf("      [fold %d] pi FALLBACK to mean(delta_Y)=%.3f (SL failed).", v, p), cfg = cfg)
      } else {
        assert_superlearner_fit(pi_fit, lib_pi, "pi", v, 3L * n_te)
        preds <- as.numeric(pi_fit$SL.predict)
        if (length(preds) != 3L * n_te || any(!is.finite(preds)))
          stop(sprintf("Fold %d pi returned %d predictions; expected %d finite values.", v, length(preds), 3L * n_te), call. = FALSE)
        pi_blocks <- extract_pi_counterfactual_blocks(preds, n_te)
        pi_1W_v <- pi_blocks$pi_1W
        pi_0W_v <- pi_blocks$pi_0W
        pi_AW_v <- pi_blocks$pi_AW
        pi_fallback <- FALSE
        pi_coef <- pi_fit$coef; pi_risk <- pi_fit$cvRisk
        top_pi <- names(pi_coef)[which.max(pi_coef)]
        msg(sprintf("      [fold %d] pi fit in %.1fs. Top learner: %s (weight=%.3f).",
          v, proc.time()[3] - t_pi, top_pi, max(pi_coef)), cfg = cfg)
      }
    }
    # Preserve raw held-out probabilities for the post-fit g/pi clipping
    # sensitivity sweep, then apply the primary configured bounds.
    pi_1W_raw[te] <- pi_1W_v
    pi_0W_raw[te] <- pi_0W_v
    pi_AW_raw[te] <- pi_AW_v
    # Both-sided clipping (v5)
    pi_1W_v <- pmin(pmax(pi_1W_v, cfg$final_tmle$pi_lower), cfg$final_tmle$pi_upper)
    pi_0W_v <- pmin(pmax(pi_0W_v, cfg$final_tmle$pi_lower), cfg$final_tmle$pi_upper)
    pi_AW_v <- pmin(pmax(pi_AW_v, cfg$final_tmle$pi_lower), cfg$final_tmle$pi_upper)

    # Overlap-product diagnostics: the real positivity quantity for TMLE+censoring.
    op_1 <- g_te * pi_1W_v
    op_0 <- (1 - g_te) * pi_0W_v
    pos_thr <- cfg$final_tmle$positivity_warning_threshold
    frac_low_1 <- mean(op_1 < pos_thr)
    frac_low_0 <- mean(op_0 < pos_thr)
    msg(sprintf("      [fold %d] Overlap products: g*pi1 in [%.3f, %.3f] (%.1f%% < %g); (1-g)*pi0 in [%.3f, %.3f] (%.1f%% < %g).",
      v, min(op_1), max(op_1), 100 * frac_low_1, pos_thr,
      min(op_0), max(op_0), 100 * frac_low_0, pos_thr), cfg = cfg)
    if (frac_low_1 > cfg$final_tmle$positivity_warning_fraction ||
        frac_low_0 > cfg$final_tmle$positivity_warning_fraction) {
      warning(sprintf(
        "Fold %d positivity warning: %.1f%% (g*pi1) or %.1f%% ((1-g)*pi0) below %g.",
        v, 100 * frac_low_1, 100 * frac_low_0, pos_thr))
    }

    # Store held-out nuisance estimates
    Qbar1W[te] <- Qbar1W_v; Qbar0W[te] <- Qbar0W_v; QbarAW[te] <- QbarAW_v
    gn[te]     <- g_te
    pi_AW[te]  <- pi_AW_v; pi_1W[te] <- pi_1W_v; pi_0W[te] <- pi_0W_v

    # Per-fold assertions
    stopifnot(all(is.finite(Qbar1W[te])), all(is.finite(Qbar0W[te])),
              all(is.finite(QbarAW[te])), all(is.finite(gn[te])),
              all(is.finite(pi_AW[te])), all(is.finite(pi_1W[te])),
              all(is.finite(pi_0W[te])))
    stopifnot(all(Qbar1W[te] > 0 & Qbar1W[te] < 1),
              all(Qbar0W[te] > 0 & Qbar0W[te] < 1))
    stopifnot(all(gn[te]    >= cfg$final_tmle$g_lower - 1e-10 &
                  gn[te]    <= cfg$final_tmle$g_upper + 1e-10))
    stopifnot(all(pi_AW[te] >= cfg$final_tmle$pi_lower - 1e-10 &
                  pi_AW[te] <= cfg$final_tmle$pi_upper + 1e-10))

    fold_times[v] <- proc.time()[3] - t0
    msg(sprintf("    [fold %d/%d DONE] total %.1fs.", v, V, fold_times[v]), cfg = cfg)

    # --- Per-fold SL log row -----------------------------------------------
    pack_named_metric <- function(nm, values, digits = 6L) {
      if (length(values) == 0L) return("")
      nm <- as.character(nm)
      if (length(nm) != length(values) || anyNA(nm) || any(!nzchar(nm)))
        nm <- paste0("learner", seq_along(values))
      fmt <- paste0("%.", as.integer(digits), "f")
      paste(paste0(nm, "=", sprintf(fmt, as.numeric(values))), collapse = ";")
    }
    weighted_loss <- function(y, p, w, family_name) {
      ww <- normalize_positive_weights(w, length(y), "outer validation loss")
      if (identical(family_name, "binomial")) {
        pc <- pmin(pmax(p, 1e-12), 1-1e-12)
        c(brier=sum(ww*(y-p)^2)/sum(ww), logloss=-sum(ww*(y*log(pc)+(1-y)*log(1-pc)))/sum(ww))
      } else c(mse=sum(ww*(y-p)^2)/sum(ww), logloss=NA_real_)
    }
    q_outer <- weighted_loss(Y_star[te][delta_Y[te]==1L], QbarAW_v[delta_Y[te]==1L], weights[te][delta_Y[te]==1L],
                             if (identical(data_pack$outcome_type,"binary")) "binomial" else "gaussian")
    g_outer <- weighted_loss(A_te, g_te_preclip, weights[te], "binomial")
    pi_outer <- weighted_loss(delta_Y[te], pi_AW_raw[te], weights[te], "binomial")
    sl_log <- data.frame(
      fold = v,
      n_train = length(tr), n_valid = n_te,
      n_selected_vars = length(sel_vars),
      Q_fallback  = q_fallback,  Q_coef  = pack_named_metric(names(q_coef), q_coef, 3L),
      Q_cv_risk   = pack_named_metric(names(q_risk) %||% names(q_coef), q_risk, 6L),
      g_fallback  = g_fallback,  g_coef  = pack_named_metric(names(g_coef), g_coef, 3L),
      g_cv_risk   = pack_named_metric(names(g_risk) %||% names(g_coef), g_risk, 6L),
      pi_fallback = pi_fallback, pi_deterministic = pi_deterministic,
      pi_coef = pack_named_metric(names(pi_coef), pi_coef, 3L),
      pi_cv_risk  = pack_named_metric(names(pi_risk) %||% names(pi_coef), pi_risk, 6L),
      Q_outer_mse = unname(q_outer["mse"]), Q_outer_brier = unname(q_outer["brier"]),
      g_outer_brier = unname(g_outer["brier"]), g_outer_logloss = unname(g_outer["logloss"]),
      pi_outer_brier = unname(pi_outer["brier"]), pi_outer_logloss = unname(pi_outer["logloss"]),
      fold_seconds = fold_times[v],
      stringsAsFactors = FALSE)
    per_fold_log[[v]] <- sl_log
    pooled_q_clip <- q_clip_log[[v]][q_clip_log[[v]]$prediction_set %in% c("pooled", "QAW_fallback"), , drop = FALSE]
    q_clip_fraction_fold <- if (nrow(pooled_q_clip)) max(pooled_q_clip$fraction_clipped) else 0
    run_manifest_rows[[v]]$q_clip_fraction <- q_clip_fraction_fold
    run_manifest_rows[[v]]$q_clip_review_required <-
      q_clip_fraction_fold >= (cfg$final_tmle$Q_clip_review_fraction %||% 0.05)
    if (q_clip_fraction_fold >= (cfg$final_tmle$Q_clip_warning_fraction %||% 0.01))
      warning(sprintf("Fold %d clipped %.2f%% of pooled held-out Q predictions.",
                      v, 100 * q_clip_fraction_fold), call. = FALSE)
    if (q_clip_fraction_fold >= (cfg$final_tmle$Q_clip_review_fraction %||% 0.05) &&
        isTRUE(cfg$safety$fail_on_q_clip_review_threshold %||% FALSE))
      stop(sprintf("Fold %d exceeded the configured Q-clipping review threshold.", v), call. = FALSE)
    if (isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE) &&
        (isTRUE(q_fallback) || isTRUE(g_fallback) || isTRUE(pi_fallback))) {
      stop(sprintf("Fold %d used a nuisance fallback (Q=%s, g=%s, pi=%s). Set fail_on_nuisance_fallback=FALSE to allow this diagnostic run.",
                   v, q_fallback, g_fallback, pi_fallback), call. = FALSE)
    }

    if (isTRUE(cfg$final_tmle$use_fold_checkpoints)) {
      atomic_save_rds(list(
        valid_idx = te, Qbar1W = Qbar1W_v, Qbar0W = Qbar0W_v, QbarAW = QbarAW_v,
        gn = g_te, pi_AW = pi_AW_v, pi_1W = pi_1W_v, pi_0W = pi_0W_v,
        gn_raw = g_te_preclip, pi_AW_raw = pi_AW_raw[te],
        pi_1W_raw = pi_1W_raw[te], pi_0W_raw = pi_0W_raw[te],
        sl_log = sl_log, fold_time = fold_times[v],
        selected_vars = sel_vars,
        protected_raw_to_processed = protected_mapping_by_fold[[v]],
        selection_table = nested_selection_log[[v]],
        cluster_assignments = if (exists("rough_sel") && !is.null(rough_sel$cluster_assignments))
          rough_sel$cluster_assignments else NULL,
        outer_support = fold_support_log[[v]],
        run_manifest_row = run_manifest_rows[[v]],
        q_clip_log = q_clip_log[[v]],
        internal_support = {
          nm_int <- grep(paste0("^", v, "_"), names(internal_fold_support_log), value = TRUE)
          if (length(nm_int)) do.call(rbind, internal_fold_support_log[nm_int]) else NULL
        },
        fingerprint = current_fp), fold_ck, overwrite = TRUE)
    }
    if (isTRUE(cfg$final_tmle$gc_after_fold %||% TRUE)) {
      rm(list = intersect(c("W_tr", "W_te", "W_pack", "Q_fit", "g_fit", "pi_fit",
                            "X_Q_tr_AW", "X_Q_te_A1", "X_Q_te_A0", "X_Q_te_AA",
                            "X_g_tr", "X_g_te", "X_pi_tr", "X_pi_te_A1",
                            "X_pi_te_A0", "X_pi_te_AA"), ls()), inherits = FALSE)
      invisible(gc(verbose = FALSE))
    }
  }

  q_clip_df <- if (length(q_clip_log)) do.call(rbind, q_clip_log) else data.frame()
  q_clip_primary <- if (nrow(q_clip_df))
    q_clip_df[q_clip_df$prediction_set %in% c("pooled", "QAW_fallback"), , drop = FALSE]
  else data.frame()
  q_clip_overall_fraction <- if (nrow(q_clip_primary) &&
      sum(q_clip_primary$n_predictions) > 0L)
    sum(q_clip_primary$n_clipped) / sum(q_clip_primary$n_predictions) else NA_real_
  q_clip_max_fold_fraction <- if (nrow(q_clip_primary) &&
      any(is.finite(q_clip_primary$fraction_clipped)))
    max(q_clip_primary$fraction_clipped[is.finite(q_clip_primary$fraction_clipped)])
  else NA_real_
  q_clip_review_required <- is.finite(q_clip_max_fold_fraction) &&
    q_clip_max_fold_fraction >= (cfg$final_tmle$Q_clip_review_fraction %||% 0.05)
  if (nrow(q_clip_df) && isTRUE(cfg$global$save_stage_csvs))
    write_run_csv(q_clip_df, cfg,
                  cfg$diagnostics$q_prediction_clipping_csv %||%
                    "q_prediction_clipping.csv")
  msg(sprintf("\n  [TMLE] All %d outer folds complete. Total fold wall time: %.1fs (mean %.1fs/fold).",
    V, sum(fold_times), mean(fold_times)), cfg = cfg)

  # ==========================================================================
  # TMLE fluctuation step on held-out predictions (whole-sample).
  # ==========================================================================
  msg("  [TMLE] Running fluctuation step (logit-link on bounded Y)...", cfg = cfg)
  fluc_H1 <- (A / gn)       * (delta_Y / pi_AW)
  fluc_H0 <- ((1 - A) / (1 - gn)) * (delta_Y / pi_AW)

  # Rows with non-missing Y for updating
  idx_up <- which(delta_Y == 1L)
  # Fluctuation regression: minimize negative binomial-style loss.
  H_obs <- fluc_H1[idx_up] - fluc_H0[idx_up]
  Q_obs <- QbarAW[idx_up]
  # Solve the one-dimensional targeting score directly. A failed score solve
  # is a production error, not a reason to silently use epsilon=0.
  ate_target <- solve_target_score(
    offset = stats::qlogis(Q_obs), H = H_obs, Y = Y_star_obs[idx_up],
    w = weights[idx_up], score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
    max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
    label = "ATE targeting")
  eps_hat <- ate_target$epsilon
  msg(sprintf("    ATE fluctuation epsilon = %.6f; normalized score = %.3e.",
              eps_hat, ate_target$normalized_score), cfg = cfg)

  # BUG FIX: counterfactual updates must use INTERVENTION-SPECIFIC
  # clever covariates evaluated at A=1 and A=0 respectively, NOT the
  # observed-data clever covariate. The observed A and delta_Y belong in
  # the fluctuation regression (which fits eps_hat above) and the EIF
  # residual term, not in the counterfactual update itself. The previous
  # implementation zeroed out the targeting correction for rows where
  # A != 1 (in the Q*(1,W) update) and delta_Y == 0 (in either update),
  # which biased the targeted means toward the initial Q estimates exactly
  # at the rows where targeting is supposed to do the most work.
  H1_all <- 1 / gn        / pi_1W   # clever covariate at A=1, every row
  H0_all <- -1 / (1 - gn) / pi_0W   # clever covariate at A=0, every row
  Qstar1W <- stats::plogis(stats::qlogis(Qbar1W) + eps_hat * H1_all)
  Qstar0W <- stats::plogis(stats::qlogis(Qbar0W) + eps_hat * H0_all)
  # Back-transform from [0,1] to original Y scale
  Qstar1W_orig <- Qstar1W * y_range + y_lower
  Qstar0W_orig <- Qstar0W * y_range + y_lower

  # Weighted ATE on original scale. Also compute reviewer-facing
  # comparators from the same cross-fitted nuisance estimates:
  # - initial plug-in using Qbar before targeting;
  # - initial AIPW/one-step estimator using Qbar, g, and pi.
  # These are diagnostics only; the headline estimate remains the TMLE.
  w_norm <- weights / mean(weights)
  Qbar1W_orig <- Qbar1W * y_range + y_lower
  Qbar0W_orig <- Qbar0W * y_range + y_lower
  QbarAW_orig <- QbarAW * y_range + y_lower
  # Observed outcome winsorized to the SAME bounded support [y_lower, y_upper] on
  # which Q was fit, so the AIPW residual (Y - Qbar) is on the estimand's bounded
  # scale. The one-step ATE below is then computed on the same outcome scale as
  # the one-step ATT (directly comparable) rather than raw-vs-bounded. (The ATT
  # block further below independently recomputes this identical quantity.)
  # Y_bounded_orig is the canonical bounded outcome created once in prepare_final_analysis_data().
  psi_plugin_initial <- sum(w_norm * (Qbar1W_orig - Qbar0W_orig)) / length(weights)
  aipw_resid <- ((A / gn) - ((1 - A) / (1 - gn))) *
                (delta_Y / pi_AW) *
                ifelse(delta_Y == 1L & is.finite(Y_raw), Y_bounded_orig - QbarAW_orig, 0)
  psi_aipw_initial <- sum(w_norm * ((Qbar1W_orig - Qbar0W_orig) + aipw_resid)) / length(weights)
  psi_hat <- sum(w_norm * (Qstar1W_orig - Qstar0W_orig)) / length(weights)
  msg(sprintf("    Weighted ATEs: TMLE=%.4f | initial plug-in=%.4f | initial AIPW=%.4f.",
              psi_hat, psi_plugin_initial, psi_aipw_initial), cfg = cfg)

  # ==========================================================================
  # Positivity remediation -- ATT and overlap-trimmed ATE.
  # The full-sample ATE above requires overlap across the entire covariate
  # space. At 9% prevalence with a rich W, ~50% of rows had g*pi1 < 0.05
  # (treated-arm non-overlap), so E[Y(1)|W] is extrapolated where no treated
  # units exist. The ATT and trimmed ATE are BETTER-IDENTIFIED estimands,
  # reported as SECONDARY columns by default. They become the headline only
  # if cfg$final_tmle$primary_estimand is set to "att" or "trimmed".
  # --------------------------------------------------------------------------
  # ==========================================================================
  # ATT estimation on the configured bounded outcome scale.
  # We report an initial one-step AIPTW comparator and a canonical joint-component
  # CV-TMLE. The CV-TMLE separately targets mu1 = E[Y(1)|A=1] and
  # mu0 = E[Y(0)|A=1]. The ATT and any configured ratio translations are
  # smooth functions of these same targeted component means.
  att_estimate <- att_se <- att_eif_mean <- att_eif_mean_scaled <- NA_real_
  att_nocens_check <- NA_real_; att_components <- NULL
  att_tmle_estimate <- att_tmle_se <- att_tmle_ci_lower <- att_tmle_ci_upper <- NA_real_
  att_tmle_p_value <- att_tmle_G_star <- att_tmle_cf_under_control <- NA_real_
  att_tmle_eif_mean <- att_tmle_eif_mean_scaled <- NA_real_
  att_onestep_estimate <- att_onestep_se <- NA_real_
  att_plugin_estimate <- NA_real_
  eps_att <- eps_att_mu1 <- eps_att_mu0 <- NA_real_
  target_score_mu1 <- target_score_mu0 <- NA_real_
  mu1_att <- mu0_att <- NA_real_
  mu1_att_se <- mu0_att_se <- NA_real_
  pct_depression_effect <- pct_prevention_gain <- NA_real_
  pct_depression_se <- pct_prevention_se <- NA_real_
  pct_depression_ci <- pct_prevention_ci <- c(NA_real_, NA_real_)
  pct_depression_p <- pct_prevention_p <- NA_real_
  D_mu1 <- D_mu0 <- D_pct_depression <- D_pct_prevention <- rep(NA_real_, length(weights))
  natural_course_mean <- natural_course_mean_se <- NA_real_
  treatment_prevalence <- treatment_prevalence_se <- NA_real_
  att_prevalence_elasticity <- att_prevalence_elasticity_se <- NA_real_
  att_prevalence_elasticity_ci <- c(NA_real_, NA_real_)
  att_prevalence_elasticity_p <- NA_real_
  gain_per_prevented_case <- gain_per_prevented_case_se <- NA_real_
  gain_per_one_percentage_point <- gain_per_one_percentage_point_se <- NA_real_
  D_natural_mean <- D_treat_prevalence <- D_att_prevalence_elasticity <- rep(NA_real_, length(weights))
  policy_translation_table <- NULL
  natural_course_target_score <- natural_course_eif_mean_scaled <- NA_real_
  treatment_prevalence_eif_mean <- att_prevalence_elasticity_eif_mean <- NA_real_
  pct_primary_name <- tolower(cfg$final_tmle$percentage_primary %||% "prevention_gain")
  pct_primary_estimate <- pct_primary_se <- pct_primary_p <- NA_real_
  pct_primary_ci <- c(NA_real_, NA_real_)
  att_headline_method <- "CV-TMLE joint component targeting"
  att_headline_tag <- "CVTMLE_JOINT"
  att_wt_ctrl_max <- att_wt_ctrl_p99 <- att_wt_ctrl_ess <- att_ctrl_odds_ess <- NA_real_
  att_wt_ctrl_n <- NA_integer_; att_wt_trt_max <- att_g_ctrl_max <- NA_real_
  att_tmle_nocens <- att_onestep_nocens <- NA_real_

  if (isTRUE(cfg$final_tmle$report_att %||% FALSE)) {
    den_att <- sum(w_norm * A)
    p_treat_w <- den_att / length(weights)
    if (!is.finite(den_att) || den_att <= 0 || !is.finite(p_treat_w) || p_treat_w <= 0)
      stop("ATT undefined: no positive survey-weighted treated mass.", call. = FALSE)

    # Initial-Q plug-in and one-step AIPTW comparator on the canonical bounded
    # original-dollar outcome.
    att_plugin_estimate <- sum(w_norm * A * (Qbar1W_orig - Qbar0W_orig)) / den_att
    resid_initial <- ifelse(delta_Y == 1L, Y_bounded_orig - QbarAW_orig, 0)
    att_resid_onestep <- A * resid_initial / pi_AW -
      (1 - A) * (gn / (1 - gn)) * resid_initial / pi_AW
    att_summand_onestep <- Qbar1W_orig - Qbar0W_orig
    att_onestep_estimate <- sum(w_norm * (A * att_summand_onestep + att_resid_onestep)) / den_att
    att_resid_onestep_nocens <- A * resid_initial -
      (1 - A) * (gn / (1 - gn)) * resid_initial
    att_onestep_nocens <- sum(w_norm * (A * att_summand_onestep + att_resid_onestep_nocens)) / den_att
    D_att_onestep <- (1 / p_treat_w) *
      (A * (att_summand_onestep - att_onestep_estimate) + att_resid_onestep)
    inf_onestep <- infer_primary_eif(D_att_onestep, att_onestep_estimate)
    att_onestep_se <- inf_onestep$se

    # Joint ATT component targeting. On observed rows, the two clever
    # covariates have disjoint supports, so solving each score separately is
    # equivalent to a two-parameter pooled fluctuation and is numerically robust.
    H1_att_obs <- A[idx_up] / pi_AW[idx_up]
    H0_att_obs <- (1 - A[idx_up]) * (gn[idx_up] / (1 - gn[idx_up])) / pi_AW[idx_up]
    t_mu1 <- solve_target_score(
      offset = stats::qlogis(QbarAW[idx_up]), H = H1_att_obs,
      Y = Y_star_obs[idx_up], w = weights[idx_up],
      score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
      max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
      label = "ATT mu1 targeting")
    t_mu0 <- solve_target_score(
      offset = stats::qlogis(QbarAW[idx_up]), H = H0_att_obs,
      Y = Y_star_obs[idx_up], w = weights[idx_up],
      score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
      max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
      label = "ATT mu0 targeting")
    eps_att_mu1 <- t_mu1$epsilon; eps_att_mu0 <- t_mu0$epsilon
    eps_att <- max(abs(c(eps_att_mu1, eps_att_mu0)))
    target_score_mu1 <- t_mu1$normalized_score
    target_score_mu0 <- t_mu0$normalized_score

    H1_att_all <- 1 / pi_1W
    H0_att_all <- (gn / (1 - gn)) / pi_0W
    Qstar1W_att <- stats::plogis(stats::qlogis(Qbar1W) + eps_att_mu1 * H1_att_all)
    Qstar0W_att <- stats::plogis(stats::qlogis(Qbar0W) + eps_att_mu0 * H0_att_all)
    Qstar1W_att_orig <- Qstar1W_att * y_range + y_lower
    Qstar0W_att_orig <- Qstar0W_att * y_range + y_lower
    QstarAW_att_orig <- ifelse(A == 1L, Qstar1W_att_orig, Qstar0W_att_orig)

    mu1_att <- sum(w_norm * A * Qstar1W_att_orig) / den_att
    mu0_att <- sum(w_norm * A * Qstar0W_att_orig) / den_att
    att_tmle_estimate <- mu1_att - mu0_att
    att_summand_tmle <- Qstar1W_att_orig - Qstar0W_att_orig
    resid_obs_tmle <- ifelse(delta_Y == 1L, Y_bounded_orig - QstarAW_att_orig, 0)
    att_resid_tmle <- A * resid_obs_tmle / pi_AW -
      (1 - A) * (gn / (1 - gn)) * resid_obs_tmle / pi_AW

    D_mu1 <- (1 / p_treat_w) *
      (A * (Qstar1W_att_orig - mu1_att) + A * delta_Y / pi_AW *
         ifelse(delta_Y == 1L, Y_bounded_orig - Qstar1W_att_orig, 0))
    D_mu0 <- (1 / p_treat_w) *
      (A * (Qstar0W_att_orig - mu0_att) + (1 - A) * (gn / (1 - gn)) *
         delta_Y / pi_AW * ifelse(delta_Y == 1L, Y_bounded_orig - Qstar0W_att_orig, 0))
    D_att_tmle <- D_mu1 - D_mu0
    # Independent direct ATT EIF construction. This is deliberately computed
    # from the ATT contrast and residual representation rather than defined as
    # D_mu1 - D_mu0, so the unit check can catch a sign, denominator, or
    # censoring-weight error in either component formula.
    D_att_direct <- (1 / p_treat_w) *
      (A * (att_summand_tmle - att_tmle_estimate) + att_resid_tmle)
    D_att_identity_error <- max(abs(D_att_direct - D_att_tmle), na.rm = TRUE)
    if (!is.finite(D_att_identity_error) || D_att_identity_error > 1e-8)
      stop(sprintf("ATT EIF identity direct-D_att = D_mu1 - D_mu0 failed (max error %.3e).",
                   D_att_identity_error), call. = FALSE)

    inf_mu1 <- infer_primary_eif(D_mu1, mu1_att)
    inf_mu0 <- infer_primary_eif(D_mu0, mu0_att)
    inf_att <- infer_primary_eif(D_att_tmle, att_tmle_estimate)
    mu1_att_se <- inf_mu1$se; mu0_att_se <- inf_mu0$se
    att_tmle_se <- inf_att$se
    att_tmle_ci_lower <- inf_att$ci[1]; att_tmle_ci_upper <- inf_att$ci[2]
    att_tmle_p_value <- inf_att$p; att_tmle_G_star <- inf_att$Gstar
    cl_eic_att_tmle <- inf_att$cluster_eic
    J_att <- inf_att$J; fsc_att <- inf_att$fsc

    # Scale-free centering checks. The dollar mean is retained for audit, but
    # the production gate uses the outcome-range-scaled mean.
    att_tmle_eif_mean <- sum(w_norm * D_att_tmle) / length(weights)
    att_tmle_eif_mean_scaled <- att_tmle_eif_mean / y_range
    mu1_eif_mean_scaled <- (sum(w_norm * D_mu1) / length(weights)) / y_range
    mu0_eif_mean_scaled <- (sum(w_norm * D_mu0) / length(weights)) / y_range
    center_tol <- cfg$final_tmle$att_eif_center_tol_scaled %||% 1e-8
    if (any(!is.finite(c(att_tmle_eif_mean_scaled, mu1_eif_mean_scaled, mu0_eif_mean_scaled))) ||
        max(abs(c(att_tmle_eif_mean_scaled, mu1_eif_mean_scaled, mu0_eif_mean_scaled))) > center_tol) {
      stop(sprintf(paste0("Joint ATT targeting failed scale-free EIF centering: ATT=%.3e, ",
                          "mu1=%.3e, mu0=%.3e; tolerance %.3e."),
                   att_tmle_eif_mean_scaled, mu1_eif_mean_scaled,
                   mu0_eif_mean_scaled, center_tol), call. = FALSE)
    }

    if (isTRUE(cfg$policy$enable_policy_components %||% TRUE)) {
      policy_component_error <- NULL
      policy_component_ok <- tryCatch({
        # Target the natural-course population mean under the observed exposure
        # distribution and the same outcome-response model. This is required for a
        # formal ATT-based elasticity with respect to depression prevalence.
        H_nat_obs <- 1 / pi_AW[idx_up]
        t_nat <- solve_target_score(
          offset = stats::qlogis(QbarAW[idx_up]), H = H_nat_obs,
          Y = Y_star_obs[idx_up], w = weights[idx_up],
          score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
          max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
          label = "natural-course mean targeting")
        QstarAW_nat <- stats::plogis(stats::qlogis(QbarAW) + t_nat$epsilon / pi_AW)
        QstarAW_nat_orig <- QstarAW_nat * y_range + y_lower
        natural_course_mean <- sum(w_norm * QstarAW_nat_orig) / length(weights)
        D_natural_mean <- delta_Y / pi_AW *
          ifelse(delta_Y == 1L, Y_bounded_orig - QstarAW_nat_orig, 0) +
          QstarAW_nat_orig - natural_course_mean
        inf_natural_mean <- infer_primary_eif(D_natural_mean, natural_course_mean)
        natural_course_mean_se <- inf_natural_mean$se
        natural_course_target_score <- t_nat$normalized_score
        natural_course_eif_mean_scaled <-
          (sum(w_norm * D_natural_mean) / length(weights)) / y_range
  
        treatment_prevalence <- p_treat_w
        D_treat_prevalence <- A - treatment_prevalence
        inf_treat_prevalence <- infer_primary_eif(D_treat_prevalence, treatment_prevalence)
        treatment_prevalence_se <- inf_treat_prevalence$se
        treatment_prevalence_eif_mean <-
          sum(w_norm * D_treat_prevalence) / length(weights)
        if (any(!is.finite(c(natural_course_target_score,
                             natural_course_eif_mean_scaled,
                             treatment_prevalence_eif_mean))) ||
            max(abs(c(natural_course_target_score,
                      natural_course_eif_mean_scaled,
                      treatment_prevalence_eif_mean))) > center_tol) {
          msg_txt <- sprintf(
            paste0("Policy-component targeting/centering failed: natural score=%.3e, ",
                   "natural EIF/range=%.3e, prevalence EIF=%.3e; tolerance %.3e."),
            natural_course_target_score, natural_course_eif_mean_scaled,
            treatment_prevalence_eif_mean, center_tol)
          stop(msg_txt, call. = FALSE)
        }
  
        if (isTRUE(cfg$policy$enable_att_prevalence_translation %||% TRUE) &&
            isTRUE(is_raw_dollar_outcome)) {
          translation_ready <- is.finite(natural_course_mean) && natural_course_mean > 0
          if (!translation_ready) {
            msg_txt <- "ATT prevalence elasticity requires a positive natural-course mean."
            stop(msg_txt, call. = FALSE)
          }
          if (translation_ready) {
          att_prevalence_elasticity <-
            treatment_prevalence * att_tmle_estimate / natural_course_mean
          D_att_prevalence_elasticity <-
            (treatment_prevalence / natural_course_mean) * D_att_tmle +
            (att_tmle_estimate / natural_course_mean) * D_treat_prevalence -
            (treatment_prevalence * att_tmle_estimate / natural_course_mean^2) * D_natural_mean
          inf_elasticity <- infer_primary_eif(
            D_att_prevalence_elasticity, att_prevalence_elasticity)
          att_prevalence_elasticity_se <- inf_elasticity$se
          att_prevalence_elasticity_ci <- inf_elasticity$ci
          att_prevalence_elasticity_p <- inf_elasticity$p
          att_prevalence_elasticity_eif_mean <-
            sum(w_norm * D_att_prevalence_elasticity) / length(weights)
          if (!is.finite(att_prevalence_elasticity_eif_mean) ||
              abs(att_prevalence_elasticity_eif_mean) > center_tol)
            {
              msg_txt <- sprintf(
                "ATT-based prevalence elasticity EIF failed centering (%.3e > %.3e).",
                att_prevalence_elasticity_eif_mean, center_tol)
              stop(msg_txt, call. = FALSE)
            }
  
          gain_per_prevented_case <- -att_tmle_estimate
          gain_per_prevented_case_se <- att_tmle_se
          D_gain_one_pp <- -0.01 * D_att_tmle
          gain_per_one_percentage_point <- -0.01 * att_tmle_estimate
          inf_gain_one_pp <- infer_primary_eif(
            D_gain_one_pp, gain_per_one_percentage_point)
          gain_per_one_percentage_point_se <- inf_gain_one_pp$se
  
          reductions <- sort(unique(as.numeric(
            cfg$policy$relative_prevalence_reductions %||% c(0.10, 0.25, 0.50))))
          policy_translation_table <- do.call(rbind, lapply(reductions, function(r) {
            gain <- -r * treatment_prevalence * att_tmle_estimate
            D_gain <- -r * (treatment_prevalence * D_att_tmle +
                            att_tmle_estimate * D_treat_prevalence)
            inf_gain <- infer_primary_eif(D_gain, gain)
            rel_gain <- gain / natural_course_mean
            D_rel_gain <- D_gain / natural_course_mean -
              gain * D_natural_mean / natural_course_mean^2
            inf_rel_gain <- infer_primary_eif(D_rel_gain, rel_gain)
            data.frame(
              relative_prevalence_reduction = r,
              baseline_depression_prevalence = treatment_prevalence,
              post_policy_depression_prevalence = treatment_prevalence * (1 - r),
              implied_population_mean_earnings_gain = gain,
              gain_se = inf_gain$se,
              gain_ci_lower = inf_gain$ci[1L],
              gain_ci_upper = inf_gain$ci[2L],
              relative_population_earnings_gain = rel_gain,
              relative_gain_se = inf_rel_gain$se,
              relative_gain_ci_lower = inf_rel_gain$ci[1L],
              relative_gain_ci_upper = inf_rel_gain$ci[2L],
              outcome_scale = cfg$policy$outcome_scale_label %||%
                "configured bounded/capped outcome",
              outcome_transform = compensation_transform,
              cap_quantile = cfg$outcome$continuous_upper_quantile,
              assumption = paste0(
                "Prevented cases have the current treated-population ATT; ",
                "the translation applies to the configured bounded/capped outcome."),
              stringsAsFactors = FALSE)
          }))
          if (isTRUE(cfg$global$save_stage_csvs))
            write_run_csv(policy_translation_table, cfg,
                          cfg$policy$output_csv %||% "att_policy_translation.csv")
          }
        }
      TRUE
      }, error = function(e) {
        policy_component_error <<- conditionMessage(e)
        FALSE
      })
      if (!isTRUE(policy_component_ok)) {
        natural_course_mean <- natural_course_mean_se <- NA_real_
        treatment_prevalence <- treatment_prevalence_se <- NA_real_
        att_prevalence_elasticity <- att_prevalence_elasticity_se <- NA_real_
        att_prevalence_elasticity_ci <- c(NA_real_, NA_real_)
        att_prevalence_elasticity_p <- NA_real_
        gain_per_prevented_case <- gain_per_prevented_case_se <- NA_real_
        gain_per_one_percentage_point <- gain_per_one_percentage_point_se <- NA_real_
        D_natural_mean <- D_treat_prevalence <- D_att_prevalence_elasticity <-
          rep(NA_real_, length(weights))
        policy_translation_table <- NULL
        natural_course_target_score <- natural_course_eif_mean_scaled <- NA_real_
        treatment_prevalence_eif_mean <- att_prevalence_elasticity_eif_mean <- NA_real_
        msg_txt <- paste0(
          "Optional policy-component analysis failed and produced no policy outputs: ",
          policy_component_error)
        if (isTRUE(cfg$policy$fail_on_policy_component_checks %||% TRUE))
          stop(msg_txt, call. = FALSE) else warning(msg_txt, call. = FALSE)
      }
    }

    # Optional ratio translations with their own delta-method EIFs and the same
    # REGION-stratified PSU design inference used for the ATT. These are enabled
    # only for outcome families whose verified scale supports ratios, currently
    # raw annual Compensation. Other outcomes retain the ATT on their own scale.
    if (isTRUE(report_ratio_translations)) {
      if (!is.finite(mu1_att) || !is.finite(mu0_att) || mu1_att <= 0 || mu0_att <= 0)
        stop("Ratio translation is undefined because a targeted ATT component mean is non-positive.", call. = FALSE)
      pct_depression_effect <- 100 * (mu1_att - mu0_att) / mu0_att
      pct_prevention_gain <- 100 * (mu0_att - mu1_att) / mu1_att
      D_pct_depression <- 100 * (D_mu1 / mu0_att - mu1_att * D_mu0 / mu0_att^2)
      D_pct_prevention <- 100 * (D_mu0 / mu1_att - mu0_att * D_mu1 / mu1_att^2)
      inf_pct_dep <- infer_primary_eif(D_pct_depression, pct_depression_effect)
      inf_pct_prev <- infer_primary_eif(D_pct_prevention, pct_prevention_gain)
      pct_depression_se <- inf_pct_dep$se; pct_depression_ci <- inf_pct_dep$ci; pct_depression_p <- inf_pct_dep$p
      pct_prevention_se <- inf_pct_prev$se; pct_prevention_ci <- inf_pct_prev$ci; pct_prevention_p <- inf_pct_prev$p
      if (identical(pct_primary_name, "depression_effect")) {
        pct_primary_estimate <- pct_depression_effect; pct_primary_se <- pct_depression_se
        pct_primary_ci <- pct_depression_ci; pct_primary_p <- pct_depression_p
      } else {
        pct_primary_name <- "prevention_gain"
        pct_primary_estimate <- pct_prevention_gain; pct_primary_se <- pct_prevention_se
        pct_primary_ci <- pct_prevention_ci; pct_primary_p <- pct_prevention_p
      }
    } else {
      pct_primary_name <- NA_character_
    }

    # Complete-data/IPCW->1 diagnostic, using the same targeted Q and canonical
    # bounded outcome. This is a wiring check, not an alternative estimand.
    resid_full_tmle <- ifelse(is.finite(Y_bounded_orig), Y_bounded_orig - QstarAW_att_orig, 0)
    att_resid_nc_tmle <- A * resid_full_tmle -
      (1 - A) * (gn / (1 - gn)) * resid_full_tmle
    att_tmle_nocens <- sum(w_norm * (A * att_summand_tmle + att_resid_nc_tmle)) / den_att
    att_tmle_cf_under_control <- mu0_att

    # Canonical headline routing is fixed to joint-component CV-TMLE. The
    # one-step remains an explicitly labeled diagnostic comparator only.
    att_estimator_choice <- tolower(cfg$final_tmle$att_estimator %||% "tmle")
    if (!identical(att_estimator_choice, "tmle"))
      stop("Internal configuration error: ATT headline estimator must be 'tmle'.", call. = FALSE)
    att_estimate <- att_tmle_estimate; att_se <- att_tmle_se
    D_att <- D_att_tmle; cl_eic_att <- cl_eic_att_tmle
    att_eif_mean <- att_tmle_eif_mean; att_eif_mean_scaled <- att_tmle_eif_mean_scaled
    att_summand <- att_summand_tmle; att_resid <- att_resid_tmle; resid_obs <- resid_obs_tmle
    att_nocens_check <- att_tmle_nocens
    cf_treated_under_control <- mu0_att
    att_support_ok <- mu0_att >= y_lower - 1e-8 && mu0_att <= y_upper + 1e-8
    att_tmle_se_centered <- infer_primary_eif(
      D_att_tmle - att_tmle_eif_mean, att_tmle_estimate)$se
    att_se_centered <- att_tmle_se_centered

    ctrl_rows <- which(A == 0L); trt_rows <- which(A == 1L)
    att_ctrl_odds <- gn / (1 - gn)
    att_ctrl_wt <- weights * att_ctrl_odds / pi_0W
    att_trt_wt <- weights / pi_1W
    att_wt_ctrl_max <- if (length(ctrl_rows)) max(att_ctrl_wt[ctrl_rows]) else NA_real_
    att_wt_ctrl_p99 <- if (length(ctrl_rows)) as.numeric(stats::quantile(att_ctrl_wt[ctrl_rows], 0.99, names = FALSE, type = 8)) else NA_real_
    att_wt_ctrl_ess <- if (length(ctrl_rows)) kish_ess_safe(att_ctrl_wt[ctrl_rows]) else NA_real_
    att_wt_ctrl_n <- length(ctrl_rows)
    att_wt_trt_max <- if (length(trt_rows)) max(att_trt_wt[trt_rows]) else NA_real_
    att_g_ctrl_max <- if (length(ctrl_rows)) max(gn[ctrl_rows]) else NA_real_
    att_ctrl_odds_ess <- if (length(ctrl_rows)) kish_ess_safe(weights[ctrl_rows] * att_ctrl_odds[ctrl_rows]) else NA_real_

    if (isTRUE(is_raw_dollar_outcome)) {
      msg(sprintf("    ATT joint CV-TMLE: mu1=$%.2f, mu0=$%.2f, dollar ATT=$%.2f (SE $%.2f).",
                  mu1_att, mu0_att, att_tmle_estimate, att_tmle_se), cfg = cfg)
    } else {
      msg(sprintf("    ATT joint CV-TMLE: mu1=%.4f, mu0=%.4f, ATT=%.4f (SE %.4f) on the configured outcome scale.",
                  mu1_att, mu0_att, att_tmle_estimate, att_tmle_se), cfg = cfg)
    }
    if (isTRUE(report_ratio_translations)) {
      msg(sprintf("      Depression effect = %.2f%% [%.2f, %.2f]; prevention gain = %.2f%% [%.2f, %.2f].",
                  pct_depression_effect, pct_depression_ci[1], pct_depression_ci[2],
                  pct_prevention_gain, pct_prevention_ci[1], pct_prevention_ci[2]), cfg = cfg)
    }
    msg(sprintf("      Target scores: mu1 %.3e, mu0 %.3e; scaled EIF mean %.3e (tol %.1e).",
                target_score_mu1, target_score_mu0, att_eif_mean_scaled, center_tol), cfg = cfg)

    att_components <- list(
      psi_att = att_estimate, att_se = att_se, att_se_centered = att_se_centered,
      p_treat_w = p_treat_w, D_att = D_att, D_mu1 = D_mu1, D_mu0 = D_mu0,
      D_pct_depression = D_pct_depression, D_pct_prevention = D_pct_prevention,
      D_natural_mean = D_natural_mean, D_treat_prevalence = D_treat_prevalence,
      D_att_prevalence_elasticity = D_att_prevalence_elasticity,
      natural_course_mean = natural_course_mean,
      natural_course_target_score = natural_course_target_score,
      natural_course_eif_mean_scaled = natural_course_eif_mean_scaled,
      treatment_prevalence = treatment_prevalence,
      treatment_prevalence_eif_mean = treatment_prevalence_eif_mean,
      att_prevalence_elasticity = att_prevalence_elasticity,
      att_prevalence_elasticity_eif_mean = att_prevalence_elasticity_eif_mean,
      policy_translation_table = policy_translation_table,
      mu1_att = mu1_att, mu0_att = mu0_att,
      pct_depression_effect = pct_depression_effect,
      pct_prevention_gain = pct_prevention_gain,
      att_summand = att_summand, att_resid = att_resid, resid_obs = resid_obs,
      cl_eic_att = cl_eic_att, J_att = J_att, fsc_att = fsc_att,
      inference_method = inf_att$method, inference_df = inf_att$df,
      inference_design_n = inf_att$design_n, inference_domain_n = inf_att$domain_n,
      inference_design_psu_n = inf_att$design_psu_n,
      inference_design_strata_n = inf_att$design_strata_n,
      inference_ci = inf_att$ci, inference_p = inf_att$p,
      se_psu_only_sensitivity = inf_att$se_cluster_only,
      ci_psu_only_sensitivity = inf_att$ci_cluster_only,
      p_psu_only_sensitivity = inf_att$p_cluster_only,
      Gstar_diagnostic = inf_att$Gstar,
      eif_mean = att_eif_mean, eif_mean_scaled = att_eif_mean_scaled,
      center_tol_scaled = center_tol, nocens = att_nocens_check,
      target_score_mu1 = target_score_mu1, target_score_mu0 = target_score_mu0,
      eps_mu1 = eps_att_mu1, eps_mu0 = eps_att_mu0,
      att_tmle_estimate = att_tmle_estimate, att_tmle_se = att_tmle_se,
      cl_eic_att_tmle = cl_eic_att_tmle, att_tmle_eif_mean = att_tmle_eif_mean,
      att_tmle_cf_under_control = att_tmle_cf_under_control,
      att_onestep_estimate = att_onestep_estimate, att_onestep_se = att_onestep_se,
      eps_att = eps_att, att_headline_method = att_headline_method,
      cf_treated_under_control = cf_treated_under_control,
      support_ok = att_support_ok, y_lower = y_lower, y_upper = y_upper,
      A = A, Y_raw = Y_raw, Y_bounded_orig = Y_bounded_orig,
      delta_Y = delta_Y, cluster = cluster, strata = strata, weights = weights, w_norm = w_norm,
      outer_fold = outer_fold, gn = gn, pi_AW = pi_AW, pi_1W = pi_1W, pi_0W = pi_0W,
      gn_raw = gn_raw, pi_AW_raw = pi_AW_raw, pi_1W_raw = pi_1W_raw, pi_0W_raw = pi_0W_raw,
      Qbar1W_orig = Qstar1W_att_orig, Qbar0W_orig = Qstar0W_att_orig,
      QbarAW_orig = QstarAW_att_orig)
  }

  # Overlap-trimmed ATE: restrict to the common-support propensity band.
  # when retarget_trimmed = TRUE this is a FORMALLY TARGETED estimate
  # of the trimmed estimand -- the fluctuation epsilon is re-fit on the
  # in-support observed rows and the counterfactual means are re-updated with
  # that epsilon, rather than reusing the full-sample epsilon. The EIF is then
  # built from the re-targeted predictions on the trimmed sample.
  trim_estimate <- NA_real_; trim_se <- NA_real_; n_trim <- NA_integer_
  trim_ci <- c(NA_real_, NA_real_); trim_statistic <- NA_real_; trim_p <- NA_real_
  inf_trim <- NULL; trim_eps <- NA_real_
  if (isTRUE(cfg$final_tmle$trim_enable %||% FALSE)) {
    tlo <- cfg$final_tmle$trim_g_lower %||% 0.05
    thi <- cfg$final_tmle$trim_g_upper %||% 0.95
    keep_t <- gn >= tlo & gn <= thi
    n_trim <- sum(keep_t)
    if (n_trim >= 1L && any(keep_t)) {
      w_t <- weights[keep_t] / mean(weights[keep_t])
      # Default: reuse the full-sample targeted predictions on the bounded
      # scale (rough check). If retarget_trimmed, re-fit the fluctuation.
      Q1_t <- Qstar1W[keep_t]; Q0_t <- Qstar0W[keep_t]
      if (isTRUE(cfg$final_tmle$retarget_trimmed %||% FALSE)) {
        # Re-target: fluctuate the INITIAL Q (Qbar) on the trimmed observed
        # rows, using the same logit-offset clever-covariate regression as the
        # main step but restricted to the in-support sample.
        idx_t   <- which(keep_t & delta_Y == 1L)
        if (length(idx_t) >= 2L && length(unique(A[idx_t])) == 2L) {
          H_obs_t <- (fluc_H1 - fluc_H0)[idx_t]
          fit_t <- solve_target_score(
            offset = stats::qlogis(QbarAW[idx_t]), H = H_obs_t,
            Y = Y_star_obs[idx_t], w = weights[idx_t],
            score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
            max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
            label = "trimmed ATE targeting")
          eps_t <- fit_t$epsilon
          trim_eps <- eps_t
          Q1_t <- stats::plogis(stats::qlogis(Qbar1W[keep_t]) + eps_t * H1_all[keep_t])
          Q0_t <- stats::plogis(stats::qlogis(Qbar0W[keep_t]) + eps_t * H0_all[keep_t])
          msg(sprintf("    Trimmed ATE re-targeting epsilon = %.6f (on %d in-support observed rows).",
                      eps_t, length(idx_t)), cfg = cfg)
        } else {
          stop(paste0(
            "Trimmed ATE re-targeting requires at least two in-support observed rows and both treatment arms. ",
            "The pipeline will not reuse the full-sample fluctuation for a different trimmed estimand."),
            call. = FALSE)
        }
      }
      Q1_t_orig <- Q1_t * y_range + y_lower
      Q0_t_orig <- Q0_t * y_range + y_lower
      psi_trim <- sum(w_t * (Q1_t_orig - Q0_t_orig)) / n_trim
      Ys_resid_t <- Y_star; Ys_resid_t[delta_Y == 0L] <- 0
      D_star_t <- ((A[keep_t] / gn[keep_t]) - ((1 - A[keep_t]) / (1 - gn[keep_t]))) *
                  (delta_Y[keep_t] / pi_AW[keep_t]) *
                  (Ys_resid_t[keep_t] - ifelse(A[keep_t] == 1, Q1_t, Q0_t)) +
                  (Q1_t - Q0_t) -
                  sum(w_t * (Q1_t - Q0_t)) / n_trim
      D_t <- D_star_t * y_range
      trim_estimate <- psi_trim
      trim_design_frame <- full_survey_design_frame
      trim_design_ids <- as.character(trim_design_frame[[cfg$analysis$id_var]])
      trim_domain_ids <- respondent_ids[keep_t]
      trim_design_frame$.analysis_domain <- trim_design_ids %in% trim_domain_ids
      inf_trim <- cluster_inference_from_eif(
        D_t, weights[keep_t], cluster[keep_t], trim_estimate,
        strata = if (is.null(strata)) NULL else strata[keep_t],
        design_frame = trim_design_frame, domain_ids = trim_domain_ids,
        id_var = cfg$analysis$id_var, cluster_var = cfg$analysis$cluster_var,
        strata_var = cfg$analysis$strata_var, weight_var = cfg$analysis$weight_var)
      trim_se <- inf_trim$se
      trim_ci <- inf_trim$ci
      trim_statistic <- inf_trim$statistic
      trim_p <- inf_trim$p
      J_t <- inf_trim$J
      msg(sprintf("    Trimmed ATE (g in [%.2f, %.2f], n=%d of %d, %d clusters, %s): %.4f (design-aware SE %.4f).",
                  tlo, thi, n_trim, length(weights), J_t,
                  if (isTRUE(cfg$final_tmle$retarget_trimmed)) "re-targeted" else "reused-epsilon",
                  trim_estimate, trim_se), cfg = cfg)
    } else {
      msg("    Trimmed ATE: no rows survived the common-support band; skipping.", cfg = cfg)
    }
  }

  # ==========================================================================
  # Efficient influence function on the BOUNDED [0,1] scale, then back-
  # transformed by y_range.
  # ==========================================================================
  msg("  [TMLE] Computing EIF on bounded scale, then back-transforming...", cfg = cfg)
  Y_star_resid <- Y_star
  Y_star_resid[delta_Y == 0L] <- 0   # masked out by delta in the clever cov
  D_star <- ((A / gn) - ((1 - A) / (1 - gn))) *
            (delta_Y / pi_AW) * (Y_star_resid - ifelse(A == 1, Qstar1W, Qstar0W)) +
            (Qstar1W - Qstar0W) -
            sum(w_norm * (Qstar1W - Qstar0W)) / length(weights)
  # Convert to original scale
  D <- D_star * y_range
  D_orig <- D
  D_w <- w_norm * D

  inf_ate <- infer_primary_eif(D, psi_hat)
  cluster_eic <- inf_ate$cluster_eic
  J <- inf_ate$J; fsc <- inf_ate$fsc
  se_hat <- inf_ate$se; ci <- inf_ate$ci
  z <- inf_ate$statistic; p_val <- inf_ate$p
  msg(sprintf("    Design-aware SE (%s; df=%.1f) = %.4f; PSU-only sensitivity SE = %.4f.",
              inf_ate$method, inf_ate$df, se_hat, inf_ate$se_cluster_only), cfg = cfg)

  # select which estimand is the HEADLINE. The full-sample ATE values
  # (psi_hat, se_hat, ci, z, p_val) are always retained as ate_tmle/se_full
  # etc. in the result row; this block only chooses what estimate/se/ci/p
  # point to. Default "ate" => no change. "trimmed"/"att" change the ESTIMAND.
  primary <- cfg$final_tmle$primary_estimand %||% "ate"
  ate_full_estimate <- psi_hat; ate_full_se <- se_hat
  ate_full_ci <- ci; ate_full_z <- z; ate_full_p <- p_val
  base_label <- if (outcome_type == "continuous")
    sprintf("bounded_%s_q%.3f", cfg$analysis$outcome_var, cfg$outcome$continuous_upper_quantile)
  else "binary"
  if (identical(primary, "trimmed")) {
    if (!is.finite(trim_estimate) || !is.finite(trim_se) || trim_se <= 0)
      stop("primary_estimand='trimmed' but the trimmed ATE is not available (trim_enable must be TRUE and yield a finite estimate).", call. = FALSE)
    head_est <- trim_estimate; head_se <- trim_se
    head_ci <- trim_ci; head_z <- trim_statistic; head_p <- trim_p
    estimand_label <- sprintf("TRIMMED_ATE_%s_g[%.2f,%.2f]", base_label,
                              cfg$final_tmle$trim_g_lower %||% 0.05,
                              cfg$final_tmle$trim_g_upper %||% 0.95)
    msg(sprintf("  [TMLE] PRIMARY ESTIMAND = overlap-trimmed ATE (%.4f, SE %.4f). Full-sample ATE retained as secondary (%.4f).",
                head_est, head_se, ate_full_estimate), cfg = cfg)
  } else if (identical(primary, "att")) {
    if (!is.finite(att_estimate) || !is.finite(att_se) || att_se <= 0)
      stop("primary_estimand='att' but the ATT is not available (report_att must be TRUE and yield a finite estimate).", call. = FALSE)
    # The ATT SE is the REGION-stratified, PSUSCID-clustered survey-design
    # EIF SE. Refuse to headline it
    # if the EIF centering self-check failed, since that indicates the
    # influence function was miscomputed and the SE would be invalid.
    center_tol <- cfg$final_tmle$att_eif_center_tol_scaled %||% 1e-8
    if (!is.finite(att_eif_mean_scaled) || abs(att_eif_mean_scaled) > center_tol) {
      stop(sprintf("primary_estimand='att' but the scale-free ATT EIF centering check failed (%.3e > %.3e).",
                   att_eif_mean_scaled, center_tol), call. = FALSE)
    }
    head_est <- att_estimate; head_se <- att_se
    head_z <- inf_att$statistic
    head_ci <- inf_att$ci
    head_p <- inf_att$p
    att_Gstar_h <- inf_att$Gstar
    att_dfeff_h <- inf_att$df
    estimand_label <- sprintf("ATT_%s_%s", att_headline_tag, base_label)
    if (isTRUE(is_raw_dollar_outcome)) {
      msg(sprintf("  [TMLE] PRIMARY DOLLAR ESTIMAND = ATT via %s ($%.2f, design-aware EIF SE $%.2f, t reference df=%.1f). Policy-facing primary percentage (%s) = %.2f%% [%.2f, %.2f].",
                  att_headline_method, head_est, head_se, att_dfeff_h,
                  pct_primary_name, pct_primary_estimate, pct_primary_ci[1], pct_primary_ci[2]), cfg = cfg)
    } else {
      msg(sprintf("  [TMLE] PRIMARY ESTIMAND = ATT via %s (%.4f, design-aware EIF SE %.4f, t reference df=%.1f) on the configured outcome scale.",
                  att_headline_method, head_est, head_se, att_dfeff_h), cfg = cfg)
    }
  } else {
    if (!identical(primary, "ate"))
      warning(sprintf("Unknown primary_estimand '%s'; defaulting to full-sample ATE.", primary))
    head_est <- ate_full_estimate; head_se <- ate_full_se
    head_ci  <- ate_full_ci; head_z <- ate_full_z; head_p <- ate_full_p
    estimand_label <- if (outcome_type == "continuous")
      sprintf("ATE_%s", base_label) else "ATE_binary"
  }

  # Nested rough-selection audit log
  sel_log_df <- if (length(nested_selection_log) > 0L) do.call(rbind, nested_selection_log) else NULL
  if (!is.null(sel_log_df) && isTRUE(cfg$global$save_stage_csvs)) {
    p_sel <- write_run_csv(sel_log_df, cfg, cfg$final_tmle$nested_rough_selection_log_csv)
    msg(sprintf("  [TMLE] Nested rough-selection log written: %s", basename(p_sel)), cfg = cfg)
  }

  # cluster-assignment diagnostic. Lets a reviewer verify the pre-score
  # correlation clustering did NOT over-collapse meaningful confounders (e.g. the
  # baseline mental-health H1FS block): each row is one member variable with its
  # cluster id, representative, cluster size, correlation to the representative,
  # and whether the cluster/representative was ultimately selected. Multi-member
  # clusters containing distinct substantive variables would be the warning sign.
  clu_log_df <- if (length(cluster_assignment_log) > 0L) do.call(rbind, cluster_assignment_log) else NULL
  if (!is.null(clu_log_df) && isTRUE(cfg$global$save_stage_csvs)) {
    p_clu <- write_run_csv(clu_log_df, cfg,
                           cfg$final_tmle$cluster_assignment_log_csv %||% "cluster_assignments.csv")
    msg(sprintf("  [TMLE] Cluster-assignment diagnostic written: %s (%d member rows, %d multi-member clusters).",
                basename(p_clu), nrow(clu_log_df),
                length(unique(clu_log_df$cluster_id[clu_log_df$cluster_size > 1L]))), cfg = cfg)
  }

  # Per-fold SL log
  sl_log_df <- do.call(rbind, per_fold_log)
  if (isTRUE(cfg$global$save_stage_csvs)) {
    p <- write_run_csv(sl_log_df, cfg, cfg$final_tmle$per_fold_sl_log_csv)
    msg(sprintf("  [TMLE] Per-fold SL log written: %s", basename(p)), cfg = cfg)
    if (length(fold_support_log) > 0L) {
      fs <- do.call(rbind, fold_support_log)
      p_fs <- write_run_csv(fs, cfg, cfg$final_tmle$fold_support_log_csv %||% "fold_support_log.csv")
      msg(sprintf("  [TMLE] Fold support log written: %s", basename(p_fs)), cfg = cfg)
    }
    if (length(internal_fold_support_log) > 0L) {
      ifs <- do.call(rbind, internal_fold_support_log)
      p_ifs <- write_run_csv(ifs, cfg, cfg$final_tmle$internal_fold_support_log_csv %||% "internal_fold_support_log.csv")
      msg(sprintf("  [TMLE] Internal fold support log written: %s", basename(p_ifs)), cfg = cfg)
    }
  }

  # Overlap diagnostics
  overlap_df <- data.frame(
    gn = gn, pi_AW = pi_AW,
    g_times_pi1 = gn * pi_1W,
    one_minus_g_times_pi0 = (1 - gn) * pi_0W,
    A = A, delta_Y = delta_Y)
  if (isTRUE(cfg$global$save_stage_csvs)) {
    p <- write_run_csv(overlap_df, cfg, cfg$final_tmle$overlap_diagnostics_csv)
    msg(sprintf("  [TMLE] Overlap diagnostics written: %s", basename(p)), cfg = cfg)
  }

  # Save the influence curve for the declared primary estimand. ATE influence
  # objects remain available under explicitly named secondary fields.
  primary_D <- if (identical(primary, "att")) D_att else if (identical(primary, "trimmed")) D_t else D
  primary_cluster_eic <- if (identical(primary, "att")) cl_eic_att else if (identical(primary, "trimmed")) inf_trim$cluster_eic else cluster_eic
  primary_inf <- if (identical(primary, "att")) inf_att else if (identical(primary, "trimmed")) inf_trim else inf_ate
  eic_path <- build_unique_path(cfg, cfg$final_tmle$cluster_eic_rds)
  atomic_save_rds(primary_cluster_eic, eic_path, overwrite = TRUE)
  msg(sprintf("  [TMLE] Cluster-level EIC saved: %s", basename(eic_path)), cfg = cfg)

  # Aggregate fold-specific design dimensions for transparent reporting.
  manifest_df_for_result <- if (length(run_manifest_rows)) do.call(rbind, run_manifest_rows) else NULL
  design_min <- function(nm) if (!is.null(manifest_df_for_result) && nm %in% names(manifest_df_for_result))
    min(manifest_df_for_result[[nm]], na.rm = TRUE) else NA_real_
  design_max <- function(nm) if (!is.null(manifest_df_for_result) && nm %in% names(manifest_df_for_result))
    max(manifest_df_for_result[[nm]], na.rm = TRUE) else NA_real_

  # Main results row
  res_df <- data.frame(
    run_id = cfg$global$run_id %||% NA_character_,
    pipeline_version = cfg$global$version %||% NA_character_,
    script_md5 = get_frozen_source_fingerprint(cfg)$md5,
    analysis_spec_md5 = get_frozen_config_hash(cfg, "analysis"),
    resolved_run_config_md5 = get_frozen_config_hash(cfg, "resolved"),
    R_version = cfg$provenance$runtime$R_version,
    runtime_platform = cfg$provenance$runtime$platform,
    package_versions = cfg$provenance$runtime$packages,
    q_clip_overall_fraction = q_clip_overall_fraction,
    q_clip_max_fold_fraction = q_clip_max_fold_fraction,
    q_clip_review_required = q_clip_review_required,
    estimand = estimand_label,
    estimate = head_est, se = head_se,
    ci_lower = head_ci[1], ci_upper = head_ci[2],
    test_statistic = head_z,
    reference_distribution = "t",
    z_legacy = head_z,
    p_value = head_p,
    primary_estimand = primary,
    inference_method = primary_inf$method,
    inference_df = primary_inf$df,
    inference_design_n = primary_inf$design_n,
    inference_domain_n = primary_inf$domain_n,
    inference_design_psu_n = primary_inf$design_psu_n,
    inference_design_strata_n = primary_inf$design_strata_n,
    primary_se_psu_only_sensitivity = primary_inf$se_cluster_only,
    primary_ci_lower_psu_only_sensitivity = primary_inf$ci_cluster_only[1],
    primary_ci_upper_psu_only_sensitivity = primary_inf$ci_cluster_only[2],
    cap_probability = if (identical(outcome_type, "continuous"))
      cfg$outcome$continuous_upper_quantile else NA_real_,
    cap_value = data_pack$cap_value,
    cap_weighted = data_pack$cap_weighted,
    cap_weight_variable = if (identical(outcome_type, "continuous") && data_pack$cap_weighted)
      cfg$analysis$weight_var else NA_character_,
    cap_censoring_adjusted = if (identical(outcome_type, "continuous")) FALSE else NA,
    cap_quantile_rule = data_pack$cap_qrule,
    cap_population = if (identical(outcome_type, "continuous"))
      "pooled outcome-observed analytic sample" else NA_character_,
    cap_inference_interpretation = if (identical(outcome_type, "continuous"))
      "conditional_on_realized_empirical_cap" else NA_character_,
    compensation_transform = if (identical(cfg$outcome$family, "Compensation"))
      compensation_transform else NA_character_,
    compensation_exact_only = if (identical(cfg$outcome$family, "Compensation"))
      isTRUE(cfg$outcome$compensation_exact_only) else NA,
    ate_tmle = ate_full_estimate,
    ate_se_full = ate_full_se,
    ate_ci_lower_full = ate_full_ci[1], ate_ci_upper_full = ate_full_ci[2],
    ate_p_full = ate_full_p,
    ate_plugin_initial = psi_plugin_initial,
    ate_aipw_initial = psi_aipw_initial,
    tmle_minus_plugin_initial = ate_full_estimate - psi_plugin_initial,
    tmle_minus_aipw_initial = ate_full_estimate - psi_aipw_initial,
    att_estimate = att_estimate, att_se = att_se,
    att_headline_method = att_headline_method,
    att_mu1 = mu1_att,
    att_mu0 = mu0_att,
    # Legacy Compensation-specific aliases retained only for Compensation.
    att_mu1_earnings_depressed = if (identical(cfg$outcome$family, "Compensation")) mu1_att else NA_real_,
    att_mu0_earnings_no_depression = if (identical(cfg$outcome$family, "Compensation")) mu0_att else NA_real_,
    att_mu1_se = mu1_att_se, att_mu0_se = mu0_att_se,
    pct_depression_effect = pct_depression_effect,
    pct_depression_effect_se = pct_depression_se,
    pct_depression_effect_ci_lower = pct_depression_ci[1],
    pct_depression_effect_ci_upper = pct_depression_ci[2],
    pct_depression_effect_p = pct_depression_p,
    pct_prevention_gain = pct_prevention_gain,
    pct_prevention_gain_se = pct_prevention_se,
    pct_prevention_gain_ci_lower = pct_prevention_ci[1],
    pct_prevention_gain_ci_upper = pct_prevention_ci[2],
    pct_prevention_gain_p = pct_prevention_p,
    policy_primary_percentage = pct_primary_name,
    policy_primary_pct_estimate = pct_primary_estimate,
    policy_primary_pct_se = pct_primary_se,
    policy_primary_pct_ci_lower = pct_primary_ci[1],
    policy_primary_pct_ci_upper = pct_primary_ci[2],
    policy_primary_pct_p = pct_primary_p,
    policy_components_enabled = isTRUE(cfg$policy$enable_policy_components %||% TRUE),
    policy_translation_enabled = isTRUE(cfg$policy$enable_att_prevalence_translation %||% TRUE),
    mortality_sensitivity_enabled = isTRUE(cfg$mortality_sensitivity$enabled %||% FALSE),
    mortality_composite_zero_at_death =
      isTRUE(cfg$mortality_sensitivity$composite_zero_at_death %||% FALSE),
    mortality_source_variable = cfg$mortality_sensitivity$source_var %||% NA_character_,
    mortality_death_year_start = cfg$mortality_sensitivity$death_year_start %||% NA_integer_,
    mortality_death_year_end = cfg$mortality_sensitivity$death_year_end %||% NA_integer_,
    bounded_natural_course_population_mean = natural_course_mean,
    bounded_natural_course_population_mean_se = natural_course_mean_se,
    natural_course_population_mean = natural_course_mean,
    natural_course_population_mean_se = natural_course_mean_se,
    natural_course_target_score = natural_course_target_score,
    natural_course_eif_mean_scaled = natural_course_eif_mean_scaled,
    depression_prevalence = treatment_prevalence,
    depression_prevalence_se = treatment_prevalence_se,
    treatment_prevalence_eif_mean = treatment_prevalence_eif_mean,
    att_based_bounded_outcome_prevalence_elasticity = att_prevalence_elasticity,
    att_prevalence_elasticity = att_prevalence_elasticity,
    att_prevalence_elasticity_se = att_prevalence_elasticity_se,
    att_prevalence_elasticity_ci_lower = att_prevalence_elasticity_ci[1],
    att_prevalence_elasticity_ci_upper = att_prevalence_elasticity_ci[2],
    att_prevalence_elasticity_p = att_prevalence_elasticity_p,
    att_prevalence_elasticity_eif_mean = att_prevalence_elasticity_eif_mean,
    policy_translation_outcome_scale = cfg$policy$outcome_scale_label %||%
      "configured bounded/capped outcome",
    gain_per_prevented_case = gain_per_prevented_case,
    gain_per_prevented_case_se = gain_per_prevented_case_se,
    gain_per_one_percentage_point_prevalence_reduction = gain_per_one_percentage_point,
    gain_per_one_percentage_point_se = gain_per_one_percentage_point_se,
    att_target_eps_mu1 = eps_att_mu1, att_target_eps_mu0 = eps_att_mu0,
    att_target_score_mu1 = target_score_mu1, att_target_score_mu0 = target_score_mu0,
    att_eif_weighted_mean_scaled = att_eif_mean_scaled,
    att_onestep_estimate = att_onestep_estimate, att_onestep_se = att_onestep_se,
    att_plugin_estimate = att_plugin_estimate,
    att_tmle_eps = eps_att,
    att_wt_ctrl_max = att_wt_ctrl_max, att_wt_ctrl_p99 = att_wt_ctrl_p99,
    att_wt_ctrl_ess = att_wt_ctrl_ess,
    att_survey_odds_control_ess = att_ctrl_odds_ess, att_wt_ctrl_n = att_wt_ctrl_n,
    att_wt_trt_max = att_wt_trt_max, att_g_ctrl_max = att_g_ctrl_max,
    att_tmle_estimate = att_tmle_estimate, att_tmle_se = att_tmle_se,
    att_tmle_ci_lower = att_tmle_ci_lower, att_tmle_ci_upper = att_tmle_ci_upper,
    att_tmle_p_value = att_tmle_p_value, att_tmle_G_star = att_tmle_G_star,
    att_tmle_cf_under_control = att_tmle_cf_under_control,
    att_tmle_eif_weighted_mean = att_tmle_eif_mean,
    att_eif_weighted_mean = att_eif_mean,
    att_pi_set_to_one_wiring_check = att_nocens_check,
    att_pi_set_to_one_wiring_gap = att_nocens_check - att_estimate,
    att_nocensoring_check_legacy = att_nocens_check,
    att_tmle_pi_set_to_one = att_tmle_nocens, att_onestep_pi_set_to_one = att_onestep_nocens,
    trim_ate_estimate = trim_estimate, trim_ate_se = trim_se,
    trim_ate_ci_lower = trim_ci[1], trim_ate_ci_upper = trim_ci[2], trim_ate_p = trim_p,
    trim_eps = trim_eps,
    n_trimmed = n_trim,
    trim_g_lower = if (isTRUE(cfg$final_tmle$trim_enable)) cfg$final_tmle$trim_g_lower %||% NA_real_ else NA_real_,
    trim_g_upper = if (isTRUE(cfg$final_tmle$trim_enable)) cfg$final_tmle$trim_g_upper %||% NA_real_ else NA_real_,
    n = length(weights), n_clusters = J,
    n_treated = sum(A == 1L), n_control = sum(A == 0L),
    kish_ess_overall = kish_ess_safe(weights),
    kish_ess_treated = kish_ess_safe(weights[A == 1L]),
    kish_ess_control = kish_ess_safe(weights[A == 0L]),
    protected_processed_columns_min = design_min("n_protected_cols"),
    protected_processed_columns_max = design_max("n_protected_cols"),
    nonprotected_processed_columns_min = design_min("n_processed_cols_nonprotected"),
    nonprotected_processed_columns_max = design_max("n_processed_cols_nonprotected"),
    optional_budgeted_processed_columns_min =
      design_min("n_processed_cols_optional_budgeted"),
    optional_budgeted_processed_columns_max =
      design_max("n_processed_cols_optional_budgeted"),
    total_processed_columns_min = design_min("n_processed_cols"),
    total_processed_columns_max = design_max("n_processed_cols"),
    epsilon_fluctuation = eps_hat,
    y_lower = y_lower, y_upper = y_upper,
    stringsAsFactors = FALSE)
  # Supplemental fixed-nuisance multiplier diagnostics. These fields are
  # always present in the result schema; a failure is fatal rather than silently
  # deleting columns from the production output.
  if (is.null(cl_eic_att) || !is.finite(att_estimate) || !is.finite(att_se) || att_se <= 0)
    stop("ATT robustness diagnostics require a finite ATT, positive SE, and cluster EIF.", call. = FALSE)
  Sg_h <- as.numeric(cl_eic_att)
  n_h <- length(weights)
  s2_h <- sum(Sg_h^2); s4_h <- sum(Sg_h^4)
  Gstar_h <- if (is.finite(s4_h) && s4_h > 0) (s2_h^2) / s4_h else NA_real_
  boot_h <- with_local_seed(seed_for(cfg, 424242L),
    vapply(seq_len(2000L), function(b)
      att_estimate + sum(sample(c(-1, 1), length(Sg_h), replace = TRUE) * Sg_h) / n_h,
      numeric(1)))
  zc <- stats::qnorm(0.975)
  res_df$att_G_star <- Gstar_h
  res_df$att_se_multiplier_boot <- stats::sd(boot_h)
  res_df$ci_lower_normal <- att_estimate - zc * att_se
  res_df$ci_upper_normal <- att_estimate + zc * att_se
  res_df$p_normal <- 2 * stats::pnorm(-abs(att_estimate / att_se))
  res_df$att_pct_of_no_depression_baseline <- pct_depression_effect / 100
  res_df$att_prevention_gain_fraction <- pct_prevention_gain / 100
  res_df$att_cf_under_control <- cf_treated_under_control
  res_df$att_support_ok <- isTRUE(att_support_ok)
  res_df$multiplier_bootstrap_status <- "success_fixed_nuisance_cluster_sign_flip"

  if (isTRUE(cfg$global$save_stage_csvs)) {
    p <- write_run_csv(res_df, cfg, cfg$final_tmle$results_csv)
    msg(sprintf("  [TMLE] Main result written: %s", basename(p)), cfg = cfg)
  }

  if (isTRUE(is_raw_dollar_outcome)) {
    msg(sprintf(
      "\n===== Final CV-TMLE COMPLETE =====\n  DOLLAR ATT (%s): $%.2f, SE $%.2f, 95%% CI [$%.2f, $%.2f], p = %.4g\n  POLICY-FACING %s: %.2f%%, SE %.2f, 95%% CI [%.2f%%, %.2f%%], p = %.4g\n  Full-sample ATE (secondary): %.2f, SE %.2f.\n==================================\n",
      estimand_label, head_est, head_se, head_ci[1], head_ci[2], head_p,
      pct_primary_name, pct_primary_estimate, pct_primary_se,
      pct_primary_ci[1], pct_primary_ci[2], pct_primary_p,
      ate_full_estimate, ate_full_se), cfg = cfg)
  } else {
    msg(sprintf(
      "\n===== Final CV-TMLE COMPLETE =====\n  ATT (%s): %.4f, SE %.4f, 95%% CI [%.4f, %.4f], p = %.4g\n  Full-sample ATE (secondary): %.4f, SE %.4f.\n==================================\n",
      estimand_label, head_est, head_se, head_ci[1], head_ci[2], head_p,
      ate_full_estimate, ate_full_se), cfg = cfg)
  }

  if (!is.null(timers)) timers$stop("final_cv_tmle")

  list(
    result = res_df,
    comparator_estimates = list(
      ate_tmle = psi_hat,
      ate_plugin_initial = psi_plugin_initial,
      ate_aipw_initial = psi_aipw_initial,
      tmle_minus_plugin_initial = psi_hat - psi_plugin_initial,
      tmle_minus_aipw_initial = psi_hat - psi_aipw_initial,
      att_estimate = att_estimate, att_se = att_se,
      att_tmle_estimate = att_tmle_estimate, att_tmle_se = att_tmle_se,
      att_tmle_ci_lower = att_tmle_ci_lower, att_tmle_ci_upper = att_tmle_ci_upper,
      att_tmle_G_star = att_tmle_G_star, att_tmle_cf_under_control = att_tmle_cf_under_control,
      att_onestep_estimate = att_onestep_estimate, att_onestep_se = att_onestep_se,
      eps_att = eps_att, att_headline_method = att_headline_method,
      att_eif_weighted_mean = att_eif_mean,
      att_pi_set_to_one_wiring_check = att_nocens_check,
      trim_ate_estimate = trim_estimate, trim_ate_se = trim_se,
      n_trimmed = n_trim),
    Qbar1W = Qbar1W, Qbar0W = Qbar0W, QbarAW = QbarAW,
    Qstar1W_orig = Qstar1W_orig, Qstar0W_orig = Qstar0W_orig,
    gn = gn, pi_AW = pi_AW, pi_1W = pi_1W, pi_0W = pi_0W,
    gn_raw = gn_raw, pi_AW_raw = pi_AW_raw, pi_1W_raw = pi_1W_raw, pi_0W_raw = pi_0W_raw,
    att_components = att_components,
    D = primary_D, D_primary = primary_D, cluster_eic = primary_cluster_eic,
    D_ate = D, D_ate_orig = D_orig, cluster_eic_ate = cluster_eic,
    weights = weights, strata = strata, respondent_ids = respondent_ids,
    survey_design_frame = full_survey_design_frame,
    outer_fold = outer_fold,
    sl_log = sl_log_df,
    selection_log = sel_log_df,
    fold_support_log = if (length(fold_support_log)) do.call(rbind, fold_support_log) else NULL,
    run_manifest = if (length(run_manifest_rows)) do.call(rbind, run_manifest_rows) else NULL,
    internal_fold_support_log = if (length(internal_fold_support_log)) do.call(rbind, internal_fold_support_log) else NULL,
    fold_times = fold_times,
    q_prediction_clipping = q_clip_df,
    selected_by_fold = selected_by_fold,
    protected_mapping_by_fold = protected_mapping_by_fold,
    y_lower = y_lower, y_upper = y_upper, y_range = y_range,
    outcome_type = outcome_type,
    policy_translation = policy_translation_table
  )
}

# =============================================================================
