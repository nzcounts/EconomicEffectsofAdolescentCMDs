# Generated from the reviewed v8.28 production source.
# Original lines: 1249-1855.
# Module role: Package loading and configuration validation.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 1) PACKAGE LOADING AND CONFIG VALIDATION
# =============================================================================
# Plain-English role: Make sure the packages needed for the enabled stages
# are installed, then attach them. Validate the cfg object so that impossible
# settings (e.g., no learners enabled, or no screening reaches the final
# estimator) stop the pipeline loudly before any expensive work begins.

`%||%` <- function(x, y) if (is.null(x)) y else x

count_enabled_learners <- function(lib_cfg) {
  toggles <- c("use_mean","use_glm","use_glmnet","use_glmnet_h1fs",
               "use_glmnet_A_unpenalized","use_ranger",
               "use_xgboost","use_xgboost_rich","use_earth","use_gam",
               "use_svm","use_nnet")
  sum(vapply(intersect(toggles, names(lib_cfg)),
             function(nm) isTRUE(lib_cfg[[nm]]), logical(1)))
}

validate_runtime_environment <- function(cfg, required_packages) {
  rt <- cfg$runtime %||% list()
  if (isTRUE(rt$enforce_exact_R_version %||% FALSE)) {
    found_R <- paste(R.version$major, R.version$minor, sep = ".")
    expected_R <- as.character(rt$required_R_version %||% "")
    if (!nzchar(expected_R) || !identical(found_R, expected_R))
      stop(sprintf("Production R version mismatch: required %s, found %s.",
                   expected_R, found_R), call. = FALSE)
  }
  if (isTRUE(rt$enforce_exact_package_versions %||% FALSE)) {
    expected <- rt$required_package_versions %||% character(0)
    if (is.null(names(expected)) || any(!nzchar(names(expected))))
      stop("runtime$required_package_versions must be a named character vector.",
           call. = FALSE)
    check <- intersect(required_packages, names(expected))
    missing_expectations <- setdiff(required_packages, names(expected))
    if (length(missing_expectations))
      stop("No frozen production version was configured for required package(s): ",
           paste(missing_expectations, collapse = ", "), call. = FALSE)
    found <- vapply(check, function(pkg)
      as.character(utils::packageVersion(pkg)), character(1))
    bad <- check[found != unname(expected[check])]
    if (length(bad)) {
      detail <- paste(sprintf("%s required=%s found=%s", bad,
                              unname(expected[bad]), found[bad]), collapse = "; ")
      stop("Production package-version mismatch: ", detail, call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Production-verification gate for outcome constructors. This prevents a
# placeholder or illustrative constructor from producing plausible-looking
# results merely because it returns nonmissing values. Wave IV Compensation is
# verified against H4EC2/H4EC3. PassThrough is allowed only when its source is
# explicitly named because it performs no guessed recoding.
is_verified_outcome_spec <- function(cfg, family, wave) {
  wave <- as.integer(wave)
  fam_cfg <- cfg$outcome$families[[family]]
  if (is.null(fam_cfg)) return(FALSE)

  if (identical(family, "Compensation")) {
    src <- fam_cfg$sources[[as.character(wave)]]
    return(
      identical(wave, 4L) && is.list(src) &&
      identical(as.character(src$exact_var), "H4EC2") &&
      identical(as.character(src$bracket_var), "H4EC3") &&
      isTRUE(all.equal(as.numeric(fam_cfg$exact_valid_min), 0)) &&
      isTRUE(all.equal(as.numeric(fam_cfg$exact_valid_max), 999995)) &&
      setequal(as.numeric(fam_cfg$exact_missing_codes), c(9999996, 9999998)) &&
      setequal(as.numeric(fam_cfg$bracket_valid_codes), 1:12) &&
      setequal(as.numeric(fam_cfg$bracket_missing_codes), c(96, 97, 98)) &&
      isTRUE(all.equal(
        as.numeric(fam_cfg$bracket_map[as.character(1:12)]),
        c(2500, 7500, 12500, 17500, 22500, 27500,
          35000, 45000, 62500, 87500, 125000, 175000)))
    )
  }

  if (identical(family, "PassThrough")) {
    src <- fam_cfg$source_var
    return(is.character(src) && length(src) == 1L && !is.na(src) && nzchar(src) &&
           (fam_cfg$type %||% "continuous") %in% c("continuous", "binary_single"))
  }

  FALSE
}

validate_cfg <- function(cfg) {
  # --- Rough-screen helper settings -----------------------------------------
  if (!cfg$rough_prescreen$cutoff_rule %in% c("positive","knee","topk"))
    stop("rough_prescreen$cutoff_rule must be one of 'positive', 'knee', 'topk'.", call. = FALSE)
  if (!isTRUE(cfg$rough_prescreen$cluster_aware_folds))
    stop("rough_prescreen$cluster_aware_folds must remain TRUE; row-level screening folds are disabled.", call. = FALSE)
  if (!identical(tolower(cfg$final_tmle$att_estimator %||% "tmle"), "tmle"))
    stop(paste0(
      "final_tmle$att_estimator must be 'tmle'. The one-step AIPTW estimate ",
      "is retained as a diagnostic comparator but is not a supported headline option."),
      call. = FALSE)
  if (!is.null(cfg$final_tmle$primary_estimand) &&
      !cfg$final_tmle$primary_estimand %in% c("ate", "trimmed", "att"))
    stop("final_tmle$primary_estimand must be one of 'ate', 'trimmed', 'att'.", call. = FALSE)
  if (identical(cfg$final_tmle$primary_estimand, "att") && !isTRUE(cfg$final_tmle$report_att))
    stop("final_tmle$primary_estimand='att' requires report_att=TRUE.", call. = FALSE)
  if (cfg$rough_prescreen$folds < 2L || cfg$final_tmle$vfolds < 2L ||
      cfg$final_tmle$internal_superlearner_folds < 2L ||
      cfg$final_tmle$rough_folds < 2L)
    stop("All rough/final fold counts must be at least 2.", call. = FALSE)

  # --- Causal-variable governance -----------------------------------------
  mandatory_W <- get_mandatory_W(cfg)
  if (anyDuplicated(canonical_role_key(mandatory_W)))
    stop("Mandatory W names are duplicated after canonical join-suffix removal.", call. = FALSE)
  excluded_keys <- canonical_role_key(cfg$analysis$extra_exclude_from_candidates %||% character(0))
  conflict_mandatory <- mandatory_W[canonical_role_key(mandatory_W) %in% excluded_keys]
  if (length(conflict_mandatory))
    stop("Mandatory W variable(s) are also configured for exclusion: ",
         paste(conflict_mandatory, collapse = ", "), call. = FALSE)
  if (!isTRUE(cfg$final_tmle$mandatory_W_bypass_screening %||% FALSE))
    stop("mandatory_W_bypass_screening must remain TRUE.", call. = FALSE)

  transform <- tolower(cfg$outcome$compensation_transform %||% "identity")
  if (!transform %in% c("identity", "log1p", "asinh"))
    stop("outcome$compensation_transform must be identity, log1p, or asinh.", call. = FALSE)
  if (isTRUE(cfg$outcome$log_transform %||% FALSE))
    stop("Legacy outcome$log_transform must remain FALSE; use compensation_transform.", call. = FALSE)
  if (!is.finite(cfg$outcome$compensation_asinh_scale %||% NA_real_) ||
      cfg$outcome$compensation_asinh_scale <= 0)
    stop("outcome$compensation_asinh_scale must be positive and finite.", call. = FALSE)
  if (!is.logical(cfg$outcome$compensation_exact_only) ||
      length(cfg$outcome$compensation_exact_only) != 1L)
    stop("outcome$compensation_exact_only must be a single logical value.", call. = FALSE)
  policy_components <- cfg$policy$enable_policy_components %||% TRUE
  policy_translation <- cfg$policy$enable_att_prevalence_translation %||% TRUE
  policy_fail <- cfg$policy$fail_on_policy_component_checks %||% TRUE
  if (!is.logical(policy_components) || length(policy_components) != 1L || is.na(policy_components) ||
      !is.logical(policy_translation) || length(policy_translation) != 1L || is.na(policy_translation) ||
      !is.logical(policy_fail) || length(policy_fail) != 1L || is.na(policy_fail))
    stop("Policy component, translation, and failure controls must each be one nonmissing logical value.", call. = FALSE)
  if (isTRUE(policy_translation) && !isTRUE(policy_components))
    stop("policy$enable_att_prevalence_translation=TRUE requires enable_policy_components=TRUE.", call. = FALSE)
  if (isTRUE(policy_translation) &&
      length(cfg$policy$relative_prevalence_reductions %||% numeric(0)) &&
      (any(!is.finite(cfg$policy$relative_prevalence_reductions)) ||
       any(cfg$policy$relative_prevalence_reductions <= 0) ||
       any(cfg$policy$relative_prevalence_reductions > 1)))
    stop("Policy relative prevalence reductions must lie in (0,1].", call. = FALSE)
  q_clip_bounds <- c(cfg$final_tmle$Q_clip_warning_fraction,
                     cfg$final_tmle$Q_clip_review_fraction)
  if (any(!is.finite(q_clip_bounds)) || any(q_clip_bounds < 0) ||
      any(q_clip_bounds > 1) || q_clip_bounds[1] > q_clip_bounds[2])
    stop("Q clipping warning/review fractions must be ordered in [0,1].", call. = FALSE)
  completion_toggle <- cfg$diagnostics$enable_wave2_completion_diagnostic %||% TRUE
  mnar_toggle <- cfg$diagnostics$enable_mnar_pattern_mixture %||% TRUE
  evalue_toggle <- cfg$diagnostics$enable_evalue %||% FALSE
  diagnostic_toggles <- list(
    enable_wave2_completion_diagnostic = completion_toggle,
    enable_mnar_pattern_mixture = mnar_toggle,
    enable_evalue = evalue_toggle)
  bad_diagnostic_toggles <- names(diagnostic_toggles)[!vapply(
    diagnostic_toggles,
    function(z) is.logical(z) && length(z) == 1L && !is.na(z),
    logical(1))]
  if (length(bad_diagnostic_toggles))
    stop("Diagnostic toggles must each be one nonmissing logical value: ",
         paste(bad_diagnostic_toggles, collapse = ", "), ".", call. = FALSE)
  balance_progress_every <- cfg$diagnostics$balance_progress_every %||% 500L
  if (!is.numeric(balance_progress_every) || length(balance_progress_every) != 1L ||
      is.na(balance_progress_every) || !is.finite(balance_progress_every) ||
      balance_progress_every < 1 ||
      abs(balance_progress_every - round(balance_progress_every)) > 1e-8)
    stop("diagnostics$balance_progress_every must be one positive integer.", call. = FALSE)
  if (isTRUE(cfg$diagnostics$enable_mnar_pattern_mixture %||% TRUE)) {
    mnar_grid <- as.numeric(cfg$diagnostics$mnar_shift_sd_grid %||% numeric(0))
    if (!length(mnar_grid) || any(!is.finite(mnar_grid)) || !any(mnar_grid == 0))
      stop("Enabled MNAR diagnostics require a finite shift grid containing zero.", call. = FALSE)
  }

  mort <- cfg$mortality_sensitivity %||% list(enabled = FALSE)
  mortality_enabled <- mort$enabled %||% FALSE
  if (!is.logical(mortality_enabled) || length(mortality_enabled) != 1L ||
      is.na(mortality_enabled))
    stop("mortality_sensitivity$enabled must be one nonmissing logical value.", call. = FALSE)
  if (isTRUE(mortality_enabled)) {
    if (!is.character(cfg$paths$mortality) || length(cfg$paths$mortality) != 1L ||
        is.na(cfg$paths$mortality) || !nzchar(trimws(cfg$paths$mortality)))
      stop("Enabled mortality sensitivity requires cfg$paths$mortality.", call. = FALSE)
    scalar_text <- c("source_var", "interview_year_var", "derived_death_year_var",
                     "death_in_window_var", "death_before_outcome_var")
    for (nm in scalar_text) {
      z <- mort[[nm]] %||% NULL
      if (!is.character(z) || length(z) != 1L || is.na(z) || !nzchar(trimws(z)))
        stop("Enabled mortality sensitivity requires one nonblank ", nm, ".", call. = FALSE)
    }
    yr <- as.integer(c(mort$death_year_start, mort$death_year_end,
                       mort$valid_year_min, mort$valid_year_max))
    if (length(yr) != 4L || anyNA(yr) || yr[1L] > yr[2L] ||
        yr[3L] > yr[1L] || yr[4L] < yr[2L])
      stop("Mortality year-window and valid-year limits are inconsistent.", call. = FALSE)
    mortality_gate_values <- list(
      native_missing_means_no_death = mort$native_missing_means_no_death,
      fail_on_unrecognized_codes = mort$fail_on_unrecognized_codes,
      require_complete_linkage = mort$require_complete_linkage,
      fail_on_death_with_observed_original_outcome =
        mort$fail_on_death_with_observed_original_outcome)
    bad_mortality_gates <- names(mortality_gate_values)[!vapply(
      mortality_gate_values, function(z)
        is.logical(z) && length(z) == 1L && !is.na(z), logical(1))]
    if (length(bad_mortality_gates))
      stop("Mortality gate controls must each be one nonmissing logical value: ",
           paste(bad_mortality_gates, collapse = ", "), ".", call. = FALSE)
    mortality_roles <- canonical_role_key(c(
      cfg$analysis$id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
      cfg$analysis$weight_var, cfg$analysis$exposure_var,
      cfg$analysis$outcome_var, cfg$analysis$outcome_observed_var))
    if (canonical_role_key(mort$source_var) == canonical_role_key(mort$death_before_outcome_var))
      stop("Mortality source_var and death_before_outcome_var must be different.", call. = FALSE)
    if (canonical_role_key(mort$source_var) %in% mortality_roles)
      stop("mortality source_var conflicts with a core analysis variable.", call. = FALSE)
    if (canonical_role_key(mort$death_before_outcome_var) %in% mortality_roles)
      stop("death_before_outcome_var conflicts with a core analysis variable.", call. = FALSE)
    if (length(mort$no_death_codes %||% numeric(0)) &&
        any(!is.finite(as.numeric(mort$no_death_codes))))
      stop("mortality_sensitivity$no_death_codes must be finite numeric codes.", call. = FALSE)
    if (!is.logical(mort$composite_zero_at_death) ||
        length(mort$composite_zero_at_death) != 1L ||
        is.na(mort$composite_zero_at_death))
      stop("mortality_sensitivity$composite_zero_at_death must be one nonmissing logical value.",
           call. = FALSE)
    iv <- as.integer(c(mort$interview_year_valid_min,
                       mort$interview_year_valid_max))
    if (length(iv) != 2L || anyNA(iv) || iv[1L] > iv[2L])
      stop("Mortality interview-year audit limits are invalid.", call. = FALSE)
    if (!identical(mort$earnings_price_basis %||% "",
                   "nominal_past_year_dollars_no_inflation_adjustment"))
      stop(paste0(
        "mortality_sensitivity$earnings_price_basis must remain ",
        "'nominal_past_year_dollars_no_inflation_adjustment' for this specification."),
        call. = FALSE)
    timing_names <- canonical_role_key(get_mortality_role_vars(cfg))
    if (anyDuplicated(timing_names))
      stop("Mortality source/derived/audit variable names must be distinct after canonicalization.",
           call. = FALSE)
  }

  # --- Source-informed exact-code missing classifier ----------------------
  if (!identical(cfg$preprocessing$missing_classifier %||% "global_source_informed_exact_v1",
                 "global_source_informed_exact_v1"))
    stop("preprocessing$missing_classifier must be 'global_source_informed_exact_v1'.", call. = FALSE)
  if (!identical(cfg$preprocessing$numeric_missing_scheme %||% "dual_indicators", "dual_indicators"))
    stop("preprocessing$numeric_missing_scheme must remain 'dual_indicators' so numeric general-missing and structural-skip indicators are preserved.", call. = FALSE)
  if (!is.logical(cfg$preprocessing$global_missing_dictionary_required) ||
      length(cfg$preprocessing$global_missing_dictionary_required) != 1L ||
      !is.character(cfg$preprocessing$global_missing_dictionary_native_only_patterns %||% character(0)))
    stop("Global missing-dictionary enforcement settings are invalid.", call. = FALSE)
  if ((cfg$preprocessing$auto_questionnaire_max_unique %||% 0L) < 2L ||
      (cfg$preprocessing$auto_questionnaire_max_unique_prop %||% 0) <= 0 ||
      (cfg$preprocessing$auto_questionnaire_max_unique_prop %||% 0) > 1 ||
      (cfg$preprocessing$auto_categorical_max_abs_value %||% 0) <= 0 ||
      (cfg$preprocessing$auto_integer_like_min_prop %||% 0) <= 0 ||
      (cfg$preprocessing$auto_integer_like_min_prop %||% 0) > 1 ||
      (cfg$preprocessing$auto_integer_tolerance %||% 0) <= 0 ||
      (cfg$preprocessing$auto_special_code_max_digits %||% 0L) < 2L ||
      (cfg$preprocessing$auto_special_code_max_digits %||% 8L) > 7L ||
      (cfg$preprocessing$auto_percentage_min_unique %||% 0L) < 2L ||
      (cfg$preprocessing$auto_percentage_min_span %||% -1) < 0 ||
      (cfg$preprocessing$auto_dense_small_count_min_unique %||% 0L) < 2L ||
      (cfg$preprocessing$auto_dense_small_count_max_value %||% 0L) < 1L ||
      !is.character(cfg$preprocessing$questionnaire_name_patterns %||% character(0)) ||
      !is.character(cfg$preprocessing$questionnaire_name_exclude_patterns %||% character(0)) ||
      !is.character(cfg$preprocessing$known_codebook_overlap_vars %||% character(0)) ||
      !is.character(cfg$preprocessing$nonquestionnaire_long_factors %||% character(0)))
    stop("Source-informed exact-code classifier settings are invalid.", call. = FALSE)
  mnar_toggles <- cfg$diagnostics[c(
    "enable_mnar_breakdown", "enable_manski_bounds", "enable_mnar_calibrated")]
  bad_mnar_toggles <- names(mnar_toggles)[!vapply(mnar_toggles, function(z)
    is.logical(z) && length(z) == 1L && !is.na(z), logical(1))]
  if (length(bad_mnar_toggles))
    stop("MNAR extension toggles must be scalar nonmissing logical values: ",
         paste(bad_mnar_toggles, collapse = ", "), call. = FALSE)
  if (!is.finite(cfg$diagnostics$mnar_breakdown_max_sd) ||
      cfg$diagnostics$mnar_breakdown_max_sd <= 0)
    stop("diagnostics$mnar_breakdown_max_sd must be positive and finite.", call. = FALSE)
  grid_n <- cfg$diagnostics$mnar_breakdown_grid_n
  if (!is.numeric(grid_n) || length(grid_n) != 1L || is.na(grid_n) ||
      grid_n < 11L || abs(grid_n - round(grid_n)) > 1e-8)
    stop("diagnostics$mnar_breakdown_grid_n must be an integer >= 11.", call. = FALSE)
  probs <- as.numeric(cfg$diagnostics$mnar_calibration_probs)
  if (length(probs) != 2L || any(!is.finite(probs)) ||
      probs[1L] <= 0 || probs[2L] >= 1 || probs[1L] >= probs[2L])
    stop("diagnostics$mnar_calibration_probs must contain two increasing probabilities inside (0,1).",
         call. = FALSE)
  B <- cfg$diagnostics$mnar_calibration_boot_reps
  minB <- cfg$diagnostics$mnar_calibration_min_valid_boot_reps
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 50L ||
      abs(B - round(B)) > 1e-8 || !is.numeric(minB) || length(minB) != 1L ||
      is.na(minB) || minB < 50L || minB > B || abs(minB - round(minB)) > 1e-8)
    stop("MNAR calibration bootstrap repetitions/minimum-valid repetitions are invalid.",
         call. = FALSE)
  boot_seed <- cfg$diagnostics$mnar_calibration_boot_seed
  if (!is.numeric(boot_seed) || length(boot_seed) != 1L || is.na(boot_seed) ||
      !is.finite(boot_seed) || abs(boot_seed - round(boot_seed)) > 1e-8)
    stop("diagnostics$mnar_calibration_boot_seed must be one finite integer.", call. = FALSE)
  if (!identical(cfg$diagnostics$mnar_calibration_bootstrap_design,
                 "region_stratified_psu"))
    stop("diagnostics$mnar_calibration_bootstrap_design must be 'region_stratified_psu'.",
         call. = FALSE)
  if (!isTRUE(cfg$diagnostics$mnar_calibration_weighted_quantiles))
    stop("diagnostics$mnar_calibration_weighted_quantiles must remain TRUE.",
         call. = FALSE)
  if (isTRUE(cfg$safety$require_publication_ready_marker %||% TRUE) &&
      (!isTRUE(cfg$stages$run_diagnostics) || !isTRUE(cfg$diagnostics$save_csvs)))
    stop("Publication-readiness gating requires diagnostics and diagnostic CSV output.",
         call. = FALSE)
  rt <- cfg$runtime %||% list()
  if (!is.logical(rt$enforce_exact_R_version) || length(rt$enforce_exact_R_version) != 1L ||
      !is.character(rt$required_R_version) || length(rt$required_R_version) != 1L ||
      !nzchar(rt$required_R_version) || !is.logical(rt$enforce_exact_package_versions) ||
      length(rt$enforce_exact_package_versions) != 1L)
    stop("Runtime-version controls are invalid.", call. = FALSE)
  if (!is.logical(cfg$global$resume_mode) ||
      length(cfg$global$resume_mode) != 1L || is.na(cfg$global$resume_mode))
    stop("global$resume_mode must be one nonmissing logical value.", call. = FALSE)
  operational_safety <- cfg$safety[c(
    "require_fresh_primary_output_dir", "allow_output_overwrite",
    "verify_atomic_writes", "require_publication_ready_marker")]
  bad_operational_safety <- names(operational_safety)[!vapply(
    operational_safety, function(z)
      is.logical(z) && length(z) == 1L && !is.na(z), logical(1))]
  if (length(bad_operational_safety))
    stop("Operational safety controls must be scalar nonmissing logical values: ",
         paste(bad_operational_safety, collapse = ", "), call. = FALSE)
  expected_versions <- rt$required_package_versions %||% character(0)
  if (!is.character(expected_versions) || !length(expected_versions) ||
      is.null(names(expected_versions)) || anyNA(expected_versions) ||
      any(!nzchar(names(expected_versions))) ||
      any(!nzchar(trimws(expected_versions))))
    stop("runtime$required_package_versions must be a nonempty named character vector.",
         call. = FALSE)
  allowed_fresh <- cfg$safety$fresh_output_allowed_basenames %||% character(0)
  if (!is.character(allowed_fresh) || anyNA(allowed_fresh) ||
      any(!nzchar(trimws(allowed_fresh))) || any(basename(allowed_fresh) != allowed_fresh))
    stop("safety$fresh_output_allowed_basenames must contain base filenames only.",
         call. = FALSE)
  max_path <- cfg$safety$windows_max_path
  if (!is.numeric(max_path) || length(max_path) != 1L || is.na(max_path) ||
      max_path < 100L || abs(max_path - round(max_path)) > 1e-8)
    stop("safety$windows_max_path must be an integer >= 100.", call. = FALSE)

  clip_floors <- as.numeric(cfg$diagnostics$att_g_pi_clip_sensitivity_floors %||% numeric(0))
  if (length(clip_floors) &&
      (any(!is.finite(clip_floors)) || any(clip_floors <= 0) || any(clip_floors >= 0.5)))
    stop("diagnostics$att_g_pi_clip_sensitivity_floors must lie in (0, 0.5).", call. = FALSE)

  primary_bounds <- c(cfg$final_tmle$g_lower, cfg$final_tmle$g_upper,
                      cfg$final_tmle$pi_lower, cfg$final_tmle$pi_upper)
  if (any(!is.finite(primary_bounds)) ||
      cfg$final_tmle$g_lower <= 0 || cfg$final_tmle$g_upper >= 1 ||
      cfg$final_tmle$g_lower >= cfg$final_tmle$g_upper ||
      cfg$final_tmle$pi_lower <= 0 || cfg$final_tmle$pi_upper >= 1 ||
      cfg$final_tmle$pi_lower >= cfg$final_tmle$pi_upper)
    stop("Primary g/pi clipping bounds must be finite, ordered, and strictly inside (0,1).", call. = FALSE)

  # --- Stage dependency and strict production behavior --------------------
  if (isTRUE(cfg$stages$run_diagnostics) && !isTRUE(cfg$stages$run_final_cv_tmle))
    stop("run_diagnostics=TRUE requires run_final_cv_tmle=TRUE because no final-fit cache is defined.", call. = FALSE)
  if (!isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE))
    stop("final_tmle$fail_on_nuisance_fallback must remain TRUE for the final production analysis.", call. = FALSE)

  # --- Final TMLE learner libraries ---------------------------------------
  if (isTRUE(cfg$final_tmle$use_fold_checkpoints))
    warning("Fold checkpoints are enabled. The fixed first production run uses fresh folds with use_fold_checkpoints=FALSE.", call. = FALSE)
  expected_protected <- paste0("H1FS", 1:19)
  if (!isTRUE(cfg$final_tmle$protected_W_preserve_substantive_levels %||% FALSE))
    stop("final_tmle$protected_W_preserve_substantive_levels must be TRUE for the primary analysis.", call. = FALSE)
  if (!identical(as.character(cfg$final_tmle$protected_W), expected_protected)) {
    stop("final_tmle$protected_W must contain exactly H1FS1-H1FS19 in order for the fixed primary analysis.", call. = FALSE)
  }
  if (!isTRUE(cfg$final_tmle$protected_W_bypass_screening %||% FALSE))
    stop("protected_W_bypass_screening must remain TRUE because H1FS1-H1FS19 are mandatory baseline confounders.", call. = FALSE)
  if (!all(expected_protected %in% get_mandatory_W(cfg)))
    stop("Every protected H1FS item must also be included in the mandatory W set.", call. = FALSE)
  if (!tolower(cfg$final_tmle$percentage_primary %||% "prevention_gain") %in%
      c("prevention_gain", "depression_effect")) {
    stop("final_tmle$percentage_primary must be 'prevention_gain' or 'depression_effect'.", call. = FALSE)
  }
  if (!is.finite(cfg$final_tmle$target_score_tol) || cfg$final_tmle$target_score_tol <= 0 ||
      !is.finite(cfg$final_tmle$att_eif_center_tol_scaled) || cfg$final_tmle$att_eif_center_tol_scaled <= 0) {
    stop("Targeting tolerances must be positive finite numbers.", call. = FALSE)
  }
  q_bound <- cfg$outcome$continuous_upper_quantile
  if (!is.finite(q_bound) || q_bound <= 0 || q_bound > 1)
    stop("outcome$continuous_upper_quantile must be in (0, 1].", call. = FALSE)
  require_script_md5 <- cfg$global$require_script_md5 %||% FALSE
  if (!is.logical(require_script_md5) || length(require_script_md5) != 1L ||
      is.na(require_script_md5))
    stop("global$require_script_md5 must be one nonmissing logical value.",
         call. = FALSE)
  if (isTRUE(require_script_md5) &&
      (isTRUE(cfg$stages$run_final_cv_tmle) || isTRUE(cfg$stages$run_multiseed_att)))
    pipeline_script_fingerprint(cfg, strict = TRUE)

  if (isTRUE(cfg$learners$g$use_xgboost_rich %||% FALSE) ||
      isTRUE(cfg$learners$pi$use_xgboost_rich %||% FALSE))
    stop("use_xgboost_rich is supported only for Q.", call. = FALSE)
  rich_cfg <- cfg$learners$xgboost_rich
  if (isTRUE(cfg$learners$Q$use_xgboost_rich %||% FALSE)) {
    rich_vals <- c(rich_cfg$ntrees, rich_cfg$max_depth,
                   rich_cfg$shrinkage, rich_cfg$min_child_weight)
    if (length(rich_vals) != 4L || any(!is.finite(as.numeric(rich_vals))) ||
        rich_cfg$ntrees < 1L || rich_cfg$max_depth < 1L ||
        rich_cfg$shrinkage <= 0 || rich_cfg$min_child_weight < 0)
      stop("learners$xgboost_rich contains invalid Q-only hyperparameters.", call. = FALSE)
  }
  if (isTRUE(cfg$analysis$enforce_expected_sample_gates %||% FALSE)) {
    selective_gate_flags <- cfg$analysis[c(
      "enforce_expected_cutpoint_gate", "enforce_expected_treated_gate")]
    if (length(selective_gate_flags) != 2L ||
        any(!vapply(selective_gate_flags, function(z)
          is.logical(z) && length(z) == 1L && !is.na(z), logical(1))))
      stop("Selective sample-gate flags must be nonmissing scalar logical values.", call. = FALSE)

    invariant_gate_values <- unlist(cfg$analysis[c(
      "expected_complete_cesd_n", "expected_final_n",
      "expected_cluster_n", "expected_strata_n")], use.names = TRUE)
    if (length(invariant_gate_values) != 4L ||
        any(!is.finite(as.numeric(invariant_gate_values))) ||
        any(as.numeric(invariant_gate_values) <= 0))
      stop("Invariant sample-gate values must be positive and finite when enforcement is enabled.", call. = FALSE)

    if (isTRUE(cfg$analysis$enforce_expected_cutpoint_gate) &&
        (!is.finite(as.numeric(cfg$analysis$expected_exposure_cutpoint)) ||
         as.numeric(cfg$analysis$expected_exposure_cutpoint) <= 0))
      stop("expected_exposure_cutpoint must be positive and finite when its gate is enabled.", call. = FALSE)
    if (isTRUE(cfg$analysis$enforce_expected_treated_gate) &&
        (!is.finite(as.numeric(cfg$analysis$expected_treated_n)) ||
         as.numeric(cfg$analysis$expected_treated_n) <= 0))
      stop("expected_treated_n must be positive and finite when its gate is enabled.", call. = FALSE)
  }
  if (!is.finite(cfg$outcome$continuous_bound_eps) || cfg$outcome$continuous_bound_eps < 0)
    stop("outcome$continuous_bound_eps must be a nonnegative finite number.", call. = FALSE)
  if (identical(cfg$outcome$family, "Compensation") &&
      !identical(tolower(cfg$outcome$compensation_transform %||% "identity"), "identity"))
    warning("The configured Compensation outcome is transformed; ratio and dollar policy translations will be disabled.", call. = FALSE)

  if (!isTRUE(cfg$final_tmle$cluster_aware_internal_cv))
    stop("final_tmle$cluster_aware_internal_cv must remain TRUE; row-level internal CV is not supported.", call. = FALSE)
  fold_ctl <- cfg$final_tmle[c("fold_max_attempts", "fold_projected_size_tolerance_prop",
                                "fold_max_size_ratio", "fold_max_size_deviation_prop",
                                "fold_internal_max_size_ratio",
                                "fold_internal_max_size_deviation_prop",
                                "fold_min_active_cell_n")]
  if (!is.finite(fold_ctl$fold_max_attempts) || fold_ctl$fold_max_attempts < 1L ||
      !is.finite(fold_ctl$fold_projected_size_tolerance_prop) ||
        fold_ctl$fold_projected_size_tolerance_prop < 0 ||
        fold_ctl$fold_projected_size_tolerance_prop > 0.20 ||
      !is.finite(fold_ctl$fold_max_size_ratio) || fold_ctl$fold_max_size_ratio <= 1 ||
      !is.finite(fold_ctl$fold_max_size_deviation_prop) ||
        fold_ctl$fold_max_size_deviation_prop <= 0 ||
      !is.finite(fold_ctl$fold_internal_max_size_ratio) ||
        fold_ctl$fold_internal_max_size_ratio <= 1 ||
      !is.finite(fold_ctl$fold_internal_max_size_deviation_prop) ||
        fold_ctl$fold_internal_max_size_deviation_prop <= 0 ||
      !is.finite(fold_ctl$fold_min_active_cell_n) || fold_ctl$fold_min_active_cell_n < 1L)
    stop("Whole-PSU fold controls are invalid.", call. = FALSE)
  if (!identical(cfg$outcome$continuous_cap_qrule %||% "hf8", "hf8"))
    stop("outcome$continuous_cap_qrule must be 'hf8' for the production specification.", call. = FALSE)
  if (isTRUE(cfg$outcome$continuous_cap_censoring_adjusted %||% FALSE))
    stop("The primary pipeline does not implement a censoring-adjusted cap; use a separately labeled full-refit sensitivity.", call. = FALSE)
  if (isTRUE(cfg$final_tmle$use_epp_cap %||% FALSE))
    stop("use_epp_cap=TRUE is a legacy raw-count heuristic and is not supported; use the explicit nonprotected-column budget.", call. = FALSE)
  if (isTRUE(cfg$stages$run_final_cv_tmle) && isTRUE(cfg$safety$stop_if_no_learners)) {
    libs <- c(Q  = count_enabled_learners(cfg$learners$Q),
              g  = count_enabled_learners(cfg$learners$g),
              pi = count_enabled_learners(cfg$learners$pi))
    empty <- names(libs)[libs == 0L]
    if (length(empty) > 0L)
      stop("Final TMLE learner libraries cannot be empty for: ",
           paste(empty, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(cfg$stages$run_final_cv_tmle) &&
      !isTRUE(cfg$final_tmle$nested_rough_prescreen_in_final_cv) &&
      is.null(cfg$final_tmle$prespecified_W))
    stop("Final CV-TMLE requires nested_rough_prescreen_in_final_cv=TRUE (or a non-null prespecified_W) in the pruned production pipeline.", call. = FALSE)
  if ((cfg$final_tmle$rough_top_n_outcome %||% 0L) < 1L ||
      (cfg$final_tmle$rough_top_n_missingness %||% 0L) < 0L ||
      (cfg$final_tmle$rough_top_n_joint_AY %||% 0L) < 0L ||
      (cfg$final_tmle$rough_top_n_exposure_only %||% 0L) < 0L ||
      (cfg$final_tmle$rough_top_n_exposure_for_lasso %||% 0L) < 0L ||
      (cfg$final_tmle$rough_candidate_pool_max %||% 0L) < 1L ||
      (cfg$final_tmle$rough_max_total_vars %||% 0L) < 1L)
    stop("Final rough-screen caps must be nonnegative, with positive outcome/pool/final caps.", call. = FALSE)
  if (isTRUE(cfg$final_tmle$nested_lasso_after_rough) &&
      (cfg$final_tmle$lasso_screen_folds %||% 0L) < 3L)
    stop("lasso_screen_folds must be at least 3 when nested_lasso_after_rough=TRUE because cv.glmnet requires at least three folds.", call. = FALSE)
  if ((cfg$learners$glmnet$internal_folds %||% 0L) < 3L)
    stop("learners$glmnet$internal_folds must be at least 3.", call. = FALSE)
  h1mult <- as.numeric(cfg$learners$glmnet_h1fs$h1fs_penalty_multiplier %||% NA_real_)
  amult <- as.numeric(cfg$learners$glmnet_pi_A$A_penalty_multiplier %||% NA_real_)
  if (!is.finite(h1mult) || h1mult < 0 || h1mult > 1)
    stop("learners$glmnet_h1fs$h1fs_penalty_multiplier must lie in [0,1].", call. = FALSE)
  if (!is.finite(amult) || amult < 0 || amult > 1)
    stop("learners$glmnet_pi_A$A_penalty_multiplier must lie in [0,1].", call. = FALSE)
  if (isTRUE(cfg$learners$Q$use_glmnet_h1fs %||% FALSE) ||
      isTRUE(cfg$learners$pi$use_glmnet_h1fs %||% FALSE))
    stop("use_glmnet_h1fs is supported only for g.", call. = FALSE)
  if (isTRUE(cfg$learners$Q$use_glmnet_A_unpenalized %||% FALSE) ||
      isTRUE(cfg$learners$g$use_glmnet_A_unpenalized %||% FALSE))
    stop("use_glmnet_A_unpenalized is supported only for pi.", call. = FALSE)

  # --- v6 outcome-family validation ---------------------------------------
  allowed_families <- c("EducationalAttainment","LaborForceParticipation","UsualHours",
                        "Compensation","HealthStatus","MentalHealth","SubstanceUse",
                        "PassThrough")   # pass-through negative-control outcome family
  if (!cfg$outcome$family %in% allowed_families)
    stop(sprintf("cfg$outcome$family must be one of: %s.",
                 paste(allowed_families, collapse = ", ")), call. = FALSE)
  fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
  if (is.null(fam_cfg))
    stop(sprintf("cfg$outcome$families$%s is missing.", cfg$outcome$family), call. = FALSE)
  if (identical(fam_cfg$type, "binary_nested")) {
    if (!is.null(cfg$outcome$family_member) &&
        !(cfg$outcome$family_member %in% names(fam_cfg$members)))
      stop(sprintf("cfg$outcome$family_member '%s' not in %s$members.",
                   cfg$outcome$family_member, cfg$outcome$family), call. = FALSE)
  } else if (!is.null(cfg$outcome$family_member)) {
    warning(sprintf("family_member ignored for non-nested family '%s'.", cfg$outcome$family))
  }

  waves <- cfg$outcome$waves
  if (identical(waves, "all")) waves <- 3:5
  if (!is.numeric(waves) || any(!waves %in% 3:5))
    stop("cfg$outcome$waves must be integer(s) in 3:5 (post-exposure waves) ",
         "or the string 'all'.", call. = FALSE)

  verified <- vapply(as.integer(waves), function(w)
    is_verified_outcome_spec(cfg, cfg$outcome$family, w), logical(1))
  if (any(!verified)) {
    bad_waves <- paste(as.integer(waves)[!verified], collapse = ", ")
    msg_txt <- sprintf(
      paste0("Outcome specification is not production-verified: family='%s', wave(s)=%s. ",
             "Only verified Wave IV Compensation and explicitly configured PassThrough outcomes ",
             "are allowed by default. Verify the codebook mapping, then set ",
             "cfg$safety$allow_unverified_outcome_specs=TRUE deliberately."),
      cfg$outcome$family, bad_waves)
    if (!isTRUE(cfg$safety$allow_unverified_outcome_specs %||% FALSE))
      stop(msg_txt, call. = FALSE)
    warning(msg_txt, call. = FALSE)
  }
  invisible(TRUE)
}

load_required_packages <- function(cfg) {
  required <- c("haven", "dplyr", "purrr")
  need_modeling <- isTRUE(cfg$stages$run_final_cv_tmle) ||
    isTRUE(cfg$stages$run_preflight_unit_test)
  if (need_modeling) {
    required <- c(required, "SuperLearner", "glmnet", "survey")
    use_ranger <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                             function(x) isTRUE(x$use_ranger), logical(1)))
    use_xgb <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                          function(x) isTRUE(x$use_xgboost) ||
                            isTRUE(x$use_xgboost_rich), logical(1)))
    use_earth <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                            function(x) isTRUE(x$use_earth), logical(1)))
    use_gam <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                          function(x) isTRUE(x$use_gam), logical(1)))
    use_nnet <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                           function(x) isTRUE(x$use_nnet), logical(1)))
    use_svm <- any(vapply(cfg$learners[c("Q", "g", "pi")],
                          function(x) isTRUE(x$use_svm), logical(1)))
    if (use_ranger) required <- c(required, "ranger")
    if (use_xgb) required <- c(required, "xgboost", "Matrix")
    if (use_earth) required <- c(required, "earth")
    if (use_gam) required <- c(required, "mgcv")
    if (use_nnet) required <- c(required, "nnet")
    if (use_svm) required <- c(required, "e1071")
  }
  required <- unique(required)
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L)
    stop("Please install required package(s): ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  if ("survey" %in% required && utils::packageVersion("survey") < package_version("4.1.0"))
    stop("Package 'survey' version 4.1.0 or newer is required for the declared weighted Hyndman-Fan quantile rules.", call. = FALSE)
  validate_runtime_environment(cfg, required)
  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(purrr)
    if ("SuperLearner" %in% required) library(SuperLearner)
    if ("glmnet" %in% required) library(glmnet)
    if ("survey" %in% required) library(survey)
    if ("ranger" %in% required) library(ranger)
    if ("xgboost" %in% required) library(xgboost)
    if ("Matrix" %in% required) library(Matrix)
    if ("earth" %in% required) library(earth)
    if ("mgcv" %in% required) library(mgcv)
    if ("nnet" %in% required) library(nnet)
    if ("e1071" %in% required) library(e1071)
  })
}


# Deliberately do not validate or attach packages at source time. This file is
# safe to source with autorun_pipeline=FALSE so the analyst can resolve the
# explicit restricted-codebook decisions in cfg. Every executable entry point
# validates the complete configuration and loads exactly the packages required
# by the enabled learners before reading data or fitting models.

# =============================================================================
