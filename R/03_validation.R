# =============================================================================
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

# Hard family/wave scope. This is deliberately separate from the configurable
# verification override: unsupported waves cannot be enabled by setting
# allow_unverified_outcome_specs=TRUE.
supported_outcome_waves <- function(family) {
  switch(as.character(family),
    EducationalAttainment = c(3L, 4L),
    HealthStatus = c(3L, 4L),
    Compensation = 4L,
    LaborForceParticipation = 4L,
    HoursWorked = 4L,
    PassThrough = 4L,
    UsualHours = integer(0),
    MentalHealth = integer(0),
    SubstanceUse = integer(0),
    integer(0))
}

# Outcome-specification verification gate. Only exact, codebook-locked
# family/wave mappings can produce supported results. PassThrough is allowed
# only when its source is explicitly named because it performs no guessed
# recoding.

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

  if (identical(family, "EducationalAttainment")) {
    src <- fam_cfg$sources[[as.character(wave)]]
    expected_members <- c("at_least_hs", "at_least_some_college",
                          "at_least_college_grad")
    members_ok <- identical(names(fam_cfg$members), expected_members) &&
      all(vapply(fam_cfg$members, function(z) isTRUE(z$primary), logical(1)))
    if (!members_ok) return(FALSE)
    if (identical(wave, 3L)) {
      return(is.character(src) &&
        identical(unname(src["highest_grade"]), "H3ED1") &&
        identical(unname(src["high_school_equivalency"]), "H3ED2") &&
        identical(unname(src["high_school_diploma"]), "H3ED3") &&
        identical(unname(src["college_graduate"]), "H3ED5") &&
        setequal(fam_cfg$drop_from_candidates_by_wave[["3"]],
                 c("H3ED1", "H3ED2", "H3ED3", "H3ED5")))
    }
    if (identical(wave, 4L)) {
      return(is.character(src) && length(src) == 1L &&
        identical(unname(src), "H4ED2") &&
        identical(unname(fam_cfg$drop_from_candidates_by_wave[["4"]]), "H4ED2"))
    }
    return(FALSE)
  }

  if (identical(family, "HealthStatus")) {
    src <- fam_cfg$sources[[as.character(wave)]]
    members_ok <- identical(names(fam_cfg$members), "at_least_good") &&
      isTRUE(fam_cfg$members$at_least_good$primary)
    return(members_ok && wave %in% c(3L, 4L) && is.character(src) &&
      length(src) == 1L && identical(unname(src), paste0("H", wave, "GH1")))
  }

  if (identical(family, "LaborForceParticipation")) {
    src <- fam_cfg$sources[[as.character(wave)]]
    if (identical(wave, 4L)) {
      return(is.character(src) &&
             identical(unname(src["first_job_current"]), "H4LM6") &&
             identical(unname(src["current_work"]), "H4LM11") &&
             identical(unname(src["current_status"]), "H4LM14"))
    }
    return(FALSE)
  }

  if (identical(family, "HoursWorked")) {
    src <- fam_cfg$sources[[as.character(wave)]]
    if (identical(wave, 4L)) {
      return(is.character(src) &&
             identical(unname(src["first_job_current"]), "H4LM6") &&
             identical(unname(src["current_work"]), "H4LM11") &&
             identical(unname(src["current_jobs"]), "H4LM12") &&
             identical(unname(src["total_hours"]), "H4LM13") &&
             identical(unname(src["primary_job_hours"]), "H4LM19") &&
             isTRUE(all.equal(as.numeric(fam_cfg$cap_hours), 120)))
    }
    return(FALSE)
  }

  if (identical(family, "PassThrough")) {
    src <- fam_cfg$source_var
    return(is.character(src) && length(src) == 1L && !is.na(src) && nzchar(src) &&
           (fam_cfg$type %||% "continuous") %in% c("continuous", "binary_single"))
  }

  FALSE
}

resolve_mortality_spec <- function(cfg, wave = NULL) {
  base <- cfg$mortality_sensitivity %||% list(enabled = FALSE)
  if (is.null(wave)) wave <- cfg$outcome$current_wave %||% NULL
  if (is.null(wave) || length(wave) != 1L || !is.finite(as.numeric(wave)))
    stop("resolve_mortality_spec requires one explicit outcome wave.", call. = FALSE)
  wave <- as.integer(wave)
  spec <- (base$wave_specs %||% list())[[as.character(wave)]]
  enabled_waves <- as.integer(base$enabled_waves %||% integer(0))
  enabled_here <- isTRUE(base$enabled %||% FALSE) && wave %in% enabled_waves
  common_names <- setdiff(names(base), c("wave_specs", "enabled_waves"))
  out <- base[common_names]
  if (!is.null(spec)) out <- utils::modifyList(out, spec)

  out$enabled <- enabled_here
  out$outcome_wave <- wave
  out
}

mortality_timing_rule_text <- function(mort) {
  mode <- as.character(mort$timing_mode %||% "fixed_window")
  if (identical(mode, "fixed_window")) {
    return(sprintf(
      "%s fixed year-window %d-%d inclusive",
      mort$source_var %||% "NDIDD19Y",
      as.integer(mort$death_year_start), as.integer(mort$death_year_end)))
  }
  if (identical(mode, "interview_month")) {
    return(sprintf(
      paste0("%s/%s month-timed before %s/%s for interviewed respondents; ",
             "no-interview deaths on or before the latest complete interview ",
             "month in the wave file are classified before the fieldwork endpoint"),
      mort$source_var %||% "NDIDD19Y",
      mort$source_month_var %||% "NDIDD19M",
      mort$interview_year_var %||% "interview_year",
      mort$interview_month_var %||% "interview_month"))
  }
  stop("Unknown mortality timing mode: ", mode, call. = FALSE)
}

mortality_enabled_for_wave <- function(cfg, wave = NULL) {
  if (is.null(wave)) wave <- cfg$outcome$current_wave %||% NULL
  if (is.null(wave) || length(wave) != 1L) return(FALSE)
  isTRUE(resolve_mortality_spec(cfg, wave)$enabled)
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

  mort_base <- cfg$mortality_sensitivity %||% list(enabled = FALSE)
  mortality_enabled <- mort_base$enabled %||% FALSE
  if (!is.logical(mortality_enabled) || length(mortality_enabled) != 1L ||
      is.na(mortality_enabled))
    stop("mortality_sensitivity$enabled must be one nonmissing logical value.", call. = FALSE)
  enabled_waves <- as.integer(mort_base$enabled_waves %||% integer(0))
  if (anyNA(enabled_waves) || any(!enabled_waves %in% c(3L, 4L)) || anyDuplicated(enabled_waves))
    stop("mortality_sensitivity$enabled_waves must contain unique Wave III/IV integers only.", call. = FALSE)
  if (isTRUE(mortality_enabled) && !setequal(enabled_waves, c(3L, 4L)))
    stop("Production mortality must be enabled for both supported outcome waves III and IV.",
         call. = FALSE)
  if (isTRUE(mortality_enabled) && length(enabled_waves)) {
    if (!is.character(cfg$paths$mortality) || length(cfg$paths$mortality) != 1L ||
        is.na(cfg$paths$mortality) || !nzchar(trimws(cfg$paths$mortality)))
      stop("Enabled mortality sensitivity requires cfg$paths$mortality.", call. = FALSE)
    if (!is.character(mort_base$source_var) || length(mort_base$source_var) != 1L ||
        is.na(mort_base$source_var) || !nzchar(trimws(mort_base$source_var)))
      stop("Enabled mortality sensitivity requires one nonblank source_var.", call. = FALSE)
    if (!is.character(mort_base$source_month_var) || length(mort_base$source_month_var) != 1L ||
        is.na(mort_base$source_month_var) || !nzchar(trimws(mort_base$source_month_var)))
      stop("Enabled mortality sensitivity requires one nonblank source_month_var.", call. = FALSE)
    month_limits <- suppressWarnings(as.integer(c(
      mort_base$valid_month_min, mort_base$valid_month_max)))
    if (length(month_limits) != 2L || anyNA(month_limits) ||
        !identical(month_limits, c(1L, 12L)))
      stop("Mortality valid month limits must be exactly 1 through 12.", call. = FALSE)
    invalid_month_codes <- suppressWarnings(as.integer(
      mort_base$invalid_month_codes %||% integer(0)))
    if (anyNA(invalid_month_codes) || anyDuplicated(invalid_month_codes) ||
        !997L %in% invalid_month_codes)
      stop("Mortality invalid_month_codes must be unique integers and include 997.", call. = FALSE)
    common_gates <- c("native_missing_means_no_death", "fail_on_unrecognized_codes",
                      "require_complete_linkage", "fail_on_death_with_observed_original_outcome",
                      "composite_zero_at_death")
    for (nm in common_gates) {
      z <- mort_base[[nm]]
      if (!is.logical(z) || length(z) != 1L || is.na(z))
        stop("Mortality gate control must be one nonmissing logical: ", nm, call. = FALSE)
    }
    for (w in enabled_waves) {
      mort <- resolve_mortality_spec(cfg, w)
      scalar_text <- c("derived_death_year_var", "derived_death_month_var",
                       "death_in_window_var", "death_before_outcome_var",
                       "timing_status_var", "linkage_audit_csv",
                       "interview_timing_audit_csv", "contradiction_audit_csv",
                       "output_csv")
      for (nm in scalar_text) {
        z <- mort[[nm]] %||% NULL
        if (!is.character(z) || length(z) != 1L || is.na(z) || !nzchar(trimws(z)))
          stop(sprintf("Enabled Wave %d mortality requires one nonblank %s.", w, nm), call. = FALSE)
      }
      yr <- as.integer(c(mort$death_year_start, mort$death_year_end,
                         mort$valid_year_min, mort$valid_year_max))
      if (length(yr) != 4L || anyNA(yr) || yr[1L] > yr[2L] ||
          yr[3L] > yr[1L] || yr[4L] < yr[2L])
        stop(sprintf("Wave %d mortality year-window and valid-year limits are inconsistent.", w), call. = FALSE)
      raw_spec <- (mort_base$wave_specs %||% list())[[as.character(w)]] %||% list()
      mode <- as.character(mort$timing_mode %||% "fixed_window")
      if (length(mode) != 1L || is.na(mode) ||
          !mode %in% c("fixed_window", "interview_month"))
        stop(sprintf("Wave %d mortality timing_mode is invalid.", w), call. = FALSE)
      expected_end <- c(`3` = 2002L, `4` = 2009L)[as.character(w)]
      if (!identical(mode, "interview_month") ||
          !identical(as.integer(mort$death_year_end), as.integer(expected_end)))
        stop(sprintf(
          "Wave %d mortality must use interview_month timing through %d.",
          w, as.integer(expected_end)), call. = FALSE)
      if (!is.null(raw_spec$timing_mode_by_family) ||
          !is.null(raw_spec$death_year_end_by_family))
        stop(sprintf(
          "Wave %d mortality must use one harmonized timing_mode and endpoint across supported outcome families.",
          w), call. = FALSE)
      mortality_roles <- canonical_role_key(c(
        cfg$analysis$id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
        cfg$analysis$weight_var, cfg$analysis$exposure_var,
        cfg$analysis$outcome_var, cfg$analysis$outcome_observed_var))
      role_vars <- unique(c(mort$source_var, mort$source_month_var,
        mort$derived_death_year_var, mort$derived_death_month_var,
        mort$death_in_window_var, mort$death_before_outcome_var,
        mort$timing_status_var, mort$interview_year_var %||% character(0),
        mort$interview_month_var %||% character(0)))
      if (anyDuplicated(canonical_role_key(role_vars)))
        stop(sprintf("Wave %d mortality role variables are not unique after canonicalization.", w),
             call. = FALSE)
      if (any(canonical_role_key(role_vars) %in% mortality_roles))
        stop("Mortality role variable conflicts with a core analysis variable.", call. = FALSE)

      if (identical(mode, "interview_month")) {
        iy <- mort$interview_year_var %||% NULL
        im <- mort$interview_month_var %||% NULL
        for (z in list(iy, im)) {
          if (!is.character(z) || length(z) != 1L || is.na(z) || !nzchar(trimws(z)))
            stop(sprintf("Wave %d interview-month mortality requires nonblank interview year/month variables.", w),
                 call. = FALSE)
        }
        iy_lim <- suppressWarnings(as.integer(c(
          mort$interview_year_valid_min, mort$interview_year_valid_max)))
        im_lim <- suppressWarnings(as.integer(c(
          mort$interview_month_valid_min, mort$interview_month_valid_max)))
        if (length(iy_lim) != 2L || anyNA(iy_lim) || iy_lim[1L] > iy_lim[2L])
          stop(sprintf("Wave %d mortality interview-year limits are invalid.", w), call. = FALSE)
        if (length(im_lim) != 2L || anyNA(im_lim) ||
            !identical(im_lim, c(1L, 12L)))
          stop(sprintf("Wave %d mortality interview-month limits must be 1 through 12.", w),
               call. = FALSE)
      }
    }
  }

  # --- Outcome-family validation ------------------------------------------
  allowed_families <- c("EducationalAttainment","LaborForceParticipation","HoursWorked","UsualHours",
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
  if (identical(waves, "all")) {
    waves <- supported_outcome_waves(cfg$outcome$family)
  }
  if (!is.numeric(waves) || any(!waves %in% 3:5))
    stop("cfg$outcome$waves must be integer(s) in 3:5 (post-exposure waves) ",
         "or the string 'all'.", call. = FALSE)

  supported <- supported_outcome_waves(cfg$outcome$family)
  if (!length(supported))
    stop(sprintf(
      "Outcome family '%s' is hard-blocked in this production program.",
      cfg$outcome$family), call. = FALSE)
  unsupported <- setdiff(as.integer(waves), supported)
  if (length(unsupported))
    stop(sprintf(
      "Outcome family '%s' is not supported at wave(s) %s; supported wave(s): %s.",
      cfg$outcome$family, paste(unsupported, collapse = ", "),
      paste(supported, collapse = ", ")), call. = FALSE)

  verified <- vapply(as.integer(waves), function(w)
    is_verified_outcome_spec(cfg, cfg$outcome$family, w), logical(1))
  if (any(!verified)) {
    bad_waves <- paste(as.integer(waves)[!verified], collapse = ", ")
    msg_txt <- sprintf(
      paste0("Outcome specification is not production-verified: family='%s', wave(s)=%s. ",
             "Only production-verified family/wave mappings and explicitly configured PassThrough outcomes ",
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
