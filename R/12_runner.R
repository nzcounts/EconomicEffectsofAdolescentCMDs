# Generated from the reviewed v8.28 production source.
# Original lines: 13033-13492.
# Module role: Pipeline runner and publication gate.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 10) PIPELINE RUNNER WITH CHECKPOINTING
# =============================================================================
# Plain-English role: the single entry point that walks through stages in
# order, respects the cfg$stages toggles, caches intermediate outputs as
# .rds files, writes a timing log, and calls diagnostics at the end.

# ---------------------------------------------------------------------------
# Internal: run ONE configured pipeline end-to-end.
# cfg must have cfg$outcome$current_wave set. Returns a list with main_df,
# tmle_fit, and a timers data.frame.
# ---------------------------------------------------------------------------

obtain_wave1_for_run <- function(cfg, cache_path, allow_build = TRUE,
                                 supplied = NULL) {
  if (!is.null(supplied)) {
    if (!is.data.frame(supplied)) stop("Supplied Wave-I object is not a data frame.", call. = FALSE)
    if (is.null(attr(supplied, "global_missing_dictionary")) ||
        is.null(attr(supplied, "full_survey_design_frame")) ||
        is.null(attr(supplied, "variable_source_registry")))
      stop("Supplied Wave-I object lacks required dictionary, survey-design, or source-registry attributes.",
           call. = FALSE)
    supplied_registry <- validate_variable_source_registry(
      attr(supplied, "variable_source_registry"), "supplied Wave-I source registry")
    return(supplied)
  }
  should_try_cache <- file.exists(cache_path) &&
    (isTRUE(cfg$cache$use_cached_wave1) || !isTRUE(allow_build))
  if (should_try_cache) {
    cached <- load_wave1_cache(cache_path, cfg)
    if (!is.null(cached)) return(cached)
    if (!isTRUE(allow_build))
      stop("Wave-I stage is disabled and the required cache is stale or invalid: ",
           cache_path, call. = FALSE)
  }
  if (!isTRUE(allow_build))
    stop("Wave-I stage is disabled and no valid Wave-I cache exists at: ",
         cache_path, call. = FALSE)
  w1 <- read_wave1_merged(cfg)
  if (isTRUE(cfg$cache$save_intermediate_rds)) save_wave1_cache(w1, cache_path, cfg)
  w1
}

obtain_main_dataset_for_run <- function(cfg, w1_all, cache_path,
                                        allow_build = TRUE) {
  if (is.null(w1_all) || !is.data.frame(w1_all))
    stop("Main-dataset acquisition requires valid Wave-I data.", call. = FALSE)
  should_try_cache <- file.exists(cache_path) &&
    (isTRUE(cfg$cache$use_cached_main_dataset) || !isTRUE(allow_build))
  if (should_try_cache) {
    cached <- load_main_dataset_cache(cache_path, cfg, w1_all)
    if (!is.null(cached)) return(cached)
    if (!isTRUE(allow_build))
      stop("Main-dataset stage is disabled and the required cache is stale or invalid: ",
           cache_path, call. = FALSE)
  }
  if (!isTRUE(allow_build))
    stop("Main-dataset stage is disabled and no valid main-dataset cache exists at: ",
         cache_path, call. = FALSE)
  main_df <- build_main_dataset(w1_all, cfg)
  attr(main_df, "main_dataset_cache_fingerprint") <-
    make_main_dataset_cache_fingerprint(cfg, w1_all)
  if (isTRUE(cfg$cache$save_intermediate_rds))
    save_main_dataset_cache(main_df, cache_path, cfg, w1_all)
  main_df
}

enforce_output_directory_spec <- function(cfg) {
  tag <- sanitize_piece(build_run_tag(cfg))
  existing <- list.files(cfg$global$output_dir, pattern = "^resolved_config__.*\\.rds$", full.names = TRUE)
  if (length(existing)) {
    existing <- existing[grepl(paste0("resolved_config__", tag, "__"), basename(existing), fixed = TRUE)]
  }
  if (!length(existing)) return(invisible(NULL))
  hashes <- vapply(existing, function(f) {
    z <- tryCatch(readRDS(f), error=function(e) NULL)
    if (is.null(z)) return(NA_character_)
    z$provenance$analysis_spec_md5 %||% NA_character_
  }, character(1))
  current <- get_frozen_config_hash(cfg, "analysis")
  if (any(hashes == current, na.rm = TRUE)) return(invisible(NULL))
  stop("Output directory contains the same run tag with a different analysis-specification hash; use a fresh directory.", call.=FALSE)
}

required_publication_paths <- function(cfg) {
  out_dir <- file.path(cfg$global$output_dir, cfg$diagnostics$diagnostics_dir)
  paths <- c(
    build_unique_path(cfg, cfg$final_tmle$results_csv),
    build_unique_diag_path(cfg, out_dir, "sample_flow.csv"),
    build_unique_diag_path(cfg, out_dir,
      cfg$diagnostics$analysis_sample_audit_csv %||% "analysis_sample_audit.csv"),
    build_unique_diag_path(cfg, out_dir,
      cfg$diagnostics$diagnostic_status_csv %||% "diagnostic_status.csv"))
  if (isTRUE(cfg$diagnostics$enable_mnar_breakdown %||% TRUE))
    paths <- c(paths, build_unique_diag_path(cfg, out_dir,
      cfg$diagnostics$mnar_breakdown_csv %||% "att_mnar_breakdown_point.csv"))
  if (isTRUE(cfg$diagnostics$enable_manski_bounds %||% TRUE))
    paths <- c(paths, build_unique_diag_path(cfg, out_dir,
      cfg$diagnostics$manski_bounds_csv %||% "att_fixed_nuisance_extreme_mean_bounds.csv"))
  if (isTRUE(cfg$diagnostics$enable_mnar_calibrated %||% TRUE))
    paths <- c(paths, build_unique_diag_path(cfg, out_dir,
      cfg$diagnostics$mnar_calibrated_csv %||% "att_mnar_calibrated_sensitivity.csv"))
  if (isTRUE(cfg$mortality_sensitivity$enabled %||% FALSE)) {
    paths <- c(paths,
      build_unique_path(cfg, cfg$mortality_sensitivity$linkage_audit_csv %||%
        "mortality_linkage_1997_2007_audit.csv"),
      build_unique_path(cfg, cfg$mortality_sensitivity$interview_year_audit_csv %||%
        "wave4_interview_year_audit.csv"))
    if (isTRUE(cfg$mortality_sensitivity$composite_zero_at_death %||% FALSE))
      paths <- c(paths, build_unique_path(cfg,
        cfg$mortality_sensitivity$output_csv %||%
          "mortality_composite_zero_at_death_audit.csv"))
  }
  unique(paths)
}

assert_publication_ready <- function(cfg, main_df, tmle_fit) {
  verify_frozen_source_unchanged(cfg)
  required <- required_publication_paths(cfg)
  finfo <- file.info(required)
  absent <- required[!file.exists(required) | is.na(finfo$size) | finfo$size <= 0]
  if (length(absent))
    stop("Publication-readiness gate is missing required output(s): ",
         paste(absent, collapse = ", "), call. = FALSE)

  status_path <- build_unique_diag_path(
    cfg, file.path(cfg$global$output_dir, cfg$diagnostics$diagnostics_dir),
    cfg$diagnostics$diagnostic_status_csv %||% "diagnostic_status.csv")
  status <- utils::read.csv(status_path, stringsAsFactors = FALSE,
                            comment.char = "#")
  required_labels <- character(0)
  if (isTRUE(cfg$diagnostics$enable_mnar_breakdown %||% TRUE))
    required_labels <- c(required_labels, "att MNAR breakdown point")
  if (isTRUE(cfg$diagnostics$enable_manski_bounds %||% TRUE))
    required_labels <- c(required_labels, "att fixed-nuisance extreme-mean bounds")
  if (isTRUE(cfg$diagnostics$enable_mnar_calibrated %||% TRUE))
    required_labels <- c(required_labels, "att MNAR calibrated sensitivity")
  if (length(required_labels)) {
    hit <- match(required_labels, status$diagnostic)
    if (anyNA(hit) || any(status$status[hit] != "success"))
      stop("Publication-readiness gate: one or more required MNAR diagnostics failed or are absent.",
           call. = FALSE)
  }

  rr <- tmle_fit$result
  if (is.null(rr) || any(!is.finite(c(rr$estimate[1L], rr$se[1L],
                                      rr$ci_lower[1L], rr$ci_upper[1L]))))
    stop("Publication-readiness gate: primary result is incomplete/nonfinite.",
         call. = FALSE)
  if (rr$se[1L] <= 0)
    stop("Publication-readiness gate: primary standard error is non-positive.",
         call. = FALSE)

  ratio_expected <- compensation_ratio_translation_enabled(cfg)
  ratio_fields <- c("pct_prevention_gain", "pct_prevention_gain_se",
                    "pct_prevention_gain_ci_lower", "pct_prevention_gain_ci_upper",
                    "pct_prevention_gain_p")
  ratio_vals <- vapply(ratio_fields, function(nm) {
    z <- rr[[nm]]
    if (is.null(z) || !length(z)) NA_real_ else as.numeric(z[1L])
  }, numeric(1))
  if (ratio_expected) {
    if (any(!is.finite(ratio_vals)) || ratio_vals["pct_prevention_gain_se"] <= 0)
      stop("Publication-readiness gate: primary prevention-gain percentage inference is incomplete.",
           call. = FALSE)
    primary_name <- as.character(rr$policy_primary_percentage[1L] %||% NA_character_)
    primary_est <- as.numeric(rr$policy_primary_pct_estimate[1L] %||% NA_real_)
    if (!identical(primary_name, "prevention_gain") || !isTRUE(all.equal(
        primary_est, ratio_vals["pct_prevention_gain"], tolerance = 1e-10)))
      stop("Publication-readiness gate: policy-primary percentage is not the prevention gain or is inconsistent with it.",
           call. = FALSE)
  } else if (identical(cfg$outcome$family, "Compensation") &&
             any(is.finite(ratio_vals))) {
    stop("Publication-readiness gate: a transformed Compensation outcome produced an invalid arithmetic-dollar prevention gain.",
         call. = FALSE)
  }

  if (!is.null(tmle_fit$sl_log) && nrow(tmle_fit$sl_log)) {
    fallback_cols <- intersect(c("Q_fallback", "g_fallback", "pi_fallback"),
                               names(tmle_fit$sl_log))
    if (length(fallback_cols) && any(as.matrix(tmle_fit$sl_log[fallback_cols]) %in% TRUE))
      stop("Publication-readiness gate: a nuisance fallback occurred.", call. = FALSE)
  }

  inventory <- build_output_inventory(cfg)
  inv_path <- write_run_csv(inventory, cfg,
    cfg$diagnostics$output_inventory_csv %||% "output_inventory.csv")
  inventory <- build_output_inventory(cfg)
  validate_output_bundle(cfg, inventory)
  marker <- build_unique_path(cfg,
    cfg$diagnostics$publication_ready_marker %||% "PUBLICATION_READY.txt")
  lines <- c(
    "ADD HEALTH PIPELINE PUBLICATION-READINESS GATE: PASS",
    paste0("Run ID: ", cfg$global$run_id),
    paste0("Pipeline version: ", cfg$global$version),
    paste0("Script MD5: ", get_frozen_source_fingerprint(cfg)$md5),
    paste0("Analysis spec MD5: ", get_frozen_config_hash(cfg, "analysis")),
    paste0("Prevention gain required: ", ratio_expected),
    paste0("Validated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  tf <- tempfile(pattern = ".publication_ready_", tmpdir = dirname(marker), fileext = ".tmp")
  on.exit(if (file.exists(tf)) unlink(tf, force = TRUE), add = TRUE)
  writeLines(lines, tf, useBytes = TRUE)
  atomic_commit_file(tf, marker, overwrite = FALSE)
  invisible(list(marker = marker, inventory = inv_path))
}

.run_single_pipeline <- function(cfg, w1_all_cached = NULL) {
  timers <- make_timer_log()
  timers$start("total")
  ensure_output_dir(cfg$global$output_dir, cfg = cfg, test_write = TRUE)
  assert_planned_output_paths(cfg)

  if (is.null(cfg$global$run_label) || !nzchar(trimws(as.character(cfg$global$run_label))))
    cfg$global$run_label <- "primary"
  run_tag <- build_run_tag(cfg)
  # Give this run its own checkpoint directory so different waves
  # /outcomes do not reuse each other's per-fold caches.
  cfg$global$checkpoint_subdir <- paste0(
    "checkpoints_cvtmle__", sanitize_piece(cfg$global$version), "__", run_tag)
  cfg <- freeze_run_provenance(cfg)
  enforce_output_directory_spec(cfg)

  msg(sprintf("\n############################################################"), cfg = cfg)
  msg(sprintf("  RUN TAG: %s", run_tag), cfg = cfg)
  msg(sprintf("    family       = %s", cfg$outcome$family), cfg = cfg)
  msg(sprintf("    wave         = %s", cfg$outcome$current_wave %||% "NA"), cfg = cfg)
  msg(sprintf("    family_member= %s", cfg$outcome$family_member %||% "NA"), cfg = cfg)
  msg(sprintf("    output_dir   = %s", cfg$global$output_dir), cfg = cfg)
  msg(sprintf("############################################################\n"), cfg = cfg)
  atomic_save_rds(cfg, file.path(cfg$global$output_dir,
    paste0("resolved_config__", sanitize_piece(run_tag), "__", cfg$global$run_id, ".rds")),
    overwrite = FALSE)

  wave1_path <- file.path(cfg$global$output_dir, cfg$cache$wave1_rds)
  # Main-dataset cache is also run-tag-specific, because different outcome
  # families / waves construct different main_df objects.
  main_filename <- sub("\\.rds$",
    paste0("__", run_tag, ".rds"), cfg$cache$main_dataset_rds)
  main_path <- file.path(cfg$global$output_dir, main_filename)

  raw_w1_rows <- NA_integer_

  # --- Stage: obtain Wave I and main dataset ----------------------------
  # A disabled stage means "load the validated artifact", not "leave the
  # object NULL". This makes stage switches safe for resumptions.
  timers$start("obtain_wave1")
  w1_all <- obtain_wave1_for_run(
    cfg, wave1_path,
    allow_build = isTRUE(cfg$stages$run_read_wave1_phase),
    supplied = w1_all_cached)
  timers$stop("obtain_wave1")
  raw_w1_rows <- nrow(w1_all)
  w1_dictionary <- attr(w1_all, "global_missing_dictionary")
  if (is.null(w1_dictionary) || !length(w1_dictionary))
    stop("Wave I data lack the frozen global missing-code dictionary.", call. = FALSE)
  cfg$preprocessing$global_missing_dictionary <- w1_dictionary

  timers$start("obtain_main")
  main_df <- obtain_main_dataset_for_run(
    cfg, w1_all, main_path,
    allow_build = isTRUE(cfg$stages$run_build_main_dataset_phase))
  timers$stop("obtain_main")
  global_missing_dictionary <- attr(main_df, "global_missing_dictionary")
  if (is.null(global_missing_dictionary) || !length(global_missing_dictionary))
    stop("Main dataset lacks the frozen global missing-code dictionary; rebuild caches from Wave I.", call. = FALSE)
  cfg$preprocessing$global_missing_dictionary <- global_missing_dictionary

  # --- Default stages: final TMLE performs nested data-driven screening -------
  tmle_fit <- NULL
  if (isTRUE(cfg$stages$run_final_cv_tmle))
    tmle_fit <- run_final_cv_tmle(
      cfg, main_df,
      timers = timers)

  # Print the completed estimator before optional descriptive supplements run.
  # The primary TMLE CSV is already written inside run_final_cv_tmle; therefore
  # a later optional-diagnostic failure cannot obscure whether estimation
  # completed successfully.
  if (!is.null(tmle_fit)) {
    message("\n================ RESULT SUMMARY ================")
    message(sprintf("  Run tag:      %s", run_tag))
    message(sprintf("  PRIMARY (%s) estimate: %.4f   SE: %.4f",
                    tmle_fit$result$estimand %||% "primary",
                    tmle_fit$result$estimate, tmle_fit$result$se))
    message(sprintf("  95%% CI:       [%.4f, %.4f]   p = %.4g",
                    tmle_fit$result$ci_lower, tmle_fit$result$ci_upper,
                    tmle_fit$result$p_value))
    message("================================================\n")
  }

  if (isTRUE(cfg$stages$run_diagnostics)) {
    timers$start("diagnostics")
    tryCatch(
      run_peer_review_diagnostics(
        cfg, main_df,
        prescreen_results = NULL,
        tmle_fit = tmle_fit,
        raw_w1_rows = raw_w1_rows,
        w1_all = w1_all),
      finally = timers$stop("diagnostics"))
  }

  timers$stop("total")
  write_run_csv(timers$get(), cfg, "pipeline_timings.csv")
  if (!is.null(tmle_fit)) {
    bundle_path <- build_unique_path(cfg,
      cfg$diagnostics$diagnostic_bundle_rds %||% "diagnostic_fit_bundle.rds")
    atomic_save_rds(build_diagnostic_fit_bundle(cfg, main_df, tmle_fit),
                    bundle_path, overwrite = FALSE)
    write_run_csv(build_manuscript_summary(cfg, main_df, tmle_fit), cfg,
      cfg$diagnostics$manuscript_summary_csv %||% "manuscript_summary.csv")
  }
  if (isTRUE(cfg$safety$require_publication_ready_marker %||% TRUE))
    assert_publication_ready(cfg, main_df, tmle_fit)
  verify_frozen_source_unchanged(cfg)

  invisible(list(
    run_tag  = run_tag,
    w1_all   = w1_all, main_df = main_df,
    tmle_fit = tmle_fit, timers = timers$get(),
    skipped  = FALSE))
}

# ---------------------------------------------------------------------------
# Top-level entry point: expands waves and runs .run_single_pipeline
# for each combination. Returns a list of all results, plus a combined
# results CSV that rolls every run's ATE into one table.
# ---------------------------------------------------------------------------

run_addhealth_pipeline <- function(cfg) {
  cfg <- ensure_run_id(cfg)
  validate_cfg(cfg)
  load_required_packages(cfg)
  if (isTRUE(cfg$stages$run_preflight_unit_test)) {
    run_preflight_unit_test(cfg)
    cfg$stages$run_preflight_unit_test <- FALSE
  }
  ensure_output_dir(cfg$global$output_dir, cfg = cfg, test_write = TRUE)
  assert_fresh_output_dir(cfg$global$output_dir, cfg)
  assert_planned_output_paths(cfg)

  waves <- cfg$outcome$waves
  if (identical(waves, "all")) waves <- 3:5
  waves <- as.integer(waves)
  msg(sprintf("[run_addhealth_pipeline] Will iterate over waves: %s.",
    paste(waves, collapse = ", ")), cfg = cfg)
  msg(sprintf("[run_addhealth_pipeline] Outcome family: %s.", cfg$outcome$family), cfg = cfg)

  fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
  if (identical(fam_cfg$type, "binary_nested")) {
    members <- cfg$outcome$family_member
    if (is.null(members)) {
      members <- names(fam_cfg$members)
      msg(sprintf("[run_addhealth_pipeline] family_member not set; will run all %d thresholds: %s.",
        length(members), paste(members, collapse = ", ")), cfg = cfg)
    }
  } else members <- list(NULL)

  aggregate_cfg <- cfg
  if (length(waves) == 1L) aggregate_cfg$outcome$current_wave <- waves[1L]
  if (length(members) == 1L) aggregate_cfg$outcome$family_member <- members[[1L]]
  aggregate_cfg <- freeze_run_provenance(aggregate_cfg)

  wave1_path <- file.path(cfg$global$output_dir, cfg$cache$wave1_rds)
  w1_all_shared <- obtain_wave1_for_run(
    cfg, wave1_path, allow_build = isTRUE(cfg$stages$run_read_wave1_phase))

  all_results <- list(); summary_rows <- list()
  for (w in waves) {
    for (m in members) {
      cfg_run <- cfg
      cfg_run$outcome$current_wave <- w
      cfg_run$outcome$family_member <- m
      tag_sub <- build_run_tag(cfg_run)
      cfg_run$global$output_dir <- file.path(cfg$global$output_dir, tag_sub)
      ensure_output_dir(cfg_run$global$output_dir, cfg = cfg_run, test_write = TRUE)
      assert_fresh_output_dir(cfg_run$global$output_dir, cfg_run)
      res <- tryCatch(
        .run_single_pipeline(cfg_run, w1_all_cached = w1_all_shared),
        error = function(e) stop(sprintf("Requested outcome run '%s' failed: %s",
          build_run_tag(cfg_run), conditionMessage(e)), call. = FALSE))
      all_results[[build_run_tag(cfg_run)]] <- res

      if (!isTRUE(res$skipped) && !is.null(res$tmle_fit)) {
        rr <- res$tmle_fit$result
        get1 <- function(field) if (!is.null(rr[[field]])) rr[[field]][1L] else NA
        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
          run_tag = build_run_tag(cfg_run), family = cfg_run$outcome$family,
          wave = w, family_member = m %||% NA_character_, n = rr$n,
          n_clusters = rr$n_clusters, estimate = rr$estimate, se = rr$se,
          ci_lower = rr$ci_lower, ci_upper = rr$ci_upper, p_value = rr$p_value,
          primary_estimand = if (!is.null(rr$primary_estimand)) rr$primary_estimand[1L] else "ate",
          estimand = get1("estimand"), compensation_transform = get1("compensation_transform"),
          compensation_exact_only = get1("compensation_exact_only"),
          inference_method = get1("inference_method"), inference_df = get1("inference_df"),
          cap_probability = get1("cap_probability"), cap_value = get1("cap_value"),
          cap_weighted = get1("cap_weighted"), cap_quantile_rule = get1("cap_quantile_rule"),
          n_treated = get1("n_treated"), kish_ess_overall = get1("kish_ess_overall"),
          kish_ess_treated = get1("kish_ess_treated"), kish_ess_control = get1("kish_ess_control"),
          protected_processed_columns_min = get1("protected_processed_columns_min"),
          protected_processed_columns_max = get1("protected_processed_columns_max"),
          nonprotected_processed_columns_min = get1("nonprotected_processed_columns_min"),
          nonprotected_processed_columns_max = get1("nonprotected_processed_columns_max"),
          total_processed_columns_min = get1("total_processed_columns_min"),
          total_processed_columns_max = get1("total_processed_columns_max"),
          ate_tmle_full = get1("ate_tmle"), ate_se_full = get1("ate_se_full"),
          ate_plugin_initial = get1("ate_plugin_initial"), ate_aipw_initial = get1("ate_aipw_initial"),
          att_estimate = get1("att_estimate"), att_se = get1("att_se"),
          att_mu1 = get1("att_mu1"), att_mu0 = get1("att_mu0"),
          att_mu1_earnings_depressed = get1("att_mu1_earnings_depressed"),
          att_mu0_earnings_no_depression = get1("att_mu0_earnings_no_depression"),
          pct_depression_effect = get1("pct_depression_effect"),
          pct_depression_effect_se = get1("pct_depression_effect_se"),
          pct_depression_effect_ci_lower = get1("pct_depression_effect_ci_lower"),
          pct_depression_effect_ci_upper = get1("pct_depression_effect_ci_upper"),
          pct_depression_effect_p = get1("pct_depression_effect_p"),
          pct_prevention_gain = get1("pct_prevention_gain"),
          pct_prevention_gain_se = get1("pct_prevention_gain_se"),
          pct_prevention_gain_ci_lower = get1("pct_prevention_gain_ci_lower"),
          pct_prevention_gain_ci_upper = get1("pct_prevention_gain_ci_upper"),
          pct_prevention_gain_p = get1("pct_prevention_gain_p"),
          policy_primary_percentage = get1("policy_primary_percentage"),
          policy_primary_pct_estimate = get1("policy_primary_pct_estimate"),
          policy_primary_pct_se = get1("policy_primary_pct_se"),
          policy_primary_pct_ci_lower = get1("policy_primary_pct_ci_lower"),
          policy_primary_pct_ci_upper = get1("policy_primary_pct_ci_upper"),
          policy_primary_pct_p = get1("policy_primary_pct_p"),
          natural_course_population_mean = get1("natural_course_population_mean"),
          depression_prevalence = get1("depression_prevalence"),
          att_prevalence_elasticity = get1("att_prevalence_elasticity"),
          att_prevalence_elasticity_se = get1("att_prevalence_elasticity_se"),
          att_prevalence_elasticity_ci_lower = get1("att_prevalence_elasticity_ci_lower"),
          att_prevalence_elasticity_ci_upper = get1("att_prevalence_elasticity_ci_upper"),
          gain_per_prevented_case = get1("gain_per_prevented_case"),
          gain_per_one_percentage_point_prevalence_reduction =
            get1("gain_per_one_percentage_point_prevalence_reduction"),
          att_onestep_estimate = get1("att_onestep_estimate"),
          att_tmle_eps = get1("att_tmle_eps"), att_headline_method = get1("att_headline_method"),
          att_tmle_estimate = get1("att_tmle_estimate"), att_tmle_se = get1("att_tmle_se"),
          att_G_star = get1("att_G_star"), att_se_multiplier_boot = get1("att_se_multiplier_boot"),
          ci_lower_normal = get1("ci_lower_normal"), ci_upper_normal = get1("ci_upper_normal"),
          p_normal = get1("p_normal"), att_cf_under_control = get1("att_cf_under_control"),
          att_support_ok = get1("att_support_ok"), trim_ate_estimate = get1("trim_ate_estimate"),
          trim_ate_se = get1("trim_ate_se"),
          n_trimmed = if (!is.null(rr$n_trimmed)) rr$n_trimmed[1L] else NA_integer_,
          stringsAsFactors = FALSE)
      }
    }
  }

  if (length(summary_rows)) {
    combined <- do.call(rbind, summary_rows)
    write_run_csv(combined, aggregate_cfg, "combined_tmle_results.csv")
  } else combined <- data.frame()
  verify_frozen_source_unchanged(aggregate_cfg)
  invisible(list(results = all_results, combined = combined, summary = combined))
}



# =============================================================================
