# Generated from the reviewed v8.28 production source.
# Original lines: 15066-15584.
# Module role: Sensitivity-analysis runner.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 12) SENSITIVITY-ANALYSIS RUNNER WITH PROGRESS + RESUME
# =============================================================================
# Plain-English role: run an analyst-specified list of sensitivity-analysis
# scenarios. After each scenario finishes, append its results to a shared
# CSV and save a checkpoint. If the process is killed midway and restarted,
# previously completed scenarios are skipped. Each scenario is specified by
# a cfg-overlay (a partial list merged on top of the baseline cfg).

merge_cfg_overlay <- function(base, overlay) {
  if (is.null(overlay)) return(base)
  for (k in names(overlay)) {
    if (is.list(overlay[[k]]) && is.list(base[[k]]))
      base[[k]] <- merge_cfg_overlay(base[[k]], overlay[[k]])
    else
      base[[k]] <- overlay[[k]]
  }
  base
}

default_sensitivity_scenarios <- function() {
  list(
    S0_main = list(label = "Main specification: q0.995 cap, 260-column budget, primary Q library", overlay = list()),
    S1_budget_160 = list(
      label = "Nonprotected processed-column budget = 160 (vs 260 main)",
      overlay = list(final_tmle = list(
        rough_top_n_outcome = 60L,
        rough_top_n_missingness = 25L,
        rough_top_n_joint_AY = 30L,
        rough_top_n_exposure_for_lasso = 40L,
        rough_candidate_pool_max = 90L,
        lasso_screen_max_vars = 45L,
        lasso_screen_max_processed_cols = 160L,
        rough_max_total_vars = 80L,
        final_max_processed_columns = 160L))),
    S1b_budget_210 = list(
      label = "Nonprotected processed-column budget = 210 (vs 260 main)",
      overlay = list(final_tmle = list(
        lasso_screen_max_processed_cols = 210L,
        final_max_processed_columns = 210L))),
    S2_larger_rough_cap = list(
      label = "Expanded screening and nonprotected processed-column budget = 320 (vs 260 main)",
      overlay = list(final_tmle = list(
        rough_top_n_outcome = 160L,
        rough_top_n_missingness = 60L,
        rough_top_n_joint_AY = 90L,
        rough_top_n_exposure_for_lasso = 120L,
        rough_candidate_pool_max = 220L,
        lasso_screen_max_vars = 160L,
        lasso_screen_max_processed_cols = 300L,
        rough_max_total_vars = 180L,
        final_max_processed_columns = 320L,
        hard_max_processed_columns = 550L))),
    S2b_richer_Q = list(
      label = "Q-only richer XGBoost added to the primary learner library",
      overlay = list(learners = list(
        Q = list(use_xgboost_rich = TRUE)))),
    S2c_expanded_budget_richer_Q = list(
      label = "Joint robustness: expanded 320-column screening/budget plus richer Q-only XGBoost",
      overlay = list(
        final_tmle = list(
          rough_top_n_outcome = 160L,
          rough_top_n_missingness = 60L,
          rough_top_n_joint_AY = 90L,
          rough_top_n_exposure_for_lasso = 120L,
          rough_candidate_pool_max = 220L,
          lasso_screen_max_vars = 160L,
          lasso_screen_max_processed_cols = 300L,
          rough_max_total_vars = 180L,
          final_max_processed_columns = 320L,
          hard_max_processed_columns = 550L),
        learners = list(Q = list(use_xgboost_rich = TRUE)))),
    S3_tighter_g_pi = list(
      label = "Tighter symmetric g/pi clipping (0.05 / 0.95)",
      overlay = list(final_tmle = list(
        g_lower = 0.05, g_upper = 0.95, pi_lower = 0.05, pi_upper = 0.95))),
    S4_wider_g_pi = list(
      label = "Wider symmetric g/pi clipping (0.01 / 0.99)",
      overlay = list(final_tmle = list(
        g_lower = 0.01, g_upper = 0.99, pi_lower = 0.01, pi_upper = 0.99))),
    S6_alt_cutpoint_low = list(
      label = "Distinct exposure definition: depression cutpoint = 20 (vs 22)",
      overlay = list(
        exposure = list(cutpoint = 20),
        analysis = list(
          expected_exposure_cutpoint = 20,
          enforce_expected_treated_gate = FALSE))),
    S8_no_xgboost = list(
      label = "Final learner library without xgboost",
      overlay = list(learners = list(
        Q = list(use_xgboost = FALSE, use_xgboost_rich = FALSE),
        g = list(use_xgboost = FALSE),
        pi = list(use_xgboost = FALSE)))),
    # S10: trimmed ATE promoted to PRIMARY estimand (distinct from the default
    # run, where the trimmed ATE is reported as a secondary column). Uses the
    # primary_estimand flag so estimate/se/ci/p in the headline row point to
    # the trimmed estimate.
    S10_trim_as_primary = list(
      label = "Overlap-trimmed ATE promoted to primary estimand (g in [0.05, 0.95])",
      overlay = list(final_tmle = list(
        trim_enable = TRUE, trim_g_lower = 0.05, trim_g_upper = 0.95,
        primary_estimand = "trimmed"))),
    S11_trim_strict = list(
      label = "Overlap-trimmed ATE, strict band (g in [0.10, 0.90])",
      overlay = list(final_tmle = list(
        trim_enable = TRUE, trim_g_lower = 0.10, trim_g_upper = 0.90,
        primary_estimand = "trimmed"))),
    # ATT-specific robustness. S11b caps the upper propensity bound to
    # limit extreme control odds-weights g/(1-g) (the ATT-specific positivity
    # concern); if the ATT is stable under this cap, the control-weight extremes
    # are not driving it.
    S11b_att_gcap = list(
      label = "ATT with upper propensity bound g_upper = 0.90 (caps control odds-weights)",
      overlay = list(final_tmle = list(
        primary_estimand = "att", g_upper = 0.90))),
    S12_redundancy_loose = list(
      label = "Redundancy filter threshold 0.95 (vs 0.90 baseline)",
      overlay = list(final_tmle = list(rough_redundancy_cor_threshold = 0.95))),
    S13_redundancy_tight = list(
      label = "Redundancy filter threshold 0.75 (vs 0.90 baseline)",
      overlay = list(final_tmle = list(rough_redundancy_cor_threshold = 0.75))),
    S14_lasso_alpha_ridge = list(
      label = "LASSO screen alpha = 0.10 (more ridge-like)",
      overlay = list(final_tmle = list(lasso_screen_alpha = 0.10))),
    S15_lasso_alpha_lasso = list(
      label = "LASSO screen alpha = 1.0 (pure LASSO)",
      overlay = list(final_tmle = list(lasso_screen_alpha = 1.0))),
    # reviewer 2's tight symmetric propensity bounds, run as a
    # sensitivity scenario rather than a default. pi_upper kept at 0.999 in
    # the default; this scenario tests the fully symmetric [0.05, 0.95]
    # variant for both g and pi.
    # (point 5): the main spec now uses RAW sampling weights
    # (weight_winsor_quantile = NULL), so the old S17 (winsor = NULL) merely
    # duplicated main and S18's "vs q0.95" label was stale. Both are now real
    # contrasts against the raw-weight main.
    S17_weight_winsor_95 = list(
      label = "Distinct target weighting: sampling-weight winsorization at q0.95 (vs raw weights)",
      overlay = list(analysis = list(weight_winsor_quantile = 0.95))),
    S18_weight_winsor_99 = list(
      label = "Distinct target weighting: sampling-weight winsorization at q0.99 (vs raw weights)",
      overlay = list(analysis = list(weight_winsor_quantile = 0.99))),
    # (point 5): a TRUE full-refit earnings-cap sensitivity. Unlike the
    # post-hoc att_outcome_bound diagnostic (which holds the fitted nuisances
    # fixed), the sensitivity runner rebuilds the outcome, re-runs screening,
    # refits nuisances and recomputes the ATT under the alternative cap.
    S19_bound_refit_q0990 = list(
      label = "Distinct capped-outcome estimand: full-refit earnings cap q0.990",
      overlay = list(outcome = list(continuous_upper_quantile = 0.99))),
    S20_bound_refit_q1000 = list(
      label = "Distinct capped-outcome estimand: full-refit cap at observed outcome maximum",
      overlay = list(outcome = list(continuous_upper_quantile = 1.0)))
  )
}


# (point 4): CURATED final sensitivity set for the paper -- ONLY the
# high-value checks, not the development sweeps. Use as:
# run_sensitivity_analyses(cfg, scenarios = final_sensitivity_scenarios(
# mh_block = c(<baseline MH var names>), negative_control_outcome = "<var>"))
# F2 uses protected_W (augments the data-driven screen; does NOT replace it like
# prespecified_W). F3 requires a pre-determined negative-control outcome; VERIFY
# the overlay routes to your column -- if your outcome is family-constructed, set
# the family/source instead of analysis$outcome_var.
final_sensitivity_scenarios <- function(mh_block = NULL,
                                        negative_control_outcome = NULL,
                                        negative_control_outcome_type = NULL) {
  sc <- list(
    F0b_budget_160 = list(
      label = "Nonprotected processed-column budget = 160 (vs 260 main)",
      overlay = list(final_tmle = list(final_max_processed_columns = 160L,
                                       lasso_screen_max_processed_cols = 160L))),
    F0c_expanded_budget_320 = list(
      label = "Expanded screening and nonprotected processed-column budget = 320 (vs 260 main)",
      overlay = list(final_tmle = list(
        rough_top_n_outcome = 160L,
        rough_top_n_missingness = 60L,
        rough_top_n_joint_AY = 90L,
        rough_top_n_exposure_for_lasso = 120L,
        rough_candidate_pool_max = 220L,
        lasso_screen_max_vars = 160L,
        lasso_screen_max_processed_cols = 300L,
        rough_max_total_vars = 180L,
        final_max_processed_columns = 320L,
        hard_max_processed_columns = 550L))),
    F0d_richer_Q = list(
      label = "Q-only richer XGBoost added to the primary learner library",
      overlay = list(learners = list(Q = list(use_xgboost_rich = TRUE)))),
    F0e_expanded_budget_richer_Q = list(
      label = "Joint robustness: expanded 320-column screening/budget plus richer Q-only XGBoost",
      overlay = list(
        final_tmle = list(
          rough_top_n_outcome = 160L,
          rough_top_n_missingness = 60L,
          rough_top_n_joint_AY = 90L,
          rough_top_n_exposure_for_lasso = 120L,
          rough_candidate_pool_max = 220L,
          lasso_screen_max_vars = 160L,
          lasso_screen_max_processed_cols = 300L,
          rough_max_total_vars = 180L,
          final_max_processed_columns = 320L,
          hard_max_processed_columns = 550L),
        learners = list(Q = list(use_xgboost_rich = TRUE)))),
    F0f_combined_nuisance_balance_pi = list(
      label = "Combined nuisance sensitivity: H1FS-prioritized g elastic net plus unpenalized-A pi elastic net",
      overlay = list(learners = list(
        g = list(use_glmnet_h1fs = TRUE),
        pi = list(use_glmnet_A_unpenalized = TRUE)))),
    F1_bound_refit_q0990 = list(
      label = "Distinct capped-outcome estimand: full-refit earnings cap q0.990",
      overlay = list(outcome = list(continuous_upper_quantile = 0.99))),
    F1b_bound_refit_q1000 = list(
      label = "Distinct capped-outcome estimand: full-refit cap at observed outcome maximum",
      overlay = list(outcome = list(continuous_upper_quantile = 1.0))),
    F1c_exact_earnings_only = list(
      label = "Measurement sensitivity: exact H4EC2 earnings only; bracket-only cases treated as missing",
      overlay = list(outcome = list(compensation_exact_only = TRUE))),
    F1d_log1p_earnings = list(
      label = "Outcome sensitivity: log1p earnings, including zero earners",
      overlay = list(outcome = list(compensation_transform = "log1p",
                                    continuous_upper_quantile = 1.0))),
    F1e_asinh_earnings = list(
      label = "Outcome sensitivity: inverse-hyperbolic-sine earnings",
      overlay = list(outcome = list(compensation_transform = "asinh",
                                    continuous_upper_quantile = 1.0))),
    F1f_no_mortality_composite = list(
      label = "Outcome comparison without mortality-zero composite",
      overlay = list(mortality_sensitivity = list(
        enabled = FALSE,
        composite_zero_at_death = FALSE))),
    F2_cutpoint_20 = list(
      label = "Distinct exposure estimand: Wave-II CES-D cutpoint 20",
      overlay = list(
        exposure = list(cutpoint = 20),
        analysis = list(
          expected_exposure_cutpoint = 20,
          # The cohort, PSU, and stratum gates remain active. The treated count
          # must change under this distinct exposure definition, so only that
          # gate is deliberately disabled for this full refit.
          enforce_expected_treated_gate = FALSE)))
  )
  # ATT positivity sensitivity: the ATT control weights g/(1-g) are governed by
  # the propensity truncation, so vary g_lower/g_upper and re-report the headline.
  sc$F4_gbound_tight <- list(
    label = "Positivity: symmetric g/pi bounds [0.05, 0.95] (full pipeline refit)",
    overlay = list(final_tmle = list(g_lower = 0.05, g_upper = 0.95, pi_lower = 0.05, pi_upper = 0.95)))
  sc$F5_gbound_loose <- list(
    label = "Positivity: symmetric g/pi bounds [0.01, 0.99] (full pipeline refit)",
    overlay = list(final_tmle = list(g_lower = 0.01, g_upper = 0.99, pi_lower = 0.01, pi_upper = 0.99)))
  # Nuisance-model sensitivity: probe the double-robust remainder by dropping the
  # flexible tree learners, leaving a regularized-regression library (glmnet +
  # mean) for Q, g, and pi. Deep-merged, so other learner flags are preserved.
  sc$F6_regularized_learners <- list(
    label = "Nuisance sensitivity: mean-plus-glmnet library only for Q, g, and pi",
    overlay = list(learners = list(
      Q  = list(use_ranger = FALSE, use_xgboost = FALSE, use_xgboost_rich = FALSE, use_gam = FALSE),
      g  = list(use_ranger = FALSE, use_xgboost = FALSE, use_gam = FALSE),
      pi = list(use_ranger = FALSE, use_xgboost = FALSE, use_gam = FALSE))))
  # Screening-threshold sensitivity: vary the redundancy-cluster correlation
  # cutoff around the 0.90 baseline, so the grid actually tests the 0.90 choice.
  sc$F7_screen_threshold_loose <- list(
    label = "Screening: redundancy-cluster threshold 0.95 (looser, vs 0.90 baseline)",
    overlay = list(final_tmle = list(rough_redundancy_cor_threshold = 0.95)))
  sc$F8_screen_threshold_tight <- list(
    label = "Screening: redundancy-cluster threshold 0.75 (tighter, vs 0.90 baseline)",
    overlay = list(final_tmle = list(rough_redundancy_cor_threshold = 0.75)))
  if (!is.null(mh_block) && length(mh_block)) {
    warning("mh_block is ignored: H1FS1-H1FS19 are already mandatory protected covariates in every primary and sensitivity run.",
            call. = FALSE)
  }
  if (is.null(negative_control_outcome) && !is.null(negative_control_outcome_type))
    stop("negative_control_outcome_type was supplied without a negative_control_outcome.",
         call. = FALSE)
  if (!is.null(negative_control_outcome)) {
    if (!is.character(negative_control_outcome) || length(negative_control_outcome) != 1L ||
        is.na(negative_control_outcome) || !nzchar(trimws(negative_control_outcome)))
      stop("negative_control_outcome must be one nonblank column name.", call. = FALSE)
    nc_type <- tolower(as.character(negative_control_outcome_type %||% ""))
    if (!nc_type %in% c("continuous", "binary"))
      stop("F3_negative_control requires negative_control_outcome_type='continuous' or 'binary'.",
           call. = FALSE)
    family_type <- if (identical(nc_type, "binary")) "binary_single" else "continuous"
    sc$F3_negative_control <- list(
      label = sprintf("Negative-control outcome via PassThrough column '%s' (%s; expect approximately zero under its identifying assumptions)",
                      negative_control_outcome, nc_type),
      overlay = list(
        analysis = list(outcome_type = nc_type),
        outcome = list(
          family = "PassThrough", family_member = NULL, waves = 4L,
          log_transform = FALSE,
          families = list(PassThrough = list(
            type = family_type, source_var = negative_control_outcome))),
        mortality_sensitivity = list(enabled = FALSE,
                                     composite_zero_at_death = FALSE),
        policy = list(enable_policy_components = FALSE,
                      enable_att_prevalence_translation = FALSE),
        diagnostics = list(
          enable_mnar_pattern_mixture = FALSE,
          mnar_shift_sd_grid = numeric(0),
          enable_mnar_breakdown = FALSE,
          enable_manski_bounds = FALSE,
          enable_mnar_calibrated = FALSE,
          enable_evalue = FALSE),
        safety = list(require_publication_ready_marker = FALSE)))
  }

  sc
}


validate_sensitivity_scenarios <- function(scenarios, cfg, out_dir) {
  if (!is.list(scenarios) || !length(scenarios))
    stop("Sensitivity scenarios must be a nonempty named list.", call. = FALSE)
  nm <- names(scenarios)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(trimws(nm))) || anyDuplicated(nm))
    stop("Sensitivity scenario names must be unique and nonblank.", call. = FALSE)
  bad_path <- nm[sanitize_piece(nm) != nm]
  if (length(bad_path))
    stop("Sensitivity scenario names are not path-safe: ",
         paste(bad_path, collapse = ", "), call. = FALSE)
  rows <- lapply(seq_along(scenarios), function(i) {
    z <- scenarios[[i]]
    if (is.null(z) || !is.list(z) || !is.character(z$label) ||
        length(z$label) != 1L || is.na(z$label) || !nzchar(trimws(z$label)) ||
        is.null(z$overlay) || !is.list(z$overlay))
      stop("Scenario '", nm[i], "' must contain one nonblank label and a list-valued overlay.",
           call. = FALSE)
    merged <- merge_cfg_overlay(cfg, z$overlay)
    validate_cfg(merged)
    scenario_dir <- file.path(out_dir, paste0("sens_", nm[i]))
    assert_path_length_safe(scenario_dir, cfg, "sensitivity scenario directory")
    data.frame(scenario = nm[i], label = z$label,
               overlay_md5 = object_md5(z$overlay),
               resolved_analysis_md5 = object_md5(
                 strip_runtime_config_state(merged, analysis_only = TRUE)),
               output_directory = scenario_dir,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

run_sensitivity_analyses <- function(base_cfg,
                                     scenarios = default_sensitivity_scenarios(),
                                     out_dir = NULL) {
  base_cfg <- ensure_run_id(base_cfg)
  validate_cfg(base_cfg)
  load_required_packages(base_cfg)
  if (isTRUE(base_cfg$stages$run_preflight_unit_test))
    run_preflight_unit_test(base_cfg)
  base_cfg$stages$run_preflight_unit_test <- FALSE
  out_dir <- out_dir %||% base_cfg$global$output_dir
  ensure_output_dir(out_dir, cfg = base_cfg, test_write = TRUE)

  aggregate_cfg <- base_cfg
  aggregate_cfg$global$output_dir <- out_dir
  aggregate_cfg$global$run_label <- "sensitivity"
  aggregate_cfg <- freeze_run_provenance(aggregate_cfg)

  scenario_manifest <- validate_sensitivity_scenarios(scenarios, base_cfg, out_dir)
  message(sprintf("\n===== STAGE: Sensitivity analyses (%d scenarios) =====",
                  length(scenarios)))
  message(sprintf("  Output base directory: %s", out_dir))

  manifest_path <- file.path(out_dir, "sensitivity_scenario_manifest.csv")
  results_csv <- file.path(out_dir, "sensitivity_results.csv")
  status_csv <- file.path(out_dir, "sensitivity_status.csv")
  checkpoint <- file.path(out_dir, "sensitivity_checkpoint.rds")
  sensitivity_sig <- list(
    version = base_cfg$global$version %||% "NA",
    script = get_frozen_source_fingerprint(aggregate_cfg),
    analysis = base_cfg$analysis, exposure = base_cfg$exposure,
    outcome = base_cfg$outcome, preprocessing = base_cfg$preprocessing,
    final_preprocess = base_cfg$final_preprocess, final_tmle = base_cfg$final_tmle,
    learners = base_cfg$learners, causal_governance = base_cfg$causal_governance,
    mortality_sensitivity = base_cfg$mortality_sensitivity,
    policy = base_cfg$policy, diagnostics = base_cfg$diagnostics,
    safety = base_cfg$safety, scenarios = scenarios,
    source_files = lapply(base_cfg$paths, file_fingerprint))

  done <- character(0)
  result_blocks <- list()
  status_blocks <- list()
  resume_compatible <- FALSE
  if (file.exists(checkpoint)) {
    ck <- tryCatch(readRDS(checkpoint), error = function(e) e)
    if (inherits(ck, "error"))
      stop("Sensitivity checkpoint could not be read: ", conditionMessage(ck),
           call. = FALSE)
    if (!is.list(ck) || !identical(ck$sig, sensitivity_sig))
      stop(paste0(
        "Sensitivity checkpoint exists but its fingerprint is incompatible; ",
        "use a fresh directory."), call. = FALSE)
    if (is.null(ck$aggregate_run_id) || length(ck$aggregate_run_id) != 1L ||
        is.na(ck$aggregate_run_id) || !nzchar(as.character(ck$aggregate_run_id)))
      stop("Sensitivity checkpoint lacks its aggregate run ID; use a fresh directory.",
           call. = FALSE)
    aggregate_cfg$global$run_id <- as.character(ck$aggregate_run_id)
    aggregate_cfg <- freeze_run_provenance(aggregate_cfg)
    done <- ck$done %||% character(0)
    result_blocks <- ck$results %||% list()
    status_blocks <- ck$status %||% list()
    resume_compatible <- TRUE
    message(sprintf("  [sensitivity] Resuming: %d scenarios already done.",
                    length(done)))
  }

  if (!isTRUE(resume_compatible))
    assert_fresh_output_dir(out_dir, base_cfg)

  aggregate_files <- c(manifest_path, results_csv, status_csv)
  existing_aggregate <- aggregate_files[file.exists(aggregate_files)]
  if (length(existing_aggregate) && !isTRUE(resume_compatible))
    stop(paste0(
      "Sensitivity aggregate output(s) already exist without a compatible checkpoint: ",
      paste(basename(existing_aggregate), collapse = ", "),
      ". Use a fresh directory."), call. = FALSE)

  # Commit an empty checkpoint before the first aggregate CSV. This makes a
  # crash after manifest creation resumable instead of leaving a manifest that
  # blocks the next invocation before any scenario has completed.
  if (!isTRUE(resume_compatible))
    atomic_save_rds(
      list(sig = sensitivity_sig,
           aggregate_run_id = aggregate_cfg$global$run_id,
           done = done, results = result_blocks, status = status_blocks),
      checkpoint, overwrite = FALSE)

  write_provenance_csv_at_path(
    scenario_manifest, aggregate_cfg, manifest_path,
    "sensitivity_scenario_manifest.csv",
    overwrite = isTRUE(resume_compatible) && file.exists(manifest_path))
  if (isTRUE(resume_compatible) && length(result_blocks))
    write_provenance_csv_at_path(
      do.call(rbind, result_blocks), aggregate_cfg, results_csv,
      "sensitivity_results.csv", overwrite = file.exists(results_csv))
  if (isTRUE(resume_compatible) && length(status_blocks))
    write_provenance_csv_at_path(
      do.call(rbind, status_blocks), aggregate_cfg, status_csv,
      "sensitivity_status.csv", overwrite = file.exists(status_csv))

  for (nm in names(scenarios)) {
    if (nm %in% done) {
      message(sprintf("  [sensitivity] SKIP %s (already done).", nm))
      next
    }
    sc <- scenarios[[nm]]
    message(sprintf("\n  [sensitivity] Running scenario %s -- %s", nm, sc$label))
    t_sc <- proc.time()[3]
    cfg_k <- merge_cfg_overlay(base_cfg, sc$overlay)
    wants_h1fs <- isTRUE(sc$overlay$learners$g$use_glmnet_h1fs)
    wants_piA <- isTRUE(sc$overlay$learners$pi$use_glmnet_A_unpenalized)
    if ((wants_h1fs && !isTRUE(cfg_k$learners$g$use_glmnet_h1fs)) ||
        (wants_piA && !isTRUE(cfg_k$learners$pi$use_glmnet_A_unpenalized)))
      stop(sprintf(
        "Scenario '%s' requested dedicated nuisance learners that did not activate.",
        nm), call. = FALSE)

    scenario_dir <- file.path(out_dir, paste0("sens_", nm))
    if (dir.exists(scenario_dir)) {
      archived_incomplete <- archive_existing_path(
        scenario_dir, if (isTRUE(resume_compatible)) "INCOMPLETE_RESUME" else "STALE")
      message("  [sensitivity] Archived incomplete/stale directory: ",
              archived_incomplete)
    }
    cfg_k$global$output_dir <- scenario_dir
    cfg_k$global$run_label <- nm
    cfg_k$global$run_id <- NULL
    cfg_k$global$resume_mode <- FALSE
    cfg_k$stages$run_preflight_unit_test <- FALSE
    cfg_k$stages$run_read_wave1_phase <- TRUE
    cfg_k$stages$run_build_main_dataset_phase <- TRUE
    cfg_k$cache$use_cached_wave1 <- TRUE
    cfg_k$cache$use_cached_main_dataset <- TRUE
    validate_cfg(cfg_k)

    scenario_error <- NA_character_
    r <- tryCatch(run_addhealth_pipeline(cfg_k), error = function(e) {
      scenario_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(r) && !is.null(r$summary) && nrow(r$summary) > 0L) {
      rows <- cbind(scenario = nm, scenario_label = sc$label, r$summary)
      result_blocks[[nm]] <- rows
      done <- unique(c(done, nm))
      status_blocks[[nm]] <- data.frame(
        scenario = nm, scenario_label = sc$label, status = "success",
        elapsed_seconds = proc.time()[3] - t_sc, error_message = NA_character_,
        archived_partial_directory = NA_character_, stringsAsFactors = FALSE)
    } else {
      archived <- if (dir.exists(scenario_dir))
        archive_existing_path(scenario_dir, "FAILED") else NA_character_
      status_blocks[[nm]] <- data.frame(
        scenario = nm, scenario_label = sc$label, status = "failure",
        elapsed_seconds = proc.time()[3] - t_sc,
        error_message = if (is.na(scenario_error))
          "No successful result rows returned." else scenario_error,
        archived_partial_directory = archived,
        stringsAsFactors = FALSE)
    }

    if (length(result_blocks))
      write_provenance_csv_at_path(
        do.call(rbind, result_blocks), aggregate_cfg, results_csv,
        "sensitivity_results.csv", overwrite = file.exists(results_csv))
    write_provenance_csv_at_path(
      do.call(rbind, status_blocks), aggregate_cfg, status_csv,
      "sensitivity_status.csv", overwrite = file.exists(status_csv))
    atomic_save_rds(
      list(sig = sensitivity_sig,
           aggregate_run_id = aggregate_cfg$global$run_id,
           done = done, results = result_blocks, status = status_blocks),
      checkpoint, overwrite = TRUE)
  }

  verify_frozen_source_unchanged(aggregate_cfg)
  message("\n===== Sensitivity analyses complete =====")
  if (length(result_blocks)) do.call(rbind, result_blocks) else data.frame()
}


# =============================================================================
