# =============================================================================
# Add Health: Adolescent Depression and Adult Economic Outcomes
# A cross-fitted, doubly-robust causal-inference pipeline (one-step / AIPW ATT)
# =============================================================================
#
# WHAT THIS DOES
#   Estimates the average effect of adolescent depression (measured at Wave II)
#   on adult economic outcomes -- earnings and labor-force participation
#   (Waves III-V) -- in the Add Health cohort. The design is observational, so
#   the estimate carries a causal interpretation only under the assumptions set
#   out in the accompanying methods notes (no unmeasured confounding given the
#   adjustment set, positivity/overlap, and consistency). The estimator is a
#   cross-fitted, doubly-robust one-step (AIPW-style) estimator of the effect of
#   treatment on the treated (ATT). Nuisance models are fit with an ensemble
#   (SuperLearner); uncertainty is quantified with cluster-robust,
#   influence-function standard errors; and estimate stability is checked across
#   multiple random seeds.
#
# HOW TO RUN
#   1. Install R (>= 4.0) and the packages named in the accompanying
#      dependency file.
#   2. In the USER CONFIGURATION block below, set the data paths to your local
#      Add Health files (see DATA ACCESS) and confirm the analysis options.
#   3. Source this file, then call:   run_addhealth_pipeline(cfg)
#      (or set  cfg$global$autorun_pipeline <- TRUE  before sourcing).
#   Results, diagnostics, figures, and tables are written to cfg$global$output_dir.
#
# DATA ACCESS
#   Add Health is restricted-use data and is NOT included with this code. No part
#   of the underlying data is reproduced or redistributed here. Qualified
#   researchers can obtain it from the Carolina Population Center
#   (https://addhealth.cpc.unc.edu) or via ICPSR/DSDR; set the paths in USER
#   CONFIGURATION to your own licensed copy.
#
# HOW THIS FILE IS ORGANISED
#   The single entry point is run_addhealth_pipeline(cfg). Reading top to bottom:
#     0) USER CONFIGURATION - every analytic choice, collected in one place.
#     - Data construction   - read Wave I, assemble the analytic dataset, build
#                             the exposure and outcome variables.
#     - Estimation          - nuisance models, cross-fitting, the ATT estimator.
#     - Inference           - cluster-robust standard errors and seed stability.
#     - Diagnostics         - overlap/positivity, covariate balance, robustness.
#     - Sensitivity         - alternative specifications reported alongside the
#                             headline estimate.
#
# LICENSE
#   See the LICENSE file in this repository. The Add Health data are not covered
#   by that license and remain subject to their own access conditions.
# =============================================================================

# =============================================================================
# 0) USER CONFIGURATION
# =============================================================================
cfg <- list(

  # ---------------------------------------------------------------------------
  # Stage switches: turn individual stages on or off for reruns.
  # ---------------------------------------------------------------------------
  stages = list(
    run_preflight_unit_test        = TRUE,
    run_read_wave1_phase           = TRUE,
    run_build_main_dataset_phase   = TRUE,
    run_final_cv_tmle              = TRUE,
    run_diagnostics                = TRUE,
    # opt-in multi-seed final inference. OFF by default because
    # it runs the FULL pipeline once per seed (multiplies runtime). When TRUE,
    # the autorun block calls run_multiseed_att() using global$multiseed_seeds.
    run_multiseed_att              = FALSE
  ),

  # ---------------------------------------------------------------------------
  # Global controls: seed, verbosity, I/O.
  # ---------------------------------------------------------------------------
  global = list(
    pipeline_seed     = 20260402L,
    output_dir        = ".",
    verbose           = TRUE,
    save_stage_csvs   = TRUE,
    autorun_pipeline  = FALSE,
    # version string participates in the multiseed checkpoint fingerprint.
    # BUMP THIS whenever you change analysis-relevant code, so a rerun in the
    # same output dir does not reuse stale per-seed results (or pass fresh=TRUE).
    version           = "v6.22_cluster_robust_multiseed",
    multiseed_seeds   = c(20260402L, 1L, 2L, 3L, 4L),  # pre-committed set for run_multiseed_att
    checkpoint_subdir = "checkpoints_cvtmle",
    # v6: a short label that tags every output file produced by this run.
    # E.g. "health_w3_edu=college" produces files like
    # "cv_tmle_results__health_w3_edu=college__<timestamp>.csv".
    run_label         = NULL,
    append_timestamp_to_outputs = TRUE
  ),

  # ---------------------------------------------------------------------------
  # Cache controls.
  # ---------------------------------------------------------------------------
  cache = list(
    use_cached_wave1        = FALSE,
    use_cached_main_dataset = FALSE,
    save_intermediate_rds   = TRUE,
    wave1_rds               = "wave1_merged.rds",
    main_dataset_rds        = "main_dataset.rds",
    # cache the main dataset with a fingerprint so a cached RDS is
    # never reused after changing the outcome family/wave, exposure cutpoint,
    # source paths, or key preprocessing settings.
    use_main_dataset_fingerprint = TRUE,
    use_wave1_fingerprint = TRUE
  ),

  # ---------------------------------------------------------------------------
  # Analysis roles (variable names after dataset build).
  # ---------------------------------------------------------------------------
  analysis = list(
    exposure_var         = "Depressed",
    outcome_var          = "Y",            # v6: generic name, populated by outcome family constructor
    exposure_type        = "auto",
    outcome_type         = "auto",
    id_var               = "AID",
    cluster_var          = "PSUSCID",
    weight_var           = "GSWGT1",
    outcome_observed_var = "delta_Y",
    # winsorize sampling weights at this upper quantile to dampen the
    # influence of very-high-weight respondents (weights span ~16 to ~6649).
    # Applied to the finalized analytic sample (after the invalid-weight
    # drop), on clean positive weights only. Set to NULL to disable.
    # DISABLED (set to NULL). Winsorizing the survey weights changes the
    # target population -- it systematically down-weights the most
    # underrepresented (high-weight) respondents, so the estimand would no longer
    # be the survey-weighted ATT for the Add Health target population. The
    # headline is the raw Add Health survey-weighted ATT. Winsorization, if
    # desired, belongs in a clearly-labeled sensitivity analysis, not the primary
    # estimand. The gate below (wq > 0 && wq < 1) skips winsorization when NULL.
    weight_winsor_quantile = NULL,
    # hard-coded transform of H1GH50 (usual bedtime), which is stored
    # as a 12-hour clock STRING ("12:59A" .. "00:00P") with sentinels
    # "999996"/"999998"/"999999". It is parsed to minutes-since-midnight and
    # encoded as a sin/cos pair (H1GH50__tsin, H1GH50__tcos) so the midnight
    # wraparound is preserved (bedtimes cluster on both sides of midnight) and
    # the spurious linear ordering of a raw clock is removed. Sentinels/
    # unparseable values become NA and are handled by the missingness
    # machinery. Set transform_time_variables = FALSE to disable.
    transform_time_variables = TRUE,
    extra_exclude_from_candidates = character(0)
  ),

  # ---------------------------------------------------------------------------
  # Data source paths.
  # ---------------------------------------------------------------------------
  paths = list(
    wave1_inhome    = "M:/AddHealth/Data/Standard/Survey/W1/allwave1.xpt",
    birth_records   = "M:/AddHealth/Data/Standard/BirthRecords/brdw5.xpt",
    neighborhood_w1 = "M:/AddHealth/Data/Standard/Grouping/nhood1.xpt",
    inschool_w1     = "M:/AddHealth/Data/Standard/InSchool/Inschool.xpt",
    contextual_w1   = "M:/AddHealth/Data/Standard/Contextual/W1/Context1.xpt",
    health_w1       = "M:/AddHealth/Data/Standard/Contextual/W1/Health1.xpt",
    spatial_w1      = "M:/AddHealth/Data/Standard/Contextual/W1/Spatial.xpt",
    stchr95_w1      = "M:/AddHealth/Data/Standard/Contextual/W1/stchr95.xpt",
    polcon_w1       = "M:/AddHealth/Data/Standard/Contextual/W1/w1polcon.xpt",
    weights_w1      = "M:/AddHealth/Data/Standard/Weights/Homewt1.xpt",
    school_admin_w1 = "M:/AddHealth/Data/Standard/InSchool/Schadm1.xpt",
    wave2_inhome    = "M:/AddHealth/Data/Standard/Survey/W2/wave2.xpt",
    wave3_inhome    = "M:/AddHealth/Data/Standard/Survey/W3/wave3.xpt",
    wave4_inhome    = "M:/AddHealth/Data/Standard/Survey/W4/wave4.xpt",
    wave5_inhome    = "M:/AddHealth/Data/Standard/Survey/W5/wave5.xpt"
  ),
  # ---------------------------------------------------------------------------
  # Exposure: CES-D -> binary Depressed. Unchanged across all outcomes/waves.
  # ---------------------------------------------------------------------------
  exposure = list(
    cesd_items            = paste0("H2FS", 1:19),
    reverse_score_items   = c("H2FS4", "H2FS8", "H2FS11", "H2FS15"),
    nonresponse_codes     = c(6, 8, 9),
    cutpoint              = 22,
    drop_missing_exposure = TRUE,
    drop_from_candidates  = c("MHSum", paste0("H2FS", 1:19))
  ),

  # ---------------------------------------------------------------------------
  # OUTCOME FAMILY SYSTEM (v6)
  # ---------------------------------------------------------------------------
  # - `family`        : one of the seven outcome families.
  # - `family_member` : for families with multiple nested binary thresholds
  #                     (EducationalAttainment, HealthStatus), which
  #                     threshold to use. NULL for single-outcome families.
  # - `waves`         : integer vector of waves to run (subset of 1..4),
  #                     or the string "all" for all four.
  # - `families`      : per-family configuration, including per-wave source
  #                     variable names and threshold definitions.
  # ---------------------------------------------------------------------------
  outcome = list(
    family        = "Compensation",  # primary toggle: select the outcome family
    family_member = NULL,            # used only for nested-binary families
    waves         = 4L,              # integer, integer vector, or "all"
    # hard stop if the selected outcome constructor is still a
    # placeholder and returns all missing values.
    stop_on_all_missing_outcome = TRUE,

    # Shared transform controls (used by families that produce a continuous Y)
    log_transform              = TRUE,
    continuous_upper_quantile  = 0.99,
    continuous_bound_eps       = 0.001,
    drop_from_candidates       = character(0),   # family constructors may add to this

    # Family definitions (per-wave source variables and any recoding maps)
    families = list(

      # ----- Educational Attainment -----
      # Nested binary outcomes at four thresholds.
      # PLACEHOLDER: fill in wave-specific source variable names below.
      EducationalAttainment = list(
        type    = "binary_nested",
        members = list(
          at_least_hs           = list(label = "At least HS graduation"),
          at_least_some_college = list(label = "At least some college"),
          at_least_college_grad = list(label = "At least college graduation"),
          some_grad_school      = list(label = "Some graduate school")
        ),
        # Per-wave source variables for outcome waves 3, 4, 5.
        # PLACEHOLDER: replace with the correct Add Health item names.
        sources = list(
          "3" = "H3ED1",   # example; replace with correct item
          "4" = "H4ED2",   # example; replace with correct item
          "5" = "H5ED1"    # example; PLACEHOLDER for Wave 5
        ),
        # per-wave drop list. Each wave's outcome source is excluded
        # from W only when that wave is the outcome wave; other waves'
        # source vars remain available as confounders.
        drop_from_candidates_by_wave = list(
          "3" = "H3ED1",
          "4" = "H4ED2",
          "5" = "H5ED1"
        ),
        # Constructor: see construct_outcome_educational_attainment() below.
        drop_from_candidates = character(0)
      ),

      # ----- Labor Force Participation -----
      # Binary: currently working (for pay) vs not.
      LaborForceParticipation = list(
        type    = "binary_single",
        members = NULL,
        sources = list(
          "3" = "H3EC1",   # example; replace with correct item
          "4" = "H4EC1",   # example; replace with correct item
          "5" = "H5EC1"    # example; PLACEHOLDER for Wave 5
        ),
        drop_from_candidates_by_wave = list(
          "3" = "H3EC1", "4" = "H4EC1", "5" = "H5EC1"
        ),
        drop_from_candidates = character(0)
      ),

      # ----- Usual Hours -----
      # Continuous: usual hours worked per week.
      UsualHours = list(
        type    = "continuous",
        members = NULL,
        sources = list(
          "3" = NULL,            # PLACEHOLDER: not measured at Wave 3?
          "4" = "H4EC4",         # example; replace with correct item
          "5" = "H5EC4"          # example; PLACEHOLDER for Wave 5
        ),
        drop_from_candidates_by_wave = list(
          "3" = character(0), "4" = "H4EC4", "5" = "H5EC4"
        ),
        drop_from_candidates = character(0)
      ),

      # ----- Compensation (earnings) -----
      # Continuous, log-transformed. Wave 4 only in Add Health.
      Compensation = list(
        type    = "continuous",
        members = NULL,
        sources = list(
          "3" = NULL,                                                 # PLACEHOLDER: not measured at Wave 3?
          "4" = list(exact_var = "H4EC2", bracket_var = "H4EC3"),
          "5" = list(exact_var = "H5EC2", bracket_var = "H5EC3")      # PLACEHOLDER for Wave 5
        ),
        exact_valid_upper = 9999996,
        bracket_map = c(`1` = 2500, `2` = 7500, `3` = 12500, `4` = 17500,
                        `5` = 22500, `6` = 27500, `7` = 35000, `8` = 45000,
                        `9` = 62500, `10` = 87500, `11` = 125000, `12` = 175000),
        earnings_floor_for_log = 0.1,
        drop_from_candidates_by_wave = list(
          "3" = character(0),
          "4" = c("Earnings", "H4EC2", "H4EC3"),
          "5" = c("Earnings", "H5EC2", "H5EC3")
        ),
        drop_from_candidates = c("Earnings", "H4EC2", "H4EC3")   # legacy fallback
      ),

      # ----- Health Status -----
      # Nested binary outcomes on self-rated health.
      HealthStatus = list(
        type    = "binary_nested",
        members = list(
          at_least_fair      = list(label = "At least fair health"),
          at_least_good      = list(label = "At least good health"),
          at_least_very_good = list(label = "At least very good health"),
          excellent          = list(label = "Excellent health")
        ),
        sources = list(
          "3" = "H3GH1",         # example; replace with correct item
          "4" = "H4GH1",         # example; replace with correct item
          "5" = "H5GH1"          # PLACEHOLDER for Wave 5
        ),
        drop_from_candidates_by_wave = list(
          "3" = "H3GH1", "4" = "H4GH1", "5" = "H5GH1"
        ),
        drop_from_candidates = character(0)
      ),

      # ----- Mental Health -----
      # PLACEHOLDER: fill in per-wave source variables and recoding rules.
      MentalHealth = list(
        type    = "continuous",  # change to "binary_single" if using a dichotomous measure
        members = NULL,
        sources = list(
          "3" = NULL, "4" = NULL, "5" = NULL  # PLACEHOLDER: fill in per-wave items
        ),
        # PLACEHOLDER: when source vars are filled in, mirror them here.
        drop_from_candidates_by_wave = list(
          "3" = character(0), "4" = character(0), "5" = character(0)
        ),
        drop_from_candidates = character(0)
      ),

      # ----- Substance Use -----
      SubstanceUse = list(
        type    = "binary_single",
        members = NULL,
        sources = list(
          "3" = NULL, "4" = NULL, "5" = NULL  # PLACEHOLDER: fill in per-wave items
        ),
        drop_from_candidates_by_wave = list(
          "3" = character(0), "4" = character(0), "5" = character(0)
        ),
        drop_from_candidates = character(0)
      )
    )  # end families
  ),

  # ---------------------------------------------------------------------------
  # Preprocessing.
  # ---------------------------------------------------------------------------
  preprocessing = list(
    factor_unique_threshold = 10L,
    bad_codes_high          = c(96, 97, 98, 99),
    bad_codes_low           = c(6, 8, 9),
    numeric_missing_scheme  = "dual_indicators",
    numeric_imputation      = "median",
    factor_missing_label    = "Missing",
    factor_other_label      = "_Other_",

    # factor missingness scheme.
    # Add Health uses two coding patterns for factor missingness depending
    # on the variable's value range:
    #   - "low" scheme:  missingness codes are {6, 7, 8, 9}, used when the
    #                    variable's highest substantive level number is < 6.
    #   - "high" scheme: missingness codes are {96, 97, 98, 99}, used when
    #                    the variable's highest substantive level number is >= 6.
    # The pipeline auto-detects which scheme applies to each factor.
    factor_skip_code_low    = 7L,    # "legitimate skip" in low scheme
    factor_refusal_codes_low  = c(6L, 8L, 9L),
    factor_skip_code_high   = 97L,   # "legitimate skip" in high scheme
    factor_refusal_codes_high = c(96L, 98L, 99L),
    factor_max_substantive_level_threshold = 5L,   # boundary between schemes

    # Special-code retention for factors. If a "skip" code (7 in low, 97 in
    # high) appears in the column at high enough frequency, it is preserved
    # as its own factor level (called factor_skip_label) instead of being
    # collapsed into the generic "Missing" bucket. This keeps the
    # information that "this respondent was legitimately skipped" available
    # to the model without exploding the design matrix with rare-code
    # dummies.
    # `factor_special_code_min_n`     : skip code must appear in at least
    #                                   this many rows to be retained.
    # `factor_special_code_min_prop`  : OR at least this fraction of rows
    #                                   (whichever criterion fires first).
    factor_skip_label                = "Skip",
    factor_special_code_min_n        = 30L,
    factor_special_code_min_prop     = 0.02,

    remove_duplicate_y_suffix_columns = TRUE,
    constant_variance_tol   = 1e-10,
    scale_eps               = 1e-12,
    sanitize_column_names_for_model_matrix = TRUE,
    # v6 Fix C: drop rows with invalid sampling weights at dataset-build time
    # so every downstream stage sees the same analytic sample.
    drop_invalid_weights_at_build = TRUE,
    long_factors = c(
      "H1HR3A","H1HR5A","H1HR13","H1GI12","H1NM4","H1NF4","H1RM1","H1RM3","H1RM4",
      "H1RF1","H1RF3","H1RF4","H1RI5A_1","H1RI5A_2","H1RI5A_3","H1RI15_1","H1RI29A1",
      "H1RI29B1","H1RI29C1","H1RI15_2","H1RI15_3","H1RX5A_1","H1RX15_1","H1RX5A_2",
      "H1RX15_2","H1RX5A_3","H1RX15_3","H1RE1","PA12","PA22","PB7","PB8","COMMID.x",
      "COMMID.y","H1TO37","H1TO34","H1TO30","H1TO2","H1TO40","H1FV14M","H1FV14Y"
    ),
    force_factor_prefixes = c("H1HR6")
  ),

  # ---------------------------------------------------------------------------
  # Rough-screen helper settings used by nested fold-specific screening and diagnostics.
  # ---------------------------------------------------------------------------
  rough_prescreen = list(
    seed                    = NULL,
    # Rough-screen internal CV folds. 2 is ~1/3 faster than 3 and ranks the
    # strong signals near-identically (the screen is a filter, not the final
    # estimator). NOTE: fewer folds => sparser per-fold designs => more
    # constant/duplicate dummies => more rank-deficiency downstream. That is
    # now handled (SL.glm dropped from Q; ridge screen has maxit=1e6), so 2
    # is safe. Left at 3 as a conservative default; set 2 to reduce runtime.
    folds                   = 3L,
    binomial_eps            = 1e-15,
    # iteration / convergence controls for the per-variable ridge
    # logistic fit in screen_binom_linear. The previous hard-coded maxit=10000
    # caused "Convergence not reached / empty model" warnings on sparse-factor
    # variables at the 13,500-row scale. 1e6 removes the iteration ceiling as a
    # practical constraint; the ridge penalty keeps each fit well-posed.
    glmnet_maxit            = 1000000L,
    glmnet_thresh           = 1e-5,
    # ridge penalty for the per-variable screen fit. Raised from the
    # old hard-coded 0.01 because maxit=1e6 still failed ~75 fits/fold: the
    # cause is ill-posed fits (separation/collinearity on rare indicators),
    # not iteration budget. A strong ridge makes each fit strongly convex and
    # convergent. For a ranking screen the exact value is immaterial.
    ridge_lambda            = 1.0,
    create_plots            = FALSE,
    save_plots_to_file      = FALSE,
    cutoff_rule             = "topk",
    min_score               = 0,
    max_keep                = 120L,
    cluster_aware_folds     = TRUE,
    plot_exposure_png       = "rough_prescreen_exposure_knee.png",
    plot_outcome_png        = "rough_prescreen_outcome_knee.png"
  ),

  # ---------------------------------------------------------------------------
  # Final W preprocessing after nested rough screening.
  # These settings are used only to build the numeric model matrix for Q/g/pi.
  # They are intentionally conservative for a small, high-collinearity dataset.
  # ---------------------------------------------------------------------------
  final_preprocess = list(
    rare_level_min_n                  = 25L,
    # for treatment/missingness designs, factor levels with too few
    # exposed observations in the training fold are collapsed to _Other_.
    # This reduces separation and tiny-cell propensity instability.
    factor_min_exposed_per_level      = 20L,
    factor_max_levels_after_collapse  = 6L,
    numeric_min_observed_n            = 40L,
    numeric_min_observed_prop         = 0.10,
    winsor_probs                      = c(0.01, 0.99)
  ),

  # ---------------------------------------------------------------------------
  # Final CV-TMLE.
  # ---------------------------------------------------------------------------
  final_tmle = list(
    vfolds                       = 5L,
    internal_superlearner_folds  = 3L,
    cluster_aware_internal_cv    = TRUE,
    outer_fold_balance_on_weights = TRUE,
    internal_fold_balance_on_weights = TRUE,

    # nested screening inside each final TMLE outer fold.
    # The rough screen is a broad first pass, not a causal variable selector.
    # Exposure-only predictors are NOT force-selected because strong A-only
    # predictors may behave like instruments. Candidates are driven by outcome
    # prediction, outcome-observation prediction, and joint A/Y ranking.
    nested_rough_prescreen_in_final_cv = TRUE,
    # rough-screen internal CV folds 3 -> 2. The rough screen is a
    # ranking filter, not the final estimator; 2-fold ranks the strong signals
    # near-identically to 3-fold and is ~1/3 faster, which offsets the larger
    # caps below. Robustness-neutral.
    rough_folds                   = 2L,
    # retention caps raised for the 13,500-row regime (were sized for a
    # 1,500-row sample). A richer pool reaches the double-selection LASSO; the
    # FINAL model size is governed by the processed-column cap, so these do not
    # enlarge the expensive nuisance fits. Near-free on runtime (rough screen
    # is cheap relative to SuperLearner).
    rough_top_n_outcome           = 120L,
    rough_top_n_missingness       = 40L,
    rough_top_n_joint_AY          = 60L,
    rough_top_n_exposure_only     = 0L,

    # Exposure-predictive candidates are allowed into the nested multivariable
    # elastic-net step for double-selection, but are not directly forced into
    # final W by the marginal rough screen.
    rough_top_n_exposure_for_lasso = 90L,

    # Fully data-driven redundancy control. Variables are represented by a
    # one-dimensional empirical signature learned from the fold's training
    # rows; variables whose signatures are too correlated with already-kept
    # variables are skipped greedily. No manual domain grouping is used.
    rough_redundancy_control      = TRUE,
    # 0.75 -> 0.90. The 0.75 threshold dropped ~50-63% of the rough
    # pool, which can discard genuine confounders that merely share a dominant
    # signature direction with another variable. At 13,500 rows there are
    # enough degrees of freedom to keep more correlated variables; the
    # downstream double-selection LASSO does the real winnowing. Free on
    # runtime (a correlation comparison, not a model fit).
    rough_redundancy_cor_threshold = 0.90,
    # redundancy handling switched from a greedy, rank-order-dependent
    # "keep-first" filter to DETERMINISTIC correlation clustering. The greedy
    # filter kept whichever member of a correlated cluster ranked first, but the
    # rank came from fold-dependent screening scores, so a different seed kept a
    # different cluster member -> the dominant source of seed churn in the ATT.
    # Clustering assigns variables to clusters by their correlation structure
    # (order-independent, seed-independent) and picks a deterministic
    # representative per cluster, eliminating that churn. The cor threshold
    # above is reused as the clustering cut (a cluster is a set of variables all
    # mutually correlated above the threshold under complete linkage). Complete
    # linkage is deliberately conservative: it only groups variables that are
    # ALL mutually highly correlated, protecting weakly-correlated substantive
    # confounders (e.g. baseline mental-health items) from being absorbed.
    redundancy_method             = "cluster",       # "cluster" or "greedy" (legacy)
    redundancy_linkage            = "complete",      # complete = conservative; protects real confounders
    cluster_dedupe_max_vars       = 6000L,           # skip pre-score clustering above this many SUBSTANTIVE candidates (runtime guard; indicators excluded from count)
    # 120 -> 180. Lets a richer pool reach the redundancy filter and
    # LASSO. Final model size is governed by the processed-column cap, not
    # this, so a larger pool does not enlarge the expensive nuisance fits.
    rough_candidate_pool_max      = 180L,

    # fold-specific, data-driven prefilter before the expensive
    # nested rough screen. This removes variables that are unusable within a
    # training fold because they are nearly all missing, constant, exact
    # duplicates, or have too little observed variation.
    rough_prefilter_enable        = TRUE,
    rough_prefilter_max_missing_prop = 0.95,
    rough_prefilter_min_observed_n   = 30L,
    rough_prefilter_min_unique       = 2L,
    rough_prefilter_min_nonmodal_n   = 3L,
    rough_prefilter_drop_exact_duplicates = TRUE,
    # estimability/positivity prefilter for (near-)binary indicators.
    # Drops indicator-like variables that lack a minimum count of present
    # cases in BOTH the exposed and unexposed arms. Targets the rare-cell
    # single-variable positivity failures (e.g. CUBAN: almost no depressed
    # cases) that drive extreme propensities and wreck weighted balance. This
    # is a uniform pre-estimation estimability rule, not an importance/balance
    # filter, so it does not bias inference. Set min_n to 0 to disable.
    # max_levels_for_two_arm_rule limits the rule to (near-)binary indicators
    # so it does not penalize legitimately rich multi-level factors.
    rough_prefilter_two_arm_min_n    = 20L,
    rough_prefilter_two_arm_max_levels = 3L,
    rough_screen_batch_size       = 500L,

    # Optional nested multivariable elastic-net screen after the rough screen.
    # This addresses the concern that univariate screening misses variables
    # that matter only jointly. It is fit only on the final fold's training
    # rows and uses double-selection logic: fit A~W, Y~W, and delta_Y~W,
    # then take the union. A predictors are not forced by the rough screen;
    # they enter only if selected by the multivariable penalized model.
    nested_lasso_after_rough      = TRUE,
    # pre-specified W override. NULL = use the data-driven nested
    # screen (default). Set to a character vector of variable names to bypass
    # selection entirely and use a fixed, theoretically-motivated confounder
    # set (used by the pre-specified-W sensitivity scenario).
    prespecified_W                = NULL,
    # v6.22 (protected_W): a character vector of variable names to (a) union into
    # the selected W and (b) exempt from the processed-column cap, WITHOUT
    # disabling the data-driven screen (unlike prespecified_W, which replaces it).
    # NULL = no-op (default; production runs unchanged). Set to the baseline
    # mental-health block names for the forced-MH-block sensitivity.
    protected_W                   = NULL,
    # alpha 0.25 -> 0.5 and maxit 1e5 -> 1e6 to fix the empty-model /
    # non-convergence failures seen at the 13,500-row scale. lambda.1se gives
    # a more stable, more parsimonious selection now that the LASSO converges.
    lasso_screen_alpha            = 0.50,
    lasso_screen_nlambda          = 100L,
    lasso_screen_glmnet_maxit    = 1000000L,
    # lambda.min (was lambda.1se). In CAUSAL screening, a false negative
    # (dropping a true confounder that is a weak outcome predictor) biases the
    # estimate, whereas a false positive (keeping an irrelevant variable) only
    # costs a little efficiency. lambda.1se is more parsimonious but more likely
    # to drop weak-but-real confounders -- the opposite of what double-selection
    # is meant to protect. lambda.min is the more inclusive, confounding-safe
    # choice and is used as primary.
    lasso_screen_lambda_choice    = "lambda.min",
    lasso_screen_folds            = 5L,
    lasso_screen_include_delta    = TRUE,
    lasso_screen_min_vars         = 30L,
    lasso_screen_max_vars         = 130L,
    lasso_screen_max_processed_cols = 240L,

    # scaled up for the 13,500-row regime. Rough pool feeds the
    # double-selection LASSO, which trims to lasso_screen_max_vars (130);
    # effective per-fold cap = min(rough_max_total_vars, lasso_screen_max_vars).
    rough_max_total_vars          = 150L,
    # 220 -> 260 processed columns. At ~1,000 treated training rows this
    # is ~4 events/column, still within regularized-estimation stability, and
    # buys more complete confounder adjustment. Column count affects the
    # regularized/tree nuisance fits sub-linearly, so the runtime impact is
    # well under the proportional column increase.
    final_max_processed_columns  = 260L,
    use_epp_cap                  = TRUE,
    events_per_parameter         = 5L,
    epp_cap_floor                = 40L,
    rough_min_total_vars          = 10L,
    nested_rough_selection_log_csv = "nested_rough_selection_log.csv",
    cluster_assignment_log_csv = "cluster_assignments.csv",

    min_treated_warning_n        = 30L,
    min_observed_treated_warning_n = 20L,
    internal_fold_min_treated_warning_n = 8L,
    internal_fold_min_observed_treated_warning_n = 6L,
    min_ess_treated_train_warning = 20,
    min_ess_treated_valid_warning = 10,
    fail_on_nuisance_fallback = FALSE,
    g_lower                      = 0.025,
    g_upper                      = 0.975,
    pi_lower                     = 0.025,
    pi_upper                     = 0.999,
    positivity_warning_threshold = 0.05,
    positivity_warning_fraction  = 0.10,

    # positivity remediation. The 13,500-row run showed ~50% of rows
    # with g*pi1 < 0.05 (treated-arm non-overlap). Two diagnostics are added:
    #   (1) report the ATT alongside the ATE. The ATT only needs overlap
    #       where treated units exist and is far better identified at 9%
    #       prevalence.
    #   (2) optionally trim to a common-support propensity band and report
    #       the trimmed-sample ATE. Rows with g outside [lo, hi] are dropped
    #       from the targeted means and the EIF.
    # DEFAULT HEADLINE: the full-sample ATE (primary_estimand = "ate"). The
    # ATT and trimmed ATE are reported as SECONDARY columns unless you
    # deliberately set primary_estimand = "trimmed" or "att". Promoting either
    # changes the ESTIMAND reported in the headline -- state it in the methods.
    report_att                   = TRUE,
    trim_enable                  = TRUE,
    trim_g_lower                 = 0.05,
    trim_g_upper                 = 0.95,
    # which estimand is the HEADLINE (estimate/se/ci/p in the main
    # result row and console summary). "ate" = full-sample ATE (default, no
    # behavior change); "trimmed" = overlap-trimmed ATE; "att" = ATT. The
    # other estimands remain available as columns regardless of this choice.
    # Choosing "trimmed" or "att" changes the ESTIMAND being reported as the
    # headline -- make this choice deliberately and state it in the methods.
    # headline set to the ATT. The research question concerns an
    # intervention that prevents/remits depression among those who have it, so
    # the policy-relevant population is the treated (the depressed). The ATT is
    # (a) the estimand matching that population, (b) well-identified without a
    # trim band (it needs only that treated units have comparable controls,
    # which holds; the non-overlap is on the treated side), and (c) free of the
    # band-selection criticism that applies to the trimmed ATE. Its SE is the
    # cluster-robust EIF (sandwich) SE computed in the ATT block, with built-in
    # centering and no-censoring self-checks. The full ATE, trimmed ATE, ATT,
    # plug-in, and AIPW all remain reported as columns; the trimmed ATE and the
    # band-sensitivity scenarios become supporting robustness analyses.
    primary_estimand             = "att",
    # when TRUE, the trimmed ATE re-fits the TMLE fluctuation on the
    # in-support rows so it is a formally targeted estimate of the trimmed
    # estimand, rather than reusing the full-sample epsilon. Recommended TRUE
    # whenever the trimmed ATE is used as anything more than a rough check.
    retarget_trimmed             = TRUE,

    # Bounds applied to held-out Q predictions before logit targeting.
    Q_pred_lower                 = 0.005,
    Q_pred_upper                 = 0.995,

    use_fold_checkpoints         = TRUE,
    gc_after_fold                = TRUE,
    hard_max_processed_columns   = 300L,
    results_csv                  = "cv_tmle_results.csv",
    cluster_eic_rds              = "cluster_eic.rds",
    per_fold_sl_log_csv          = "per_fold_sl_log.csv",
    overlap_diagnostics_csv      = "overlap_diagnostics.csv",
    candidate_prefilter_log_csv  = "candidate_prefilter_log.csv",
    fold_support_log_csv         = "fold_support_log.csv",
    internal_fold_support_log_csv = "internal_fold_support_log.csv"
  ),

  # ---------------------------------------------------------------------------
  # Learner libraries. Toggles removed: allow_non_weight_aware, enable_plotting.
  # ---------------------------------------------------------------------------
  learners = list(
    # use_glm set FALSE for Q. Stock SL.glm fits an unregularized glm
    # on the full dummy-expanded W; on rank-deficient per-fold designs (more
    # common after reducing rough-screen folds, which shrinks each fold's
    # sample and leaves constant/duplicated dummies) predict.glm emits
    # "prediction from rank-deficient fit" warnings and can yield unstable
    # predictions. SL.glmnet.fixed (elastic net) is the regularized
    # replacement and cannot be rank-deficient. g and pi already excluded glm.
    Q  = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_ranger = TRUE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    g  = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_ranger = TRUE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    pi = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_ranger = TRUE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    # alpha = 0.5 (elastic net) actually takes effect via SL.glmnet.fixed.
    # Stock SL.glmnet ignores alpha; the wrapper passes it through.
    # lambda_choice: "min" (CV-minimum, more permissive) or "1se" (1-SE rule).
    glmnet  = list(alpha = 0.5, nlambda = 100L, lambda_choice = "min", maxit = 100000L),
    ranger  = list(num.trees = 500L),
    xgboost = list(ntrees = 150L, max_depth = 2L, shrinkage = 0.05,
                   minobspernode = 20L),
    # earth (MARS) MUST stay off at this scale. On a 13,500-row x
    # ~200-column dummy-heavy design its forward-pass knot search ran 35-88
    # HOURS per fold and dominated total runtime (~330h for one scenario).
    # Do not set learners$Q$use_earth = TRUE on the full sample.
    earth   = list(degree = NULL, nprune = NULL),
    gam     = list(df = NULL),
    svm     = list(cost = 1, gamma = NULL, kernel = "radial"),
    nnet    = list(size = 5, decay = 0.0001, maxit = 200, MaxNWts = 10000)
  ),

  # ---------------------------------------------------------------------------
  # Diagnostics.
  # ---------------------------------------------------------------------------
  diagnostics = list(
    enable                    = TRUE,
    save_plots                = TRUE,
    save_csvs                 = TRUE,
    plot_selection            = TRUE,
    plot_propensity           = TRUE,
    plot_missingness          = TRUE,
    plot_outcome_distribution = TRUE,
    plot_fold_times           = TRUE,
    plot_overlap_product      = TRUE,
    plot_qq_eic               = TRUE,
    diagnostics_dir           = "diagnostics",
    variable_consistency_csv = "variable_selection_consistency.csv",
    learner_weight_summary_csv = "learner_weight_summary.csv",
    learner_weight_long_csv = "learner_weight_long.csv",
    per_fold_ate_csv = "per_fold_ate_diagnostics.csv",
    effective_sample_size_csv = "effective_sample_size.csv",
    top_cluster_influence_csv = "top_cluster_influence.csv",
    positivity_summary_csv = "positivity_summary.csv",
    balance_treatment_csv = "balance_treatment_loveplot_data.csv",
    balance_missingness_csv = "balance_missingness_loveplot_data.csv",
    # trimmed-sample treatment balance (the trimmed estimand's balance).
    balance_on_trimmed = TRUE,
    balance_treatment_trimmed_csv = "balance_treatment_trimmed_loveplot_data.csv",
    # ATT-weighted balance (the headline estimand's balance) and the
    # ATT control-weight / treated-side positivity diagnostic.
    balance_att = TRUE,
    balance_treatment_att_csv = "balance_treatment_att_loveplot_data.csv",
    att_positivity_csv = "att_positivity_control_weights.csv",
    evalue_csv = "evalue_sensitivity.csv",
    candidate_prefilter_log_csv = "candidate_prefilter_log.csv",
    fold_support_log_csv = "fold_support_log.csv",
    internal_fold_support_log_csv = "internal_fold_support_log.csv",
    selection_jaccard_csv = "selection_jaccard.csv",
    seed_stability_csv = "seed_stability.csv",
    cluster_influence_leaveout_csv = "cluster_influence_leaveout.csv",
    # ATT-specific diagnostics (all derived from the canonical per-row
    # ATT influence function, consistent with the headline estimate), the
    # outcome-bound sensitivity, and a per-fold/per-outcome run manifest.
    att_per_fold_csv               = "att_per_fold_diagnostics.csv",
    att_cluster_influence_all_csv  = "att_cluster_influence_all.csv",
    att_top_cluster_influence_csv  = "top_att_cluster_influence.csv",
    att_cluster_influence_leaveout_csv = "att_cluster_influence_leaveout.csv",
    att_estimator_decomposition_csv = "att_estimator_decomposition.csv",
    att_eif_diagnostics_csv        = "att_eif_diagnostics.csv",
    att_weighted_control_ess_by_fold_csv = "att_weighted_control_ess_by_fold.csv",
    att_pi_positivity_calibration_csv = "att_pi_positivity_calibration.csv",
    att_outcome_bound_sensitivity_csv = "att_outcome_bound_sensitivity.csv",
    att_outcome_bound_quantiles    = c(0.95, 0.975, 0.98, 0.99, 0.995),
    att_leaveout_top_k             = c(1L, 3L, 5L),
    run_manifest_csv               = "run_manifest.csv",
    max_balance_variables = 1000L,
    max_love_plot_variables = 30L,
    save_full_balance_tables = TRUE
  ),

  # ---------------------------------------------------------------------------
  # Safety. Toggle removed: max_candidate_vars_warning.
  # ---------------------------------------------------------------------------
  safety = list(
    stop_if_no_learners           = TRUE,
    max_processed_columns_warning = 20000L,
    strict_model_matrix_row_checks = TRUE,
    allow_placeholder_outcomes = FALSE,
    stop_on_stale_wave1_cache = FALSE
  )
)

# =============================================================================
# 1) PACKAGE LOADING AND CONFIG VALIDATION
# =============================================================================
# Plain-English role: Make sure the packages needed for the enabled stages
# are installed, then attach them. Validate the cfg object so that impossible
# settings (e.g., no learners enabled, or no screening reaches the final
# estimator) stop the pipeline loudly before any expensive work begins.

`%||%` <- function(x, y) if (is.null(x)) y else x

count_enabled_learners <- function(lib_cfg) {
  toggles <- c("use_mean","use_glm","use_glmnet","use_ranger",
               "use_xgboost","use_earth","use_gam","use_svm","use_nnet")
  sum(vapply(intersect(toggles, names(lib_cfg)),
             function(nm) isTRUE(lib_cfg[[nm]]), logical(1)))
}

validate_cfg <- function(cfg) {
  # --- Rough-screen helper settings -----------------------------------------
  if (!cfg$rough_prescreen$cutoff_rule %in% c("positive","knee","topk"))
    stop("rough_prescreen$cutoff_rule must be one of 'positive', 'knee', 'topk'.", call. = FALSE)
  if (cfg$rough_prescreen$folds < 2L || cfg$final_tmle$vfolds < 2L ||
      cfg$final_tmle$internal_superlearner_folds < 2L ||
      cfg$final_tmle$rough_folds < 2L)
    stop("All rough/final fold counts must be at least 2.", call. = FALSE)

  # --- Final TMLE learner libraries ---------------------------------------
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
      (cfg$final_tmle$lasso_screen_folds %||% 0L) < 2L)
    stop("lasso_screen_folds must be at least 2 when nested_lasso_after_rough=TRUE.", call. = FALSE)

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
  invisible(TRUE)
}

load_required_packages <- function(cfg) {
  required <- c("haven","dplyr","purrr")
  if (isTRUE(cfg$stages$run_final_cv_tmle))
    required <- c(required, "SuperLearner","glmnet")
  if (isTRUE(cfg$stages$run_final_cv_tmle)) {
    use_ranger <- any(vapply(cfg$learners[c("Q","g","pi")],
                             function(x) isTRUE(x$use_ranger), logical(1)))
    use_xgb <- any(vapply(cfg$learners[c("Q","g","pi")],
                          function(x) isTRUE(x$use_xgboost), logical(1)))
    use_earth <- any(vapply(cfg$learners[c("Q","g","pi")],
                            function(x) isTRUE(x$use_earth), logical(1)))
    if (use_ranger) required <- c(required, "ranger")
    if (use_xgb)    required <- c(required, "xgboost","Matrix")
    if (use_earth)  required <- c(required, "earth")
  }
  use_gam  <- any(vapply(cfg$learners[c("Q","g","pi")],
                         function(x) isTRUE(x$use_gam), logical(1)))
  use_nnet <- any(vapply(cfg$learners[c("Q","g","pi")],
                         function(x) isTRUE(x$use_nnet), logical(1)))
  use_svm  <- any(vapply(cfg$learners[c("Q","g","pi")],
                         function(x) isTRUE(x$use_svm), logical(1)))
  if (use_gam)  required <- c(required, "mgcv")
  if (use_nnet) required <- c(required, "nnet")
  if (use_svm)  required <- c(required, "e1071")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L)
    stop("Please install required package(s): ",
         paste(unique(missing_pkgs), collapse = ", "), call. = FALSE)
  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(purrr)
    if (requireNamespace("SuperLearner", quietly = TRUE)) library(SuperLearner)
    if (requireNamespace("glmnet", quietly = TRUE))       library(glmnet)
    if (requireNamespace("ranger", quietly = TRUE))       library(ranger)
    if (requireNamespace("xgboost", quietly = TRUE))      library(xgboost)
    if (requireNamespace("Matrix", quietly = TRUE))       library(Matrix)
    if (requireNamespace("earth", quietly = TRUE))        library(earth)
    if (requireNamespace("mgcv", quietly = TRUE))         library(mgcv)
    if (requireNamespace("nnet", quietly = TRUE))         library(nnet)
    if (requireNamespace("e1071", quietly = TRUE))        library(e1071)
  })
}

validate_cfg(cfg)
load_required_packages(cfg)


# =============================================================================
# 2) SMALL UTILITIES
# =============================================================================

msg <- function(..., cfg) { if (isTRUE(cfg$global$verbose)) message(...) }

seed_for <- function(cfg, offset = 0L) as.integer(cfg$global$pipeline_seed + offset)

ensure_output_dir <- function(path_dir) {
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(path_dir)
}

# v6: Build a unique, self-describing filename for an output file.
# The filename includes the base name, a tag summarizing the run (family,
# wave, family_member, run_label), and an optional timestamp so
# repeat runs never overwrite each other.
# Example: "cv_tmle_results__HealthStatus__wave3__at_least_good__2026-04-23_143205.csv"
build_run_tag <- function(cfg) {
  parts <- character(0)
  if (!is.null(cfg$outcome$family))        parts <- c(parts, cfg$outcome$family)
  if (!is.null(cfg$outcome$current_wave))  parts <- c(parts, sprintf("wave%d", cfg$outcome$current_wave))
  if (!is.null(cfg$outcome$family_member)) parts <- c(parts, cfg$outcome$family_member)
  if (!is.null(cfg$global$run_label) && nzchar(cfg$global$run_label))
    parts <- c(parts, cfg$global$run_label)
  if (length(parts) == 0L) parts <- "run"
  tag <- paste(parts, collapse = "__")
  tag <- gsub("[^A-Za-z0-9._=-]+", "_", tag)
  tag
}

build_unique_diag_path <- function(cfg, out_dir, base_filename) {
  # Same naming scheme as build_unique_path but rooted under a custom
  # subdirectory (e.g., the diagnostics folder).
  ext_match <- regexpr("\\.[A-Za-z0-9]+$", base_filename)
  if (ext_match > 0L) {
    stem <- substr(base_filename, 1L, ext_match - 1L)
    ext  <- substr(base_filename, ext_match, nchar(base_filename))
  } else {
    stem <- base_filename; ext <- ""
  }
  tag <- build_run_tag(cfg)
  ts  <- if (isTRUE(cfg$global$append_timestamp_to_outputs))
           format(Sys.time(), "%Y-%m-%d_%H%M%S") else NULL
  pieces <- c(stem, tag, ts); pieces <- pieces[nzchar(pieces)]
  fname  <- paste0(paste(pieces, collapse = "__"), ext)
  file.path(out_dir, fname)
}

build_unique_path <- function(cfg, base_filename) {
  ext_match <- regexpr("\\.[A-Za-z0-9]+$", base_filename)
  if (ext_match > 0L) {
    stem <- substr(base_filename, 1L, ext_match - 1L)
    ext  <- substr(base_filename, ext_match, nchar(base_filename))
  } else {
    stem <- base_filename; ext <- ""
  }
  tag <- build_run_tag(cfg)
  ts  <- if (isTRUE(cfg$global$append_timestamp_to_outputs))
           format(Sys.time(), "%Y-%m-%d_%H%M%S") else NULL
  pieces <- c(stem, tag, ts)
  pieces <- pieces[nzchar(pieces)]
  fname  <- paste0(paste(pieces, collapse = "__"), ext)
  file.path(cfg$global$output_dir, fname)
}

# v6: wrapper around write.csv that uses build_unique_path() and writes a
# metadata header describing the run configuration. This makes every CSV
# self-identifying when emailed or stored outside the run folder.
write_run_csv <- function(x, cfg, base_filename, row.names = FALSE) {
  path <- build_unique_path(cfg, base_filename)
  meta_lines <- c(
    sprintf("# File:          %s", basename(path)),
    sprintf("# Generated:     %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("# Base name:     %s", base_filename),
    sprintf("# Outcome:       family=%s, wave=%s, member=%s",
      cfg$outcome$family %||% "NA",
      as.character(cfg$outcome$current_wave %||% "NA"),
      cfg$outcome$family_member %||% "NA"),
    sprintf("# Run label:     %s", cfg$global$run_label %||% ""),
    sprintf("# Pipeline seed: %s", cfg$global$pipeline_seed))
  writeLines(meta_lines, con = path)
  suppressWarnings(utils::write.table(
    x, file = path, sep = ",", row.names = row.names,
    col.names = TRUE, append = TRUE, qmethod = "double"))
  invisible(path)
}

assert_file_exists <- function(path) {
  if (!file.exists(path)) stop("Required file does not exist: ", path, call. = FALSE)
}

assert_required_columns <- function(df, cols, df_name = deparse(substitute(df))) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L)
    stop(df_name, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
}

# Lightweight stopwatch used by the pipeline runner to report per-stage time.
make_timer_log <- function() {
  e <- new.env(parent = emptyenv()); e$starts <- list(); e$elapsed <- list()
  list(
    start = function(label) { e$starts[[label]] <- proc.time()[3]; invisible(NULL) },
    stop  = function(label) {
      if (is.null(e$starts[[label]])) return(invisible(NULL))
      dt <- proc.time()[3] - e$starts[[label]]
      e$elapsed[[label]] <- (e$elapsed[[label]] %||% 0) + dt
      e$starts[[label]]  <- NULL; invisible(dt)
    },
    get = function() {
      if (length(e$elapsed) == 0L)
        return(data.frame(stage = character(0), seconds = numeric(0)))
      data.frame(stage = names(e$elapsed), seconds = unlist(e$elapsed), row.names = NULL)
    }
  )
}

to_num <- function(x) {
  x_chr <- as.character(x); x_chr <- gsub("[, ]", "", x_chr)
  suppressWarnings(as.numeric(x_chr))
}

compute_simple_impute <- function(x, method = c("median","mean")) {
  method <- match.arg(method)
  x <- suppressWarnings(as.numeric(x)); x_ok <- x[is.finite(x)]
  if (!length(x_ok)) return(0)
  val <- if (method == "median") stats::median(x_ok, na.rm = TRUE) else mean(x_ok, na.rm = TRUE)
  if (!is.finite(val)) 0 else val
}

scale_numeric_train_valid <- function(train_df, valid_df) {
  common   <- intersect(names(train_df), names(valid_df))
  num_cols <- common[vapply(train_df[common], is.numeric, logical(1))]
  if (!length(num_cols)) return(list(train = train_df, valid = valid_df))
  for (nm in num_cols) {
    mu <- mean(train_df[[nm]], na.rm = TRUE); s <- stats::sd(train_df[[nm]], na.rm = TRUE)
    if (!is.finite(mu)) mu <- 0
    if (!is.finite(s) || s <= 0) s <- 1
    train_df[[nm]] <- (train_df[[nm]] - mu) / s
    valid_df[[nm]] <- (valid_df[[nm]] - mu) / s
  }
  list(train = train_df, valid = valid_df)
}

read_xpt_df <- function(path) {
  assert_file_exists(path)
  data.frame(haven::read_xpt(path), check.names = FALSE)
}

coerce_join_key <- function(df, key) {
  if (!key %in% names(df)) stop("Join key not found: ", key, call. = FALSE)
  df[[key]] <- suppressWarnings(as.numeric(as.character(df[[key]]))); df
}

merge_by_key <- function(dfs, key) {
  dfs <- lapply(dfs, coerce_join_key, key = key)
  purrr::reduce(dfs, ~ dplyr::left_join(.x, .y, by = key))
}

safe_remove_cols <- function(df, pattern) {
  idx <- grep(pattern, names(df))
  if (length(idx) > 0L) df <- df[, -idx, drop = FALSE]
  df
}

remove_constant_columns <- function(df, tol = 1e-10, verbose = TRUE) {
  if (ncol(df) == 0L) return(df)
  is_constant <- vapply(df, function(col) {
    col_clean <- col[!is.na(col)]
    if (length(col_clean) == 0L) return(TRUE)
    if (is.factor(col_clean))    return(length(unique(col_clean)) < 2L)
    if (is.numeric(col_clean))   return(stats::var(col_clean) < tol)
    length(unique(col_clean)) < 2L
  }, logical(1))
  n_const <- sum(is_constant)
  if (n_const > 0L && isTRUE(verbose))
    message(sprintf("[Safety] Removing %d constant columns", n_const))
  df[, !is_constant, drop = FALSE]
}

safe_scale <- function(x, eps = 1e-12) {
  if (!(is.numeric(x) || is.integer(x))) return(x)
  x <- as.numeric(x); x[!is.finite(x)] <- NA_real_
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s < eps) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Keep only letters, digits, and underscores in a single atomic string piece.
sanitize_piece <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "NA"
  x <- gsub("[^A-Za-z0-9]+", "_", x); x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- "blank"; x
}

# Sanitizes processed design-matrix column names. Special characters in
# column names (dots, spaces, parens) otherwise leak into SL formulas and
# produce inscrutable errors.
sanitize_column_names <- function(df) {
  clean_names <- gsub("[^A-Za-z0-9_]", "_", names(df))
  clean_names <- make.unique(clean_names)
  old_to_new  <- setNames(clean_names, names(df))
  names(df)   <- clean_names
  list(data = df, old_to_new = old_to_new)
}

# v5.1: Convert all factor columns to dummy (indicator) variables so that
# every learner in the SuperLearner library receives pure numeric input.
# Without this, as.matrix() on a mixed data.frame coerces the entire matrix
# to character, which crashes xgboost and inflates memory by ~10x.
# Factors are expanded one column at a time rather than via a single
# model.matrix(~ ., data) call, because the latter overflows R's protect()
# stack when the data.frame has hundreds of factor columns.
expand_factors_to_numeric <- function(df) {
  fac_cols <- names(df)[vapply(df, is.factor, logical(1))]
  if (length(fac_cols) == 0L) return(df)
  num_part <- df[, !names(df) %in% fac_cols, drop = FALSE]
  dummy_parts <- list()
  for (fn in fac_cols) {
    f <- df[[fn]]
    levs <- levels(f)
    if (length(levs) < 2L) next
    # Treatment coding: drop first level as reference to avoid collinearity
    # in GLM-based learners. Tree-based learners (ranger, xgboost) are
    # unaffected by the choice of reference level.
    mm_full <- stats::model.matrix(~ f, data = data.frame(f = f), na.action = stats::na.pass)
    if (nrow(mm_full) != length(f)) stop(sprintf("expand_factors_to_numeric(): factor '%s' produced %d rows but expected %d.", fn, nrow(mm_full), length(f)), call. = FALSE)
    mm <- mm_full[, -1L, drop = FALSE]
    colnames(mm) <- paste0(fn, "_", levs[-1L])
    dummy_parts[[fn]] <- mm
  }
  if (length(dummy_parts) == 0L) return(num_part)
  # v6: ensure data.frame return type. cbind(data.frame, matrix) can return
  # a matrix under R's method dispatch, which crashes earth's formula path.
  out <- cbind(num_part, do.call(cbind, dummy_parts))
  if (!is.data.frame(out)) out <- as.data.frame(out, stringsAsFactors = FALSE)
  out
}

compute_earnings <- function(h4ec2, h4ec3, cfg_outcome) {
  if (length(h4ec2) != length(h4ec3)) stop("h4ec2 and h4ec3 must have same length.", call. = FALSE)
  exact   <- suppressWarnings(as.numeric(h4ec2))
  bracket <- as.character(h4ec3)
  out <- ifelse(!is.na(exact) & exact < cfg_outcome$exact_valid_upper,
                exact, unname(cfg_outcome$bracket_map[bracket]))
  as.numeric(out)
}

detect_binary01 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(FALSE)
  if (is.logical(x))   return(TRUE)
  if (is.factor(x))    return(nlevels(droplevels(x)) == 2L)
  if (is.character(x)) return(length(unique(x)) == 2L)
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(as.numeric(x))); return(identical(ux, c(0,1)))
  }
  FALSE
}

infer_variable_type <- function(x, requested = "auto", name = "variable") {
  if (!requested %in% c("auto","binary","continuous"))
    stop("Requested type for ", name, " must be 'auto', 'binary', or 'continuous'.", call. = FALSE)
  if (requested != "auto") return(requested)
  if (detect_binary01(x)) "binary" else "continuous"
}

normalize_binary_var <- function(x, name = "variable") {
  if (anyNA(x)) stop(name, " contains missing values; binary variable must be fully observed here.", call. = FALSE)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  {
    x <- droplevels(x)
    if (nlevels(x) != 2L) stop(name, " must have exactly 2 levels if factor.", call. = FALSE)
    return(as.integer(x == levels(x)[2L]))
  }
  if (is.character(x)) {
    ux <- unique(x); ux <- ux[!is.na(ux)]
    if (length(ux) != 2L) stop(name, " must have exactly 2 distinct values if character.", call. = FALSE)
    return(as.integer(x == ux[2L]))
  }
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(as.numeric(x))); ux <- ux[!is.na(ux)]
    if (!identical(ux, c(0,1))) stop(name, " numeric values must be coded 0/1.", call. = FALSE)
    return(as.integer(x))
  }
  stop(name, " must be logical, factor, character, or numeric 0/1.", call. = FALSE)
}

normalize_binary_allow_missing <- function(x, name = "variable") {
  out <- rep(NA_integer_, length(x)); keep <- !is.na(x)
  if (!any(keep)) return(out)
  out[keep] <- normalize_binary_var(x[keep], name); out
}

make_observed_mask <- function(x) {
  if (is.numeric(x) || is.integer(x)) is.finite(x) else !is.na(x)
}

prepare_modeled_outcome <- function(x, requested = "auto", name = "outcome") {
  type <- infer_variable_type(x, requested, name)
  values <- if (type == "binary") normalize_binary_allow_missing(x, name) else as.numeric(x)
  list(values = values, type = type, observed = make_observed_mask(values))
}

# Identify the "knee" in a ranked score curve - the point of diminishing
# returns, used as the rough-prescreen cutoff.
make_knee_cutoff <- function(scores) {
  s <- sort(scores, decreasing = TRUE, na.last = NA)
  if (length(s) == 0L) stop("Cannot compute knee cutoff from empty score vector.", call. = FALSE)
  if (length(s) == 1L || isTRUE(all.equal(max(s), min(s))))
    return(list(sorted_scores = s, rank = 1L, value = s[1L]))
  x  <- seq_along(s)
  xn <- (x - x[1]) / (x[length(x)] - x[1])
  yn <- (s - s[length(s)]) / (s[1] - s[length(s)])
  d  <- abs(yn - (1 - xn)) / sqrt(2)
  knee_rank <- which.max(d)
  list(sorted_scores = s, rank = knee_rank, value = s[knee_rank])
}

plot_knee_curve <- function(knee_obj, label_y = "Score") {
  s <- knee_obj$sorted_scores
  plot(s, type = "l", xlab = "Rank best -> worst", ylab = label_y)
  text(knee_obj$rank, knee_obj$value + 0.02,
       labels = sprintf("Cutoff = %.4f (rank %d)", knee_obj$value, knee_obj$rank),
       pos = 4, xpd = NA)
}

# v5.1: Harmonize cutoff_rule and the legacy use_knee_cutoff toggle.
# Precedence: explicit cutoff_rule wins; otherwise fall back to legacy toggle.
resolve_cutoff_rule <- function(rp_cfg) {
  if (!is.null(rp_cfg$cutoff_rule)) return(rp_cfg$cutoff_rule)
  if (isTRUE(rp_cfg$use_knee_cutoff))  return("knee")
  if (identical(rp_cfg$use_knee_cutoff, FALSE)) {
    warning("use_knee_cutoff = FALSE is ambiguous; defaulting to cutoff_rule = 'positive'. ",
            "Set cfg$rough_prescreen$cutoff_rule explicitly to silence this.")
    return("positive")
  }
  "knee"
}

# v5.1: Unified cutoff application for the rough prescreen.
# "positive" keeps variables that beat the null model (score > min_score).
# "knee"     uses the geometric knee on the sorted-score curve.
# "topk"     keeps the top max_keep variables by score.
# max_keep is applied as a safety cap regardless of rule.
apply_cutoff <- function(scores, knee, rule, min_score = 0, max_keep = 2000L) {
  s <- sort(scores[is.finite(scores)], decreasing = TRUE)
  kept <- switch(rule,
    "knee"     = s[s >= knee$value],
    "positive" = s[s >  min_score],
    "topk"     = s[seq_len(min(length(s), max_keep))],
    s[s >= knee$value])
  if (length(kept) > max_keep) kept <- kept[seq_len(max_keep)]
  kept
}


# =============================================================================
# 3) BASE DATA CONSTRUCTION
# =============================================================================
# Plain-English role: read the Add Health .xpt files, join them on AID/PSUSCID,
# then build the exposure (depression) and outcome (log earnings) from raw
# items. Everything downstream starts from the single main_df produced here.

read_wave1_merged <- function(cfg) {
  msg("Reading and merging Wave 1 source files...", cfg = cfg)
  inhome_w1  <- read_xpt_df(cfg$paths$wave1_inhome)
  birth      <- read_xpt_df(cfg$paths$birth_records)
  nhood      <- read_xpt_df(cfg$paths$neighborhood_w1)
  inschool   <- read_xpt_df(cfg$paths$inschool_w1)
  context_w1 <- read_xpt_df(cfg$paths$contextual_w1)
  health_w1  <- read_xpt_df(cfg$paths$health_w1)
  spatial_w1 <- read_xpt_df(cfg$paths$spatial_w1)
  stchr95_w1 <- read_xpt_df(cfg$paths$stchr95_w1)
  polcon_w1  <- read_xpt_df(cfg$paths$polcon_w1)
  weights_w1 <- read_xpt_df(cfg$paths$weights_w1)
  weights_w1$PSUSCID <- as.integer(weights_w1$PSUSCID)
  w1_all <- merge_by_key(
    list(inhome_w1, birth, nhood, inschool, context_w1, health_w1,
         spatial_w1, stchr95_w1, polcon_w1, weights_w1), "AID")
  school_admin <- read_xpt_df(cfg$paths$school_admin_w1)
  names(school_admin)[names(school_admin) == "ASCHLCDE"] <- "PSUSCID"
  school_admin$PSUSCID <- as.integer(school_admin$PSUSCID)
  w1_all$PSUSCID <- as.integer(w1_all$PSUSCID)
  merge_by_key(list(w1_all, school_admin), "PSUSCID")
}

# Column typing: a survey-coded variable with few unique values (after
# ignoring 96/97/98/99) is treated as a factor. This is a global structural
# decision about the data itself (like "income is continuous; race is a
# factor"), not a model-tuning decision, and is therefore safe to do on the
# full sample without creating outcome/exposure leakage.
classify_factors_by_uniques <- function(df, cfg_pre) {
  bad_codes <- cfg_pre$bad_codes_high
  should_be_factor <- vapply(df, function(col) {
    num  <- to_num(col)
    uniq <- unique(num[!is.na(num) & !(num %in% bad_codes)])
    length(uniq) <= cfg_pre$factor_unique_threshold
  }, logical(1))
  factor_cols <- names(df)[should_be_factor]
  df[factor_cols] <- lapply(df[factor_cols], as.factor)
  for (v in cfg_pre$long_factors)
    if (v %in% names(df)) df[[v]] <- as.factor(df[[v]])
  for (prefix in cfg_pre$force_factor_prefixes) {
    hits <- grep(prefix, names(df), value = TRUE)
    if (length(hits) > 0L) df[hits] <- lapply(df[hits], as.factor)
  }
  df
}

# For every covariate, attach two indicators:
#   *_missA  = actual missing / general-missingness codes
#   *_miss97 = explicit not-applicable / skip code 97
# Then median-impute the value so the indicator carries the signal.
add_dual_missingness_indicators <- function(data, factor_vars, numeric_vars, cfg_pre) {
  data <- as.data.frame(data)
  n_factor_low  <- 0L  # track count of factors using each scheme
  n_factor_high <- 0L
  n_factor_skip_retained <- 0L
  total_missA_numeric <- 0L; total_miss97_numeric <- 0L
  total_missA_factor  <- 0L; total_miss97_factor  <- 0L
  # Numeric variables
  for (nm in intersect(numeric_vars, names(data))) {
    x   <- suppressWarnings(as.numeric(data[[nm]]))
    suf <- abs(round(x)) %% 100
    miss97 <- !is.na(x) & suf == 97
    missA  <- is.na(x) | suf %in% c(96, 98, 99)
    fill_value <- compute_simple_impute(x[!(missA | miss97)], cfg_pre$numeric_imputation)
    if (!isTRUE(identical(cfg_pre$numeric_missing_scheme, "dual_indicators"))) {
      missA <- is.na(x) | miss97 | missA
      miss97 <- rep(FALSE, length(x))
    }
    data[[paste0(nm, "_missA")]]  <- as.integer(missA)
    data[[paste0(nm, "_miss97")]] <- as.integer(miss97)
    total_missA_numeric  <- total_missA_numeric  + sum(missA)
    total_miss97_numeric <- total_miss97_numeric + sum(miss97)
    x[missA | miss97] <- fill_value
    data[[nm]] <- x
  }
  # Factor variables. adaptive missingness scheme.
  #   - If the maximum substantive (non-missingness) level is <= 5, use the
  #     "low" scheme: refusal codes are {6, 8, 9}, skip code is 7.
  #   - Otherwise use the "high" scheme: refusal codes are {96, 98, 99},
  #     skip code is 97.
  # The skip code is preserved as its own factor level when frequent enough
  # (count >= factor_special_code_min_n OR proportion >= factor_special_code_min_prop),
  # otherwise it is folded into the generic _Missing_ bucket. This avoids
  # exploding the design while retaining substantively-meaningful skip info.
  thresh        <- cfg_pre$factor_max_substantive_level_threshold %||% 5L
  skip_low      <- as.character(cfg_pre$factor_skip_code_low      %||% 7L)
  ref_low       <- as.character(cfg_pre$factor_refusal_codes_low  %||% c(6L, 8L, 9L))
  skip_high     <- as.character(cfg_pre$factor_skip_code_high     %||% 97L)
  ref_high      <- as.character(cfg_pre$factor_refusal_codes_high %||% c(96L, 98L, 99L))
  miss_label    <- cfg_pre$factor_missing_label %||% "Missing"
  skip_label    <- cfg_pre$factor_skip_label    %||% "Skip"
  min_n_skip    <- cfg_pre$factor_special_code_min_n    %||% 30L
  min_pr_skip   <- cfg_pre$factor_special_code_min_prop %||% 0.02
  for (nm in intersect(factor_vars, names(data))) {
    xc <- as.character(data[[nm]])
    # Determine which coding scheme applies based on the maximum substantive
    # numeric level (ignoring known refusal/skip codes from BOTH schemes).
    all_miss_codes_either_scheme <- c(skip_low, ref_low, skip_high, ref_high)
    xn_substantive <- suppressWarnings(as.numeric(
      xc[!(xc %in% all_miss_codes_either_scheme) & !is.na(xc)]))
    max_subst_level <- if (any(is.finite(xn_substantive)))
      max(xn_substantive, na.rm = TRUE) else 0
    use_low_scheme <- is.finite(max_subst_level) && max_subst_level <= thresh
    skip_code <- if (use_low_scheme) skip_low else skip_high
    ref_codes <- if (use_low_scheme) ref_low else ref_high
    if (use_low_scheme) n_factor_low  <- n_factor_low  + 1L
    else                n_factor_high <- n_factor_high + 1L

    # Build masks for the three categories of missingness.
    miss97 <- !is.na(xc) & xc == skip_code
    missA  <- is.na(xc) | xc %in% ref_codes
    data[[paste0(nm, "_missA")]]  <- as.integer(missA)
    data[[paste0(nm, "_miss97")]] <- as.integer(miss97)
    total_missA_factor  <- total_missA_factor  + sum(missA)
    total_miss97_factor <- total_miss97_factor + sum(miss97)

    # Decide whether to retain the skip code as its own factor level.
    n_skip <- sum(miss97)
    pr_skip <- n_skip / length(xc)
    retain_skip <- n_skip >= min_n_skip || pr_skip >= min_pr_skip

    if (retain_skip) {
      n_factor_skip_retained <- n_factor_skip_retained + 1L
      # Replace skip-code values with the skip label; drop refusal-codes and NA
      # to the generic missing bucket.
      xc[missA] <- NA_character_
      xc[miss97] <- skip_label
      f <- addNA(factor(xc))
      levels(f)[is.na(levels(f))] <- miss_label
    } else {
      # Collapse skip + refusal + NA into one missing bucket (legacy behavior).
      xc[missA | miss97] <- NA_character_
      f <- addNA(factor(xc))
      levels(f)[is.na(levels(f))] <- miss_label
    }
    data[[nm]] <- f
  }
  # report what just happened so the analyst can verify behavior.
  message(sprintf(
    "  [add_dual_missingness_indicators] processed %d numeric and %d factor cols.",
    length(intersect(numeric_vars, names(data))),
    length(intersect(factor_vars,  names(data)))))
  message(sprintf(
    "    Numeric: %d _missA flags, %d _miss97 flags (imputation = '%s').",
    total_missA_numeric, total_miss97_numeric, cfg_pre$numeric_imputation))
  message(sprintf(
    "    Factor:  %d _missA flags, %d _miss97 flags. %d factors used the LOW (6/8/9 + 7) scheme; %d used the HIGH (96/98/99 + 97) scheme.",
    total_missA_factor, total_miss97_factor, n_factor_low, n_factor_high))
  message(sprintf(
    "    Factor skip code retained as its own level for %d of %d factors (threshold: n>=%d or prop>=%.2f).",
    n_factor_skip_retained, n_factor_low + n_factor_high, min_n_skip, min_pr_skip))
  data
}

# =============================================================================
# OUTCOME FAMILY DISPATCHER (v6)
# =============================================================================
# Each family has a constructor that takes (main_df, wave, family_cfg, outcome_cfg)
# and returns a numeric or integer vector Y of length nrow(main_df), with NA
# for rows where the outcome is not observed at the requested wave.
# construct_outcome() is the dispatcher called by build_main_dataset(). It
# reads cfg$outcome$family and cfg$outcome$family_member to pick which
# constructor to call, writes the result to main_df[[cfg$analysis$outcome_var]],
# and sets the censoring indicator.

# ---- Placeholder helper: read the Wave-N in-home file when available --------
read_wave_inhome <- function(wave, cfg) {
  path_name <- paste0("wave", wave, "_inhome")
  if (!path_name %in% names(cfg$paths)) {
    stop(sprintf("cfg$paths$%s is not configured.", path_name), call. = FALSE)
  }
  p <- cfg$paths[[path_name]]
  if (is.null(p) || !file.exists(p)) {
    stop(sprintf("Wave %d in-home file not found at: %s", wave, p %||% "<NULL>"), call. = FALSE)
  }
  df <- read_xpt_df(p)
  df$AID <- as.numeric(as.character(df$AID))
  df
}

# Small helper so constructor functions can access cfg without being passed it.
# Defined here (before constructors) so lookup works if the file is source'd
# out of order for testing.
.CFG_ENV <- new.env(parent = emptyenv())
cfg_env_lookup <- function() .CFG_ENV$cfg

# ---- Educational Attainment (nested binary) --------------------------------
# PLACEHOLDER: fill in the recoding rules for your Add Health item values.
construct_outcome_educational_attainment <- function(main_df, wave, family_cfg,
                                                    outcome_cfg, member) {
  message(sprintf("    [outcome] Educational Attainment, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("EducationalAttainment source variable for wave %d is not defined in cfg.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  # Attach the source variable if not already present
  if (!src %in% names(main_df)) {
    inhome <- read_wave_inhome(wave, cfg_env_lookup())
    assert_required_columns(inhome, c("AID", src), sprintf("wave%d inhome", wave))
    main_df <- main_df %>%
      dplyr::left_join(inhome %>% dplyr::select(AID, dplyr::all_of(src)), by = "AID")
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # PLACEHOLDER threshold coding: update these numeric cutoffs to match your
  # Add Health coding scheme for this source variable. The values below are
  # illustrative and MUST be verified against the codebook.
  Y <- switch(member,
    at_least_hs           = as.integer(raw >= 2),
    at_least_some_college = as.integer(raw >= 4),
    at_least_college_grad = as.integer(raw >= 6),
    some_grad_school      = as.integer(raw >= 7),
    stop(sprintf("Unknown EducationalAttainment member: %s", member), call. = FALSE))
  Y[!is.finite(raw)] <- NA_integer_
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Labor Force Participation (binary) -----------------------------------
# PLACEHOLDER: fill in the recoding rule.
construct_outcome_labor_force_participation <- function(main_df, wave, family_cfg,
                                                       outcome_cfg, member) {
  message(sprintf("    [outcome] Labor Force Participation, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("LaborForceParticipation source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    inhome <- read_wave_inhome(wave, cfg_env_lookup())
    main_df <- main_df %>%
      dplyr::left_join(inhome %>% dplyr::select(AID, dplyr::all_of(src)), by = "AID")
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # the previous body silently coded EVERY finite value other
  # than 1 (including refusal/DK/legitimate-skip codes) as 0, i.e. it miscoded
  # missing as non-participation. Require an EXPLICIT codebook mapping and
  # refuse to construct LFP otherwise, so it can never run on a guessed rule:
  #   family_cfg$codes$participate    = codes meaning "in labor force / working"    -> 1
  #   family_cfg$codes$nonparticipate = codes meaning "not in labor force"          -> 0
  #   family_cfg$codes$missing        = codes meaning refused / DK / skip / missing -> NA
  codes <- family_cfg$codes
  if (is.null(codes) || is.null(codes$participate) || is.null(codes$nonparticipate)) {
    stop(sprintf(paste0(
      "LaborForceParticipation has no codebook mapping. Set family_cfg$codes$participate, ",
      "$nonparticipate (and $missing) for source '%s' (wave %d) from the Add Health codebook ",
      "before constructing LFP. Refusing to guess (would miscode missing as 0)."), src, wave),
      call. = FALSE)
  }
  Y <- rep(NA_integer_, length(raw))
  Y[is.finite(raw) & raw %in% codes$participate]    <- 1L
  Y[is.finite(raw) & raw %in% codes$nonparticipate] <- 0L
  # Any finite value not in participate/nonparticipate/missing is an UNMAPPED
  # code -- fail loudly rather than silently dropping or zero-coding it.
  mapped   <- c(codes$participate, codes$nonparticipate, codes$missing)
  unmapped <- unique(raw[is.finite(raw) & !(raw %in% mapped)])
  if (length(unmapped) > 0L) {
    stop(sprintf("LaborForceParticipation source '%s' (wave %d) has unmapped finite codes: %s. Add them to family_cfg$codes.",
      src, wave, paste(sort(unmapped), collapse = ", ")), call. = FALSE)
  }
  message(sprintf("    [outcome] Constructed %d observed (%d in-LF, %d not-in-LF), %d missing. Participation: %.1f%%.",
    sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE), sum(Y == 0L, na.rm = TRUE),
    sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Usual Hours (continuous) ---------------------------------------------
# PLACEHOLDER: fill in the recoding rule.
construct_outcome_usual_hours <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  message(sprintf("    [outcome] Usual Hours, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("UsualHours source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    inhome <- read_wave_inhome(wave, cfg_env_lookup())
    main_df <- main_df %>%
      dplyr::left_join(inhome %>% dplyr::select(AID, dplyr::all_of(src)), by = "AID")
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # PLACEHOLDER: treat explicit refusal codes (>= 996) as missing.
  Y <- ifelse(is.finite(raw) & raw < 996, raw, NA_real_)
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Hours range: [%s, %s].",
    sum(!is.na(Y)), sum(is.na(Y)),
    ifelse(any(!is.na(Y)), sprintf("%.1f", min(Y, na.rm = TRUE)), "NA"),
    ifelse(any(!is.na(Y)), sprintf("%.1f", max(Y, na.rm = TRUE)), "NA")))
  Y
}

# ---- Compensation (continuous; log-transformed when configured) -----------
construct_outcome_compensation <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  message(sprintf("    [outcome] Compensation, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("Compensation source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (is.list(src)) {
    exact_v <- src$exact_var; brack_v <- src$bracket_var
  } else {
    stop("Compensation source must be a list with exact_var and bracket_var.", call. = FALSE)
  }
  # Join in the two source variables if not already present
  need <- setdiff(c(exact_v, brack_v), names(main_df))
  if (length(need) > 0L) {
    inhome <- read_wave_inhome(wave, cfg_env_lookup())
    assert_required_columns(inhome, c("AID", need), sprintf("wave%d inhome", wave))
    main_df <- main_df %>%
      dplyr::left_join(inhome %>% dplyr::select(AID, dplyr::all_of(need)), by = "AID")
  }
  local_cfg <- list(
    exact_valid_upper = family_cfg$exact_valid_upper,
    bracket_map       = family_cfg$bracket_map
  )
  earnings <- compute_earnings(main_df[[exact_v]], main_df[[brack_v]], local_cfg)
  floor_v <- family_cfg$earnings_floor_for_log %||% 0.1
  earnings <- ifelse(!is.na(earnings) & earnings < 1, floor_v, earnings)
  Y <- if (isTRUE(outcome_cfg$log_transform)) log(earnings) else earnings
  message(sprintf("    [outcome] Constructed %d observed, %d missing. %s range: [%s, %s].",
    sum(!is.na(Y)), sum(is.na(Y)),
    if (isTRUE(outcome_cfg$log_transform)) "log(earnings)" else "earnings",
    ifelse(any(!is.na(Y)), sprintf("%.2f", min(Y, na.rm = TRUE)), "NA"),
    ifelse(any(!is.na(Y)), sprintf("%.2f", max(Y, na.rm = TRUE)), "NA")))
  Y
}

# ---- Health Status (nested binary) ----------------------------------------
# PLACEHOLDER: Add Health self-rated health uses 1=Excellent ... 5=Poor.
# We invert below so that "at least good" means a healthier rating.
construct_outcome_health_status <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  message(sprintf("    [outcome] Health Status, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("HealthStatus source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    inhome <- read_wave_inhome(wave, cfg_env_lookup())
    assert_required_columns(inhome, c("AID", src), sprintf("wave%d inhome", wave))
    main_df <- main_df %>%
      dplyr::left_join(inhome %>% dplyr::select(AID, dplyr::all_of(src)), by = "AID")
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # Treat refusal/DK codes (>= 6) as missing. 1=Excellent, 5=Poor.
  raw[!is.finite(raw) | raw >= 6] <- NA_real_
  Y <- switch(member,
    at_least_fair      = as.integer(raw <= 4),   # fair, good, very good, excellent
    at_least_good      = as.integer(raw <= 3),   # good, very good, excellent
    at_least_very_good = as.integer(raw <= 2),   # very good, excellent
    excellent          = as.integer(raw <= 1),
    stop(sprintf("Unknown HealthStatus member: %s", member), call. = FALSE))
  Y[is.na(raw)] <- NA_integer_
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Mental Health (PLACEHOLDER) ------------------------------------------
construct_outcome_mental_health <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  message(sprintf("    [outcome] Mental Health, wave %d. [PLACEHOLDER]", wave))
  rep(NA_real_, nrow(main_df))
}

# ---- Substance Use (PLACEHOLDER) ------------------------------------------
construct_outcome_substance_use <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  message(sprintf("    [outcome] Substance Use, wave %d. [PLACEHOLDER]", wave))
  rep(NA_real_, nrow(main_df))
}

# ---- Pass-through (negative-control / placebo) outcome --------------------
# Uses a PRE-EXISTING numeric column of the built dataset directly as Y, WITHOUT
# running any headline constructor. This is the correct mechanism for a
# negative-control outcome: it swaps the outcome FAMILY so the dispatcher does
# not build the headline outcome and relabel it (setting only analysis$outcome_var
# would do exactly that -- construct_outcome dispatches on cfg$outcome$family, and
# build_main_dataset writes the constructed Y into whatever analysis$outcome_var
# names). The named column must already be present in the built dataset; this
# constructor never fabricates, joins, or transforms it (no log_transform is
# applied here -- set cfg$outcome$log_transform = FALSE for a raw column). The
# downstream [0,1] bounding is computed from this column's own observed values,
# so it adapts automatically; set cfg$analysis$outcome_type ("continuous" or
# "binary") to match the column. Fails loud if the column is absent or non-numeric.
construct_outcome_pass_through <- function(main_df, wave, family_cfg, outcome_cfg, member) {
  src <- family_cfg$source_var
  if (is.null(src) || !is.character(src) || length(src) != 1L || !nzchar(src))
    stop("PassThrough outcome: cfg$outcome$families$PassThrough$source_var must be a single existing column name.", call. = FALSE)
  if (!src %in% names(main_df))
    stop(sprintf("PassThrough outcome: source column '%s' is not present in the built dataset. Add it to the covariate read or pick a present column.", src), call. = FALSE)
  Y <- suppressWarnings(as.numeric(main_df[[src]]))
  if (all(is.na(Y)))
    warning(sprintf("PassThrough outcome column '%s' is entirely non-numeric/NA after coercion.", src))
  message(sprintf("    [outcome] PassThrough negative-control column '%s', wave %d: %d observed, %d missing.",
    src, wave, sum(!is.na(Y)), sum(is.na(Y))))
  Y
}

# Small helper so constructor functions can access cfg without being passed it.
# (Definition moved to top of outcome-family section.)

construct_outcome <- function(main_df, cfg) {
  .CFG_ENV$cfg <- cfg  # stash for per-constructor lookups
  fam_name <- cfg$outcome$family
  wave     <- cfg$outcome$current_wave %||% cfg$outcome$waves
  if (length(wave) != 1L || !is.numeric(wave))
    stop("construct_outcome(): cfg$outcome$current_wave must be a single integer.", call. = FALSE)
  wave <- as.integer(wave)
  message(sprintf("  [outcome dispatcher] family = '%s', wave = %d.", fam_name, wave))
  fam_cfg <- cfg$outcome$families[[fam_name]]
  if (is.null(fam_cfg))
    stop(sprintf("Unknown outcome family '%s' (not in cfg$outcome$families).", fam_name), call. = FALSE)
  member <- cfg$outcome$family_member
  if (identical(fam_cfg$type, "binary_nested") && is.null(member)) {
    member <- names(fam_cfg$members)[length(fam_cfg$members)]
    message(sprintf("  [outcome dispatcher] No family_member set; defaulting to highest threshold '%s'.", member))
  }
  Y <- switch(fam_name,
    EducationalAttainment   = construct_outcome_educational_attainment(main_df, wave, fam_cfg, cfg$outcome, member),
    LaborForceParticipation = construct_outcome_labor_force_participation(main_df, wave, fam_cfg, cfg$outcome, member),
    UsualHours              = construct_outcome_usual_hours(main_df, wave, fam_cfg, cfg$outcome, member),
    Compensation            = construct_outcome_compensation(main_df, wave, fam_cfg, cfg$outcome, member),
    HealthStatus            = construct_outcome_health_status(main_df, wave, fam_cfg, cfg$outcome, member),
    MentalHealth            = construct_outcome_mental_health(main_df, wave, fam_cfg, cfg$outcome, member),
    SubstanceUse            = construct_outcome_substance_use(main_df, wave, fam_cfg, cfg$outcome, member),
    PassThrough             = construct_outcome_pass_through(main_df, wave, fam_cfg, cfg$outcome, member),
    stop(sprintf("Unknown outcome family '%s'.", fam_name), call. = FALSE))
  if (isTRUE(cfg$outcome$stop_on_all_missing_outcome) && !isTRUE(cfg$safety$allow_placeholder_outcomes %||% FALSE) && all(is.na(Y))) {
    stop(sprintf(
      "Outcome constructor for family '%s' wave %d returned all missing values. This usually means the selected outcome family is still a placeholder or the configured source variables are unavailable.",
      fam_name, wave), call. = FALSE)
  }
  # The constructor may have left-joined source columns into main_df via a
  # local copy; re-attach them by re-reading here would duplicate work. We
  # assume the return vector Y is aligned row-for-row with main_df, which it
  # is because dplyr::left_join preserves row order for existing keys and the
  # constructors do not reorder.
  list(Y = Y, fam_name = fam_name, wave = wave, member = member)
}

# hard-coded transform of H1GH50 (usual bedtime). H1GH50 is stored as
# a 12-hour clock STRING ("HH:MMA"/"HH:MMP", hours 00-12, minutes 00-59) with
# string sentinels "999996"/"999998"/"999999". It is parsed to minutes-since-
# midnight (AM 12->0, PM 12->12, PM h->h+12; hour 00 treated as 12 on the
# clock face) and REPLACED by H1GH50__tsin / H1GH50__tcos = sin/cos of the
# 24h angle, preserving the midnight wraparound. Sentinel and unparseable
# values become NA in both columns and are handled by the existing
# missingness machinery. The transform is gated by transform_time_variables.
transform_time_variables_in_df <- function(df, cfg) {
  if (!isTRUE(cfg$analysis$transform_time_variables %||% FALSE)) return(df)
  v <- "H1GH50"
  if (!(v %in% names(df))) {
    msg("  [time] H1GH50 not present; nothing to transform.", cfg = cfg)
    return(df)
  }
  raw <- trimws(as.character(df[[v]]))                  # column is character
  sentinels <- c("999996", "999998", "999999")          # refused / DK / N/A
  is_sent   <- raw %in% sentinels | is.na(raw) | raw == ""
  # Parse "HH:MM" + A/P. Hours 00-12 are all accepted (this encoding uses 00),
  # minutes 00-59, suffix A or P (case-insensitive). The suffix may be followed
  # by an optional "M"/"m" ("AM"/"PM") and preceded by optional whitespace, to
  # absorb extract-to-extract variation in how the meridiem is coded.
  m <- regmatches(raw, regexec("^([0-9]{1,2}):([0-5][0-9])\\s*([AaPp])[Mm]?$", raw))
  hour12 <- rep(NA_real_, length(raw))
  minute <- rep(NA_real_, length(raw))
  ampm   <- rep(NA_character_, length(raw))
  for (i in seq_along(m)) {
    g <- m[[i]]
    if (length(g) == 4L) {
      hour12[i] <- as.numeric(g[2]); minute[i] <- as.numeric(g[3])
      ampm[i]   <- toupper(g[4])
    }
  }
  # Treat hour 00 as 12 on the 12-hour face (same clock position) so the
  # AM/PM rule below applies uniformly.
  hour12[is.finite(hour12) & hour12 == 0] <- 12
  parsed_ok <- !is.na(hour12) & !is.na(minute) & !is.na(ampm) &
               hour12 >= 1 & hour12 <= 12 & !is_sent
  # 12h -> 24h: AM 12 -> 0, AM h -> h ; PM 12 -> 12, PM h -> h+12.
  hour24 <- ifelse(ampm == "A",
                   ifelse(hour12 == 12, 0, hour12),
                   ifelse(hour12 == 12, 12, hour12 + 12))
  minutes <- ifelse(parsed_ok, hour24 * 60 + minute, NA_real_)
  # Cyclical encoding preserves the midnight wraparound (bedtimes cluster on
  # both sides of midnight).
  ang <- 2 * pi * minutes / 1440
  df[[paste0(v, "__tsin")]] <- sin(ang)
  df[[paste0(v, "__tcos")]] <- cos(ang)
  df[[v]] <- NULL
  msg(sprintf("  [time] H1GH50 (12h A/P) -> sin/cos: %d parsed, %d sentinel, %d UNPARSEABLE of %d.",
      sum(parsed_ok), sum(is_sent), sum(!parsed_ok & !is_sent), length(raw)), cfg = cfg)
  df
}

build_main_dataset <- function(w1_all, cfg) {
  msg("\n===== STAGE: Build main dataset =====", cfg = cfg)
  assert_required_columns(w1_all,
    c("AID", cfg$analysis$cluster_var, cfg$analysis$weight_var), "w1_all")
  main_sample <- w1_all
  msg(sprintf("  Input W1 merged table: %d rows x %d cols.", nrow(main_sample), ncol(main_sample)), cfg = cfg)

  # ---- Build exposure (CES-D -> Depressed): unchanged across outcomes/waves ---
  msg("  [exposure] Reading Wave 2 CES-D items to build 'Depressed'...", cfg = cfg)
  inhome_w2 <- read_xpt_df(cfg$paths$wave2_inhome)
  mh_cols   <- cfg$exposure$cesd_items
  assert_required_columns(inhome_w2, c("AID", mh_cols), "inhome_w2")
  inhome_w2$AID <- as.numeric(as.character(inhome_w2$AID))
  main_sample <- main_sample %>%
    dplyr::left_join(inhome_w2 %>% dplyr::select(AID, dplyr::all_of(mh_cols)), by = "AID")
  n_before <- nrow(main_sample)
  if (isTRUE(cfg$exposure$drop_missing_exposure)) {
    main_sample <- main_sample %>%
      dplyr::filter(dplyr::if_all(
        dplyr::all_of(mh_cols),
        ~ !(.x %in% cfg$exposure$nonresponse_codes) & !is.na(.x)))
    msg(sprintf("  [exposure] Dropped %d of %d rows with missing/refused CES-D items; %d remain.",
      n_before - nrow(main_sample), n_before, nrow(main_sample)), cfg = cfg)
  }
  # v6.21 BUG FIX: H1GH50 (usual bedtime) is stored as a 12-hour clock STRING
  # ("HH:MMA"/"HH:MMP") and is parsed later by transform_time_variables_in_df.
  # The blanket to_num() below would coerce those strings to NA before the
  # string parser ever runs, silently destroying the variable (the transform
  # would then report "0 parsed"). Exempt the time column(s) from to_num so they
  # survive as character until their dedicated transform. Only the time
  # variables named for transformation are exempted; everything else is coerced
  # exactly as before.
  time_vars_keep <- if (isTRUE(cfg$analysis$transform_time_variables %||% FALSE)) "H1GH50" else character(0)
  time_vars_keep <- intersect(time_vars_keep, names(main_sample))
  coerce_cols <- setdiff(names(main_sample), time_vars_keep)
  main_sample[coerce_cols] <- lapply(main_sample[coerce_cols], to_num)
  main_sample <- as.data.frame(main_sample, check.names = FALSE)
  for (col in mh_cols) main_sample[[col]] <- as.numeric(as.character(main_sample[[col]]))
  for (col in cfg$exposure$reverse_score_items)
    if (col %in% names(main_sample)) main_sample[[col]] <- 3 - main_sample[[col]]
  main_sample$MHSum <- rowSums(main_sample[, mh_cols, drop = FALSE], na.rm = FALSE)
  main_sample[[cfg$analysis$exposure_var]] <-
    as.integer(main_sample$MHSum >= cfg$exposure$cutpoint)
  msg(sprintf("  [exposure] MHSum in [%d, %d]; Depressed (cutpoint=%d): %d of %d = %.2f%%.",
    min(main_sample$MHSum, na.rm = TRUE), max(main_sample$MHSum, na.rm = TRUE),
    cfg$exposure$cutpoint,
    sum(main_sample[[cfg$analysis$exposure_var]] == 1L, na.rm = TRUE),
    nrow(main_sample),
    100 * mean(main_sample[[cfg$analysis$exposure_var]], na.rm = TRUE)), cfg = cfg)

  # ---- Build outcome via the family dispatcher ---------------------------
  msg("  [outcome] Constructing outcome via family dispatcher...", cfg = cfg)
  oc <- construct_outcome(main_sample, cfg)
  main_sample[[cfg$analysis$outcome_var]] <- oc$Y
  # Censoring indicator
  main_sample[[cfg$analysis$outcome_observed_var]] <-
    as.integer(!is.na(main_sample[[cfg$analysis$outcome_var]]))
  msg(sprintf("  [outcome] %d rows with observed Y (%.1f%%); %d censored.",
    sum(main_sample[[cfg$analysis$outcome_observed_var]] == 1L),
    100 * mean(main_sample[[cfg$analysis$outcome_observed_var]]),
    sum(main_sample[[cfg$analysis$outcome_observed_var]] == 0L)), cfg = cfg)

  if (isTRUE(cfg$preprocessing$remove_duplicate_y_suffix_columns)) {
    n_col_before <- ncol(main_sample)
    main_sample <- main_sample[, !grepl("\\.y$", names(main_sample)), drop = FALSE]
    n_removed <- n_col_before - ncol(main_sample)
    if (n_removed > 0L)
      msg(sprintf("  [cleanup] Removed %d duplicate .y-suffix columns from prior merges.", n_removed), cfg = cfg)
  }

  # v6 Fix C: drop rows with invalid sampling weights at dataset-build time
  # so every downstream stage sees the same analytic sample.
  if (isTRUE(cfg$preprocessing$drop_invalid_weights_at_build)) {
    w <- suppressWarnings(as.numeric(main_sample[[cfg$analysis$weight_var]]))
    keep_w <- is.finite(w) & w > 0
    n_drop <- sum(!keep_w); n_total <- length(w)
    if (n_drop > 0L) {
      msg(sprintf("  [weights] Dropping %d of %d rows (%.2f%%) with non-positive or missing %s.",
        n_drop, n_total, 100 * n_drop / n_total, cfg$analysis$weight_var), cfg = cfg)
      main_sample <- main_sample[keep_w, , drop = FALSE]
      msg(sprintf("  [weights] Analytic sample now has %d rows; weights range [%.4g, %.4g].",
        nrow(main_sample),
        min(main_sample[[cfg$analysis$weight_var]]),
        max(main_sample[[cfg$analysis$weight_var]])), cfg = cfg)
    } else {
      msg(sprintf("  [weights] All %d rows have valid %s (no drops).",
        n_total, cfg$analysis$weight_var), cfg = cfg)
    }
  }

  # winsorize sampling weights at the configured upper quantile, on the
  # FINALIZED analytic sample (clean positive weights only). This dampens the
  # influence of very-high-weight respondents. Runs after the invalid-weight
  # drop so the quantile is not distorted by zero/negative/missing weights.
  wq <- cfg$analysis$weight_winsor_quantile
  if (!is.null(wq) && is.finite(wq) && wq > 0 && wq < 1) {
    w_cur <- suppressWarnings(as.numeric(main_sample[[cfg$analysis$weight_var]]))
    cap <- unname(stats::quantile(w_cur, probs = wq, na.rm = TRUE))
    n_wins <- sum(w_cur > cap, na.rm = TRUE)
    w_new <- pmin(w_cur, cap)
    # Optional renormalization to preserve the original mean weight, so the
    # winsorized weights represent the same effective population total.
    if (isTRUE(cfg$analysis$weight_winsor_renormalize %||% FALSE)) {
      mean_old <- mean(w_cur, na.rm = TRUE)
      mean_new <- mean(w_new, na.rm = TRUE)
      if (is.finite(mean_new) && mean_new > 0) w_new <- w_new * (mean_old / mean_new)
    }
    main_sample[[cfg$analysis$weight_var]] <- w_new
    msg(sprintf("  [weights] Winsorized %d of %d weights at q%.2f (cap=%.4g); range now [%.4g, %.4g]%s.",
      n_wins, length(w_cur), wq, cap,
      min(w_new, na.rm = TRUE), max(w_new, na.rm = TRUE),
      if (isTRUE(cfg$analysis$weight_winsor_renormalize %||% FALSE)) ", renormalized to original mean" else ""),
      cfg = cfg)
  }

  # parse and cyclically encode the H1GH50 bedtime (12-hour clock
  # string) on the finalized analytic sample, before it is cached and before
  # screening.
  main_sample <- transform_time_variables_in_df(main_sample, cfg)

  msg(sprintf("===== Main dataset ready: %d rows x %d cols. =====",
    nrow(main_sample), ncol(main_sample)), cfg = cfg)
  main_sample
}

# Which columns are eligible to enter screening/estimation as W. Excludes
# the exposure, outcome, IDs, cluster, weights, outcome-observed indicator,
# and any helper columns the exposure/outcome constructors created.
get_candidate_vars <- function(df, cfg) {
  # pull drop_from_candidates from the selected outcome family AND
  # prefer the per-wave list when cfg$outcome$current_wave is known. Falls
  # back to the family's legacy single drop list when no per-wave list
  # exists or current_wave is unset. The per-wave logic ensures Wave-5
  # outcome source vars remain in W when running Wave 3 or 4 outcomes
  # (where they are valid confounders), and vice versa.
  fam_drop <- character(0)
  if (!is.null(cfg$outcome$family)) {
    fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
    if (!is.null(fam_cfg)) {
      cur_wave <- cfg$outcome$current_wave
      if (!is.null(cur_wave) && !is.null(fam_cfg$drop_from_candidates_by_wave)) {
        per_wave <- fam_cfg$drop_from_candidates_by_wave[[as.character(cur_wave)]]
        fam_drop <- if (!is.null(per_wave)) as.character(per_wave)
                    else fam_cfg$drop_from_candidates %||% character(0)
      } else {
        fam_drop <- fam_cfg$drop_from_candidates %||% character(0)
      }
      # a pass-through outcome's source column IS the outcome (its values
      # are copied into outcome_var); it must never remain a candidate covariate,
      # or the analysis would adjust for a copy of Y (outcome leakage).
      if (!is.null(fam_cfg$source_var))
        fam_drop <- unique(c(fam_drop, as.character(fam_cfg$source_var)))
    }
  }
  special <- unique(c(
    cfg$analysis$exposure_var, cfg$analysis$outcome_var,
    cfg$analysis$id_var, cfg$analysis$cluster_var,
    cfg$analysis$weight_var, cfg$analysis$outcome_observed_var,
    cfg$analysis$extra_exclude_from_candidates,
    cfg$exposure$drop_from_candidates %||% character(0),
    cfg$outcome$drop_from_candidates  %||% character(0),
    fam_drop))
  setdiff(names(df), special)
}

# =============================================================================
# 4) CLUSTER-AWARE FOLD CONSTRUCTION
# =============================================================================
# Plain-English role: build cross-validation folds at the CLUSTER level (not
# the row level). Entire clusters stay together in train or validation so
# dependence within a cluster cannot leak across the CV boundary. We also
# greedily balance the number of exposed units across folds, which is
# important when exposure prevalence is only ~9%.

make_cluster_folds_balanced <- function(cluster, A, k = 5L, seed = 1L,
                                        weights = NULL, delta = NULL,
                                        balance_on_weights = FALSE) {
  cluster <- as.character(cluster)
  if (anyNA(cluster)) stop("Cluster column contains missing values.", call. = FALSE)
  A <- as.integer(A)
  if (is.null(weights)) weights <- rep(1, length(A))
  weights <- as.numeric(weights)
  weights[!is.finite(weights) | weights <= 0] <- 0
  if (is.null(delta)) delta <- rep(1L, length(A))
  delta <- as.integer(delta)

  cl_df <- data.frame(cluster = cluster, A = A, w = weights, delta = delta,
                      stringsAsFactors = FALSE)
  cl_stats <- stats::aggregate(
    x = list(n = rep(1L, nrow(cl_df)),
             a_sum = cl_df$A,
             a_w = cl_df$w * cl_df$A,
             obs = cl_df$delta,
             obs_a = cl_df$delta * cl_df$A),
    by = list(cluster = cl_df$cluster), FUN = sum)
  n_clusters <- nrow(cl_stats)
  k <- min(as.integer(k), n_clusters)
  if (k < 2L) stop("Need at least 2 unique clusters to make folds.", call. = FALSE)
  set.seed(seed)
  cl_stats <- cl_stats[sample.int(n_clusters), , drop = FALSE]
  if (isTRUE(balance_on_weights)) {
    cl_stats <- cl_stats[order(-cl_stats$a_w, -cl_stats$obs_a, -cl_stats$a_sum, -cl_stats$n), , drop = FALSE]
  } else {
    cl_stats <- cl_stats[order(-cl_stats$a_sum, -cl_stats$n), , drop = FALSE]
  }
  fold_a <- rep(0, k); fold_aw <- rep(0, k); fold_n <- rep(0, k); fold_obs_a <- rep(0, k)
  fold_assign_cluster <- integer(n_clusters)
  for (i in seq_len(n_clusters)) {
    if (isTRUE(balance_on_weights)) {
      best_aw <- which(fold_aw == min(fold_aw))
      best_obs <- best_aw[fold_obs_a[best_aw] == min(fold_obs_a[best_aw])]
      best <- best_obs[fold_n[best_obs] == min(fold_n[best_obs])]
    } else {
      best_a <- which(fold_a == min(fold_a))
      best <- best_a[fold_n[best_a] == min(fold_n[best_a])]
    }
    chosen <- sample(best, 1L)
    fold_assign_cluster[i] <- chosen
    fold_a[chosen] <- fold_a[chosen] + cl_stats$a_sum[i]
    fold_aw[chosen] <- fold_aw[chosen] + cl_stats$a_w[i]
    fold_obs_a[chosen] <- fold_obs_a[chosen] + cl_stats$obs_a[i]
    fold_n[chosen] <- fold_n[chosen] + cl_stats$n[i]
  }
  map <- stats::setNames(fold_assign_cluster, cl_stats$cluster)
  unname(as.integer(map[cluster]))
}

adjust_inner_k <- function(cluster, A, requested_k) {
  cluster <- as.character(cluster)
  n_clusters   <- length(unique(cluster))
  pos_clusters <- length(unique(cluster[A == 1L]))
  k <- min(as.integer(requested_k), n_clusters)
  if (pos_clusters >= 2L) k <- min(k, pos_clusters)
  k <- min(k, n_clusters)
  if (k < 2L) return(1L); k
}

# Build fold ids for the rough prescreen. If cluster_aware is FALSE (fast
# default for exploratory reruns), use simple random row-level folds.
make_rough_fold_ids <- function(n, K, seed, cluster_vec = NULL, A_vec = NULL,
                                cluster_aware = FALSE) {
  if (isTRUE(cluster_aware) && !is.null(cluster_vec) && !is.null(A_vec))
    return(make_cluster_folds_balanced(cluster_vec, A_vec, k = K, seed = seed))
  set.seed(seed)
  sample(rep(seq_len(K), length.out = n))
}


# =============================================================================
# 5) STAGE 1: ROUGH PRE-SCREEN
# =============================================================================
# Plain-English role: a fast first-pass filter. For every candidate variable
# we fit a single-variable cross-validated model predicting (a) the exposure
# and (b) the outcome, compute a score (logloss- or MSE-based R^2), then keep
# the variables whose scores exceed a "knee" cutoff. The purpose is to drop
# obvious noise columns before final TMLE. In the default
# pipeline, the rough screen is nested inside final TMLE folds. Cluster-awareness can be toggled with cfg$rough_prescreen$
# cluster_aware_folds.

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
                                fold = NULL, ridge_lambda = 1.0,
                                glmnet_maxit = 1000000L, glmnet_thresh = 1e-5) {
  # Grouped marginal binary screen: one score per raw variable. For a numeric
  # variable, the score model includes the value plus its missingness indicators;
  # for a factor, the score model includes all factor dummies plus missing/skip
  # indicators. This avoids dummy-level selection. Ridge-penalized logistic
  # regression avoids perfect-separation failures from sparse factors.
  keep <- !is.na(y); y <- y[keep]; X <- X[keep, , drop = FALSE]
  stopifnot(all(y %in% 0:1))
  n <- length(y)
  if (n < max(10L, K)) return(setNames(rep(NA_real_, ncol(X)), names(X)))
  if (is.null(fold)) { set.seed(seed); fold <- sample(rep(seq_len(K), length.out = n)) }
  else               fold <- fold[keep]
  clip    <- function(p) pmin(pmax(p, eps), 1 - eps)
  logloss <- function(y, p) -mean(y * log(p) + (1 - y) * log(1 - p))
  p0 <- numeric(n)
  for (k in seq_len(K)) {
    tr <- fold != k; te <- !tr
    p0[te] <- mean(y[tr])
  }
  ll0 <- logloss(y, clip(p0))

  fit_predict <- function(Xtr, ytr, Xte) {
    if (length(unique(ytr)) < 2L || nrow(Xtr) < 5L) {
      return(rep(mean(ytr), nrow(Xte)))
    }
    Xtr <- suppressWarnings(data.matrix(Xtr)); Xte <- suppressWarnings(data.matrix(Xte))
    Xtr[!is.finite(Xtr)] <- 0; Xte[!is.finite(Xte)] <- 0
    # drop zero-variance columns before glmnet. A constant column
    # (common for rare indicators within a fold) makes the design rank-
    # deficient and can stall convergence even under a strong ridge.
    # v6.21b FIX (false-negative bug): when exactly ONE non-constant column
    # survives, do NOT fall back to the null/mean model. Complete numeric
    # variables and clean binary/two-level factors legitimately reduce to a
    # single non-constant column (their missingness indicators are all-constant
    # and get dropped), and returning the mean would assign them a ~zero score
    # even when they are predictive -- penalizing exactly the best-measured
    # confounders. Fit a real one-column ridge logistic in that case.
    nc0 <- 0L
    if (ncol(Xtr) >= 1L) {
      sds <- apply(Xtr, 2, stats::sd)
      keep_cols <- is.finite(sds) & sds > 0
      nc0 <- sum(keep_cols)
      if (nc0 == 0L) return(rep(mean(ytr), nrow(Xte)))
      Xtr <- Xtr[, keep_cols, drop = FALSE]
      Xte <- Xte[, keep_cols, drop = FALSE]
    }
    if (nc0 == 1L) {
      # Single non-constant predictor (e.g. a complete numeric variable or a
      # clean binary/two-level factor whose missingness indicators were dropped
      # as constant). A plain logistic glm here is NOT separation-safe: a
      # perfectly-predictive binary indicator yields divergent coefficients and
      # extreme predictions. Use a GENUINE ridge logistic via glmnet so the
      # penalty bounds the coefficients under separation. glmnet historically
      # wants >= 2 columns, so we duplicate the standardized column; under the
      # ridge (alpha=0) the penalty splits the coefficient across the duplicates
      # without changing the fitted linear predictor. Falls back to the mean only
      # if glmnet is unavailable or the fit genuinely fails.
      xs_mu <- mean(Xtr[, 1]); xs_sd <- stats::sd(Xtr[, 1])
      if (!is.finite(xs_sd) || xs_sd <= 0) return(rep(mean(ytr), nrow(Xte)))
      ztr <- (Xtr[, 1] - xs_mu) / xs_sd
      zte <- (Xte[, 1] - xs_mu) / xs_sd
      if (!requireNamespace("glmnet", quietly = TRUE))
        return(rep(mean(ytr), nrow(Xte)))
      Ztr <- cbind(ztr, ztr); Zte <- cbind(zte, zte)   # duplicate for glmnet's >=2-col requirement
      fit1 <- tryCatch(
        glmnet::glmnet(x = Ztr, y = ytr, family = "binomial",
                       alpha = 0, lambda = ridge_lambda,
                       standardize = FALSE, intercept = TRUE,
                       thresh = glmnet_thresh, maxit = glmnet_maxit),
        error = function(e) NULL)
      if (is.null(fit1)) return(rep(mean(ytr), nrow(Xte)))
      pr <- tryCatch(
        as.numeric(stats::predict(fit1, newx = Zte, type = "response", s = ridge_lambda)),
        error = function(e) NULL)
      if (is.null(pr) || !all(is.finite(pr))) return(rep(mean(ytr), nrow(Xte)))
      return(pr)
    }
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      return(rep(mean(ytr), nrow(Xte)))
    }
    fit <- tryCatch(
      glmnet::glmnet(
        x = Xtr, y = ytr, family = "binomial",
        alpha = 0, lambda = ridge_lambda,
        standardize = TRUE, intercept = TRUE,
        thresh = glmnet_thresh, maxit = glmnet_maxit),
      error = function(e) NULL)
    if (is.null(fit)) return(rep(mean(ytr), nrow(Xte)))
    as.numeric(stats::predict(fit, newx = Xte, type = "response", s = ridge_lambda))
  }

  out <- vapply(names(X), function(nm) {
    if (grepl("(_missA|_miss97)$", nm)) return(NA_real_)
    Xnm <- tryCatch(build_single_var_screen_df(nm, X, impute_method = "median"),
                    error = function(e) NULL)
    if (is.null(Xnm) || ncol(Xnm) == 0L) return(NA_real_)
    p <- numeric(n)
    for (k in seq_len(K)) {
      tr <- fold != k; te <- !tr
      p[te] <- fit_predict(Xnm[tr, , drop = FALSE], y[tr], Xnm[te, , drop = FALSE])
    }
    ll <- logloss(y, clip(p))
    1 - ll / ll0
  }, numeric(1))
  out
}
screen_gauss_linear <- function(y, X, K = 5L, seed = 1L, fold = NULL) {
  keep <- is.finite(y); y <- y[keep]; X <- X[keep, , drop = FALSE]
  n <- length(y)
  if (n < max(10L, K)) return(setNames(rep(NA_real_, ncol(X)), names(X)))
  if (is.null(fold)) { set.seed(seed); fold <- sample(rep(seq_len(K), length.out = n)) }
  else               fold <- fold[keep]
  mse <- function(y, yhat) mean((y - yhat)^2)
  y0 <- numeric(n)
  for (k in seq_len(K)) { tr <- fold != k; te <- !tr; y0[te] <- mean(y[tr]) }
  mse0 <- mse(y, y0)
  vapply(names(X), function(nm) {
    col <- X[[nm]]
    if (is.matrix(col)) { if (ncol(col) == 1L) col <- col[, 1] else return(NA_real_) }
    if (grepl("(_missA|_miss97)$", nm)) return(NA_real_)
    yhat <- numeric(n)
    if (is.numeric(col)) {
      miss_names <- paste0(nm, c("_missA","_miss97"))
      miss_names <- miss_names[miss_names %in% names(X)]
      for (k in seq_len(K)) {
        tr <- fold != k; te <- !tr
        xt <- col[tr]; yt <- y[tr]
        good_y <- is.finite(yt); xt <- xt[good_y]; yt <- yt[good_y]
        if (length(yt) < 2L) { yhat[te] <- mean(y[tr], na.rm = TRUE); next }
        mu <- mean(xt, na.rm = TRUE)
        if (!is.finite(mu)) { yhat[te] <- mean(yt); next }
        xtr <- xt - mu; xtr[is.na(xtr)] <- 0
        Xtr <- matrix(1, nrow = length(yt), ncol = 1)
        if (!all(xtr == 0)) Xtr <- cbind(Xtr, xtr)
        if (length(miss_names) > 0L) {
          Mtr <- as.matrix(X[tr, miss_names, drop = FALSE])[good_y, , drop = FALSE]
          Mtr[is.na(Mtr)] <- 0
          Xtr <- cbind(Xtr, Mtr)
        } else { mtr <- is.na(xt); Xtr <- cbind(Xtr, mtr) }
        b <- tryCatch(stats::lm.fit(Xtr, yt)$coefficients,
                      error = function(e) rep(NA_real_, ncol(Xtr)))
        b[is.na(b)] <- 0
        xte <- col[te] - mu; xte[is.na(xte)] <- 0
        Xte <- matrix(1, nrow = sum(te), ncol = 1)
        if (!all(xtr == 0)) Xte <- cbind(Xte, xte)
        if (length(miss_names) > 0L) {
          Mte <- data.matrix(X[te, miss_names, drop = FALSE])
          Mte[is.na(Mte)] <- 0
          Xte <- cbind(Xte, Mte)
        } else { mte <- is.na(col[te]); Xte <- cbind(Xte, mte) }
        yhat[te] <- drop(Xte %*% b)
      }
    } else {
      f <- addNA(as.factor(col))
      for (k in seq_len(K)) {
        tr <- fold != k; te <- !tr
        yt <- y[tr]; ft <- f[tr]
        levm <- tapply(yt, ft, mean); ybar <- mean(yt)
        mk <- levm[as.character(f[te])]
        yhat[te] <- ifelse(is.na(mk), ybar, mk)
      }
    }
    mse1 <- mse(y, yhat); 1 - mse1 / mse0
  }, numeric(1))
}

# Orchestrator for the rough prescreen. Writes the shortlist to CSV and
# saves knee-curve PNGs so you can see where the cutoff landed.
# =============================================================================
# 6) FINAL W PREPROCESSING HELPERS
# =============================================================================
# Plain-English role: these helpers learn preprocessing rules on each final
# TMLE training fold and apply the frozen rules to that fold's validation rows.
# They are used by the nested fold-specific rough screen and the optional
# augmented multivariable elastic-net double-selection step inside final TMLE.

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

prep_numeric_train <- function(x, cfg_gp, cfg_pre) {
  n       <- length(x)
  obs     <- is.finite(x)
  n_obs   <- sum(obs); p_obs <- n_obs / n
  # v6.21b CONFIRMED INTENT: drop a numeric variable only if it fails BOTH the
  # absolute-count AND the proportion minimum (&&, not ||). This is the more
  # permissive rule: a variable observed in a small fraction of rows is retained
  # as long as it has enough absolute observations. This is safe here because the
  # dual missingness-indicator scheme explicitly models where the variable is
  # absent (it does not silently drop rows), so a sparse-but-real confounder is
  # adjusted-for where present and flagged-as-missing elsewhere. Empirically this
  # choice does not move the ATT estimate.
  if (n_obs < cfg_gp$numeric_min_observed_n && p_obs < cfg_gp$numeric_min_observed_prop)
    return(NULL)
  x_obs <- x[obs]
  win   <- winsorize_vec(x_obs, cfg_gp$winsor_probs)
  mu    <- mean(win, na.rm = TRUE); s <- stats::sd(win, na.rm = TRUE)
  if (!is.finite(mu)) mu <- 0; if (!is.finite(s) || s < cfg_pre$scale_eps) s <- 1
  list(mu = mu, s = s, win_q = stats::quantile(x_obs, probs = cfg_gp$winsor_probs,
                                               na.rm = TRUE, names = FALSE, type = 8),
       fill = stats::median(win, na.rm = TRUE))
}

apply_numeric_transform <- function(x, prep) {
  n <- length(x); obs <- is.finite(x)
  val <- x; val[!obs] <- prep$fill
  val <- pmin(pmax(val, prep$win_q[1]), prep$win_q[2])
  z   <- (val - prep$mu) / prep$s
  as.matrix(z)
}

prep_factor_train <- function(x, cfg_gp, cfg_pre, A = NULL) {
  x_chr <- as.character(x)
  is_na <- is.na(x_chr)

  other_label   <- cfg_pre$factor_other_label   %||% "_Other_"
  missing_label <- cfg_pre$factor_missing_label %||% "Missing"

  tab <- table(x_chr, useNA = "no")
  if (length(tab) == 0L) {
    return(list(
      levels = c(missing_label, other_label),
      other_label = other_label,
      missing_label = missing_label
    ))
  }

  rare_levels <- names(tab)[tab < cfg_gp$rare_level_min_n]
  x_chr[x_chr %in% rare_levels] <- other_label
  x_chr[is_na] <- missing_label

  # collapse levels with too few exposed observations in the current
  # training fold. This is fully fold-pure and prevents sparse factor levels
  # from causing separation in A~W and pi~W. Missing/_Other_ are retained.
  min_exp <- cfg_gp$factor_min_exposed_per_level %||% 0L
  if (!is.null(A) && min_exp > 0L && length(A) == length(x_chr)) {
    A01 <- suppressWarnings(as.integer(A))
    tab_all <- table(x_chr, useNA = "no")
    tab_exp <- table(x_chr[A01 == 1L], useNA = "no")
    exp_counts <- setNames(rep(0L, length(tab_all)), names(tab_all))
    exp_counts[names(tab_exp)] <- as.integer(tab_exp)
    low_exp_levels <- names(exp_counts)[
      exp_counts < min_exp & !(names(exp_counts) %in% c(missing_label, other_label))
    ]
    if (length(low_exp_levels) > 0L) x_chr[x_chr %in% low_exp_levels] <- other_label
  }

  final_levels <- unique(x_chr)

  if (length(final_levels) > cfg_gp$factor_max_levels_after_collapse) {
    n_top <- max(1L, cfg_gp$factor_max_levels_after_collapse - 2L)
    top_levels <- names(sort(table(x_chr), decreasing = TRUE))[seq_len(n_top)]
    keep_levels <- unique(c(top_levels, missing_label, other_label))
    x_chr[!(x_chr %in% keep_levels)] <- other_label
    final_levels <- unique(x_chr)
  }

  # Critical invariant: every factor recipe always contains Missing and
  # _Other_. Otherwise validation-only levels can become NA and model.matrix()
  # may silently drop rows, producing "number of rows of matrices must match".
  final_levels <- unique(c(final_levels, missing_label, other_label))

  list(
    levels = final_levels,
    other_label = other_label,
    missing_label = missing_label
  )
}
apply_factor_transform <- function(x, prep) {
  x_chr <- as.character(x)

  levels_safe <- unique(c(
    prep$levels,
    prep$missing_label,
    prep$other_label
  ))

  x_chr[is.na(x_chr)] <- prep$missing_label
  x_chr[!(x_chr %in% levels_safe)] <- prep$other_label

  f <- factor(x_chr, levels = levels_safe)
  mm <- stats::model.matrix(
    ~ f - 1,
    data = data.frame(f = f),
    na.action = stats::na.pass
  )

  if (nrow(mm) != length(x_chr)) stop(sprintf("apply_factor_transform(): factor produced %d rows but expected %d.", nrow(mm), length(x_chr)), call. = FALSE)
  colnames(mm) <- gsub("^f", "", colnames(mm))
  as.matrix(mm)
}

# Build design matrix for a set of columns. Returns processed matrix and
# a "group" id per column (all dummies for one factor share a group id).
# v5: enforces a HARD stop at hard_max_processed_columns to prevent RAM
# blowouts on the small-memory workstations used for this project.
build_grouped_design_train <- function(df, cfg_gp, cfg_pre, hard_max_cols = NULL, A = NULL) {
  if (ncol(df) == 0L) return(list(X = matrix(0, nrow = nrow(df), ncol = 0),
                                  group = integer(0), recipes = list()))
  recipes <- list(); mats <- list(); groups <- integer(0); grp <- 0L
  types <- infer_var_types(df, cfg_pre$numeric_imputation)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (identical(types[[nm]], "numeric")) {
      prep <- prep_numeric_train(as.numeric(col), cfg_gp, cfg_pre)
      if (is.null(prep)) next
      M <- apply_numeric_transform(as.numeric(col), prep)
      colnames(M) <- nm
      rec_obj <- list(type = "numeric", prep = prep)
    } else {
      prep <- prep_factor_train(col, cfg_gp, cfg_pre, A = A)
      if (is.null(prep) || length(prep$levels) < 2L) next
      M <- apply_factor_transform(col, prep)
      if (ncol(M) == 0L) next
      colnames(M) <- paste0(nm, "_", colnames(M))
      rec_obj <- list(type = "factor", prep = prep)
    }
    # Drop columns whose variance is degenerate.
    keep <- nonconstant_cols(M, tol = cfg_pre$constant_variance_tol)
    M <- M[, keep, drop = FALSE]
    if (ncol(M) == 0L) next
    recipes[[nm]] <- rec_obj
    grp <- grp + 1L
    groups <- c(groups, rep(grp, ncol(M))); mats[[nm]] <- M
    # v5: RAM-safety hard stop
    if (!is.null(hard_max_cols) && length(groups) > hard_max_cols) {
      stop(sprintf(
        "Processed design matrix exceeded hard_max_processed_columns = %d columns. ",
        hard_max_cols),
        "Tighten nested rough-screen caps, collapse long factors more aggressively, or ",
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
    if (!nm %in% names(df)) next
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

register_custom_learners <- function(cfg) {
  lcfg <- cfg$learners
  # Random forest with num.threads = 1L to avoid nested parallelism.
  if (isTRUE(lcfg$Q$use_ranger) || isTRUE(lcfg$g$use_ranger) || isTRUE(lcfg$pi$use_ranger)) {
    assign("SL.ranger.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        # v6: ranger is picky about class; force data.frame with all-numeric cols.
        if (!is.data.frame(X))    X    <- as.data.frame(X,    stringsAsFactors = FALSE)
        if (!is.data.frame(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
        rf <- ranger::ranger(
          y = Y, x = X, num.trees = lcfg$ranger$num.trees,
          probability = (family$family == "binomial"),
          case.weights = obsWeights, num.threads = 1L)
        pred <- stats::predict(rf, data = newX, num.threads = 1L)$predictions
        if (family$family == "binomial" && is.matrix(pred)) pred <- pred[, 2L]
        list(pred = as.numeric(pred), fit = list(object = rf))
      }, envir = .GlobalEnv)
  }
  # Gradient boosting with nthread = 1 for the same reason.
  if (isTRUE(lcfg$Q$use_xgboost) || isTRUE(lcfg$g$use_xgboost) || isTRUE(lcfg$pi$use_xgboost)) {
    assign("SL.xgboost.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        # data.matrix() converts factors to integer codes rather than
        # character strings, avoiding the 'class character' crash.
        # With the v5.1 expand_factors_to_numeric() call upstream this
        # should never encounter factors, but we keep it as a safety net.
        Xm <- data.matrix(X); newXm <- data.matrix(newX)
        storage.mode(Xm) <- "double"; storage.mode(newXm) <- "double"
        Xm[!is.finite(Xm)] <- 0; newXm[!is.finite(newXm)] <- 0
        dtrain <- xgboost::xgb.DMatrix(Xm, label = Y,
          weight = if (is.null(obsWeights)) rep(1, length(Y)) else obsWeights)
        obj <- if (family$family == "binomial") "binary:logistic" else "reg:squarederror"
        eval_metric <- if (family$family == "binomial") "logloss" else "rmse"
        fit <- xgboost::xgb.train(
          params = list(
            eta = lcfg$xgboost$shrinkage, max_depth = lcfg$xgboost$max_depth,
            min_child_weight = lcfg$xgboost$minobspernode, nthread = 1L,
            objective = obj, eval_metric = eval_metric),
          data = dtrain, nrounds = lcfg$xgboost$ntrees, verbose = 0)
        pred <- stats::predict(fit, newdata = xgboost::xgb.DMatrix(newXm))
        list(pred = as.numeric(pred), fit = list(object = fit))
      }, envir = .GlobalEnv)
  }
  if (isTRUE(lcfg$Q$use_earth)) {
    assign("SL.earth.fixed",
      function(Y, X, newX, family, obsWeights = NULL, ...) {
        # v6 Fix A: earth() rebuilds the call via a formula interface in some
        # versions, which calls model.frame() and requires a data.frame input.
        # Force X and newX to data.frame and ensure all columns are numeric.
        if (!is.data.frame(X))    X    <- as.data.frame(X,    stringsAsFactors = FALSE)
        if (!is.data.frame(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
        X[]    <- lapply(X,    function(c) if (is.numeric(c)) c else suppressWarnings(as.numeric(as.character(c))))
        newX[] <- lapply(newX, function(c) if (is.numeric(c)) c else suppressWarnings(as.numeric(as.character(c))))
        earth_obj <- earth::earth(x = X, y = Y, weights = obsWeights,
          glm = if (family$family == "binomial") list(family = stats::binomial()) else NULL,
          degree = lcfg$earth$degree %||% 2, nprune = lcfg$earth$nprune)
        pred <- stats::predict(earth_obj, newdata = newX,
          type = if (family$family == "binomial") "response" else "link")
        list(pred = as.numeric(pred), fit = list(object = earth_obj))
      }, envir = .GlobalEnv)
  }
  # v6.4 BUG FIX: stock SL.glmnet hardcodes alpha = 1 (pure LASSO) and ignores
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
        # glmnet wants a numeric matrix; model.matrix() expands any residual
        # factors to dummies. With v6.3 Fix 2 the data has already been
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
        foldid_glmnet <- NULL
        if (!is.null(id) && length(id) == length(Y) && length(unique(id)) >= 2L) {
          ids <- as.character(id)
          set.seed(91317L)
          id_tab <- data.frame(id = unique(ids), stringsAsFactors = FALSE)
          id_tab <- id_tab[sample.int(nrow(id_tab)), , drop = FALSE]
          id_tab$fold <- rep(seq_len(min(3L, nrow(id_tab))), length.out = nrow(id_tab))
          foldid_glmnet <- id_tab$fold[match(ids, id_tab$id)]
          if (length(unique(foldid_glmnet)) < 2L) foldid_glmnet <- NULL
        }
        x_glmnet <- if (requireNamespace("Matrix", quietly = TRUE)) Matrix::Matrix(Xm, sparse = TRUE) else Xm
        w_glmnet <- if (is.null(obsWeights)) rep(1, length(Y)) else obsWeights
        if (is.null(foldid_glmnet)) {
          fit <- glmnet::cv.glmnet(
            x = x_glmnet, y = Y, family = family$family,
            alpha = lcfg$glmnet$alpha %||% 1,
            nlambda = lcfg$glmnet$nlambda %||% 100L,
            weights = w_glmnet,
            nfolds = min(3L, max(2L, length(Y) - 1L)),
            maxit = lcfg$glmnet$maxit %||% 100000L)
        } else {
          fit <- glmnet::cv.glmnet(
            x = x_glmnet, y = Y, family = family$family,
            alpha = lcfg$glmnet$alpha %||% 1,
            nlambda = lcfg$glmnet$nlambda %||% 100L,
            weights = w_glmnet, foldid = foldid_glmnet,
            maxit = lcfg$glmnet$maxit %||% 100000L)
        }
        # lambda choice: "min" (default) or "1se"
        lam <- if (identical(lcfg$glmnet$lambda_choice %||% "min", "1se"))
                 fit$lambda.1se else fit$lambda.min
        pred <- stats::predict(fit, newx = newXm, s = lam,
          type = if (family$family == "binomial") "response" else "link")
        list(pred = as.numeric(pred), fit = list(object = fit, lambda = lam))
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
  if (isTRUE(tcfg$use_ranger)) lib <- c(lib, "SL.ranger.fixed")
  if (isTRUE(tcfg$use_xgboost)) lib <- c(lib, "SL.xgboost.fixed")
  if (isTRUE(tcfg$use_earth) && target == "Q") lib <- c(lib, "SL.earth.fixed")
  if (isTRUE(tcfg$use_gam))    lib <- c(lib, "SL.gam")
  if (isTRUE(tcfg$use_svm))    lib <- c(lib, "SL.svm")
  if (isTRUE(tcfg$use_nnet))   lib <- c(lib, "SL.nnet")
  if (length(lib) == 0L) lib <- c("SL.mean", "SL.glm")
  lib
}

# =============================================================================
# 8) FINAL CV-TMLE
# =============================================================================
# Plain-English role: the main estimator. Splits the data into outer folds
# (balanced on exposure at the cluster level), then for each fold:
#   (a) Runs nested cluster-aware rough screening on training rows only
#       to decide which raw covariates W to keep (no selection leakage).
#   (b) Builds a fold-specific processed design matrix W using only the
#       training rows' transformation recipes (levels, medians, winsor
#       bounds), and sanitizes column names.
#   (c) Fits three SuperLearner models on the training rows: Q (outcome),
#       g (propensity), pi (outcome-observation). Each uses cluster-aware
#       internal CV when cfg$final_tmle$cluster_aware_internal_cv = TRUE.
#   (d) Predicts Q1W, Q0W, QAW, gn, piAW, pi1W, pi0W on validation rows.
#   (e) Runs the TMLE fluctuation step on held-out predictions to produce
#       targeted Qbar1W*, Qbar0W*.
#   (f) Assembles the ATE estimator and the efficient influence function
#       on the bounded [0,1] outcome scale, back-transformed for variance.
#   (g) Uses cluster-robust variance to build a 95% CI.
# v5 ADDITIONS:
#   * Cluster-aware SuperLearner internal CV via validRows in cvControl.
#   * Per-fold checkpointing: fold results written to RDS so that if the
#     process crashes, a rerun resumes from the last completed fold.
#   * Per-fold SL summaries (coef, CV risk, fallback flags) written to CSV.
#   * Both-sided clipping on pi predictions; overlap product diagnostics.
#   * EIF residuals built on bounded [0,1] Y-scale, scaled back for
#     reporting, so heavy-tailed outliers above the 99th percentile do
#     not inflate the reported standard error.
#   * Fold-level assertions: every nuisance estimate is checked to be
#     finite and within admissible range as the fold completes.

prepare_final_analysis_data <- function(main_df, cfg) {
  df <- main_df
  A <- normalize_binary_var(df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  outcome_info <- prepare_modeled_outcome(
    df[[cfg$analysis$outcome_var]], cfg$analysis$outcome_type, cfg$analysis$outcome_var)
  Y_raw        <- outcome_info$values
  outcome_type <- outcome_info$type
  outcome_obs  <- outcome_info$observed
  delta_Y <- as.integer(outcome_obs)
  w_raw <- as.numeric(df[[cfg$analysis$weight_var]])
  # v6: invalid weights are already dropped at dataset-build time (Fix C).
  # This block remains as defensive code in case a user passes a pre-built
  # dataset that bypassed build_main_dataset().
  keep_w <- is.finite(w_raw) & w_raw > 0
  if (any(!keep_w)) {
    message(sprintf("[final TMLE] Defensive drop: %d rows with non-positive or missing weight.",
                    sum(!keep_w)))
    df      <- df[keep_w, , drop = FALSE]
    A       <- A[keep_w]; Y_raw <- Y_raw[keep_w]
    delta_Y <- delta_Y[keep_w]; w_raw <- w_raw[keep_w]
  }
  cluster <- df[[cfg$analysis$cluster_var]]
  # Bound Y to a finite range for TMLE. We use (min, upper-quantile) of
  # observed Y to define the range; the TMLE fluctuation runs on the scaled
  # [0,1] version of Y.
  y_obs_vals <- Y_raw[delta_Y == 1L & is.finite(Y_raw)]
  if (outcome_type == "binary") {
    y_lower <- 0 - cfg$outcome$continuous_bound_eps
    y_upper <- 1 + cfg$outcome$continuous_bound_eps
  } else {
    q_up    <- stats::quantile(y_obs_vals, probs = cfg$outcome$continuous_upper_quantile,
                               na.rm = TRUE, names = FALSE, type = 8)
    y_lower <- min(y_obs_vals, na.rm = TRUE) - cfg$outcome$continuous_bound_eps
    y_upper <- q_up + cfg$outcome$continuous_bound_eps
  }
  y_range <- y_upper - y_lower
  if (!is.finite(y_range) || y_range <= 0) stop("Invalid bounded-Y range.", call. = FALSE)
  Y_star <- rep(NA_real_, length(Y_raw))
  obs_i  <- which(delta_Y == 1L & is.finite(Y_raw))
  Y_star[obs_i] <- pmin(pmax((Y_raw[obs_i] - y_lower) / y_range, 1e-6), 1 - 1e-6)
  list(df = df, A = A, Y_raw = Y_raw, Y_star = Y_star, delta_Y = delta_Y,
       weights = w_raw, cluster = cluster, outcome_type = outcome_type,
       y_lower = y_lower, y_upper = y_upper, y_range = y_range)
}

make_final_cv_folds <- function(data_pack, cfg) {
  make_cluster_folds_balanced(
    data_pack$cluster, data_pack$A, k = cfg$final_tmle$vfolds,
    seed = seed_for(cfg, 7777L), weights = data_pack$weights,
    delta = data_pack$delta_Y,
    balance_on_weights = isTRUE(cfg$final_tmle$outer_fold_balance_on_weights))
}

# Learn and apply final-W preprocessing recipes --------------------------------
# These functions are fold-pure: all type decisions, missing-code handling,
# imputation, rare-level handling, winsorization, scaling, dummy levels, and
# column retention are learned from the training rows only.

learn_final_missing_recipe <- function(df, cfg_pre) {
  df <- classify_factors_by_uniques(df, cfg_pre)
  recipes <- list()
  thresh      <- cfg_pre$factor_max_substantive_level_threshold %||% 5L
  skip_low    <- as.character(cfg_pre$factor_skip_code_low      %||% 7L)
  ref_low     <- as.character(cfg_pre$factor_refusal_codes_low  %||% c(6L, 8L, 9L))
  skip_high   <- as.character(cfg_pre$factor_skip_code_high     %||% 97L)
  ref_high    <- as.character(cfg_pre$factor_refusal_codes_high %||% c(96L, 98L, 99L))
  miss_label  <- cfg_pre$factor_missing_label %||% "Missing"
  skip_label  <- cfg_pre$factor_skip_label    %||% "Skip"
  other_label <- cfg_pre$factor_other_label   %||% "_Other_"
  min_n_skip  <- cfg_pre$factor_special_code_min_n    %||% 30L
  min_pr_skip <- cfg_pre$factor_special_code_min_prop %||% 0.02

  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.factor(col) || is.character(col)) {
      xc <- as.character(col)
      all_miss <- c(skip_low, ref_low, skip_high, ref_high)
      xn_sub <- suppressWarnings(as.numeric(
        xc[!(xc %in% all_miss) & !is.na(xc)]))
      max_sub <- if (any(is.finite(xn_sub))) max(xn_sub, na.rm = TRUE) else 0
      use_low <- is.finite(max_sub) && max_sub <= thresh
      skip_code <- if (use_low) skip_low else skip_high
      ref_codes <- if (use_low) ref_low else ref_high
      miss97 <- !is.na(xc) & xc == skip_code
      missA  <- is.na(xc) | xc %in% ref_codes
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
      levs <- levels(f)
      levs <- unique(c(levs, other_label))
      recipes[[nm]] <- list(type = "factor", skip_code = skip_code,
                            ref_codes = ref_codes, retain_skip = retain_skip,
                            levels = levs, missing_label = miss_label,
                            skip_label = skip_label, other_label = other_label)
    } else {
      x <- suppressWarnings(as.numeric(col))
      suf <- abs(round(x)) %% 100
      miss97 <- !is.na(x) & suf == 97
      missA  <- is.na(x) | suf %in% c(96, 98, 99)
      fill <- compute_simple_impute(x[!(missA | miss97)], cfg_pre$numeric_imputation)
      recipes[[nm]] <- list(type = "numeric", fill = fill)
    }
  }
  recipes
}

apply_final_missing_recipe <- function(df, recipes, cfg_pre) {
  n <- nrow(df)
  out <- data.frame(row_id_internal = seq_len(n))[0]
  for (nm in names(recipes)) {
    rec <- recipes[[nm]]
    col <- if (nm %in% names(df)) df[[nm]] else rep(NA, n)
    if (identical(rec$type, "numeric")) {
      x <- suppressWarnings(as.numeric(col))
      suf <- abs(round(x)) %% 100
      miss97 <- !is.na(x) & suf == 97
      missA  <- is.na(x) | suf %in% c(96, 98, 99)
      x[missA | miss97] <- rec$fill
      out[[nm]] <- x
      out[[paste0(nm, "_missA")]]  <- as.integer(missA)
      out[[paste0(nm, "_miss97")]] <- as.integer(miss97)
    } else {
      xc <- as.character(col)
      miss97 <- !is.na(xc) & xc == rec$skip_code
      missA  <- is.na(xc) | xc %in% rec$ref_codes
      if (isTRUE(rec$retain_skip)) {
        xc[missA] <- NA_character_
        xc[miss97] <- rec$skip_label
      } else {
        xc[missA | miss97] <- NA_character_
      }
      xc[is.na(xc)] <- rec$missing_label
      xc[!(xc %in% rec$levels)] <- rec$other_label
      out[[nm]] <- factor(xc, levels = rec$levels)
      out[[paste0(nm, "_missA")]]  <- as.integer(missA)
      out[[paste0(nm, "_miss97")]] <- as.integer(miss97)
    }
  }
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
  aonly   <- flag("selected_by_exposure_only") | flag("selected_by_exposure_candidate_for_lasso")
  priority <- ifelse(lasso_y, 1L,
              ifelse(joint, 2L,
              ifelse(lasso_d, 3L,
              ifelse(outc, 4L,
              ifelse(delt, 5L,
              ifelse(lasso_a, 6L,
              ifelse(aonly, 7L, 8L)))))))
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
  # v6.22 (protected_W): union a protected block (e.g. baseline mental-health
  # variables) into the selected set and keep it at the FRONT, preserving the
  # data-driven screen. NO-OP when protected_W is NULL/empty (default).
  protected_W <- intersect(cfg$final_tmle$protected_W %||% character(0), names(train_df))
  if (length(protected_W))
    cand_vars <- c(protected_W, setdiff(cand_vars, protected_W))
  original_cand_vars <- cand_vars
  missing_selected <- setdiff(unique(selected_vars), names(train_df))
  if (length(cand_vars) == 0L) {
    stop("Final W construction received no valid selected variables. ",
         "This is a hard stop to avoid silent fallback to all candidates.",
         call. = FALSE)
  }
  t0 <- proc.time()[3]
  raw_tr <- train_df[, cand_vars, drop = FALSE]
  raw_te <- valid_df[, cand_vars, drop = FALSE]

  miss_rec <- learn_final_missing_recipe(raw_tr, cfg$preprocessing)
  proc_tr <- apply_final_missing_recipe(raw_tr, miss_rec, cfg$preprocessing)
  proc_te <- apply_final_missing_recipe(raw_te, miss_rec, cfg$preprocessing)

  fp_cfg <- cfg$final_preprocess
  A_factor <- normalize_binary_var(train_df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  des_tr <- build_grouped_design_train(
    proc_tr, fp_cfg, cfg$preprocessing,
    hard_max_cols = NULL, A = A_factor)
  if (ncol(des_tr$X) == 0L)
    stop("Final W preprocessing produced an empty training design matrix.",
         call. = FALSE)

  # Enforce a post-expansion processed-column cap. Raw-variable caps are not
  # enough with many factors; this cap prevents the factor multiplier from
  # producing a 300-400 column g/pi design in a rare-exposure sample.
  processed_cap <- processed_cap_override %||%
    cfg$final_tmle$final_max_processed_columns %||%
    cfg$final_tmle$hard_max_processed_columns %||% 300L
  if (ncol(des_tr$X) > processed_cap) {
    raw_map_tmp <- strip_missing_suffix(names(des_tr$recipes)[des_tr$group])
    col_count <- table(raw_map_tmp)
    cap_order <- order_vars_for_final_cap(cand_vars, priority_table)
    if (length(protected_W))   # protected vars ordered first and cap-exempt below
      cap_order <- c(intersect(protected_W, cap_order), setdiff(cap_order, protected_W))
    # Greedy priority-aware cap. protected_W vars are cap-EXEMPT: always kept AND
    # they do NOT consume the processed-column budget, so the ordinary data-driven
    # adjustment set still receives the full processed_cap and a forced-block
    # sensitivity (data-driven screen + protected block) stays interpretable --
    # forcing the block in does not silently shrink the data-driven set. Only the
    # NON-protected running total is checked against processed_cap.
    keep_vars <- character(0); used_np <- 0L   # used_np = NON-protected processed columns
    for (v in cap_order) {
      add <- if (v %in% names(col_count)) as.integer(col_count[[v]]) else 1L
      if (length(protected_W) && v %in% protected_W) {
        keep_vars <- c(keep_vars, v)                        # cap-exempt: budget untouched
      } else if (used_np + add <= processed_cap) {
        keep_vars <- c(keep_vars, v); used_np <- used_np + add
      }
    }
    if (length(keep_vars) == 0L)
      stop("Processed-column cap removed all final W variables; raise final_max_processed_columns or lower factor expansion.",
           call. = FALSE)
    dropped_vars <- setdiff(cand_vars, keep_vars)
    message(sprintf("  [final W] priority-aware processed-column cap %d trimmed selected raw variables from %d to %d; dropped %d.",
                    processed_cap, length(cand_vars), length(keep_vars), length(dropped_vars)))
    cand_vars <- keep_vars
    raw_tr <- train_df[, cand_vars, drop = FALSE]
    raw_te <- valid_df[, cand_vars, drop = FALSE]
    miss_rec <- learn_final_missing_recipe(raw_tr, cfg$preprocessing)
    proc_tr <- apply_final_missing_recipe(raw_tr, miss_rec, cfg$preprocessing)
    proc_te <- apply_final_missing_recipe(raw_te, miss_rec, cfg$preprocessing)
    # Total processed columns across all kept vars. Because protected_W vars are
    # cap-exempt (added on top of the data-driven cap), this can legitimately
    # exceed processed_cap; the rebuild cap is raised to accommodate them so the
    # design builder does not hard-stop. max() is a strict no-op when protected_W
    # is empty (then used_total == used_np <= processed_cap).
    used_total <- sum(vapply(keep_vars, function(v)
      if (v %in% names(col_count)) as.integer(col_count[[v]]) else 1L, integer(1)))
    if (length(protected_W) && used_total > processed_cap)
      message(sprintf("  [final W] protected_W adds %d protected cols on top of the %d-col data-driven set (design = %d processed cols).",
                      used_total - used_np, processed_cap, used_total))
    des_tr <- build_grouped_design_train(
      proc_tr, fp_cfg, cfg$preprocessing,
      hard_max_cols = max(processed_cap, used_total), A = A_factor)
    if (ncol(des_tr$X) == 0L)
      stop("Final W preprocessing produced an empty design after processed-column trimming.",
           call. = FALSE)
  }

  X_te <- apply_preprocess_recipe(proc_te, des_tr$recipes)

  tr_cols <- colnames(des_tr$X)
  missing_in_te <- setdiff(tr_cols, colnames(X_te))
  extra_in_te   <- setdiff(colnames(X_te), tr_cols)
  if (length(missing_in_te) > 0L) {
    add <- matrix(0, nrow = nrow(X_te), ncol = length(missing_in_te))
    colnames(add) <- missing_in_te
    X_te <- cbind(X_te, add)
  }
  if (length(extra_in_te) > 0L)
    X_te <- X_te[, !colnames(X_te) %in% extra_in_te, drop = FALSE]
  X_te <- X_te[, tr_cols, drop = FALSE]

  W_tr <- as.data.frame(des_tr$X, check.names = FALSE)
  W_te <- as.data.frame(X_te, check.names = FALSE)
  if (isTRUE(cfg$preprocessing$sanitize_column_names_for_model_matrix)) {
    clean_names <- make.unique(gsub("[^A-Za-z0-9_]", "_", names(W_tr)))
    names(W_tr) <- clean_names
    names(W_te) <- clean_names
  }
  list(train = W_tr, valid = W_te,
       n_raw = length(cand_vars),
       n_original_raw = length(original_cand_vars),
       kept_raw_vars = cand_vars,
       dropped_by_column_cap = setdiff(original_cand_vars, cand_vars),
       n_missing_selected = length(missing_selected),
       n_processed = ncol(W_tr),
       missing_in_validation = length(missing_in_te),
       extra_in_validation = length(extra_in_te),
       seconds = proc.time()[3] - t0)
}

# v6.12 high-dimensional safety helpers ---------------------------------------
truthy_or_false <- function(x) isTRUE(x)

compact_var_signature_for_duplicate <- function(x) {
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
    if (is.numeric(x) || is.integer(x)) {
      xn <- suppressWarnings(as.numeric(x))
      suf <- abs(round(xn)) %% 100
      obs <- is.finite(xn) & !(suf %in% c(96, 97, 98, 99))
      vals <- xn[obs]
    } else {
      xc <- as.character(x)
      obs <- !is.na(xc) & !(xc %in% c("6", "7", "8", "9", "96", "97", "98", "99"))
      vals <- xc[obs]
    }
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
      # Minority (rarest) observed level is the cell that drives rare-cell
      # positivity failures. Count its presence in each exposure arm.
      # v6.21b BUG FIX: when picking the minority level, exclude missing/refusal/
      # skip codes under BOTH schemes -- high {96,97,98,99} and low {6,8,9}
      # (refusal) + {7} (skip) -- using the same canonical codes as
      # add_dual_missingness_indicators()/the factor recoder. Previously the
      # numeric branch left low-scheme codes in `vals`, so a rare refusal/skip
      # code could be treated as the substantive minority level and trigger a
      # spurious rare_cell_two_arm deletion of an otherwise well-supported
      # variable. This exclusion is scoped to the two-arm minority determination
      # so it cannot disturb the global observed-count / cardinality checks above
      # (protecting genuine multi-level numeric scales whose values include 6-9).
      low_codes_chr  <- as.character(c(cfg$preprocessing$bad_codes_low %||% c(6L, 8L, 9L),
                                       cfg$preprocessing$factor_skip_code_low %||% 7L))
      high_codes_chr <- as.character(cfg$preprocessing$bad_codes_high %||% c(96L, 97L, 98L, 99L))
      vals_ta <- as.character(vals)[!(as.character(vals) %in% c(low_codes_chr, high_codes_chr))]
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
    sig <- vapply(kept_vars, function(v) compact_var_signature_for_duplicate(df[[v]]), character(1))
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

warn_internal_valid_rows <- function(validRows, A_vec, label, cfg, fold_id, delta_vec = NULL, weights = NULL) {
  if (!length(validRows)) return(invisible(NULL))
  if (is.null(delta_vec)) delta_vec <- rep(1L, length(A_vec))
  if (is.null(weights)) weights <- rep(1, length(A_vec))
  treated <- vapply(validRows, function(ix) sum(A_vec[ix] == 1L, na.rm = TRUE), integer(1))
  observed_treated <- vapply(validRows, function(ix) sum(A_vec[ix] == 1L & delta_vec[ix] == 1L, na.rm = TRUE), integer(1))
  rows <- vapply(validRows, length, integer(1))
  ess_t <- vapply(validRows, function(ix) kish_ess_safe(weights[ix][A_vec[ix] == 1L]), numeric(1))
  min_t <- cfg$final_tmle$internal_fold_min_treated_warning_n %||% 8L
  min_ot <- cfg$final_tmle$internal_fold_min_observed_treated_warning_n %||% 6L
  message(sprintf("    [fold %d] Internal %s folds support: rows=[%s], treated=[%s], obs_treated=[%s], ess_treated=[%s].",
                 fold_id, label, paste(rows, collapse = ","), paste(treated, collapse = ","),
                 paste(observed_treated, collapse = ","), paste(sprintf("%.1f", ess_t), collapse = ",")))
  if (any(treated < min_t)) {
    warning(sprintf("Fold %d internal %s CV has a validation fold with only %d treated rows (< %d).",
                    fold_id, label, min(treated), min_t))
  }
  if (any(observed_treated < min_ot)) {
    warning(sprintf("Fold %d internal %s CV has a validation fold with only %d observed treated rows (< %d).",
                    fold_id, label, min(observed_treated), min_ot))
  }
  invisible(NULL)
}

file_fingerprint <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list(path = path, exists = FALSE))
  info <- file.info(path)
  list(path = normalizePath(path, winslash = "/", mustWork = FALSE),
       exists = TRUE, size = unname(info$size), mtime = as.character(info$mtime))
}

make_wave1_cache_fingerprint <- function(cfg) {
  list(version = "v6.14_wave1_cache",
       paths = lapply(cfg$paths[c("wave1_inhome", "birth_records", "neighborhood_w1", "inschool_w1",
                                  "contextual_w1", "health_w1", "spatial_w1", "stchr95_w1",
                                  "polcon_w1", "weights_w1", "school_admin_w1")], file_fingerprint),
       preprocessing_core = cfg$preprocessing[c("factor_unique_threshold", "long_factors", "force_factor_prefixes")])
}

load_wave1_cache <- function(path, cfg) {
  obj <- readRDS(path)
  current_fp <- make_wave1_cache_fingerprint(cfg)
  if (is.list(obj) && !is.null(obj$data) && !is.null(obj$fingerprint)) {
    if (identical(obj$fingerprint, current_fp)) return(obj$data)
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
    saveRDS(list(data = w1_all, fingerprint = make_wave1_cache_fingerprint(cfg)), path)
  } else {
    saveRDS(w1_all, path)
  }
}

make_main_dataset_cache_fingerprint <- function(cfg, w1_all = NULL) {
  list(
    version = "v6.21b_main_cache_h1gh50_pre_to_num_fix",
    outcome_family = cfg$outcome$family,
    # include the ACTIVE family's full config so two runs of the same
    # family with different settings (e.g. a PassThrough negative control with a
    # different source_var) do not silently reuse each other's cached outcome Y.
    outcome_family_config = cfg$outcome$families[[cfg$outcome$family]],
    outcome_wave = cfg$outcome$current_wave %||% cfg$outcome$waves,
    family_member = cfg$outcome$family_member %||% NA_character_,
    exposure_cutpoint = cfg$exposure$cutpoint,
    weight_var = cfg$analysis$weight_var,
    cluster_var = cfg$analysis$cluster_var,
    exposure_var = cfg$analysis$exposure_var,
    outcome_var = cfg$analysis$outcome_var,
    # winsorization changes the stored weight column, so it must be
    # part of the cache identity or a changed quantile would silently reuse
    # a stale dataset.
    weight_winsor_quantile = cfg$analysis$weight_winsor_quantile %||% NA_real_,
    transform_time_variables = isTRUE(cfg$analysis$transform_time_variables),
    weight_winsor_renormalize = isTRUE(cfg$analysis$weight_winsor_renormalize),
    paths = cfg$paths,
    log_transform = cfg$outcome$log_transform,
    continuous_upper_quantile = cfg$outcome$continuous_upper_quantile,
    preprocessing_core = cfg$preprocessing[c("factor_unique_threshold", "numeric_missing_scheme",
                                             "factor_special_code_min_n", "factor_special_code_min_prop")],
    n_w1 = if (is.null(w1_all)) NA_integer_ else nrow(w1_all),
    names_w1 = if (is.null(w1_all)) character(0) else names(w1_all)
  )
}

load_main_dataset_cache <- function(path, cfg, w1_all) {
  obj <- readRDS(path)
  current_fp <- make_main_dataset_cache_fingerprint(cfg, w1_all)
  if (is.list(obj) && !is.null(obj$data) && !is.null(obj$fingerprint)) {
    if (identical(obj$fingerprint, current_fp)) return(obj$data)
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
    saveRDS(list(data = main_df, fingerprint = make_main_dataset_cache_fingerprint(cfg, w1_all)), path)
  } else {
    saveRDS(main_df, path)
  }
}

# Nested data-driven screen run on a final TMLE fold's training rows only.
# This is the selection step. It uses marginal rough
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

# v6.21 (Option A): de-duplicate the FULL candidate set by correlation
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
  if (ncol(X) == 0L || length(y) != nrow(X)) return(character(0))
  if (length(raw_map) != ncol(X)) {
    stop(sprintf("LASSO raw-variable map length (%d) does not match processed columns (%d).",
                 length(raw_map), ncol(X)), call. = FALSE)
  }
  names(raw_map) <- colnames(X)
  if (family == "binomial" && length(unique(y[is.finite(y)])) < 2L) return(character(0))
  if (length(unique(foldid)) < 2L) return(character(0))
  w <- if (is.null(weights)) rep(1, length(y)) else as.numeric(weights)
  w[!is.finite(w) | w <= 0] <- 1
  cvfit <- tryCatch(
    glmnet::cv.glmnet(
      x = if (requireNamespace("Matrix", quietly = TRUE)) Matrix::Matrix(X, sparse = TRUE) else X,
      y = y, family = family,
      alpha = cfg$final_tmle$lasso_screen_alpha %||% 0.25,
      nlambda = cfg$final_tmle$lasso_screen_nlambda %||% 50L,
      weights = w, foldid = foldid, standardize = FALSE,
      maxit = cfg$final_tmle$lasso_screen_glmnet_maxit %||% 100000L),
    error = function(e) NULL)
  if (is.null(cvfit)) return(character(0))
  lam <- switch(cfg$final_tmle$lasso_screen_lambda_choice %||% "lambda.min",
                "lambda.1se" = cvfit$lambda.1se,
                "lambda.min" = cvfit$lambda.min,
                cvfit$lambda.min)
  beta <- as.numeric(stats::coef(cvfit, s = lam))[-1L]
  nz <- which(is.finite(beta) & abs(beta) > 0)
  if (length(nz) == 0L) return(character(0))
  unique(strip_missing_suffix(raw_map[nz]))
}

run_nested_multivar_lasso_after_rough <- function(tr_df, rough_vars, cfg, fold_id,
                                                  A_tr, y_out, outcome_type,
                                                  outcome_obs, delta, cl_tr) {
  # Double-selection multivariable screen fit only on the final fold's
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

  # Build the grouped design. If factor expansion is too large, trim variables
  # from the end of the already priority-ranked rough list until the
  # processed-column cap is met. The upstream rough pool is ordered as
  # joint A/Y, outcome, missingness, then exposure candidates, so this
  # preferentially preserves confounding/outcome evidence over A-only signal.
  processed_cap <- cfg$final_tmle$lasso_screen_max_processed_cols %||% 120L
  current_vars <- rough_vars
  repeat {
    proc_cur <- proc[, intersect(names(proc), c(current_vars, paste0(current_vars, "_missA"), paste0(current_vars, "_miss97"))), drop = FALSE]
    des <- build_grouped_design_train(
      proc_cur, cfg$final_preprocess, cfg$preprocessing,
      hard_max_cols = NULL, A = A_tr)
    if (ncol(des$X) == 0L) {
      out <- empty; out$seconds <- proc.time()[3] - t0; return(out)
    }
    if (ncol(des$X) <= processed_cap || length(current_vars) <= (cfg$final_tmle$rough_min_total_vars %||% 10L)) break
    raw_map_tmp <- strip_missing_suffix(names(des$recipes)[des$group])
    col_count <- table(raw_map_tmp)
    keep <- character(0); used <- 0L
    for (v in current_vars) {
      add <- if (v %in% names(col_count)) as.integer(col_count[[v]]) else 1L
      if (used + add <= processed_cap) {
        keep <- c(keep, v); used <- used + add
      }
    }
    if (length(keep) == 0L || identical(keep, current_vars)) break
    message(sprintf("    [fold %s lasso] trimmed rough pool from %d to %d raw variables to respect processed-column cap %d.",
                    as.character(fold_id), length(current_vars), length(keep), processed_cap))
    current_vars <- keep
  }

  raw_map <- strip_missing_suffix(names(des$recipes)[des$group])
  X <- des$X
  storage.mode(X) <- "double"
  X[!is.finite(X)] <- 0
  K <- cfg$final_tmle$lasso_screen_folds %||% 3L
  wt <- as.numeric(tr_df[[cfg$analysis$weight_var]])

  # Exposure screen: included for double-selection. It is multivariable and
  # penalized, not a forced marginal A-only inclusion rule.
  selected_a <- character(0)
  if (length(unique(A_tr)) == 2L) {
    fold_a <- make_cluster_folds_balanced(cl_tr, A_tr, k = K,
                                          seed = seed_for(cfg, 14000L + ifelse(is.na(fold_id), 0L, fold_id)),
                                          weights = wt, delta = delta,
                                          balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
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
      fold_y <- make_cluster_folds_balanced(cl_tr[keep_y], A_tr[keep_y], k = K,
                                            seed = seed_for(cfg, 15000L + ifelse(is.na(fold_id), 0L, fold_id)),
                                            weights = wt[keep_y], delta = delta[keep_y],
                                            balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
      selected_y <- fit_nested_glmnet_screen(
        X[keep_y, , drop = FALSE], yy, fam_y, fold_y,
        wt[keep_y], raw_map, cfg)
    }
  }

  # Outcome-observation screen. This is included because missing Y is part of
  # the observed-data problem.
  selected_delta <- character(0)
  if (isTRUE(cfg$final_tmle$lasso_screen_include_delta) && length(unique(delta)) == 2L) {
    fold_d <- make_cluster_folds_balanced(cl_tr, A_tr, k = K,
                                          seed = seed_for(cfg, 16000L + ifelse(is.na(fold_id), 0L, fold_id)))
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
  cand <- get_candidate_vars(tr_df, cfg)
  if (length(cand) == 0L)
    stop("Nested rough screen has zero candidate variables.", call. = FALSE)
  prefilter_info <- prefilter_candidate_vars_for_screen(tr_df, cand, cfg, fold_id = fold_id)
  cand <- prefilter_info$vars

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

  # v6.21 (Option A): cluster the FULL candidate set BEFORE marginal scoring, so
  # large redundant blocks collapse to single representatives before the top-N
  # ranking caps are applied. This stops a block of ~85 mutually-correlated
  # contextual variables from consuming most of the ranking slots and crowding
  # out distinct weak confounders. Deterministic and seed-independent. Only the
  # surviving representatives are scored and ranked below.
  # IMPORTANT: cluster only the SUBSTANTIVE (non-missingness-indicator) columns.
  # add_dual_missingness_indicators() can roughly double the column count, which
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
    dedupe <- cluster_dedupe_candidates(
      subst_cols, X_base,
      cor_threshold = cfg$final_tmle$rough_redundancy_cor_threshold %||% 0.90,
      linkage = cfg$final_tmle$redundancy_linkage %||% "complete",
      impute_method = cfg$preprocessing$numeric_imputation,
      max_cluster_vars = cfg$final_tmle$cluster_dedupe_max_vars %||% 6000L,
      enable = TRUE)
    if (isTRUE(dedupe$skipped)) {
      message(sprintf("    [fold %s screen] pre-score clustering SKIPPED: %d substantive candidates exceed cap %d; scoring full set.",
                      as.character(fold_id), length(subst_cols),
                      cfg$final_tmle$cluster_dedupe_max_vars %||% 6000L))
    } else {
      # Keep representatives + singletons, plus indicators whose base variable
      # survived. An indicator survives iff its stripped base name is kept.
      kept_base   <- dedupe$kept
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
  outcome_obs <- outcome_info$observed
  delta <- as.integer(outcome_obs)
  cl_tr <- tr_df[[cfg$analysis$cluster_var]]

  K <- cfg$final_tmle$rough_folds %||% cfg$rough_prescreen$folds %||% 3L
  base_seed <- seed_for(cfg, 12000L + ifelse(is.na(fold_id), 0L, fold_id * 10L))
  fold_A <- make_rough_fold_ids(nrow(tr_df), K, base_seed,
                                cluster_vec = cl_tr, A_vec = A_tr,
                                cluster_aware = TRUE)
  fold_Y <- make_rough_fold_ids(nrow(tr_df), K, base_seed + 1L,
                                cluster_vec = cl_tr, A_vec = A_tr,
                                cluster_aware = TRUE)
  fold_D <- make_rough_fold_ids(nrow(tr_df), K, base_seed + 2L,
                                cluster_vec = cl_tr, A_vec = A_tr,
                                cluster_aware = TRUE)

  # A is scored only to build a joint A/Y ranking. A-only predictors are not
  # force-selected by default.
  t_A <- proc.time()[3]
  exposure_scores <- screen_binom_linear(
    A_tr, X_base, K = K, seed = base_seed,
    eps = cfg$rough_prescreen$binomial_eps, fold = fold_A,
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
      K = K, seed = base_seed + 1L,
      eps = cfg$rough_prescreen$binomial_eps,
      fold = fold_Y[outcome_obs],
      glmnet_maxit = cfg$rough_prescreen$glmnet_maxit %||% 1000000L,
      glmnet_thresh = cfg$rough_prescreen$glmnet_thresh %||% 1e-5,
      ridge_lambda = cfg$rough_prescreen$ridge_lambda %||% 1.0)
  } else {
    outcome_scores <- screen_gauss_linear(
      y_out, X_base, K = K, seed = base_seed + 1L, fold = fold_Y)
  }
  message(sprintf("    [fold %s screen] outcome score pass in %.1fs (%s).",
                  as.character(fold_id), proc.time()[3] - t_Y,
                  score_summary_msg(outcome_scores)))

  missing_scores <- setNames(rep(NA_real_, length(exposure_scores)), names(exposure_scores))
  t_D <- proc.time()[3]
  if (length(unique(delta)) == 2L) {
    missing_scores <- screen_binom_linear(
      delta, X_base, K = K, seed = base_seed + 2L,
      eps = cfg$rough_prescreen$binomial_eps, fold = fold_D,
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

  # Ranked candidate pool: joint and outcome/missingness predictors first.
  # Exposure predictors are included as candidates for the nested multivariable
  # A~W screen, but they are not marginally forced into final W.
  ranked_pool <- unique(c(vars_AY, vars_Y, vars_D, vars_A_for_lasso, vars_A_only))
  ranked_pool <- ranked_pool[ranked_pool %in% names(X_base)]
  pool_cap <- cfg$final_tmle$rough_candidate_pool_max %||% 175L
  if (identical(cfg$final_tmle$redundancy_method %||% "cluster", "cluster")) {
    # Redundancy was already removed by pre-score clustering of the full
    # candidate set, so X_base (and hence ranked_pool) contains only cluster
    # representatives. Here we only apply the size cap; no second clustering.
    # Define `red`/`red_frac` from the pre-score dedupe result so the downstream
    # selection-audit table (which references red$dropped and red_frac) is
    # populated correctly. Guard for the case where clustering was skipped or
    # disabled, in which case nothing was dropped as redundant.
    pre_dropped <- if (exists("dedupe") && is.list(dedupe)) dedupe$dropped %||% character(0) else character(0)
    red <- list(dropped = pre_dropped)
    red_denom <- length(red$dropped) + length(ranked_pool)
    red_frac <- if (red_denom > 0L) length(red$dropped) / red_denom else NA_real_
    n_pre_cap <- length(ranked_pool)
    rough_pool <- ranked_pool[seq_len(min(length(ranked_pool), pool_cap))]
    message(sprintf("    [fold %s screen] ranked pool (already de-duplicated pre-score): %d -> %d after size cap %d; %d collapsed pre-score.",
                    as.character(fold_id), n_pre_cap, length(rough_pool), pool_cap, length(red$dropped)))
  } else {
    red <- greedy_redundancy_filter(
      ranked_pool, X_base,
      max_vars = pool_cap,
      cor_threshold = cfg$final_tmle$rough_redundancy_cor_threshold %||% 0.80,
      impute_method = cfg$preprocessing$numeric_imputation,
      enable = isTRUE(cfg$final_tmle$rough_redundancy_control))
    rough_pool <- red$selected
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
      tr_df = tr_df, rough_vars = rough_pool, cfg = cfg, fold_id = fold_id,
      A_tr = A_tr, y_out = y_out, outcome_type = outcome_type,
      outcome_obs = outcome_obs, delta = delta, cl_tr = cl_tr)
    message(sprintf("    [fold %s screen] nested elastic-net double-selection in %.1fs: raw=%d, processed=%d, selected A=%d, Y=%d, delta=%d, union=%d.",
                    as.character(fold_id), lasso_info$seconds,
                    lasso_info$n_raw_screened, lasso_info$n_processed,
                    length(lasso_info$selected_a), length(lasso_info$selected_y),
                    length(lasso_info$selected_delta), length(lasso_info$selected)))
  }

  # Final selected variables: nested LASSO augments the diversified
  # rough pool; it never acts as a brittle hard bottleneck. This avoids
  # pathological runs where glmnet convergence/separation leaves only a few
  # variables and the final TMLE becomes underadjusted.
  # Nested LASSO is an augmenting double-selection screen, not a
  # replacement gate. The diversified rough pool stays eligible so that
  # convergence issues or rare-event folds cannot collapse the final W set
  # to a handful of variables.
  selected <- unique(c(lasso_info$selected, rough_pool))

  min_total <- cfg$final_tmle$rough_min_total_vars %||% 1L
  min_lasso_augmented <- if (isTRUE(cfg$final_tmle$nested_lasso_after_rough))
    cfg$final_tmle$lasso_screen_min_vars %||% 30L else min_total
  target_min <- max(min_total, min_lasso_augmented)

  if (length(selected) < target_min) {
    add_pool <- setdiff(ranked_pool, selected)
    add_needed <- target_min - length(selected)
    selected <- unique(c(selected, add_pool[seq_len(min(add_needed, length(add_pool)))]))
  }

  max_lasso <- if (isTRUE(cfg$final_tmle$nested_lasso_after_rough))
    cfg$final_tmle$lasso_screen_max_vars %||% cfg$final_tmle$rough_max_total_vars else
    cfg$final_tmle$rough_max_total_vars
  max_total <- min(cfg$final_tmle$rough_max_total_vars %||% 90L, max_lasso %||% 90L)
  if (length(selected) > max_total) {
    # Cap using double-selection and outcome/missingness priority; A-only
    # evidence is deliberately lower priority to avoid instrument enrichment.
    priority <- ifelse(selected %in% lasso_info$selected_y, 1L,
                ifelse(selected %in% vars_AY, 2L,
                ifelse(selected %in% lasso_info$selected_delta, 3L,
                ifelse(selected %in% vars_Y, 4L,
                ifelse(selected %in% vars_D, 5L,
                ifelse(selected %in% lasso_info$selected_a, 6L, 7L))))))
    best_score <- pmax(outcome_scores[selected], missing_scores[selected],
                       exposure_scores[selected], na.rm = TRUE)
    best_score[!is.finite(best_score)] <- -Inf
    selected <- selected[order(priority, -best_score)]
    selected <- selected[seq_len(max_total)]
  }

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

  audit_vars <- unique(c(names(exposure_scores), names(outcome_scores), names(missing_scores)))
  sel_tab <- data.frame(
    fold = fold_id,
    variable = audit_vars,
    exposure_score = as.numeric(exposure_scores[audit_vars]),
    outcome_score = as.numeric(outcome_scores[audit_vars]),
    delta_score = as.numeric(missing_scores[audit_vars]),
    in_rough_pool = audit_vars %in% rough_pool,
    dropped_redundant = audit_vars %in% red$dropped,
    redundancy_drop_fraction = red_frac,
    selected = audit_vars %in% selected,
    selected_by_joint_AY = audit_vars %in% vars_AY,
    selected_by_outcome = audit_vars %in% vars_Y,
    selected_by_delta = audit_vars %in% vars_D,
    selected_by_exposure_candidate_for_lasso = audit_vars %in% vars_A_for_lasso,
    selected_by_exposure_only = audit_vars %in% vars_A_only,
    selected_by_lasso_A = audit_vars %in% lasso_info$selected_a,
    selected_by_lasso_Y = audit_vars %in% lasso_info$selected_y,
    selected_by_lasso_delta = audit_vars %in% lasso_info$selected_delta,
    stringsAsFactors = FALSE)
  if (exists("prefilter_info") && !is.null(prefilter_info$log)) {
    sel_tab <- merge(sel_tab, prefilter_info$log, by = c("fold", "variable"),
                     all.x = TRUE, sort = FALSE)
    dropped_pf <- prefilter_info$log[!prefilter_info$log$prefilter_kept, , drop = FALSE]
    if (nrow(dropped_pf) > 0L) {
      dropped_rows <- data.frame(
        fold = dropped_pf$fold, variable = dropped_pf$variable,
        exposure_score = NA_real_, outcome_score = NA_real_, delta_score = NA_real_,
        in_rough_pool = FALSE, dropped_redundant = FALSE, redundancy_drop_fraction = red_frac,
        selected = FALSE, selected_by_joint_AY = FALSE, selected_by_outcome = FALSE,
        selected_by_delta = FALSE, selected_by_exposure_candidate_for_lasso = FALSE,
        selected_by_exposure_only = FALSE, selected_by_lasso_A = FALSE,
        selected_by_lasso_Y = FALSE, selected_by_lasso_delta = FALSE,
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

make_fold_checkpoint_fingerprint <- function(cfg, main_df, data_pack, outer_fold, fold_id) {
  list(
    pipeline_version = "v6.22_cluster_robust_multiseed",  # fold_support schema gained binary event counts and the checkpoint now stores cluster_assignments; old caches must not be reused (mismatched columns would break rbind).
    run_tag = build_run_tag(cfg),
    fold_id = fold_id,
    ids = as.character(main_df[[cfg$analysis$id_var]]),
    outer_fold = as.integer(outer_fold),
    exposure = as.integer(data_pack$A),
    delta_Y = as.integer(data_pack$delta_Y),
    weights = round(as.numeric(data_pack$weights), 8),
    y_lower = data_pack$y_lower,
    y_upper = data_pack$y_upper,
    final_tmle = cfg$final_tmle,
    final_preprocess = cfg$final_preprocess,
    learners = cfg$learners,
    preprocessing = cfg$preprocessing
  )
}

# Cluster-aware validRows for SuperLearner's internal CV. For each internal
# fold, the validation rows are whole clusters. This lets SuperLearner choose
# its meta-weights using held-out clusters, matching the outer-fold scheme.
build_cluster_valid_rows <- function(cluster_vec, A_vec, V, seed, weights = NULL,
                                     delta = NULL, balance_on_weights = FALSE) {
  fold_ids <- make_cluster_folds_balanced(cluster_vec, A_vec, k = V, seed = seed,
                                          weights = weights, delta = delta,
                                          balance_on_weights = balance_on_weights)
  lapply(seq_len(max(fold_ids)), function(v) which(fold_ids == v))
}

make_internal_fold_support <- function(validRows, A_vec, delta_vec = NULL, weights = NULL,
                                       label = "", outer_fold = NA_integer_) {
  if (is.null(delta_vec)) delta_vec <- rep(1L, length(A_vec))
  if (is.null(weights)) weights <- rep(1, length(A_vec))
  do.call(rbind, lapply(seq_along(validRows), function(j) {
    ix <- validRows[[j]]
    data.frame(
      outer_fold = outer_fold, nuisance = label, internal_fold = j,
      n = length(ix), treated = sum(A_vec[ix] == 1L, na.rm = TRUE),
      observed = sum(delta_vec[ix] == 1L, na.rm = TRUE),
      observed_treated = sum(A_vec[ix] == 1L & delta_vec[ix] == 1L, na.rm = TRUE),
      ess = kish_ess_safe(weights[ix]),
      ess_treated = kish_ess_safe(weights[ix][A_vec[ix] == 1L]),
      stringsAsFactors = FALSE)
  }))
}

# The main driver. Runs one outer fold at a time; checkpoints after each.
run_final_cv_tmle <- function(cfg, main_df, timers = NULL) {
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
  msg("  [TMLE] Registering custom SuperLearner wrappers (ranger/xgboost/earth)...", cfg = cfg)
  register_custom_learners(cfg)
  checkpoint_dir <- file.path(cfg$global$output_dir, cfg$global$checkpoint_subdir)
  ensure_output_dir(checkpoint_dir)
  msg(sprintf("  [TMLE] Checkpoint directory: %s", checkpoint_dir), cfg = cfg)

  msg("  [TMLE] Preparing final analysis data (bounding Y, building delta)...", cfg = cfg)
  data_pack <- prepare_final_analysis_data(main_df, cfg)
  main_df   <- data_pack$df
  A         <- data_pack$A
  Y_raw     <- data_pack$Y_raw
  Y_star    <- data_pack$Y_star
  delta_Y   <- data_pack$delta_Y
  weights   <- data_pack$weights
  cluster   <- data_pack$cluster
  outcome_type <- data_pack$outcome_type
  y_lower <- data_pack$y_lower; y_upper <- data_pack$y_upper
  y_range <- data_pack$y_range
  n <- nrow(main_df); V <- cfg$final_tmle$vfolds
  msg(sprintf("  [TMLE] Analytic sample: n=%d, clusters=%d, exposed=%d (%.1f%%), outcome-observed=%d (%.1f%%).",
    n, length(unique(cluster)), sum(A == 1L), 100 * mean(A),
    sum(delta_Y == 1L), 100 * mean(delta_Y)), cfg = cfg)
  msg(sprintf("  [TMLE] Y bounded to [%.3f, %.3f] (range=%.3f) for fluctuation step.",
    y_lower, y_upper, y_range), cfg = cfg)

  msg("  [TMLE] Constructing cluster-balanced outer folds...", cfg = cfg)
  outer_fold <- make_final_cv_folds(data_pack, cfg)
  msg(sprintf("    Outer fold sizes:      %s", paste(as.vector(table(outer_fold)), collapse = ", ")), cfg = cfg)
  msg(sprintf("    Treated per fold:      %s",
    paste(tapply(A, outer_fold, sum), collapse = ", ")), cfg = cfg)

  Qbar1W <- Qbar0W <- QbarAW <- rep(NA_real_, n)
  gn     <- rep(NA_real_, n); pi_AW <- pi_1W <- pi_0W <- rep(NA_real_, n)
  Y_star_obs <- Y_star

  per_fold_log <- list()
  fold_support_log <- list()
  internal_fold_support_log <- list()
  fold_times   <- numeric(V)
  selected_by_fold <- list()
  nested_selection_log <- list()
  cluster_assignment_log <- list()   # per-fold cluster assignments
  run_manifest_rows <- list()

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
    current_fp <- make_fold_checkpoint_fingerprint(cfg, main_df, data_pack, outer_fold, v)

    if (isTRUE(cfg$final_tmle$use_fold_checkpoints) && file.exists(fold_ck)) {
      msg(sprintf("\n  [fold %d/%d] Found checkpoint %s; validating fingerprint...", v, V, fold_ck), cfg = cfg)
      cached <- readRDS(fold_ck)
      if (!is.null(cached$fingerprint) && identical(cached$fingerprint, current_fp)) {
        te <- cached$valid_idx
        Qbar1W[te] <- cached$Qbar1W; Qbar0W[te] <- cached$Qbar0W
        QbarAW[te] <- cached$QbarAW
        gn[te]    <- cached$gn
        pi_AW[te] <- cached$pi_AW; pi_1W[te] <- cached$pi_1W; pi_0W[te] <- cached$pi_0W
        per_fold_log[[v]] <- cached$sl_log
        fold_times[v]     <- cached$fold_time
        selected_by_fold[[v]] <- cached$selected_vars
        if (!is.null(cached$selection_table)) nested_selection_log[[v]] <- cached$selection_table
        if (!is.null(cached$outer_support)) fold_support_log[[v]] <- cached$outer_support
        if (!is.null(cached$internal_support)) internal_fold_support_log[[paste0(v, "_cached")]] <- cached$internal_support
        # v6.22 (point 2): also restore cached cluster assignments, so a fold
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
    # v6.22 (point 3): per-fold binary event counts among OBSERVED rows
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
    # outcome-level events-per-parameter cap. Computed from the FULL
    # treated count (sum(A==1L)) scaled by the training fraction, so it is
    # identical for every fold (no fold-draw noise) and scales per outcome.
    epp_cap_arg <- NULL
    if (isTRUE(cfg$final_tmle$use_epp_cap %||% FALSE)) {
      train_frac  <- (cfg$final_tmle$vfolds - 1) / cfg$final_tmle$vfolds
      n_treat_tot <- sum(A == 1L, na.rm = TRUE)
      epp         <- cfg$final_tmle$events_per_parameter %||% 5L
      epp_floor   <- cfg$final_tmle$epp_cap_floor %||% 40L
      epp_cap_arg <- max(epp_floor, floor(n_treat_tot * train_frac / epp))
      if (v == 1L)
        msg(sprintf("  [final W] EPP cap (outcome-level): %d treated x %.2f / %d -> %d (floor %d).",
                    n_treat_tot, train_frac, epp, epp_cap_arg, epp_floor), cfg = cfg)
    }
    epp_cap_applied <- epp_cap_arg %||% (cfg$final_tmle$final_max_processed_columns %||% NA_integer_)
    W_pack <- build_final_W_train_valid(
      train_df = main_df[tr, , drop = FALSE],
      valid_df = main_df[te, , drop = FALSE],
      selected_vars = sel_vars,
      cfg = cfg,
      priority_table = nested_selection_log[[v]],
      processed_cap_override = epp_cap_arg)
    W_tr <- W_pack$train
    W_te <- W_pack$valid
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
      fold = v,
      treated_train = sum(A[tr] == 1L),
      treated_valid = sum(A[te] == 1L),
      clusters_valid = length(unique(cluster[te])),
      epp_cap_applied = epp_cap_applied,
      n_selected_vars = length(sel_vars),
      n_raw_into_W = W_pack$n_raw,
      n_processed_cols = W_pack$n_processed,
      cap_binding = isTRUE(W_pack$n_processed >= (epp_cap_applied %||% Inf)),
      ess_treated_train = kish_ess_safe(weights[tr][A[tr] == 1L]),
      stringsAsFactors = FALSE)

    A_tr <- A[tr]; A_te <- A[te]
    Y_star_tr <- Y_star[tr]; delta_tr <- delta_Y[tr]
    w_tr <- weights[tr]; cl_tr <- cluster[tr]

    # Internal CV folds: cluster-aware if toggled on
    V_int <- cfg$final_tmle$internal_superlearner_folds
    if (isTRUE(cfg$final_tmle$cluster_aware_internal_cv)) {
      validRows_Q  <- build_cluster_valid_rows(cl_tr[delta_tr == 1L],
                                               A_tr[delta_tr == 1L],
                                               V_int, base_seed + v,
                                               weights = w_tr[delta_tr == 1L],
                                               delta = rep(1L, sum(delta_tr == 1L)),
                                               balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
      validRows_g  <- build_cluster_valid_rows(cl_tr, A_tr, V_int, base_seed + v + 100L,
                                               weights = w_tr, delta = delta_tr,
                                               balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
      validRows_pi <- build_cluster_valid_rows(cl_tr, A_tr, V_int, base_seed + v + 200L,
                                               weights = w_tr, delta = delta_tr,
                                               balance_on_weights = isTRUE(cfg$final_tmle$internal_fold_balance_on_weights))
      cvQ  <- list(V = length(validRows_Q),  validRows = validRows_Q,  stratifyCV = FALSE, shuffle = FALSE)
      cvg  <- list(V = length(validRows_g),  validRows = validRows_g,  stratifyCV = FALSE, shuffle = FALSE)
      cvpi <- list(V = length(validRows_pi), validRows = validRows_pi, stratifyCV = FALSE, shuffle = FALSE)
      msg(sprintf("    [fold %d] Internal SL folds: Q=%d, g=%d, pi=%d.",
        v, length(validRows_Q), length(validRows_g), length(validRows_pi)), cfg = cfg)
      warn_internal_valid_rows(validRows_Q,  A_tr[delta_tr == 1L], "Q",  cfg, v,
                               delta_vec = rep(1L, sum(delta_tr == 1L)), weights = w_tr[delta_tr == 1L])
      warn_internal_valid_rows(validRows_g,  A_tr, "g",  cfg, v,
                               delta_vec = delta_tr, weights = w_tr)
      warn_internal_valid_rows(validRows_pi, A_tr, "pi", cfg, v,
                               delta_vec = delta_tr, weights = w_tr)
      internal_fold_support_log[[paste0(v, "_Q")]]  <- make_internal_fold_support(validRows_Q, A_tr[delta_tr == 1L], rep(1L, sum(delta_tr == 1L)), w_tr[delta_tr == 1L], "Q", v)
      internal_fold_support_log[[paste0(v, "_g")]]  <- make_internal_fold_support(validRows_g, A_tr, delta_tr, w_tr, "g", v)
      internal_fold_support_log[[paste0(v, "_pi")]] <- make_internal_fold_support(validRows_pi, A_tr, delta_tr, w_tr, "pi", v)
    } else {
      cvQ  <- list(V = V_int, stratifyCV = FALSE)
      cvg  <- list(V = V_int, stratifyCV = TRUE)
      cvpi <- list(V = V_int, stratifyCV = TRUE)
    }

    # --- Fit Q on rows with observed outcome --------------------------------
    idx_obs <- which(delta_tr == 1L)
    msg(sprintf("    [fold %d] Fitting Q on %d outcome-observed training rows...", v, length(idx_obs)), cfg = cfg)
    t_Q <- proc.time()[3]
    # v6 Fix B: data.frame() (not cbind) guarantees a data.frame result.
    # cbind(vector, data.frame) can return a matrix under R's method dispatch,
    # which breaks earth's formula path and other learners that call model.frame.
    X_Q_tr  <- data.frame(A = A_tr, W_tr, check.names = FALSE)[idx_obs, , drop = FALSE]
    X_Q_te_A1 <- data.frame(A = rep(1L, length(te)), W_te, check.names = FALSE)
    X_Q_te_A0 <- data.frame(A = rep(0L, length(te)), W_te, check.names = FALSE)
    X_Q_te_AA <- data.frame(A = A_te,                W_te, check.names = FALSE)
    Q_fit <- tryCatch(SuperLearner::SuperLearner(
      Y = Y_star_tr[idx_obs], X = X_Q_tr,
      newX = rbind(X_Q_te_A1, X_Q_te_A0, X_Q_te_AA),
      family = stats::gaussian(), SL.library = lib_Q,
      obsWeights = w_tr[idx_obs], cvControl = cvQ, verbose = FALSE),
      error = function(e) { warning("Q SuperLearner failed: ", conditionMessage(e)); NULL })
    n_te <- length(te)
    if (is.null(Q_fit)) {
      ybar <- mean(Y_star_tr[idx_obs], na.rm = TRUE)
      Qbar1W_v <- rep(ybar, n_te); Qbar0W_v <- rep(ybar, n_te); QbarAW_v <- rep(ybar, n_te)
      q_fallback <- TRUE
      q_coef <- numeric(0); q_risk <- numeric(0)
      msg(sprintf("      [fold %d] Q FALLBACK to grand mean %.4f (SL failed).", v, ybar), cfg = cfg)
    } else {
      # bound predictions before they enter the logit-fluctuation step.
      # SL.glmnet, SL.glm, and SL.earth use squared-error loss and can produce
      # predictions outside [0,1]. The bounds keep qlogis() offsets numerically
      # stable AND make miscalibrated learners visible to the SL meta-learner
      # (their CV-MSE penalizes them) instead of being silently rescued by a
      # boundary clip at 1e-6.
      q_lo <- cfg$final_tmle$Q_pred_lower %||% 0.005
      q_hi <- cfg$final_tmle$Q_pred_upper %||% 0.995
      preds <- as.numeric(Q_fit$SL.predict)
      n_clip_q <- sum(preds < q_lo | preds > q_hi, na.rm = TRUE)
      Qbar1W_v <- pmin(pmax(preds[1:n_te],                 q_lo), q_hi)
      Qbar0W_v <- pmin(pmax(preds[(n_te+1):(2*n_te)],      q_lo), q_hi)
      QbarAW_v <- pmin(pmax(preds[(2*n_te+1):(3*n_te)],    q_lo), q_hi)
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
    g_fit <- tryCatch(SuperLearner::SuperLearner(
      Y = A_tr, X = W_tr, newX = W_te, family = stats::binomial(),
      SL.library = lib_g, obsWeights = w_tr, cvControl = cvg, verbose = FALSE),
      error = function(e) { warning("g SuperLearner failed: ", conditionMessage(e)); NULL })
    if (is.null(g_fit)) {
      g_te <- rep(mean(A_tr), n_te); g_fallback <- TRUE
      g_coef <- numeric(0); g_risk <- numeric(0)
      msg(sprintf("      [fold %d] g FALLBACK to mean(A)=%.3f (SL failed).", v, mean(A_tr)), cfg = cfg)
    } else {
      g_te <- as.numeric(g_fit$SL.predict); g_fallback <- FALSE
      g_coef <- g_fit$coef; g_risk <- g_fit$cvRisk
      top_g <- names(g_coef)[which.max(g_coef)]
      msg(sprintf("      [fold %d] g fit in %.1fs. Top learner: %s (weight=%.3f).",
        v, proc.time()[3] - t_g, top_g, max(g_coef)), cfg = cfg)
    }
    g_te_preclip <- g_te
    g_te <- pmin(pmax(g_te, cfg$final_tmle$g_lower), cfg$final_tmle$g_upper)
    n_clip <- sum(g_te != g_te_preclip)
    msg(sprintf("      [fold %d] g predictions: range=[%.3f, %.3f], %d of %d clipped to [%g, %g].",
      v, min(g_te), max(g_te), n_clip, n_te, cfg$final_tmle$g_lower, cfg$final_tmle$g_upper), cfg = cfg)

    # --- Fit pi (outcome-observed | A, W) ---------------------------------
    msg(sprintf("    [fold %d] Fitting pi (outcome-observed | A,W)...", v), cfg = cfg)
    t_pi <- proc.time()[3]
    # v6 Fix B: data.frame() guarantees a data.frame; see Q block above.
    X_pi_tr    <- data.frame(A = A_tr,              W_tr, check.names = FALSE)
    X_pi_te_A1 <- data.frame(A = rep(1L, n_te),     W_te, check.names = FALSE)
    X_pi_te_A0 <- data.frame(A = rep(0L, n_te),     W_te, check.names = FALSE)
    X_pi_te_AA <- data.frame(A = A_te,              W_te, check.names = FALSE)
    pi_fit <- tryCatch(SuperLearner::SuperLearner(
      Y = delta_tr, X = X_pi_tr,
      newX = rbind(X_pi_te_A1, X_pi_te_A0, X_pi_te_AA),
      family = stats::binomial(), SL.library = lib_pi,
      obsWeights = w_tr, cvControl = cvpi, verbose = FALSE),
      error = function(e) { warning("pi SuperLearner failed: ", conditionMessage(e)); NULL })
    if (is.null(pi_fit)) {
      p <- mean(delta_tr); pi_1W_v <- rep(p, n_te); pi_0W_v <- rep(p, n_te); pi_AW_v <- rep(p, n_te)
      pi_fallback <- TRUE
      pi_coef <- numeric(0); pi_risk <- numeric(0)
      msg(sprintf("      [fold %d] pi FALLBACK to mean(delta_Y)=%.3f (SL failed).", v, p), cfg = cfg)
    } else {
      preds <- as.numeric(pi_fit$SL.predict)
      pi_1W_v <- preds[1:n_te]
      pi_0W_v <- preds[(n_te+1):(2*n_te)]
      pi_AW_v <- preds[(2*n_te+1):(3*n_te)]
      pi_fallback <- FALSE
      pi_coef <- pi_fit$coef; pi_risk <- pi_fit$cvRisk
      top_pi <- names(pi_coef)[which.max(pi_coef)]
      msg(sprintf("      [fold %d] pi fit in %.1fs. Top learner: %s (weight=%.3f).",
        v, proc.time()[3] - t_pi, top_pi, max(pi_coef)), cfg = cfg)
    }
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
              all(is.finite(gn[te])),     all(is.finite(pi_AW[te])))
    stopifnot(all(Qbar1W[te] > 0 & Qbar1W[te] < 1),
              all(Qbar0W[te] > 0 & Qbar0W[te] < 1))
    stopifnot(all(gn[te]    >= cfg$final_tmle$g_lower - 1e-10 &
                  gn[te]    <= cfg$final_tmle$g_upper + 1e-10))
    stopifnot(all(pi_AW[te] >= cfg$final_tmle$pi_lower - 1e-10 &
                  pi_AW[te] <= cfg$final_tmle$pi_upper + 1e-10))

    fold_times[v] <- proc.time()[3] - t0
    msg(sprintf("    [fold %d/%d DONE] total %.1fs.", v, V, fold_times[v]), cfg = cfg)

    # --- Per-fold SL log row -----------------------------------------------
    pack_coef <- function(nm, coefv) if (length(coefv) == 0L) ""
      else paste(paste0(nm, "=", sprintf("%.3f", as.numeric(coefv))), collapse = ";")
    sl_log <- data.frame(
      fold = v,
      n_train = length(tr), n_valid = n_te,
      n_selected_vars = length(sel_vars),
      Q_fallback  = q_fallback,  Q_coef  = pack_coef(names(q_coef),  q_coef),
      Q_cv_risk   = paste(sprintf("%.4f", q_risk), collapse = ";"),
      g_fallback  = g_fallback,  g_coef  = pack_coef(names(g_coef),  g_coef),
      g_cv_risk   = paste(sprintf("%.4f", g_risk), collapse = ";"),
      pi_fallback = pi_fallback, pi_coef = pack_coef(names(pi_coef), pi_coef),
      pi_cv_risk  = paste(sprintf("%.4f", pi_risk), collapse = ";"),
      fold_seconds = fold_times[v],
      stringsAsFactors = FALSE)
    per_fold_log[[v]] <- sl_log
    if (isTRUE(cfg$final_tmle$fail_on_nuisance_fallback %||% FALSE) &&
        (isTRUE(q_fallback) || isTRUE(g_fallback) || isTRUE(pi_fallback))) {
      stop(sprintf("Fold %d used a nuisance fallback (Q=%s, g=%s, pi=%s). Set fail_on_nuisance_fallback=FALSE to allow this diagnostic run.",
                   v, q_fallback, g_fallback, pi_fallback), call. = FALSE)
    }

    if (isTRUE(cfg$final_tmle$use_fold_checkpoints)) {
      saveRDS(list(
        valid_idx = te, Qbar1W = Qbar1W_v, Qbar0W = Qbar0W_v, QbarAW = QbarAW_v,
        gn = g_te, pi_AW = pi_AW_v, pi_1W = pi_1W_v, pi_0W = pi_0W_v,
        sl_log = sl_log, fold_time = fold_times[v],
        selected_vars = sel_vars,
        selection_table = nested_selection_log[[v]],
        cluster_assignments = rough_sel$cluster_assignments,  # v6.22 (point 2): persist for cached-fold audits
        outer_support = fold_support_log[[v]],
        internal_support = {
          nm_int <- grep(paste0("^", v, "_"), names(internal_fold_support_log), value = TRUE)
          if (length(nm_int)) do.call(rbind, internal_fold_support_log[nm_int]) else NULL
        },
        fingerprint = current_fp), fold_ck)
    }
    if (isTRUE(cfg$final_tmle$gc_after_fold %||% TRUE)) {
      rm(list = intersect(c("W_tr", "W_te", "W_pack", "Q_fit", "g_fit", "pi_fit",
                            "X_Q_tr_AW", "X_Q_te_A1", "X_Q_te_A0", "X_Q_te_AA",
                            "X_g_tr", "X_g_te", "X_pi_tr", "X_pi_te_A1",
                            "X_pi_te_A0", "X_pi_te_AA"), ls()), inherits = FALSE)
      invisible(gc(verbose = FALSE))
    }
  }

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
  # Using logit-link fluctuation for bounded Y (Gruber & van der Laan 2010).
  fluct_fit <- tryCatch(
    stats::glm(Y_star_obs[idx_up] ~ -1 + offset(stats::qlogis(Q_obs)) + H_obs,
               family = stats::quasibinomial(), weights = weights[idx_up]),
    error = function(e) NULL)
  eps_hat <- if (is.null(fluct_fit)) 0 else unname(stats::coef(fluct_fit)["H_obs"])
  if (!is.finite(eps_hat)) eps_hat <- 0
  msg(sprintf("    Fluctuation epsilon = %.6f (near-zero = initial Q was well-calibrated).", eps_hat), cfg = cfg)

  # v6.3 BUG FIX: counterfactual updates must use INTERVENTION-SPECIFIC
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

  # Weighted ATE on original scale. Also compute reader-facing
  # comparators from the same cross-fitted nuisance estimates:
  #   - initial plug-in using Qbar before targeting;
  #   - initial AIPW/one-step estimator using Qbar, g, and pi.
  # These are diagnostics only; the headline estimate remains the TMLE.
  w_norm <- weights / mean(weights)
  Qbar1W_orig <- Qbar1W * y_range + y_lower
  Qbar0W_orig <- Qbar0W * y_range + y_lower
  QbarAW_orig <- QbarAW * y_range + y_lower
  psi_plugin_initial <- sum(w_norm * (Qbar1W_orig - Qbar0W_orig)) / length(weights)
  aipw_resid <- ((A / gn) - ((1 - A) / (1 - gn))) *
                (delta_Y / pi_AW) *
                ifelse(delta_Y == 1L & is.finite(Y_raw), Y_raw - QbarAW_orig, 0)
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
  # ATT via the doubly-robust, censoring-adjusted one-step estimator.
  # Estimand: psi_ATT = E[Y(1) - Y(0) | A=1].
  # Efficient influence function (Hahn 1998 ATT EIF, IPCW-augmented for the
  # censored outcome; verified in the accompanying derivation):
  #   D_ATT(O) = (1/p) * [ A*(Q1 - Q0 - psi)
  #                        + A*(Delta/pi1)*(Y - Q1)
  #                        - (1-A)*(g/(1-g))*(Delta/pi0)*(Y - Q0) ]
  # where p = P(A=1), Qa = E[Y|A=a,W,Delta=1], pi_a = P(Delta=1|A=a,W).
  # IMPORTANT correctness points (all verified against this codebase):
  #  - The contrast and the residuals BOTH use the INITIAL outcome regression
  #    Qbar (NOT the ATE-targeted Qstar). The one-step EIF is defined at the
  #    initial nuisance; reusing the ATE fluctuation here would target the
  #    wrong estimand. (This fixes a prior Qstar/Qbar inconsistency.)
  #  - A single pi_AW vector is correct: pi_AW[i] = pi(A_i, W_i), the censoring
  #    model (fit WITH A as a predictor) evaluated at each row's OBSERVED A.
  #    Because the treated residual is masked by A and the control residual by
  #    (1-A), each row only ever uses its own-arm censoring probability, so
  #    pi_AW delivers pi1 to treated rows and pi0 to control rows exactly.
  #  - The cluster-robust sandwich SE reuses the same aggregation as the ATE
  #    SE; the N^2 normalization cancels the weight normalization exactly
  #    (verified), yielding the exact EIF-based variance for the weighted
  #    ratio estimand.
  # The block also runs two self-checks that fail loudly if the EIF is
  # miscoded: (1) the weighted EIF must have mean ~0 (Neyman centering);
  # (2) in the no-censoring limit (Delta/pi -> 1) the estimate must reduce to
  # the plug-in/uncensored ATT. These catch implementation errors that a
  # purely algebraic proof cannot.
  # --------------------------------------------------------------------------
  att_estimate <- NA_real_; att_se <- NA_real_
  att_eif_mean <- NA_real_; att_nocens_check <- NA_real_
  att_components <- NULL
  if (isTRUE(cfg$final_tmle$report_att %||% FALSE)) {
    p_treat_w <- sum(w_norm * A) / length(weights)        # weighted P(A=1)
    # BOTH contrast and residuals use the INITIAL Qbar (see note above).
    att_summand <- (Qbar1W_orig - Qbar0W_orig)            # contrast, all rows
    # v6.21b ESTIMAND-CONSISTENCY FIX: the estimand is ATT_bounded_Y_q0.990, and
    # the Q nuisances were trained on the bounded outcome Y_star (Y winsorized at
    # y_upper = q0.990 cap, mapped to [0,1]) then back-transformed via
    # QbarAW_orig = QbarAW * y_range + y_lower, so Qbar predictions live in the
    # BOUNDED original-scale support [y_lower, y_upper]. The residual must use the
    # bounded outcome on the original scale, not the raw uncapped Y, otherwise a
    # high outlier contributes (raw Y) - (capped prediction), an estimand
    # mismatch (raw-outcome residual against a bounded Q). Using the bounded
    # outcome makes the one-step ATT correspond to the stated bounded estimand and
    # keeps the residual in the same support as the predictions. For observed rows
    # Y_bounded_orig == Y_star * y_range + y_lower; we compute it directly by
    # winsorizing Y_raw at the same [y_lower, y_upper] used to fit Q.
    Y_bounded_orig <- pmin(pmax(Y_raw, y_lower), y_upper)
    resid_obs   <- ifelse(delta_Y == 1L & is.finite(Y_raw), Y_bounded_orig - QbarAW_orig, 0)
    # IPCW-augmented residual. pi_AW is arm-specific (pi(A_i,W_i)); masking by
    # A / (1-A) routes pi1 to treated and pi0 to controls.
    att_resid <- (A * resid_obs / pi_AW) -
                 ((1 - A) * (gn / (1 - gn)) * resid_obs / pi_AW)
    psi_att <- sum(w_norm * (A * att_summand + att_resid)) / sum(w_norm * A)
    # v6.21b (support monitor): the headline ATT is a one-step EIF estimator on
    # the initial Qbar (NOT an ATT-targeted TMLE -- using the ATE-targeted Qstar
    # here would contaminate the ATT with ATE-directed fluctuation, the verified
    # v6.18->v6.19 bug). The Q1-Q0 CONTRAST is bounded (Qbar*_orig are back-
    # transformed from [0,1]), but the IPCW-augmented residual is added linearly,
    # so the one-step form is not range-guaranteed the way a logistic-fluctuation
    # TMLE is. In principle a large weighted control residual could push the
    # implied treated counterfactual mean E[Y(0)|A=1] outside the outcome support
    # [y_lower, y_upper]. We MONITOR this rather than silently assume it: compute
    # the implied counterfactual treated-arm mean under control and flag if it
    # leaves support. Empirically it has stayed well inside; if this ever fires,
    # an ATT-specific pooled TMLE fluctuation (with the ATT clever covariate)
    # would be the principled bounded alternative.
    cf_treated_under_control <- {
      # E[Y(0)|A=1] implied by the one-step: weighted mean over treated of the
      # control-counterfactual prediction plus the control-residual correction.
      num <- sum(w_norm * (A * Qbar0W_orig)) +
             sum(w_norm * ((1 - A) * (gn / (1 - gn)) * resid_obs / pi_AW))
      den <- sum(w_norm * A)
      if (is.finite(den) && den > 0) num / den else NA_real_
    }
    att_support_ok <- is.finite(cf_treated_under_control) &&
      cf_treated_under_control >= y_lower - 1e-8 &&
      cf_treated_under_control <= y_upper + 1e-8
    if (!isTRUE(att_support_ok))
      warning(sprintf(
        "ATT one-step implied counterfactual E[Y(0)|A=1] = %.4f is OUTSIDE outcome support [%.4f, %.4f]; consider an ATT-specific bounded TMLE fluctuation.",
        cf_treated_under_control, y_lower, y_upper))
    # Efficient influence function evaluated at the point estimate.
    D_att   <- (1 / p_treat_w) * ( A * (att_summand - psi_att) + att_resid )
    D_att_w <- w_norm * D_att
    cl_eic_att <- tapply(D_att_w, cluster, sum)
    J_att <- length(unique(cluster))
    fsc_att <- J_att / max(1, J_att - 1L)
    att_estimate <- psi_att
    att_se <- sqrt(fsc_att * sum(cl_eic_att^2) / (length(weights)^2))
    # v6.21b (defensive): also compute a CENTERED cluster-sum SE. The EIF is
    # centered in expectation, so summing squared cluster contributions is
    # asymptotically valid; but in finite samples with cross-fitting and clipping
    # tiny numerical drift can remain. Subtracting the mean cluster contribution
    # before squaring guards against that. The two SEs should be nearly identical;
    # a material difference signals a centering problem and is surfaced below.
    cl_centered <- cl_eic_att - mean(cl_eic_att)
    att_se_centered <- sqrt(fsc_att * sum(cl_centered^2) / (length(weights)^2))
    if (is.finite(att_se) && is.finite(att_se_centered) && att_se > 0 &&
        abs(att_se_centered - att_se) / att_se > 0.01)
      warning(sprintf(
        "ATT centered vs uncentered cluster SE differ by >1%% (%.4g vs %.4g); possible EIF centering drift.",
        att_se_centered, att_se))

    # --- Self-check 1: Neyman centering. The weighted EIF must average to ~0.
    att_eif_mean <- sum(w_norm * D_att) / length(weights)
    eif_tol <- 1e-6 * max(1, abs(psi_att))
    if (!is.finite(att_eif_mean) || abs(att_eif_mean) > eif_tol) {
      warning(sprintf(
        "ATT EIF centering check FAILED: weighted mean(D_att) = %.3e (tol %.1e). SE may be invalid.",
        att_eif_mean, eif_tol))
    }
    # --- Self-check 2: complete-data recomputation (censoring-term sanity
    # check). Recompute the ATT with the IPCW weights Delta/pi set to 1, i.e. as
    # if every outcome were observed. This is NOT a formal "no-censoring limit"
    # (the data ARE censored and this recomputation is not the estimand); it is a
    # finite-sample sanity check that the censoring (IPCW) term is wired
    # correctly. The complete-data recomputation must be finite and the actual
    # (IPCW-weighted) estimate must not diverge wildly from it; a large gap flags
    # a censoring-term error. Uses the same bounded outcome as the headline.
    resid_full   <- ifelse(is.finite(Y_raw), Y_bounded_orig - QbarAW_orig, 0)
    att_resid_nc <- (A * resid_full) - ((1 - A) * (gn / (1 - gn)) * resid_full)
    att_nocens_check <- sum(w_norm * (A * att_summand + att_resid_nc)) / sum(w_norm * A)

    msg(sprintf("    ATT (one-step EIF estimator): %.4f (cluster-robust EIF SE %.4f, J=%d).",
                att_estimate, att_se, J_att), cfg = cfg)
    msg(sprintf("      [ATT check] EIF weighted mean = %.2e (tol %.1e) -> %s.",
                att_eif_mean, eif_tol,
                if (is.finite(att_eif_mean) && abs(att_eif_mean) <= eif_tol) "PASS" else "FAIL"),
        cfg = cfg)
    msg(sprintf("      [ATT check] complete-data recomputation (IPCW->1) = %.4f (IPCW-weighted = %.4f; gap %.4f).",
                att_nocens_check, att_estimate, att_nocens_check - att_estimate), cfg = cfg)
    # store the canonical per-row ATT building blocks so the diagnostics
    # section computes every ATT diagnostic from the SAME numbers used for the
    # headline (no re-derivation, no risk of divergence). All vectors are
    # full-length, pooled, cross-fitted.
    att_components <- list(
      psi_att = psi_att, att_se = att_se, att_se_centered = att_se_centered,
      p_treat_w = p_treat_w,
      D_att = D_att,                       # efficient influence function (per row)
      att_summand = att_summand,           # Q1-Q0 contrast (per row)
      att_resid = att_resid,               # IPCW-augmented residual (per row)
      resid_obs = resid_obs,               # observed-arm residual Y - Qbar_AW
      cl_eic_att = cl_eic_att, J_att = J_att, fsc_att = fsc_att,
      eif_mean = att_eif_mean, nocens = att_nocens_check,
      cf_treated_under_control = cf_treated_under_control,
      support_ok = att_support_ok, y_lower = y_lower, y_upper = y_upper,
      # raw per-row vectors (pooled, cross-fitted) so all ATT diagnostics use
      # one canonical source consistent with the headline estimate:
      A = A, Y_raw = Y_raw, delta_Y = delta_Y, cluster = cluster,
      weights = weights, w_norm = w_norm, outer_fold = outer_fold,
      gn = gn, pi_AW = pi_AW, pi_1W = pi_1W, pi_0W = pi_0W,
      Qbar1W_orig = Qbar1W_orig, Qbar0W_orig = Qbar0W_orig,
      QbarAW_orig = QbarAW_orig, y_lower = y_lower, y_upper = y_upper)
  }

  # Overlap-trimmed ATE: restrict to the common-support propensity band.
  # when retarget_trimmed = TRUE this is a FORMALLY TARGETED estimate
  # of the trimmed estimand -- the fluctuation epsilon is re-fit on the
  # in-support observed rows and the counterfactual means are re-updated with
  # that epsilon, rather than reusing the full-sample epsilon. The EIF is then
  # built from the re-targeted predictions on the trimmed sample.
  trim_estimate <- NA_real_; trim_se <- NA_real_; n_trim <- NA_integer_
  trim_eps <- NA_real_
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
          fit_t <- tryCatch(
            stats::glm(Y_star_obs[idx_t] ~ -1 + offset(stats::qlogis(QbarAW[idx_t])) + H_obs_t,
                       family = stats::quasibinomial(), weights = weights[idx_t]),
            error = function(e) NULL)
          eps_t <- if (is.null(fit_t)) 0 else unname(stats::coef(fit_t)["H_obs_t"])
          if (!is.finite(eps_t)) eps_t <- 0
          trim_eps <- eps_t
          Q1_t <- stats::plogis(stats::qlogis(Qbar1W[keep_t]) + eps_t * H1_all[keep_t])
          Q0_t <- stats::plogis(stats::qlogis(Qbar0W[keep_t]) + eps_t * H0_all[keep_t])
          msg(sprintf("    Trimmed ATE re-targeting epsilon = %.6f (on %d in-support observed rows).",
                      eps_t, length(idx_t)), cfg = cfg)
        } else {
          msg("    Trimmed ATE re-targeting skipped (insufficient in-support observed variation); reusing full-sample epsilon.", cfg = cfg)
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
      D_t_w <- w_t * D_t
      cl_eic_t <- tapply(D_t_w, cluster[keep_t], sum)
      J_t <- length(unique(cluster[keep_t]))
      fsc_t <- J_t / max(1, J_t - 1L)
      trim_estimate <- psi_trim
      trim_se <- sqrt(fsc_t * sum(cl_eic_t^2) / (n_trim^2))
      msg(sprintf("    Trimmed ATE (g in [%.2f, %.2f], n=%d of %d, %d clusters, %s): %.4f (SE %.4f).",
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

  # Cluster-robust variance (Liang-Zeger style with FSC)
  clusters <- cluster
  cluster_eic <- tapply(D_w, clusters, sum)
  J <- length(unique(clusters))
  fsc <- J / max(1, J - 1L)
  var_hat <- fsc * sum(cluster_eic^2) / (length(weights)^2)
  se_hat  <- sqrt(var_hat)
  ci <- psi_hat + c(-1, 1) * stats::qnorm(0.975) * se_hat
  z  <- psi_hat / se_hat
  p_val <- 2 * stats::pnorm(-abs(z))
  msg(sprintf("    Cluster-robust SE (J=%d clusters, FSC=%.4f) = %.4f.", J, fsc, se_hat), cfg = cfg)

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
    head_ci  <- trim_estimate + c(-1, 1) * stats::qnorm(0.975) * trim_se
    head_z   <- trim_estimate / trim_se; head_p <- 2 * stats::pnorm(-abs(head_z))
    estimand_label <- sprintf("TRIMMED_ATE_%s_g[%.2f,%.2f]", base_label,
                              cfg$final_tmle$trim_g_lower %||% 0.05,
                              cfg$final_tmle$trim_g_upper %||% 0.95)
    msg(sprintf("  [TMLE] PRIMARY ESTIMAND = overlap-trimmed ATE (%.4f, SE %.4f). Full-sample ATE retained as secondary (%.4f).",
                head_est, head_se, ate_full_estimate), cfg = cfg)
  } else if (identical(primary, "att")) {
    if (!is.finite(att_estimate) || !is.finite(att_se) || att_se <= 0)
      stop("primary_estimand='att' but the ATT is not available (report_att must be TRUE and yield a finite estimate).", call. = FALSE)
    # The ATT SE is the cluster-robust EIF (sandwich) SE. Refuse to headline it
    # if the EIF centering self-check failed, since that indicates the
    # influence function was miscomputed and the SE would be invalid.
    if (!is.finite(att_eif_mean) ||
        abs(att_eif_mean) > 1e-6 * max(1, abs(att_estimate))) {
      stop("primary_estimand='att' but the ATT EIF centering check failed (weighted mean(D_att) not ~0); the SE is not trustworthy. Investigate before headlining ATT.", call. = FALSE)
    }
    head_est <- att_estimate; head_se <- att_se
    head_z   <- att_estimate / att_se
    # the PRIMARY ATT interval uses the
    # effective-df t (df = G* - 1), because the EIF Wald normal interval is
    # anti-conservative under near-positivity. G* = (sum cl_eic^2)^2 /
    # sum(cl_eic^4). The normal-approx interval is retained as ci_*_normal in
    # the result row. Falls back to normal if G* is not finite.
    att_Gstar_h <- tryCatch({
      s2h <- sum(cl_eic_att^2); s4h <- sum(cl_eic_att^4)
      if (is.finite(s4h) && s4h > 0) (s2h^2) / s4h else NA_real_
    }, error = function(e) NA_real_)
    att_dfeff_h <- if (is.finite(att_Gstar_h)) max(att_Gstar_h - 1, 1) else Inf
    head_crit <- if (is.finite(att_dfeff_h)) stats::qt(0.975, att_dfeff_h) else stats::qnorm(0.975)
    head_ci  <- att_estimate + c(-1, 1) * head_crit * att_se
    head_p   <- if (is.finite(att_dfeff_h)) 2 * stats::pt(-abs(head_z), att_dfeff_h) else
      2 * stats::pnorm(-abs(head_z))
    estimand_label <- sprintf("ATT_%s", base_label)
    msg(sprintf("  [TMLE] PRIMARY ESTIMAND = ATT (%.4f, cluster-robust EIF SE %.4f, effective-df t CI on df=G*-1=%.1f). Full-sample ATE retained as secondary (%.4f). EIF centering check PASSED.",
                head_est, head_se, att_dfeff_h, ate_full_estimate), cfg = cfg)
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

  # cluster-assignment diagnostic. Lets you verify the pre-score
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

  # Cluster EIC (primary ingredient for variance)
  eic_path <- build_unique_path(cfg, cfg$final_tmle$cluster_eic_rds)
  saveRDS(cluster_eic, eic_path)
  msg(sprintf("  [TMLE] Cluster-level EIC saved: %s", basename(eic_path)), cfg = cfg)

  # Main results row
  res_df <- data.frame(
    estimand = estimand_label,
    estimate = head_est, se = head_se,
    ci_lower = head_ci[1], ci_upper = head_ci[2],
    z = head_z, p_value = head_p,
    primary_estimand = primary,
    ate_tmle = ate_full_estimate,
    ate_se_full = ate_full_se,
    ate_ci_lower_full = ate_full_ci[1], ate_ci_upper_full = ate_full_ci[2],
    ate_p_full = ate_full_p,
    ate_plugin_initial = psi_plugin_initial,
    ate_aipw_initial = psi_aipw_initial,
    tmle_minus_plugin_initial = ate_full_estimate - psi_plugin_initial,
    tmle_minus_aipw_initial = ate_full_estimate - psi_aipw_initial,
    att_estimate = att_estimate, att_se = att_se,
    att_eif_weighted_mean = att_eif_mean,
    att_nocensoring_check = att_nocens_check,
    trim_ate_estimate = trim_estimate, trim_ate_se = trim_se,
    trim_eps = trim_eps,
    n_trimmed = n_trim,
    trim_g_lower = if (isTRUE(cfg$final_tmle$trim_enable)) cfg$final_tmle$trim_g_lower %||% NA_real_ else NA_real_,
    trim_g_upper = if (isTRUE(cfg$final_tmle$trim_enable)) cfg$final_tmle$trim_g_upper %||% NA_real_ else NA_real_,
    n = length(weights), n_clusters = J,
    epsilon_fluctuation = eps_hat,
    y_lower = y_lower, y_upper = y_upper,
    stringsAsFactors = FALSE)
  # attach cluster-robust ATT inference to the HEADLINE result row so a
  # topline reader of cv_tmle_results sees the effective-df t interval and G*
  # next to the normal-approx CI. The existing estimate/se/ci_lower/ci_upper/
  # p_value columns are the normal approximation and are LEFT UNCHANGED; these
  # are additive columns only. The LOCO jackknife SE and the bootstrap
  # percentile CI live in att_inference_robustness.csv. Fully guarded: on any
  # problem the columns are simply absent and the headline row is not altered.
  tryCatch({
    if (exists("cl_eic_att", inherits = FALSE) && !is.null(cl_eic_att) &&
        is.finite(att_estimate) && is.finite(att_se) && att_se > 0) {
      Sg_h    <- as.numeric(cl_eic_att)
      n_h     <- length(weights)
      s2_h    <- sum(Sg_h^2); s4_h <- sum(Sg_h^4)
      Gstar_h <- if (is.finite(s4_h) && s4_h > 0) (s2_h^2) / s4_h else NA_real_
      set.seed(seed_for(cfg, 424242L))   # same stream as the diagnostics battery
      boot_h  <- vapply(seq_len(2000L), function(b)
        att_estimate + sum(sample(c(-1, 1), length(Sg_h), replace = TRUE) * Sg_h) / n_h,
        numeric(1))
      zc <- stats::qnorm(0.975)
      res_df$att_G_star             <- Gstar_h
      res_df$att_se_multiplier_boot <- stats::sd(boot_h)
      # Normal-approx interval kept as a REFERENCE. The primary ci_lower/
      # ci_upper/p_value columns are the effective-df t interval (ATT branch).
      res_df$ci_lower_normal <- att_estimate - zc * att_se
      res_df$ci_upper_normal <- att_estimate + zc * att_se
      res_df$p_normal        <- 2 * stats::pnorm(-abs(att_estimate / att_se))
      # surface the one-step support monitor so the manuscript can report
      # the implied counterfactual E[Y(0)|A=1] and confirm it stayed within the
      # outcome support [y_lower, y_upper] (the one-step is not range-guaranteed
      # like a logistic-fluctuation TMLE; this is the empirical check).
      if (exists("cf_treated_under_control", inherits = FALSE)) {
        res_df$att_cf_under_control <- cf_treated_under_control
        res_df$att_support_ok       <- isTRUE(att_support_ok)
      }
    }
  }, error = function(e)
    message(sprintf("  [TMLE] headline robust-inference columns skipped: %s", conditionMessage(e))))
  if (isTRUE(cfg$global$save_stage_csvs)) {
    p <- write_run_csv(res_df, cfg, cfg$final_tmle$results_csv)
    msg(sprintf("  [TMLE] Main result written: %s", basename(p)), cfg = cfg)
  }

  msg(sprintf(
    "\n===== Final CV-TMLE COMPLETE =====\n  PRIMARY (%s): estimate = %.4f, SE = %.4f, 95%% CI = [%.4f, %.4f], p = %.4g\n  Full-sample ATE (always reported): %.4f, SE = %.4f.\n==================================\n",
    estimand_label, head_est, head_se, head_ci[1], head_ci[2], head_p,
    ate_full_estimate, ate_full_se), cfg = cfg)

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
      att_eif_weighted_mean = att_eif_mean, att_nocensoring_check = att_nocens_check,
      trim_ate_estimate = trim_estimate, trim_ate_se = trim_se,
      n_trimmed = n_trim),
    Qbar1W = Qbar1W, Qbar0W = Qbar0W, QbarAW = QbarAW,
    Qstar1W_orig = Qstar1W_orig, Qstar0W_orig = Qstar0W_orig,
    gn = gn, pi_AW = pi_AW, pi_1W = pi_1W, pi_0W = pi_0W,
    att_components = att_components,
    D = D, D_orig = D_orig, cluster_eic = cluster_eic,
    weights = weights,
    outer_fold = outer_fold,
    sl_log = sl_log_df,
    selection_log = sel_log_df,
    fold_support_log = if (length(fold_support_log)) do.call(rbind, fold_support_log) else NULL,
    run_manifest = if (length(run_manifest_rows)) do.call(rbind, run_manifest_rows) else NULL,
    internal_fold_support_log = if (length(internal_fold_support_log)) do.call(rbind, internal_fold_support_log) else NULL,
    fold_times = fold_times,
    selected_by_fold = selected_by_fold,
    y_lower = y_lower, y_upper = y_upper, y_range = y_range,
    outcome_type = outcome_type
  )
}

# =============================================================================
# 9) PEER-REVIEW DIAGNOSTICS
# =============================================================================
# Plain-English role: produce the standard set of figures and tables a
# you will want to see alongside the headline effect. Covariate balance,
# propensity distributions, learner weights, overlap products, fold timings,
# QQ plot of the cluster-level EIC, and a CONSORT-style sample-flow CSV.


write_diag_csv <- function(x, cfg, out_dir, filename) {
  path <- build_unique_diag_path(cfg, out_dir, filename)
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

kish_eff_n <- function(w) {
  w <- as.numeric(w)
  w <- w[is.finite(w) & w > 0]
  if (length(w) == 0L) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

weighted_var_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2L) return(NA_real_)
  ww <- w[ok] / sum(w[ok])
  mu <- sum(ww * x[ok])
  sum(ww * (x[ok] - mu)^2)
}

balance_one_variable <- function(x, group, w_pre, w_post, variable) {
  group <- as.integer(group)
  if (!all(group %in% c(0L, 1L))) return(NULL)
  if (is.numeric(x) || is.integer(x)) {
    xx <- suppressWarnings(as.numeric(x))
    smd_num <- function(w) {
      ok1 <- group == 1L & is.finite(xx) & is.finite(w) & w > 0
      ok0 <- group == 0L & is.finite(xx) & is.finite(w) & w > 0
      if (sum(ok1) < 5L || sum(ok0) < 5L) return(NA_real_)
      m1 <- stats::weighted.mean(xx[ok1], w[ok1])
      m0 <- stats::weighted.mean(xx[ok0], w[ok0])
      v1 <- weighted_var_safe(xx[ok1], w[ok1])
      v0 <- weighted_var_safe(xx[ok0], w[ok0])
      sp <- sqrt(mean(c(v1, v0), na.rm = TRUE))
      if (!is.finite(sp) || sp <= 0) return(NA_real_)
      (m1 - m0) / sp
    }
    out <- data.frame(variable = variable, type = "numeric",
                      smd_pre = smd_num(w_pre), smd_post = smd_num(w_post),
                      max_level = NA_character_, stringsAsFactors = FALSE)
    return(out)
  }
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "Missing"
  ff <- as.factor(x_chr)
  levs <- levels(ff)
  if (length(levs) < 2L) return(NULL)
  smd_levels <- function(w) {
    vals <- numeric(length(levs)); names(vals) <- levs
    for (j in seq_along(levs)) {
      z <- as.integer(ff == levs[j])
      ok1 <- group == 1L & is.finite(w) & w > 0
      ok0 <- group == 0L & is.finite(w) & w > 0
      if (sum(ok1) < 5L || sum(ok0) < 5L) { vals[j] <- NA_real_; next }
      p1 <- stats::weighted.mean(z[ok1], w[ok1])
      p0 <- stats::weighted.mean(z[ok0], w[ok0])
      pp <- (p1 + p0) / 2
      sp <- sqrt(pp * (1 - pp))
      vals[j] <- if (is.finite(sp) && sp > 0) (p1 - p0) / sp else NA_real_
    }
    vals
  }
  pre <- smd_levels(w_pre)
  post <- smd_levels(w_post)
  idx_pre <- if (any(is.finite(pre))) which.max(abs(pre)) else NA_integer_
  idx_post <- if (any(is.finite(post))) which.max(abs(post)) else NA_integer_
  data.frame(variable = variable, type = "factor",
             smd_pre = if (is.na(idx_pre)) NA_real_ else pre[idx_pre],
             smd_post = if (is.na(idx_post)) NA_real_ else post[idx_post],
             max_level = if (is.na(idx_post)) NA_character_ else names(post)[idx_post],
             stringsAsFactors = FALSE)
}

make_balance_table <- function(df, cfg, group, post_weights, label) {
  cand <- get_candidate_vars(df, cfg)
  max_bal <- cfg$diagnostics$max_balance_variables %||% length(cand)
  if (length(cand) > max_bal) {
    message(sprintf("  [diag] Balance table for %s limited from %d to %d variables by diagnostics$max_balance_variables.", label, length(cand), max_bal))
    cand <- cand[seq_len(max_bal)]
  }
  w_pre <- rep(1, nrow(df))
  w_post <- as.numeric(post_weights)
  w_post[!is.finite(w_post) | w_post <= 0] <- NA_real_
  rows <- list()
  for (v in cand) {
    rows[[v]] <- tryCatch(balance_one_variable(df[[v]], group, w_pre, w_post, v),
                          error = function(e) NULL)
  }
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out) || nrow(out) == 0L) {
    return(data.frame(comparison = character(0), variable = character(0), type = character(0),
                      smd_pre = numeric(0), smd_post = numeric(0), max_level = character(0)))
  }
  out$comparison <- label
  out$abs_smd_pre <- abs(out$smd_pre)
  out$abs_smd_post <- abs(out$smd_post)
  out <- out[, c("comparison", "variable", "type", "smd_pre", "smd_post",
                 "abs_smd_pre", "abs_smd_post", "max_level")]
  out[order(-pmax(out$abs_smd_pre, out$abs_smd_post, na.rm = TRUE)), , drop = FALSE]
}

parse_coef_string <- function(s, fold, nuisance) {
  if (is.na(s) || !nzchar(s)) {
    return(data.frame(fold = integer(0), nuisance = character(0), learner = character(0), weight = numeric(0)))
  }
  parts <- strsplit(as.character(s), ";", fixed = TRUE)[[1]]
  pieces <- strsplit(parts, "=", fixed = TRUE)
  data.frame(fold = fold, nuisance = nuisance,
             learner = vapply(pieces, `[`, character(1), 1L),
             weight = suppressWarnings(as.numeric(vapply(pieces, `[`, character(1), 2L))),
             stringsAsFactors = FALSE)
}

make_sl_weight_long <- function(sl_log) {
  if (is.null(sl_log) || nrow(sl_log) == 0L) return(data.frame())
  rows <- list()
  for (i in seq_len(nrow(sl_log))) {
    rows[[length(rows) + 1L]] <- parse_coef_string(sl_log$Q_coef[i], sl_log$fold[i], "Q")
    rows[[length(rows) + 1L]] <- parse_coef_string(sl_log$g_coef[i], sl_log$fold[i], "g")
    rows[[length(rows) + 1L]] <- parse_coef_string(sl_log$pi_coef[i], sl_log$fold[i], "pi")
  }
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$weight), , drop = FALSE]
  out
}

make_evalue_approx <- function(estimate, ci_lower, ci_upper, y) {
  y <- y[is.finite(y)]
  # Binary outcome: the ATT estimate is a risk difference. Convert to an
  # approximate risk ratio using the sample baseline risk, then apply the
  # standard VanderWeele E-value formula RR* + sqrt(RR*(RR*-1)) on RR* >= 1.
  is_binary <- length(unique(y)) <= 2L && all(y %in% c(0, 1))
  if (is_binary) {
    p0 <- mean(y)
    if (!is.finite(p0) || p0 <= 0 || p0 >= 1) {
      return(data.frame(effect_scale = "binary_risk_difference",
                        std_effect = NA_real_, approx_rr = NA_real_,
                        ci_min_abs_bound_sd = NA_real_,
                        evalue_point = NA_real_, evalue_ci = NA_real_,
                        note = "Binary outcome; baseline risk degenerate.",
                        stringsAsFactors = FALSE))
    }
    rd_to_rr <- function(rd) {
      p1 <- min(max(p0 + rd, 1e-6), 1 - 1e-6)
      p1 / p0
    }
    rr <- rd_to_rr(estimate)
    if (is.finite(rr) && rr < 1) rr <- 1 / rr           # express away from null
    ev <- if (is.finite(rr) && rr >= 1) rr + sqrt(rr * (rr - 1)) else NA_real_
    if (ci_lower <= 0 && ci_upper >= 0) {
      ev_ci <- 1                                         # CI crosses null
    } else {
      rd_near <- if (abs(ci_lower) < abs(ci_upper)) ci_lower else ci_upper
      rr_ci <- rd_to_rr(rd_near)
      if (is.finite(rr_ci) && rr_ci < 1) rr_ci <- 1 / rr_ci
      ev_ci <- if (is.finite(rr_ci) && rr_ci >= 1) rr_ci + sqrt(rr_ci * (rr_ci - 1)) else NA_real_
    }
    return(data.frame(effect_scale = "binary_risk_difference_to_rr",
                      std_effect = estimate, approx_rr = rr,
                      ci_min_abs_bound_sd = NA_real_,
                      evalue_point = ev, evalue_ci = ev_ci,
                      note = "Binary outcome; E-value from approximate risk ratio (baseline risk = sample mean).",
                      stringsAsFactors = FALSE))
  }
  if (length(y) < 10L || !is.finite(stats::sd(y)) || stats::sd(y) <= 0) {
    return(data.frame(effect_scale = "continuous_approx", std_effect = NA_real_,
                      approx_rr = NA_real_, ci_min_abs_bound_sd = NA_real_,
                      evalue_point = NA_real_, evalue_ci = NA_real_,
                      note = "Outcome SD degenerate.", stringsAsFactors = FALSE))
  }
  sd_y <- stats::sd(y)
  d <- abs(estimate / sd_y)
  rr <- exp(0.91 * d)
  ev <- if (is.finite(rr) && rr >= 1) rr + sqrt(rr * (rr - 1)) else NA_real_
  # Closest confidence-limit bound to the null, expressed in SD units.
  # If the CI crosses the null, the CI-limit E-value is 1 by definition.
  ci_min_abs_bound_sd <- if (ci_lower <= 0 && ci_upper >= 0) {
    0
  } else {
    min(abs(ci_lower), abs(ci_upper)) / sd_y
  }
  rr_ci <- exp(0.91 * ci_min_abs_bound_sd)
  ev_ci <- if (ci_min_abs_bound_sd == 0) 1 else rr_ci + sqrt(rr_ci * (rr_ci - 1))
  data.frame(effect_scale = "continuous_approx_vanderweele_rr_transform",
             std_effect = estimate / sd_y, approx_rr = rr,
             ci_min_abs_bound_sd = ci_min_abs_bound_sd,
             evalue_point = ev, evalue_ci = ev_ci,
             note = "Approximation for continuous outcomes; interpret as supplemental sensitivity only.",
             stringsAsFactors = FALSE)
}

plot_love <- function(tab, path, title, n_top = 30L) {
  if (is.null(tab) || nrow(tab) == 0L) return(invisible(NULL))
  ord <- order(pmax(tab$abs_smd_pre, tab$abs_smd_post, na.rm = TRUE), decreasing = TRUE)
  top <- head(ord, n_top)
  tt <- tab[top, , drop = FALSE]
  png(path, width = 1100, height = 900, res = 150)
  par(mar = c(4, 9, 3, 1))
  y <- seq_len(nrow(tt))
  plot(tt$abs_smd_pre, y, pch = 1, xlim = c(0, max(c(tt$abs_smd_pre, tt$abs_smd_post, 0.1), na.rm = TRUE)),
       yaxt = "n", xlab = "Absolute standardized mean difference", ylab = "", main = title)
  points(tt$abs_smd_post, y, pch = 19)
  axis(2, at = y, labels = tt$variable, las = 2, cex.axis = 0.7)
  abline(v = 0.1, lty = 2)
  legend("topright", legend = c("Pre", "Post"), pch = c(1, 19), bty = "n")
  dev.off()
}

run_peer_review_diagnostics <- function(cfg, main_df, prescreen_results = NULL,
                                        tmle_fit = NULL, raw_w1_rows = NULL) {
  if (!isTRUE(cfg$diagnostics$enable)) return(invisible(NULL))
  msg("\n===== STAGE: Peer-review diagnostics =====", cfg = cfg)
  out_dir <- file.path(cfg$global$output_dir, cfg$diagnostics$diagnostics_dir)
  ensure_output_dir(out_dir)
  msg(sprintf("  [diag] Output directory: %s", out_dir), cfg = cfg)

  A <- normalize_binary_var(main_df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  delta_Y <- as.integer(!is.na(main_df[[cfg$analysis$outcome_var]]))
  w <- as.numeric(main_df[[cfg$analysis$weight_var]])
  w[!is.finite(w) | w <= 0] <- NA_real_
  cluster <- as.character(main_df[[cfg$analysis$cluster_var]])

  # ---- Sample flow summary -----------------------------------------------
  msg("  [diag] Building sample-flow summary...", cfg = cfg)
  sample_flow <- data.frame(
    step = c("Wave 1 in-home raw rows", "Rows in current analytic dataset", "Valid sampling weight", "Rows used in final TMLE", "Outcome observed"),
    n = c(raw_w1_rows %||% NA_integer_, nrow(main_df), sum(is.finite(w) & w > 0, na.rm = TRUE),
          if (!is.null(tmle_fit)) tmle_fit$result$n else NA_integer_, sum(delta_Y == 1L)),
    stringsAsFactors = FALSE)
  if (isTRUE(cfg$diagnostics$save_csvs)) {
    write_diag_csv(sample_flow, cfg, out_dir, "sample_flow.csv")
  }

  # ---- Effective sample size -----------------------------------------------
  ess_rows <- list(
    overall = data.frame(group = "overall", n = length(w), n_eff = kish_eff_n(w)),
    treated = data.frame(group = "A=1", n = sum(A == 1L), n_eff = kish_eff_n(w[A == 1L])),
    unexposed = data.frame(group = "A=0", n = sum(A == 0L), n_eff = kish_eff_n(w[A == 0L])),
    observed = data.frame(group = "delta_Y=1", n = sum(delta_Y == 1L), n_eff = kish_eff_n(w[delta_Y == 1L])),
    missing = data.frame(group = "delta_Y=0", n = sum(delta_Y == 0L), n_eff = kish_eff_n(w[delta_Y == 0L]))
  )
  if (!is.null(tmle_fit)) {
    for (vv in sort(unique(tmle_fit$outer_fold))) {
      ii <- tmle_fit$outer_fold == vv
      ess_rows[[paste0("fold_", vv)]] <- data.frame(group = paste0("outer_fold=", vv), n = sum(ii), n_eff = kish_eff_n(w[ii]))
    }
  }
  ess_df <- do.call(rbind, ess_rows)
  rownames(ess_df) <- NULL
  if (isTRUE(cfg$diagnostics$save_csvs)) {
    write_diag_csv(ess_df, cfg, out_dir, cfg$diagnostics$effective_sample_size_csv %||% "effective_sample_size.csv")
  }

  # ---- Selection consistency -----------------------------------------------
  if (!is.null(tmle_fit)) {
    msg("  [diag] Aggregating cross-fold variable-selection consistency...", cfg = cfg)
    if (!is.null(tmle_fit$selection_log) && nrow(tmle_fit$selection_log) > 0L) {
      sl <- tmle_fit$selection_log
      vars <- sort(unique(sl$variable))
      sel_cons <- data.frame(variable = vars, stringsAsFactors = FALSE)
      bool_cols <- intersect(c("selected", "in_rough_pool", "dropped_redundant", "selected_by_joint_AY",
                               "selected_by_outcome", "selected_by_delta", "selected_by_lasso_A",
                               "selected_by_lasso_Y", "selected_by_lasso_delta", "kept_in_final_W",
                               "dropped_by_column_cap"), names(sl))
      for (cc in bool_cols) {
        sel_cons[[paste0("n_folds_", cc)]] <- vapply(vars, function(vv) sum(isTRUE_vec(sl[sl$variable == vv, cc])), integer(1))
      }
      if ("selected" %in% bool_cols) sel_cons <- sel_cons[order(-sel_cons$n_folds_selected, sel_cons$variable), , drop = FALSE]
    } else {
      vars <- sort(unique(unlist(tmle_fit$selected_by_fold)))
      sel_cons <- data.frame(variable = vars,
                             n_folds_selected = vapply(vars, function(vv) sum(vapply(tmle_fit$selected_by_fold, function(z) vv %in% z, logical(1))), integer(1)),
                             stringsAsFactors = FALSE)
      sel_cons <- sel_cons[order(-sel_cons$n_folds_selected, sel_cons$variable), , drop = FALSE]
    }
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(sel_cons, cfg, out_dir, cfg$diagnostics$variable_consistency_csv %||% "variable_selection_consistency.csv")
      if (!is.null(tmle_fit$selected_by_fold) && length(tmle_fit$selected_by_fold) > 1L) {
        pairs <- utils::combn(seq_along(tmle_fit$selected_by_fold), 2)
        jac <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
          a <- unique(tmle_fit$selected_by_fold[[pairs[1, j]]]); b <- unique(tmle_fit$selected_by_fold[[pairs[2, j]]])
          u <- union(a, b); inter <- intersect(a, b)
          data.frame(fold_1 = pairs[1, j], fold_2 = pairs[2, j],
                     n_fold_1 = length(a), n_fold_2 = length(b),
                     n_intersection = length(inter), n_union = length(u),
                     jaccard = if (length(u) == 0L) NA_real_ else length(inter) / length(u),
                     stringsAsFactors = FALSE)
        }))
        write_diag_csv(jac, cfg, out_dir, cfg$diagnostics$selection_jaccard_csv %||% "selection_jaccard.csv")
      }
      # cross-seed stability artifact. One row per run recording the
      # pipeline seed, the size of the selected confounder set (union across
      # folds), the core size (selected in ALL folds), the mean pairwise fold
      # Jaccard, and a fingerprint (count + first/last vars) of the sorted core.
      # Running two seeds and comparing these rows shows whether the selected
      # confounders are stable across partitions: identical core size and
      # fingerprint, and high Jaccard, indicate the seed churn is resolved.
      nfold_sel <- length(tmle_fit$selected_by_fold %||% list())
      core_vars <- if (nfold_sel > 0L && "n_folds_selected" %in% names(sel_cons)) {
        sort(sel_cons$variable[sel_cons$n_folds_selected == nfold_sel])
      } else if (nfold_sel > 0L) {
        # Fallback: recompute core directly from selected_by_fold.
        allv <- sort(unique(unlist(tmle_fit$selected_by_fold)))
        sort(allv[vapply(allv, function(vv)
          all(vapply(tmle_fit$selected_by_fold, function(z) vv %in% z, logical(1))), logical(1))])
      } else character(0)
      union_vars <- sort(unique(unlist(tmle_fit$selected_by_fold %||% list())))
      mean_jac <- if (exists("jac") && is.data.frame(jac) && nrow(jac) > 0L)
        mean(jac$jaccard, na.rm = TRUE) else NA_real_
      seed_stab <- data.frame(
        pipeline_seed = cfg$global$pipeline_seed %||% NA_integer_,
        outcome_var = cfg$analysis$outcome_var,
        n_folds = nfold_sel,
        n_selected_union = length(union_vars),
        n_selected_core = length(core_vars),
        mean_pairwise_fold_jaccard = mean_jac,
        core_first_var = if (length(core_vars)) core_vars[1] else NA_character_,
        core_last_var = if (length(core_vars)) core_vars[length(core_vars)] else NA_character_,
        core_fingerprint = paste0(length(core_vars), ":",
                                  substr(paste(core_vars, collapse = "|"), 1, 200)),
        stringsAsFactors = FALSE)
      write_diag_csv(seed_stab, cfg, out_dir,
                     cfg$diagnostics$seed_stability_csv %||% "seed_stability.csv")
    }
  }

  # ---- Learner weights ------------------------------------------------------
  if (!is.null(tmle_fit) && !is.null(tmle_fit$sl_log)) {
    msg("  [diag] Summarizing SuperLearner meta-weights...", cfg = cfg)
    lw <- make_sl_weight_long(tmle_fit$sl_log)
    if (nrow(lw) > 0L) {
      nuis <- sort(unique(lw$nuisance))
      combos <- unique(lw[, c("nuisance", "learner"), drop = FALSE])
      lw_sum <- do.call(rbind, lapply(seq_len(nrow(combos)), function(ii) {
        nn <- combos$nuisance[ii]; ll <- combos$learner[ii]
        z <- lw$weight[lw$nuisance == nn & lw$learner == ll]
        data.frame(nuisance = nn, learner = ll, mean_weight = mean(z, na.rm = TRUE),
                   sd_weight = stats::sd(z, na.rm = TRUE), min_weight = min(z, na.rm = TRUE),
                   max_weight = max(z, na.rm = TRUE), n_folds_nonzero = sum(z > 1e-8, na.rm = TRUE),
                   stringsAsFactors = FALSE)
      }))
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(lw, cfg, out_dir, cfg$diagnostics$learner_weight_long_csv %||% "learner_weight_long.csv")
        write_diag_csv(lw_sum, cfg, out_dir, cfg$diagnostics$learner_weight_summary_csv %||% "learner_weight_summary.csv")
      }
      if (isTRUE(cfg$diagnostics$save_plots)) {
        for (nn in nuis) {
          mat <- xtabs(weight ~ learner + fold, data = lw[lw$nuisance == nn, , drop = FALSE])
          plot_path <- build_unique_diag_path(cfg, out_dir, paste0("sl_weights_stacked_", nn, ".png"))
          png(plot_path, width = 1000, height = 650, res = 150)
          par(mar = c(4, 8, 3, 1))
          barplot(mat, legend.text = rownames(mat), args.legend = list(x = "topright", cex = 0.7),
                  xlab = "Outer fold", ylab = "SuperLearner weight", main = paste("SL weights:", nn), las = 2)
          dev.off()
        }
      }
    }
  }

  # ---- Per-fold plug-in ATE diagnostics ------------------------------------
  if (!is.null(tmle_fit)) {
    folds <- sort(unique(tmle_fit$outer_fold))
    fold_ate <- do.call(rbind, lapply(folds, function(vv) {
      ii <- tmle_fit$outer_fold == vv
      ww <- w[ii]
      psi <- sum(ww * (tmle_fit$Qstar1W_orig[ii] - tmle_fit$Qstar0W_orig[ii]), na.rm = TRUE) / sum(ww, na.rm = TRUE)
      data.frame(fold = vv, n = sum(ii), n_treated = sum(A[ii] == 1L), n_eff = kish_eff_n(ww), plugin_ate = psi)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(fold_ate, cfg, out_dir, cfg$diagnostics$per_fold_ate_csv %||% "per_fold_ate_diagnostics.csv")
    }
  }

  # ---- ATT-specific diagnostics --------------------------------------------
  # All computed from the canonical per-row ATT components saved by the TMLE
  # routine (att_components), so every number is consistent with the headline
  # ATT and its EIF SE. ac$D_att is the efficient influence function; the
  # numerator of psi_att is (A*att_summand + att_resid); psi_att normalizes by
  # sum(w_norm*A). Restricting any of these to a subset and renormalizing gives
  # the subset's one-step ATT.
  if (!is.null(tmle_fit) && !is.null(tmle_fit$att_components)) {
    ac  <- tmle_fit$att_components
    Aa  <- ac$A; wn <- ac$w_norm; of <- ac$outer_fold; cl <- ac$cluster
    num <- Aa * ac$att_summand + ac$att_resid    # per-row numerator of psi_att
    den_all <- sum(wn * Aa)                        # denominator = weighted treated count

    # Helper: one-step ATT on an arbitrary row-subset (logical or index vector).
    att_on_subset <- function(keep) {
      d <- sum(wn[keep] * Aa[keep])
      if (!is.finite(d) || d <= 0) return(NA_real_)
      sum(wn[keep] * num[keep]) / d
    }

    # (1) Per-fold ATT diagnostics ------------------------------------------
    folds_a <- sort(unique(of))
    # for a binary outcome, log per-fold treated/control event counts
    # among OBSERVED rows (delta_Y == 1), so sparse event cells are visible
    # before a fold is trusted. Columns are NA for non-binary outcomes.
    yraw_a <- ac$Y_raw; dobs_a <- ac$delta_Y
    is_bin_out <- any(dobs_a == 1L, na.rm = TRUE) &&
      all(yraw_a[is.finite(yraw_a) & dobs_a == 1L] %in% c(0, 1))
    att_per_fold <- do.call(rbind, lapply(folds_a, function(vv) {
      ii <- of == vv
      data.frame(
        fold = vv,
        n = sum(ii),
        n_treated = sum(Aa[ii] == 1L),
        n_control = sum(Aa[ii] == 0L),
        ess_treated = kish_eff_n(ac$weights[ii][Aa[ii] == 1L]),
        att_fold = att_on_subset(which(ii)),
        n_treated_obs = sum(ii & Aa == 1L & dobs_a == 1L, na.rm = TRUE),
        n_control_obs = sum(ii & Aa == 0L & dobs_a == 1L, na.rm = TRUE),
        n_treated_event = if (is_bin_out) sum(ii & Aa == 1L & dobs_a == 1L & yraw_a == 1, na.rm = TRUE) else NA_integer_,
        n_treated_nonevent = if (is_bin_out) sum(ii & Aa == 1L & dobs_a == 1L & yraw_a == 0, na.rm = TRUE) else NA_integer_,
        n_control_event = if (is_bin_out) sum(ii & Aa == 0L & dobs_a == 1L & yraw_a == 1, na.rm = TRUE) else NA_integer_,
        n_control_nonevent = if (is_bin_out) sum(ii & Aa == 0L & dobs_a == 1L & yraw_a == 0, na.rm = TRUE) else NA_integer_,
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_per_fold, cfg, out_dir,
        cfg$diagnostics$att_per_fold_csv %||% "att_per_fold_diagnostics.csv")

    # (2) Per-cluster ATT influence (all clusters) --------------------------
    # cl_eic_att[k] = sum over rows in cluster k of (w_norm * D_att); the ATT
    # SE is built from these. Report each cluster's contribution and its
    # leave-one-cluster-out delta (linear approximation via the EIF sum).
    cl_names <- names(ac$cl_eic_att)
    cl_contrib <- as.numeric(ac$cl_eic_att)
    # Exact leave-one-out ATT for each cluster (drop the cluster, renormalize).
    loo_one <- vapply(cl_names, function(cn) att_on_subset(cl != cn), numeric(1))
    att_cl_all <- data.frame(
      cluster = cl_names,
      n_rows = as.integer(tapply(rep(1L, length(cl)), cl, sum)[cl_names]),
      n_treated = as.integer(tapply(Aa, cl, sum)[cl_names]),
      eic_contribution = cl_contrib,
      abs_eic_contribution = abs(cl_contrib),
      att_leave_this_cluster_out = loo_one,
      delta_from_full = loo_one - ac$psi_att,
      stringsAsFactors = FALSE)
    att_cl_all <- att_cl_all[order(-att_cl_all$abs_eic_contribution), , drop = FALSE]
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_cl_all, cfg, out_dir,
        cfg$diagnostics$att_cluster_influence_all_csv %||% "att_cluster_influence_all.csv")

    # (2b) Cluster-robust inference battery (finite-sample, NO refit) --------
    # Post-estimation robustness for the ATT SE that requires neither a
    # nuisance re-fit nor a nonparametric bootstrap. It reuses the per-cluster
    # EIF contributions (cl_contrib = cl_eic_att, the SAME quantities that
    # build att_se at the estimation step: att_se = sqrt(fsc_att *
    # sum(cl_eic_att^2) / n^2)) and the EXACT leave-one-cluster-out estimates
    # (loo_one). All SEs below are on the same scale and directly comparable.
    # Reports: (i) effective number of clusters G* = (sum S^2)^2 / sum(S^4);
    # (ii) a fixed-nuisance leave-one-cluster-out (LOCO) jackknife SE -- this
    # drops each cluster and renormalizes the one-step ATT holding the fitted
    # cross-fitted nuisances FIXED; it is NOT a full-refit CV3 jackknife (no
    # re-screening / re-fitting per cluster), so it is labeled loco not cv3;
    # (iii) a cluster multiplier (wild) bootstrap SE + percentile CI; (iv) an
    # effective-df t interval (df = G* - 1).
    # Wrapped in tryCatch so a diagnostic failure can never abort the run.
    att_inf <- tryCatch({
      Sg         <- cl_contrib                  # per-cluster summed w_norm*D_att (= S_g)
      n_rows_att <- length(cl)                  # analysis rows (matches att_se denominator)
      G_cl       <- length(Sg)
      sumS2      <- sum(Sg^2); sumS4 <- sum(Sg^4)
      G_star     <- if (is.finite(sumS4) && sumS4 > 0) (sumS2^2) / sumS4 else NA_real_
      se_analytic        <- ac$att_se           # reported SE (includes fsc_att correction)
      se_analytic_nocorr <- sqrt(sumS2) / n_rows_att
      # Fixed-nuisance leave-one-cluster-out (LOCO) jackknife from the exact
      # leave-one-cluster-out estimates (no refit; nuisances held fixed).
      lo   <- loo_one[is.finite(loo_one)]
      Gj   <- length(lo)
      se_jack <- if (Gj > 1L) sqrt((Gj - 1) / Gj * sum((lo - mean(lo))^2)) else NA_real_
      # Cluster multiplier (wild) bootstrap on the EIF contributions (no refit).
      # Seeded off the pipeline seed so it is reproducible AND distinct per seed.
      Bboot <- 2000L
      set.seed(seed_for(cfg, 424242L))
      boot <- vapply(seq_len(Bboot), function(b) {
        xi <- sample(c(-1, 1), G_cl, replace = TRUE)   # Rademacher multipliers
        ac$psi_att + sum(xi * Sg) / n_rows_att
      }, numeric(1))
      se_boot <- stats::sd(boot)
      boot_ci <- stats::quantile(boot, c(0.025, 0.975), names = FALSE, type = 7)
      df_eff  <- max(G_star - 1, 1)
      tcrit   <- stats::qt(0.975, df_eff)
      zcrit   <- stats::qnorm(0.975)
      tstat   <- ac$psi_att / se_analytic
      s2sorted <- sort(Sg^2, decreasing = TRUE)
      share_k  <- function(k) if (is.finite(sumS2) && sumS2 > 0)
        sum(s2sorted[seq_len(min(k, G_cl))]) / sumS2 else NA_real_
      data.frame(
        psi_att = ac$psi_att,
        n_rows = n_rows_att,
        G_nominal = G_cl,
        G_star = G_star,
        top1_var_share = share_k(1L),
        top5_var_share = share_k(5L),
        top10_var_share = share_k(10L),
        se_analytic = se_analytic,
        se_analytic_nocorr = se_analytic_nocorr,
        se_jackknife_loco = se_jack,
        se_multiplier_boot = se_boot,
        ci_lower_normal = ac$psi_att - zcrit * se_analytic,
        ci_upper_normal = ac$psi_att + zcrit * se_analytic,
        p_normal = 2 * stats::pnorm(-abs(tstat)),
        df_eff_tdf = df_eff,
        ci_lower_tdf = ac$psi_att - tcrit * se_analytic,
        ci_upper_tdf = ac$psi_att + tcrit * se_analytic,
        p_tdf = 2 * stats::pt(-abs(tstat), df_eff),
        ci_lower_jack_tdf = ac$psi_att - tcrit * se_jack,
        ci_upper_jack_tdf = ac$psi_att + tcrit * se_jack,
        ci_lower_boot_pctile = boot_ci[1],
        ci_upper_boot_pctile = boot_ci[2],
        boot_reps = Bboot,
        stringsAsFactors = FALSE)
    }, error = function(e) {
      message(sprintf("  [att-inference] battery failed: %s", conditionMessage(e))); NULL
    })
    if (!is.null(att_inf) && isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_inf, cfg, out_dir, "att_inference_robustness.csv")

    # (3) Top influential clusters ------------------------------------------
    top_k_show <- min(10L, nrow(att_cl_all))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(utils::head(att_cl_all, top_k_show), cfg, out_dir,
        cfg$diagnostics$att_top_cluster_influence_csv %||% "top_att_cluster_influence.csv")

    # (4) Leave-top-k-clusters-out ATT --------------------------------------
    ranked_clusters <- att_cl_all$cluster   # already sorted by |contribution|
    kvec <- cfg$diagnostics$att_leaveout_top_k %||% c(1L, 3L, 5L)
    att_leaveout <- do.call(rbind, lapply(kvec, function(kk) {
      kk <- min(kk, length(ranked_clusters))
      drop_cl <- ranked_clusters[seq_len(kk)]
      keep <- !(cl %in% drop_cl)
      est <- att_on_subset(which(keep))
      data.frame(
        drop_top_abs_clusters = kk,
        dropped_clusters = paste(drop_cl, collapse = ";"),
        n_remaining = sum(keep),
        att_remaining = est,
        difference_from_full = est - ac$psi_att,
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_leaveout, cfg, out_dir,
        cfg$diagnostics$att_cluster_influence_leaveout_csv %||% "att_cluster_influence_leaveout.csv")

    # (5) Estimator decomposition -------------------------------------------
    # psi_att = [ contrast_term + treated_resid_term - control_resid_term ] / den
    # where the three numerator pieces are the weighted sums. The control resid
    # term enters att_resid with a minus sign; report its signed contribution.
    contrast_term  <- sum(wn * Aa * ac$att_summand)
    treated_resid  <- sum(wn * (Aa * ac$resid_obs / ac$pi_AW))
    control_resid  <- sum(wn * ((1 - Aa) * (ac$gn / (1 - ac$gn)) * ac$resid_obs / ac$pi_AW))
    att_decomp <- data.frame(
      component = c("contrast_term_sum", "treated_residual_term_sum",
                    "control_residual_term_sum", "denominator_weighted_treated",
                    "contrast_term_per_treated", "treated_residual_per_treated",
                    "control_residual_per_treated", "psi_att_total"),
      value = c(contrast_term, treated_resid, control_resid, den_all,
                contrast_term / den_all, treated_resid / den_all,
                control_resid / den_all,
                (contrast_term + treated_resid - control_resid) / den_all),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_decomp, cfg, out_dir,
        cfg$diagnostics$att_estimator_decomposition_csv %||% "att_estimator_decomposition.csv")

    # (6) EIF diagnostics ----------------------------------------------------
    Dw <- wn * ac$D_att
    eif_sd_w <- sqrt(sum((Dw - mean(Dw))^2) / length(Dw))
    att_eif_diag <- data.frame(
      metric = c("psi_att", "att_se", "att_se_centered", "eif_weighted_mean",
                 "complete_data_recompute_att", "complete_data_recompute_gap",
                 "n_clusters", "small_sample_correction",
                 "eif_min", "eif_max", "eif_sd_unweighted",
                 "weighted_eif_sd", "max_abs_cluster_eic",
                 "sum_cluster_eic_sq"),
      value = c(ac$psi_att, ac$att_se, ac$att_se_centered %||% NA_real_, ac$eif_mean, ac$nocens,
                ac$nocens - ac$psi_att, ac$J_att, ac$fsc_att,
                min(ac$D_att), max(ac$D_att), stats::sd(ac$D_att),
                eif_sd_w, max(abs(ac$cl_eic_att)),
                sum(ac$cl_eic_att^2)),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_eif_diag, cfg, out_dir,
        cfg$diagnostics$att_eif_diagnostics_csv %||% "att_eif_diagnostics.csv")

    # (7) ATT control-weight ESS by fold ------------------------------------
    # Under the ATT, controls carry odds-weights g/(1-g) (times survey weight).
    # The Kish ESS of those weights among controls in each fold measures how
    # much independent information the reweighted control pool actually carries.
    att_ctrl_ess <- do.call(rbind, lapply(folds_a, function(vv) {
      ii <- of == vv & Aa == 0L
      ow <- (ac$gn[ii] / (1 - ac$gn[ii])) * ac$weights[ii]
      data.frame(
        fold = vv,
        n_control = sum(ii),
        control_oddsw_sum = sum(ow),
        control_oddsw_max = if (any(ii)) max(ow) else NA_real_,
        ess_control_attweighted = kish_eff_n(ow),
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_ctrl_ess, cfg, out_dir,
        cfg$diagnostics$att_weighted_control_ess_by_fold_csv %||% "att_weighted_control_ess_by_fold.csv")

    # (8) Censoring (pi) positivity / calibration ---------------------------
    # Distribution of pi(A,W) and the IPCW weight 1/pi, by arm. Extreme small
    # pi (large IPCW weight) is the censoring analogue of a positivity problem.
    pi_arm_summary <- function(mask, label) {
      pim <- ac$pi_AW[mask]; ipw <- 1 / pim
      data.frame(
        arm = label, n = sum(mask),
        pi_min = min(pim), pi_p01 = stats::quantile(pim, 0.01, names = FALSE),
        pi_median = stats::median(pim), pi_max = max(pim),
        ipcw_median = stats::median(ipw),
        ipcw_p99 = stats::quantile(ipw, 0.99, names = FALSE),
        ipcw_max = max(ipw),
        ipcw_frac_gt_10 = mean(ipw > 10), ipcw_frac_gt_20 = mean(ipw > 20),
        stringsAsFactors = FALSE)
    }
    att_pi_cal <- rbind(
      pi_arm_summary(Aa == 1L, "treated"),
      pi_arm_summary(Aa == 0L, "control"),
      pi_arm_summary(rep(TRUE, length(Aa)), "all"))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_pi_cal, cfg, out_dir,
        cfg$diagnostics$att_pi_positivity_calibration_csv %||% "att_pi_positivity_calibration.csv")

    # (9) Outcome-bound (winsorization) sensitivity -------------------------
    # The headline residual uses the UN-winsorized Y_raw against the back-
    # transformed Q (the bound is applied to the model, not the outcome data --
    # standard bounded-Y TMLE). This diagnostic instead caps Y_raw at several
    # upper quantiles, recomputes the IPCW-augmented residual, and re-forms the
    # one-step ATT, to test how much the heavy right tail of earnings drives the
    # estimate. The contrast term (Qbar1-Qbar0) is held fixed (it is the fitted
    # model contrast). An "uncapped" reference row reproduces the headline
    # exactly (difference ~0) and anchors the table.
    # Outcome-bound winsorization is meaningless for a binary outcome (capping
    # a 0/1 vector does nothing), so this diagnostic runs only for continuous
    # outcomes.
    if (identical(tmle_fit$outcome_type, "continuous")) {
      qs <- cfg$diagnostics$att_outcome_bound_quantiles %||% c(0.95, 0.975, 0.98, 0.99, 0.995)
      obs_mask <- ac$delta_Y == 1L & is.finite(ac$Y_raw)
      y_obs_vals <- ac$Y_raw[obs_mask]
      att_bound_q <- do.call(rbind, lapply(qs, function(q) {
        cap <- stats::quantile(y_obs_vals, q, names = FALSE)
        y_w <- ac$Y_raw
        y_w[obs_mask] <- pmin(ac$Y_raw[obs_mask], cap)
        resid_w  <- ifelse(obs_mask, y_w - ac$QbarAW_orig, 0)
        aresid_w <- (Aa * resid_w / ac$pi_AW) -
                    ((1 - Aa) * (ac$gn / (1 - ac$gn)) * resid_w / ac$pi_AW)
        psi_w <- sum(wn * (Aa * ac$att_summand + aresid_w)) / den_all
        data.frame(
          winsor_quantile = q,
          outcome_cap = cap,
          att_estimate = psi_w,
          difference_from_headline = psi_w - ac$psi_att,
          stringsAsFactors = FALSE)
      }))
      att_bound_ref <- data.frame(
        winsor_quantile = NA_real_,
        outcome_cap = NA_real_,
        att_estimate = ac$psi_att,
        difference_from_headline = 0,
        stringsAsFactors = FALSE)
      att_bound <- rbind(att_bound_ref, att_bound_q)
      if (isTRUE(cfg$diagnostics$save_csvs))
        write_diag_csv(att_bound, cfg, out_dir,
          cfg$diagnostics$att_outcome_bound_sensitivity_csv %||% "att_outcome_bound_sensitivity.csv")
    } else {
      msg("  [diag] Outcome-bound sensitivity skipped (binary outcome).", cfg = cfg)
    }

    msg("  [diag] ATT-specific diagnostics written (per-fold, cluster influence, leave-out, decomposition, EIF, control ESS, pi calibration, outcome-bound sensitivity).", cfg = cfg)
  }

  # ---- Run manifest --------------------------------------------------------
  # One per-outcome, per-fold design-verification table: did each fold respect
  # the cap, keep adequate support, and did the run pass its global ATT checks.
  if (!is.null(tmle_fit) && !is.null(tmle_fit$run_manifest)) {
    man <- tmle_fit$run_manifest
    ac2 <- tmle_fit$att_components
    man$att_estimate        <- if (!is.null(ac2)) ac2$psi_att else NA_real_
    man$att_se              <- if (!is.null(ac2)) ac2$att_se else NA_real_
    man$att_eif_weighted_mean <- if (!is.null(ac2)) ac2$eif_mean else NA_real_
    man$att_centering_pass  <- if (!is.null(ac2))
      is.finite(ac2$eif_mean) && abs(ac2$eif_mean) <= 1e-6 * max(1, abs(ac2$psi_att)) else NA
    man$att_nocensoring_gap <- if (!is.null(ac2)) ac2$nocens - ac2$psi_att else NA_real_
    man$outcome_var <- cfg$analysis$outcome_var
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(man, cfg, out_dir, cfg$diagnostics$run_manifest_csv %||% "run_manifest.csv")
    msg("  [diag] Run manifest written (per-fold cap/support + global ATT checks).", cfg = cfg)
  }

  # ---- Balance diagnostics and Love plots ----------------------------------
  if (!is.null(tmle_fit)) {
    msg("  [diag] Computing numeric and categorical balance diagnostics...", cfg = cfg)
    iptw <- ifelse(A == 1L, 1 / tmle_fit$gn, 1 / (1 - tmle_fit$gn))
    treat_post_w <- w * iptw
    bal_A <- make_balance_table(main_df, cfg, A, treat_post_w, "A=1_vs_A=0")
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(bal_A, cfg, out_dir, cfg$diagnostics$balance_treatment_csv %||% "balance_treatment_loveplot_data.csv")
    }
    if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A) > 0L) {
      plot_love(bal_A, build_unique_diag_path(cfg, out_dir, "love_plot_treatment.png"), "Treatment balance: pre vs post propensity weighting", n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
    }
    # treatment balance recomputed on the OVERLAP-TRIMMED sample (rows
    # with g in the trim band). This is the balance for the trimmed/ATT
    # headline estimand; extreme-weight rows from the no-overlap region are
    # removed, so rare-indicator imbalance (CUBAN/DISABLE-type) should be far
    # better behaved than on the full sample. This is the plot to report.
    if (isTRUE(cfg$diagnostics$balance_on_trimmed %||% TRUE) &&
        isTRUE(cfg$final_tmle$trim_enable %||% FALSE)) {
      tlo <- cfg$final_tmle$trim_g_lower %||% 0.05
      thi <- cfg$final_tmle$trim_g_upper %||% 0.95
      keep_bal <- tmle_fit$gn >= tlo & tmle_fit$gn <= thi
      n_keep_bal <- sum(keep_bal)
      if (n_keep_bal >= 2L && length(unique(A[keep_bal])) == 2L) {
        bal_A_trim <- make_balance_table(
          main_df[keep_bal, , drop = FALSE], cfg,
          A[keep_bal], treat_post_w[keep_bal], "A=1_vs_A=0_trimmed")
        msg(sprintf("  [diag] Trimmed-sample treatment balance on %d of %d rows (g in [%.2f, %.2f]).",
                    n_keep_bal, length(A), tlo, thi), cfg = cfg)
        if (isTRUE(cfg$diagnostics$save_csvs)) {
          write_diag_csv(bal_A_trim, cfg, out_dir,
            cfg$diagnostics$balance_treatment_trimmed_csv %||% "balance_treatment_trimmed_loveplot_data.csv")
        }
        if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A_trim) > 0L) {
          plot_love(bal_A_trim,
            build_unique_diag_path(cfg, out_dir, "love_plot_treatment_trimmed.png"),
            "Treatment balance (overlap-trimmed): pre vs post weighting",
            n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
        }
      } else {
        msg("  [diag] Trimmed-sample balance skipped: too few rows or no treatment variation in band.", cfg = cfg)
      }
    }
    # ATT-WEIGHTED balance -- the balance for the ATT headline estimand.
    # Under the ATT, treated units keep weight 1 and controls are reweighted by
    # the odds g/(1-g) to resemble the treated covariate distribution. This is
    # the correct balance diagnostic when the ATT is primary (distinct from the
    # full-sample ATE balance and the trimmed balance above). Survey weights are
    # carried through. Controls with g near 1 receive large odds weights; the
    # control-weight extremes are reported separately below as a positivity-style
    # diagnostic for the ATT.
    if (isTRUE(cfg$diagnostics$balance_att %||% TRUE)) {
      att_bal_w <- w * ifelse(A == 1L, 1, tmle_fit$gn / pmax(1 - tmle_fit$gn, 1e-6))
      if (length(unique(A)) == 2L) {
        bal_A_att <- make_balance_table(main_df, cfg, A, att_bal_w, "A=1_vs_A=0_ATT")
        msg("  [diag] ATT-weighted treatment balance computed (treated w=1, controls w=g/(1-g)).", cfg = cfg)
        if (isTRUE(cfg$diagnostics$save_csvs)) {
          write_diag_csv(bal_A_att, cfg, out_dir,
            cfg$diagnostics$balance_treatment_att_csv %||% "balance_treatment_att_loveplot_data.csv")
        }
        if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A_att) > 0L) {
          plot_love(bal_A_att,
            build_unique_diag_path(cfg, out_dir, "love_plot_treatment_att.png"),
            "Treatment balance (ATT-weighted): pre vs post weighting",
            n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
        }
      }
      # ATT positivity / control-weight diagnostic: distribution of the control
      # odds weights g/(1-g) and the propensity among the treated. The ATT only
      # needs treated units to have comparable controls; extreme control weights
      # (g near 1) are the ATT-specific positivity concern.
      ctrl_odds <- (tmle_fit$gn / pmax(1 - tmle_fit$gn, 1e-6))[A == 0L]
      g_treated <- tmle_fit$gn[A == 1L]
      att_pos <- data.frame(
        metric = c("n_treated", "n_control",
                   "g_treated_min", "g_treated_median", "g_treated_max",
                   "control_oddsw_median", "control_oddsw_p95",
                   "control_oddsw_p99", "control_oddsw_max",
                   "control_oddsw_frac_gt_10", "control_oddsw_frac_gt_50"),
        value = c(sum(A == 1L), sum(A == 0L),
                  min(g_treated), stats::median(g_treated), max(g_treated),
                  stats::median(ctrl_odds),
                  stats::quantile(ctrl_odds, 0.95, names = FALSE),
                  stats::quantile(ctrl_odds, 0.99, names = FALSE),
                  max(ctrl_odds),
                  mean(ctrl_odds > 10), mean(ctrl_odds > 50)),
        stringsAsFactors = FALSE)
      msg(sprintf("  [diag] ATT control odds-weights: median %.2f, p99 %.2f, max %.2f; %.1f%% exceed 10.",
                  stats::median(ctrl_odds), stats::quantile(ctrl_odds, 0.99, names = FALSE),
                  max(ctrl_odds), 100 * mean(ctrl_odds > 10)), cfg = cfg)
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(att_pos, cfg, out_dir,
          cfg$diagnostics$att_positivity_csv %||% "att_positivity_control_weights.csv")
      }
    }
    pi_for_delta <- pmin(pmax(tmle_fit$pi_AW, cfg$final_tmle$pi_lower), cfg$final_tmle$pi_upper)
    censor_w <- w * ifelse(delta_Y == 1L, 1 / pi_for_delta, 1 / pmax(1 - pi_for_delta, 1e-6))
    if (length(unique(delta_Y)) == 2L) {
      bal_D <- make_balance_table(main_df, cfg, delta_Y, censor_w, "delta_Y=1_vs_delta_Y=0")
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(bal_D, cfg, out_dir, cfg$diagnostics$balance_missingness_csv %||% "balance_missingness_loveplot_data.csv")
      }
      if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_D) > 0L) {
        plot_love(bal_D, build_unique_diag_path(cfg, out_dir, "love_plot_missingness.png"), "Outcome-observation balance: pre vs post censoring weighting", n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
      }
    }
  }

  # ---- Positivity / overlap diagnostics ------------------------------------
  if (!is.null(tmle_fit)) {
    g <- tmle_fit$gn
    pos <- data.frame(
      metric = c("g<0.05", "g>0.95", "g<=g_lower", "g>=g_upper", "g*pi1<threshold", "(1-g)*pi0<threshold"),
      fraction = c(mean(g < 0.05), mean(g > 0.95), mean(g <= cfg$final_tmle$g_lower + 1e-12),
                   mean(g >= cfg$final_tmle$g_upper - 1e-12),
                   mean(g * tmle_fit$pi_1W < cfg$final_tmle$positivity_warning_threshold),
                   mean((1 - g) * tmle_fit$pi_0W < cfg$final_tmle$positivity_warning_threshold)),
      count = c(sum(g < 0.05), sum(g > 0.95), sum(g <= cfg$final_tmle$g_lower + 1e-12),
                sum(g >= cfg$final_tmle$g_upper - 1e-12),
                sum(g * tmle_fit$pi_1W < cfg$final_tmle$positivity_warning_threshold),
                sum((1 - g) * tmle_fit$pi_0W < cfg$final_tmle$positivity_warning_threshold)),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(pos, cfg, out_dir, cfg$diagnostics$positivity_summary_csv %||% "positivity_summary.csv")
    }
    if (isTRUE(cfg$diagnostics$plot_propensity)) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "propensity_by_exposure.png")
      png(plot_path, width = 1000, height = 700, res = 150)
      par(mar = c(4, 4, 2, 1))
      hist(g[A == 0L], breaks = 30, main = "Propensity by exposure", xlab = "Predicted P(A = 1 | W)", xlim = c(0, 1))
      hist(g[A == 1L], breaks = 30, add = TRUE)
      abline(v = c(cfg$final_tmle$g_lower, cfg$final_tmle$g_upper, 0.05, 0.95), lty = c(1, 1, 2, 2))
      legend("topright", legend = c("A=0", "A=1", "truncation/near-violation lines"), bty = "n")
      dev.off()
    }
  }

  # ---- Cluster-level influence diagnostics ---------------------------------
  if (!is.null(tmle_fit) && !is.null(tmle_fit$cluster_eic)) {
    ce <- tmle_fit$cluster_eic
    cl_names <- names(ce)
    cluster_chr <- as.character(cluster)
    cl_names_chr <- as.character(cl_names)
    cluster_summary <- data.frame(
      cluster = names(table(cluster_chr)),
      cluster_size = as.integer(table(cluster_chr)),
      treated_count = as.integer(tapply(A == 1L, cluster_chr, sum)),
      outcome_observed_count = as.integer(tapply(delta_Y == 1L, cluster_chr, sum)),
      weight_sum = as.numeric(tapply(w, cluster_chr, sum, na.rm = TRUE)),
      stringsAsFactors = FALSE)
    cl_diag <- data.frame(
      cluster = cl_names_chr,
      cluster_eic = as.numeric(ce),
      abs_cluster_eic = abs(as.numeric(ce)),
      stringsAsFactors = FALSE)
    cl_diag <- merge(cl_diag, cluster_summary, by = "cluster", all.x = TRUE, sort = FALSE)
    if (anyNA(cl_diag$cluster_size)) {
      bad <- cl_diag$cluster[is.na(cl_diag$cluster_size)]
      warning(sprintf("Cluster influence diagnostics: %d cluster(s) in cluster_eic were not found in the analytic data: %s",
                      length(bad), paste(head(bad, 10L), collapse = ", ")))
    }
    denom <- sum(cl_diag$cluster_eic^2)
    cl_diag$variance_share <- if (denom > 0) cl_diag$cluster_eic^2 / denom else NA_real_
    cl_diag <- cl_diag[order(-cl_diag$abs_cluster_eic), , drop = FALSE]
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(head(cl_diag, 10L), cfg, out_dir, cfg$diagnostics$top_cluster_influence_csv %||% "top_cluster_influence.csv")
      write_diag_csv(cl_diag, cfg, out_dir, "cluster_influence_all.csv")
      if (!is.null(tmle_fit$D_orig) && !is.null(tmle_fit$weights)) {
        top_abs <- head(cl_diag$cluster, 5L)
        leave_rows <- do.call(rbind, lapply(c(1L, 3L, 5L), function(kdrop) {
          drop_cl <- head(top_abs, kdrop)
          keep <- !(as.character(cluster) %in% drop_cl)
          if (!any(keep)) return(NULL)
          ww <- tmle_fit$weights[keep]
          psi <- sum(ww * (tmle_fit$Qstar1W_orig[keep] - tmle_fit$Qstar0W_orig[keep]), na.rm = TRUE) / sum(ww, na.rm = TRUE)
          # Reference is the full-sample ATE (ate_tmle), since this leave-out
          # plug-in is computed over all remaining rows (untrimmed). Falls back
          # to estimate if ate_tmle is absent (older result objects).
          ref_full <- if (!is.null(tmle_fit$result$ate_tmle)) tmle_fit$result$ate_tmle[1] else tmle_fit$result$estimate[1]
          data.frame(drop_top_abs_clusters = kdrop, dropped_clusters = paste(drop_cl, collapse = ";"),
                     n_remaining = sum(keep), estimate_plugin_targeted_remaining = psi,
                     difference_from_full_ate = psi - ref_full,
                     stringsAsFactors = FALSE)
        }))
        if (!is.null(leave_rows) && nrow(leave_rows) > 0L)
          write_diag_csv(leave_rows, cfg, out_dir, cfg$diagnostics$cluster_influence_leaveout_csv %||% "cluster_influence_leaveout.csv")
      }
    }
    if (isTRUE(cfg$diagnostics$plot_qq_eic)) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "cluster_eic_qq.png")
      png(plot_path, width = 900, height = 900, res = 150)
      par(mar = c(4, 4, 2, 1))
      qqnorm(tmle_fit$cluster_eic, main = "QQ plot: cluster-level EIC sums")
      qqline(tmle_fit$cluster_eic)
      dev.off()
    }
  }

  # ---- Overlap product plot -------------------------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$plot_overlap_product)) {
    op1 <- tmle_fit$gn * tmle_fit$pi_1W
    op0 <- (1 - tmle_fit$gn) * tmle_fit$pi_0W
    thr <- cfg$final_tmle$positivity_warning_threshold
    plot_path <- build_unique_diag_path(cfg, out_dir, "overlap_product.png")
    png(plot_path, width = 1000, height = 700, res = 150)
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    hist(op1, breaks = 30, main = "g(W) * pi(1, W)", xlab = "Product")
    abline(v = thr, lty = 2)
    hist(op0, breaks = 30, main = "(1 - g(W)) * pi(0, W)", xlab = "Product")
    abline(v = thr, lty = 2)
    dev.off()
  }

  # ---- Fold time and outcome distribution ----------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$plot_fold_times)) {
    plot_path <- build_unique_diag_path(cfg, out_dir, "fold_times.png")
    png(plot_path, width = 900, height = 600, res = 150)
    par(mar = c(4, 4, 2, 1))
    barplot(tmle_fit$fold_times, names.arg = seq_along(tmle_fit$fold_times),
            xlab = "Outer fold", ylab = "Seconds", main = "Final CV-TMLE per-fold runtime")
    dev.off()
  }
  if (isTRUE(cfg$diagnostics$plot_outcome_distribution)) {
    y <- main_df[[cfg$analysis$outcome_var]]
    y_ok <- y[is.finite(y)]
    if (length(y_ok) > 0L) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "outcome_distribution.png")
      png(plot_path, width = 900, height = 700, res = 150)
      par(mar = c(4, 4, 2, 1))
      hist(y_ok, breaks = 40, main = "Outcome distribution", xlab = cfg$analysis$outcome_var)
      dev.off()
    }
  }

  # ---- Approximate E-value --------------------------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$save_csvs)) {
    ev <- make_evalue_approx(tmle_fit$result$estimate[1], tmle_fit$result$ci_lower[1],
                             tmle_fit$result$ci_upper[1], main_df[[cfg$analysis$outcome_var]])
    write_diag_csv(ev, cfg, out_dir, cfg$diagnostics$evalue_csv %||% "evalue_sensitivity.csv")
  }

  msg("===== Diagnostics complete. =====\n", cfg = cfg)
  invisible(sample_flow)
}


# =============================================================================
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
.run_single_pipeline <- function(cfg, w1_all_cached = NULL) {
  timers <- make_timer_log()
  timers$start("total")
  ensure_output_dir(cfg$global$output_dir)

  run_tag <- build_run_tag(cfg)
  # Give this run its own checkpoint directory so different waves
  # /outcomes do not reuse each other's per-fold caches.
  cfg$global$checkpoint_subdir <- paste0("checkpoints_cvtmle__", run_tag)

  msg(sprintf("\n############################################################"), cfg = cfg)
  msg(sprintf("  RUN TAG: %s", run_tag), cfg = cfg)
  msg(sprintf("    family       = %s", cfg$outcome$family), cfg = cfg)
  msg(sprintf("    wave         = %s", cfg$outcome$current_wave %||% "NA"), cfg = cfg)
  msg(sprintf("    family_member= %s", cfg$outcome$family_member %||% "NA"), cfg = cfg)
  msg(sprintf("    output_dir   = %s", cfg$global$output_dir), cfg = cfg)
  msg(sprintf("############################################################\n"), cfg = cfg)

  wave1_path <- file.path(cfg$global$output_dir, cfg$cache$wave1_rds)
  # Main-dataset cache is also run-tag-specific, because different outcome
  # families / waves construct different main_df objects.
  main_filename <- sub("\\.rds$",
    paste0("__", run_tag, ".rds"), cfg$cache$main_dataset_rds)
  main_path <- file.path(cfg$global$output_dir, main_filename)

  raw_w1_rows <- NA_integer_

  # --- Stage: read wave 1 -----------------------------------------------
  w1_all <- w1_all_cached
  if (is.null(w1_all) && isTRUE(cfg$stages$run_read_wave1_phase)) {
    if (isTRUE(cfg$cache$use_cached_wave1) && file.exists(wave1_path)) {
      msg(sprintf("  [cache] Loading cached wave 1 merge from %s", wave1_path), cfg = cfg)
      w1_all <- load_wave1_cache(wave1_path, cfg)
    }
    if (is.null(w1_all)) {
      timers$start("read_wave1")
      w1_all <- read_wave1_merged(cfg)
      timers$stop("read_wave1")
      if (isTRUE(cfg$cache$save_intermediate_rds)) {
        save_wave1_cache(w1_all, wave1_path, cfg)
        msg(sprintf("  [cache] Saved wave 1 merge to %s", wave1_path), cfg = cfg)
      }
    }
  }
  if (!is.null(w1_all)) raw_w1_rows <- nrow(w1_all)

  # --- Stage: build main dataset ----------------------------------------
  main_df <- NULL
  if (isTRUE(cfg$stages$run_build_main_dataset_phase)) {
    if (isTRUE(cfg$cache$use_cached_main_dataset) && file.exists(main_path)) {
      msg(sprintf("  [cache] Loading cached main dataset from %s", main_path), cfg = cfg)
      main_df <- load_main_dataset_cache(main_path, cfg, w1_all)
    }
    if (is.null(main_df)) {
      timers$start("build_main")
      main_df <- build_main_dataset(w1_all, cfg)
      timers$stop("build_main")
      if (isTRUE(cfg$cache$save_intermediate_rds)) {
        save_main_dataset_cache(main_df, main_path, cfg, w1_all)
        msg(sprintf("  [cache] Saved main dataset to %s", main_path), cfg = cfg)
      }
    }
  }
  # --- Default stages: final TMLE performs nested data-driven screening -------
  tmle_fit <- NULL
  if (isTRUE(cfg$stages$run_final_cv_tmle))
    tmle_fit <- run_final_cv_tmle(
      cfg, main_df,
      timers = timers)

  if (isTRUE(cfg$stages$run_diagnostics))
    run_peer_review_diagnostics(cfg, main_df,
      prescreen_results = NULL,
      tmle_fit = tmle_fit,
      raw_w1_rows = raw_w1_rows)

  timers$stop("total")
  write_run_csv(timers$get(), cfg, "pipeline_timings.csv")

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
  validate_cfg(cfg)
  load_required_packages(cfg)
  ensure_output_dir(cfg$global$output_dir)

  # Normalize wave selection -- outcome waves are 3:5 only.
  waves <- cfg$outcome$waves
  if (identical(waves, "all")) waves <- 3:5
  waves <- as.integer(waves)
  msg(sprintf("[run_addhealth_pipeline] Will iterate over waves: %s.",
    paste(waves, collapse = ", ")), cfg = cfg)
  msg(sprintf("[run_addhealth_pipeline] Outcome family: %s.", cfg$outcome$family), cfg = cfg)

  # For nested-binary families with no family_member set, run all members.
  fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
  if (identical(fam_cfg$type, "binary_nested")) {
    members <- cfg$outcome$family_member
    if (is.null(members)) {
      members <- names(fam_cfg$members)
      msg(sprintf("[run_addhealth_pipeline] family_member not set; will run all %d thresholds: %s.",
        length(members), paste(members, collapse = ", ")), cfg = cfg)
    }
  } else {
    members <- list(NULL)
  }

  # Pre-read wave 1 once (independent of outcome family) and share across runs.
  w1_all_shared <- NULL
  if (isTRUE(cfg$stages$run_read_wave1_phase)) {
    wave1_path <- file.path(cfg$global$output_dir, cfg$cache$wave1_rds)
    if (isTRUE(cfg$cache$use_cached_wave1) && file.exists(wave1_path)) {
      msg(sprintf("[pipeline] Loading cached wave 1 merge from %s", wave1_path), cfg = cfg)
      w1_all_shared <- load_wave1_cache(wave1_path, cfg)
    }
    if (is.null(w1_all_shared)) {
      timers0 <- make_timer_log(); timers0$start("read_wave1")
      w1_all_shared <- read_wave1_merged(cfg)
      timers0$stop("read_wave1")
      if (isTRUE(cfg$cache$save_intermediate_rds))
        save_wave1_cache(w1_all_shared, wave1_path, cfg)
    }
  }

  all_results <- list()
  summary_rows <- list()

  for (w in waves) {
    for (m in members) {
      cfg_run <- cfg
      cfg_run$outcome$current_wave  <- w
      cfg_run$outcome$family_member <- m

      tag_sub <- build_run_tag(cfg_run)
      cfg_run$global$output_dir <- file.path(cfg$global$output_dir, tag_sub)
      ensure_output_dir(cfg_run$global$output_dir)

      res <- tryCatch(.run_single_pipeline(cfg_run, w1_all_cached = w1_all_shared),
        error = function(e) {
          message(sprintf("[pipeline] Run '%s' failed: %s",
            build_run_tag(cfg_run), conditionMessage(e)))
          list(skipped = TRUE, reason = conditionMessage(e))
        })
      all_results[[build_run_tag(cfg_run)]] <- res

      if (!isTRUE(res$skipped) && !is.null(res$tmle_fit)) {
        rr <- res$tmle_fit$result
        # %||% guards keep this robust if an older result object lacks the
        # v6.15 columns (e.g., a cached fold from a prior version).
        get1 <- function(field) if (!is.null(rr[[field]])) rr[[field]][1] else NA_real_
        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
          run_tag        = build_run_tag(cfg_run),
          family         = cfg_run$outcome$family,
          wave           = w,
          family_member  = m %||% NA_character_,
          n              = rr$n,
          n_clusters     = rr$n_clusters,
          estimate       = rr$estimate,
          se             = rr$se,
          ci_lower       = rr$ci_lower,
          ci_upper       = rr$ci_upper,
          p_value        = rr$p_value,
          primary_estimand = if (!is.null(rr$primary_estimand)) rr$primary_estimand[1] else "ate",
          ate_tmle_full  = get1("ate_tmle"),
          ate_se_full    = get1("ate_se_full"),
          ate_plugin_initial = get1("ate_plugin_initial"),
          ate_aipw_initial   = get1("ate_aipw_initial"),
          att_estimate   = get1("att_estimate"),
          att_se         = get1("att_se"),
          att_G_star             = get1("att_G_star"),
          att_se_multiplier_boot = get1("att_se_multiplier_boot"),
          ci_lower_normal = get1("ci_lower_normal"),
          ci_upper_normal = get1("ci_upper_normal"),
          p_normal        = get1("p_normal"),
          att_cf_under_control = get1("att_cf_under_control"),
          att_support_ok  = get1("att_support_ok"),
          trim_ate_estimate = get1("trim_ate_estimate"),
          trim_ate_se    = get1("trim_ate_se"),
          n_trimmed      = if (!is.null(rr$n_trimmed)) rr$n_trimmed[1] else NA_integer_,
          stringsAsFactors = FALSE)
      }
    }
  }

  if (length(summary_rows) > 0L) {
    combined <- do.call(rbind, summary_rows)
    write_run_csv(combined, cfg, "combined_tmle_results.csv")
  } else {
    combined <- data.frame()
  }
  invisible(list(results = all_results, combined = combined, summary = combined))
}



# =============================================================================
# 11) PREFLIGHT UNIT TEST (synthetic data)
# =============================================================================
# Plain-English role: a fast end-to-end run on a tiny synthetic dataset to
# detect obvious breakage (missing functions, signature bugs, TMLE failing
# to produce a finite number) before you launch the multi-hour real run.
# Takes ~30-60 seconds on 4 cores.

run_preflight_unit_test <- function(cfg_template) {
  message("=== Preflight unit test: start ===")
  set.seed(1)
  n <- 300L; J <- 30L
  cluster <- sample(seq_len(J), n, replace = TRUE)
  W1 <- stats::rnorm(n); W2 <- stats::rbinom(n, 1, 0.3)
  A  <- stats::rbinom(n, 1, plogis(-1 + 0.5 * W1 + 0.6 * W2))
  Y  <- 1 + 0.4 * A + 0.3 * W1 + 0.2 * W2 + stats::rnorm(n, sd = 0.7)
  delta <- stats::rbinom(n, 1, plogis(2 - 0.5 * A + 0.2 * W1))
  Y_obs <- ifelse(delta == 1L, Y, NA_real_)
  df <- data.frame(
    AID = seq_len(n), PSUSCID = cluster, GSWGT1 = stats::runif(n, 0.5, 2),
    Depressed = A, Y = Y_obs, delta_Y = delta,
    W1 = W1, W2 = factor(ifelse(W2 == 1L, "yes", "no")),
    W3 = stats::rnorm(n), W4 = factor(sample(letters[1:4], n, replace = TRUE)))

  cfg_pre <- cfg_template
  cfg_pre$stages <- list(
    run_preflight_unit_test = FALSE,
    run_read_wave1_phase = FALSE,
    run_build_main_dataset_phase = FALSE,
    run_final_cv_tmle = TRUE,
    run_diagnostics = FALSE)
  cfg_pre$cache$use_cached_wave1 <- FALSE
  cfg_pre$cache$use_cached_main_dataset <- FALSE
  cfg_pre$global$output_dir <- tempfile("preflight_"); dir.create(cfg_pre$global$output_dir)
  cfg_pre$global$verbose <- TRUE
  cfg_pre$final_tmle$vfolds <- 2L
  # preflight uses the full-sample ATE as headline so it tests the
  # machinery end-to-end and cannot abort if the small synthetic sample has
  # no rows inside the trim band. The real run keeps primary_estimand from cfg.
  # the headline of the REAL run is the ATT, so the preflight must also
  # exercise the ATT path (one-step EIF estimator, bounded residual, cluster-
  # robust SE, centering check). We keep the ATE as the run's primary_estimand
  # (trim-band safety on the tiny synthetic sample) but enable report_att so the
  # ATT is computed and then validated below.
  cfg_pre$final_tmle$primary_estimand <- "ate"
  cfg_pre$final_tmle$report_att <- TRUE
  cfg_pre$final_tmle$internal_superlearner_folds <- 2L
  cfg_pre$final_tmle$rough_folds <- 2L
  cfg_pre$final_tmle$rough_top_n_outcome <- 3L
  cfg_pre$final_tmle$rough_top_n_missingness <- 2L
  cfg_pre$final_tmle$rough_top_n_joint_AY <- 2L
  cfg_pre$final_tmle$rough_top_n_exposure_only <- 0L
  cfg_pre$final_tmle$rough_top_n_exposure_for_lasso <- 3L
  cfg_pre$final_tmle$rough_candidate_pool_max <- 5L
  cfg_pre$final_tmle$rough_max_total_vars <- 4L
  cfg_pre$final_tmle$nested_lasso_after_rough <- TRUE
  cfg_pre$final_tmle$lasso_screen_min_vars <- 2L
  cfg_pre$final_tmle$lasso_screen_max_vars <- 4L
  cfg_pre$final_tmle$lasso_screen_max_processed_cols <- 12L
  cfg_pre$final_tmle$final_max_processed_columns <- 12L
  cfg_pre$final_tmle$use_epp_cap <- FALSE
  cfg_pre$final_tmle$hard_max_processed_columns <- 20L
  cfg_pre$final_tmle$lasso_screen_folds <- 2L
  cfg_pre$final_tmle$cluster_aware_internal_cv <- FALSE
  cfg_pre$outcome$current_wave <- 4L
  cfg_pre$outcome$family_member <- NULL
  cfg_pre$learners$Q$use_ranger <- FALSE; cfg_pre$learners$Q$use_xgboost <- FALSE
  cfg_pre$learners$Q$use_earth  <- FALSE
  cfg_pre$learners$g$use_ranger <- FALSE; cfg_pre$learners$g$use_xgboost <- FALSE
  cfg_pre$learners$pi$use_ranger <- FALSE; cfg_pre$learners$pi$use_xgboost <- FALSE
  load_required_packages(cfg_pre)
  res <- run_final_cv_tmle(cfg_pre, df)
  ok <- is.finite(res$result$estimate) && is.finite(res$result$se) && res$result$se > 0
  if (!ok) stop("Preflight unit test FAILED: estimate/SE not finite.", call. = FALSE)
  # validate the ATT path specifically, since the ATT is the real run's
  # headline. Confirm the ATT estimate and cluster-robust SE are finite and the
  # SE is positive, and that the ATT EIF centering check is at machine-zero
  # (the centering identity must hold for the one-step ATT SE to be valid).
  att_e  <- res$result$att_estimate
  att_s  <- res$result$att_se
  att_c  <- res$result$att_eif_weighted_mean
  att_ok <- is.finite(att_e) && is.finite(att_s) && att_s > 0
  if (!att_ok)
    stop("Preflight unit test FAILED: ATT estimate/SE not finite or SE <= 0.", call. = FALSE)
  att_center_tol <- 1e-6 * max(1, abs(att_e))
  if (!is.finite(att_c) || abs(att_c) > att_center_tol)
    stop(sprintf("Preflight unit test FAILED: ATT EIF centering = %.3e exceeds tol %.1e.",
                 att_c, att_center_tol), call. = FALSE)
  message(sprintf("Preflight unit test OK. ATE Psi=%.3f SE=%.3f | ATT=%.3f SE=%.3f (centering %.1e).",
                  res$result$estimate, res$result$se, att_e, att_s, att_c))
  invisible(TRUE)
}


# =============================================================================
# 12) SENSITIVITY-ANALYSIS RUNNER WITH PROGRESS + RESUME
# =============================================================================
# Plain-English role: run a prespecified list of sensitivity-analysis
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
    S0_main = list(label = "Main specification", overlay = list()),
    S1_smaller_rough_cap = list(
      label = "Nested rough screen: smaller cap (vs v6.15 baseline 120/220)",
      overlay = list(final_tmle = list(
        rough_top_n_outcome = 60L,
        rough_top_n_missingness = 25L,
        rough_top_n_joint_AY = 30L,
        rough_top_n_exposure_for_lasso = 40L,
        rough_candidate_pool_max = 90L,
        lasso_screen_max_vars = 45L,
        lasso_screen_max_processed_cols = 100L,
        rough_max_total_vars = 45L,
        final_max_processed_columns = 100L))),
    S2_larger_rough_cap = list(
      label = "Nested rough screen: larger cap (vs v6.15 baseline 120/220)",
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
        hard_max_processed_columns = 400L))),
    S3_tighter_g = list(
      label = "Tighter propensity truncation (0.05 / 0.95)",
      overlay = list(final_tmle = list(g_lower = 0.05, g_upper = 0.95))),
    S4_wider_g = list(
      label = "Wider propensity truncation (0.01 / 0.99)",
      overlay = list(final_tmle = list(g_lower = 0.01, g_upper = 0.99))),
    S5_alt_cutpoint = list(
      label = "Depression cutpoint = 24 (vs 22)",
      overlay = list(exposure = list(cutpoint = 24))),
    S6_alt_cutpoint_low = list(
      label = "Depression cutpoint = 20 (vs 22)",
      overlay = list(exposure = list(cutpoint = 20))),
    S7_untransformed_earnings = list(
      label = "Earnings, untransformed (no log)",
      overlay = list(outcome = list(log_transform = FALSE))),
    S8_no_xgboost = list(
      label = "Final learner library without xgboost",
      overlay = list(learners = list(
        Q = list(use_xgboost = FALSE),
        g = list(use_xgboost = FALSE),
        pi = list(use_xgboost = FALSE)))),
    # pre-specified-W comparison. INERT BY DEFAULT: prespecified_W is
    # NULL so this scenario falls back to the normal data-driven screen. To
    # actually run the no-data-driven-selection comparison, replace NULL with
    # a character vector of REAL codebook variable names (e.g. sex, age, race,
    # parental education/income, family structure, region, baseline health).
    # Placeholder names are intentionally NOT used, because a list of names
    # absent from the data would error at the screen step.
    S9_prespecified_W = list(
      label = "Pre-specified W (INERT until real variable names supplied)",
      overlay = list(final_tmle = list(prespecified_W = NULL))),
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
        trim_enable = TRUE, trim_g_lower = 0.10, trim_g_upper = 0.90))),
    # ATT-specific robustness. S11b caps the upper propensity bound to
    # limit extreme control odds-weights g/(1-g) (the ATT-specific positivity
    # concern); if the ATT is stable under this cap, the control-weight extremes
    # are not driving it. S11c reports the ATT headline explicitly for the
    # primary specification (redundant with the default once primary_estimand
    # = "att", but kept so the sensitivity table has an explicit ATT row).
    S11b_att_gcap = list(
      label = "ATT with upper propensity bound g_upper = 0.90 (caps control odds-weights)",
      overlay = list(final_tmle = list(
        primary_estimand = "att", g_upper = 0.90))),
    S11c_att_primary = list(
      label = "ATT as primary estimand (headline specification)",
      overlay = list(final_tmle = list(primary_estimand = "att"))),
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
    # tight symmetric propensity bounds, run as a
    # sensitivity scenario rather than a default. pi_upper kept at 0.999 in
    # the default; this scenario tests the fully symmetric [0.05, 0.95]
    # variant for both g and pi.
    S16_tight_propensity = list(
      label = "Tight symmetric propensity/censoring bounds (0.05 / 0.95)",
      overlay = list(final_tmle = list(
        g_lower = 0.05, g_upper = 0.95, pi_lower = 0.05, pi_upper = 0.95))),
    # v6.22 (point 5): the main spec now uses RAW sampling weights
    # (weight_winsor_quantile = NULL), so the old S17 (winsor = NULL) merely
    # duplicated main and S18's "vs q0.95" label was stale. Both are now real
    # contrasts against the raw-weight main.
    S17_weight_winsor_95 = list(
      label = "Weight winsorization at q0.95 (vs raw sampling weights, main)",
      overlay = list(analysis = list(weight_winsor_quantile = 0.95))),
    S18_weight_winsor_99 = list(
      label = "Weight winsorization at q0.99 (vs raw sampling weights, main)",
      overlay = list(analysis = list(weight_winsor_quantile = 0.99))),
    # v6.22 (point 5): a TRUE full-refit earnings-cap sensitivity. Unlike the
    # post-hoc att_outcome_bound diagnostic (which holds the fitted nuisances
    # fixed), the sensitivity runner rebuilds the outcome, re-runs screening,
    # refits nuisances, and recomputes the ATT under a q0.995 cap (vs q0.99).
    S19_bound_refit_q0995 = list(
      label = "Full-refit earnings cap q0.995 (rebuilds outcome/screen/nuisances/ATT)",
      overlay = list(outcome = list(continuous_upper_quantile = 0.995)))
  )
}


# v6.22 (point 4): CURATED final sensitivity set for the paper -- ONLY the
# high-value checks, not the development sweeps. Use as:
#   run_sensitivity_analyses(cfg, scenarios = final_sensitivity_scenarios(
#     mh_block = c(<baseline MH var names>), negative_control_outcome = "<var>"))
# F2 uses protected_W (augments the data-driven screen; does NOT replace it like
# prespecified_W). F3 requires a pre-determined negative-control outcome; VERIFY
# the overlay routes to your column -- if your outcome is family-constructed, set
# the family/source instead of analysis$outcome_var.
final_sensitivity_scenarios <- function(mh_block = NULL,
                                        negative_control_outcome = NULL,
                                        cap_quantile = 0.995) {
  sc <- list(
    S0_main = list(label = "Main specification (curated final)", overlay = list()),
    F1_bound_refit = list(
      label = sprintf("Full-refit earnings cap q%.3f (rebuilds outcome/screen/nuisances/ATT)", cap_quantile),
      overlay = list(outcome = list(continuous_upper_quantile = cap_quantile)))
  )
  if (!is.null(mh_block) && length(mh_block)) {
    sc$F2_forced_mh_block <- list(
      label = "Forced/protected baseline mental-health block (data-driven screen + protected_W)",
      overlay = list(final_tmle = list(protected_W = mh_block)))
  }
  if (!is.null(negative_control_outcome)) {
    # A REAL negative control swaps the outcome FAMILY to a pass-through that
    # reads the named pre-existing column directly. (Setting analysis$outcome_var
    # alone does NOT work: construct_outcome dispatches on cfg$outcome$family, so
    # it would build the HEADLINE outcome and merely relabel the column.) The
    # column must already be present in the built dataset; log_transform is turned
    # off (raw column). Set analysis$outcome_type = "binary" in base_cfg first if
    # the negative-control column is binary. SMOKE-TEST before trusting it.
    sc$F3_negative_control <- list(
      label = sprintf("Negative-control outcome via PassThrough column '%s' (expect ~0 under unconfoundedness)",
                       negative_control_outcome),
      overlay = list(
        outcome = list(
          family = "PassThrough",
          log_transform = FALSE,
          families = list(PassThrough = list(type = "continuous",
                                             source_var = negative_control_outcome)))))
  }
  sc
}


run_sensitivity_analyses <- function(base_cfg,
                                     scenarios = default_sensitivity_scenarios(),
                                     out_dir = NULL) {
  out_dir <- out_dir %||% base_cfg$global$output_dir
  ensure_output_dir(out_dir)
  message(sprintf("\n===== STAGE: Sensitivity analyses (%d scenarios) =====", length(scenarios)))
  message(sprintf("  Output base directory: %s", out_dir))
  results_csv <- file.path(out_dir, "sensitivity_results.csv")
  checkpoint  <- file.path(out_dir, "sensitivity_checkpoint.rds")
  done <- character(0)
  if (file.exists(checkpoint)) {
    done <- readRDS(checkpoint)
    message(sprintf("  [sensitivity] Resuming: %d scenarios already done (%s).",
      length(done), paste(done, collapse = ", ")))
  }
  for (nm in names(scenarios)) {
    if (nm %in% done) {
      message(sprintf("  [sensitivity] SKIP %s (already done).", nm))
      next
    }
    sc <- scenarios[[nm]]
    message(sprintf("\n  [sensitivity] Running scenario %s -- %s", nm, sc$label))
    t_sc <- proc.time()[3]
    cfg_k <- merge_cfg_overlay(base_cfg, sc$overlay)
    cfg_k$global$output_dir <- file.path(out_dir, paste0("sens_", nm))
    ensure_output_dir(cfg_k$global$output_dir)
    cfg_k$global$run_label <- nm   # v6: tags every CSV in this scenario
    cfg_k$stages$run_preflight_unit_test <- FALSE
    cfg_k$stages$run_read_wave1_phase    <- TRUE  # v6: pipeline re-reads; cache handles duplicates
    cfg_k$stages$run_build_main_dataset_phase <- TRUE
    cfg_k$cache$use_cached_wave1         <- TRUE
    cfg_k$cache$use_cached_main_dataset  <- TRUE
    r <- tryCatch(run_addhealth_pipeline(cfg_k), error = function(e) {
      message(sprintf("  [sensitivity] scenario %s FAILED: %s", nm, conditionMessage(e)))
      NULL
    })
    # v6: run_addhealth_pipeline now returns a summary across waves.
    # Flatten: append every non-skipped successful run's summary row with the
    # scenario id prepended.
    if (!is.null(r) && !is.null(r$summary) && nrow(r$summary) > 0L) {
      rows <- r$summary
      rows <- cbind(scenario = nm, scenario_label = sc$label, rows)
      append_hdr <- !file.exists(results_csv)
      suppressWarnings(write.table(rows, results_csv, sep = ",",
        row.names = FALSE, col.names = append_hdr, append = !append_hdr))
      message(sprintf("  [sensitivity] Scenario %s complete in %.1fs; wrote %d result rows.",
        nm, proc.time()[3] - t_sc, nrow(rows)))
    } else {
      message(sprintf("  [sensitivity] Scenario %s produced no successful runs.", nm))
    }
    done <- c(done, nm); saveRDS(done, checkpoint)
  }
  message("\n===== Sensitivity analyses complete =====")
  if (file.exists(results_csv)) utils::read.csv(results_csv, stringsAsFactors = FALSE, comment.char = "#")
  else data.frame()
}


# =============================================================================
# 12b) MULTI-SEED ATT STABILITY + TWO-PART INFERENCE
# =============================================================================
# Pre-committed multi-seed runner. Runs the FULL pipeline once per seed (each
# ~ the normal per-run cost), collects the ATT estimate and its cluster-robust
# EIF SE from each run's summary, and reports the MEDIAN ATT (the reproducible
# point estimate) with a two-part variance (Zivich & Breskin 2021; DML median
# rule): median over seeds of [ within-seed EIF variance (att_se^2) +
# (att_seed - median_att)^2 ]. Mirrors run_sensitivity_analyses (checkpointed;
# per-seed table written before aggregation). Does NOT change the estimand or
# the estimator. PRE-COMMIT the seed vector; do not add/drop seeds after seeing
# results. VERIFY on the first (2-seed) run by reconciling the aggregate row
# against the raw per-seed table before trusting it.
run_multiseed_att <- function(base_cfg,
                              seeds = c(20260402L, 1L, 2L, 3L, 4L),
                              out_dir = NULL,
                              fresh = FALSE) {
  out_dir <- out_dir %||% base_cfg$global$output_dir
  ensure_output_dir(out_dir)
  message(sprintf("\n===== STAGE: Multi-seed ATT stability (%d seeds) =====", length(seeds)))
  per_seed_csv <- file.path(out_dir, "multiseed_att_per_seed.csv")
  agg_csv      <- file.path(out_dir, "multiseed_att_aggregate.csv")
  checkpoint   <- file.path(out_dir, "multiseed_checkpoint.rds")
  # fingerprint the seed set + ALL estimation-relevant config
  # blocks (compared with identical(), the same pattern as the fold checkpoint)
  # so a rerun in the same output dir with ANY changed config -- outcome family/
  # wave/member, continuous_upper_quantile, rough-screen or learner settings,
  # protected_W, g/pi bounds, preprocessing, exposure cutpoint, etc. -- does not
  # silently reuse stale per-seed results. On mismatch (or fresh=TRUE) start
  # clean. Arbitrary CODE (not config) changes are still not detected; bump
  # base_cfg$global$version or call with fresh=TRUE for those.
  ms_sig <- list(
    seeds            = sort(as.integer(seeds)),
    version          = base_cfg$global$version %||% "NA",
    analysis         = base_cfg$analysis,
    exposure         = base_cfg$exposure,
    outcome          = base_cfg$outcome,
    preprocessing    = base_cfg$preprocessing,
    final_preprocess = base_cfg$final_preprocess,
    final_tmle       = base_cfg$final_tmle,
    learners         = base_cfg$learners
  )
  collected <- list()
  resume_ok <- FALSE
  if (!isTRUE(fresh) && file.exists(checkpoint)) {
    ck <- tryCatch(readRDS(checkpoint), error = function(e) NULL)
    if (is.list(ck) && !is.null(ck$sig) && identical(ck$sig, ms_sig)) {
      collected <- ck$data %||% list(); resume_ok <- TRUE
    } else {
      message("  [multiseed] checkpoint fingerprint changed (or unreadable); starting fresh.")
    }
  }
  if (!resume_ok) {
    # Not resuming (fresh=TRUE, no checkpoint, or fingerprint mismatch): clear any
    # stale per-seed table AND checkpoint together, so a leftover CSV from a prior
    # run can never be appended to (which would corrupt the aggregate).
    if (file.exists(per_seed_csv)) file.remove(per_seed_csv)
    if (file.exists(checkpoint)) file.remove(checkpoint)
  }
  for (s in seeds) {
    key <- as.character(s)
    if (!is.null(collected[[key]])) {
      message(sprintf("  [multiseed] seed %s already done; skipping.", key)); next
    }
    cfg_s <- base_cfg
    cfg_s$global$pipeline_seed          <- as.integer(s)
    cfg_s$global$run_label              <- paste0("seed", key)
    cfg_s$cache$use_cached_wave1        <- TRUE
    cfg_s$cache$use_cached_main_dataset <- TRUE
    r <- tryCatch(run_addhealth_pipeline(cfg_s), error = function(e) {
      message(sprintf("  [multiseed] seed %s FAILED: %s", key, conditionMessage(e))); NULL
    })
    if (is.null(r) || is.null(r$summary) || nrow(r$summary) == 0L) next
    rows <- r$summary
    rows$seed <- as.integer(s)
    collected[[key]] <- rows
    saveRDS(list(sig = ms_sig, data = collected), checkpoint)
    append_hdr <- !file.exists(per_seed_csv)
    suppressWarnings(write.table(rows, per_seed_csv, sep = ",",
      row.names = FALSE, col.names = append_hdr, append = !append_hdr))
    message(sprintf("  [multiseed] seed %s complete; wrote %d row(s).", key, nrow(rows)))
  }
  if (!length(collected)) { message("  [multiseed] no successful seeds."); return(invisible(NULL)) }
  # Direct rbind normally succeeds (all seeds share the pipeline's summary schema).
  # Guard against schema drift from a resumed checkpoint written by an earlier
  # build (a column added later): fall back to the common columns rather than
  # crash. If att_G_star survives the intersection the df combination still uses
  # it; if not, the aggregate degrades to M-1 df (handled below).
  all_rows <- tryCatch(do.call(rbind, collected), error = function(e) {
    common <- Reduce(intersect, lapply(collected, names))
    message(sprintf("  [multiseed] per-seed schema drift; aligning on %d common columns.", length(common)))
    do.call(rbind, lapply(collected, function(d) d[, common, drop = FALSE]))
  })
  if (!all(c("att_estimate", "att_se") %in% names(all_rows))) {
    message("  [multiseed] att_estimate/att_se not found in summary; wrote per-seed only.")
    return(invisible(list(per_seed = all_rows, aggregate = NULL)))
  }
  grp_cols  <- intersect(c("family", "wave", "estimand"), names(all_rows))
  split_key <- if (length(grp_cols))
    do.call(interaction, c(as.list(all_rows[grp_cols]), list(drop = TRUE))) else
    factor(rep("all", nrow(all_rows)))
  agg <- do.call(rbind, lapply(split(all_rows, split_key), function(d) {
    ok  <- is.finite(d$att_estimate) & is.finite(d$att_se)
    est <- d$att_estimate[ok]; se <- d$att_se[ok]
    if (!length(est)) return(NULL)
    med    <- stats::median(est)
    v_two  <- stats::median(se^2 + (est - med)^2)   # two-part variance (median rule)
    se_two <- sqrt(v_two)
    k <- length(est); df_between <- max(k - 1L, 1L)
    # v6.22 (cross-review): the combined two-part SE is dominated by the
    # WITHIN-seed EIF variance, whose effective df is G*-1 (cluster sparsity),
    # not M-1. Using M-1 alone is mildly anti-conservative. Take the
    # conservative combined df = min(median(G*)-1, M-1). NOTE: the median rule
    # is not Rubin's variance decomposition, so we use this transparent minimum
    # rather than a Barnard-Rubin df (which assumes Rubin pooling). If per-seed
    # G* is unavailable, fall back to M-1.
    med_gstar <- if ("att_G_star" %in% names(d))
      suppressWarnings(stats::median(d$att_G_star[is.finite(d$att_G_star)])) else NA_real_
    df_eff_total <- if (is.finite(med_gstar)) max(min(med_gstar - 1, df_between), 1) else df_between
    tcrit <- stats::qt(0.975, df_eff_total)
    hdr <- if (length(grp_cols)) d[1, grp_cols, drop = FALSE] else data.frame(group = "all")
    cbind(hdr, data.frame(
      n_seeds = k,
      att_median = med,
      att_mean = mean(est),
      att_min = min(est),
      att_max = max(est),
      att_range = max(est) - min(est),
      se_within_rms = sqrt(mean(se^2)),
      se_between = stats::sd(est),
      se_two_part = se_two,
      df_between = df_between,
      median_G_star = med_gstar,
      df_effective_total = df_eff_total,
      ci_lower_two_part = med - tcrit * se_two,
      ci_upper_two_part = med + tcrit * se_two,
      p_two_part = 2 * stats::pt(-abs(med / se_two), df_eff_total),
      seeds = paste(sort(d$seed), collapse = ";"),
      stringsAsFactors = FALSE))
  }))
  utils::write.csv(agg, agg_csv, row.names = FALSE)
  message(sprintf("  [multiseed] wrote per-seed (%s) and aggregate (%s).",
    basename(per_seed_csv), basename(agg_csv)))
  message("  [multiseed] VERIFY: reconcile the aggregate against the per-seed table by hand.")
  invisible(list(per_seed = all_rows, aggregate = agg))
}


# =============================================================================
# 13) AUTORUN BLOCK
# =============================================================================
if (isTRUE(cfg$global$autorun_pipeline)) {
  if (isTRUE(cfg$stages$run_preflight_unit_test)) {
    try(run_preflight_unit_test(cfg), silent = FALSE)
  }
  if (isTRUE(cfg$stages$run_multiseed_att)) {
    # multi-seed final inference runs the full pipeline once
    # per seed in global$multiseed_seeds and writes multiseed_att_*.csv. It
    # supersedes the single run (each seed run is a full pipeline execution).
    results <- run_multiseed_att(cfg,
      seeds = cfg$global$multiseed_seeds %||% c(20260402L, 1L, 2L, 3L, 4L))
  } else {
    results <- run_addhealth_pipeline(cfg)
  }
  # To also run sensitivity analyses after the main run, uncomment:
  # sensitivity_table <- run_sensitivity_analyses(cfg)
}
