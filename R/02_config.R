# Generated from the reviewed v8.28 production source.
# Original lines: 178-1248.
# Module role: Default analysis configuration.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

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
    # Multi-seed stability is run only after the headline configuration is
    # locked. It multiplies runtime, so it is deliberately OFF for the first
    # fresh headline run; enable it explicitly afterward.
    run_multiseed_att              = FALSE
  ),

  # ---------------------------------------------------------------------------
  # Global controls: seed, verbosity, I/O.
  # ---------------------------------------------------------------------------
  global = list(
    # Chosen fixed headline partition for the fresh final run. Multi-seed
    # results are descriptive algorithmic-stability diagnostics only.
    pipeline_seed     = 1L,
    # Windows MAX_PATH is 260 chars. The run tag appears TWICE in every output
    # path (subdirectory + filename), so keep this root and run_label SHORT.
    # Override explicitly before run_addhealth_pipeline() if getwd() is deep.
    output_dir        = file.path(getwd(), "runs", format(Sys.time(), "%Y%m%d_%H%M%S")),
    # Auto-resolved when knitting/sourcing/RStudio succeeds. Set this explicitly
    # to the full .R or .Rmd path if your execution environment cannot resolve it.
    pipeline_source_path = .PIPELINE_SOURCE_PATH,
    require_script_md5 = TRUE,
    verbose           = TRUE,
    save_stage_csvs   = TRUE,
    autorun_pipeline  = FALSE,
    # version string participates in the multiseed checkpoint fingerprint.
    # BUMP THIS whenever you change analysis-relevant code, so a rerun in the
    # same output dir does not reuse stale per-seed results (or pass fresh=TRUE).
    version           = "v8.28_final_production",
    # Ten fixed partitions are the required algorithmic-stability set.
    # Never choose a seed based on the resulting ATT; the designated seed above
    # remains the inferential headline and the across-seed summary is descriptive.
    multiseed_seeds   = c(20260402L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
    multiseed_parallel_cores = 1L,   # >1 = run seeds in parallel, each in its own isolated subdir; 1 = sequential (default, unchanged behavior)
    multiseed_require_all_seeds = TRUE,   # TRUE (recommended for the final run) stops if any fixed seed fails, rather than aggregating only the successes
    checkpoint_subdir = "checkpoints_cvtmle_v8_28_final_production",
    # v6: a short label that tags every output file produced by this run.
    # E.g. "health_w3_edu=college" produces files like
    # "cv_tmle_results__health_w3_edu=college__<timestamp>.csv".
    # Short by design: build_run_tag() embeds this in the output subdirectory
    # AND in every filename, so each character costs two per path.
    # Excluded from analysis_spec_md5 by strip_runtime_config_state(), so
    # shortening it does NOT change the analysis fingerprint.
    run_label         = "p1",
    # One immutable identifier is generated at the start of a pipeline call
    # and reused by every result, diagnostic, manifest, and checkpoint path.
    run_id            = NULL,
    # FALSE removes the 27-char run_id from every FILENAME. The run_id is still
    # written into each CSV header block, so provenance is retained; the only
    # cost is that two runs sharing one output_dir would overwrite each other.
    append_timestamp_to_outputs = FALSE,
    # FALSE for every new primary/sensitivity run. Set TRUE only for an
    # explicitly validated checkpoint-resume operation.
    resume_mode = FALSE
  ),

  # ---------------------------------------------------------------------------
  # Frozen production software environment. Exact versions are checked after
  # the enabled package set is resolved and before any data are read.
  # ---------------------------------------------------------------------------
  runtime = list(
    enforce_exact_R_version = TRUE,
    required_R_version = "4.4.1",
    enforce_exact_package_versions = TRUE,
    required_package_versions = c(
      haven = "2.5.4", dplyr = "1.1.4", purrr = "1.0.2",
      SuperLearner = "2.0.29", glmnet = "4.1.8", survey = "4.4.2",
      ranger = "0.16.0", xgboost = "1.7.8.1", Matrix = "1.7.4"
    )
  ),

  # ---------------------------------------------------------------------------
  # Cache controls.
  # ---------------------------------------------------------------------------
  cache = list(
    use_cached_wave1        = FALSE,
    use_cached_main_dataset = FALSE,
    save_intermediate_rds   = TRUE,
    wave1_rds               = "wave1_merged_v8_24_final_run_corrected.rds",
    main_dataset_rds        = "main_dataset_v8_24_final_run_corrected_production.rds",
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
    strata_var           = "REGION",
    weight_var           = "GSWGT1",
    outcome_observed_var = "delta_Y",
    # Production integrity gates for the verified Wave-II CES-D / Wave-IV
    # Compensation analysis. Preflight disables these because it uses synthetic
    # data. A mismatch stops the run before any causal modeling.
    enforce_expected_sample_gates = TRUE,
    # The complete-CES-D, final-n, PSU, and stratum gates remain active in every
    # primary and sensitivity run. The exposure-cutpoint and treated-count
    # gates are separately switchable because a cutpoint sensitivity changes
    # the number treated by design while the underlying analytic cohort should
    # remain identical.
    enforce_expected_cutpoint_gate = TRUE,
    enforce_expected_treated_gate = TRUE,
    expected_complete_cesd_n = 14660L,
    expected_final_n = 13500L,
    expected_exposure_cutpoint = 22,
    expected_treated_n = 1342L,
    expected_cluster_n = 132L,
    expected_strata_n = 4L,
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
    weight_winsor_renormalize = FALSE,
    # hard-coded transform of H1GH50 (usual bedtime), which is stored
    # as a 12-hour clock STRING ("12:59A" .. "00:00P") with sentinels
    # "999996"/"999998"/"999999". It is parsed to minutes-since-midnight and
    # encoded as a sin/cos pair (H1GH50__tsin, H1GH50__tcos) so the midnight
    # wraparound is preserved (bedtimes cluster on both sides of midnight) and
    # the spurious linear ordering of a raw clock is removed. Sentinels/
    # unparseable values become NA and are handled by the missingness
    # machinery. Set transform_time_variables = FALSE to disable.
    transform_time_variables = TRUE,
    extra_exclude_from_candidates = c("IMONTH","IDAY","IYEAR","SCID","SSCID","COMMID","MACNO","INTID","VERSION","CORE1","CORE2","DISABLE","HIEDBLK","CUBAN","PRICAN","CHINESE","SMP01","SMP02","SMP03","SMP04","SMP05","SMP06","SMP07","SMP08","SMP09","SMP10","SMP11","SMP12","AH_RAW","INHOME","TWINONLY","SCHADMW1","INSCHOOL","SCHADMW2","N_ROSTER","N_INSCHL","GRADES","SAT_SCHL","COMMENTS","SCHMONTH","SCHDAY","SCHYEAR","STRATA","SQID","SSCHLCDE","AMONTH","ADAY","AYEAR","H1HR4I", "H1HR4L", "H1HR6L", "H1HR4M", "H1HR4N", "H1HR6N", "H1HR4O", "H1HR6O", "H1HR11O", "H1HR4P", "H1HR5P", "H1HR6P", "H1HR11P", "H1HR4Q", "H1HR5Q", "H1HR6Q", "H1HR11Q","H1HR4R", "H1HR5R", "H1HR6R", "H1HR11R", "H1HR4S", "H1HR5S", "H1HR6S", "H1HR11S", "H1HR4T", "H1HR5T", "H1HR6T", "H1HR11T", "H1RI22R1")
  ),

  # ---------------------------------------------------------------------------
  # Causal-variable governance.
  # ---------------------------------------------------------------------------
  # The source registry is descriptive only. It never acts as an allowlist and
  # never determines whether a Wave-I field may enter screening. Candidate
  # eligibility is controlled only by suffix-safe analysis-role exclusions and
  # the ordinary preprocessing/screening pipeline. H1FS1-H1FS19 remain the
  # protected baseline-depression block; optional additional mandatory fields
  # can be named directly without theory-role or codebook-approval gates.
  causal_governance = list(
    additional_mandatory_W = character(0),
    candidate_alias_audit_csv = "candidate_alias_audit.csv",
    mandatory_W_canonicalization_audit_csv = "mandatory_W_canonicalization_audit.csv",
    variable_source_registry_csv = "variable_source_registry.csv"
  ),

  # ---------------------------------------------------------------------------
  # Data source paths.
  # ---------------------------------------------------------------------------
  paths = list(
    wave1_inhome    = NA_character_,
    birth_records   = NA_character_,
    neighborhood_w1 = NA_character_,
    inschool_w1     = NA_character_,
    contextual_w1   = NA_character_,
    health_w1       = NA_character_,
    spatial_w1      = NA_character_,
    stchr95_w1      = NA_character_,
    polcon_w1       = NA_character_,
    weights_w1      = NA_character_,
    school_admin_w1 = NA_character_,
    wave2_inhome    = NA_character_,
    wave3_inhome    = NA_character_,
    wave4_inhome    = NA_character_,
    wave5_inhome    = NA_character_,
    # Fixed restricted-use NDI 2019 linkage path for the primary analysis.
    mortality       = NA_character_
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
  # - `family` : one of the seven outcome families.
  # - `family_member` : for families with multiple nested binary thresholds
  # (EducationalAttainment, HealthStatus), which
  # threshold to use. NULL for single-outcome families.
  # - `waves` : integer vector of waves to run (subset of 1..4),
  # or the string "all" for all four.
  # - `families` : per-family configuration, including per-wave source
  # variable names and threshold definitions.
  # ---------------------------------------------------------------------------
  outcome = list(
    family        = "Compensation",  # primary toggle: select the outcome family
    family_member = NULL,            # used only for nested-binary families
    waves         = 4L,              # integer, integer vector, or "all"
    # hard stop if the selected outcome constructor is still a
    # placeholder and returns all missing values.
    stop_on_all_missing_outcome = TRUE,

    # Shared transform controls (used by families that produce a continuous Y)
    log_transform              = FALSE,  # legacy compatibility; keep FALSE
    compensation_transform     = "identity", # identity, log1p, or asinh
    compensation_asinh_scale   = 1000,
    compensation_exact_only    = FALSE,
    continuous_upper_quantile  = 0.995,
    # The primary continuous-outcome upper cap is the pooled GSWGT1-weighted
    # observed-outcome quantile. It is not censoring-adjusted in the headline
    # analysis. Every cap calculation uses the same weighted Hyndman-Fan rule.
    continuous_cap_weighted    = TRUE,
    continuous_cap_censoring_adjusted = FALSE,
    continuous_cap_qrule       = "hf8",
    continuous_bound_eps       = 0,
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
        # Constructor: see construct_outcome_educational_attainment below.
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
      # Continuous annual earnings in raw dollars. Wave 4 is the verified primary source.
      Compensation = list(
        type    = "continuous",
        members = NULL,
        sources = list(
          "3" = NULL,                                                 # PLACEHOLDER: not measured at Wave 3?
          "4" = list(exact_var = "H4EC2", bracket_var = "H4EC3"),
          "5" = list(exact_var = "H5EC2", bracket_var = "H5EC3")      # PLACEHOLDER for Wave 5
        ),
        exact_valid_min = 0,
        exact_valid_max = 999995,
        exact_missing_codes = c(9999996, 9999998),
        bracket_valid_codes = as.character(1:12),
        bracket_missing_codes = c(96, 97, 98),
        bracket_map = c(`1` = 2500, `2` = 7500, `3` = 12500, `4` = 17500,
                        `5` = 22500, `6` = 27500, `7` = 35000, `8` = 45000,
                        `9` = 62500, `10` = 87500, `11` = 125000, `12` = 175000),
        earnings_floor_for_log = 0.1,
        natural_lower_bound = 0,
        natural_upper_bound = Inf,
        lower_bound_rule = "natural",
        upper_bound_rule = "weighted_observed_quantile",
        report_ratio_translations = TRUE,
        drop_from_candidates_by_wave = list(
          "3" = character(0),
          "4" = c("Earnings", "EarningsSource", "H4EC2", "H4EC3"),
          "5" = c("Earnings", "H5EC2", "H5EC3")
        ),
        drop_from_candidates = c("Earnings", "EarningsSource", "H4EC2", "H4EC3")   # legacy fallback
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
    numeric_missing_scheme  = "dual_indicators",
    numeric_imputation      = "median",
    factor_missing_label    = "Missing",
    factor_other_label      = "_Other_",

    # Source-informed exact-code missing classifier. The old implementation
    # matched numeric SUFFIXES, so ordinary values such as 196 or 22,198 were
    # falsely treated as 96/98. This classifier never uses suffixes. It learns
    # exact missing-code families once on the complete Wave I merge, using:
    #   * Add Health questionnaire-name structure (H1*, PA*, PB*, S<digit>*,
    #     A<digit>*),
    #   * the existing long-factor/forced-factor declarations,
    #   * observed response support, and
    #   * exact wider sentinel blocks (996-999, 9996-9999, ...).
    # Common skips remain skips even when they exceed 20% of observations.
    # Finite values from contextual/continuous sources remain substantive by
    # default unless a questionnaire-source exact sentinel block is supported.
    missing_classifier = "global_source_informed_exact_v1",
    # Semantic code meanings are frozen once from the complete Wave I merge
    # before Wave II exposure restriction, fold creation, or any screening.
    global_missing_dictionary = NULL,
    # In production every raw Wave I candidate must have a frozen semantic
    # rule. Only explicitly derived variables may use native-missing-only rules.
    global_missing_dictionary_required = TRUE,
    global_missing_dictionary_native_only_patterns = c("__tsin$", "__tcos$"),
    auto_questionnaire_max_unique = 30L,
    auto_questionnaire_max_unique_prop = 0.01,
    auto_categorical_max_abs_value = 100,
    auto_integer_like_min_prop = 0.99,
    auto_integer_tolerance = 1e-8,
    auto_special_code_max_digits = 7L,
    auto_percentage_min_unique = 20L,
    auto_percentage_min_span = 50,
    auto_dense_small_count_min_unique = 8L,
    auto_dense_small_count_max_value = 20L,
    questionnaire_name_patterns = c(
      "^H1[A-Z0-9_]",   # Wave I in-home questionnaire items
      "^PA[0-9]",       # parent questionnaire section A
      "^PB[0-9]",       # parent questionnaire section B
      "^S[0-9]",        # in-school questionnaire items; excludes SGB/STC
      "^A[0-9]"         # school-administrator questionnaire items
    ),
    questionnaire_name_exclude_patterns = c(
      "^AID$", "^ASCHLCDE$", "^COMMID(?:\\.|$)"
    ),
    explicit_questionnaire_vars = character(0),
    # Codebook-documented variables where a substantive response can share an
    # exact numeric value with a reserve code. These are DIAGNOSTIC FLAGS only:
    # the data cannot distinguish the two meanings, so the automated rule still
    # follows the conventional reserve-code interpretation but reports the
    # ambiguity for transparent limitation/audit review.
    known_codebook_overlap_vars = c(
      "H1GH45", "H1TO7", "H1TO16", "H1EE5", "H1EE7", "H1EE8",
      "H1IR8A", "H1IR8B", "H1IR8C", "H1IR8D", "H1IR10", "H1IR13"
    ),
    # Ambiguous documented overlaps are preserved as substantive values rather
    # than silently recoded. They remain prominently flagged in the dictionary
    # audit for targeted review.
    known_overlap_policy = "preserve_as_substantive",
    # long_factors controls factorization. Most entries are questionnaire
    # categories, but COMMID is an identifier and must not inherit response-code
    # missingness merely because it is factorized.
    nonquestionnaire_long_factors = c("COMMID.x", "COMMID.y"),

    # Exact conventional one- and two-digit families. Wider families
    # (996-999, 9996-9999, ..., through auto_special_code_max_digits)
    # are generated automatically and are never matched by suffix.
    factor_skip_code_low    = 7L,
    factor_refusal_codes_low  = c(6L, 8L, 9L),
    factor_skip_code_high   = 97L,
    factor_refusal_codes_high = c(96L, 98L, 99L),

    # Special-code retention for factors. If a learned exact skip code
    # (7, 97, 997, 9997, ...) appears often enough, it is preserved
    # as its own factor level (called factor_skip_label) instead of being
    # collapsed into the generic "Missing" bucket. This keeps the
    # information that "this respondent was legitimately skipped" available
    # to the model without exploding the design matrix with rare-code
    # dummies.
    # `factor_special_code_min_n` : skip code must appear in at least
    # this many rows to be retained.
    # `factor_special_code_min_prop` : OR at least this fraction of rows
    # (whichever criterion fires first).
    factor_skip_label                = "Skip",
    factor_special_code_min_n        = 30L,
    factor_special_code_min_prop     = 0.02,

    # Resolve .x/.y name collisions only after auditing the paired values.
    # Agreeing pairs are coalesced into .x; conflicting or unverifiable pairs
    # are retained separately so no information is silently discarded.
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
    # Seven permits a four-category H1FS item plus Missing, Skip, and
    # _Other_ without collapsing a substantive baseline-depression category.
    factor_max_levels_after_collapse  = 7L,
    numeric_min_observed_n            = 40L,
    numeric_min_observed_prop         = 0.10,
    winsor_probs                      = c(0.01, 0.99),
    winsorize_binary_numeric          = FALSE
  ),

  # ---------------------------------------------------------------------------
  # Final CV-TMLE.
  # ---------------------------------------------------------------------------
  final_tmle = list(
    vfolds                       = 5L,
    internal_superlearner_folds  = 3L,
    # Internal nuisance-model CV is always whole-PSU cluster aware. This field
    # is retained only for cache compatibility and validation requires TRUE.
    cluster_aware_internal_cv    = TRUE,
    outer_fold_balance_on_weights = FALSE,
    internal_fold_balance_on_weights = FALSE,
    # Whole-PSU fold assignment is size-first. Treatment/outcome-observation
    # support is a hard acceptance condition, not an objective that may trade
    # away fold size. These limits reject pathological allocations such as
    # three near-empty validation folds.
    fold_max_attempts             = 500L,
    # At each greedy assignment, candidate folds must be within this fraction
    # of the best projected total size before treatment/censoring balance is
    # considered. This makes size balance lexicographically primary.
    fold_projected_size_tolerance_prop = 0.02,
    fold_max_size_ratio           = 1.60,
    fold_max_size_deviation_prop  = 0.35,
    fold_internal_max_size_ratio  = 1.75,
    fold_internal_max_size_deviation_prop = 0.45,
    fold_min_active_cell_n        = 1L,

    # Reviewer-responsive nested screening inside each final TMLE outer fold.
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
    # 1,500-row sample). A richer pool reaches the multivariable elastic-net union screen; the
    # FINAL model size is governed by the processed-column cap, so these do not
    # enlarge the expensive nuisance fits. Near-free on runtime (rough screen
    # is cheap relative to SuperLearner).
    rough_top_n_outcome           = 120L,
    rough_top_n_missingness       = 40L,
    rough_top_n_joint_AY          = 60L,
    rough_top_n_exposure_only     = 0L,

    # Exposure-predictive candidates are allowed into the nested multivariable
    # elastic-net step for union screening, but are not directly forced into
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
    # enough degrees of freedom to keep more correlated variables. The later
    # rough-variable and processed-column budgets provide the principal size
    # controls; this threshold is only a conservative redundancy filter.
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
    # Sparse treatment-by-level support is audited but is not used to delete
    # an entire candidate variable in the primary analysis. Dropping a genuine
    # confounder merely because one level is rare in one treatment arm can hide
    # a positivity problem rather than solve it. Set above zero only in a
    # clearly labelled sensitivity analysis.
    rough_prefilter_two_arm_min_n    = 0L,
    rough_prefilter_two_arm_max_levels = 3L,
    rough_screen_batch_size       = 500L,

    # Optional nested multivariable elastic-net screen after the rough screen.
    # Within the retained marginal candidate pool, it can identify conditional
    # associations not apparent from each variable's marginal rank. It is fit
    # only on the outer training rows: fit A~W, Y~W, and delta_Y~W and take the
    # union. Exposure-ranked candidates enter final W only through an allowed
    # multivariable or outcome/missingness pathway.
    nested_lasso_after_rough      = TRUE,
    # pre-specified W override. NULL = use the data-driven nested
    # screen (default). Set to a character vector of variable names to bypass
    # selection entirely and use a fixed, theoretically-motivated confounder
    # set (used by the pre-specified-W sensitivity scenario).
    prespecified_W                = NULL,
    # (protected_W): a character vector of variable names to (a) union into
    # the selected W and (b) exempt from the processed-column cap, WITHOUT
    # disabling the data-driven screen (unlike prespecified_W, which replaces it).
    # NULL = no-op (default; production runs unchanged). Set to the baseline
    # mental-health block names for the forced-MH-block sensitivity.
    protected_W                   = paste0("H1FS", 1:19),
    # All variables returned by get_mandatory_W() bypass screening and the
    # optional processed-column budget. protected_W remains the subset whose
    # substantive factor levels must be preserved exactly.
    mandatory_W_bypass_screening      = TRUE,
    protected_W_bypass_screening      = TRUE,
    # Preserve every observed substantive level of protected H1FS factors.
    # These variables bypass rare-level, sparse-exposure, and maximum-level collapsing.
    protected_W_preserve_substantive_levels = TRUE,
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
    # to drop weak-but-real confounders -- the opposite of what union screening
    # is meant to protect. lambda.min is the more inclusive, confounding-safe
    # choice and is used as primary.
    lasso_screen_lambda_choice    = "lambda.min",
    lasso_screen_folds            = 5L,
    lasso_screen_include_delta    = TRUE,
    lasso_screen_min_vars         = 30L,
    lasso_screen_max_vars         = 130L,
    lasso_screen_max_processed_cols = 240L,

    # Raw-variable cap after the rough pool is augmented and prioritized by
    # the A, Y, and delta elastic-net selections. Elastic-net nonselection does
    # not automatically remove a rough-pool variable.
    rough_max_total_vars          = 150L,
    # Nonprotected processed-column engineering budget. It is not justified by
    # an events-per-column formula; stability is assessed through held-out nuisance
    # performance, convergence, positivity, ESS diagnostics, and budget sensitivities.
    # Protected H1FS columns
    # are reported separately and may sit on top of this allowance. This is not
    # described as an events-per-parameter validity rule.
    final_max_processed_columns  = 260L,
    # Legacy raw-count events-per-column logic is deliberately disabled.
    use_epp_cap                  = FALSE,
    rough_min_total_vars          = 10L,
    nested_rough_selection_log_csv = "nested_rough_selection_log.csv",
    cluster_assignment_log_csv = "cluster_assignments.csv",

    min_treated_warning_n        = 30L,
    min_observed_treated_warning_n = 20L,
    internal_fold_min_treated_warning_n = 8L,
    internal_fold_min_observed_treated_warning_n = 6L,
    min_ess_treated_train_warning = 20,
    min_ess_treated_valid_warning = 10,
    fail_on_nuisance_fallback = TRUE,
    g_lower                      = 0.025,
    g_upper                      = 0.975,
    pi_lower                     = 0.025,
    pi_upper                     = 0.999,
    positivity_warning_threshold = 0.05,
    positivity_warning_fraction  = 0.10,

    # positivity remediation. The 13,500-row run showed ~50% of rows
    # with g*pi1 < 0.05 (treated-arm non-overlap). Two diagnostics are added:
    # (1) report the ATT alongside the ATE. The ATT only needs overlap
    # where treated units exist and is far better identified at 9%
    # prevalence.
    # (2) optionally trim to a common-support propensity band and report
    # the trimmed-sample ATE. Rows with g outside [lo, hi] are dropped
    # from the targeted means and the EIF.
    # DEFAULT HEADLINE: the ATT via joint-component CV-TMLE
    # (primary_estimand = "att"). The full-sample ATE, trimmed ATE, plug-in,
    # AIPW, and one-step ATT remain explicitly labeled secondary estimates.
    # Changing primary_estimand changes the estimand reported in the headline.
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
    # policy-relevant population is the treated (the depressed). The ATT is
    # the estimand matching that population and does not require treatment-side
    # support for individuals who would never be treated, although adequate
    # comparable-control support for treated individuals remains an empirical
    # requirement checked by the ATT diagnostics. Its SE is the REGION-stratified,
    # PSUSCID-clustered survey-design EIF SE computed in the ATT block, with
    # centering and pi-set-to-one wiring checks. The full ATE, trimmed ATE, ATT,
    # plug-in, and AIPW all remain reported as columns; the trimmed ATE and the
    # band-sensitivity scenarios become supporting robustness analyses.
    primary_estimand             = "att",
    # The headline ATT estimator is fixed to joint-component CV-TMLE.
    # The one-step AIPTW estimate is still computed and reported as a labeled
    # diagnostic comparator, but it cannot be selected as the headline because
    # the policy-facing component percentages and targeting diagnostics are
    # defined from the joint CV-TMLE component means.
    att_estimator                = "tmle",
    # when TRUE, the trimmed ATE re-fits the TMLE fluctuation on the
    # in-support rows so it is a formally targeted estimate of the trimmed
    # estimand, rather than reusing the full-sample epsilon. Recommended TRUE
    # whenever the trimmed ATE is used as anything more than a rough check.
    retarget_trimmed             = TRUE,

    # Strict numerical checks for raw-dollar ATT targeting and percentage effects.
    target_score_tol             = 1e-10,
    att_eif_center_tol_scaled    = 1e-8,
    target_root_max_expand       = 60L,
    percentage_primary           = "prevention_gain",

    # Bounds applied to held-out Q predictions before logit targeting.
    Q_pred_lower                 = 0.005,
    Q_pred_upper                 = 0.995,
    Q_clip_warning_fraction      = 0.01,
    Q_clip_review_fraction       = 0.05,

    use_fold_checkpoints         = FALSE,
    gc_after_fold                = TRUE,
    # Genuine total matrix safety guard, including protected columns.
    hard_max_processed_columns   = 450L,
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
              use_ranger = TRUE, use_xgboost = TRUE,
              use_xgboost_rich = FALSE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    g  = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_glmnet_h1fs = FALSE,
              use_ranger = TRUE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    pi = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_glmnet_A_unpenalized = FALSE,
              use_ranger = FALSE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    # alpha = 0.5 (elastic net) actually takes effect via SL.glmnet.fixed.
    # Stock SL.glmnet ignores alpha; the wrapper passes it through.
    # lambda_choice: "min" (CV-minimum, more permissive) or "1se" (1-SE rule).
    glmnet  = list(alpha = 0.5, nlambda = 100L, lambda_choice = "min",
                   maxit = 100000L, standardize = TRUE, internal_folds = 5L),
    glmnet_h1fs = list(h1fs_penalty_multiplier = 0.25),
    glmnet_pi_A = list(A_penalty_multiplier = 0.0),
    ranger  = list(num.trees = 500L),
    xgboost = list(ntrees = 150L, max_depth = 2L, shrinkage = 0.05,
                   min_child_weight = 20),
    # Optional Q-only sensitivity learner. It is registered under a distinct
    # SuperLearner name so enabling it does not alter g or pi.
    xgboost_rich = list(ntrees = 250L, max_depth = 3L, shrinkage = 0.04,
                        min_child_weight = 10),
    # earth (MARS) MUST stay off at this scale. On a 13,500-row x
    # ~200-column dummy-heavy design its forward-pass knot search ran 35-88
    # HOURS per fold and dominated total runtime (~330h for one scenario).
    # Do not set learners$Q$use_earth = TRUE on the full sample.
    earth   = list(degree = NULL, nprune = NULL),
    gam     = list(k = 4L, smooth_unique_min = 10L, eps = 1e-6, maxit = 100L),
    svm     = list(cost = 1, gamma = NULL, kernel = "radial"),
    nnet    = list(size = 5, decay = 0.0001, maxit = 200, MaxNWts = 10000)
  ),

  # ---------------------------------------------------------------------------
  # Optional policy translation of the ATT.
  # ---------------------------------------------------------------------------
  # enable_policy_components controls the additional natural-course mean and
  # prevalence targeting. enable_att_prevalence_translation controls the final
  # elasticity/per-prevented-case/prevalence-reduction table. When both are
  # FALSE, the primary ATT is still estimated normally.
  policy = list(
    enable_policy_components = TRUE,
    enable_att_prevalence_translation = TRUE,
    fail_on_policy_component_checks = TRUE,
    relative_prevalence_reductions = c(0.10, 0.25, 0.50),
    output_csv = "att_policy_translation.csv",
    outcome_scale_label = "configured bounded/capped outcome"
  ),

  # Optional mortality linkage/composite sensitivity. enabled controls linkage;
  # composite_zero_at_death separately controls whether the outcome is recoded.
  # NDIDD19Y is read from the configured mortality file. The primary mortality
  # definition is independent of Wave-IV participation: a recognized death year
  # in the inclusive 1997-2007 window sets Death1997_2007 = 1. IYEAR4 is read
  # from Wave IV only for an audit of interview-year availability/distribution;
  # it NEVER changes mortality classification and is not required for deceased
  # respondents. H4EC2 is a rolling past-year earnings measure, so the pipeline
  # deliberately does NOT manufacture a calendar earnings-reference year from
  # IYEAR4. NDIDD19Y, IYEAR4, and all mortality-derived fields are excluded from W.
  # Native NDIDD19Y missing values are treated as no recorded death by default.
  # Any other non-year code must be explicitly listed in no_death_codes or the
  # run stops when fail_on_unrecognized_codes is TRUE.
  mortality_sensitivity = list(
    enabled = TRUE,
    source_var = "NDIDD19Y",
    # Audit-only Wave-IV interview year. Missing IYEAR4 is expected for people
    # who were not interviewed at Wave IV, including deaths; it is not a failure.
    interview_year_var = "IYEAR4",
    interview_year_valid_min = 2007L,
    interview_year_valid_max = 2009L,
    earnings_price_basis = "nominal_past_year_dollars_no_inflation_adjustment",
    derived_death_year_var = "NDI19DeathYear",
    death_in_window_var = "Death1997_2007_RawWindow",
    death_year_start = 1997L,
    death_year_end = 2007L,
    valid_year_min = 1900L,
    valid_year_max = 2100L,
    native_missing_means_no_death = TRUE,
    no_death_codes = 99997,
    fail_on_unrecognized_codes = TRUE,
    require_complete_linkage = TRUE,
    # A respondent linked as dead during 1997-2007 should not also have a
    # finite Wave-IV earnings value. Stop before overwriting such a value unless
    # the analyst explicitly disables this contradiction gate after review.
    fail_on_death_with_observed_original_outcome = TRUE,
    death_before_outcome_var = "Death1997_2007",
    composite_zero_at_death = TRUE,
    linkage_audit_csv = "mortality_linkage_1997_2007_audit.csv",
    interview_year_audit_csv = "wave4_interview_year_audit.csv",
    contradiction_audit_csv = "mortality_death_with_observed_outcome_records.csv",
    output_csv = "mortality_composite_zero_at_death_audit.csv"
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
    balance_factor_levels_treatment_csv = "balance_factor_levels_treatment.csv",
    balance_missingness_csv = "balance_missingness_loveplot_data.csv",
    balance_factor_levels_missingness_csv = "balance_factor_levels_missingness.csv",
    # trimmed-sample treatment balance (the trimmed estimand's balance).
    balance_on_trimmed = TRUE,
    balance_treatment_trimmed_csv = "balance_treatment_trimmed_loveplot_data.csv",
    balance_factor_levels_trimmed_csv = "balance_factor_levels_treatment_trimmed.csv",
    # ATT-weighted balance (the headline estimand's balance) and the
    # ATT control-weight / treated-side positivity diagnostic.
    balance_att = TRUE,
    balance_treatment_att_csv = "balance_treatment_att_loveplot_data.csv",
    balance_treatment_att_all_candidates_csv = "balance_treatment_att_all_candidates.csv",
    balance_factor_levels_att_csv = "balance_factor_levels_treatment_att.csv",
    att_positivity_csv = "att_positivity_control_weights.csv",
    evalue_csv = "evalue_sensitivity.csv",
    evalue_contour_csv = "evalue_contour.csv",
    candidate_prefilter_log_csv = "candidate_prefilter_log.csv",
    fold_support_log_csv = "fold_support_log.csv",
    internal_fold_support_log_csv = "internal_fold_support_log.csv",
    selection_jaccard_csv = "selection_jaccard.csv",
    fold_selection_stability_csv = "fold_selection_stability.csv",
    learner_risk_summary_csv = "learner_cv_risk_summary.csv",
    nuisance_outer_validation_csv = "nuisance_outer_validation_summary.csv",
    learner_risk_long_csv = "learner_cv_risk_long.csv",
    q_prediction_clipping_csv = "q_prediction_clipping.csv",
    enable_wave2_completion_diagnostic = TRUE,
    wave2_completion_balance_csv = "wave2_completion_balance.csv",
    wave2_completion_summary_csv = "wave2_completion_summary.csv",
    enable_mnar_pattern_mixture = TRUE,
    mnar_pattern_mixture_csv = "att_mnar_pattern_mixture.csv",
    mnar_shift_sd_grid = c(-0.50, -0.25, 0, 0.25, 0.50),
    enable_evalue = TRUE,
    exclude_too_missing_from_love_plots = TRUE,
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
    pi_treatment_contrast_csv = "pi_treatment_contrast.csv",
    pi_treatment_invariance_tolerance = 1e-10,
    att_g_pi_clip_sensitivity_csv = "att_g_pi_clip_sensitivity.csv",
    att_g_pi_clip_sensitivity_floors = c(0.01, 0.025, 0.05),
    att_g_pi_clip_include_configured = TRUE,
    att_outcome_bound_sensitivity_csv = "att_tail_perturbation_diagnostic.csv",
    att_outcome_bound_quantiles    = c(0.99, 0.995, 1.00),
    earnings_construction_audit_csv = "earnings_construction_audit.csv",
    join_suffix_collision_audit_csv = "join_suffix_collision_audit.csv",
    earnings_tail_audit_csv         = "earnings_tail_cap_audit.csv",
    missing_code_audit_csv          = "targeted_missing_code_audit.csv",
    global_missing_dictionary_csv    = "global_missing_code_dictionary.csv",
    design_strata_source_audit_csv   = "design_strata_source_audit.csv",
    canonical_design_field_audit_csv = "canonical_design_field_audit.csv",
    core_balance_csv                 = "protected_h1fs_att_balance.csv",
    att_leaveout_top_k             = c(1L, 3L, 5L),
    run_manifest_csv               = "run_manifest.csv",
    wave2_completion_expanded_balance_csv = "wave2_completion_expanded_balance.csv",
    wave2_completion_all_candidates_csv = "wave2_completion_all_candidates.csv",
    wave2_completion_model_csv = "wave2_completion_model.csv",
    diagnostic_status_csv = "diagnostic_status.csv",

    # -------------------------------------------------------------------------
    # MNAR sensitivity extensions (v8.28).
    # All three are FIXED-NUISANCE post-hoc diagnostics computed from the
    # already-targeted ATT components.  They do NOT refit any nuisance, do NOT
    # change the estimand, and do NOT alter the primary ATT.  They re-express
    # the existing pattern-mixture grid as (a) a breakdown point, (b) fixed-
    # nuisance extreme-mean bounds for the configured bounded outcome, and
    # (c) a calibrated sensitivity model in
    # which the required unmeasured shift is expressed as a multiple of an
    # observable missingness-related outcome gradient.
    # -------------------------------------------------------------------------
    enable_mnar_breakdown            = TRUE,
    mnar_breakdown_csv               = "att_mnar_breakdown_point.csv",
    mnar_breakdown_max_sd            = 3,
    mnar_breakdown_grid_n            = 601L,
    enable_manski_bounds             = TRUE,
    manski_bounds_csv                = "att_fixed_nuisance_extreme_mean_bounds.csv",
    enable_mnar_calibrated           = TRUE,
    mnar_calibrated_csv              = "att_mnar_calibrated_sensitivity.csv",
    # Response-propensity strata used to define MEASURED missingness bias.
    # Pre-declare these; the calibrated Gamma is only interpretable relative
    # to this choice (McClean, Branson & Kennedy 2024).
    mnar_calibration_probs           = c(0.20, 0.80),
    mnar_calibration_boot_reps       = 1000L,
    mnar_calibration_boot_seed       = 20260801L,
    mnar_calibration_bootstrap_design = "region_stratified_psu",
    mnar_calibration_weighted_quantiles = TRUE,
    mnar_calibration_min_valid_boot_reps = 500L,
    diagnostic_bundle_rds = "diagnostic_fit_bundle.rds",
    analysis_sample_audit_csv = "analysis_sample_audit.csv",
    manuscript_summary_csv = "manuscript_summary.csv",
    output_inventory_csv = "output_inventory.csv",
    publication_ready_marker = "PUBLICATION_READY.txt",
    balance_scan_timing_csv = "balance_scan_timings.csv",
    balance_progress_every = 500L,
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
    # Only the verified Wave IV Compensation constructor and an explicitly
    # configured PassThrough negative-control outcome are allowed by default.
    # Set TRUE only after a new outcome family/wave has been codebook-verified.
    allow_unverified_outcome_specs = FALSE,
    fail_on_role_alias_leakage = TRUE,
    fail_on_missing_mandatory_W = TRUE,
    fail_on_q_clip_review_threshold = FALSE,
    stop_on_stale_wave1_cache = FALSE,
    require_fresh_primary_output_dir = TRUE,
    fresh_output_allowed_basenames = c("run.log"),
    allow_output_overwrite = FALSE,
    verify_atomic_writes = TRUE,
    windows_max_path = 259L,
    require_publication_ready_marker = TRUE
  )
)

# =============================================================================
