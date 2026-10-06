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
    # Repeated splitting reruns the full pipeline for each configured seed.
    # Leave disabled for a single designated fit; require all seeds for aggregation.

    run_multiseed_att              = FALSE,
    run_configured_sensitivities   = FALSE
  ),
  
  # ---------------------------------------------------------------------------
  # Global controls: seed, verbosity, I/O.
  # ---------------------------------------------------------------------------
  global = list(
    # Seed for the designated single-run partition. The optional repeated-split
    # runner also reports an aggregated ATT and retains this seed as a comparator.
    pipeline_seed     = 1L,
    # Windows MAX_PATH is 260 chars. The run tag appears TWICE in every output
    # path (subdirectory + filename), so keep this root and run_label SHORT.
    # Override explicitly before run_addhealth_pipeline() if getwd() is deep.
    output_dir        = file.path(getwd(), "Final_Code_Runs_Hours_9_18", format(Sys.time(), "%Y%m%d_%H%M%S")),
    # Auto-resolved when knitting/sourcing/RStudio succeeds. Set this explicitly
    # to the full .R or .Rmd path if your execution environment cannot resolve it.
    pipeline_source_path = .PIPELINE_SOURCE_PATH,
    require_script_md5 = TRUE,
    verbose           = TRUE,
    save_stage_csvs   = TRUE, # required result/provenance audits for production
    autorun_pipeline  = FALSE,
    # version string participates in the multiseed checkpoint fingerprint.
    # BUMP THIS whenever you change analysis-relevant code, so a rerun in the
    # same output dir does not reuse stale per-seed results (or pass fresh=TRUE).
    version           = "v8.44_control_arm_Q_calselect_scope",
    # These partitions are used ONLY if run_multiseed_att is explicitly enabled.
    # That optional runner reports a split-aggregated ATT; the designated seed
    # remains a labeled comparator. Ordinary single-run behavior is unchanged.
    multiseed_seeds   = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
    multiseed_parallel_cores = 1L,   # >1 = run seeds in parallel, each in its own isolated subdir; 1 = sequential (sequential)
    multiseed_require_all_seeds = TRUE,   # TRUE (recommended for the final run) stops if any fixed seed fails, rather than aggregating only the successes
    # Run only these named comparisons when the corresponding stage is TRUE.
    # Available names: names(configuration_action_scenarios()).
    sensitivity_actions = c("D1_summary"), # the primary items run is the reference
    checkpoint_subdir = "ck_v844",
    # Short label included in output names and run directories.
    # The analysis fingerprint excludes this runtime naming label.


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
    resume_mode = FALSE,
    # Compact output naming is ON by default. Provenance remains in CSV headers,
    # manifests, hashes, and RDS configuration objects; filenames no longer need
    # to carry long descriptive stems.
    short_file_names = TRUE,
    max_output_stem_chars = 16L,
    family_tag_map = c(
      Compensation = "earn",
      LaborForceParticipation = "lfp",
      HoursWorked = "hrs",
      UsualHours = "hrs_legacy",
      EducationalAttainment = "edu",
      HealthStatus = "health",
      MentalHealth = "mh",
      SubstanceUse = "sub",
      PassThrough = "pt"
    )
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
    wave1_rds               = "w1.rds",
    main_dataset_rds        = "main.rds",
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
    outcome_var          = "Y",            # generic name, populated by outcome family constructor
    exposure_type        = "auto",
    outcome_type         = "auto",
    id_var               = "AID",
    cluster_var          = "PSUSCID",
    strata_var           = "REGION",
    weight_var           = "GSWGT1",
    outcome_observed_var = "delta_Y",
    # Configuration switches for analysis scope, baseline adjustment,
    # probability calibration, and descriptive sensitivity checks.


    # [1] Wave-I subsample. FALSE = full analytic sample. Integrity counts are
    # checked upstream before restricting the final survey analysis domain.
    restrict_wave1_depressed = FALSE,
    wave1_depression_cutpoint = 22,
    
    # Baseline-depression protection: items forces H1FS1-19 into the nuisances;
    # summary protects MHSum_W1 while leaving items screen-eligible; both protects
    # both representations. Protected columns are additional to the optional
    # processed-column budget. Evaluate alternatives with held-out loss and balance.


    protected_W_mode = "summary",
    
    # [3] Optional probability calibration of the propensity score.
    #     Prespecified comparison: calibrated and uncalibrated g are both
    #     re-targeted under the primary bounds in the existing clip table.
    #     Slope >1 means compressed predictions; calibration de-compresses them,
    #     which can widen weight tails -- recheck att_positivity_control_weights.
    calibrate_g = TRUE,
    # select chooses among g_calibration_candidates within each outer training
    # fold using survey-weighted inner whole-PSU cross-validated log loss.
    # The default candidates are identity and Platt; isotonic is an explicit option.
    # Selection uses no outcome, outer-validation label, or effect estimate.
    g_calibration_method = "select", # "select", "platt" or "isotonic"
    # Maps compared when the method is "select" (first-listed wins exact ties).
    g_calibration_candidates = c("identity", "platt"),
    pi_calibration_candidates = c("identity", "platt"),
    calibrate_pi = FALSE,
    pi_calibration_method = "platt",
    # Calibration is fitted within EACH outer training fold using inner
    # out-of-fold base predictions and survey weights; never validation labels.
    
    # Descriptive g clipping comparison: retarget under each configured bound.
    # Do not select bounds using the resulting ATT or SE. Differences do not
    # identify causal bias or MSE. The adaptive_g_truncation_apply switch is rejected.


    adaptive_g_truncation = FALSE,
    adaptive_g_truncation_apply = FALSE,
    adaptive_g_truncation_grid = c(0.005, 0.01, 0.025, 0.05),
    
    # [5] Wave-IV military/prison contribution zeros use documented interview
    #     or final field status. Set auto_military_prison_status=FALSE to use a
    #     separately adjudicated CSV instead. Ordinary nonresponse is not zero.
    #     Deaths use the dated mortality composite only; annual earnings are excluded.
    dsp4_mode = "auto_verified",
    dsp4_structural_zero_families = c("HoursWorked", "LaborForceParticipation"),
    dsp4_structural_zero_codes = c(477L, 478L, 613L, 486L, 482L),
    # Data-integrity gate on composite status masking (see apply_verified_labor_status).
    max_status_masked_fraction = 0.10,
    status_mask_gate_min_nonzero = 500L,
    enable_dsp4_audit = TRUE,
    
    # [6] Diagnostic variable set for the baseline-depression balance table.
    #     Decoupled from protected_W so the diagnostic survives [2] unchanged.
    h1fs_balance_vars = c(paste0("H1FS", 1:19), "MHSum_W1"),
    
    # Production integrity gates for the verified Wave-II CES-D / Wave-IV
    # Compensation analysis. Preflight disables these because it uses synthetic
    # data. A mismatch stops the run before any causal modeling.
    # Expected-n gates apply to the unrestricted valid-weight completer cohort
    # before optional analysis-domain restrictions, not to the subgroup size.
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
    extra_exclude_from_candidates = c(".labor_status_zero", ".labor_status_verified", ".labor_status_reason", "IMONTH","IDAY","IYEAR","SCID","SSCID","COMMID","MACNO","INTID","VERSION","CORE1","CORE2","DISABLE","HIEDBLK","CUBAN","PRICAN","CHINESE","SMP01","SMP02","SMP03","SMP04","SMP05","SMP06","SMP07","SMP08","SMP09","SMP10","SMP11","SMP12","AH_RAW","INHOME","TWINONLY","SCHADMW1","INSCHOOL","SCHADMW2","N_ROSTER","N_INSCHL","GRADES","SAT_SCHL","COMMENTS","SCHMONTH","SCHDAY","SCHYEAR","STRATA","SQID","SSCHLCDE","AMONTH","ADAY","AYEAR","H1HR4I", "H1HR4L", "H1HR6L", "H1HR4M", "H1HR4N", "H1HR6N", "H1HR4O", "H1HR6O", "H1HR11O", "H1HR4P", "H1HR5P", "H1HR6P", "H1HR11P", "H1HR4Q", "H1HR5Q", "H1HR6Q", "H1HR11Q","H1HR4R", "H1HR5R", "H1HR6R", "H1HR11R", "H1HR4S", "H1HR5S", "H1HR6S", "H1HR11S", "H1HR4T", "H1HR5T", "H1HR6T", "H1HR11T", "H1RI22R1")
  ),
  
  # ---------------------------------------------------------------------------
  # Causal-variable governance.
  # ---------------------------------------------------------------------------
  # The registry is not an allowlist: unregistered otherwise eligible W remain
  # candidates. It additionally enforces the explicit SOURCE DENYLIST below,
  # including registered descendants and missingness representations. The
  # existing protected_W_mode determines the baseline-depression protection.
  causal_governance = list(
    excluded_candidate_sources = "birth_records",
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
    wave4_disposition = NA_character_,
    verified_labor_status = NULL, # adjudicated CSV; required for status recoding
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
    # Exposure scope preserves the Wave-II indicator and restricts the cohort
    # after full-cohort integrity checks. wave2_only retains the primary cohort.
    # wave3_complete requires the Wave-III nine-item scale. durable_control retains
    # Wave-II positives and controls negative at both waves; incomplete scales and
    # Wave-III-only positives are excluded. durable_control conditions controls on
    # a post-exposure measure and requires disabling prevalence-based translation.
    # Confirm Wave-III fields and reserve codes against the restricted codebook.


    exposure_mode = "wave2_only",
    wave3_cesd_items = paste0("H3SP", 5:13),
    wave3_cesd_reverse_items = c("H3SP7", "H3SP11"),
    wave3_cesd_missing_codes = c(6, 8, 9),
    wave3_cesd_cutpoint = 12,
    drop_from_candidates  = c("MHSum", paste0("H2FS", 1:19), paste0("H3SP", 5:13))
  ),
  
  # Outcome configuration: family selects the constructor; family_member
  # selects a nested education/health threshold; waves selects supported waves.
  # Education and health support III/IV; earnings, participation, and hours
  # support IV. all expands only to waves supported by the chosen family.
  # The families mapping supplies source fields and threshold rules.


  outcome = list(
    family        = "HoursWorked",  # primary toggle: select the outcome family
    family_member = NULL,            # used only for nested-binary families
    waves         = 4L,              # integer, integer vector, or "all"
    # hard stop if the selected outcome constructor is still a
    # placeholder and returns all missing values.
    stop_on_all_missing_outcome = TRUE,
    
    # Shared transform controls (used by families that produce a continuous Y)
    log_transform              = FALSE,  # compatibility setting; keep FALSE
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
      # Three nested binary outcomes. All three are designated primary because
      # the paper emphasizes effect magnitude across the attainment distribution;
      # no multiplicity correction is imposed by this program.
      EducationalAttainment = list(
        type    = "binary_nested",
        members = list(
          at_least_hs           = list(label = "At least HS graduation", primary = TRUE),
          at_least_some_college = list(label = "At least some college", primary = TRUE),
          at_least_college_grad = list(label = "At least college graduation", primary = TRUE)
        ),
        # Wave III uses member-specific evidence. The constructor enforces
        # nesting: verified higher attainment establishes every lower threshold.
        # Wave IV uses the single H4ED2 attainment code.
        sources = list(
          "3" = c(highest_grade = "H3ED1",
                  high_school_equivalency = "H3ED2",
                  high_school_diploma = "H3ED3",
                  college_graduate = "H3ED5"),
          "4" = "H4ED2"
        ),
        # per-wave drop list. Each wave's outcome source is excluded
        # from W only when that wave is the outcome wave; other waves'
        # source vars remain available as confounders.
        drop_from_candidates_by_wave = list(
          "3" = c("H3ED1", "H3ED2", "H3ED3", "H3ED5"),
          "4" = "H4ED2"
        ),
        report_ratio_translations = TRUE,
        drop_from_candidates = character(0)
      ),
      
      # Wave-IV participation uses H4LM6/H4LM11/H4LM14.
      # The Wave-III mapping is hard-blocked by validation and construction because
      # it is not harmonized to the supported participation definition.


      LaborForceParticipation = list(
        type    = "binary_single",
        members = NULL,
        report_ratio_translations = TRUE,
        definition_by_wave = c(
          `3` = "employment_10plus_proxy",
          `4` = "employment_10plus_or_temporarily_absent_or_unemployed_looking_proxy"
        ),
        sources = list(
          "3" = NULL,
          "4" = c(first_job_current = "H4LM6",
                  current_work = "H4LM11",
                  current_status = "H4LM14")
        ),
        wave3_codes = list(
          work_yes = 1L,
          work_no = 0L,
          missing = 7L
        ),
        wave4_codes = list(
          first_job_yes = 1L,
          current_work_yes = 1L,
          current_work_no = 0L,
          in_labor_force_status = c(1L, 2L, 3L, 5L),
          out_labor_force_status = c(4L, 6L, 7L, 8L, 9L),
          unresolved_status = 10L,
          lm6_missing = c(6L, 7L),
          lm11_missing = c(5L, 6L, 7L),
          lm14_missing = c(95L, 96L, 97L)
        ),
        drop_from_candidates_by_wave = list(
          "3" = character(0),
          "4" = c("H4LM6", "H4LM11", "H4LM14"),
          "5" = character(0)
        ),
        drop_from_candidates = character(0)
      ),
      
      # Wave-IV unconditional current weekly hours: established nonworkers
      # receive zero; multiple-job totals use H4LM13 and one-job hours use H4LM19.
      # The outcome is capped at 120 hours. Wave-III hours are not supported.


      HoursWorked = list(
        type    = "continuous",
        members = NULL,
        report_ratio_translations = TRUE,
        cap_hours = 120,
        natural_lower_bound = 0,
        natural_upper_bound = 120,
        lower_bound_rule = "natural",
        upper_bound_rule = "fixed",
        sources = list(
          "3" = NULL,
          "4" = c(first_job_current = "H4LM6", current_work = "H4LM11",
                  current_jobs = "H4LM12", total_hours = "H4LM13",
                  primary_job_hours = "H4LM19")
        ),
        wave3_codes = list(
          work_yes = 1L, work_no = 0L,
          work_missing = 7L,
          # H3LM16 is observed on a 0-90 numeric range, but H3LM7=1 defines
          # current paid work at >=10 h/week. Values 0-9 are therefore
          # recognized for audit purposes but not accepted as logically
          # consistent worker-hours outcomes.
          hours_observed_min = 0, hours_valid_min = 10, hours_valid_max = 90,
          hours_missing = c(96L, 97L, 98L, 99L)
        ),
        wave4_codes = list(
          first_job_yes = 1L,
          current_work_yes = 1L, current_work_no = 0L,
          current_jobs_valid = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 11L, 12L, 15L),
          current_jobs_total_hours_route = c(2L, 3L, 4L, 5L, 6L, 7L, 11L, 12L, 15L, 98L),
          current_jobs_missing = c(95L, 97L, 98L),
          total_hours_valid_min = 10, total_hours_valid_max = 120,
          total_hours_missing = c(995L, 997L, 998L),
          primary_hours_valid_min = 10, primary_hours_valid_max = 168,
          primary_hours_missing = c(995L, 996L, 997L, 998L)
        ),
        drop_from_candidates_by_wave = list(
          "3" = character(0),
          "4" = c("H4LM6", "H4LM11", "H4LM12", "H4LM13", "H4LM19"),
          "5" = character(0)
        ),
        drop_from_candidates = character(0)
      ),
      
      # UsualHours is an unsupported family and fails explicitly rather than
      # routing to a production outcome.
      UsualHours = list(
        type    = "continuous",
        members = NULL,
        alias_for = "HoursWorked",
        cap_hours = 120,
        natural_lower_bound = 0,
        natural_upper_bound = 120,
        lower_bound_rule = "natural",
        upper_bound_rule = "fixed",
        sources = list(
          "3" = c(current_work = "H3LM7", main_job_hours = "H3LM16"),
          "4" = c(first_job_current = "H4LM6", current_work = "H4LM11",
                  current_jobs = "H4LM12", total_hours = "H4LM13",
                  primary_job_hours = "H4LM19")
        ),
        wave3_codes = list(
          work_yes = 1L, work_no = 0L,
          work_missing = 7L,
          # H3LM16 is observed on a 0-90 numeric range, but H3LM7=1 defines
          # current paid work at >=10 h/week. Values 0-9 are therefore
          # recognized for audit purposes but not accepted as logically
          # consistent worker-hours outcomes.
          hours_observed_min = 0, hours_valid_min = 10, hours_valid_max = 90,
          hours_missing = c(96L, 97L, 98L, 99L)
        ),
        wave4_codes = list(
          first_job_yes = 1L,
          current_work_yes = 1L, current_work_no = 0L,
          current_jobs_valid = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 11L, 12L, 15L),
          current_jobs_total_hours_route = c(2L, 3L, 4L, 5L, 6L, 7L, 11L, 12L, 15L, 98L),
          current_jobs_missing = c(95L, 97L, 98L),
          total_hours_valid_min = 10, total_hours_valid_max = 120,
          total_hours_missing = c(995L, 997L, 998L),
          primary_hours_valid_min = 10, primary_hours_valid_max = 168,
          primary_hours_missing = c(995L, 996L, 997L, 998L)
        ),
        drop_from_candidates_by_wave = list(
          "3" = c("H3LM7", "H3LM16"),
          "4" = c("H4LM6", "H4LM11", "H4LM12", "H4LM13", "H4LM19"),
          "5" = character(0)
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
        drop_from_candidates = c("Earnings", "EarningsSource", "H4EC2", "H4EC3")   # fallback fields
      ),
      
      # ----- Health Status -----
      # Binary indicator for at least good self-rated health.
      HealthStatus = list(
        type    = "binary_nested",
        members = list(
          at_least_good = list(label = "At least good health", primary = TRUE)
        ),
        sources = list(
          "3" = "H3GH1",
          "4" = "H4GH1"
        ),
        drop_from_candidates_by_wave = list(
          "3" = "H3GH1", "4" = "H4GH1"
        ),
        report_ratio_translations = TRUE,
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
    
    # Source-informed exact-code missing classifier. Values such as 196 and
    # 22198 are not classified as reserve codes by their numeric suffix.
    # The frozen dictionary separates documented general-missing and skip codes.


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
    # Drop invalid sampling weights during dataset construction so every
    # subsequent stage uses the same valid-weight analytic cohort.
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
    # Whole-PSU internal fold count for rough-screen ranking. This is a
    # training-only filter separate from final CV-TMLE cross-fitting.


    folds                   = 3L,
    binomial_eps            = 1e-15,
    # Iteration and convergence controls for per-variable ridge-logistic
    # ranking fits. A positive ridge penalty regularizes sparse-factor designs.


    glmnet_maxit            = 1000000L,
    glmnet_thresh           = 1e-5,
    # Ridge penalty used in the per-variable ranking screen.
    # Regularization and convergence must be assessed within each training fold.


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
    
    # Nested screening uses only the final outer-fold training rows.
    # Outcome, outcome-observation and joint A/Y signals drive the candidate pool;
    # strong exposure-only predictors are not directly forced into final W.


    nested_rough_prescreen_in_final_cv = TRUE,
    # Internal whole-PSU fold count for the rough ranking screen.
    # This ranking filter is separate from the final estimator cross-fitting.


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
    
    # Training-only redundancy control represents each variable by a
    # one-dimensional empirical signature and applies the configured
    # correlation-based deduplication method.

    rough_redundancy_control      = TRUE,
    # Correlation threshold for redundancy grouping. Final rough-variable
    # and processed-column budgets separately constrain model size.


    rough_redundancy_cor_threshold = 0.90,
    # cluster uses deterministic correlation grouping and a deterministic
    # representative. Complete linkage groups only mutually highly correlated
    # variables at the configured threshold. greedy applies an ordered filter.


    redundancy_method             = "cluster",       # "cluster" or "greedy" 
    redundancy_linkage            = "complete",      # complete = conservative; protects real confounders
    cluster_dedupe_max_vars       = 6000L,           # skip pre-score clustering above this many SUBSTANTIVE candidates (runtime guard; indicators excluded from count)
    # Maximum diversified candidate pool entering redundancy control and
    # multivariable screening; the final processed-column cap is a separate limit.

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
    # protected_W is resolved from cfg$analysis$protected_W_mode via
    # resolve_protected_W(). These two entries define the candidate sets; do
    # not edit protected_W directly.
    protected_W_items             = paste0("H1FS", 1:19),
    protected_W_summary           = "MHSum_W1",
    protected_W                   = paste0("H1FS", 1:19),
    # All variables returned by get_mandatory_W() bypass screening and the
    # optional processed-column budget. protected_W remains the subset whose
    # substantive factor levels must be preserved exactly.
    mandatory_W_bypass_screening      = TRUE,
    protected_W_bypass_screening      = TRUE,
    # Preserve every observed substantive level of protected H1FS factors.
    # These variables bypass rare-level, sparse-exposure, and maximum-level collapsing.
    protected_W_preserve_substantive_levels = TRUE,
    # Elastic-net alpha and iteration limit for the multivariable union screen.


    lasso_screen_alpha            = 0.50,
    lasso_screen_nlambda          = 100L,
    lasso_screen_glmnet_maxit    = 1000000L,
    # lambda.min retains the cross-validation loss minimizer for causal
    # screening; lambda.1se applies the more parsimonious one-standard-error rule.


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
    # Raw events-per-column gating is disabled; use held-out fit and support diagnostics.
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
    g_lower                      = 0.025, # fixed-rule value and sensitivity comparator
    g_lower_rule                 = "sample_size",
    g_lower_sample_size_constant = 5,
    broad_Q_candidate            = TRUE,
    broad_Q_max_raw              = 300L,
    broad_Q_max_processed        = 520L,
    # One extra candidate: reuse Q ensemble residuals, screen ONCE per outer
    # training fold, then fit the fixed design on the existing inner PSU folds.
    # No ATT/p-value selects its weights. Inner risk is not fully nested over screening.
    residual_Q_candidate         = TRUE,
    residual_Q_pilot_max_raw     = 300L, # compatibility setting; no separate pilot is fitted
    residual_Q_add_max_raw       = 150L,
    residual_Q_max_processed     = 520L,
    g_upper                      = 0.975,
    pi_lower                     = 0.025,
    pi_upper                     = 0.999,
    positivity_warning_threshold = 0.05,
    positivity_warning_fraction  = 0.10,
    
    # Report ATT and labelled ATE comparators. Optional overlap trimming
    # defines a secondary fitted-support population and changes its target.
    # The designated headline is the joint-component ATT, not a data-selected
    # estimate. Examine overlap in the target population without selecting
    # an estimand or bound based on its effect estimate.


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
    
    use_fold_checkpoints         = TRUE,
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
    auxiliary_binary_library = c("SL.mean", "SL.glmnet.fixed"),
    # The regularized elastic-net Q candidate is enabled instead of an
    # unregularized dummy-expanded GLM. Per-fit validation checks finite predictions.


    Q  = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_ranger = TRUE, use_xgboost = TRUE,
              # Optional control-arm Q candidates fit the base learner on unexposed
              # training rows with A removed. Exposed predictions add a survey-weighted
              # shift learned from exposed training rows. These candidates are disabled
              # in the primary library and enabled by D13_Q_control_arm; ensemble weights
              # are fitted using the SuperLearner loss criterion.


              use_glmnet_control_arm = FALSE,
              use_ranger_control_arm = FALSE,
              use_xgboost_control_arm = FALSE,
              use_xgboost_rich = FALSE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    g  = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_glmnet_h1fs = FALSE,
              use_ranger = TRUE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    pi = list(use_mean = TRUE, use_glm = FALSE, use_glmnet = TRUE,
              use_glmnet_A_unpenalized = FALSE,
              # The primary fixed glmnet leaves A unpenalized in Q and pi.
              # A separate unpenalized-A pi sensitivity fit is optional and disabled.

              dual_fit_A_unpenalized = FALSE,
              use_ranger = FALSE, use_xgboost = TRUE, use_earth = FALSE,
              use_gam = FALSE, use_svm = FALSE, use_nnet = FALSE),
    # alpha = 0.5 (elastic net) actually takes effect via SL.glmnet.fixed.
    # Stock SL.glmnet ignores alpha; the wrapper passes it through.
    # lambda_choice: "min" (CV-minimum, more permissive) or "1se" (1-SE rule).
    glmnet  = list(alpha = 0.5, nlambda = 100L, lambda_choice = "min",
                   maxit = 100000L, standardize = TRUE, internal_folds = 5L),
    glmnet_h1fs = list(h1fs_penalty_multiplier = 0.25),
    glmnet_pi_A = list(A_penalty_multiplier = 0.0),
    # always_split_A makes each forest node consider A when A is a Q
    # predictor. It has no effect on g, whose design excludes A.

    ranger  = list(num.trees = 500L, always_split_A = FALSE),
    xgboost = list(ntrees = 150L, max_depth = 2L, shrinkage = 0.05,
                   min_child_weight = 20),
    # Optional Q-only sensitivity learner. It is registered under a distinct
    # SuperLearner name so enabling it does not alter g or pi.
    xgboost_rich = list(ntrees = 250L, max_depth = 3L, shrinkage = 0.04,
                        min_child_weight = 10),
    # MARS knot search can be expensive on large dummy-expanded designs.
    # Keep the candidate disabled unless memory and runtime have been assessed.


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
  
  # Optional wave-specific mortality linkage/composite. `enabled_waves` selects
  # which requested outcome waves use the mortality-inclusive zero composite.
  # NDI supplies death year (NDIDD19Y) and month (NDIDD19M). For NDIDD19M,
  # native missing and code 997 are recognized invalid/unknown month values;
  # they are NEVER interpreted as evidence that no death occurred. Other
  # nonmissing month codes outside 1:12 fail loudly.
  #
  # Every supported outcome uses month-level ordering against the respondent's
  # actual interview year/month. For a noninterviewed respondent, the fallback
  # reference is the latest complete interview year-month observed in that wave
  # file (the fieldwork endpoint). A death in an interviewed respondent's same
  # interview month is not assumed to precede the observed outcome because the
  # respondent was alive at interview. A noninterviewed respondent who died on
  # or before the fieldwork endpoint is assigned the mortality-composite zero;
  # an endpoint-year death with unknown month remains timing-unresolved.
  mortality_sensitivity = list(
    enabled = TRUE,
    enabled_waves = c(3L, 4L),
    source_var = "NDIDD19Y",
    source_month_var = "NDIDD19M",
    valid_year_min = 1900L,
    valid_year_max = 2100L,
    valid_month_min = 1L,
    valid_month_max = 12L,
    invalid_month_codes = 997L,
    native_missing_means_no_death = TRUE,
    no_death_codes = 99997,
    fail_on_unrecognized_codes = TRUE,
    require_complete_linkage = TRUE,
    fail_on_death_with_observed_original_outcome = TRUE,
    composite_zero_at_death = TRUE,
    wave_specs = list(
      `3` = list(
        timing_mode = "interview_month",
        death_year_start = 1997L,
        death_year_end = 2002L,
        derived_death_year_var = "NDIY3",
        derived_death_month_var = "NDIM3",
        death_in_window_var = "D3Raw",
        death_before_outcome_var = "DeathW3",
        timing_status_var = "D3Time",
        interview_year_var = "IYEAR3",
        interview_month_var = "IMONTH3",
        interview_year_valid_min = 2001L,
        interview_year_valid_max = 2002L,
        interview_month_valid_min = 1L,
        interview_month_valid_max = 12L,
        linkage_audit_csv = "mort_link.csv",
        interview_timing_audit_csv = "itime.csv",
        contradiction_audit_csv = "mort_conf.csv",
        output_csv = "mort_zero.csv"
      ),
      `4` = list(
        timing_mode = "interview_month",
        death_year_start = 1997L,
        death_year_end = 2009L,
        derived_death_year_var = "NDIY4",
        derived_death_month_var = "NDIM4",
        death_in_window_var = "D4Raw",
        death_before_outcome_var = "DeathW4",
        timing_status_var = "D4Time",
        interview_year_var = "IYEAR4",
        interview_month_var = "IMONTH4",
        interview_year_valid_min = 2007L,
        interview_year_valid_max = 2009L,
        interview_month_valid_min = 1L,
        interview_month_valid_max = 12L,
        linkage_audit_csv = "mort_link.csv",
        interview_timing_audit_csv = "itime.csv",
        contradiction_audit_csv = "mort_conf.csv",
        output_csv = "mort_zero.csv"
      )
    )
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
    diagnostics_dir           = "d",
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
    # Complementary aggregate pattern-mixture diagnostic for unresolved W4
    # hours/LFP only. No row-level output and no additional nuisance fitting.
    enable_mnar_two_part = TRUE,
    mnar_two_part_zero_grid = c(0, 0.05, 0.10, 0.20),
    # NULL: hours {-10,-5,0,5,10}; LFP probability {-0.10,-0.05,0,0.05,0.10}.
    mnar_two_part_delta_grid = NULL,
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
    # g-only clipping rows vary the propensity floor while holding pi bounds
    # fixed. Joint g/pi sweeps cannot isolate the contribution of either bound.
    # ATT control odds g/(1-g) are bounded above through g_upper.


    att_g_only_clip_floors = c(0.005, 0.01, 0.025, 0.05),
    # Sample-size-based lower bound c/(sqrt(n) * log(n)) of Gruber, Phillips,
    # Lee & van der Laan (Am J Epidemiol 2022;191:1640-1651), the default in
    # the CRAN tmle package. With missing outcomes the paper directs that n be
    # the number of OBSERVED outcomes, not the analytic sample size; the bound
    # is computed at runtime from the realized observed-outcome count so it
    # tracks whatever the outcome constructor actually produces.
    att_g_clip_include_sample_size_bound = TRUE,
    att_g_clip_sample_size_constant = 5,
    # Verified structural zeros are observed from records rather than
    # interview response. The sequential estimator separates these observations
    # from outcomes requiring interview response.


    att_structural_zero_pi_audit_csv = "att_structural_zero_pi_audit.csv",
    att_structural_zero_pi_override = FALSE, # obsolete: primary uses sequential observation model
    # Optional fitted-DGP positivity simulation runs after primary outputs
    # and never alters the estimate. Screening, recipes, Q and response surfaces
    # are held fixed; g fitting and cross-fitting are repeated. It is not proof
    # of actual-data coverage. The configured time budget limits this stage.


    enable_bootstrap_positivity_diagnostic = FALSE,
    bootstrap_positivity_replicates = 200L,
    # Soft time budget checked between replicates. The first replicate
    # determines the planned count; a single replicate can exceed the budget.
    # Progress and failures are saved after every completed attempt.
    bootstrap_positivity_max_seconds = 5400,
    # Use the same data-adaptive g library as the primary fit. A different
    # library is rejected; screening and fold recipes remain fixed here.
    bootstrap_positivity_g_library = NULL, # NULL uses the full primary g library
    bootstrap_positivity_beta_precision = 12,
    # g floors evaluated per replicate. The same simulated data and the same
    # refitted g are reused across floors, so the grid is nearly free and the
    # comparison between floors is paired.
    bootstrap_positivity_g_floors = c(0.005, 0.025, 0.05),
    bootstrap_positivity_include_sample_size_bound = TRUE,
    bootstrap_positivity_csv = "att_bootstrap_positivity_diagnostic.csv",
    bootstrap_positivity_replicate_csv = "att_bootstrap_positivity_replicates.csv",
    persist_diagnostic_W_design = TRUE,
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
    dsp4_outcome_audit_csv           = "dsp4_outcome_audit.csv",
    g_calibration_audit_csv          = "g_calibration_audit.csv",
    prognostic_balance_csv           = "prognostic_score_balance.csv",
    # Export held-out initial Q(0,W) keyed by respondent ID only inside the
    # restricted workspace. A fixed reference export enables comparable
    # prognostic-balance checks across specifications. Never upload this export.


    prognostic_score_export_csv      = "prognostic_score_export.csv",
    prognostic_reference_score_path  = NULL,
    ovb_robustness_csv               = "att_additive_bias_sensitivity.csv",
    adaptive_truncation_csv          = "adaptive_truncation_selection.csv",
    enable_prognostic_balance        = TRUE,
    enable_ovb_robustness            = TRUE,
    # This switch produces fixed additive-bias sensitivity in outcome units,
    # not an OLS partial-R2 robustness value.
    additive_bias_grid = NULL, # NULL uses a descriptive outcome-SD grid
    att_leaveout_top_k             = c(1L, 3L, 5L),
    run_manifest_csv               = "run_manifest.csv",
    wave2_completion_expanded_balance_csv = "wave2_completion_expanded_balance.csv",
    wave2_completion_all_candidates_csv = "wave2_completion_all_candidates.csv",
    wave2_completion_model_csv = "wave2_completion_model.csv",
    diagnostic_status_csv = "diagnostic_status.csv",
    
    # MNAR diagnostics hold nuisance fits and the primary estimate fixed.
    # They describe breakdown points, support-extreme means, and shifts scaled
    # to observed missingness-related gradients; they do not refit the pipeline.


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
    # Only exact verified family/wave specifications and an explicitly configured
    # PassThrough negative-control outcome are allowed by default.
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

# Apply before any data preparation, cache lookup, or pipeline invocation.
# Existing settings remain: g_calibration_method = "platt",
# calibrate_pi = FALSE; recorded Wave-IV interview/disposition evidence defines
# military/prison zeros for current hours/LFP only; never annual earnings.
cfg$analysis$extra_exclude_from_candidates <- unique(c(
  cfg$analysis$extra_exclude_from_candidates,
  "MOVER", "SGB96A39",
  "SGT96A82", "SGT96A83", "SGT96A84", "SGT96A85", "SGT96A86", "SGT96A87",
  "STC9503", "STC9511",
  "C1DI001T", "C1DI001M", "C1DI001F",
  "C1DI002T", "C1DI002M", "C1DI002F",
  "C1DI003T", "C1DI003M", "C1DI003F",
  "C1DI004T", "C1DI004M", "C1DI004F",
  "C1DI005T", "C1DI005M", "C1DI005F",
  "C1DR001T", "C1DR001M", "C1DR001F",
  "C1DR003T", "C1DR003M", "C1DR003F",
  "C1SM001T", "C1SM001M", "C1SM001F",
  "C1SM002T", "C1SM002M", "C1SM002F",
  "C1OP001M", "C1OP001F", "C1OP002M", "C1OP002F",
  "C1EC001", "C1EC002", "C1EC003T", "C1EC003M", "C1EC003F", "C1EC004",
  # Separate documentation conflict: STC9518's codebook lists 998 as valid;
  # the guide describes a sum of eight 1-4 items. Exclude until clarified.
  "STC9518"
))

# Explicit rules precede heuristic inference. Storing the complete map in cfg
# makes the existing Wave-I/main/fold cache fingerprints detect every edit.
cfg$preprocessing$codebook_missing_overrides <- local({
  rules <- list()
  add <- function(vars, general = numeric(0), skip = numeric(0),
                  as_factor = NULL, source) {
    for (v in vars) {
      if (!is.null(rules[[v]])) stop("Duplicate codebook rule: ", v)
      rules[[v]] <<- list(general = general, skip = skip,
                          as_factor = as_factor, source = source)
    }
  }
  
  # Official County Health codebook, pp. 2-7.
  # https://addhealth.cpc.unc.edu/wp-content/uploads/docs/data/codebooks/Wave-I-Health-Codebook.pdf
  health <- "Wave-I-Health-Codebook.pdf, pp. 2-7"
  add(c("C1LE001", "C1LE004", "C1LE005", "C1LE006"),
      general = 98, as_factor = FALSE, source = health)
  add(c("C1LE002", "C1LE003", "S1CT001",
        as.vector(outer(sprintf("C1DI%03d", 1:5), c("T", "M", "F"), paste0)),
        as.vector(outer(c("C1DR001", "C1DR003", "C1SM001", "C1SM002"),
                        c("T", "M", "F"), paste0)),
        as.vector(outer(c("C1OP001", "C1OP002"), c("M", "F"), paste0)),
        "C1EC003T"), general = 8, as_factor = FALSE, source = health)
  add(c("C1EC001", "C1EC003M", "C1EC003F", "C1EC004"),
      general = c(2, 8), as_factor = FALSE, source = health)
  add("C1EC002", general = c(92, 98), as_factor = FALSE, source = health)
  
  # Official state-characteristics codebook, pp. 3-11; exact widths differ.
  # https://addhealth.cpc.unc.edu/wp-content/uploads/docs/data/codebooks/Wave-I-State-Characteristics-Exclusionary-Indices-and-Inclusionary-Indices-Codebook.pdf
  state <- "State-Characteristics-Codebook.pdf, pp. 3-11"
  add(sprintf("STC95%02d", c(1:5, 20, 21)),
      general = 8, as_factor = FALSE, source = state)
  add(sprintf("STC95%02d", c(8, 11:17, 19, 22, 23)),
      general = 98, as_factor = FALSE, source = state)
  add(sprintf("STC95%02d", c(6, 7, 10, 24)),
      general = 999998, as_factor = FALSE, source = state)
  add("STC9509", general = 99998, as_factor = FALSE, source = state)
  
  # Official political-context codebook, Appendix B, pp. 12-26.
  # https://addhealth.cpc.unc.edu/wp-content/uploads/docs/data/codebooks/W1POLCON.pdf
  political <- "W1POLCON.pdf, Appendix B"
  add("W1PCA195", general = 8, as_factor = TRUE, source = political)
  add("W1PCA295", general = 98, as_factor = FALSE, source = political)
  add("W1PCC290", general = 98, as_factor = TRUE, source = political)
  add(c("W1PCC190", "W1PCG195", "W1PCG295", "W1PCG395",
        "W1PCP192", "W1PCP292", "W1PCP392"),
      general = 8, as_factor = FALSE, source = political)
  add(c("W1PCS192", "W1PCS292", "W1PCS392",
        "W1PCS194", "W1PCS294", "W1PCS394"),
      general = 8, skip = 7, as_factor = FALSE, source = political)
  
  # Use the detailed birth codebook's RPDA003-RPDA007 names, not the
  # differently numbered user-guide summary. All 14 fields are categorical.
  # https://addhealth.cpc.unc.edu/wp-content/uploads/docs/data/codebooks/Birth_Records_Database_Codebook.pdf
  birth <- "Birth_Records_Database_Codebook.pdf, pp. 2-9"
  add(c("RIWA001", "RIDA002", "RPDA004", "RPDA005"),
      general = 98, as_factor = TRUE, source = birth)
  add("RIWA002", general = c(995, 996, 998), as_factor = TRUE, source = birth)
  add("RIWA003", general = c(996, 998), as_factor = TRUE, source = birth)
  add(c("RIWA004", "RIDA001", "RPDA001", "RPDA002", "RPDA003",
        "RPDA006", "RPDA007", "RMMA001"),
      general = c(96, 98), as_factor = TRUE, source = birth)
  
  # In-home sections 1, 3, 11-13, 28, 32B, 38 and 40.
  # These documented codes must not be suppressed by the percentage heuristic.
  # https://addhealth.cpc.unc.edu/wp-content/uploads/docs/data/codebooks/Wave-I-In-Home-Interview-Data.zip
  inhome <- "Wave-I-In-Home-Interview-Data.zip; variable-specific codebooks"
  add(c("H1GI3", "H1GH51", "H1TO10", "H1IR16"),
      general = c(96, 98, 99), source = inhome)
  add(c("H1TO5", "H1FP5D", "H1NF8", "H1NF9", paste0("H1HR10", LETTERS[1:20])),
      general = c(96, 98, 99), skip = 97, source = inhome)
  add(c("H1TO11", "H1TO14", "H1NM3", "H1NM8"),
      general = c(96, 98), skip = 97, source = inhome)
  add("H1NF3", general = 98, skip = 97, source = inhome)
  add("H1NM9", general = c(98, 99), skip = 97, source = inhome)
  
  # Introductory Guide documents genuine value/reserve-code collisions.
  # Preserve those ambiguous values, but restore separately identifiable skips.
  add(c("H1TO7", "H1TO16"), skip = 97, source = inhome)
  add(c("H1EE5", "H1EE7"), skip = 997, source = inhome)
  add(paste0("H1IR8", LETTERS[1:4]), skip = 7, source = inhome)
  add("H1IR13", general = 99, skip = 97, source = inhome)
  rules
})

# =============================================================================
