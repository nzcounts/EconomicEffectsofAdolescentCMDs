# =============================================================================
# 0) USER CONFIGURATION
# -----------------------------------------------------------------------------
# Edit these values to explore design choices.
# =============================================================================

cfg <- list(
  stages = list(
    # run_read_wave1_phase: TRUE = read and merge the raw Wave 1 source files for this run.
    run_read_wave1_phase = TRUE,
    # run_build_main_dataset_phase: TRUE = build the person-level analysis dataset for this run.
    run_build_main_dataset_phase = TRUE,
    # run_rough_regression_prescreen: TRUE = run the fast standard-regression pre-screen.
    run_rough_regression_prescreen = TRUE,
    # run_grouped_lasso_prescreen: TRUE = run the standalone full-data grouped-lasso stage.
    # Default FALSE here because your primary workflow is now Option 3: use the rough
    # pre-screen shortlist in final TMLE, then rerun grouped screening only within TMLE folds.
    run_grouped_lasso_prescreen = FALSE,
    # run_final_cv_tmle: TRUE = run the final cross-validated TMLE stage.
    run_final_cv_tmle = TRUE
  ),
  
  global = list(
    # pipeline_seed: one master seed for the whole pipeline.
    pipeline_seed = 20260402L,
    # output_dir: folder where CSV/RDS/results files are written.
    output_dir = ".",
    # verbose: TRUE = print progress messages as the pipeline runs.
    verbose = TRUE,
    # save_stage_csvs: TRUE = write stage CSV outputs for audit/restart purposes.
    save_stage_csvs = TRUE,
    # autorun_pipeline: TRUE = run the pipeline automatically when the script is sourced.
    autorun_pipeline = TRUE,
    # drop_invalid_weight_rows_globally: TRUE = remove rows with missing/nonpositive/nonfinite
    # survey weights once near the start so every later phase targets the same weighted sample.
    drop_invalid_weight_rows_globally = TRUE
  ),
  
  cache = list(
    # use_cached_wave1: TRUE = read the merged Wave 1 object from RDS instead of rebuilding it.
    use_cached_wave1 = FALSE,
    # use_cached_main_dataset: TRUE = read the constructed main dataset from RDS instead of rebuilding it.
    use_cached_main_dataset = FALSE,
    # save_intermediate_rds: TRUE = save intermediate objects after each major stage so a later
    # crash in CSV writing or diagnostics does not force a full rerun.
    save_intermediate_rds = TRUE,
    # wave1_rds: filename for the cached merged Wave 1 object.
    wave1_rds = "wave1_merged.rds",
    # main_dataset_rds: filename for the cached constructed main dataset.
    main_dataset_rds = "main_dataset.rds",
    # rough_prescreen_rds: filename for the cached rough pre-screen output object.
    rough_prescreen_rds = "rough_prescreen_result.rds",
    # grouped_prescreen_rds: filename for the cached standalone grouped prescreen output object.
    grouped_prescreen_rds = "grouped_prescreen_result.rds",
    # final_tmle_rds: filename for the cached final CV-TMLE output object.
    final_tmle_rds = "final_cv_tmle_result.rds"
  ),
  
  analysis = list(
    # exposure_var: name of the exposure variable used downstream after construction.
    exposure_var = "Depressed",
    # outcome_var: name of the outcome variable used downstream after construction.
    outcome_var = "logEarnings",
    # exposure_type: "auto" = infer whether the exposure is binary from the realized values.
    exposure_type = "auto",
    # outcome_type: "auto" = infer whether the outcome is binary or continuous from observed values.
    outcome_type = "auto",
    # id_var: individual identifier column.
    id_var = "AID",
    # cluster_var: cluster identifier used for fold construction and cluster-robust inference.
    cluster_var = "PSUSCID",
    # weight_var: sampling-weight column used for the weighted analytic sample and weighted TMLE.
    weight_var = "GSWGT1",
    # outcome_observed_var: indicator that the outcome is observed.
    outcome_observed_var = "delta_Y",
    # extra_exclude_from_candidates: helper columns to exclude from screening/final W if custom constructors create them.
    extra_exclude_from_candidates = character(0)
  ),
  
  variable_sets = list(
    # use_all_wave1_vars_for_rough_prescreen: TRUE = let the rough pre-screen start from all eligible Wave 1 variables.
    use_all_wave1_vars_for_rough_prescreen = TRUE,
    # use_stage1_shortlist_for_grouped: TRUE = if you run a standalone grouped screen, start it from the rough pre-screen shortlist.
    use_stage1_shortlist_for_grouped = TRUE,
    # use_stage1_shortlist_for_final: TRUE = start final TMLE from the rough pre-screen shortlist.
    # This is the key default for your chosen workflow: standard regression pre-screen first, then nested grouped screening within TMLE folds.
    use_stage1_shortlist_for_final = TRUE,
    # stage1_shortlist_source: "memory" = use the in-session stage-1 object; "csv" = read the saved stage-1 shortlist CSV.
    stage1_shortlist_source = "memory",
    # stage2_shortlist_source: "memory" = use the in-session stage-2 object; "csv" = read the saved stage-2 shortlist CSV.
    stage2_shortlist_source = "memory",
    # write_stage1_shortlist_csv: filename for the saved rough pre-screen shortlist.
    write_stage1_shortlist_csv = "stage1_keepboth.csv",
    # write_stage2_shortlist_csv: filename for the saved standalone grouped-screen shortlist.
    write_stage2_shortlist_csv = "stage2_selected_groups.csv"
  ),
  
  exposure = list(
    # derive_from_cesd: TRUE = build the default CES-D-based binary depression exposure.
    derive_from_cesd = TRUE,
    # custom_constructor: optional user-supplied function for a different exposure definition.
    custom_constructor = NULL,
    # cesd_items: Wave 2 CES-D item columns used in the default exposure constructor.
    cesd_items = paste0("H2FS", 1:19),
    # reverse_score_items: CES-D items that are reverse-scored before summing.
    reverse_score_items = c("H2FS4", "H2FS8", "H2FS11", "H2FS15"),
    # nonresponse_codes: item codes treated as nonresponse for the default exposure constructor.
    nonresponse_codes = c(6, 8, 9),
    # cutpoint: CES-D sum threshold used to create the default binary exposure.
    cutpoint = 22,
    # drop_missing_exposure: TRUE = drop records without usable exposure-construction data.
    drop_missing_exposure = TRUE,
    # drop_from_candidates: helper columns created by exposure construction that should not enter screening/final W.
    drop_from_candidates = c("MHSum", paste0("H2FS", 1:19))
  ),
  
  outcome = list(
    # derive_from_wave4_earnings: TRUE = build the default Wave 4 earnings outcome.
    derive_from_wave4_earnings = TRUE,
    # custom_constructor: optional user-supplied function for a different outcome definition.
    custom_constructor = NULL,
    # exact_var: exact earnings variable used by the default outcome constructor.
    exact_var = "H4EC2",
    # bracket_var: bracketed earnings variable used when exact earnings are unavailable.
    bracket_var = "H4EC3",
    # exact_valid_upper: upper bound above which exact_var is treated as not a usable exact value.
    exact_valid_upper = 9999996,
    # bracket_map: midpoint map from bracket code to numeric earnings value.
    bracket_map = c(`1` = 2500, `2` = 7500, `3` = 12500, `4` = 17500,
                    `5` = 22500, `6` = 27500, `7` = 35000, `8` = 45000,
                    `9` = 62500, `10` = 87500, `11` = 125000, `12` = 175000),
    # nonresponse_codes: special codes treated as nonresponse in the default outcome constructor.
    nonresponse_codes = c(96, 98),
    # earnings_floor_for_log: small positive floor applied before log-transforming earnings.
    earnings_floor_for_log = 0.1,
    # rough_prescreen_zero_floor: floor used when very small values would destabilize rough continuous screens.
    rough_prescreen_zero_floor = 1e-10,
    # log_transform: TRUE = log-transform the default earnings outcome.
    log_transform = TRUE,
    # continuous_upper_quantile: upper quantile used to bound continuous outcomes for TMLE.
    continuous_upper_quantile = 0.99,
    # continuous_bound_eps: small epsilon used to keep bounded outcomes away from exact 0/1.
    continuous_bound_eps = 0.001,
    # drop_from_candidates: helper columns created by outcome construction that should not enter screening/final W.
    drop_from_candidates = c("Earnings", "H4EC2", "H4EC3")
  ),
  
  preprocessing = list(
    # factor_unique_threshold: variables with this many usable unique values or fewer are initially treated as factors.
    factor_unique_threshold = 10L,
    # bad_codes_high: high-value special codes ignored when counting usable unique values.
    bad_codes_high = c(96, 97, 98, 99),
    # bad_codes_low: low-value special codes ignored when counting usable unique values.
    bad_codes_low = c(6, 8, 9),
    # numeric_missing_scheme: "dual_indicators" keeps separate actual-missing and skip indicators for numeric variables.
    numeric_missing_scheme = "dual_indicators",
    # numeric_imputation: scalar imputation rule for numeric preprocessing when imputation is required.
    numeric_imputation = "median",
    # factor_missing_label: label assigned to factor missingness after recoding.
    factor_missing_label = "Missing",
    # factor_other_label: label assigned when rare factor levels are collapsed.
    factor_other_label = "_Other_",
    # remove_duplicate_y_suffix_columns: TRUE = drop duplicate .y columns created by joins.
    remove_duplicate_y_suffix_columns = TRUE,
    # constant_variance_tol: variance threshold below which a numeric column is treated as constant.
    constant_variance_tol = 1e-10,
    # scale_eps: lower bound used when deciding whether a scale estimate is effectively zero.
    scale_eps = 1e-12,
    # sanitize_column_names_for_model_matrix: TRUE = clean model-matrix column names when needed.
    sanitize_column_names_for_model_matrix = TRUE,
    # long_factors: manually specified variables that should be treated as factors even if they exceed the unique-value threshold.
    long_factors = c(
      "H1HR3A","H1HR5A","H1HR13","H1GI12","H1NM4","H1NF4","H1RM1","H1RM3","H1RM4",
      "H1RF1","H1RF3","H1RF4","H1RI5A_1","H1RI5A_2","H1RI5A_3","H1RI15_1","H1RI29A1",
      "H1RI29B1","H1RI29C1","H1RI15_2","H1RI15_3","H1RX5A_1","H1RX15_1","H1RX5A_2",
      "H1RX15_2","H1RX5A_3","H1RX15_3","H1RE1","PA12","PA22","PB7","PB8","COMMID.x",
      "COMMID.y","H1TO37","H1TO34","H1TO30","H1TO2","H1TO40","H1FV14M","H1FV14Y"
    ),
    # force_factor_prefixes: variable-name prefixes that should always be treated as factors.
    force_factor_prefixes = c("H1HR6")
  ),
  
  rough_prescreen = list(
    # method: "linear" = standard regression rough screen; "light_ensemble" = lightweight SL rough screen.
    method = "linear",
    # seed: optional stage-specific seed override for the rough pre-screen.
    seed = NULL,
    # pre_scale_numeric: TRUE = full-data pre-scaling before rough screening; FALSE keeps scaling leakage-free within folds.
    pre_scale_numeric = FALSE,
    # folds: number of folds used in the rough pre-screen.
    folds = 5L,
    # binomial_eps: clipping constant used in rough binomial log-loss calculations.
    binomial_eps = 1e-15,
    # create_plots: TRUE = draw rough-screen knee plots.
    create_plots = TRUE,
    # use_knee_cutoff: TRUE = retain variables above the knee cutoff.
    use_knee_cutoff = TRUE,
    # light_ensemble_library: learner library used if method = "light_ensemble".
    light_ensemble_library = c("SL.mean", "SL.glm", "SL.glmnet"),
    # light_ensemble_scale_numeric_within_fold: TRUE = scale numeric columns within training folds in the light-ensemble rough screen.
    light_ensemble_scale_numeric_within_fold = TRUE,
    # exposure_output_csv: filename for the rough exposure-screen score output.
    exposure_output_csv = "keepexposure.csv",
    # outcome_output_csv: filename for the rough outcome-screen score output.
    outcome_output_csv = "keepoutcome.csv"
  ),
  
  grouped_prescreen = list(
    # seed: optional stage-specific seed override for grouped screening.
    seed = NULL,
    # engine: grouped-screen fitting engine.
    engine = "grpreg_unweighted",
    # use_survey_weights_when_supported: TRUE = use sampling weights where the grouped-screen engine can support them.
    use_survey_weights_when_supported = TRUE,
    # screen_outcome: TRUE = allow grouped screening to use the outcome.
    screen_outcome = TRUE,
    # outer_folds: number of outer folds used by grouped screening.
    outer_folds = 5L,
    # inner_folds: number of inner folds used by grouped screening.
    inner_folds = 5L,
    # rare_level_min_n: minimum total count for a factor level to avoid collapsing.
    rare_level_min_n = 15L,
    # rare_level_min_exposed: minimum exposed and control support for a factor level to avoid collapsing.
    rare_level_min_exposed = 3L,
    # factor_max_levels_after_collapse: maximum number of factor levels retained after rare-level collapsing.
    factor_max_levels_after_collapse = 25L,
    # numeric_min_observed_n: minimum observed numeric count for a numeric group to be kept.
    numeric_min_observed_n = 40L,
    # numeric_min_observed_prop: minimum observed proportion for a numeric group to be kept.
    numeric_min_observed_prop = 0.10,
    # group_missing_max_prop: maximum allowed missingness proportion before a group is dropped.
    group_missing_max_prop = 0.95,
    # winsor_probs: winsorization cut points used when the grouped screen models a continuous outcome.
    winsor_probs = c(0.01, 0.99),
    # selection_rule: lambda-selection rule used after grouped CV.
    selection_rule = "min",
    # penalty: grouped penalty family.
    penalty = "grLasso",
    # alpha: grouped elastic-net mixing value when alpha tuning is not used.
    alpha = 1,
    # tune_alpha: TRUE = search over alpha_grid.
    tune_alpha = FALSE,
    # alpha_grid: candidate alpha values if tune_alpha = TRUE.
    alpha_grid = c(1.0, 0.8, 0.6, 0.4, 0.2),
    # nlambda: number of lambda values to evaluate in grouped screening.
    nlambda = 100L,
    # grpreg_eps: convergence tolerance for grpreg CV fits.
    grpreg_eps = 1e-4,
    # glmnet_proxy_alpha: alpha used by the weighted glmnet proxy grouped-screen engine.
    glmnet_proxy_alpha = 1,
    # glmnet_proxy_nlambda: number of lambda values used by the weighted glmnet proxy engine.
    glmnet_proxy_nlambda = 100L,
    # glmnet_proxy_lambda_choice: lambda rule used by the weighted glmnet proxy engine.
    glmnet_proxy_lambda_choice = "lambda.min",
    # max_exposure_only: maximum number of groups retained only because they predict exposure.
    max_exposure_only = 30L,
    # screen_outcome_missingness: TRUE = allow grouped screening to model outcome-observed status.
    screen_outcome_missingness = TRUE,
    # max_missingness_only: maximum number of groups retained only because they predict outcome observation.
    max_missingness_only = 20L,
    # min_group_frequency: stability threshold across outer folds for a group to survive the grouped screen.
    min_group_frequency = 2L,
    # fallback_criterion: fallback model-selection rule if grouped CV fails and plain grpreg is used.
    fallback_criterion = "BIC",
    # special_code_top_n: number of top values inspected when detecting survey-style special numeric codes.
    special_code_top_n = 4L,
    # skip_suffix: two-digit suffix interpreted as a skip/not-applicable code.
    skip_suffix = 97L,
    # missing_suffixes: two-digit suffixes interpreted as nonresponse/missing codes.
    missing_suffixes = c(96L, 98L, 99L),
    # dfmax: optional upper bound on the number of nonzero processed columns.
    dfmax = NULL,
    # gmax: optional upper bound on the number of selected groups.
    gmax = NULL,
    # always_keep: raw variables always retained if they survive preprocessing.
    always_keep = character(0),
    # selection_table_csv: filename for the standalone grouped-screen selection table.
    selection_table_csv = "grouped_prescreen_selection_table.csv"
  ),
  
  final_tmle = list(
    # restrict_to_valid_weights: TRUE = require valid positive survey weights in the final analytic sample.
    restrict_to_valid_weights = TRUE,
    # vfolds: number of outer folds used in final CV-TMLE.
    vfolds = 5L,
    # max_fold_tries: maximum attempts to construct valid cluster-respecting outer folds.
    max_fold_tries = 500L,
    # internal_superlearner_folds: number of internal CV folds used by SuperLearner nuisance fits.
    internal_superlearner_folds = 3L,
    # min_treated_warning_n: warn if the analytic sample has fewer treated observations than this.
    min_treated_warning_n = 30L,
    # g_lower: lower truncation bound for the treatment mechanism.
    g_lower = 0.025,
    # g_upper: upper truncation bound for the treatment mechanism.
    g_upper = 0.975,
    # pi_lower: lower truncation bound for the outcome-observed mechanism.
    pi_lower = 0.025,
    # positivity_warning_threshold: threshold used when flagging weak positivity support.
    positivity_warning_threshold = 0.05,
    # positivity_warning_fraction: fraction of observations beyond the positivity threshold that triggers a warning.
    positivity_warning_fraction = 0.10,
    # nested_grouped_prescreen_in_final_cv: TRUE = rerun grouped screening inside each outer TMLE fold.
    # This is your chosen Option 3 default because it reduces leakage from grouped variable selection.
    nested_grouped_prescreen_in_final_cv = TRUE,
    # allow_full_data_grouped_W_in_final_cv: TRUE = let the full-data grouped screen define final W directly.
    # Keep FALSE here because you explicitly want to avoid leakage.
    allow_full_data_grouped_W_in_final_cv = FALSE,
    # results_csv: filename for final TMLE results.
    results_csv = "cv_tmle_results.csv"
  ),
  
  learners = list(
    Q = list(
      # use_mean: TRUE = include the intercept-only mean learner in Q.
      use_mean = TRUE,
      # use_glm: TRUE = include a main-effects GLM learner in Q.
      use_glm = TRUE,
      # use_glmnet: TRUE = include penalized regression in Q.
      use_glmnet = TRUE,
      # use_ranger: TRUE = include ranger random forest in Q.
      use_ranger = FALSE,
      # use_xgboost: TRUE = include shallow boosted trees in Q.
      use_xgboost = FALSE,
      # use_earth: TRUE = include MARS in Q.
      use_earth = TRUE,
      # use_gam: TRUE = include GAM in Q.
      use_gam = FALSE,
      # use_svm: TRUE = include SVM in Q.
      use_svm = FALSE,
      # use_nnet: TRUE = include neural net in Q.
      use_nnet = FALSE
    ),
    g = list(
      # use_mean: TRUE = include the intercept-only mean learner in g.
      use_mean = TRUE,
      # use_glm: TRUE = include a main-effects GLM learner in g.
      use_glm = TRUE,
      # use_glmnet: TRUE = include penalized regression in g.
      use_glmnet = TRUE,
      # use_ranger: TRUE = include ranger random forest in g.
      use_ranger = FALSE,
      # use_xgboost: TRUE = include shallow boosted trees in g.
      use_xgboost = FALSE,
      # use_earth: TRUE = include MARS in g.
      use_earth = FALSE,
      # use_gam: TRUE = include GAM in g.
      use_gam = FALSE,
      # use_svm: TRUE = include SVM in g.
      use_svm = FALSE,
      # use_nnet: TRUE = include neural net in g.
      use_nnet = FALSE
    ),
    pi = list(
      # use_mean: TRUE = include the intercept-only mean learner in pi.
      use_mean = TRUE,
      # use_glm: TRUE = include a main-effects GLM learner in pi.
      use_glm = TRUE,
      # use_glmnet: TRUE = include penalized regression in pi.
      use_glmnet = TRUE,
      # use_ranger: TRUE = include ranger random forest in pi.
      use_ranger = FALSE,
      # use_xgboost: TRUE = include shallow boosted trees in pi.
      use_xgboost = FALSE,
      # use_earth: TRUE = include MARS in pi.
      use_earth = FALSE,
      # use_gam: TRUE = include GAM in pi.
      use_gam = FALSE,
      # use_svm: TRUE = include SVM in pi.
      use_svm = FALSE,
      # use_nnet: TRUE = include neural net in pi.
      use_nnet = FALSE
    ),
    # alpha: glmnet mixing value for the custom glmnet learner.
    glmnet = list(alpha = 1, nlambda = 100L),
    # num.trees: number of trees for ranger.
    ranger = list(num.trees = 500L),
    # xgboost settings for the custom xgboost learner.
    xgboost = list(ntrees = 200L, max_depth = 2L, shrinkage = 0.05, minobspernode = 20L),
    # earth settings for the custom earth learner.
    earth = list(degree = NULL, nprune = NULL),
    # gam settings for the custom GAM learner.
    gam = list(df = NULL),
    # svm settings for the custom weighted SVM learner.
    svm = list(cost = 1, gamma = NULL, kernel = "radial"),
    # nnet settings for the custom neural-net learner.
    nnet = list(size = 5, decay = 0.0001, maxit = 200, MaxNWts = 10000),
    # allow_non_weight_aware: FALSE = exclude learners that cannot reliably use observation weights.
    allow_non_weight_aware = FALSE,
    # enable_plotting: TRUE = allow learner-level plotting hooks where implemented.
    enable_plotting = TRUE
  ),
  
  diagnostics = list(
    # enable: TRUE = run the post-estimation diagnostics helper when you call it.
    enable = FALSE,
    # save_plots: TRUE = write diagnostic PNG plots.
    save_plots = TRUE,
    # save_csvs: TRUE = write diagnostic CSV outputs.
    save_csvs = TRUE,
    # plot_selection: TRUE = write grouped-selection plots when grouped results exist.
    plot_selection = TRUE,
    # plot_propensity: TRUE = write propensity-score plots.
    plot_propensity = TRUE,
    # plot_missingness: TRUE = write outcome-observed-model plots.
    plot_missingness = TRUE,
    # plot_outcome_distribution: TRUE = write observed-outcome distribution plots.
    plot_outcome_distribution = TRUE,
    # plot_fold_times: TRUE = write per-fold runtime plots.
    plot_fold_times = TRUE,
    # diagnostics_dir: folder where the diagnostics helper writes outputs.
    diagnostics_dir = "diagnostics"
  ),
  
  safety = list(
    # stop_if_no_learners: TRUE = fail early if any final TMLE learner library is empty.
    stop_if_no_learners = TRUE,
    # max_candidate_vars_warning: warn when the raw candidate set is this wide or wider.
    max_candidate_vars_warning = 4000L,
    # max_processed_columns_warning: warn when processed grouped designs become extremely wide.
    max_processed_columns_warning = 20000L
  )
)
