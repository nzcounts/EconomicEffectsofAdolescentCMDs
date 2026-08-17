# Executable methods and code map

This document describes the analysis implemented in the R source. It does not
use the placeholder Methods or Results sections of the supplied manuscript
background.

| Methodological component | Implemented specification | Primary code |
|---|---|---|
| Target estimand | Survey-weighted ATT: the contrast in adult earnings among adolescents who screened above the threshold, comparing observed exposure with the counterfactual below-threshold condition. Joint component targeting is the headline estimator; ATE, trimmed, one-step, and plug-in quantities are secondary. | `R/10_tmle.R`: `run_final_cv_tmle()`, `solve_target_score()` |
| Exposure | Wave II 19-item depressive-symptom score from `H2FS1:H2FS19`; reverse items 4, 8, 11, 15; exact nonresponse codes 6/8/9; threshold 22; incomplete scores excluded. | `R/02_config.R`; `R/05_data_construction.R`: `build_main_dataset()` |
| Primary outcome | Wave IV Compensation. Valid exact `H4EC2` values are 0-999995. Exact codes 9999996/9999998 are missing and may fall back to valid `H4EC3` bracket midpoints. Valid zero earnings are retained. | `R/04_utils.R`: `compute_earnings()`; `R/05_data_construction.R`: `construct_outcome_compensation()` |
| Mortality-inclusive outcome | NDI death year defines an inclusive 1997-2007 death window. Linked deaths are assigned observed zero earnings when the composite is enabled. `IYEAR4` is used only for audit checks, not death classification. | `R/05_data_construction.R`: `derive_mortality_indicator_from_data()`, `merge_mortality_indicator_from_data()`, `apply_mortality_composite()` |
| Continuous-outcome bound | Primary upper bound is the pooled observed-outcome `GSWGT1`-weighted 0.995 quantile, calculated with the configured weighted HF8 rule. It is part of the primary outcome definition. | `R/02_config.R`; `R/10_tmle.R`: `compute_continuous_cap()` |
| Baseline covariates | Candidate confounders are restricted to Wave I sources after role-based exclusions. `H1FS1:H1FS19` are protected mandatory covariates in every fold. The descriptive source registry is not an allowlist. | `R/05_data_construction.R`: `get_candidate_vars()`, `get_mandatory_W()`, `validate_candidate_governance()` |
| Missing-code handling | A source-informed exact-code dictionary is frozen on the complete Wave I distribution before fold construction. General missingness and structural skips are represented separately; rules are then reused without fold-specific relearning. | `R/04_utils.R`: `learn_conservative_missing_rule()`, `build_global_missing_code_dictionary()`; `R/10_tmle.R`: `learn_final_missing_recipe()` |
| Special transform | Wave I usual bedtime `H1GH50` is parsed as a 24-hour circular variable and represented by sine/cosine terms; invalid and sentinel values become missing. | `R/05_data_construction.R`: `transform_time_variables_in_df()` |
| Cross-fitting | Five outer folds assign whole sampled-school clusters to one fold while balancing size and active exposure-by-observation cells. Internal learner folds are also cluster-aware. | `R/06_folds.R`: `make_cluster_folds_balanced()`; `R/10_tmle.R`: `make_final_cv_folds()` |
| Screening | Conducted only within outer-training data: usability prefilter, weighted marginal outcome/observation/joint ranking, deterministic correlation-cluster redundancy control, and nested elastic-net union screening. Exposure-only ranking does not directly force variables into final W. | `R/07_screening.R`; `R/10_tmle.R`: `prefilter_candidate_vars_for_screen()`, `run_nested_rough_prescreen_for_final()` |
| Final preprocessing | Fold-pure numeric imputation/scaling and factor encoding are learned on training rows and applied to validation rows. Protected H1FS substantive factor levels are retained. Processed-column budgets apply only to nonprotected variables. | `R/08_preprocessing.R`; `R/10_tmle.R`: `build_final_W_train_valid()` |
| Nuisance learners | Super Learner libraries for outcome `Q`, exposure propensity `g`, and outcome-observation probability `pi`; configured custom elastic net, ranger, and xgboost learners use cluster-aware internal validation and deterministic seeds. | `R/09_learners.R`: `register_custom_learners()`, `build_sl_library()` |
| Missing outcomes | A treatment-aware observation model estimates `pi(A,W)`. Counterfactual `pi(1,W)` and `pi(0,W)` predictions enter ATT targeting and positivity diagnostics. | `R/09_learners.R`: `extract_pi_counterfactual_blocks()`; `R/10_tmle.R`: `run_final_cv_tmle()` |
| Survey inference | Point estimation uses raw positive Wave I sampling weights. ATT influence functions are evaluated with the full survey-domain frame using `REGION` strata and `PSUSCID` PSUs; a PSU-only calculation is sensitivity output. | `R/10_tmle.R`: `cluster_inference_from_eif()` |
| Integrity gates | Configured primary gates expect complete CES-D n=14,660; analytic n=13,500; treated n=1,342; 132 PSUs; four strata; cutpoint 22. These are validation expectations, not values newly calculated for this repository. | `R/02_config.R`; `R/05_data_construction.R`: `validate_expected_final_sample_gates()` |
| Diagnostics | Sample flow, effective sample sizes, fold support, selection stability, learner weights/risks, balance and Love plots, overlap, cluster influence, targeting/EIC checks, missing-code audit, cap audit, Wave II completion, g/pi clipping, MNAR analyses, and output inventory. | `R/11_diagnostics.R`: `run_peer_review_diagnostics()` |
| Sensitivity analyses | Full-refit scenarios vary outcome caps/transforms, exact-only earnings, mortality composite, exposure cutpoint, positivity bounds, learner sets, screening budgets, and redundancy thresholds. Fixed-nuisance diagnostics are clearly distinguished from full refits. | `R/14_sensitivity.R`: `final_sensitivity_scenarios()`, `run_sensitivity_analyses()` |
| Algorithmic stability | Ten prespecified seeds rerun the complete pipeline. Across-seed summaries and selection Jaccard statistics are descriptive only; the designated seed remains inferential. | `R/15_multiseed.R`: `run_multiseed_att()` |

## Identification and interpretation

The code estimates a causal ATT only under the usual observational-study
conditions: consistency, conditional exchangeability after the measured Wave I
covariates, positivity for the treated-population comparison, and a defensible
model for outcome observation. The exposure is a screening threshold, not a
clinical diagnosis, and the hypothetical intervention interpretation should
not be extended to interventions with effects outside the exposure pathway
without additional assumptions.

## Outcome-family boundary

Validation permits the verified Wave IV Compensation constructor and an
explicitly configured pass-through negative-control outcome. Other inherited
constructors are scaffolding and must remain disabled until their source
variables, coding, timing, and estimands are independently codebook-verified.
