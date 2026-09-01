# Executable methods and code map

This document describes the analysis implemented in the R source. It does not
use the placeholder Methods or Results sections of the supplied background.

| Component | Implemented specification | Primary code |
|---|---|---|
| Target estimand | Survey-weighted ATT among adolescents above the Wave II threshold. Joint component CV-TMLE is the headline estimator; ATE, trimmed, plug-in, AIPW, and one-step ATT quantities are comparators. | `R/10_tmle.R`: `run_final_cv_tmle()` |
| Exposure | Wave II `H2FS1:H2FS19`; reverse items 4, 8, 11, 15; codes 6/8/9 nonresponse; threshold 22; incomplete scores excluded. | `R/02_config.R`; `R/05_data_construction.R`: `build_main_dataset()` |
| Outcome routing | Each run selects one verified family/member/wave. Validation and the dispatcher reject unsupported or unverified mappings. | `R/03_validation.R`: `supported_outcome_waves()`, `is_verified_outcome_spec()`; `R/05_data_construction.R`: `construct_outcome()` |
| Hours worked | Wave IV unconditional current weekly hours. Established nonworkers receive 0. One-job workers use `H4LM19`; multiple- or unknown-job workers use `H4LM13`; missing routed totals do not fall back to primary-job hours; values are capped at 120. | `R/05_data_construction.R`: `construct_outcome_hours_worked()` |
| Labor-force participation | Wave IV route-aware binary outcome: employment can be established by `H4LM6` or `H4LM11`; among current nonworkers, configured `H4LM14` states separate in- and out-of-labor-force categories. | `R/05_data_construction.R`: `construct_outcome_labor_force_participation()` |
| Compensation | Wave IV exact `H4EC2` values or valid `H4EC3` midpoint when exact is unavailable; valid zero retained; configured identity/log1p/asinh scale and support rules applied. | `R/04_utils.R`: `compute_earnings()`; `R/05_data_construction.R`: `construct_outcome_compensation()` |
| Education and health | Verified Wave III/IV education thresholds and at-least-good self-rated health. | `R/05_data_construction.R`: family constructors |
| Mortality composite | `NDIDD19Y`/`NDIDD19M` are ordered against actual interview year/month. Same-interview-month deaths are not assumed pre-outcome. For noninterviewed respondents, the latest complete wave interview month is the endpoint. A classified pre-outcome death receives observed zero when the composite is enabled. | `R/03_validation.R`: `resolve_mortality_spec()`; `R/05_data_construction.R`: `derive_mortality_indicator_from_data()`, `classify_mortality_timing()`, `apply_mortality_composite()` |
| Baseline covariates | Wave I candidates after suffix-safe role exclusions; `H1FS1:H1FS19` mandatory in every fold; descriptive source registry is not an allowlist. | `R/05_data_construction.R`: candidate-governance helpers |
| Missing codes | Source-informed exact-code dictionary frozen on the complete Wave I merge before fold construction; structural skips and other missingness represented separately. | `R/04_utils.R`; `R/10_tmle.R` |
| Cross-fitting | Five outer folds and internal validation keep sampled-school clusters intact while checking exposure-by-observation support and fold balance. | `R/06_folds.R`; `R/10_tmle.R` |
| Screening/preprocessing | Outer-training-only usability filters, weighted marginal ranks, redundancy control, nested elastic-net union screening, imputation, scaling, and encoding. | `R/07_screening.R`; `R/08_preprocessing.R`; `R/10_tmle.R` |
| Nuisance models | Super Learner models for `Q`, exposure propensity `g`, and outcome observation `pi`, with deterministic cluster-aware internal validation. | `R/09_learners.R` |
| Survey inference | Raw positive Wave I weights; full survey-domain influence-function inference stratified by `REGION` and clustered by `PSUSCID`; PSU-only result retained as sensitivity output. | `R/10_tmle.R`: `cluster_inference_from_eif()` |
| Integrity gates | Expected complete CES-D n=14,660; analytic n=13,500; treated n=1,342; 132 PSUs; four strata; cutpoint 22. These are configured gates, not newly reproduced results. | `R/02_config.R`; `R/05_data_construction.R` |
| Diagnostics | Sample flow, folds, selection, learner performance, balance, overlap, influence, targeting/EIF, missing codes, outcome support/tails, response models, clipping, MNAR, and output inventory. | `R/11_diagnostics.R` |
| Sensitivity/stability | Full-refit alternative specifications, fixed-nuisance diagnostics, and ten prespecified full-pipeline seeds. Seed summaries are descriptive and are not pooled for inference. | `R/14_sensitivity.R`; `R/15_multiseed.R` |

## Identification and interpretation

The causal ATT interpretation requires consistency, conditional exchangeability
given measured Wave I covariates, treated-population positivity, and a defensible
outcome-observation model. The exposure is a screening threshold rather than a
clinical diagnosis. The policy interpretation must not be extended beyond the
hypothetical exposure contrast without additional assumptions.
