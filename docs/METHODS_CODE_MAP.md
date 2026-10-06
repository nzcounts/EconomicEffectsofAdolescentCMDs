# Methods-to-code map

The uploaded code is the methods source of truth.

| Module | Responsibility |
|---|---|
| `R/01_source_fingerprint.R` | Source resolution and complete modular-directory fingerprints. |
| `R/02_config.R` | Configuration, source paths, exact runtime requirements and missing-code overrides. |
| `R/03_validation.R` | Package loading, outcome support and configuration validation. |
| `R/04_utils.R` | Atomic output writing, naming, provenance, missing-code and design utilities. |
| `R/05_data_construction.R` | Wave-I merging, exposure, outcome construction, status/mortality and sample gates. |
| `R/06_folds.R` | Size-first whole-PSU fold allocation with hard exposure/response support gates. |
| `R/07_screening.R` | Survey-weighted rough marginal ranking within training folds. |
| `R/08_preprocessing.R` | Training-learned imputation, levels, indicators and numeric design matrices. |
| `R/09_learners.R` | SuperLearner wrappers, exposure penalty handling and optional control-arm Q fits. |
| `R/10_tmle.R` | Nested screening, sequential observation, nuisance fitting, targeting and survey EIF inference. |
| `R/11_diagnostics.R` | Output summaries, overlap, balance, loss, fixed-fit robustness and MNAR diagnostics. |
| `R/12_runner.R` | Wave/member expansion, fresh-output checks, fit persistence and numerical readiness gates. |
| `R/13_preflight.R` | Synthetic preflight plus stress/configuration checks; no restricted data. |
| `R/14_sensitivity.R` | Full-refit scenarios and the configured analysis dispatcher. |
| `R/15_multiseed.R` | Optional full-pipeline repeated splits, median aggregation and selection stability. |
| `R/16_auxiliary.R` | Saved simulation resume, runtime g bounds, broad/residual Q, two-part MNAR and focused self-tests. |
| `R/17_run_defaults.R` | Final source-time defaults and disabled guarded autorun; loaded before the YAML overlay. |

The nonprotected processed-column budget is 260; mandatory/protected columns
are additional. The primary protected baseline-depression representation is
`MHSum_W1`; birth-record predictors and registered derivatives are denied.
Propensity calibration selects identity versus Platt within outer training
data using whole-PSU CV loss; control-arm Q candidates are off by default.
The residual-Q screen is learned once on outer training data and evaluated
with a fixed design in inner folds: this is not fully nested screen validation.

The observation model separates documented contribution zeros from the
remaining cohort and response. Unknown status is not a baseline propensity
covariate and does not restrict the primary ATT population. Causal and response
identification assumptions remain necessary. The full valid-weight Wave-I
survey design is used for domain linearization, with REGION strata and
PSUSCID clusters. Fixed-fit diagnostics are not full-pipeline bootstrap inference.
