# Module order

The bootstrap sources numbered `.R` files lexicographically. Do not rename or
reorder them without rerunning the full synthetic preflight and reviewing the
source fingerprint.

| Module | Responsibility |
|---|---|
| `00_config_io.R` | Strict YAML overlays and restricted-input checks. |
| `01_source_fingerprint.R` | Original overview and source provenance. |
| `02_config.R` | Production defaults; public data paths are unset. |
| `03_validation.R` | Configuration, runtime, outcome-verification, and learner gates. |
| `04_utils.R` | Atomic I/O, provenance, missing-code rules, joins, and shared helpers. |
| `05_data_construction.R` | Wave I merge, exposure/outcome construction, mortality, and analytic sample. |
| `06_folds.R` | Whole-school balanced fold allocation. |
| `07_screening.R` | Weighted single-variable screening helpers. |
| `08_preprocessing.R` | Fold-pure numeric and categorical preprocessing. |
| `09_learners.R` | Custom Super Learner wrappers and libraries. |
| `10_tmle.R` | Nested screening, nuisance fitting, ATT targeting, and survey inference. |
| `11_diagnostics.R` | Reviewer-facing diagnostics and sensitivity calculations. |
| `12_runner.R` | Caches, output gates, and top-level pipeline orchestration. |
| `13_preflight.R` | End-to-end synthetic validation. |
| `14_sensitivity.R` | Full-refit scenario runner with resume support. |
| `15_multiseed.R` | Descriptive algorithmic-stability runner. |
