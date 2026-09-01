# Module order

The bootstrap sources numbered `.R` files in lexical order. Do not rename or
reorder them without rerunning the full synthetic preflight and checking the
source fingerprint.

| Module | Responsibility |
|---|---|
| `00_config_io.R` | Strict YAML overlays and restricted-input checks. |
| `01_source_fingerprint.R` | Pipeline overview and source fingerprinting. |
| `02_config.R` | Production defaults; public data paths are unset. |
| `03_validation.R` | Runtime, configuration, outcome, and mortality gates. |
| `04_utils.R` | I/O, provenance, missing-code, join, and shared helpers. |
| `05_data_construction.R` | Exposure, supported outcomes, mortality, and analytic sample construction. |
| `06_folds.R` | Whole-school balanced fold allocation. |
| `07_screening.R` | Weighted single-variable screening. |
| `08_preprocessing.R` | Fold-pure numeric and categorical preprocessing. |
| `09_learners.R` | Custom Super Learner wrappers and libraries. |
| `10_tmle.R` | Nested screening, nuisance fitting, targeting, and survey inference. |
| `11_diagnostics.R` | Diagnostics and fixed-nuisance sensitivity calculations. |
| `12_runner.R` | Caches, output gates, and pipeline orchestration. |
| `13_preflight.R` | Synthetic validation, including outcomes and mortality. |
| `14_sensitivity.R` | Full-refit scenario runner with resume support. |
| `15_multiseed.R` | Descriptive algorithmic-stability runner. |
