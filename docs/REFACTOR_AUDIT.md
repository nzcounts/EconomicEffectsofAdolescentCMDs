# Source-preservation audit

The reviewed production source contains 16,842 lines. Its SHA-256 is
`1b331f6f7d6181685e562646629d9603821a1435555451caf5874c5eaf052f3f`.

The source was divided at its existing section boundaries:

| Module | Original lines |
|---|---:|
| `R/01_source_fingerprint.R` | 1-178 |
| `R/02_config.R` | 179-1402 |
| `R/03_validation.R` | 1403-1920 |
| `R/04_utils.R` | 1921-3476 |
| `R/05_data_construction.R` | 3477-5819 |
| `R/06_folds.R` | 5820-6040 |
| `R/07_screening.R` | 6041-6241 |
| `R/08_preprocessing.R` | 6242-6516 |
| `R/09_learners.R` | 6517-7267 |
| `R/10_tmle.R` | 7268-11010 |
| `R/11_diagnostics.R` | 11011-13687 |
| `R/12_runner.R` | 13688-14169 |
| `R/13_preflight.R` | 14170-15948 |
| `R/14_sensitivity.R` | 15949-16487 |
| `R/15_multiseed.R` | 16488-16825 |

Thirteen source-derived module bodies are byte-identical to those ranges. Three
repository-interface changes are deliberate:

1. The 16 local drive paths in `R/02_config.R` are unset; real paths are supplied
   through the ignored `config/config.yml`.
2. `R/01_source_fingerprint.R` hashes the complete numbered `R/` directory when
   the repository runner supplies that directory.
3. Source-time autorun lines 16826-16842 are replaced by the explicit
   `scripts/run_analysis.R` entry point.

No estimator, exposure, outcome constructor, mortality rule, preprocessing step,
learner, targeting calculation, survey-inference procedure, diagnostic,
sensitivity scenario, or synthetic assertion was intentionally changed.

The executable-line comparison against the cleaned source also retains the
comment-only edit guarantee from the preceding review: all 14,385 executable
source lines have the same code-only SHA-256,
`7d93e2a5ed5db5adeb66b82e0916d513fb464134644bb5e81037367952a6156f`.

R was unavailable in the preparation workspace. The data-free static audit was
completed, but `testthat` and the full synthetic preflight remain required before
the updated analysis is treated as released.
