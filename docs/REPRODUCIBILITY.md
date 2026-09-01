# Reproducibility guide

## Frozen environment

The production configuration enforces R 4.4.1 and these exact analysis package
versions before restricted data are read:

| Package | Version |
|---|---:|
| `haven` | 2.5.4 |
| `dplyr` | 1.1.4 |
| `purrr` | 1.0.2 |
| `SuperLearner` | 2.0.29 |
| `glmnet` | 4.1.8 |
| `survey` | 4.4.2 |
| `ranger` | 0.16.0 |
| `xgboost` | 1.7.8.1 |
| `Matrix` | 1.7.4 |

`renv.lock` records the direct production dependencies and repository tooling.
After the first successful restore on the approved platform, run
`renv::snapshot()` and review any transitive additions before tagging a release.
Do not update packages between the headline, sensitivity, and multiseed runs.

## Clean run sequence

1. Clone the repository into the restricted workspace and record the commit.
2. Restore packages with `renv::restore()`.
3. Copy `config/config.example.yml` to the ignored `config/config.yml`.
4. Resolve all required restricted source paths and select one outcome.
5. Run `Rscript scripts/run_preflight.R` in a clean session.
6. Use a fresh output directory.
7. Run `Rscript scripts/run_analysis.R --config=config/config.yml`.
8. Require the configured publication-ready marker before treating a run as complete.
9. Review sample gates, mortality audits, support, nuisance fallbacks, targeting,
   diagnostics, output inventory, and disclosure status.
10. Archive the commit, ignored configuration, session information, and output
    manifest inside the restricted project record.

## Determinism and provenance

- The designated headline seed is 1.
- The ten-seed set is fixed in `cfg$global$multiseed_seeds`.
- Whole-school outer and internal folds are deterministic from configured seeds.
- The modular fingerprint hashes every numbered R file in stable relative-path order.
- Multi-seed summaries describe algorithmic stability and do not define a pooled CI.

## Test boundary

`Rscript tests/testthat.R` runs data-free structural and focused logic tests.
`Rscript scripts/run_preflight.R` runs the full synthetic estimator preflight.
Neither reads Add Health data. The full preflight requires the frozen learner
stack and is intentionally not run by the lightweight GitHub Actions workflow.
