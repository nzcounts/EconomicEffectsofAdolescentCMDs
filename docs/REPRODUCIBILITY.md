# Reproducibility guide

## Frozen environment

The production configuration enforces R 4.4.1 and the following exact analysis
package versions before restricted data are read:

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

`renv.lock` records these direct production dependencies plus repository tooling.
After the first successful restore on the approved analysis platform, run
`renv::snapshot()` and review any added transitive dependency records before
tagging the release. Do not update packages opportunistically between the
headline analysis and its sensitivity or multiseed runs.

## Clean-room run sequence

1. Clone the repository into the restricted workspace.
2. Verify the commit or release tag and record it in the analysis log.
3. Restore the R environment with `renv::restore()`.
4. Copy `config/config.example.yml` to the ignored `config/config.yml` and
   resolve all 16 restricted source paths.
5. Run `Rscript scripts/run_preflight.R` in a clean session.
6. Create a fresh, empty output directory.
7. Run `Rscript scripts/run_analysis.R --config=config/config.yml`.
8. Require the pipeline's `PUBLICATION_READY.txt` marker before treating a run
   as complete.
9. Review the output inventory, diagnostic-status table, sample-flow gates,
   nuisance fallbacks, targeting checks, and disclosure status.
10. Archive the exact Git commit, ignored local configuration, R session
    information, and output manifest inside the restricted project record.

## Determinism and provenance

- The designated headline seed is 1.
- The full ten-seed stability set is fixed in `cfg$global$multiseed_seeds`.
- Whole-school outer and internal folds are deterministically generated from
  configured seeds.
- Run outputs carry source, analysis-specification, resolved-configuration, and
  input-file fingerprints.
- In the modular repository, the source fingerprint hashes every numbered R
  module in stable relative-path order and is checked again before completion.
- Multi-seed summaries are descriptive algorithmic-stability diagnostics and
  must not be pooled into a new inferential confidence interval.

## CI boundary

Default CI performs structural, configuration, and restricted-artifact checks
without Add Health data. Run the complete synthetic preflight locally after the
frozen learner stack is available. No CI workflow downloads or requests
restricted Add Health inputs.
