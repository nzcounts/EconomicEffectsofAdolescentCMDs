# Supervisor review notes

## Strong features retained

- The primary estimand is explicit and enforced as a jointly targeted ATT.
- Exposure, outcome, mortality window, survey design, sample gates, and package
  versions are all validated before production estimation.
- All data-driven preprocessing and screening are nested within outer training
  folds, with whole-school fold assignment and cluster-aware internal CV.
- The code distinguishes the primary outcome definition from fixed-nuisance
  diagnostics and from full-refit alternative estimands.
- Output writes are atomic, provenance-rich, and protected by a publication
  readiness gate.
- The synthetic preflight covers joins, code-family handling, fold support,
  nuisance learners, targeting, survey inference, outcome construction,
  mortality handling, sensitivity routing, and failure conditions.

## Required decisions before public release

1. Confirm the repository title, owner, student authorship, contributor roles,
   and citation order, then add a completed `CITATION.cff`.
2. Select and add a licence only after every code author agrees; the comparator
   repository's GPL-3.0 licence should not be inherited automatically.
3. Confirm that the Add Health data-use agreement permits public release of all
   variable names and coding details documented here.
4. Run the full synthetic preflight under the frozen R 4.4.1 environment, then
   run a clean restricted-data production analysis. This preparation workspace
   did not contain R and therefore could not execute the R tests.
5. Obtain disclosure review before publishing any empirical table, figure,
   diagnostic, run log, or configuration snapshot.

## Code issues to keep visible

- The inherited configuration and data-construction modules contain numerous
  placeholder outcome definitions. Validation blocks them by default, but their
  presence can still confuse readers. After the primary replication is frozen,
  consider moving them to an explicitly experimental branch or deleting them.
- `R/10_tmle.R` and `R/11_diagnostics.R` remain large because they preserve the
  reviewed production order. Further decomposition should be a separate,
  tested change rather than mixed into this provenance-sensitive refactor.
- The original runtime validator contains one duplicated assignment of the
  detected R version. It is harmless and retained to keep the refactor narrow.
- Sample-size values in configuration are integrity gates. They should not be
  copied into a manuscript as results unless reproduced by the frozen run and
  verified against the sample-flow output.
- The introductory header's causal wording is stronger than the identification
  assumptions warrant. Public-facing interpretation should use the qualified
  language in `METHODS_CODE_MAP.md`.

## Suggested review sequence

1. Read `R/02_config.R` and verify every estimand-defining switch.
2. Trace exposure, outcome, mortality, survey-frame, and candidate construction
   through `R/05_data_construction.R`.
3. Inspect fold purity and support in `R/06_folds.R` and screening in
   `R/07_screening.R`/`R/10_tmle.R`.
4. Check learner wrappers and failure gates in `R/09_learners.R`.
5. Review ATT targeting, counterfactual response predictions, and survey EIF
   inference in `R/10_tmle.R`.
6. Match each required publication artifact to `R/11_diagnostics.R` and
   `R/12_runner.R`.
7. Execute `R/13_preflight.R` before any restricted-data run.
8. Review every full-refit sensitivity and the descriptive multiseed contract.
