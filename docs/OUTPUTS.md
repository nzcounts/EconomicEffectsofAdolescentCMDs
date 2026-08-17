# Outputs and disclosure handling

All outputs are written to the configured restricted `global.output_dir` with a
run tag, immutable run identifier, source fingerprint, analysis-specification
fingerprint, and resolved-configuration fingerprint.

## Primary artifacts

- `cv_tmle_results*.csv`: headline ATT and secondary estimands, uncertainty,
  target-component means, policy translations, cap definition, support checks,
  effective sample sizes, and provenance.
- `combined_tmle_results.csv`: combined rows when multiple requested
  outcome/wave combinations are run.
- `manuscript_summary.csv`: compact analysis summary generated from the fitted
  object; not a substitute for independently checked manuscript text.
- `PUBLICATION_READY.txt`: produced only after the required files, diagnostics,
  source-integrity checks, finite primary inference, percentage translation,
  and nuisance-fallback gates pass.

## Diagnostic families

The diagnostics directory includes sample flow and sample audits; fold sizes
and support; selection frequency/Jaccard summaries; learner weights and risks;
outcome, exposure, and censoring balance; overlap and propensity summaries;
ATT control-weight ESS; cluster influence and leave-out analyses; targeting and
EIF checks; missing-code and earnings-tail audits; response-model calibration;
g/pi clipping; fixed-nuisance and full-refit sensitivity summaries; MNAR
pattern-mixture, breakdown, extreme-mean, and calibrated analyses; and a final
output inventory.

## Sensitive artifacts

`diagnostic_fit_bundle.rds`, respondent-level predictions, row-linked influence
functions, cluster identifiers, checkpoint files, caches, and configuration
snapshots are restricted derived data. They must remain inside the approved
environment even when the original `.xpt` files are absent.

## Public-release rule

Do not commit the run directory. Select only disclosure-reviewed, aggregate
tables or figures for public release. Re-check small cells, school/PSU
identifiers, extreme observations, influence listings, and metadata headers.
Document the disclosure decision and reviewer for every released artifact.
