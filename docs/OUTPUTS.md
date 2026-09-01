# Outputs and disclosure handling

Outputs are written below the configured restricted `global.output_dir`. CSV
headers and manifests record run identity, source fingerprint, analysis
fingerprint, and resolved configuration.

## Primary artifacts

- `cv_tmle_results*.csv`: headline ATT, comparator estimands, uncertainty,
  component means, outcome definition/support, policy translations, and provenance.
- `combined_tmle_results.csv`: rows combined across requested members or waves.
- `manuscript_summary.csv`: compact fitted-result summary for review.
- `PUBLICATION_READY.txt`: written only after the configured completeness and
  diagnostic gates pass.

## Diagnostic families

The diagnostics directory can contain sample-flow and sample-audit tables;
fold support; variable-selection stability; learner weights and risks; balance;
overlap; effective sample sizes; cluster influence; targeting and EIF checks;
missing-code audits; outcome-construction, support, and tail checks; mortality
linkage/timing/composite audits; response-model calibration; clipping checks;
MNAR analyses; sensitivity summaries; and an output inventory.

## Sensitive artifacts

The diagnostic fit bundle, respondent-linked predictions and influence values,
cluster-level results, caches, checkpoints, logs, configuration snapshots, and
run directories are restricted derived data. They remain restricted even when
the original `.xpt` inputs are not present.

## Public-release rule

Do not commit a run directory. Copy only disclosure-reviewed aggregate artifacts
to a separately reviewed release location. Record the analysis commit, outcome
specification, run date, disclosure decision, and reviewer. Recheck small cells,
cluster identifiers, extreme observations, influence listings, and metadata.
