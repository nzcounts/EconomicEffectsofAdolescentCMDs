# Tests

`Rscript tests/testthat.R` runs lightweight checks for module loading,
configuration overlays, source-tree fingerprints, and the restricted-data
boundary.

The estimator's full synthetic preflight is defined in `R/13_preflight.R` and
can be run with `Rscript scripts/run_preflight.R`. It is skipped in the default
lightweight suite unless `RUN_FULL_PREFLIGHT=true` because it loads the frozen
production learner stack and may be computationally expensive.

No test reads Add Health data.
